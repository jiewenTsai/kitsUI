test_that("檢查表沒有提醒時一列「全部通過」；沒有可檢查的東西時不顯示", {
  expect_null(checks_or_ok(NULL))
  ok <- checks_or_ok(data.frame(x = character()))
  expect_equal(names(ok), "result")
  expect_match(ok$result, "全部通過")
  tab <- data.frame(status = "warn")
  expect_identical(checks_or_ok(tab), tab)
})

test_that("說明框、提示框、下一步", {
  g <- as.character(mod_guide("目的", c("一", "二"), "結果", note = "補充"))
  expect_match(g, "kit-guide-note")
  expect_match(g, "<li>二</li>")
  expect_false(grepl("kit-guide-note", as.character(mod_guide("目的", "一", "結果"))))
  # 預設收起：只看到「這一頁做什麼」一句，步驟在收起來的內容裡
  expect_match(g, '^<details class="kit-guide kit-guide-page">')
  expect_false(grepl("<details[^>]*open", g))
  expect_match(g, "<summary>\\s*<strong>這一頁做什麼：</strong>\\s*目的")
  expect_match(g, 'class="kit-guide-body"')
  expect_match(as.character(ui_callout("x", warning = TRUE)), "kit-callout kit-callout-warn")
  expect_match(as.character(ui_alert("danger", "【停】", "x")), "alert-danger")
  expect_match(as.character(ui_next_step("好了", "go", "下一步")), "id=\"go\"")
})

test_that("下載：檔名可以是函式；表還沒有內容時寫一列說明", {
  expect_equal(download_name("item_lan M03_category"), "item_lan_M03_category")
  expect_equal(download_name(\() "raw table"), "raw_table")
  expect_equal(nrow(csv_or_note(\() data.frame(a = 1:3))), 3)
  expect_match(csv_or_note(\() NULL)$note, "還沒有內容")
  expect_match(csv_or_note(\() shiny::req(NULL))$note, "還沒有內容")
  expect_match(csv_or_note(\() stop("壞了"))$note, "壞了")
})

test_that("表格：整數欄不顯示小數，其他欄小數點 2 位", {
  df <- data.frame(wave_m = c(3, 6), theta = c(-1.234, 0.5), n = c(1L, NA), id = c("a", "b"))
  out <- whole_as_integer(df)
  expect_type(out$wave_m, "integer")
  expect_type(out$theta, "double")
  expect_equal(out$id, df$id)
  html <- shiny::testServer(\(input, output, session) {
    output$t <- kit_table(df)
  }, expect_match(output$t, "-1.23"))
})

test_that("橘色提醒的清單：沒有項目時不顯示", {
  expect_null(ui_warn_list("請注意", character()))
  html <- as.character(ui_warn_list("請注意", c("一", "二"), shiny::tags$p("補充")))
  expect_match(html, "kit-callout-warn")
  expect_match(html, "<li>二</li>")
  expect_match(html, "補充")
})

# WCAG 的對比：相對亮度的比
contrast <- function(fg, bg) {
  lum <- function(hex) {
    x <- grDevices::col2rgb(hex)[, 1] / 255
    x <- ifelse(x <= 0.03928, x / 12.92, ((x + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * x)
  }
  l <- sort(c(lum(fg), lum(bg)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

# CSS 裡一個選擇器的區塊（selector 後面接 " {"）裡，一個屬性的顏色或值
css_prop <- function(css, selector, prop) {
  at <- regexpr(paste0(selector, " {"), css, fixed = TRUE)
  block <- substr(css, at, at + regexpr("}", substr(css, at, nchar(css)), fixed = TRUE))
  m <- regmatches(block, regexpr(paste0("(^|[ ;{\n])", prop, ": [^;]+"), block))
  sub(".*: ", "", m)
}

test_that("對比 4.5:1 以上：雲端提醒、橘色提醒、頁尾；頁尾字 13px 以上", {
  css <- as.character(kit_css())
  banner <- css_prop(css, ".alert.kit-cloud-banner", "background")
  expect_gte(contrast(css_prop(css, ".alert.kit-cloud-banner", "color"), banner), 4.5)
  warn_bg <- css_prop(css, ".kit-callout-warning", "background")
  expect_gte(contrast(css_prop(css, ".kit-callout-warning", "color"), warn_bg), 4.5)
  expect_match(css, ".kit-callout-warn,\n    .kit-callout-warning {", fixed = TRUE)
  warn_text <- css_prop(css, ".kit-warn-text", "color")
  expect_gte(contrast(warn_text, "#ffffff"), 4.5)
  expect_gte(contrast(warn_text, warn_bg), 4.5)
  expect_equal(css_prop(css, ".kit-cloud-footer", "font-size"), "13px")
  expect_gte(contrast(css_prop(css, ".kit-cloud-footer", "color"), kit_colors()$body_bg), 4.5)
})

test_that("雲端版的提醒用深色字；離開頁面前的提醒（beforeunload）在頁面裡", {
  expect_match(as.character(kit_cloud_banner()), "kit-cloud-banner")
  expect_match(as.character(kit_js()), "beforeunload")
  expect_match(as.character(kit_js()), "kit_unload_guard")
  expect_match(as.character(htmltools::renderTags(kit_ui(NULL))$head), "kit_unload_guard")
})

test_that("找 pandoc：PATH、RSTUDIO_PANDOC、常見的資料夾；都沒有時 NULL", {
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  dir <- withr::local_tempdir()
  empty <- file.path(dir, "empty")
  tools <- file.path(dir, "quarto", "bin", "tools", "x86_64")
  dir.create(empty)
  dir.create(tools, recursive = TRUE)
  file.create(file.path(tools, exe))
  find <- \(...) kit_find_pandoc(..., use_rmarkdown = FALSE)
  expect_equal(find(dirs = c(empty, tools), sys = "", env = ""), tools)
  expect_null(find(dirs = empty, sys = "", env = ""))
  expect_equal(find(dirs = empty, sys = "/usr/bin/pandoc", env = ""), "/usr/bin")
  expect_equal(find(dirs = empty, sys = "", env = tools), tools)
  # RSTUDIO_PANDOC 指到沒有 pandoc 的資料夾：不算
  expect_equal(find(dirs = c(empty, tools), sys = "", env = empty), tools)
  d <- kit_pandoc_dirs()
  expect_true(all(c("/opt/quarto/bin/tools/x86_64",
                    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64",
                    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
                    "C:/Program Files/Quarto/bin/tools") %in% d))
})

test_that("找不到 pandoc：頁面最上方說明；下載圖檔時停下來並說明（不是空白的錯誤）", {
  expect_match(as.character(kit_pandoc_notice()), "找不到 pandoc：圖檔（HTML）不能下載")
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
  file <- withr::local_tempfile(fileext = ".html")
  expect_error(plot_save(p, file, pandoc_ok = FALSE), "找不到 pandoc", class = "kit_user_error")
  expect_error(plot_save(\() shiny::req(NULL), file, pandoc_ok = TRUE), "還沒有內容",
               class = "kit_user_error")
  # 畫按鈕時還找得到 pandoc、按下載時找不到：回 500，畫面上有通知
  pandoc <- TRUE
  mock_app_fn("kit_pandoc_ok", \() pandoc)
  shiny::testServer(\(input, output, session) {
    output$dl_ui <- render_download_plot(session, "dl", "下載圖（HTML）", \() p, "plot")
  }, {
    expect_match(output$dl_ui$html, "plot.html")
    pandoc <<- FALSE
    expect_message(res <- kit_test_download(session, "dl"), "找不到 pandoc")
    expect_equal(res$status, 500)
  })
  # 找不到 pandoc：頁面最上方說明；下載鈕換成不能按的鈕與說明（沒有連結）
  expect_match(as.character(kit_ui(NULL)), "kit-pandoc-notice")
  shiny::testServer(\(input, output, session) {
    output$dl_ui <- render_download_plot(session, "dl", "下載圖（HTML）", \() p, "plot")
  }, {
    html <- output$dl_ui$html
    expect_match(html, "下載圖（HTML）")
    expect_match(html, "找不到 pandoc：圖檔（HTML）不能下載")
    expect_match(html, "disabled")
    expect_no_match(html, "href")
    expect_null(session$userData$kit_downloads)
  })
})

test_that("下載鈕：檔名在畫按鈕時決定，跟著 input 變；CSV 位元組和 write_excel_csv() 一樣", {
  tab <- data.frame(item_id = c("a", "b"), beta = c(0.5, NA), note = c("王", "李"))
  shiny::testServer(\(input, output, session) {
    output$dl_ui <- render_download_csv(session, "dl", "下載表（CSV）", \() tab,
                                        \() paste0("item table_", input$scale))
    output$none_ui <- render_download_csv(session, "none", "下載（CSV）", \() NULL,
                                          \() shiny::req(input$missing))
  }, {
    session$setInputs(scale = "lan_M03")
    html <- output$dl_ui$html
    expect_match(kit_link_attr(html, session$ns("dl"), "href"), "/item_table_lan_M03\\.csv\\?")
    expect_equal(kit_link_attr(html, session$ns("dl"), "download"), "item_table_lan_M03.csv")
    expect_match(html, "btn-sm btn-outline-primary")
    res <- kit_test_download(session, "dl")
    expect_equal(res$content_type, "text/csv; charset=UTF-8")
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(tab, old)
    expect_equal(file_bytes(res$content$file), file_bytes(old))
    session$setInputs(scale = "motor_M03")
    expect_match(output$dl_ui$html, "item_table_motor_M03.csv")
    expect_equal(kit_test_download_name(session, "dl"), "item_table_motor_M03.csv")
    # 檔名拿不到（req 停下來）時先不畫按鈕
    expect_null(output$none_ui)
  })
})

test_that("啟動時找 pandoc（這台電腦有 Quarto 時找得到），圖存成單一 html", {
  withr::local_envvar(RSTUDIO_PANDOC = Sys.getenv("RSTUDIO_PANDOC"))
  kit_setup_pandoc()
  skip_if_not(kit_pandoc_ok())
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
  file <- withr::local_tempfile(fileext = ".html")
  plot_save(p, file)
  expect_true(file.exists(file))
})

test_that("畫面用的欄名：beta -> difficulty、thr -> threshold、theta -> ability、d -> d_score", {
  df <- data.frame(item_id = "a", beta = 1, beta_se = 0.1, thr = 1, thr_se = 0.1, theta = 2, d = 50,
                   d_se = 3)
  expect_named(kitsui_display_names(df),
               c("item_id", "difficulty", "difficulty_se", "threshold", "threshold_se", "ability",
                 "d_score", "d_score_se"))
})
