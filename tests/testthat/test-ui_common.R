test_that("檢查表沒有提醒時一列「全部通過」；沒有可檢查的東西時不顯示", {
  expect_null(checks_or_ok(NULL))
  ok <- checks_or_ok(data.frame(x = character()))
  expect_equal(names(ok), "result")
  expect_match(ok$result, "全部通過")
  tab <- data.frame(status = "warn")
  expect_identical(checks_or_ok(tab), tab)
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

test_that("頁面用 kitDesign：主題、樣式與小程式（離開前提醒、分頁狀態）、頁首的版本行；雲端版提醒", {
  html <- htmltools::renderTags(kit_ui(NULL))
  deps <- vapply(htmltools::resolveDependencies(html$dependencies), \(d) d$name, character(1))
  expect_true("kitDesign" %in% deps)
  ver <- kitDesign::kit_app_description("kitsUI")
  expect_match(html$html, paste0("kitsUI v", ver$version, " · kits v", utils::packageVersion("kits"),
                                 " · ", ver$date, "<"), fixed = TRUE)
  expect_match(html$html, 'data-kit-aside="manual"', fixed = TRUE)
  expect_match(html$head, ".kit-kitjoin-local code", fixed = TRUE)
  expect_match(as.character(kitsui_cloud_banner()), "kit-cloud-banner")
  expect_match(as.character(kitsui_cloud_banner()), "#kit-cloud-terms")
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
  # validate() 停下來：說的是它的原因
  expect_error(plot_save(\() shiny::validate("沒有題庫，沒有偏移把關圖。"), file, pandoc_ok = TRUE),
               "^沒有題庫，沒有偏移把關圖。$", class = "kit_user_error")
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

test_that("圖畫不出來時不畫下載鈕，畫得出來時出現；按下載失敗只通知一次，說的是原因", {
  p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
  mock_app_fn("kit_pandoc_ok", \() TRUE)
  shiny::testServer(\(input, output, session) {
    plot_fun <- shiny::reactive({
      shiny::validate(shiny::need(isTRUE(input$ok), "這個量尺沒有題庫，沒有偏移把關圖。"))
      p
    })
    output$dl_ui <- render_download_plot(session, "dl", "下載圖（HTML）", plot_fun, "plot")
  }, {
    expect_null(output$dl_ui)
    session$setInputs(ok = TRUE)
    expect_match(output$dl_ui$html, "plot.html")
    session$setInputs(ok = FALSE)
    expect_null(output$dl_ui)
    # 畫面還沒更新、按了舊的鈕：kitDesign 的下載通知一次（plot_save() 自己不通知）
    notes <- character()
    testthat::local_mocked_bindings(
      showNotification = \(ui, ...) notes <<- c(notes, as.character(ui)), .package = "shiny"
    )
    expect_message(res <- kit_test_download(session, "dl"), "沒有題庫")
    expect_equal(res$status, 500)
    expect_equal(notes, "下載失敗：這個量尺沒有題庫，沒有偏移把關圖。")
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
    expect_match(html, "btn-sm btn-outline-secondary")
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

test_that("設定摘要：世代、波次帶單位、參照世代、固定檔", {
  picks <- data.frame(cohort = c("B82", "B85", "B85"), wave_m = c(2009, 2009, 2011))
  fx <- kitsui_fixed_empty("B85")
  fx$version$wave_unit <- "years"
  d <- list(picks = picks, fixed = fx, round_label = "R1")
  expect_equal(kitsui_settings_text(d),
               "本回合 R1：B82、B85 · 2009、2011 年 · 參照世代 B85 · 沒有固定檔（第一回合）")
  fx$version$rounds <- "R1"
  d <- list(picks = picks, fixed = fx, round_label = "R2", fixed_file = "kitsui_fixed_R1.zip")
  expect_match(kitsui_settings_text(d), "固定檔：kitsui_fixed_R1.zip$")
  expect_equal(kitsui_settings_text(NULL), "")
  expect_null(kitDesign::kit_settings_line(kitsui_settings_text(NULL), prefix = NULL))
})

test_that("摘要表用中文欄名；kits 的表不換", {
  df <- data.frame(scale = "a", n_visits = 1, n_items = 2, waves = "3", calibration = "x",
                   beta = 1)
  expect_named(kitsui_screen_names(df),
               c("量尺", "列次數", "題數", "波次", "校準方式", "beta"))
})

test_that("本機版不連到雲端的 kitJoin：一行說明；雲端版是連結加一句說明", {
  local <- as.character(ui_kitjoin_button(cloud = FALSE))
  expect_false(grepl(KITJOIN_URL, local, fixed = TRUE))
  expect_match(local, "kitJoin::run_kitjoin()", fixed = TRUE)
  expect_match(local, "資料不會離開這台電腦")
  cloud <- as.character(ui_kitjoin_button(cloud = TRUE))
  expect_match(cloud, KITJOIN_URL, fixed = TRUE)
  expect_match(cloud, "雲端版 kitJoin")
  hint <- as.character(ui_kitjoin_hint(cloud = FALSE))
  expect_match(hint, "「kits 作答檔」")
  expect_false(grepl("⑥", hint))
})

test_that("表格的計數欄加千分位，波次等數字不加", {
  df <- data.frame(scale = "a", wave_m = 2009, n = 12345, mean = 1.5)
  out <- kit_count_marks(whole_as_integer(df))
  expect_equal(as.vector(out$n), "12,345")
  expect_equal(out$wave_m, 2009L)
  expect_equal(kit_table_align(out), "lrrr")
})
