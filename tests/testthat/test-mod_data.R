# ① 資料：示範資料、上傳、世代 × 波次、固定檔、這一回合的名稱、提醒

# 上傳用的檔案（fileInput 的格式）
upload_input <- function(path, name) {
  data.frame(name = name, size = file.size(path), type = "", datapath = path)
}

# 這一頁的測試不等名稱的 0.5 秒（要測停手時另外設定）
withr::local_options(kitsui.label_delay = 0, .local_envir = teardown_env())

test_that("示範資料：預設什麼都不勾，只有中性的提示（不是紅字）；勾 M03 3 回傳可以用的資料", {
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", round_label = "")
    expect_null(session$getReturned()())
    expect_null(output$problems)
    expect_match(output$hint$html, "請勾這一回合的世代與波次")
    expect_false(grepl("alert-danger", output$hint$html))
    # 勾選框：一個世代一列，值是「世代 月齡」
    expect_match(output$pick_ui$html, 'value="M03 3"')
    expect_match(output$pick_ui$html, 'value="M36 96"')
    expect_false(grepl("checked", output$pick_ui$html))
    session$setInputs(waves = "M03 3")
    d <- session$getReturned()()
    expect_type(d, "list")
    expect_equal(unique(d$df$wave_m), 3)
    expect_equal(d$waves, 3)
    expect_equal(d$picks$value, "M03 3")
    expect_equal(d$scales$scale, "lan M03")
    expect_equal(d$round_label, "R1")
    expect_equal(d$source, "demo")
    expect_null(d$files)
    expect_equal(nrow(d$fixed$bank), 0)
    expect_equal(d$persons, kitsui_persons(d$df))
    expect_null(output$hint)
    # 狀態框、設定摘要、量尺表（中文欄名）
    expect_false(grepl("停下來", output$data_status$html))
    expect_match(output$data_status$html, "【完成】")
    expect_match(output$data_status$html, "資料可以用：1 個量尺、[0-9,]+ 列作答。")
    expect_match(output$settings$html, "本回合 R1：M03 · 3 個月 · 參照世代 M03")
    expect_match(output$scales, "列次數")
    expect_match(output$scales, "校準方式")
    expect_false(grepl("n_visits", output$scales))
    expect_match(output$next_step$html, "下一步：題目校準 →")
    expect_match(output$code, "kits_resp")
    session$setInputs(design_scale = "lan M03")
    expect_s3_class(design_plot(), "plotly")
  })
})

test_that("勾世代 × 波次：只拿勾到的組合（M03 36 有、M36 36 沒有）", {
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", round_label = "",
                      waves = c("M03 36", "M36 48"))
    d <- session$getReturned()()
    got <- dplyr::distinct(d$df, cohort, wave_m) |> dplyr::arrange(cohort)
    expect_equal(got$cohort, c("M03", "M36"))
    expect_equal(got$wave_m, c(36, 48))
    expect_equal(d$waves, c(36, 48))
    expect_equal(d$scales$scale, c("lan M03", "lan M36"))
    # 只寫月齡時是所有世代的這個月齡
    session$setInputs(waves = "36")
    expect_setequal(unique(session$getReturned()()$df$cohort), c("M03", "M36"))
  })
})

test_that("名稱：預設 R1；不合規時回傳 NULL 並用紅字說明；打字停手 0.5 秒才檢查", {
  withr::local_options(kitsui.label_delay = 500)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "")
    session$elapse(501)
    expect_equal(session$getReturned()()$round_label, "R1")
    session$setInputs(round_label = "第一回合")
    expect_equal(session$getReturned()()$round_label, "R1")
    session$elapse(501)
    expect_null(session$getReturned()())
    expect_match(output$problems$html, "英文字母與數字")
    session$setInputs(round_label = "pilot2")
    session$elapse(501)
    expect_equal(session$getReturned()()$round_label, "pilot2")
  })
})

test_that("改名稱、換固定檔不重跑資料的篩選與檢查；勾選變了才重跑", {
  calls <- 0
  real <- kitsui_data_select
  mock_app_fn("kitsui_data_select", function(...) {
    calls <<- calls + 1
    real(...)
  })
  zip <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(lan_chain()[["3"]]$fixed, zip)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 6", round_label = "")
    expect_type(session$getReturned()(), "list")
    n <- calls
    session$setInputs(round_label = "abc")
    expect_equal(session$getReturned()()$round_label, "abc")
    session$setInputs(fixed_file = upload_input(zip, "fixed_R1.zip"))
    expect_match(output$scales, "固定校準")
    expect_equal(session$getReturned()()$fixed$version$rounds, "3")
    expect_equal(calls, n)
    session$setInputs(waves = c("M03 6", "M03 12"))
    expect_equal(session$getReturned()()$waves, c(6, 12))
    expect_equal(calls, n + 1)
  })
})

test_that("上傳 CSV；缺欄位時說明缺什麼", {
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(kitsui_demo("M03", 3, "lan"), path)
  bad <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(dplyr::select(kitsui_demo("M03", 3, "lan"), -"domain"), bad)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", file = upload_input(path, "m03_lan.csv"),
                      waves = "M03 3", round_label = "")
    d <- session$getReturned()()
    expect_equal(d$data_file, "m03_lan.csv")
    expect_equal(names(d$files), "m03_lan.csv")
    expect_equal(d$scales$scale, "lan M03")
    session$setInputs(file = upload_input(bad, "bad.csv"))
    expect_null(session$getReturned()())
    expect_match(output$problems$html, "domain")
    # 上傳的檔案沒通過：紅字裡有開啟 kitJoin 的按鈕（新分頁）
    expect_match(output$problems$html, KITJOIN_URL, fixed = TRUE)
    expect_match(output$problems$html, "target=\"_blank\"", fixed = TRUE)
  })
})

test_that("紅字只有名稱不合規時，不提 kitJoin", {
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "第一回合")
    session$elapse(1001)
    expect_match(output$problems$html, "英文字母與數字")
    expect_false(grepl(KITJOIN_URL, output$problems$html, fixed = TRUE))
  })
})

test_that("上傳說明有 kitJoin 的按鈕", {
  html <- as.character(mod_data_ui("data"))
  expect_match(html, KITJOIN_URL, fixed = TRUE)
  expect_match(html, "⑥ 轉成 kits 作答檔", fixed = TRUE)
})

test_that("上一回合的固定檔：讀入、顯示內容、改成固定校準、預設名稱是下一個 R", {
  zip <- withr::local_tempfile(fileext = ".zip")
  first <- suppressMessages(kitsui_round(kitsui_demo("M03", 3, "lan"), kitsui_fixed_empty(), "R1"))
  kitsui_write_fixed(first$fixed, zip)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 6", round_label = "",
                      fixed_file = upload_input(zip, "fixed_set.zip"))
    d <- session$getReturned()()
    expect_equal(d$fixed$version$rounds, "R1")
    expect_equal(d$fixed_file, "fixed_set.zip")
    expect_equal(d$round_label, "R2")
    expect_match(output$fixed_info$html, "lan M03")
    expect_match(output$scales, "固定校準")
    session$setInputs(fixed_clear = 1)
    expect_equal(nrow(session$getReturned()()$fixed$bank), 0)
    expect_equal(session$getReturned()()$round_label, "R1")
  })
})

test_that("固定檔放了不是 zip 的檔案：紅字說明，之前的固定檔留著", {
  zip <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(lan_chain()[["3"]]$fixed, zip)
  csv <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(kitsui_demo("M03", 3, "lan"), csv)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 6", round_label = "")
    expect_null(output$fixed_error)
    session$setInputs(fixed_file = upload_input(zip, "fixed_R1.zip"))
    session$setInputs(fixed_file = upload_input(csv, "responses.csv"))
    expect_match(output$fixed_error$html, "這不是固定檔（zip）")
    expect_match(output$fixed_error$html, "alert-danger")
    expect_match(output$fixed_error$html, "【停下來】")
    expect_match(output$fixed_error$html, "仍然用之前的固定檔：fixed_R1.zip")
    d <- session$getReturned()()
    expect_equal(d$fixed$version$rounds, "3")
    expect_equal(d$fixed_file, "fixed_R1.zip")
    # 沒有之前的固定檔時：說現在沒有
    session$setInputs(fixed_clear = 1)
    expect_null(output$fixed_error)
    session$setInputs(fixed_file = upload_input(csv, "again.csv"))
    expect_match(output$fixed_error$html, "現在沒有固定檔")
  })
})

test_that("作答設計：換一個列數相同的新檔案時重新整理（不用舊的）；下載鈕等圖畫好才出現", {
  a <- kitsui_demo("M03", 3, "lan")
  b <- dplyr::mutate(a, item_score = ifelse(item_id == item_id[1], 0, item_score))
  pa <- withr::local_tempfile(fileext = ".csv")
  pb <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(a, pa)
  readr::write_csv(b, pb)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", file = upload_input(pa, "a.csv"), round_label = "")
    expect_null(output$design_dl)
    session$setInputs(waves = "M03 3", design_scale = "lan M03")
    ra <- design_resp()
    expect_match(output$design_dl$html, "下載作答設計圖（HTML）")
    if (kit_pandoc_ok()) {
      href <- kit_link_attr(output$design_dl$html, session$ns("dl_design"), "href")
      expect_match(href, "/design_lan_M03\\.html\\?")
    }
    session$setInputs(file = upload_input(pb, "b.csv"))
    rb <- design_resp()
    expect_equal(nrow(checked()$df), nrow(a))
    expect_false(identical(ra, rb))
    expect_identical(rb, kitsui_resp(b, "lan M03"))
  })
})

test_that("下載的檔名：responses_<名稱>.csv、design_<量尺>.html", {
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "",
                      design_scale = "lan M03")
    html <- output$dl_data_ui$html
    expect_match(html, "下載這一回合的資料（CSV）")
    expect_equal(kit_link_attr(html, session$ns("dl_data"), "download"), "responses_R1.csv")
    expect_match(kit_link_attr(html, session$ns("dl_data"), "href"), "/responses_R1\\.csv\\?")
    res <- kit_test_download(session, "dl_data")
    expect_equal(res$headers[["Content-Disposition"]], 'attachment; filename="responses_R1.csv"')
    expect_equal(nrow(readr::read_csv(res$content$file, show_col_types = FALSE)),
                 nrow(checked()$df))
    # 位元組和原本的寫法（write_excel_csv）一樣
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(checked()$df, old)
    expect_equal(file_bytes(res$content$file), file_bytes(old))
    # 改名稱：按鈕重畫，檔名跟著變
    session$setInputs(round_label = "R7")
    expect_match(output$dl_data_ui$html, 'download="responses_R7.csv"')
    expect_equal(kit_test_download_name(session, "dl_data"), "responses_R7.csv")
    if (kit_pandoc_ok()) expect_match(output$design_dl$html, 'download="design_lan_M03.html"')
  })
})

test_that("得分不完整的題：橘色提醒（不是紅字），說 ② 怎麼處理；下一步不只說「資料沒問題」", {
  skip_if_not(exists("kitsui_category_problems", mode = "function"))
  df <- kitsui_demo("M03", 3, "lan")
  it <- df$item_id[1]
  df$item_score[df$item_id == it] <- pmax(df$item_score[df$item_id == it], 1)
  path <- withr::local_tempfile(fileext = ".csv")
  readr::write_csv(df, path)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", file = upload_input(path, "a.csv"), waves = "M03 3",
                      round_label = "")
    expect_type(session$getReturned()(), "list")
    html <- output$categories$html
    expect_match(html, "kit-callout-warn")
    expect_match(html, it)
    expect_match(html, "固定在題庫的值")
    expect_match(html, "這一回合先不估它")
    expect_match(html, "作答的分數不會被改動")
    expect_match(html, KITJOIN_URL, fixed = TRUE)
    expect_null(output$problems)
    expect_match(output$data_status$html, "【完成，但請注意】")
    expect_false(grepl("資料沒問題", output$next_step$html))
    expect_match(output$next_step$html, "要注意")
  })
})

test_that("橘色提醒：波次之前的回合校準過、資料檔之前的回合用過", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "m03_3.csv")
  readr::write_csv(kitsui_demo("M03", 3, "lan"), path)
  up <- kitsui_read_uploads(data.frame(name = "m03_3.csv", datapath = path))
  first <- suppressMessages(kitsui_round(up$data, kitsui_fixed_empty(), "R1",
                                         files = c(m03_3.csv = path)))
  zip <- file.path(dir, "fixed_R1.zip")
  kitsui_write_fixed(first$fixed, zip)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", file = upload_input(path, "m03_3.csv"),
                      waves = "M03 3", round_label = "",
                      fixed_file = upload_input(zip, "fixed_R1.zip"))
    html <- output$notes$html
    expect_match(html, "kit-callout-warn")
    expect_match(html, "這些波次之前的回合已經校準過：lan M03 的 3 個月（第 R1 回合）")
    expect_match(html, "m03_3.csv：這個檔案在第 R1 回合用過")
    # 提醒不擋下一步
    expect_type(session$getReturned()(), "list")
    expect_match(output$next_step$html, "要注意")
  })
})

test_that("兩回合接著做：④ 交出的固定檔給 ①（沒有紅字），第二回合固定校準、舊的 beta 不動", {
  shiny::testServer(kit_app(), {
    session$setInputs(`data-source` = "demo", `data-domains` = "lan",
                      `data-waves` = "M03 3", `data-round_label` = "")
    session$setInputs(`items-run` = 1)
    first <- items()
    expect_true(first$ok)
    expect_match(output$`data-fixed_choice`$html, "fixed_file")

    # ④：用這一回合的固定檔開始下一回合
    session$setInputs(`score-next_round` = 1)
    expect_equal(handover()$round, "R1")
    expect_match(output$`data-fixed_choice`$html, "上一回合的固定檔：來自這次連線的第 R1 回合")
    expect_false(grepl("fixed_file", output$`data-fixed_choice`$html))
    expect_match(output$`data-fixed_info`$html, "這次連線的第 R1 回合")
    # 交過來時沒有紅字（世代與波次清空，只有中性的提示）
    session$setInputs(`data-waves` = character())
    expect_null(output$`data-problems`)
    expect_match(output$`data-hint`$html, "請勾這一回合的世代與波次")
    expect_null(items())

    # 第二回合：M03 6 個月，用交過來的固定檔
    session$setInputs(`data-waves` = "M03 6", `data-round_label` = "")
    d <- data()
    expect_equal(d$fixed$version$rounds, "R1")
    expect_equal(d$fixed$bank, first$fixed$bank)
    expect_equal(d$round_label, "R2")
    session$setInputs(`items-run` = 2)
    second <- items()
    expect_true(second$ok)
    calib <- dplyr::filter(second$log, .data$check == "calibration")
    expect_match(calib$detail, "固定校準")
    old <- dplyr::filter(second$fixed$bank, .data$round == "R1")
    expect_equal(old, first$fixed$bank)
    expect_equal(second$fixed$version$rounds, c("R1", "R2"))

    # 改用上傳的舊固定檔（只到 R1）：這次連線已經做到 R2 以後，橘色提醒
    session$setInputs(`score-next_round` = 2, `data-waves` = character())
    expect_null(output$`data-notes`)
    session$setInputs(`data-fixed_clear` = 1)
    expect_match(output$`data-fixed_choice`$html, "fixed_file")
    expect_match(output$`data-notes`$html, "現在沒有用固定檔，這次連線已經做到第 R2 回合")
    zip <- withr::local_tempfile(fileext = ".zip")
    kitsui_write_fixed(first$fixed, zip)
    session$setInputs(`data-fixed_file` = upload_input(zip, "fixed_R1.zip"))
    expect_match(output$`data-notes`$html, "這個固定檔只到第 R1 回合")
  })
})

test_that("第一回合的固定檔（④ 下載）：VERSION 有 kitsUI 的版本與第一回合的 Sources", {
  shiny::testServer(kit_app(), {
    session$setInputs(`data-source` = "demo", `data-domains` = "lan",
                      `data-waves` = "M03 3", `data-round_label` = "")
    session$setInputs(`items-run` = 1)
    html <- output$`score-fixed_dl_ui`$html
    expect_equal(kit_link_attr(html, "score-fixed_dl", "download"), "kitsui_fixed_R1.zip")
    res <- kit_test_download(session, "score-fixed_dl")
    expect_equal(res$content_type, "application/zip")
    expect_equal(res$headers[["Content-Disposition"]], 'attachment; filename="kitsui_fixed_R1.zip"')
    zip <- res$content$file
    dir <- withr::local_tempdir()
    utils::unzip(zip, exdir = dir, junkpaths = TRUE)
    v <- read.dcf(file.path(dir, "VERSION"), keep.white = "Sources")
    expect_equal(unname(v[, "KitsUI"]), kitsui_version())
    expect_false(identical(unname(v[, "KitsUI"]), "dev"))
    expect_match(v[, "Sources"], "R1 [0-9a-f]{32} data")
    back <- kitsui_read_fixed(zip)
    expect_equal(back$sources$round, "R1")
  })
})

test_that("一次上傳好幾個檔案（含 .csv.gz）；欄位不一樣時說是哪一個檔案；範例檔可以下載", {
  dir <- withr::local_tempdir()
  a <- file.path(dir, "m03.csv")
  b <- file.path(dir, "m36.csv.gz")
  readr::write_csv(kitsui_demo("M03", c(3, 6), "lan"), a)
  readr::write_csv(kitsui_demo("M36", 36, "lan"), b)
  bad <- file.path(dir, "bad.csv")
  readr::write_csv(dplyr::select(kitsui_demo("M36", 36, "lan"), -"sex"), bad)
  files <- rbind(upload_input(a, "m03.csv"), upload_input(b, "m36.csv.gz"))
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", file = files, round_label = "")
    # 預設都不勾；勾選框有兩個世代
    expect_null(session$getReturned()())
    expect_match(output$pick_ui$html, 'value="M36 36"')
    session$setInputs(waves = c("M03 3", "M36 36"))
    d <- session$getReturned()()
    expect_equal(d$data_file, c("m03.csv", "m36.csv.gz"))
    expect_equal(names(d$files), c("m03.csv", "m36.csv.gz"))
    expect_equal(d$scales$scale, c("lan M03", "lan M36"))
    expect_match(output$upload_info$html, "讀進 2 個檔案")
    expect_match(output$upload_info$html, "匯入完成")
    expect_match(output$code, 'read_csv\\(c\\(')
    expect_match(output$dl_example_ui$html, 'download="kitsui_example.csv"')
    expect_match(output$dl_example_ui$html, "下載欄位範例檔（CSV）")
    ex <- readr::read_csv(kit_test_download_file(session, "dl_example"), show_col_types = FALSE)
    expect_named(ex, KITSUI_COLUMNS$column)
    session$setInputs(file = rbind(upload_input(a, "m03.csv"), upload_input(bad, "bad.csv")))
    expect_null(session$getReturned()())
    expect_match(output$problems$html, "bad.csv 的欄位和 m03.csv 不一樣")
  })
})

test_that("量尺表與說明：借用另一世代題庫時寫出借誰的、分數怎麼換算", {
  bc <- borrow_chain()
  zip <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(bc$fixed, zip)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "motor", waves = "M03 60",
                      round_label = "", fixed_file = upload_input(zip, "fixed_1.zip"))
    d <- session$getReturned()()
    expect_true(d$plan$borrowed)
    expect_match(output$scales, "固定校準（借用另一世代題庫：motor M36）")
    expect_match(output$borrowed$html, "借用另一世代題庫")
    expect_match(output$borrowed$html, "motor M36 → motor M03 → D")
  })
  plan <- tibble::tibble(data_scale = "motor M03", wave_m = c(48, 60),
                         scale = c("motor M03", "motor M36"), borrowed = c(FALSE, TRUE))
  sc <- tibble::tibble(scale = "motor M03", n_visits = 1, n_items = 1, waves = "48, 60")
  tab <- data_scale_table(sc, bc$fixed, plan)
  expect_equal(tab$calibration, "固定校準（60 個月借用另一世代題庫：motor M36；其他波次用自己的題庫）")
})

test_that("勾世代與波次時停手 1 秒才檢查", {
  withr::local_options(kitsui.waves_delay = 1000)
  shiny::testServer(mod_data_server, args = list(goto = \(x) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "")
    session$elapse(1001)
    expect_equal(session$returned()$waves, 3)
    session$setInputs(waves = c("M03 3", "M03 6"))
    expect_equal(session$returned()$waves, 3)
    session$elapse(1001)
    expect_equal(session$returned()$waves, c(3, 6))
  })
})

test_that("世代 × 波次的勾選框：一個世代一列，勾著的有 checked", {
  ch <- kitsui_pick_choices(kitsui_demo("M36", c(36, 48), "lan"))
  html <- as.character(data_pick_input("x", ch, "M36 48"))
  expect_match(html, "shiny-input-checkboxgroup")
  expect_match(html, 'name="x" value="M36 36"/>')
  expect_match(html, 'value="M36 48" checked')
  expect_equal(lengths(regmatches(html, gregexpr("kit-pick-row", html))), 1)
})

test_that("頁面：「加一個新的波次」預設收起來；欄位表只有一張；下載鈕寫（CSV）（HTML）", {
  html <- as.character(mod_data_ui("data"))
  guides <- regmatches(html, gregexpr("<details class=\"kit-guide\"[^>]*>", html))[[1]]
  expect_length(guides, 2)
  expect_false(any(grepl("open", guides)))
  expect_equal(lengths(regmatches(html, gregexpr("kit-columns", html))), 1)
  # 下載鈕在 server 端畫（字寫在 mod_data_server()，見下載的測試）
  expect_match(html, 'id="data-dl_data_ui"')
  expect_match(html, 'id="data-dl_example_ui"')
  expect_false(grepl("（csv）|（html）|固定集|漂移|人次", html))
})

test_that("作答設計圖：通過比例照固定檔的通過步驟；有不是 1 的題時一句說明", {
  fixed <- kitsui_fixed_empty()
  fixed$pass_steps <- tibble::tibble(item_id = "lana01", step = 2L, round = "R1")
  handover <- shiny::reactiveVal(list(fixed = fixed, round = "R1", n = 1))
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL, handover = handover), {
    session$setInputs(source = "demo", domains = "lan", round_label = "")
    session$setInputs(waves = c("M03 3", "M03 6"), design_scale = "lan M03")
    expect_match(output$design_steps$html, "1 題的通過步驟不是 1：lana01（2 分以上）")
    b <- plotly::plotly_build(design_plot())$x$data[[1]]
    r <- design_resp()
    want <- tapply(r$lana01 >= 2, r$wave_m, mean, na.rm = TRUE)
    expect_equal(unname(b$z[, which(b$x == "lana01")]), unname(as.numeric(want)))
    # 作答本身不改（kits_check 用的）
    expect_true(any(r$lana01 >= 2, na.rm = TRUE))
  })
  # 沒有固定檔（示範資料）：每題都是 1，沒有說明
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", round_label = "")
    session$setInputs(waves = "M03 3", design_scale = "lan M03")
    expect_null(output$design_steps)
    b <- plotly::plotly_build(design_plot())$x$data[[1]]
    r <- design_resp()
    expect_equal(unname(b$z[, which(b$x == "lana01")]), mean(r$lana01 >= 1, na.rm = TRUE))
  })
})

test_that("波次的單位：第一回合在 ① 選，交給 ②；有之前回合的固定檔時只顯示", {
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "",
                      wave_unit = "years")
    expect_match(output$unit_ui$html, "波次（wave_m）的單位")
    expect_equal(session$getReturned()()$fixed$version$wave_unit, "years")
    expect_equal(output$wave_hint, "數字是年。")
  })
  expect_match(data_unit_text("waves"), "波次序號（來自固定檔")
  expect_match(data_unit_text("waves"), "不算 age50")
  expect_false(grepl("age50", data_unit_text("months")))
})
