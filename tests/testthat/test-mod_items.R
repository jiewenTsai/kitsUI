# ② 題目校準：跑一回合、每個量尺的圖表、停下來的量尺；整個 app 能啟動

# 和 mod_data_server() 回傳的一樣
items_data <- function(df, fixed = kitsui_fixed_empty(), label = "3") {
  list(df = df, fixed = fixed, round_label = label, files = NULL, source = "demo",
       data_file = NULL, fixed_file = NULL, waves = sort(unique(df$wave_m)),
       scales = kitsui_scales(df), plan = kitsui_round_plan(df, fixed))
}

test_that("示範資料跑一回合：回傳 items，圖、表、程式都出得來", {
  pandoc_ok <- kit_test_local_pandoc()
  d <- items_data(kitsui_demo("M03", 3, "lan"))
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    expect_null(session$getReturned()())
    session$setInputs(run = 1, scale = "lan M03", plot_type = "category")
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_named(res$items, "lan M03")
    expect_s3_class(res$items[["lan M03"]], "kits_item")
    expect_match(output$status$html, "完成")
    expect_match(output$scale_info$html, "自由校準")
    expect_match(output$fit, "lana01")
    for (type in c("category", "wright", "fit", "observed")) {
      session$setInputs(plot_type = type)
      expect_s3_class(plot_obj(), "plotly")
    }
    expect_match(output$code, "kits_item\\(X, scale")
    expect_match(output$code, "kits version")
    expect_match(output$next_step$html, "go_link")
    # 題目參數：每一題每一步，畫面與 csv 相同
    expect_match(output$coef, "threshold_se")
    coef_csv <- kit_test_dl_csv(output, session, "dl_coef")
    expect_named(coef_csv, c("scale", "item_id", "item_score", "difficulty", "difficulty_se",
                             "threshold", "threshold_se"))
    expect_equal(unique(coef_csv$scale), "lan M03")
    # csv 保留完整的數字
    expect_equal(coef_csv$difficulty, coef(res$items[["lan M03"]])$beta, tolerance = 1e-12)
    expect_equal(nrow(coef_csv), nrow(coef(res$items[["lan M03"]])))
    # 自由校準沒有偏移表：csv 是一列說明，不是下載失敗
    expect_match(kit_test_dl_csv(output, session, "dl_drift")$note, "自由校準")
    expect_gt(nrow(kit_test_dl_csv(output, session, "dl_fit")), 0)
    log_file <- kit_test_dl_file(output, session, "dl_log")
    log_csv <- readr::read_csv(log_file, show_col_types = FALSE)
    expect_true("status" %in% names(log_csv))
    # 沒有題號的列是空的，不是 NA
    expect_false(any(grepl(",NA,", readLines(log_file, encoding = "UTF-8"))))
    expect_true(all(c("scale", "flag") %in%
                      names(kit_test_dl_csv(output, session, "dl_fit"))))
    # 下載鈕：網址最後一段、download 屬性都是檔名；CSV 和 write_excel_csv() 的位元組一樣
    html <- output$dl_coef_ui$html
    expect_match(html, "下載題目參數（csv）")
    expect_equal(kit_link_attr(html, session$ns("dl_coef"), "download"), "item_coef_lan_M03.csv")
    expect_match(kit_link_attr(html, session$ns("dl_coef"), "href"), "/item_coef_lan_M03\\.csv\\?")
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(coef_csv(), old)
    expect_equal(file_bytes(kit_test_dl_file(output, session, "dl_coef")), file_bytes(old))
    expect_equal(kit_test_download_name(session, "dl_log"), "round_log_3.csv")
    # 圖存成 html 要 pandoc：按鈕的檔名跟著圖的種類
    if (pandoc_ok) {
      expect_match(output$dl_plot_ui$html, 'download="item_lan_M03_observed_1-6.html"')
      res <- kit_test_download(session, "dl_plot")
      expect_equal(res$content_type, "text/html; charset=UTF-8")
      expect_match(paste(readLines(res$content$file, n = 5), collapse = ""), "<!DOCTYPE html>",
                   ignore.case = TRUE)
    } else {
      expect_match(output$dl_plot_ui$html, "找不到 pandoc")
    }
  })
})

test_that("有固定檔：偏移把關；沒有題庫的量尺拿掉分數類別有問題的題，照常自由校準", {
  first <- suppressMessages(kitsui_round(kitsui_demo("M03", 3, "lan"), kitsui_fixed_empty(), "3"))
  fixed <- first$fixed
  df <- dplyr::bind_rows(kitsui_demo("M03", 6, "lan"), kitsui_demo("M03", 6, "motor"))
  d <- items_data(df, fixed, "6")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "lan M03", plot_type = "fit")
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_length(res$stopped, 0)
    expect_match(output$status$html, "分數類別有問題")
    expect_match(output$scale_info$html, "固定校準")
    expect_match(output$drift, "suggest_fix")
    expect_match(output$code, "kits_drift")
    session$setInputs(scale = "motor M03")
    expect_match(output$scale_info$html, "自由校準")
    expect_match(output$scale_info$html, "這一回合不放進來：[1-9]")
    expect_match(output$drift, "沒有題庫：自由校準")
  })
})

test_that("還沒選圖的種類時，圖的檔名用第一種", {
  d <- items_data(kitsui_demo("M03", 3, "lan"))
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "lan M03")
    # 3 個月的 lan 只有 6 題
    expect_equal(download_name(plot_name), "item_lan_M03_category_1-6")
    session$setInputs(plot_type = "fit")
    expect_equal(download_name(plot_name), "item_lan_M03_fit")
  })
})

test_that("資料改了，舊結果清掉", {
  d <- shiny::reactiveVal(items_data(kitsui_demo("M03", 3, "lan")))
  shiny::testServer(mod_items_server, args = list(data = d, goto = \(tab) NULL), {
    session$setInputs(run = 1)
    expect_false(is.null(session$getReturned()()))
    d(items_data(kitsui_demo("M03", 3, "motor")))
    session$flushReact()
    expect_null(session$getReturned()())
    expect_match(output$status$html, "重新校準")
  })
})

test_that("整個 app 能啟動：資料頁的預設一路接到題目校準", {
  app <- kit_app()
  expect_s3_class(app, "shiny.appobj")
  expect_s3_class(kit_ui(NULL), "shiny.tag.list")
  shiny::testServer(app, {
    session$setInputs(`data-source` = "demo", `data-cohorts` = "M03", `data-domains` = "lan",
                      `data-waves` = "3", `data-round_label` = "")
    session$setInputs(`items-run` = 1)
    expect_match(output$`items-status`$html, "完成")
  })
})

test_that("借用另一世代題庫：② 顯示校準的量尺（另一個世代的）與說明、程式", {
  bc <- borrow_chain()
  d <- items_data(bc$df2, bc$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "motor M36", plot_type = "fit")
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_equal(scales(), "motor M36")
    expect_match(output$scale_info$html, "借用另一世代題庫：M03 60 months on motor M36 bank")
    expect_match(output$log, "borrowed bank")
    expect_match(output$code, 'keys <- "motor M03 60"')
  })
})

test_that("flag 的量尺（可固定的題少於 5 題）：照做，② 用橘底提醒，紀錄表的 status 是 warn", {
  sb <- sim_bank()
  new <- purrr::set_names(as.list(seq(-1.5, 1.5, length.out = 8)), paste0("b", 1:8))
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 1000), c(sb$betas[1:4], new), prefix = "f",
                       seed = 4)
  d <- items_data(sim_long(X2, 2), sb$r1$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "sim M03", plot_type = "fit")
    expect_true(session$getReturned()()$ok)
    expect_match(output$status$html, "kit-warn-text")
    expect_match(output$status$html, "sim M03（flag）")
    expect_match(output$status$html, "kit-callout-warn")
    expect_match(output$status$html, "被 flag 的量尺照樣往下做；請看偏移的題，必要時在下一回合處理。")
    expect_match(output$scale_info$html, "flag：")
    expect_match(output$log, "warn")
    expect_match(output$overview, "可固定的題少於 5 題")
    expect_match(output$code, "n_fixed < 5")
    expect_match(output$next_step$html, "go_link")
  })
})

test_that("用哪一個題庫：表上有規則選的與原因；手動改成自己的題庫時照選的做，記下來", {
  bc <- borrow_chain()
  d <- items_data(bc$df2, bc$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    html <- output$banks$html
    expect_match(html, "why_1")
    expect_equal(output$why_1, "M03 題庫沒有校準過 60 月；M36 題庫有（36–96 月）")
    expect_match(html, "motor M36（借用另一世代題庫）")
    expect_null(bank_choice())
    session$setInputs(bank_1 = "motor M03")
    expect_equal(bank_choice(), c("motor M03" = "motor M03"))
    session$setInputs(run = 1, scale = "motor M03", plot_type = "fit")
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_named(res$items, "motor M03")
    expect_true(res$plan$manual)
    choice <- dplyr::filter(res$fixed$checks, check == "bank choice")
    expect_equal(choice$detail, "M03 60 months on motor M03 bank; manual choice (rule: motor M36)")
    expect_match(output$code, "手動選了題庫")
    expect_match(output$overview, "motor M03 60")
  })
})

test_that("固定校準：偏移表的 reason 是中文；固定的步驟寫「固定」，csv 是數字並有 scale 欄", {
  pb <- poly_bank()
  X2 <- poly_round2()
  X2[X2[, "p1"] == 0, "p1"] <- 1L
  X2[X2[, "n1"] == 0, "n1"] <- 1L
  d <- items_data(sim_long(X2, 2), pb$r1$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "sim M03", plot_type = "category")
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_match(output$status$html, "有 2 題分數類別有問題")
    expect_match(output$status$html, "分數都沒有改")
    expect_match(output$scale_info$html, "分數類別有問題、固定在題庫的值：1 題（p1）")
    expect_match(output$scale_info$html, "分數類別有問題、這一回合不放進來：1 題（n1）")
    # 偏移表：p1 沒有交給 kits_drift()，也列出來，原因是中文
    rows <- drift_rows()
    expect_equal(rows$reason_zh[rows$item_id == "p1"],
                 "p1 沒有人得 0 分（分數 1、2、3）：不判斷偏移，固定在題庫的值")
    expect_match(output$drift, "不判斷偏移")
    expect_false(grepl("category problem", output$drift))
    drift_csv <- kit_test_dl_csv(output, session, "dl_drift")
    expect_true(all(c("scale", "reason", "reason_zh") %in% names(drift_csv)))
    expect_true("category problem, kept fixed" %in% drift_csv$reason)
    # 題目參數：固定的步驟寫「固定」；csv 是 0
    expect_match(output$coef, "固定")
    coef_csv <- kit_test_dl_csv(output, session, "dl_coef")
    expect_equal(coef_csv$difficulty_se[coef_csv$item_id == "p1"], c(0, 0, 0))
    expect_equal(unique(coef_csv$scale), "sim M03")
    expect_match(output$log, "category problem, left out")
  })
})

test_that("停下來的量尺：偏移表不說「沒有題庫」", {
  sb <- sim_bank()
  d <- items_data(sim_long(one_item_each(), 2), sb$r1$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "sim M03")
    expect_false(session$getReturned()()$ok)
    expect_match(output$drift, "這個量尺停下來了，沒有偏移表")
    expect_false(grepl("沒有題庫", output$drift))
    expect_match(output$status$html, "sim M03</strong>\\s*這一回合資訊不足，無法估計")
    expect_match(output$status$html, "不能產生新的固定檔")
    expect_match(output$scale_info$html, "停下來")
    expect_null(output$next_step)
  })
})

test_that("這一回合不做的量尺：選了再校準；停下來時按鈕改成不做它、再校準一次", {
  sb <- sim_bank()
  df <- dplyr::bind_rows(sim_long(one_item_each(), 2), kitsui_demo("M03", 3, "motor"))
  d <- items_data(df, sb$r1$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    expect_match(output$exclude_ui$html, "sim M03")
    session$setInputs(run = 1)
    expect_equal(session$getReturned()()$stopped, "sim M03")
    expect_match(output$status$html, "這一回合不做 sim M03，再校準一次")
    expect_match(output$status$html, "按「這一回合不做」跳過這個量尺；題庫與連結不會改變。")
    session$setInputs(skip_stopped = 1)
    res <- session$getReturned()()
    expect_true(res$ok)
    expect_equal(res$excluded, "sim M03")
    expect_match(output$status$html, "這一回合不做：")
    # 資訊不足而跳過：摘要寫出原因，checks.csv 也是
    expect_match(output$status$html, "sim M03 這一回合資訊不足，無法估計（每個列次只剩一題可以估計），已跳過")
    expect_match(res$fixed$checks$detail[res$fixed$checks$check == "excluded"],
                 "^insufficient information to estimate")
    expect_match(output$overview, "這一回合不做（資訊不足，無法估計")
    # 再校準一次（仍然勾著）：原因還在
    session$setInputs(exclude = "sim M03", run = 3)
    res <- session$getReturned()()
    expect_match(res$fixed$checks$detail[res$fixed$checks$check == "excluded"],
                 "^insufficient information to estimate")
    # 直接勾「這一回合不做」再校準
    session$setInputs(exclude = "motor M03", run = 2)
    expect_equal(session$getReturned()()$excluded, "motor M03")
  })
})

test_that("不能判斷偏移的量尺：照常校準，偏移表與圖說明原因", {
  sb <- sim_bank()
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 20), sb$betas, prefix = "k", seed = 7)
  d <- items_data(sim_long(X2, 2), sb$r1$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "sim M03")
    expect_true(session$getReturned()()$ok)
    expect_match(output$drift, "沒有偏移表：這一回合上下各有 30 人的步驟")
    expect_match(output$scale_info$html, "不能判斷偏移、固定在題庫的值：12 題")
    expect_match(output$fixed_status$html, "參數和題庫")
  })
})

test_that("選題：預設前 9 題，換頁、自己選；圖上面說畫了哪幾題，下載的檔名也是", {
  ids <- paste0("i", sprintf("%02d", 1:20))
  expect_equal(items_shown(ids, NULL), ids[1:9])
  expect_equal(items_shown(ids, c("i12", "i10")), c("i10", "i12"))
  expect_equal(items_shown(ids, "x"), ids[1:9])
  expect_equal(items_page_ids(ids, 3), ids[19:20])
  expect_equal(items_shown_text(ids, ids[1:9]), "這張圖畫了第 1–9 題（共 20 題）。")
  expect_equal(items_shown_text(ids, ids[19:20]), "這張圖畫了第 19–20 題（共 20 題）。")
  expect_equal(items_shown_text(ids, ids[c(1, 3)]), "這張圖畫了 2 題（共 20 題）：i01、i03。")
  expect_equal(items_shown_label(ids, ids[10:18]), "10-18")
  expect_equal(items_shown_label(ids, ids[c(1, 3, 5)]), "1_3_5")
  expect_equal(items_shown_label(ids, ids[seq(1, 20, 2)]), "1_3_5_7_9_11_13_15_17_19")
  expect_equal(items_shown_label(ids, ids[c(seq(1, 20, 2), 20)]), "11items")
  html <- as.character(items_picker_ui(shiny::NS("x"), ids, ids[1:9]))
  expect_match(html, "第 1–9 題")
  expect_match(html, "第 10–18 題")
  expect_match(html, "第 19–20 題")
  betas <- purrr::set_names(as.list(seq(-2, 2, length.out = 20)), sprintf("s%02d", 1:20))
  X <- kits::kits_sim(seq(-3, 3, length.out = 400), betas, seed = 21)
  d <- items_data(sim_long(X))
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "sim M03", plot_type = "category")
    n <- length(plot_ids())
    expect_equal(n, 20)
    expect_equal(shown(), plot_ids()[1:9])
    expect_match(output$plot_note$html, sprintf("這張圖畫了第 1–9 題（共 %d 題）", n))
    expect_match(output$plot_items_ui$html, "第 10–18 題")
    session$setInputs(plot_items = plot_ids()[10:18])
    expect_equal(download_name(plot_name), "item_sim_M03_category_10-18")
    # 換了題：下載鈕重畫，檔名是畫的題（按下載時寫的也是這張圖）
    if (kit_pandoc_ok()) {
      expect_match(output$dl_plot_ui$html, 'download="item_sim_M03_category_10-18.html"')
    }
    expect_match(output$plot_note$html, "第 10–18 題")
    p <- plot_obj()
    titles <- unlist(purrr::map(p$x$layout$annotations, "text"))
    expect_true(plot_ids()[10] %in% titles)
    expect_false(plot_ids()[1] %in% titles)
    session$setInputs(plot_type = "observed")
    expect_equal(download_name(plot_name), "item_sim_M03_observed_10-18")
    expect_s3_class(plot_obj(), "plotly")
    session$setInputs(plot_type = "fit")
    expect_null(output$plot_items_ui)
    expect_equal(download_name(plot_name), "item_sim_M03_fit")
  })
})

test_that("Wright map 與觀察圖用同一份分數（round$memo），只算一次", {
  d <- items_data(kitsui_demo("M03", 3, "lan"))
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "lan M03", plot_type = "wright")
    expect_s3_class(plot_obj(), "plotly")
    res <- session$getReturned()()
    expect_true(exists("own_scores:lan M03", envir = res$memo))
    first <- get("own_scores:lan M03", envir = res$memo)
    session$setInputs(plot_type = "observed")
    expect_s3_class(plot_obj(), "plotly")
    expect_identical(get("own_scores:lan M03", envir = res$memo), first)
    expect_identical(scores(), first)
  })
})

test_that("適配度圖：infit、outfit 的顏色各看自己；圖例不會把 infit 好的點叫 underfit", {
  fit <- data.frame(item_id = c("a", "b", "c"), n = 100L, pvalue = 0.5, rir = 0.3,
                    infit = c(1.0, 1.0, 1.6), outfit = c(1.0, 1.8, 1.0), flag = "ok")
  class(fit) <- c("kits_itemfit", class(fit))
  p <- items_fit_plot(fit)
  traces <- purrr::map(p$x$data, \(tr) list(name = tr$name, x = as.character(unlist(tr$x))))
  names <- purrr::map_chr(traces, "name")
  at <- \(name) traces[[match(name, names)]]$x
  expect_setequal(at("infit: ok"), c("a", "b"))
  expect_equal(at("infit: underfit"), "c")
  expect_setequal(at("outfit: ok"), c("a", "c"))
  expect_equal(at("outfit: underfit"), "b")
  # kits 自己的圖：b 的 infit 是 1.0，卻在「infit: underfit」裡
  q <- plot(fit)
  qn <- purrr::map_chr(q$x$data, "name")
  expect_true("b" %in% as.character(unlist(q$x$data[[match("infit: underfit", qn)]]$x)))
})

test_that("選題庫的表：why 欄跟著選單變", {
  bc <- borrow_chain()
  d <- items_data(bc$df2, bc$fixed, "2")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$flushReact()
    expect_equal(output$why_1, "M03 題庫沒有校準過 60 月；M36 題庫有（36–96 月）")
    session$setInputs(bank_1 = "motor M03")
    expect_equal(output$why_1, "手動選擇：用 motor M03 的題庫（規則是 motor M36）")
  })
})

test_that("紀錄的 csv：有 status；沒有題號是空字串；完成的句子照量尺數", {
  log <- tibble::tibble(round = "1", scale = "a", item_id = c(NA, "x"),
                        check = c("flag", "drift"), detail = c("d1", "d2"))
  out <- items_log_csv(log)
  expect_equal(out$item_id, c("", "x"))
  expect_equal(out$status, c("warn", "ok"))
  one <- list(items = list(a = 1), ok = TRUE, log = log[0, ])
  expect_match(as.character(items_status(one)), "這個量尺校準好了")
  two <- list(items = list(a = 1, b = 2), ok = TRUE, log = log[0, ])
  expect_match(as.character(items_status(two)), "2 個量尺都校準好了")
})

test_that("固定了哪些題：狀態、核對、全部固定的題、csv；偏移把關圖與程式", {
  fixed <- lan_chain()[["18"]]$fixed
  d <- items_data(kitsui_demo("M03", 24, "lan"), fixed, "24")
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "lan M03", plot_type = "drift")
    expect_true(session$getReturned()()$ok)
    html <- output$fixed_status$html
    expect_match(html, "這一回合固定 13 題（沿用題庫的參數），換成新版本 2 題")
    expect_match(html, "參數和題庫完全相同（最大差 0）")
    expect_match(html, "lanc03 → lanc03_r24")
    expect_match(html, "看全部固定的題（13 題）")
    expect_false(grepl("alert-danger", html))
    csv <- kit_test_dl_csv(output, session, "dl_status")
    expect_named(csv, c("scale", "item_id", "status", "steps", "detail"))
    expect_equal(nrow(csv), 15)
    expect_equal(csv$status[1], "偏移 → 新版本")
    # 偏移把關圖
    p <- plot_obj()
    expect_s3_class(p, "plotly")
    names <- purrr::map_chr(p$x$data, \(tr) as.character(tr$name)[1])
    expect_true("Drifted -> new version" %in% names)
    expect_match(output$plot_help$html, "落在帶子裡的步驟")
    expect_equal(download_name(plot_name), "item_lan_M03_drift")
    expect_match(output$code, "drift <- kits_drift\\(.*\\)\nplot\\(drift\\)")
  })
})

test_that("自由校準：沒有固定的題，偏移把關圖是一句說明", {
  d <- items_data(kitsui_demo("M03", 3, "lan"))
  shiny::testServer(mod_items_server, args = list(data = shiny::reactive(d), goto = \(tab) NULL), {
    session$setInputs(run = 1, scale = "lan M03", plot_type = "drift")
    expect_match(output$fixed_status$html, "沒有題庫，這個量尺是自由校準，沒有固定的題")
    expect_match(output$fixed_status$html, "看全部自由校準的題（6 題）")
    expect_error(plot_obj(), "沒有偏移把關圖")
    expect_false(grepl("plot\\(drift\\)", output$code))
  })
})
