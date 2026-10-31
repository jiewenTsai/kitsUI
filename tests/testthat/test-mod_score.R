# ④ 受試者評分：round8() 加上做好的連結（linked()）；links 直接由 pipeline 做

score_args <- function(round, df, fixed, persons = NULL) {
  list(
    data = shiny::reactive(list(df = df, fixed = NULL, round_label = "r8", files = NULL,
                                persons = persons)),
    items = shiny::reactive(round),
    links = shiny::reactive(list(fixed = fixed, links = kitsui_links(fixed))),
    goto = function(tab) NULL
  )
}

test_that("分數表有 visit_id、subj_id、round、bank、ability、ability_se、d_score、d_score_se；可搜尋、下載", {
  r8 <- round8()
  lk <- linked()
  args <- score_args(r8$round, r8$df, lk$fixed, kitsui_persons(r8$df))
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    s <- session$returned()$scores
    expect_equal(nrow(s), dplyr::n_distinct(r8$df$visit_id))
    cols <- c("visit_id", "subj_id", "cohort", "round", "bank", "ability", "ability_se", "d_score",
              "d_score_se",
              "se_link", "se_total")
    expect_true(all(cols %in% names(s)))
    expect_equal(unique(s$scale), "D")
    expect_match(output$routes, "lan M36 → lan M03 → D")
    expect_match(output$routes, "lan M36（36、48 個月）")
    # 重點：換算路線的表（中文欄名）在下載的上面；其他細節收起來
    expect_match(output$routes, "換算路線")
    expect_match(output$done$html, session$ns("routes"))
    expect_match(output$result$html, "<summary>分數對照表</summary>")
    expect_false(grepl("<details[^>]*open", output$result$html))
    # 每一欄一句說明
    expect_match(output$columns$html, "se_total")
    expect_match(output$columns$html, "量尺分數（d_score）")

    id <- s$subj_id[1]
    session$setInputs(search = id, page = 1)
    expect_true(all(grepl(id, shown()$subj_id) | grepl(id, shown()$visit_id)))
    expect_match(output$scores, id)
    # 分數表的下載：網址最後一段是檔名；位元組和 write_excel_csv() 一樣（全部的列）
    html <- output$scores_dl_ui$html
    expect_match(html, "下載這一回合的分數表（CSV）")
    expect_equal(kit_link_attr(html, session$ns("scores_dl"), "download"), "scores_round_r8.csv")
    expect_match(kit_link_attr(html, session$ns("scores_dl"), "href"), "/scores_round_r8\\.csv\\?")
    res <- kit_test_download(session, "scores_dl")
    expect_equal(res$content_type, "text/csv; charset=UTF-8")
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(s, old)
    expect_equal(file_bytes(res$content$file), file_bytes(old))
    csv <- readr::read_csv(res$content$file, show_col_types = FALSE)
    expect_equal(nrow(csv), nrow(s))
    expect_equal(csv$visit_id, s$visit_id)
    expect_equal(csv$d_score, s$d_score)
    expect_equal(csv$ability, s$ability)
    expect_true(all(c("ability", "ability_se", "d_score", "d_score_se") %in% names(csv)))

    # 分數對照表：ability、ability_linked、d_score；預設不列沒有人的原始分數
    session$setInputs(subj_scale = "lan M36")
    tab <- subj_table()
    expect_true(all(c("wave_m", "raw", "ability", "ability_linked", "d_score", "d_score_se",
                      "se_total") %in%
                      names(tab)))
    expect_true(any(tab$n == 0))
    shown_n <- nrow(kitsui_raw_table_shown(tab))
    expect_equal(lengths(regmatches(output$subj_table, gregexpr("<tr", output$subj_table))),
                 shown_n + 1)
    session$setInputs(show_empty = TRUE)
    expect_equal(lengths(regmatches(output$subj_table, gregexpr("<tr", output$subj_table))),
                 nrow(tab) + 1)
    # 下載的是完整的表
    raw_csv <- kit_test_dl_file(output, session, "subj_table_dl")
    expect_equal(nrow(readr::read_csv(raw_csv, show_col_types = FALSE)), nrow(tab))
    expect_equal(kit_test_download_name(session, "subj_table_dl"), "raw_table_lan_M36.csv")
    html <- output$result$html
    expect_match(html, "顯示沒有人的原始分數")
    expect_match(html, "作答組合")
    expect_match(html, "form_pct")
    expect_match(html, "cum_pct")
    expect_match(html, "wave_n")
    expect_match(html, "kits_norms")
    expect_match(html, session$ns("subj_table_dl_ui"))
    expect_match(output$subj_table_dl_ui$html, "下載對照表（CSV）")

    # R 程式：從新的固定檔（真的檔名）重建連結，對照表直接換算
    code <- output$code
    expect_match(code, "kits_convert\\(scores, link_lan_M36, link_D\\)")
    expect_match(code, "kits_convert\\(raw_table, link_lan_M36, link_D\\)")
    expect_match(code, 'unzip\\("kitsui_fixed_r8.zip"')
    expect_false(grepl("fixed_set.zip", code))
    expect_match(code, "kits_item_table")

    # 題目表：換到 D，通過年齡用這一回合的作答；沒有內容的欄不列出
    session$setInputs(item_scale = "lan M36")
    expect_s3_class(item_table(), "data.frame")
    expect_true(all(c("item_id", "d_threshold", "age50") %in% names(item_table())))
    expect_false(any(c("domain", "column", "stem", "DIF") %in% names(item_table())))
    expect_match(output$item_table_note$html, "kits_dif")
    expect_match(output$item_table_scale$html, "題目依 d_threshold 由小到大")
    expect_true(all(KITSUI_ITEM_THRESHOLDS %in% names(item_table())) ||
                  all(c("ability_threshold", "d_threshold") %in% names(item_table())))
    expect_match(output$item_table_dl_ui$html, "下載題目表（CSV）")
    item_csv <- kit_test_dl_file(output, session, "item_table_dl")
    csv <- readr::read_csv(item_csv, show_col_types = FALSE)
    expect_equal(csv$d_threshold, item_table()$d_threshold)
    expect_equal(kit_test_download_name(session, "item_table_dl"), "item_table_lan_M36.csv")
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(item_table(), old)
    expect_equal(file_bytes(item_csv), file_bytes(old))
    expect_match(output$checks, "share of misfit")
    expect_match(output$checks, "explain")
  })
})

test_that("算完分數後：下載分數表與新的固定檔放在一起；zip 讀得回來，連結一模一樣", {
  r8 <- round8()
  lk <- linked()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    expect_null(output$done)
    session$setInputs(run = 1)
    html <- output$done$html
    expect_match(html, "scores_dl")
    expect_match(html, "fixed_dl")
    expect_match(html, "不會重算")
    expect_match(html, "kitsui_fixed_r8.zip")
    expect_match(html, "下一回合要上傳")
    # 新的固定檔：網址最後一段、標頭、download 屬性都是 kitsui_fixed_r8.zip
    html <- output$fixed_dl_ui$html
    expect_match(html, "下載新的固定檔（zip）")
    expect_equal(kit_link_attr(html, session$ns("fixed_dl"), "download"), "kitsui_fixed_r8.zip")
    expect_match(kit_link_attr(html, session$ns("fixed_dl"), "href"), "/kitsui_fixed_r8\\.zip\\?")
    res <- kit_test_download(session, "fixed_dl")
    expect_equal(res$content_type, "application/zip")
    expect_equal(res$headers[["Content-Disposition"]], 'attachment; filename="kitsui_fixed_r8.zip"')
    zip_path <- res$content$file
    old <- withr::local_tempfile(fileext = ".zip")
    kitsui_write_fixed(fixed_out(), old)
    expect_equal(zip_bytes(zip_path), zip_bytes(old))
    back <- kitsui_read_fixed(zip_path)
    expect_equal(back$bank, lk$fixed$bank)
    expect_equal(back$links, lk$fixed$links)
    expect_equal(coef(kitsui_links(back)$D), coef(lk$D))
  })
})

test_that("用這一回合的固定檔開始下一回合：把含連結的固定檔交出去", {
  r8 <- round8()
  lk <- linked()
  got <- NULL
  args <- score_args(r8$round, r8$df, lk$fixed)
  args$next_round <- function(fixed, round) got <<- list(fixed = fixed, round = round)
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    expect_match(output$done$html, "用這一回合的固定檔開始下一回合")
    expect_match(output$next_step$html, "下一步：用這一回合的固定檔開始下一回合 →")
    expect_match(output$status$html, "【完成】")
    session$setInputs(next_round = 1)
    expect_identical(got$fixed, lk$fixed)
    expect_equal(got$round, "r8")
  })
})

test_that("有量尺停下來時不能下載固定檔，紅字說明（分數表照樣能下載）", {
  r8 <- round8()
  round <- r8$round
  round$ok <- FALSE
  round$stopped <- "motor M03"
  attr(round$fixed, "stopped") <- "motor M03"
  shiny::testServer(mod_score_server, args = score_args(round, r8$df, round$fixed), {
    session$setInputs(run = 1)
    html <- output$done$html
    expect_match(html, "text-danger")
    expect_match(html, "motor M03")
    expect_match(html, "disabled")
    expect_match(html, "scores_dl")
    expect_false(grepl("next_round", html))
    expect_null(output$next_step)
    expect_match(output$status$html, "【完成，但請注意】")
    expect_match(output$status$html, "不能下載新的固定檔")
    # 鈕不能按；就算直接打開網址，也是 500 與一句說明，不寫出固定檔
    expect_false(grepl("dataobj", html))
    output$fixed_dl_ui
    expect_message(res <- kit_test_download(session, "fixed_dl"), "停下來")
    expect_equal(res$status, 500)
    expect_match(res$content, "下載失敗")
  })
})

test_that("② 的 flag 帶到 ④：橘色提醒，說明是什麼意思", {
  r8 <- round8()
  round <- r8$round
  round$log <- dplyr::bind_rows(round$log, tibble::tibble(
    round = "r8", scale = "lan M36", item_id = NA, check = "flag",
    detail = "偏移 5 題（已判斷 12 題）（偏移題太多），照常校準"
  ))
  shiny::testServer(mod_score_server, args = score_args(round, r8$df, round$fixed), {
    html <- output$flags$html
    expect_match(html, "kit-callout-warn")
    expect_match(html, "kit-warn-text")
    expect_match(html, "lan M36（flag）")
    expect_match(html, "偏移的題比較多")
  })
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, r8$round$fixed), {
    expect_null(output$flags)
  })
})

test_that("連結的模組還沒有回傳時，用題目校準的固定檔（沒有連結，分數留在原量尺）", {
  r8 <- round8()
  args <- score_args(r8$round, r8$df, r8$round$fixed)
  args$links <- shiny::reactive(NULL)
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    s <- session$returned()$scores
    expect_setequal(unique(s$scale), c("lan M36", "motor M03"))
    expect_true(all(c("ability", "ability_se") %in% names(s)))
    session$setInputs(subj_scale = "lan M36")
    # 沒有連結：R 程式不讀固定檔，也不出錯
    expect_false(grepl("unzip", output$code))
  })
})

test_that("在 ③ 做了新的連結：分數不重算，只用新的連結重新換算", {
  r8 <- round8()
  lk <- linked()
  fx <- shiny::reactiveVal(r8$round$fixed)
  args <- score_args(r8$round, r8$df, r8$round$fixed)
  args$links <- shiny::reactive(list(fixed = fx(), links = kitsui_links(fx())))
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    first <- own()
    expect_setequal(unique(session$returned()$scores$scale), c("lan M36", "motor M03"))
    fx(lk$fixed)
    session$flushReact()
    expect_identical(own(), first)
    expect_equal(unique(session$returned()$scores$scale), "D")
    expect_match(output$status$html, "沒有重算分數")
    # 按「計算」時也沿用校準量尺上的分數
    session$setInputs(run = 2)
    expect_identical(own(), first)
  })
})

test_that("固定檔是 NULL 時，R 程式不出錯（測試者看到的 distinct() 錯誤）", {
  expect_identical(kitsui_link_path(NULL, "lan M36"), character())
  code <- score_code(NULL, "lan M36", "M36", "kitsui_fixed_r8.zip")
  expect_match(code, "kits_subj\\(X, item")
  expect_false(grepl("unzip", code))
})

test_that("還沒有題目校準時，提醒並可以回到 ②", {
  r8 <- round8()
  went <- NULL
  args <- score_args(NULL, r8$df, r8$round$fixed)
  args$goto <- function(tab) went <<- tab
  shiny::testServer(mod_score_server, args = args, {
    expect_match(output$source$html, "go_items")
    session$setInputs(go_items = 1)
    expect_equal(went, "items")
  })
})

# 通過步驟的設定檔（fileInput 的格式）
steps_upload <- function(dir, item_id, step, name = "steps.csv") {
  path <- tempfile(fileext = ".csv", tmpdir = dir)
  readr::write_csv(tibble::tibble(item_id = item_id, step = step), path)
  data.frame(name = name, size = file.size(path), type = "text/csv", datapath = path)
}

# 通過步驟的收合區塊：<details ...> 的開頭那一段
steps_details_tag <- function(html) {
  regmatches(html, regexpr("<details class=\"kit-advanced\"[^>]*>", html))
}

test_that("通過步驟：收合區塊預設收起來，標題寫每題都是 1；裡面依序是說明、選題目、表、用檔案設定", {
  r8 <- round8()
  lk <- linked()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    session$setInputs(run = 1)
    html <- output$advanced$html
    tag <- steps_details_tag(html)
    expect_length(tag, 1)
    expect_false(grepl("open", tag))
    expect_match(html, "通過步驟（每題都是 1）")
    expect_equal(output$steps_summary, "通過步驟（每題都是 1）")
    at <- \(x) regexpr(x, html, fixed = TRUE)[1]
    expect_true(at("得分至少幾分算通過。預設每題都是 1。") > at("kit-advanced"))
    expect_match(html, "進階設定：")
    expect_match(html, "只影響題目表的門檻與 age50；校準、連結、分數都不受影響。")
    expect_true(at("steps_picker") < at("steps_table"))
    expect_true(at("steps_table") < at("用檔案設定（題目很多時）"))
    expect_true(at("用檔案設定（題目很多時）") < at("steps_file"))
    # 清單：這一回合每個量尺的題，依量尺分組；標籤帶目前的 step 與最高分數
    picker <- output$steps_picker$html
    expect_match(picker, "lanb08（目前 1，最高 3 分）")
    expect_match(picker, "套用到選的題")
    expect_match(picker, "依量尺分組")
    expect_match(output$steps_table$html, "目前每一題都是 1。")
    # 每一題都是 1 時沒有「全部改回 1」
    expect_false(grepl("steps_reset", output$steps_table$html))
  })
})

test_that("通過步驟：選兩題套用 2，題目表、R 程式、固定檔、交給下一回合的都照它", {
  r8 <- round8()
  lk <- linked()
  got <- NULL
  args <- score_args(r8$round, r8$df, lk$fixed)
  args$next_round <- function(fixed, round) got <<- fixed
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1, item_scale = "lan M36", subj_scale = "lan M36")
    expect_false("step" %in% names(item_table()))
    before <- item_obj()
    session$setInputs(steps_pick = c("lanb08", "lanc02"), steps_value = 2)
    session$setInputs(steps_apply = 1)
    expect_match(output$steps_msg$html, "已把 2 題設成 2：lanb08、lanc02。")
    expect_equal(output$steps_summary, "通過步驟（2 題不是 1：lanb08＝2、lanc02＝2）")
    # 清單的標籤換成新的 step
    expect_match(output$steps_picker$html, "lanb08（目前 2，最高 3 分）")
    # 不是 1 的題：一題一列，每列有「改回 1」，下面有「全部改回 1」
    table <- output$steps_table$html
    expect_match(table, "不是 1 的題")
    expect_match(table, "最高分數")
    expect_equal(lengths(regmatches(table, gregexpr("data-item=", table))), 2)
    expect_match(table, "data-item=\"lanc02\"")
    expect_match(table, "steps_row_reset")
    expect_match(table, "全部改回 1")
    # 題目表與門檻
    tab <- item_table()
    expect_equal(names(tab)[1:2], c("item_id", "step"))
    expect_equal(tab$step[tab$item_id %in% c("lanb08", "lanc02")], c(2, 2))
    co <- coef(r8$round$items[["lan M36"]])
    expect_equal(tab$ability_threshold[tab$item_id == "lanb08"],
                 co$thr[co$item_id == "lanb08" & co$item_score == 2])
    expect_false(isTRUE(all.equal(item_obj()$D, before$D)))
    # R 程式
    expect_match(output$code, "lanb08 = 2")
    expect_match(output$code, "lanc02 = 2")
    expect_match(output$code, "kits_item_table\\(item, data = cbind\\(ages, X\\), step = steps\\)")
    # 新的固定檔與交給下一回合的固定檔
    back <- kitsui_read_fixed(kit_test_dl_file(output, session, "fixed_dl"))
    expect_equal(back$pass_steps$item_id, c("lanb08", "lanc02"))
    expect_equal(back$pass_steps$step, c(2L, 2L))
    expect_equal(unique(back$pass_steps$round), "r8")
    expect_equal(back$links, lk$fixed$links)
    session$setInputs(next_round = 1)
    expect_equal(got$pass_steps, back$pass_steps)
    # 再算一次：區塊打開，標題照樣
    session$setInputs(run = 2)
    expect_match(steps_details_tag(output$advanced$html), "open")
    expect_match(output$advanced$html, "通過步驟（2 題不是 1：lanb08＝2、lanc02＝2）")
  })
})

test_that("通過步驟：太高、不是整數、空白、沒選題目時不改，紅字說明", {
  r8 <- round8()
  lk <- linked()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    session$setInputs(run = 1, item_scale = "lan M36")
    session$setInputs(steps_pick = "lanb08", steps_value = 2, steps_apply = 1)
    expect_equal(steps(), c(lanb08 = 2L))
    try_step <- function(ids, value, n) {
      session$setInputs(steps_pick = ids, steps_value = value, steps_apply = n)
      output$steps_msg$html
    }
    msg <- try_step(c("lanb09", "lanc02"), 4, 2)
    expect_match(msg, "通過步驟沒有套用")
    expect_match(msg, "lanb09（設定 4，最高 3 分）、lanc02（設定 4，最高 3 分）")
    expect_match(msg, "設定沒有改。")
    expect_match(msg, "選好再按「套用到選的題」。")
    expect_match(try_step("lanb09", 1.5, 3), "通過步驟要是 1 以上的整數。現在是 1.5。")
    expect_match(try_step("lanb09", 0, 4), "通過步驟要是 1 以上的整數。現在是 0。")
    expect_match(try_step("lanb09", NA, 5), "通過步驟是空白")
    expect_match(try_step(character(), 2, 6), "請先選題目。")
    # 設定、題目表都沒變
    expect_equal(steps(), c(lanb08 = 2L))
    expect_equal(item_table()$step[item_table()$item_id == "lanb09"], 1)
    expect_equal(output$steps_summary, "通過步驟（1 題不是 1：lanb08＝2）")
  })
})

test_that("通過步驟：每一列改回 1、全部改回 1；最後固定檔原樣", {
  r8 <- round8()
  lk <- linked()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    session$setInputs(run = 1, item_scale = "lan M36", subj_scale = "lan M36")
    before <- item_obj()
    session$setInputs(steps_pick = c("lanb08", "lanb09", "lanc02"), steps_value = 3)
    session$setInputs(steps_apply = 1)
    expect_equal(output$steps_summary, "通過步驟（3 題不是 1：lanb08＝3、lanb09＝3、lanc02＝3）")
    # 一列改回 1
    session$setInputs(steps_row_reset = "lanb09")
    expect_match(output$steps_msg$html, "lanb09 改回 1 了。")
    expect_equal(steps(), c(lanb08 = 3L, lanc02 = 3L))
    expect_false(grepl("data-item=\"lanb09\"", output$steps_table$html))
    expect_equal(item_table()$step[item_table()$item_id == "lanb09"], 1)
    # 4 題以上時標題只列 3 題
    session$setInputs(steps_pick = c("lanc01", "lanb01"), steps_value = 2, steps_apply = 2)
    expect_match(output$steps_summary, "^通過步驟（4 題不是 1：.*、…）$")
    # 全部改回 1
    session$setInputs(steps_reset = 1)
    expect_match(output$steps_msg$html, "已經全部改回 1")
    expect_equal(output$steps_summary, "通過步驟（每題都是 1）")
    expect_match(output$steps_table$html, "目前每一題都是 1。")
    expect_false("step" %in% names(item_table()))
    expect_equal(item_obj()$D, before$D)
    expect_false(grepl("step = steps", output$code))
    expect_identical(fixed_out(), lk$fixed)
  })
})

test_that("通過步驟：用檔案設定（收在裡面）照樣能上傳、下載範本；錯誤只在那裡顯示", {
  r8 <- round8()
  lk <- linked()
  dir <- withr::local_tempdir()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    session$setInputs(run = 1, item_scale = "lan M36", subj_scale = "lan M36")
    # 範本：這一回合的每一題、目前的 step
    tpl <- readr::read_csv(kit_test_dl_file(output, session, "steps_template"),
                           show_col_types = FALSE)
    expect_equal(kit_test_download_name(session, "steps_template"), "pass_steps.csv")
    expect_equal(names(tpl)[1:2], c("item_id", "step"))
    expect_true(all(tpl$step == 1))

    session$setInputs(steps_file = steps_upload(dir, c("lanb08", "cogD01"), c(2, 3)))
    msg <- output$steps_file_msg$html
    expect_match(msg, "已套用 steps.csv：共 2 題")
    expect_match(msg, "校準裡沒有這幾題：cogD01")
    expect_null(output$steps_msg)
    expect_equal(output$steps_summary, "通過步驟（1 題不是 1：lanb08＝2）")
    expect_match(output$steps_table$html, "另有 1 題這一回合沒有.*cogD01（3）")
    expect_equal(item_table()$step[item_table()$item_id == "lanb08"], 2)
    expect_match(output$item_table_note$html, "通過步驟")
    tpl <- readr::read_csv(kit_test_dl_file(output, session, "steps_template"),
                           show_col_types = FALSE)
    expect_equal(tpl$step[tpl$item_id == "lanb08"], 2)
    back <- kitsui_read_fixed(kit_test_dl_file(output, session, "fixed_dl"))
    expect_equal(back$pass_steps$item_id, c("lanb08", "cogD01"))
    expect_equal(back$pass_steps$step, c(2L, 3L))

    # 不合規的檔案：紅字，設定不變
    session$setInputs(steps_file = steps_upload(dir, "lanb08", 7))
    msg <- output$steps_file_msg$html
    expect_match(msg, "通過步驟沒有套用")
    expect_match(msg, "lanb08（設定 7，最高 3 分）")
    expect_match(msg, "設定沒有改。")
    expect_match(msg, "請改好再上傳。")
    expect_equal(item_table()$step[item_table()$item_id == "lanb08"], 2)
    bad <- steps_upload(dir, "lanb08", 2)
    readr::write_csv(tibble::tibble(id = "lanb08"), bad$datapath)
    session$setInputs(steps_file = bad)
    expect_match(output$steps_file_msg$html, "缺少：item_id、step")
  })
})

test_that("通過步驟：預設用這一回合開始時固定檔的設定（新版本沿用）；區塊打開", {
  r8 <- round8()
  lk <- linked()
  round <- r8$round
  saved <- tibble::tibble(item_id = "lanb08", step = 3L, round = "r7")
  round$fixed$pass_steps <- saved
  start <- lk$fixed
  start$pass_steps <- saved
  shiny::testServer(mod_score_server, args = score_args(round, r8$df, start), {
    session$setInputs(run = 1, item_scale = "lan M36")
    expect_equal(item_table()$step[item_table()$item_id == "lanb08"], 3)
    html <- output$advanced$html
    expect_match(steps_details_tag(html), "open")
    expect_match(html, "通過步驟（1 題不是 1：lanb08＝3）")
    expect_match(output$steps_table$html, "data-item=\"lanb08\"")
    expect_match(output$steps_picker$html, "lanb08（目前 3，最高 3 分）")
    # 沒有改：固定檔原樣（回合仍是 r7）
    expect_identical(fixed_out(), start)
    session$setInputs(steps_row_reset = "lanb08")
    back <- kitsui_read_fixed(kit_test_dl_file(output, session, "fixed_dl"))
    expect_equal(back$pass_steps$step, 1L)
    expect_equal(back$pass_steps$round, "r8")
  })
})

test_that("題目表的欄位說明：age50 什麼時候是空的", {
  html <- as.character(item_table_note_ui(data.frame(item_id = "a", age50 = NA)))
  expect_match(html, "至少兩個波次、通過比例在波次之間跨過 50%")
  expect_match(html, "只有一個波次，或每一波都已經超過（或都還沒到）50% 時是空的。")
})

test_that("② 沒有結果了（設定或資料改了、還沒重新校準）：舊的分數不留在畫面上，說要重新計算", {
  r8 <- round8()
  lk <- linked()
  it <- shiny::reactiveVal(r8$round)
  args <- score_args(r8$round, r8$df, lk$fixed)
  args$items <- it
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    expect_false(is.null(session$returned()))
    it(NULL)
    session$flushReact()
    expect_null(session$returned())
    expect_match(output$status$html, "【要重新執行】")
    expect_match(output$status$html, "再按一次「開始校準」")
    it(r8$round)
    session$flushReact()
    expect_null(session$returned())
    expect_match(output$status$html, "請按「計算這一回合的分數」。")
  })
})

test_that("分數摘要：每個量尺 × 世代 × 波次的人數、平均、標準差（中文欄名），先於細節", {
  r8 <- round8()
  lk <- linked()
  shiny::testServer(mod_score_server, args = score_args(r8$round, r8$df, lk$fixed), {
    session$setInputs(run = 1)
    s <- session$returned()$scores
    tab <- kitsui_score_summary(s)
    # 和直接從分數表算的一樣（只摘要，不重算分數）
    by_hand <- dplyr::summarise(s, n = sum(is.finite(d_score)), mean = mean(d_score, na.rm = TRUE),
                                sd = stats::sd(d_score, na.rm = TRUE), .by = c(bank, cohort, wave_m))
    hit <- dplyr::inner_join(tab, by_hand, by = c(scale = "bank", "cohort", "wave_m"))
    expect_equal(nrow(hit), nrow(tab))
    expect_equal(hit$n.x, hit$n.y)
    expect_equal(hit$mean.x, hit$mean.y)
    expect_equal(hit$sd.x, hit$sd.y)
    expect_equal(unique(tab$score_column), "d_score")
    expect_match(output$summary, "<th[^>]*>\\s*人數\\s*</th>")
    expect_match(output$summary, "<th[^>]*>\\s*平均\\s*</th>")
    expect_match(output$summary, "<th[^>]*>\\s*標準差\\s*</th")
    # 摘要在下載與換算路線的前面
    html <- output$done$html
    expect_lt(regexpr("分數摘要", html), regexpr("請現在就存兩個檔案", html))
    expect_lt(regexpr("請現在就存兩個檔案", html), regexpr("每個量尺換到哪一把尺", html))
    # 分數欄是哪一欄：寫在下載鈕旁邊（欄名不改）
    expect_match(html, "分數欄：d_score（D 量尺）")
    # 兩個下載是主要按鈕；「下一回合」是次要按鈕
    expect_match(output$scores_dl_ui$html, "kit-download btn-primary")
    expect_match(output$fixed_dl_ui$html, "kit-download btn-primary")
  })
  # 外部連結不適用：分數欄是 ref_score；還沒有內部連結的量尺留在 ability
  sc <- data.frame(bank = c("a M03", "a M03", "a M36"), cohort = c("M03", "M03", "M36"),
                   wave_m = c(3, 3, 36), ability = c(1, 3, 5), ref_score = c(1, 3, NA))
  tab <- kitsui_score_summary(sc, ref = "M03")
  expect_equal(tab$score_column, c("ref_score", "ability"))
  expect_equal(tab$mean, c(2, 5))
  expect_equal(tab$n, c(2L, 1L))
  expect_true(is.na(tab$sd[2]))
  expect_match(kitsui_score_column_text(sc, "M03"),
               "分數欄：ref_score（參照世代 M03 的量尺）。a M36 還沒有換到這把尺，分數留在 ability")
  expect_named(kitsui_score_summary_display(tab), c("量尺", "世代", "波次", "分數欄", "人數", "平均", "標準差"))
})

test_that("沒有可以評分的量尺（② 全部停下來）：【停下來】與怎麼辦，不是【完成】0 列，也沒有空的下載", {
  r8 <- round8()
  round <- r8$round
  round$ok <- FALSE
  round$stopped <- names(round$items)
  round$items <- list()
  shiny::testServer(mod_score_server, args = score_args(round, r8$df, round$fixed), {
    expect_true(nothing())
    expect_match(output$source$html, "【停下來】")
    session$setInputs(run = 1)
    html <- output$status$html
    expect_match(html, "【停下來】")
    expect_match(html, "沒有可以評分的量尺（② 有 2 個量尺停下來")
    expect_match(html, "怎麼辦：")
    expect_false(grepl("【完成", html))
    expect_null(session$returned())
    expect_null(output$done)
  })
  # 算出來 0 列時也一樣
  html <- as.character(score_status(list(scores = data.frame()), round))
  expect_match(html, "【停下來】")
  expect_false(grepl("0 列", html))
})
