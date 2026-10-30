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

test_that("通過步驟：上傳設定檔，題目表多 step 欄、門檻照它算；寫進新的固定檔；R 程式帶 step", {
  r8 <- round8()
  lk <- linked()
  got <- NULL
  dir <- withr::local_tempdir()
  args <- score_args(r8$round, r8$df, lk$fixed)
  args$next_round <- function(fixed, round) got <<- fixed
  shiny::testServer(mod_score_server, args = args, {
    session$setInputs(run = 1)
    html <- output$result$html
    expect_match(html, "通過步驟")
    expect_match(html, "得分至少幾分算通過。預設每題都是 1。")
    expect_match(html, "全部改回 1")
    session$setInputs(item_scale = "lan M36", subj_scale = "lan M36")
    expect_false("step" %in% names(item_table()))
    expect_match(output$steps_status$html, "每一題都是 1")
    before <- item_obj()

    # 範本：這一回合的每一題、目前的 step
    tpl <- readr::read_csv(kit_test_dl_file(output, session, "steps_template"),
                           show_col_types = FALSE)
    expect_equal(kit_test_download_name(session, "steps_template"), "pass_steps.csv")
    expect_equal(names(tpl)[1:2], c("item_id", "step"))
    expect_true(all(tpl$step == 1))

    session$setInputs(steps_file = steps_upload(dir, c("lanb08", "cogD01"), c(2, 3)))
    status <- output$steps_status$html
    expect_match(status, "已套用 steps.csv：共 2 題")
    expect_match(status, "目前有 1 題不是 1：lanb08（2）")
    expect_match(status, "校準裡沒有這幾題：cogD01")
    expect_match(status, "另有 1 題這一回合沒有")
    tab <- item_table()
    expect_equal(names(tab)[1:2], c("item_id", "step"))
    expect_equal(tab$step[tab$item_id == "lanb08"], 2)
    co <- coef(r8$round$items[["lan M36"]])
    expect_equal(tab$ability_threshold[tab$item_id == "lanb08"],
                 co$thr[co$item_id == "lanb08" & co$item_score == 2])
    expect_false(isTRUE(all.equal(item_obj()$D, before$D)))
    expect_match(output$item_table_note$html, "通過步驟")
    tpl <- readr::read_csv(kit_test_dl_file(output, session, "steps_template"),
                           show_col_types = FALSE)
    expect_equal(tpl$step[tpl$item_id == "lanb08"], 2)
    # R 程式
    expect_match(output$code, "lanb08 = 2")
    expect_match(output$code, "kits_item_table\\(item, data = cbind\\(ages, X\\), step = steps\\)")
    # 新的固定檔與交給下一回合的固定檔
    back <- kitsui_read_fixed(kit_test_dl_file(output, session, "fixed_dl"))
    expect_equal(back$pass_steps$item_id, c("lanb08", "cogD01"))
    expect_equal(back$pass_steps$step, c(2L, 3L))
    expect_equal(unique(back$pass_steps$round), "r8")
    expect_equal(back$links, lk$fixed$links)
    session$setInputs(next_round = 1)
    expect_equal(got$pass_steps, back$pass_steps)

    # 不合規的檔案：紅字，設定不變
    session$setInputs(steps_file = steps_upload(dir, "lanb08", 7))
    status <- output$steps_status$html
    expect_match(status, "通過步驟沒有套用")
    expect_match(status, "lanb08（設定 7，最高 3 分）")
    expect_match(status, "設定沒有改")
    expect_equal(item_table()$step[item_table()$item_id == "lanb08"], 2)
    bad <- steps_upload(dir, "lanb08", 2)
    readr::write_csv(tibble::tibble(id = "lanb08"), bad$datapath)
    session$setInputs(steps_file = bad)
    expect_match(output$steps_status$html, "缺少：item_id、step")

    # 全部改回 1
    session$setInputs(steps_reset = 1)
    expect_match(output$steps_status$html, "已經全部改回 1")
    expect_false("step" %in% names(item_table()))
    expect_equal(item_obj()$D, before$D)
    expect_false(grepl("step = steps", output$code))
    expect_identical(fixed_out(), lk$fixed)
  })
})

test_that("通過步驟：預設用這一回合開始時固定檔的設定（新版本沿用）", {
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
    expect_match(output$steps_status$html, "lanb08（3）")
    # 沒有改：固定檔原樣（回合仍是 r7）
    expect_identical(fixed_out(), start)
    session$setInputs(steps_reset = 1)
    back <- kitsui_read_fixed(kit_test_dl_file(output, session, "fixed_dl"))
    expect_equal(back$pass_steps$step, 1L)
    expect_equal(back$pass_steps$round, "r8")
  })
})
