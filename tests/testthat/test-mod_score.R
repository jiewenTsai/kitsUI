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
    csv <- readr::read_csv(output$scores_dl, show_col_types = FALSE)
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
    expect_equal(nrow(readr::read_csv(output$subj_table_dl, show_col_types = FALSE)), nrow(tab))
    html <- output$result$html
    expect_match(html, "顯示沒有人的原始分數")
    expect_match(html, "作答組合")
    expect_match(html, "form_pct")
    expect_match(html, "cum_pct")
    expect_match(html, "wave_n")
    expect_match(html, "kits_norms")
    expect_match(html, "（CSV）")

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
    csv <- readr::read_csv(output$item_table_dl, show_col_types = FALSE)
    expect_equal(csv$d_threshold, item_table()$d_threshold)
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
    zip_path <- output$fixed_dl
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
    expect_error(output$fixed_dl, "停下來")
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
