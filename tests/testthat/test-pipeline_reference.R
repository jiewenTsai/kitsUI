# 參照世代可以選（C3）：預設、排序、寫進固定檔、③ ④ 的文字用實際的世代名稱；
# 外部連結不適用（C6）：參照世代沒有錨題時，分數停在參照世代的量尺上

# 兩個世代 B82、B85（參照世代 B85），題目不是錨題、領域不是 lan/motor/cog；
# 做好內部連結 B82 -> B85
no_anchor <- function() {
  if (is.null(pipeline_cache$no_anchor)) {
    betas <- purrr::set_names(as.list(seq(-2, 2, length.out = 8)), paste0("q", 1:8))
    X85 <- kits::kits_sim(seq(-2.5, 2, length.out = 500), betas, seed = 11)
    X82 <- kits::kits_sim(seq(-2, 2.5, length.out = 500), betas, seed = 12)
    rownames(X85) <- paste0("b85_", seq_len(nrow(X85)))
    rownames(X82) <- paste0("b82_", seq_len(nrow(X82)))
    df <- dplyr::bind_rows(
      dplyr::mutate(sim_long(X85, 0), domain = "mood", cohort = "B85"),
      dplyr::mutate(sim_long(X82, 0), domain = "mood", cohort = "B82")
    )
    r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty("B85"), "R1"))
    linked <- kitsui_link_internal(r$fixed, "mood", n_boot = 100)
    pipeline_cache$no_anchor <- list(df = df, round = r, linked = linked)
  }
  pipeline_cache$no_anchor
}

# 三個世代：M03 與 M36 的孩子分成兩個世代 M06、M40（都和 M03 有共同的題目）
three_cohorts <- function() {
  if (is.null(pipeline_cache$three)) {
    m36 <- kitsui_demo("M36", c(36, 48), "lan")
    ids <- sort(unique(m36$subj_id))
    m36$cohort <- ifelse(m36$subj_id %in% ids[seq(1, length(ids), 2)], "M06", "M40")
    df <- dplyr::bind_rows(kitsui_demo("M03", c(3, 12, 36, 48), "lan"), m36)
    r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "R1"))
    pipeline_cache$three <- list(df = df, round = r)
  }
  pipeline_cache$three
}

test_that("預設的參照世代：有 M03 時是 M03，沒有時是第一個世代；選的世代不在資料裡時用預設", {
  expect_equal(kitsui_ref_default(c("M36", "M03")), "M03")
  expect_equal(kitsui_ref_default(c("B85", "B82", NA)), "B82")
  expect_equal(kitsui_ref_default(character()), "M03")
  expect_equal(kitsui_ref_default(NULL), "M03")
  expect_equal(kitsui_ref_pick("B85", c("B82", "B85")), "B85")
  expect_equal(kitsui_ref_pick("M03", c("B82", "B85")), "B82")
  expect_equal(kitsui_ref_pick(NULL, c("B82", "B85")), "B82")
  expect_equal(kitsui_ref_pick(NULL, NULL), "M03")
})

test_that("勾選框與量尺表：參照世代在前（照實際的參照世代）", {
  data <- tibble::tibble(visit_id = c("a", "b", "c"), cohort = c("B82", "B85", "B82"),
                         wave_m = c(0, 0, 12), domain = "d", item_id = "x", item_score = 1)
  expect_equal(kitsui_pick_choices(data)$value, c("B82 0", "B82 12", "B85 0"))
  expect_equal(kitsui_pick_choices(data, "B85")$value, c("B85 0", "B82 0", "B82 12"))
  expect_equal(kitsui_scales(data, "B85")$scale, c("d B85", "d B82"))
  expect_equal(kitsui_scales(data)$scale, c("d B82", "d B85"))
  # KIT：M03 在前，和之前一樣
  kit <- kitsui_demo(waves = c(3, 36))
  expect_equal(kitsui_pick_choices(kit)$cohort[1], "M03")
  expect_equal(kitsui_scales(kit)$scale, c("lan M03", "motor M03", "lan M36", "motor M36"))
})

test_that("參照世代寫進固定檔的 VERSION，讀回來一樣；建立日期用台灣的日期", {
  fixed <- kitsui_fixed_empty("B85")
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(fixed, zip_path)
  dir <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = dir)
  expect_equal(unname(read.dcf(file.path(dir, "VERSION"))[, "Reference"]), "B85")
  expect_equal(kitsui_read_fixed(zip_path)$version$reference, "B85")
  expect_equal(fixed$version$created, format(Sys.time(), "%Y-%m-%d", tz = "Asia/Taipei"))
  withr::local_options(kitsui.tz = "Pacific/Kiritimati")
  expect_equal(kitsui_today(), format(Sys.time(), "%Y-%m-%d", tz = "Pacific/Kiritimati"))
})

test_that("③ 的標題用實際的世代名稱；KIT 的固定檔和之前一樣", {
  kit <- linked()$fixed
  expect_equal(kitsui_internal_title(kit), "1. 內部連結：M36 → M03")
  expect_match(kitsui_internal_hint(kit), "把 M36 的量尺換到參照世代 M03 的量尺。每個領域一條。",
               fixed = TRUE)
  expect_equal(kitsui_d_title(kit), "2. 外部連結：M03 → D")
  expect_match(kitsui_d_hint(kit), "外部連結用錨題把 M03 的量尺換到 D", fixed = TRUE)
  # 第一回合、還沒有其他世代
  empty <- kitsui_fixed_empty("B85")
  expect_equal(kitsui_internal_title(empty), "1. 內部連結：其他世代 → B85")
  # TDCYP 這類資料
  na <- no_anchor()$round$fixed
  expect_equal(kitsui_internal_title(na), "1. 內部連結：B82 → B85")
  expect_equal(kitsui_d_title(na, FALSE), "2. 外部連結：B85 → D（不適用）")
  expect_match(kitsui_d_hint(na, FALSE), "這不是錯誤")
  expect_equal(kitsui_code_internal_title(na, c("mood B85", "mood B82")),
               "# 內部連結後：B82 換到參照世代 B85 的量尺")
})

test_that("三個世代：M06、M40 各自有一條到 M03 的內部連結", {
  tc <- three_cohorts()
  fx <- tc$round$fixed
  expect_equal(kitsui_internal_scales(fx), c("lan M06", "lan M40"))
  expect_equal(kitsui_internal_title(fx), "1. 內部連結：M06、M40 → M03")
  expect_match(kitsui_internal_hint(fx), "每個世代、每個領域各一條")
  # 好幾個世代時要指定世代
  expect_error(kitsui_link_internal(fx, "lan"), "請指定是哪一個世代", class = "kit_user_error")
  a <- kitsui_link_internal(fx, "lan", "M06", n_boot = 100)
  b <- kitsui_link_internal(a$fixed, "lan", "M40", n_boot = 100)
  expect_true(a$new && b$new)
  links <- kitsui_links(b$fixed)
  expect_setequal(names(links), c("lan M06", "lan M40"))
  expect_equal(unname(purrr::map_chr(links, "to_scale")), c("lan M03", "lan M03"))
  expect_equal(kitsui_link_path(b$fixed, "lan M06"), "lan M06")
  expect_equal(kitsui_link_path(b$fixed, "lan M40"), "lan M40")
  ov <- kitsui_internal_overview(b$fixed, links, character())
  expect_equal(ov$to, c("lan M03", "lan M03"))
  expect_equal(ov$state, c("已凍結", "已凍結"))
  expect_equal(kitsui_internal_overview(fx, list(), character())$state, c("可以建立", "可以建立"))
  # 分數：兩個世代都換到 M03，再到 D 之前（沒有外部連結）留在 M03 的量尺
  sc <- kitsui_score(tc$round, tc$df, b$fixed)
  ends <- unique(sc$scores[c("cohort", "scale")])
  expect_setequal(ends$scale, "lan M03")
  # 垂直連結：三個世代，有兩個世代有共同的月齡，不說「沒有共同的月齡」
  v <- kitsui_verticals(tc$round, tc$df, b$fixed, "lan", links)
  expect_null(v$internal$why)
  expect_null(v$none$note)
  expect_equal(v$internal$scale, "lan M03")
})

test_that("各世代之間都沒有共同月齡時才說明（三個世代）", {
  fake <- \(w) data.frame(wave_m = w)
  expect_null(kitsui_vertical_no_common(list(a = fake(3), b = fake(36), c = fake(c(36, 48)))))
  expect_match(kitsui_vertical_no_common(list(a = fake(3), b = fake(36), c = fake(48))),
               "各世代之間沒有共同的月齡")
  expect_match(kitsui_vertical_no_common(list(a = fake(3), b = fake(36))), "兩個世代沒有共同的月齡")
  expect_null(kitsui_vertical_no_common(list(a = fake(3))))
})

test_that("外部連結不適用：參照世代沒有錨題時是 na（不是錯誤），沒有按鈕", {
  na <- no_anchor()
  fx <- na$linked$fixed
  anchors <- kitsui_anchor_counts(fx)
  expect_equal(anchors$n_anchors, 0)
  expect_false(kitsui_d_applicable(fx, anchors))
  expect_equal(kitsui_ref_final(fx), "B85")
  st <- kitsui_d_state(fx, kitsui_links(fx), character(), anchors)
  expect_equal(st$state, "na")
  expect_match(st$text, "不適用：參照世代（B85）")
  html <- as.character(link_status_ui(st, NULL, "make_d", "建立外部連結"))
  expect_false(grepl("make_d", html))
  expect_false(grepl("kit-callout-warn", html))
  expect_match(link_next_text(fx, character(), "na"), "外部連結不適用，分數停在參照世代（B85）的量尺上")
  # KIT：有錨題，照舊
  expect_true(kitsui_d_applicable(linked()$fixed))
  expect_null(kitsui_ref_final(linked()$fixed))
  expect_true(kitsui_d_applicable(round8()$round$fixed))
  # 參照世代還沒有校準時還不知道：照原來的說明
  expect_true(kitsui_d_applicable(kitsui_fixed_empty("B85")))
})

test_that("外部連結不適用：分數表是 ref_score（參照世代的量尺），沒有 d_score", {
  na <- no_anchor()
  fx <- na$linked$fixed
  sc <- kitsui_score(na$round, na$df, fx)$scores
  expect_true(all(c("ref_score", "ref_score_se", "se_link", "se_total") %in% names(sc)))
  expect_false(any(c("d_score", "d_score_se") %in% names(sc)))
  b85 <- sc[sc$cohort == "B85", ]
  b82 <- sc[sc$cohort == "B82", ]
  expect_equal(b85$ref_score, b85$ability)
  expect_true(all(is.na(b85$se_link)))
  expect_equal(unique(b82$scale), "mood B85")
  co <- coef(na$linked$link)
  ok <- is.finite(b82$ability)
  expect_equal(b82$ref_score[ok], co[["intercept"]] + co[["slope"]] * b82$ability[ok])
  expect_true(all(!is.na(b82$se_link)))
  # 還沒有內部連結：B82 的 ref_score 空白
  before <- kitsui_score(na$round, na$df, na$round$fixed)$scores
  expect_true(all(is.na(before$ref_score[before$cohort == "B82"])))
  # 欄位說明寫出參照世代
  cols <- kitsui_score_columns(sc, "B85")
  expect_match(cols$meaning[cols$column == "ref_score"], "參照世代（B85）")
  expect_false("d_score" %in% cols$column)
  # KIT 的分數表照舊有 d_score
  two <- two_cohorts_linked()
  kit <- kitsui_score(two$round, two$df, two$fixed)$scores
  expect_true("d_score" %in% names(kit))
  expect_false("ref_score" %in% names(kit))
})

test_that("外部連結不適用：垂直連結的「外部連結後」說不適用；這一回合照常寫出固定檔", {
  na <- no_anchor()
  fx <- na$linked$fixed
  v <- kitsui_verticals(na$round, na$df, fx, "mood")
  expect_null(v$internal$why)
  expect_true(v$external$na)
  expect_match(v$external$why, "外部連結不適用")
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(fx, zip_path)
  back <- kitsui_read_fixed(zip_path)
  expect_equal(back$version$reference, "B85")
  expect_equal(kitsui_link_names(back), "mood B82")
})

test_that("① 參照世代：第一回合可以選（預設第一個世代），寫進固定檔；有固定檔時只顯示", {
  path <- withr::local_tempfile(fileext = ".csv")
  df <- dplyr::bind_rows(dplyr::mutate(kitsui_demo("M03", 3, "lan"), cohort = "B85"),
                         dplyr::mutate(kitsui_demo("M36", 36, "lan"), cohort = "B82"))
  readr::write_csv(df, path)
  zip <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(kitsui_fixed_empty("B85"), zip)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "upload", round_label = "",
                      file = data.frame(name = "d.csv", size = 1, type = "", datapath = path))
    html <- output$reference_ui$html
    expect_match(html, '<option value="B82" selected>B82</option>', fixed = TRUE)
    expect_match(html, "寫進這一回合的固定檔")
    session$setInputs(reference = "B85", waves = c("B85 3", "B82 36"))
    expect_equal(session$getReturned()()$fixed$version$reference, "B85")
    # 勾選框：參照世代在前
    expect_lt(regexpr("B85", output$pick_ui$html), regexpr("B82", output$pick_ui$html))
    expect_equal(output$scale_example, "量尺 = 領域 × 世代，例如 lan B82。")
    # 有固定檔：只顯示固定檔的參照世代
    session$setInputs(fixed_file = data.frame(name = "f.zip", size = 1, type = "", datapath = zip))
    html <- output$reference_ui$html
    expect_false(grepl("<select", html))
    expect_match(gsub("<[^>]+>|\\s", "", html), "參照世代：B85")
    expect_match(output$fixed_info$html, "參照世代\\s*B85")
  })
  # KIT 示範資料：預設 M03
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "")
    expect_match(output$reference_ui$html, '<option value="M03" selected>M03</option>',
                 fixed = TRUE)
    expect_equal(session$getReturned()()$fixed$version$reference, "M03")
  })
})
