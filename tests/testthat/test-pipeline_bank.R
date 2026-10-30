# 用哪一個題庫：月齡的規則、可以借的題庫、手動選擇

# 只有題庫的量尺、波次與內部連結的固定檔（規則只看這些）
fake_fixed <- function(waves, links) {
  fixed <- kitsui_fixed_empty()
  fixed$bank <- tibble::tibble(scale = names(waves), item_id = "x", item_score = 1L, beta = 0,
                               beta_se = 0, round = "1")
  fixed$waves <- purrr::imap(waves, \(w, s) tibble::tibble(scale = s, wave_m = w, round = "1")) |>
    purrr::list_rbind()
  fixed$links <- dplyr::bind_rows(fixed$links, tibble::tibble(
    kind = "internal", from_scale = links, to_scale = "motor M03"
  ))
  fixed
}

test_that("規則：自己的題庫是空的、有涵蓋、借、沒人涵蓋、還沒有連結，各有一句原因", {
  bc <- borrow_chain()
  plan <- kitsui_round_plan(bc$df2, bc$fixed)
  expect_equal(plan$why, "M03 題庫沒有校準過 60 月；M36 題庫有（36–96 月）")
  expect_equal(plan$detail,
               "M03 60 months on motor M36 bank (it covers 36-96 months); rule default")
  expect_false(plan$manual)
  own <- kitsui_round_plan(kitsui_demo("M03", 36, "motor"), bc$fixed)
  expect_equal(own$why, "M03 題庫有校準過這個月齡（3–48 月）")
  expect_true(is.na(own$detail))
  empty <- kitsui_round_plan(kitsui_demo("M03", 3, "lan"), bc$fixed)
  expect_equal(empty$why, "自己的題庫是空的：自由校準")
  old <- dplyr::mutate(kitsui_demo("M36", 96, "motor"), wave_m = 108)
  expect_equal(kitsui_round_plan(old, bc$fixed)$why, "沒有題庫校準過 108 月：用自己的題庫")
  no_link <- kitsui_round_plan(bc$df2, bc$r1$fixed)
  expect_equal(no_link$why, "M36 題庫有校準過 60 月，但還沒有到參照世代的內部連結：用自己的題庫")
})

test_that("可以借的題庫：到參照世代有凍結的內部連結；好幾個涵蓋時先選參照世代的，再依名稱", {
  bc <- borrow_chain()
  expect_equal(kitsui_bank_candidates(bc$fixed, "motor M03"), "motor M36")
  expect_equal(kitsui_bank_candidates(bc$fixed, "motor M36"), "motor M03")
  expect_equal(kitsui_bank_candidates(bc$r1$fixed, "motor M03"), character())
  fixed <- fake_fixed(list("motor M03" = c(3, 60), "motor M36" = c(36, 96),
                           "motor M72" = c(84, 120)), c("motor M36", "motor M72"))
  expect_equal(kitsui_bank_candidates(fixed, "motor M72"), c("motor M03", "motor M36"))
  df <- tibble::tibble(visit_id = "v", cohort = c("M72", "M72", "M03"), wave_m = c(60, 70, 100),
                       item_id = "x", item_score = 1, domain = "motor")
  plan <- kitsui_round_plan(df, fixed)
  expect_equal(plan$data_scale, c("motor M03", "motor M72", "motor M72"))
  expect_equal(plan$wave_m, c(100, 60, 70))
  expect_equal(plan$scale, c("motor M72", "motor M03", "motor M36"))
})

test_that("手動選題庫：照選的做、記下規則選的；不能選的題庫以中文說明", {
  bc <- borrow_chain()
  keep <- kitsui_round_plan(bc$df2, bc$fixed, c("motor M03" = "motor M03"))
  expect_equal(keep$scale, "motor M03")
  expect_equal(keep$rule, "motor M36")
  expect_false(keep$borrowed)
  expect_true(keep$manual)
  expect_equal(keep$why, "手動選擇：用 motor M03 的題庫（規則是 motor M36）")
  # 和規則相同的選擇不算手動
  same <- kitsui_round_plan(bc$df2, bc$fixed, c("motor M03" = "motor M36"))
  expect_false(same$manual)
  expect_identical(same, kitsui_round_plan(bc$df2, bc$fixed))
  # 規則用自己的題庫時，手動借
  df36 <- kitsui_demo("M03", 36, "motor")
  lend <- kitsui_round_plan(df36, bc$fixed, c("motor M03" = "motor M36"))
  expect_true(lend$borrowed)
  expect_equal(lend$detail, "M03 36 months on motor M36 bank; manual choice (rule: motor M03)")
  choice <- c("motor M03" = "motor M36")
  r <- suppressMessages(kitsui_round(df36, bc$fixed, "2", bank_choice = choice))
  expect_named(r$items, "motor M36")
  expect_equal(dplyr::filter(r$fixed$checks, check == "borrowed bank")$detail, lend$detail)
  expect_error(kitsui_round_plan(bc$df2, bc$fixed, c("motor M03" = "lan M03")), "不能用 lan M03",
               class = "kit_user_error")
  expect_error(kitsui_round_plan(bc$df2, bc$fixed, c("lan M36" = "lan M03")), "沒有 lan M36",
               class = "kit_user_error")
  # 空字串或 NA：照規則
  expect_identical(kitsui_round_plan(bc$df2, bc$fixed, c("motor M03" = "")),
                   kitsui_round_plan(bc$df2, bc$fixed))
})

test_that("選題庫的表：每個資料量尺一列；空的題庫只給自由校準與可以借的", {
  bc <- borrow_chain()
  df <- dplyr::bind_rows(bc$df2, kitsui_demo("M03", 60, "lan"))
  b <- kitsui_bank_options(kitsui_round_plan(df, bc$fixed), bc$fixed)
  expect_equal(b$data_scale, c("lan M03", "motor M03"))
  expect_equal(b$rule, c("lan M03", "motor M36"))
  expect_equal(unname(b$choices[[1]]), "lan M03")
  expect_equal(names(b$choices[[1]]), "自由校準（自己的題庫是空的）")
  expect_equal(unname(b$choices[[2]]), c("motor M03", "motor M36"))
  expect_equal(names(b$choices[[2]]), c("motor M03（自己的題庫）", "motor M36（借用另一世代題庫）"))
  # 各波次規則不同時，第一個選項是「依月齡的規則」
  mixed <- kitsui_round_plan(dplyr::bind_rows(bc$df2, kitsui_demo("M03", 36, "motor")), bc$fixed)
  bm <- kitsui_bank_options(mixed, bc$fixed)
  expect_equal(bm$rule, "")
  expect_equal(unname(bm$choices[[1]])[1], "")
  expect_equal(bm$waves, "36, 60")
})

test_that("選題庫的表的 why 欄跟著選擇：照規則時是規則的原因，改了時說手動選擇", {
  bc <- borrow_chain()
  df <- dplyr::bind_rows(bc$df2, kitsui_demo("M03", 60, "lan"))
  b <- kitsui_bank_options(kitsui_round_plan(df, bc$fixed), bc$fixed)
  expect_equal(kitsui_bank_why(b, b$rule), b$why)
  expect_equal(kitsui_bank_why(b, c(NA, "")), b$why)
  why <- kitsui_bank_why(b, c("lan M03", "motor M03"))
  expect_equal(why[1], b$why[1])
  expect_equal(why[2], "手動選擇：用 motor M03 的題庫（規則是 motor M36）")
  # 和真的跑的時候（kitsui_round_plan(bank_choice = )）寫的一樣
  plan <- kitsui_round_plan(df, bc$fixed, c("motor M03" = "motor M03"))
  expect_equal(why[2], plan$why[plan$data_scale == "motor M03"])
  # 各波次規則不同時
  mixed <- kitsui_round_plan(dplyr::bind_rows(bc$df2, kitsui_demo("M03", 36, "motor")), bc$fixed)
  bm <- kitsui_bank_options(mixed, bc$fixed)
  expect_equal(kitsui_bank_why(bm, "motor M36"),
               "手動選擇：用 motor M36 的題庫（規則依月齡，各波次不同）")
})

test_that("波次的單位：寫進固定檔、讀回來；舊的固定檔是月；單位不是月時不借題庫、不算 age50", {
  expect_equal(kitsui_wave_unit(kitsui_fixed_empty()), "months")
  expect_equal(kitsui_wave_text(c(3, 36)), c("3 個月", "36 個月"))
  expect_equal(kitsui_wave_text(2013, "years"), "2013 年")
  expect_equal(kitsui_wave_text(2, "waves"), "第 2 波")
  bc <- borrow_chain()
  fixed <- bc$fixed
  # 沒寫單位（舊的固定檔）：月，照規則借
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(fixed, zip_path)
  expect_null(kitsui_read_fixed(zip_path)$version$wave_unit)
  df <- kitsui_demo("M03", 60, "motor")
  expect_true(kitsui_round_plan(df, fixed)$borrowed)
  # 單位是年：不借，原因寫出來；手動仍然可以選
  yrs <- fixed
  yrs$version$wave_unit <- "years"
  p <- kitsui_round_plan(df, yrs)
  expect_false(p$borrowed)
  expect_match(p$why, "波次的單位是年，借另一世代題庫的規則只用在月齡：用自己的題庫")
  expect_match(p$why, "60 年")
  manual <- kitsui_round_plan(df, yrs, c("motor M03" = "motor M36"))
  expect_true(manual$borrowed)
  expect_match(manual$detail, "60 years on motor M36 bank")
  # VERSION 的 WaveUnit 寫出再讀回
  kitsui_write_fixed(yrs, zip_path)
  back <- kitsui_read_fixed(zip_path)
  expect_equal(back$version$wave_unit, "years")
  expect_equal(kitsui_wave_unit(back), "years")
  # age50：單位不是月時不算
  two <- two_cohorts_linked()
  tab <- kitsui_item_table(two$round, two$df, two$fixed, "lan M03")
  expect_true(any(!is.na(tab$age50)))
  fx <- two$fixed
  fx$version$wave_unit <- "waves"
  tab2 <- kitsui_item_table(two$round, two$df, fx, "lan M03")
  expect_true(all(is.na(tab2$age50 %||% NA)))
  html <- as.character(item_table_note_ui(kitsui_item_table_shown(tab2), unit = "waves"))
  expect_match(html, "age50 不算：波次（wave_m）的單位不是月")
})

test_that("垂直連結圖：單位不是月時 x 軸是波次與單位；沒有共同波次的說明不假設兩個世代", {
  two <- two_cohorts_linked()
  vs <- kitsui_verticals(two$round, two$df, two$fixed, "lan")
  expect_equal(vs$none$wave_unit, "months")
  p <- plotly::plotly_build(kitsui_vertical_plot(vs$internal))
  title <- \(p) {
    t <- p$x$layout$xaxis$title
    if (is.list(t)) t$text else t
  }
  expect_match(title(p), "^Age \\(months")
  fx <- two$fixed
  fx$version$wave_unit <- "years"
  vy <- kitsui_verticals(two$round, two$df, fx, "lan")
  py <- plotly::plotly_build(kitsui_vertical_plot(vy$internal))
  expect_equal(title(py), "Wave (years)")
  cap <- vertical_caption("internal", vy$internal)
  expect_match(cap, "同一年各世代的年齡不同")
  expect_no_match(cap, "應該靠在一起")
  expect_match(vertical_caption("internal", vs$internal), "同一個月齡兩個世代的點應該靠在一起")
  # ① 的作答設計圖：kits 寫 "Wave (months)" 的軸照單位改
  rp <- plot(kitsui_resp(two$df, "lan M03"))
  axis_titles <- \(p) {
    b <- plotly::plotly_build(p)$x$layout
    vapply(c("xaxis", "yaxis"), \(a) { t <- b[[a]]$title; if (is.list(t)) t$text %||% "" else t %||% "" }, "")
  }
  expect_true("Wave (months)" %in% axis_titles(rp))
  expect_identical(kitsui_wave_axis(rp, "months"), rp)
  yr <- axis_titles(kitsui_wave_axis(rp, "years"))
  expect_true("Wave (years)" %in% yr)
  expect_false("Wave (months)" %in% yr)
  raw <- list(`a M1` = data.frame(wave_m = 1), `a M2` = data.frame(wave_m = 2),
              `a M3` = data.frame(wave_m = 2))
  expect_null(kitsui_vertical_no_common(raw))
  expect_match(kitsui_vertical_no_common(raw[1:2], "waves"),
               "^這一回合兩個世代沒有共同的波次（a M1：第 1 波；a M2：第 2 波）")
  expect_match(kitsui_vertical_no_common(raw[1:2]), "（a M1：1 個月；a M2：2 個月）")
  expect_equal(kitsui_waves_text(c(3, 6)), "3、6 個月")
  expect_equal(kitsui_waves_text(c(2009, 2011), "years"), "2009、2011 年")
  expect_equal(kitsui_waves_text(c(1, 2), "waves"), "第 1、2 波")
  raw3 <- list(`a M1` = data.frame(wave_m = 1), `a M2` = data.frame(wave_m = 2),
               `a M3` = data.frame(wave_m = 3))
  expect_match(kitsui_vertical_no_common(raw3), "^這一回合各世代之間沒有共同的月齡")
})
