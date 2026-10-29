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
