# 一回合裡共用的受試者分數：每個量尺只算一次

test_that("kitsui_own_scores()：和 kits_subj() 相同，第二次直接拿存好的", {
  r <- round8()$round
  expect_true(is.environment(r$memo))
  persons <- kitsui_persons(round8()$df)
  s <- "motor M03"
  first <- kitsui_own_scores(r, s, persons)
  direct <- kits::kits_subj(r$data[[s]], r$items[[s]], persons = persons)
  expect_equal(as.data.frame(first), as.data.frame(direct), ignore_attr = TRUE)
  expect_match(kits::kits_info(first)$call, "kits_subj(X, item, persons = persons)", fixed = TRUE)
  expect_true(exists(paste0("own_scores:", s), envir = r$memo))
  # 存好的就是同一個物件；改了 data 也不重算（同一回合的資料不會變）
  r2 <- r
  r2$data[[s]] <- r2$data[[s]][1:10, ]
  expect_identical(kitsui_own_scores(r2, s, persons), first)
  # 沒有 memo：每次都算
  r3 <- r
  r3$memo <- NULL
  expect_equal(nrow(kitsui_own_scores(r3, s)), nrow(first))
  expect_error(kitsui_own_scores(r, "lan M99"), "停下來", class = "kit_user_error")
})

test_that("每一回合有自己的 memo：新的一回合不會拿到上一回合的分數", {
  a <- suppressMessages(kitsui_round(kitsui_demo("M03", 3, "lan"), kitsui_fixed_empty(), "1"))
  b <- suppressMessages(kitsui_round(kitsui_demo("M03", 6, "lan"), kitsui_fixed_empty(), "1"))
  sa <- kitsui_own_scores(a, "lan M03")
  sb <- kitsui_own_scores(b, "lan M03")
  expect_false(identical(sa$visit_id, sb$visit_id))
})
