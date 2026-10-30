# 分數類別的問題：列出來；② 校準時題庫的題固定在題庫的值，不在題庫的題不放進來

test_that("每個資料量尺的分數類別問題：沒有 0 分、不連續、只有一種分數，一句中文", {
  df <- tibble::tibble(
    visit_id = rep(paste0("v", 1:4), 4), cohort = "M03", wave_m = 3, domain = "lan",
    item_id = rep(c("lanc01", "lanc02", "lanc03", "lanc04"), each = 4),
    item_score = c(1, 2, 3, 2, 0, 2, 3, 3, 1, 1, 1, NA, 0, 1, 2, 1)
  )
  df36 <- dplyr::mutate(df, cohort = "M36", item_score = dplyr::if_else(
    item_id == "lanc01" & item_score == 1, 0, item_score
  ))
  p <- kitsui_category_problems(dplyr::bind_rows(df, df36))
  expect_named(p, c("scale", "item_id", "problem", "detail_zh"))
  m03 <- dplyr::filter(p, scale == "lan M03")
  expect_equal(m03$item_id, c("lanc01", "lanc02", "lanc03"))
  expect_equal(m03$problem, c("no 0 score", "score gap", "one score only"))
  expect_equal(m03$detail_zh, c("lanc01 沒有人得 0 分（分數 1、2、3）",
                                "lanc02 分數不連續，沒有人得 1 分（分數 0、2、3）",
                                "lanc03 每個人都得 1 分，只有一種分數"))
  # M36 的 lanc01 有人得 0 分（0、2、3 是不連續）；只看給的列
  m36 <- dplyr::filter(p, scale == "lan M36")
  expect_equal(m36$item_id, c("lanc01", "lanc02", "lanc03"))
  expect_equal(m36$problem[1], "score gap")
  expect_equal(nrow(kitsui_category_problems(dplyr::filter(df, item_id == "lanc04"))), 0)
  # 沒有 0 分又不連續：兩個問題放在同一列
  both <- kitsui_score_problem("x", c(1, 3))
  expect_equal(both$problem, "no 0 score, score gap")
  expect_equal(both$detail, "x 沒有人得 0 分；分數不連續，沒有人得 2 分（分數 1、3）")
  expect_true(is.na(kitsui_score_problem("x", 0:2)$problem))
})

test_that("沒有 0 分：題庫的題固定在題庫的值；新的題這一回合不放進來；固定檔寫得出來", {
  pb <- poly_bank()
  X2 <- poly_round2()
  X2[X2[, "p1"] == 0, "p1"] <- 1L
  X2[X2[, "n1"] == 0, "n1"] <- 1L
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), pb$r1$fixed, "2"))
  expect_true(r2$ok)
  kept <- dplyr::filter(r2$log, check == "category problem, kept fixed")
  expect_equal(kept$item_id, "p1")
  expect_equal(kept$detail, "p1 沒有人得 0 分（分數 1、2、3）：不判斷偏移，固定在題庫的值")
  out <- dplyr::filter(r2$log, check == "category problem, left out")
  expect_equal(out$item_id, "n1")
  expect_equal(out$detail, "n1 沒有人得 0 分（分數 1、2、3）：不在題庫裡，這一回合不放進來")
  co <- coef(r2$items[["sim M03"]])
  old <- pb$r1$fixed$bank
  expect_equal(co$beta[co$item_id == "p1"], old$beta[old$item_id == "p1"])
  expect_false("n1" %in% co$item_id)
  expect_false("n1" %in% colnames(r2$data[["sim M03"]]))
  expect_false("p1" %in% r2$drift[["sim M03"]]$items$item_id)
  # 新的題 n2 照常校準，加進題庫；n1 沒有
  expect_true("n2" %in% r2$fixed$bank$item_id)
  expect_false("n1" %in% r2$fixed$bank$item_id)
  expect_identical(r2$fixed$bank[seq_len(nrow(old)), ], old)
  expect_true(all(c("category problem, kept fixed", "category problem, left out") %in%
                    r2$fixed$checks$check))
  zip_path <- withr::local_tempfile(fileext = ".zip")
  expect_no_error(kitsui_write_fixed(r2$fixed, zip_path))
  expect_s3_class(kits::kits_subj(r2$data[["sim M03"]], r2$items[["sim M03"]]), "data.frame")
})

test_that("分數不連續（0、2、3）：題庫的題固定，新的題不放進來", {
  pb <- poly_bank()
  X2 <- poly_round2(13)
  X2[X2[, "p2"] == 1, "p2"] <- 2L
  X2[X2[, "n2"] == 1, "n2"] <- 2L
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), pb$r1$fixed, "2"))
  expect_true(r2$ok)
  kept <- dplyr::filter(r2$log, check == "category problem, kept fixed")
  expect_equal(kept$item_id, "p2")
  expect_match(kept$detail, "p2 分數不連續，沒有人得 1 分（分數 0、2、3）")
  expect_equal(dplyr::filter(r2$log, check == "category problem, left out")$item_id, "n2")
  co <- coef(r2$items[["sim M03"]])
  old <- pb$r1$fixed$bank
  expect_equal(co$beta[co$item_id == "p2"], old$beta[old$item_id == "p2"])
  expect_false("n2" %in% co$item_id)
  expect_false("capped" %in% r2$log$check)
})

test_that("只有一種分數：題庫的題固定，新的題不放進來；這一回合照樣寫出固定檔", {
  pb <- poly_bank()
  X2 <- poly_round2(14)
  X2[, "a3"] <- 1L
  X2[, "n1"] <- 2L
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), pb$r1$fixed, "2"))
  expect_true(r2$ok)
  kept <- dplyr::filter(r2$log, check == "category problem, kept fixed")
  expect_equal(kept$detail, "a3 每個人都得 1 分，只有一種分數：不判斷偏移，固定在題庫的值")
  expect_equal(dplyr::filter(r2$log, check == "category problem, left out")$item_id, "n1")
  co <- coef(r2$items[["sim M03"]])
  expect_equal(co$beta[co$item_id == "a3"],
               pb$r1$fixed$bank$beta[pb$r1$fixed$bank$item_id == "a3"])
  zip_path <- withr::local_tempfile(fileext = ".zip")
  expect_no_error(kitsui_write_fixed(r2$fixed, zip_path))
  back <- kitsui_read_fixed(zip_path)
  expect_true("category problem, kept fixed" %in% back$checks$check)
})

test_that("併分數與分數類別一致：題庫最高步沒有人得到時併為最高步；固定的題不加新步驟", {
  sb <- sim_bank()
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 1000), sb$betas, prefix = "g", seed = 15)
  # a1 題庫只有 1 分；資料 0、2 分（沒有人得 1 分）：2 分不能當新步驟，併為 1 分
  X2[X2[, "a1"] == 1, "a1"] <- 2L
  # a2 資料 1、2 分（沒有 0 分）：固定在題庫的值，2 分併為 1 分，不加新步驟
  X2[, "a2"] <- X2[, "a2"] + 1L
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), sb$r1$fixed, "2"))
  expect_true(r2$ok)
  capped <- dplyr::filter(r2$log, check == "capped")
  expect_equal(capped$item_id, c("a1", "a2"))
  expect_match(capped$detail[1], "超過 1 分，併為 1 分$")
  expect_match(capped$detail[2], "併為 1 分（分數類別有問題，不加新的步驟）")
  kept <- dplyr::filter(r2$log, check == "category problem, kept fixed")
  expect_equal(kept$item_id, "a2")
  expect_match(kept$detail, "a2 沒有人得 0 分（分數 1、2）")
  # a1 併過以後沒有問題：照常判斷偏移
  expect_true("a1" %in% r2$drift[["sim M03"]]$items$item_id)
  co <- coef(r2$items[["sim M03"]])
  expect_equal(sum(co$item_id == "a2"), 1)
  expect_identical(r2$fixed$bank, sb$r1$fixed$bank)
  # 直接看 kitsui_cap()
  bank <- tibble::tibble(item_id = "x", item_score = 1L)
  X <- matrix(c(0L, 2L, 3L, 0L), ncol = 1, dimnames = list(NULL, "x"))
  expect_equal(as.vector(kitsui_cap(X, bank)$X), c(0, 1, 1, 0))
  X[2, 1] <- 1L
  expect_equal(as.vector(kitsui_cap(X, bank)$X), c(0, 1, 1, 0))
  X[3, 1] <- 2L
  expect_equal(as.vector(kitsui_cap(X, bank)$X), c(0, 1, 2, 0))
  expect_equal(as.vector(kitsui_cap(X, bank, no_new = "x")$X), c(0, 1, 1, 0))
})

test_that("kits 說估不出來的題：讀出題號；自由校準時這一回合不放進來", {
  ids <- c("a1", "a12", "a2", "x1", "x2")
  err <- \(msg) simpleError(msg)
  expect_equal(kitsui_error_items(err("Item(s) with no 0 score: a2. Scores must start at 0"), ids),
               "a2")
  expect_equal(kitsui_error_items(err("Item a12 has scores 0, 2, 3 (missing 1)."), ids), "a12")
  expect_equal(kitsui_error_items(err(paste(
    "Item(s) with a gap in their observed scores: a12 (0, 2, 3).",
    "Scores must be consecutive; recode first."
  )), ids), "a12")
  no_cml <- err(paste("Item step(s) with no finite CML estimate: x1:1 (reached by none),",
                      "x2:1 (missed by none). Only person-waves"))
  expect_equal(kitsui_error_items(no_cml, ids), c("x1", "x2"))
  expect_equal(kitsui_error_items(err("Fewer than 2 items"), ids), character())
  ev <- kitsui_error_events(no_cml, c("x1", "x2"), c(TRUE, FALSE))
  expect_equal(ev$check, c("no CML estimate, kept fixed", "category problem, left out"))
  expect_match(ev$detail[2], "估不出有限的 CML 估計值.*這一回合不放進來")
  # 自由校準：a10 只有全對的人以外都通過，CML 估不出來
  sb <- sim_bank()
  X <- kits::kits_sim(seq(-3, 3, length.out = 1000), sb$betas, prefix = "h", seed = 16)
  rest <- rowSums(X[, colnames(X) != "a10"])
  X[, "a10"] <- ifelse(rest == 0, 0L, 1L)
  r <- suppressMessages(kitsui_round(sim_long(X), kitsui_fixed_empty(), "1"))
  expect_true(r$ok)
  out <- dplyr::filter(r$log, check == "category problem, left out")
  expect_equal(out$item_id, "a10")
  expect_match(out$detail, "CML")
  expect_false("a10" %in% coef(r$items[["sim M03"]])$item_id)
})
