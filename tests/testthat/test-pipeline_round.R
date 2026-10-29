test_that("kitm03 lan 3-48 個月一波一輪：版本、併分數、舊 beta 不動（同 chain_v.R）", {
  chain <- lan_chain()
  last <- chain[["48"]]
  expect_equal(last$fixed$version$rounds, names(chain))
  expect_equal(
    dplyr::arrange(last$fixed$versions, current_id)$current_id,
    c("lana02_r12", "lanc03_r24", "lanc05_r48", "lanc08_r24")
  )
  expect_equal(nrow(last$fixed$bank), 84)
  expect_true(all(lengths(purrr::map(chain, "stopped")) == 0))
  capped <- dplyr::filter(last$fixed$checks, check == "capped")
  expect_equal(capped$item_id, "lanc09")
  expect_equal(capped$round, "18")
  # 舊的 beta 一輪一輪都沒動
  for (i in seq_along(chain)[-1]) {
    old <- chain[[i - 1]]$fixed$bank
    new <- chain[[i]]$fixed$bank
    expect_identical(new[seq_len(nrow(old)), ], old)
  }
  # 題庫新加的步驟就是這一輪校準的 beta
  item <- last$items[["lan M03"]]
  added <- dplyr::filter(last$fixed$bank, round == "48")
  co <- coef(item)[match(paste(added$item_id, added$item_score),
                         paste(coef(item)$item_id, coef(item)$item_score)), ]
  expect_equal(added$beta, co$beta)
  expect_true("lanc05_r48" %in% colnames(last$data[["lan M03"]]))
})

test_that("多個量尺的一輪：沒有題庫的量尺自由校準", {
  r <- round8()$round
  expect_setequal(names(r$items), c("lan M36", "motor M03"))
  expect_true(r$ok)
  expect_equal(r$log$detail, c("自由校準", "自由校準"))
  expect_setequal(r$fixed$version$scales, c("lan M03", "lan M36", "motor M03"))
})

test_that("強烈偏移時量尺被標記（flag），照常校準，產生新的固定檔", {
  betas <- purrr::set_names(as.list(seq(-2, 2, length.out = 12)), paste0("a", 1:12))
  X1 <- kits::kits_sim(seq(-3, 3, length.out = 1000), betas, seed = 1)
  r1 <- suppressMessages(kitsui_round(sim_long(X1), kitsui_fixed_empty(), "1"))
  # a2、a11 偏移 3 logit 以上（被抓到）；12 題的 10% 是 1.2 題，偏移 2 題就標記
  drifted <- utils::modifyList(betas, list(a2 = 2, a5 = -2, a8 = 2.5, a11 = -2))
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 1000), drifted, prefix = "q", seed = 2)
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), r1$fixed, "2", flag_share = 0.1))
  expect_true(r2$ok)
  expect_length(r2$stopped, 0)
  expect_named(r2$items, "sim M03")
  flag <- dplyr::filter(r2$log, check == "flag")
  expect_match(flag$detail, "偏移題太多")
  expect_equal(dplyr::filter(r2$fixed$checks, check == "flag")$round, "2")
  expect_false("stop" %in% r2$log$check)
  # 新版本、題庫照常更新；舊的 beta 不動；固定檔寫得出來
  moved <- dplyr::filter(r2$log, check == "drift")$item_id
  expect_true(length(moved) >= 2)
  expect_setequal(r2$fixed$versions$item_id, moved)
  expect_identical(r2$fixed$bank[seq_len(nrow(r1$fixed$bank)), ], r1$fixed$bank)
  expect_true(nrow(r2$fixed$bank) > nrow(r1$fixed$bank))
  zip_path <- withr::local_tempfile(fileext = ".zip")
  expect_no_error(kitsui_write_fixed(r2$fixed, zip_path))
  # 沒有偏移、但可固定的題少於 flag_fixed：也是標記
  X3 <- kits::kits_sim(seq(-3, 3, length.out = 1000), betas, prefix = "s", seed = 3)
  r3 <- suppressMessages(kitsui_round(sim_long(X3, 3), r1$fixed, "3", flag_fixed = 20))
  expect_true(r3$ok)
  expect_match(dplyr::filter(r3$log, check == "flag")$detail, "可固定的題少於 20 題")
})

test_that("輪的名稱不合用或重複時以中文說明", {
  fixed <- lan_chain()[["3"]]$fixed
  df <- kitsui_demo("M03", 6, "lan")
  expect_error(kitsui_round(df, fixed, "3"), "重複", class = "kit_user_error")
  expect_error(kitsui_round(df, fixed, "w 6"), "英文字母", class = "kit_user_error")
})

test_that("自由校準：分數類別有問題的題這一輪不放進來，量尺照做", {
  # lan M03 只有 36、48 個月時 lanc01 沒有人得 0 分
  df <- dplyr::bind_rows(kitsui_demo("M03", c(36, 48), "lan"), kitsui_demo("M03", 3, "motor"))
  r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "1"))
  expect_true(r$ok)
  expect_setequal(names(r$items), c("lan M03", "motor M03"))
  out <- dplyr::filter(r$log, check == "category problem, left out", scale == "lan M03")
  expect_true("lanc01" %in% out$item_id)
  expect_match(out$detail[out$item_id == "lanc01"], "^lanc01 沒有人得 0 分（分數 ")
  expect_match(out$detail[out$item_id == "lanc01"], "不在題庫裡，這一輪不放進來$")
  expect_false("lanc01" %in% coef(r$items[["lan M03"]])$item_id)
  expect_false("lanc01" %in% colnames(r$data[["lan M03"]]))
  expect_false("lanc01" %in% r$fixed$bank$item_id)
  expect_true("category problem, left out" %in% r$fixed$checks$check)
  zip_path <- withr::local_tempfile(fileext = ".zip")
  expect_no_error(kitsui_write_fixed(r$fixed, zip_path))
})

test_that("kits 出錯的量尺停下來並記下原因（中文），其他量尺照做", {
  sb <- sim_bank()
  # 只有 20 人：每一步上下都不到 30 人，kits_drift() 判斷不了偏移
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 20), sb$betas, prefix = "k", seed = 7)
  df <- dplyr::bind_rows(sim_long(X2, 2), kitsui_demo("M03", 3, "motor"))
  r <- suppressMessages(kitsui_round(df, sb$r1$fixed, "2"))
  expect_equal(r$stopped, "sim M03")
  expect_false(r$ok)
  expect_equal(names(r$items), "motor M03")
  err <- dplyr::filter(r$log, check == "error")
  expect_equal(err$scale, "sim M03")
  expect_match(err$detail, "^kits 出錯，這個量尺停下來。kits 的訊息：Only")
  expect_equal(dplyr::filter(r$fixed$checks, check == "error")$scale, "sim M03")
  zip_path <- withr::local_tempfile(fileext = ".zip")
  expect_error(kitsui_write_fixed(r$fixed, zip_path), "sim M03", class = "kit_user_error")
})

test_that("拿掉分數類別有問題的題以後沒有題可以估計：量尺停下來", {
  X <- kits::kits_sim(seq(-3, 3, length.out = 300), list(a1 = 0, a2 = 0.5, a3 = -0.5), seed = 8)
  X[, "a1"] <- 1L
  X[X[, "a2"] == 0, "a2"] <- 1L
  r <- suppressMessages(kitsui_round(sim_long(X), kitsui_fixed_empty(), "1"))
  expect_false(r$ok)
  expect_equal(r$stopped, "sim M03")
  stop_row <- dplyr::filter(r$log, check == "stop")
  expect_match(stop_row$detail, "只剩 1 題，沒有題可以估計")
  expect_setequal(dplyr::filter(r$log, check == "category problem, left out")$item_id,
                  c("a1", "a2"))
})

test_that("progress 每個量尺開始前呼叫一次：第幾個、共幾個、量尺", {
  seen <- list()
  df <- dplyr::bind_rows(kitsui_demo("M03", 3, "lan"), kitsui_demo("M03", 3, "motor"))
  note <- function(i, n, scale) seen[[length(seen) + 1]] <<- list(i = i, n = n, scale = scale)
  r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "3", progress = note))
  expect_true(r$ok)
  expect_equal(purrr::map_int(seen, "i"), 1:2)
  expect_equal(purrr::map_dbl(seen, "n"), c(2, 2))
  expect_equal(purrr::map_chr(seen, "scale"), c("lan M03", "motor M03"))
})

test_that("可固定的題少於 5 題時標記（flag），照常校準", {
  sb <- sim_bank()
  # 題庫的題只有 a1-a4，另外 8 題是新的題
  new <- purrr::set_names(as.list(seq(-1.5, 1.5, length.out = 8)), paste0("b", 1:8))
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 1000), c(sb$betas[1:4], new), prefix = "f",
                       seed = 4)
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), sb$r1$fixed, "2"))
  expect_true(r2$ok)
  flag <- dplyr::filter(r2$log, check == "flag")
  expect_equal(nrow(flag), 1)
  expect_match(flag$detail, "可固定 4 題（可固定的題少於 5 題）")
  expect_equal(dplyr::filter(r2$fixed$checks, check == "flag")$detail, flag$detail)
  expect_true(all(paste0("b", 1:8) %in% r2$fixed$bank$item_id))
  expect_equal(formals(kitsui_round)$flag_fixed, 5)
  # flag_fixed = 3 時不標記
  r3 <- suppressMessages(kitsui_round(sim_long(X2, 2), sb$r1$fixed, "2", flag_fixed = 3))
  expect_false("flag" %in% r3$log$check)
})

test_that("這一輪估不出來的題不換新版本，固定在題庫的值；kitsui_round() 不出錯", {
  sb <- sim_bank()
  X2 <- kits::kits_sim(seq(-3, 3, length.out = 1000), sb$betas, prefix = "c", seed = 2)
  # a10 很難，但這一輪有作答的人（其他題不是全錯）都通過：CML 估不出來
  rest <- rowSums(X2[, colnames(X2) != "a10"])
  X2[, "a10"] <- ifelse(rest == 0, 0L, 1L)
  ref <- structure(sb$r1$fixed$bank[c("item_id", "item_score", "beta")], scale = "sim M03")
  expect_error(kits::kits_drift(X2, ref), "no finite CML estimate")
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), sb$r1$fixed, "2"))
  expect_true(r2$ok)
  kept <- dplyr::filter(r2$log, check == "no CML estimate, kept fixed")
  expect_equal(kept$item_id, "a10")
  expect_false("a10" %in% r2$fixed$versions$item_id)
  co <- coef(r2$items[["sim M03"]])
  old <- sb$r1$fixed$bank
  expect_equal(co$beta[co$item_id == "a10"], old$beta[old$item_id == "a10"])
  expect_identical(r2$fixed$bank, old)
  expect_true("no CML estimate, kept fixed" %in% r2$fixed$checks$check)
})

test_that("自己的題庫沒有涵蓋這個波次、另一個世代的有：借另一個世代的題庫", {
  bc <- borrow_chain()
  r2 <- bc$r2
  expect_equal(r2$plan$scale, "motor M36")
  expect_true(r2$plan$borrowed)
  detail <- "M03 60 months on motor M36 bank (it covers 36-96 months); rule default"
  expect_equal(r2$plan$detail, detail)
  expect_true(r2$ok)
  expect_named(r2$items, "motor M36")
  expect_true(all(startsWith(rownames(r2$data[["motor M36"]]), "M03_")))
  log <- dplyr::filter(r2$fixed$checks, check == "borrowed bank")
  expect_equal(log$scale, "motor M36")
  expect_equal(log$detail, detail)
  expect_match(dplyr::filter(r2$log, check == "calibration")$detail, "M03 的資料借用另一世代題庫")
  # 舊的 beta 不動；新的步驟加進 motor M36 的題庫；motor M36 的波次多了 60
  old <- bc$fixed$bank
  expect_identical(r2$fixed$bank[seq_len(nrow(old)), ], old)
  added <- dplyr::filter(r2$fixed$bank, round == "2")
  expect_true(nrow(added) > 0)
  expect_equal(unique(added$scale), "motor M36")
  w <- dplyr::filter(r2$fixed$waves, round == "2")
  expect_equal(w$scale, "motor M36")
  expect_equal(w$wave_m, 60)
  # 分數走 motor M36 -> motor M03 -> D；人的 cohort 不變
  sc <- kitsui_score(r2, bc$df2, r2$fixed)
  routes <- kitsui_score_routes(r2$fixed, sc$subj)
  expect_equal(routes$route, "motor M36 → motor M03 → D")
  expect_equal(unique(sc$scores$cohort), "M03")
  expect_equal(unique(sc$scores$scale), "D")
  # 題目表的通過年齡用 M03 的人
  expect_equal(kitsui_round_cohorts(r2, "motor M36"), "M03")
  ages <- kitsui_item_ages(r2$data[["motor M36"]], bc$df2, "M03")
  expect_false(anyNA(ages$age_m))
  expect_s3_class(kitsui_item_table(r2, bc$df2, r2$fixed, "motor M36"), "kits_item_table")
})

test_that("不借題庫：自己的題庫有涵蓋、內部連結還沒凍結、題庫的波次不知道", {
  bc <- borrow_chain()
  covered <- kitsui_round_plan(kitsui_demo("M03", 36, "motor"), bc$fixed)
  expect_false(covered$borrowed)
  expect_equal(covered$scale, "motor M03")
  # 第 1 輪的固定檔還沒有內部連結
  no_link <- kitsui_round_plan(bc$df2, bc$r1$fixed)
  expect_false(no_link$borrowed)
  r <- suppressMessages(kitsui_round(bc$df2, bc$r1$fixed, "2"))
  expect_named(r$items, "motor M03")
  expect_false("borrowed bank" %in% r$log$check)
  # 舊的固定檔沒有 waves.csv：讀進來時波次是 NA，不借
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(bc$fixed, zip_path)
  dir <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = dir)
  file.remove(file.path(dir, "waves.csv"))
  old_zip <- file.path(dir, "old.zip")
  zip::zip(old_zip, list.files(dir, full.names = TRUE), mode = "cherry-pick")
  old <- kitsui_read_fixed(old_zip)
  expect_true(all(is.na(old$waves$wave_m)))
  expect_setequal(old$waves$scale, c("motor M03", "motor M36"))
  expect_false(kitsui_round_plan(bc$df2, old)$borrowed)
  # 自己的題庫是空的：自由校準，不借
  empty <- kitsui_round_plan(kitsui_demo("M03", 60, "lan"), bc$fixed)
  expect_equal(empty$scale, "lan M03")
})

test_that("兩個世代的資料都放到同一個題庫時，接成一個矩陣", {
  bc <- borrow_chain()
  df <- dplyr::bind_rows(bc$df2, kitsui_demo("M36", 60, "motor"))
  plan <- kitsui_round_plan(df, bc$fixed)
  expect_equal(plan$scale, c("motor M36", "motor M36"))
  expect_equal(plan$borrowed, c(TRUE, FALSE))
  r <- suppressMessages(kitsui_round(df, bc$fixed, "3"))
  expect_true(r$ok)
  expect_named(r$items, "motor M36")
  expect_equal(nrow(r$data[["motor M36"]]), dplyr::n_distinct(df$visit_id))
  expect_equal(sum(r$log$check == "borrowed bank"), 1)
  # 兩個世代的 visit_id 重複時以中文說明
  dup <- df
  dup$visit_id[dup$cohort == "M36"] <- dup$visit_id[dup$cohort == "M03"][1]
  expect_error(kitsui_matrix(dup, plan$data_scale), "相同的 visit_id", class = "kit_user_error")
})

test_that("新版本記在做它的資料的世代名下：M03 借 M36 題庫做的版本，M36 的資料不用", {
  bc <- borrow_chain()
  vers <- bc$r2$fixed$versions
  expect_true(nrow(vers) > 0)
  expect_equal(unique(vers$scale), "motor M36")
  expect_equal(unique(vers$cohort), "M03")
  expect_true(all(vers$current_id == paste0(vers$item_id, "_r2")))
  it <- vers$item_id[1]
  # 之後 M03 的資料用新版本；M36 的資料還是原來的題號
  m03 <- kitsui_current_ids(kitsui_demo("M03", 60, "motor"), bc$r2$fixed, "motor M36")
  expect_true(paste0(it, "_r2") %in% m03$item_id)
  expect_false(it %in% m03$item_id)
  m36_df <- kitsui_demo("M36", 84, "motor")
  m36 <- kitsui_current_ids(m36_df, bc$r2$fixed, "motor M36")
  expect_identical(m36$item_id, m36_df$item_id)
  r3 <- suppressMessages(kitsui_round(m36_df, bc$r2$fixed, "3"))
  expect_true(r3$ok)
  X <- r3$data[["motor M36"]]
  if (it %in% m36_df$item_id) expect_true(it %in% colnames(X))
  expect_false(any(grepl("_r2$", colnames(X))))
})

test_that("再偏移一次時，新版本從原來的題號做（a2_r3，不是 a2_r2_r3）", {
  sb <- sim_bank()
  theta <- seq(-3, 3, length.out = 1000)
  b2 <- utils::modifyList(sb$betas, list(a2 = 1.5))
  X2 <- kits::kits_sim(theta, b2, prefix = "d", seed = 5)
  r2 <- suppressMessages(kitsui_round(sim_long(X2, 2), sb$r1$fixed, "2"))
  expect_equal(r2$fixed$versions$current_id, "a2_r2")
  b3 <- utils::modifyList(sb$betas, list(a2 = -1.5))
  X3 <- kits::kits_sim(theta, b3, prefix = "e", seed = 6)
  r3 <- suppressMessages(kitsui_round(sim_long(X3, 3), r2$fixed, "3"))
  expect_true(r3$ok)
  expect_equal(r3$fixed$versions$item_id, "a2")
  expect_equal(r3$fixed$versions$current_id, "a2_r3")
  expect_equal(r3$fixed$versions$cohort, "M03")
  expect_true("a2_r3" %in% colnames(r3$data[["sim M03"]]))
})
