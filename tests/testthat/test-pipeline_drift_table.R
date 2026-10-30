# 偏移表：判斷用的線（第一條線）與最後的線分開寫，步驟數、拿掉的步驟（kitsui.md 發現 3）

# 和 TDCYP 的 bsrs 一樣的情況：5 題各 3 步，第 1 步上下的人不夠（沒有判斷，離線很遠），
# bsrs1 的第 3 步偏移。kits 的 max_abs_resid 對最後的線、包括沒有判斷的步驟，所以偏移的
# bsrs1 比沒有偏移的題小
fake_drift <- function() {
  ids <- paste0("bsrs", 1:5)
  pts <- expand.grid(item_score = 1:3, item_id = ids, stringsAsFactors = FALSE)
  pts <- pts[c("item_id", "item_score")]
  pts$thr_ref <- rep(c(-1, 0, 1), 5) + rep(seq(-0.4, 0.4, length.out = 5), each = 3)
  noise <- c(0.05, 0.05, -0.04, 0, 0.06, -0.05, 0.64, -0.03, 0.04, -0.66, 0.02, -0.06,
             0.58, -0.04, 0.03)
  pts$thr_new <- pts$thr_ref + noise
  pts$thr_new[3] <- pts$thr_ref[3] + 0.30
  pts$n_below <- ifelse(pts$item_score == 1, 12L, 200L)
  pts$n_above <- 300L
  # bsrs2 的第 1 步人夠，有判斷
  pts$judged <- pts$item_score > 1 | pts$item_id == "bsrs2"
  use <- pts[pts$judged, ]
  sc <- kits::algo_screen(use$thr_new, use$thr_ref, use$item_id)
  final <- kits::algo_mean_sigma(sc)
  pts$resid <- pts$thr_ref - final$intercept - final$slope * pts$thr_new
  pts$dropped <- NA
  pts$dropped[pts$judged] <- sc$dropped
  items <- data.frame(item_id = ids)
  items$max_abs_resid <- as.vector(tapply(abs(pts$resid), pts$item_id, max)[ids])
  items$drift <- ids == "bsrs1"
  items$suggest_fix <- !items$drift
  items$reason <- ""
  structure(list(items = items, points = pts, intercept = final$intercept, slope = final$slope,
                 info = list(settings = list(cut = 2, min_n = 30))),
            class = "kits_drift")
}

test_that("情境：偏移的題 max_abs_resid 比沒有偏移的題小（沒有判斷的步驟、最後的線）", {
  d <- fake_drift()
  mx <- stats::setNames(d$items$max_abs_resid, d$items$item_id)
  expect_true(all(mx["bsrs1"] < mx[c("bsrs3", "bsrs4", "bsrs5")]))
})

test_that("kitsui_drift_judging()：重算第一條線，和 kits 拿掉的步驟相同；界線 = cut × SD", {
  d <- fake_drift()
  jd <- kitsui_drift_judging(d)
  expect_equal(jd$n, 11L)
  expect_equal(jd$cut, 2)
  use <- d$points[d$points$judged, ]
  first <- kits::algo_mean_sigma(use$thr_new, use$thr_ref)
  r <- use$thr_ref - first$intercept - first$slope * use$thr_new
  expect_equal(jd$points$resid_judging, r)
  expect_equal(jd$limit, 2 * stats::sd(r))
  expect_equal(abs(r) > jd$limit, use$dropped)
  # kits_linking() 的第一條線也一樣
  lk <- kits::kits_linking(data.frame(item_id = use$item_id, item_score = use$item_score,
                                      thr = use$thr_new),
                           data.frame(item_id = use$item_id, item_score = use$item_score,
                                      thr = use$thr_ref))
  expect_equal(lk$points$resid, r)
  # 重算出來和 kits 的判斷不一樣時：NULL（不顯示假的數字）
  bad <- d
  bad$points$dropped[bad$points$judged] <- FALSE
  expect_null(kitsui_drift_judging(bad))
  expect_null(kitsui_drift_judging(NULL))
})

test_that("kitsui_drift_items()：步驟數、拿掉的步驟、判斷用的距離；max_abs_resid 改名", {
  d <- fake_drift()
  tab <- kitsui_drift_items(d)
  expect_named(tab, c("item_id", "steps", "steps_judged", "steps_dropped", "max_dist_judging",
                      "max_dist_final", "drift", "suggest_fix", "reason"))
  expect_equal(tab$steps, rep(3L, 5))
  expect_equal(tab$steps_judged, c(2L, 3L, 2L, 2L, 2L))
  expect_equal(tab$steps_dropped, c("3", "", "", "", ""))
  expect_equal(tab$max_dist_final, d$items$max_abs_resid)
  # 判斷用的數字：偏移的題大於界線、其他題不大於界線，排序和 max_abs_resid 相反
  limit <- attr(tab, "judging")$limit
  expect_gt(tab$max_dist_judging[1], limit)
  expect_true(all(tab$max_dist_judging[-1] <= limit))
  expect_true(all(tab$max_dist_judging[1] > tab$max_dist_judging[-1]))
  expect_true(tab$max_dist_final[1] < max(tab$max_dist_final[-1]))
  # 判斷不變
  expect_equal(tab$drift, d$items$drift)
  expect_equal(tab$suggest_fix, d$items$suggest_fix)
  # kits 沒有比較的題（reason 不是空的，沒有點）：0 步、距離空白
  d$items <- rbind(d$items, data.frame(item_id = "bsrs6", max_abs_resid = NA, drift = NA,
                                       suggest_fix = TRUE, reason = "score range differs"))
  tab <- kitsui_drift_items(d)
  expect_equal(tab$steps[6], 0L)
  expect_equal(tab$steps_dropped[6], "")
  expect_true(is.na(tab$max_dist_judging[6]))
})

test_that("紀錄的說明：寫被拿掉的步驟與它離判斷用的線的距離，不再寫 max_abs_resid", {
  d <- fake_drift()
  jd <- kitsui_drift_judging(d)
  x <- kitsui_drift_detail(d, "bsrs1")
  dist <- sprintf("%.2f", abs(jd$points$resid_judging[jd$points$dropped]))
  expect_equal(x$zh, sprintf(
    "第 3 步離判斷用的線 %s logit，超過界線 %.2f logit（2 個殘差標準差，11 個判斷的步驟算的）",
    dist, jd$limit
  ))
  expect_equal(x$en, sprintf(
    "step 3 is %s logit from the judging line, beyond the limit %.2f logit (2 residual SD of the 11 judged steps)",
    dist, jd$limit
  ))
  expect_false(grepl(sprintf("%.2f", d$items$max_abs_resid[1]), x$zh))
  expect_false(any(grepl("[^ -~]", x$en)))
  # 重算不出來時：只說被拿掉，不寫數字
  bad <- d
  bad$points$dropped[2] <- TRUE
  y <- kitsui_drift_detail(bad, "bsrs1")
  expect_match(y$zh, "^第 2、3 步被拿掉：離判斷用的線")
  expect_false(grepl("[0-9]\\.[0-9]", y$zh))
  expect_match(y$en, "^steps 2, 3 dropped: more than 2 residual SD")
  expect_equal(kitsui_drift_detail(d, character()), list(zh = character(), en = character()))
})

test_that("偏移表（畫面與 csv）與表下面的說明", {
  d <- fake_drift()
  x <- list(drift = d, log = tibble::tibble(item_id = character(), check = character(),
                                            detail = character()))
  rows <- items_drift_rows(x)
  expect_true("judging_limit" %in% names(rows))
  expect_equal(unique(rows$judging_limit), kitsui_drift_judging(d)$limit)
  shown <- items_drift_display(rows)
  expect_named(shown, c("item_id", "steps", "steps_judged", "steps_dropped", "max_dist_judging",
                        "max_dist_final", "drift", "suggest_fix", "reason"))
  expect_false("max_abs_resid" %in% names(rows))
  note <- as.character(items_drift_note(d))
  expect_match(note, "判斷用的線")
  expect_match(note, "最後的線")
  expect_match(note, "kits 裡叫 max_abs_resid")
  expect_match(note, sprintf("界線是 2 × %.2f = %.2f logit", kitsui_drift_judging(d)$sd,
                             kitsui_drift_judging(d)$limit), fixed = TRUE)
  expect_match(note, "11 個判斷的步驟")
  expect_null(items_drift_note(NULL))
  bad <- d
  bad$points$dropped[bad$points$judged] <- FALSE
  expect_match(as.character(items_drift_note(bad)), "重算不出 kits 判斷用的線")
})

test_that("真的回合：偏移表的欄與紀錄的說明一致，偏移的題 max_dist_judging 大於界線", {
  r <- lan_chain()[["24"]]
  drift <- r$drift[["lan M03"]]
  tab <- kitsui_drift_items(drift)
  jd <- attr(tab, "judging")
  expect_false(is.null(jd))
  moved <- tab$drift %in% TRUE
  expect_true(any(moved))
  expect_true(all(tab$max_dist_judging[moved] > jd$limit))
  expect_true(all(tab$max_dist_judging[tab$drift %in% FALSE] <= jd$limit))
  expect_true(all(nzchar(tab$steps_dropped[moved])))
  expect_true(all(!nzchar(tab$steps_dropped[!moved])))
  expect_equal(sum(tab$steps), nrow(drift$points))
  expect_equal(sum(tab$steps_judged), sum(drift$points$judged))
  log <- dplyr::filter(r$log, check == "drift", scale == "lan M03")
  expect_equal(log$item_id, tab$item_id[moved])
  expect_match(log$detail, "離判斷用的線 [0-9.]+ logit，超過界線 [0-9.]+ logit")
  expect_match(log$detail_en, "logit from the judging line, beyond the limit")
})
