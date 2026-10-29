test_that("題庫的門檻（algo_thresholds）和 kits_item 的門檻相同", {
  r <- round8()$round
  thr <- kitsui_bank_thr(r$fixed, "lan M36")
  co <- coef(r$items[["lan M36"]])
  co <- co[match(paste(thr$item_id, thr$item_score), paste(co$item_id, co$item_score)), ]
  expect_equal(thr$thr, co$thr, tolerance = 1e-10)
  expect_equal(attr(thr, "scale"), "lan M36")
})

test_that("錨題不到 3 個時沒有外部連結，說明原因", {
  out <- kitsui_link_D(lan_chain()[["48"]]$fixed)
  expect_null(out$link)
  expect_match(out$message, "at least 3")
  expect_equal(nrow(out$fixed$links), 0)
})

test_that("內部連結與外部連結各做一次，之後沿用", {
  lk <- linked()
  expect_equal(lk$internal$from_scale, "lan M36")
  expect_equal(lk$internal$to_scale, "lan M03")
  expect_equal(lk$D$to_scale, "D")
  expect_setequal(names(kitsui_links(lk$fixed)), c("lan M36", "D"))
  again <- kitsui_link_internal(lk$fixed, "lan")
  expect_false(again$new)
  expect_identical(again$fixed, lk$fixed)
  expect_identical(coef(again$link), coef(lk$internal))
  expect_false(kitsui_link_D(lk$fixed)$new)
  expect_null(kitsui_link_internal(lk$fixed, "motor")$link)
})

test_that("換算路線：M36 先內部連結再外部連結，M03 直接外部連結", {
  fixed <- linked()$fixed
  expect_equal(kitsui_link_path(fixed, "lan M36"), c("lan M36", "D"))
  expect_equal(kitsui_link_path(fixed, "motor M03"), "D")
  expect_equal(kitsui_link_path(round8()$round$fixed, "lan M36"), character())
})

test_that("從點重建的連結和 kits_linking() 相同（題數、設定、診斷）；links.csv 記下哪一輪做的", {
  lk <- linked()
  links <- kitsui_links(lk$fixed)
  expect_equal(unique(lk$fixed$links$round), "r8")
  for (nm in c("lan M36", "D")) {
    orig <- if (nm == "D") lk$D else lk$internal
    re <- links[[nm]]
    expect_identical(re$n_items, orig$n_items)
    expect_identical(re$items_total, orig$items_total)
    expect_equal(re$settings[c("kind", "step")], orig$settings[c("kind", "step")])
    expect_equal(as.data.frame(re$diagnostics), as.data.frame(orig$diagnostics))
    expect_equal(as.data.frame(re$points), as.data.frame(orig$points))
  }
  tab <- kitsui_link_table(lk$fixed, links, earlier = "lan M36")
  expect_named(tab, c("link", "kind", "from", "to", "intercept", "slope", "made", "round"))
  expect_equal(tab$round, c("r8", "r8"))
  expect_equal(tab$made[tab$link == "lan M36"], "之前的輪")
  expect_equal(tab$kind, c("外部連結", "內部連結"))
})

test_that("從點重建的連結可以做獨立檢核（kits_link_check()）", {
  skip_on_cran()
  both <- both_cohorts()
  made <- kitsui_link_internal(both$round$fixed, "lan")
  rebuilt <- kitsui_links(made$fixed)[["lan M36"]]
  expect_null(kitsui_link_check_why(both$round, rebuilt))
  expect_match(kitsui_link_check_why(round8()$round, linked()$internal), "lan M03")
  chk <- kitsui_link_check(both$round, both$df, rebuilt, kitsui_persons(both$df))
  expect_s3_class(chk, "kits_link_check")
  expect_match(chk$methods$basis[1], paste("of", made$link$n_items, "items"))
  # 小表：每個方法一列，差距與中文判斷（規則和 kits_check() 相同：2 個標準誤）
  tab <- kitsui_link_check_table(chk)
  expect_named(tab, c("method", "intercept", "slope", "max_diff", "max_z", "judgement"))
  expect_equal(tab$method[1], "共同題目（連結本身）")
  expect_equal(tab$max_diff[1], 0)
  expect_match(tab$method[2], "^同年齡組（36 \\+ 48 個月）$")
  expect_true(all(grepl("^(這就是|一致|不一致)", tab$judgement)))
  expect_match(attr(tab, "rule"), "2 個標準誤")
  # concurrent CML 差 2.27 個連結誤差：kits_check() 是 note，這裡是不一致
  cml <- tab[grepl("concurrent", tab$method), ]
  expect_match(cml$judgement, "^不一致")
  expect_equal(cml$max_z, max(abs(chk$at$z_both[chk$at$method == "concurrent CML"])))
  # 沒有重抽時用 tol
  chk$at$z_both <- NA
  expect_match(attr(kitsui_link_check_table(chk), "rule"), "沒有重抽")
  expect_false("max_z" %in% names(kitsui_link_check_table(chk)))
})

test_that("錨題數；內部連結與外部連結的狀態", {
  fixed <- round8()$round$fixed
  anchors <- kitsui_anchor_counts(fixed)
  expect_equal(anchors$scale, c("lan M03", "motor M03"))
  expect_equal(anchors$n_anchors, c(2L, 5L))
  expect_equal(kitsui_internal_scales(fixed), "lan M36")
  st <- kitsui_internal_state(fixed, "lan M36", list(), character())
  expect_equal(st$state, "ready")
  expect_equal(kitsui_d_state(fixed, list(), character(), anchors)$state, "ready")
  lk <- linked()
  frozen <- kitsui_d_state(lk$fixed, kitsui_links(lk$fixed), "D", anchors)
  expect_equal(frozen$state, "frozen")
  expect_match(frozen$text, "之前的輪")
  expect_equal(kitsui_link_names(lk$fixed), c("lan M36", "D"))
})

test_that("外部連結凍結後，新的 M03 領域沿用同一條外部連結；早先的 D 分數不變", {
  lk <- linked()
  r8 <- round8()
  # 第三個領域：motor M03 3 個月的題目改名（不是錨題），當作 soc
  df <- kitsui_demo("M03", 3, "motor") |>
    dplyr::mutate(domain = "soc", item_id = paste0("soc_", item_id))
  r9 <- suppressMessages(kitsui_round(df, lk$fixed, "r9"))
  expect_true(r9$ok)
  ext <- dplyr::filter(r9$fixed$checks, check == "D extended")
  expect_equal(ext$scale, "soc M03")
  expect_equal(ext$round, "r9")
  expect_true("D extended" %in% r9$log$check)

  links <- kitsui_links(r9$fixed)
  expect_equal(links$D$from_scale, c("lan M03", "motor M03", "soc M03"))
  expect_identical(coef(links$D), coef(lk$D))
  expect_identical(links$D$points$from, lk$D$points$from)
  expect_identical(links$D$intercept_error, lk$D$intercept_error)
  expect_equal(kitsui_link_path(r9$fixed, "soc M03"), "D")

  # soc 的分數用同一條線換到 D
  s9 <- kitsui_score(r9, df, r9$fixed)$scores
  expect_equal(unique(s9$scale), "D")
  theta <- kits::kits_subj(r9$data[["soc M03"]], r9$items[["soc M03"]],
                           persons = kitsui_persons(df))$theta
  co <- coef(lk$D)
  expect_equal(s9$d, co[["intercept"]] + co[["slope"]] * theta)

  # 早先的 lan、motor 的 D 分數不變
  before <- kitsui_score(r8$round, r8$df, lk$fixed)$scores
  after <- kitsui_score(r8$round, r8$df, r9$fixed)$scores
  expect_identical(after$d, before$d)
  expect_identical(after$se_total, before$se_total)

  # 寫出再讀回：延伸後的外部連結；③ 的表標出哪一輪延伸
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(r9$fixed, zip_path)
  expect_identical(kitsui_read_fixed(zip_path)$links, r9$fixed$links)
  cov <- kitsui_d_coverage(r9$fixed, kitsui_anchor_counts(r9$fixed))
  expect_equal(cov$external_link[cov$scale == "soc M03"], "第 r9 輪延伸（同一條線）")
  expect_equal(cov$external_link[cov$scale == "lan M03"], "建立外部連結時（第 r8 輪）")
  # 再一輪沒有新的 M03 量尺：不再延伸
  again <- kitsui_link_D_extend(r9$fixed, "r10")
  expect_null(again$log)
  expect_identical(again$fixed, r9$fixed)
})

test_that("連結公式：一行文字與 intercept、slope 的 linking error", {
  l <- linked()$D
  f <- kitsui_link_formula(l)
  expect_match(f$formula, "^D = [0-9.]+ \\+ [0-9.]+ × lan M03")
  expect_equal(f$table$estimate, c(l$intercept, l$slope))
  expect_equal(f$table$linking_error, c(l$intercept_error, l$slope_error))
})

test_that("外部連結：參照世代有五個領域（錨題只在 lan、motor），其他領域用同一條線", {
  # 示範資料改名當五個領域：motor 的題複製成 social，lan 的題複製成 cog、emotion
  base <- kitsui_demo("M03", c(3, 6), c("lan", "motor"))
  copy <- \(from, to, prefix) {
    dplyr::filter(base, domain == from) |>
      dplyr::mutate(domain = to, item_id = paste0(prefix, item_id))
  }
  df <- dplyr::bind_rows(base, copy("motor", "social", "soc_"), copy("lan", "cog", "cog_"),
                         copy("lan", "emotion", "emo_"))
  r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "1"))
  expect_true(r$ok)
  ref <- kitsui_ref_scales(r$fixed)
  expect_setequal(scale_domain(ref), c("cog", "emotion", "lan", "motor", "social"))
  d <- kitsui_link_D(r$fixed)
  expect_true(d$new)
  expect_setequal(d$link$from_scale, ref)
  expect_setequal(d$link$settings$no_anchors, c("cog", "emotion", "social"))
  # 直接呼叫 kits 也一樣
  thr <- purrr::map(purrr::set_names(ref, scale_domain(ref)), \(s) kitsui_bank_thr(r$fixed, s))
  direct <- kits::kits_linking(thr, kits::kits_anchors(), n_boot = 2000, seed = 1)
  expect_equal(coef(d$link), coef(direct))
  # 從點重建：係數、沒有錨題的領域都相同；五個量尺的分數都換到 D
  re <- kitsui_links(d$fixed)$D
  expect_identical(coef(re), coef(d$link))
  expect_setequal(re$settings$no_anchors, d$link$settings$no_anchors)
  s <- kitsui_score(r, df, d$fixed)$scores
  expect_equal(unique(s$scale), "D")
  expect_setequal(unique(s$domain), c("cog", "emotion", "lan", "motor", "social"))
  cov <- kitsui_d_coverage(d$fixed, kitsui_anchor_counts(d$fixed))
  expect_equal(cov$n_anchors[cov$scale == "social M03"], 0)
})

test_that("連結只重建需要的；沒有連結時換算路線是空的（不出錯）", {
  lk <- linked()
  only <- kitsui_links(lk$fixed, "D")
  expect_named(only, "D")
  expect_equal(coef(only$D), coef(kitsui_links(lk$fixed)$D))
  expect_identical(kitsui_links(kitsui_fixed_empty()), list())
  expect_identical(kitsui_links(list(links = NULL)), list())
  expect_identical(kitsui_link_path(list(links = NULL), "lan M36"), character())
  expect_identical(kitsui_link_path(NULL, "lan M36"), character())
  expect_identical(kitsui_link_path(kitsui_fixed_empty(), "lan M36"), character())
  # 已有的連結：沿用，訊息用「固定檔」
  again <- kitsui_link_D(lk$fixed)
  expect_false(again$new)
  expect_equal(again$message, "沿用固定檔裡的連結。")
})

test_that("凍結的說明：之前的輪做的不說是上傳的（這次連線交過來的也一樣）", {
  before <- kitsui_frozen_text(TRUE)
  expect_match(before, "這一輪開始時的固定檔")
  expect_false(grepl("上傳", before))
  expect_false(grepl(paste0("固定", "集"), before))
  expect_match(kitsui_frozen_text(FALSE), "這一輪剛做好")
})

test_that("檢查表加上一句中文說明：領域的平衡、各領域截距的差距", {
  lk <- linked()
  tab <- kitsui_check_explain(check_table(lk$D))
  expect_true("explain" %in% names(tab))
  bal <- tab$explain[tab$check == "domain balance of kept points"]
  expect_match(bal, "三分之二")
  gap <- tab$explain[grepl("domain intercept", tab$check)]
  expect_match(gap, "一條共用的線")
  # 不認得的檢查：空白；沒有 check 欄：原樣
  odd <- kitsui_check_explain(data.frame(check = "something new"))
  expect_true(is.na(odd$explain))
  ok <- data.frame(result = "全部通過")
  expect_identical(kitsui_check_explain(ok), ok)
  expect_null(kitsui_check_explain(NULL))
})
