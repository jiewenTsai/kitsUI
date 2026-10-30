test_that("這一回合的分數換算到 D，合成一張表", {
  r8 <- round8()
  lk <- linked()
  out <- kitsui_score(r8$round, r8$df, lk$fixed)
  s <- out$scores
  expect_equal(nrow(s), dplyr::n_distinct(r8$df$visit_id))
  cols <- c("visit_id", "subj_id", "domain", "round", "bank", "scale", "ability", "ability_se",
            "d_score", "d_score_se", "se_link", "se_total")
  expect_true(all(cols %in% names(s)))
  # 分開校準的 ability 與換到 D 的 d_score 都在
  expect_false("se" %in% names(s))
  own <- kits::kits_subj(r8$round$data[["lan M36"]], r8$round$items[["lan M36"]])
  expect_equal(s$ability[s$cohort == "M36"], own$theta)
  expect_equal(s$ability_se[s$cohort == "M36"], own$se)
  expect_equal(unique(s$scale), "D")
  expect_equal(unique(s$round), "r8")
  expect_setequal(unique(s$bank), c("lan M36", "motor M03"))
  # 和直接呼叫 kits 相同
  m36 <- kits::kits_subj(r8$round$data[["lan M36"]], r8$round$items[["lan M36"]]) |>
    kits::kits_convert(lk$internal, lk$D)
  expect_equal(s$d_score[s$cohort == "M36"], m36$theta)
  expect_equal(s$d_score_se[s$cohort == "M36"], m36$se)
  expect_s3_class(kits::kits_subj_table(out$subj[["lan M36"]]), "kits_subj_table")
})

test_that("沒有連結時分數留在校準的量尺", {
  r8 <- round8()
  s <- kitsui_score(r8$round, r8$df)$scores
  expect_setequal(unique(s$scale), c("lan M36", "motor M03"))
  expect_true(all(is.na(s$d_score)))
  expect_true(all(is.na(s$se_link)))
})

test_that("換算路線與找人", {
  r8 <- round8()
  lk <- linked()
  out <- kitsui_score(r8$round, r8$df, lk$fixed)
  routes <- kitsui_score_routes(lk$fixed, out$subj, r8$round$plan)
  expect_named(routes, c("bank", "data", "route", "final_scale", "n", "note"))
  expect_equal(routes$route[routes$bank == "lan M36"], "lan M36 → lan M03 → D")
  expect_equal(routes$route[routes$bank == "motor M03"], "motor M03 → D")
  expect_equal(routes$final_scale, c("D", "D"))
  expect_equal(routes$data[routes$bank == "lan M36"], "lan M36（36、48 個月）")
  # 波次單位是年時，data 也寫年
  fy <- lk$fixed
  fy$version$wave_unit <- "years"
  ry <- kitsui_score_routes(fy, out$subj, r8$round$plan)
  expect_equal(ry$data[ry$bank == "lan M36"], "lan M36（36、48 年）")
  # 沒有借用另一世代題庫：沒有說明
  expect_true(all(is.na(routes$note)))
  # 沒有 plan 時 data 就是題庫自己
  expect_equal(kitsui_score_routes(lk$fixed, out$subj)$data, routes$bank)
  s <- out$scores
  expect_identical(kitsui_score_search(s, "  "), s)
  hit <- kitsui_score_search(s, s$visit_id[3])
  expect_true(nrow(hit) >= 1)
  expect_true(s$visit_id[3] %in% hit$visit_id)
  expect_equal(nrow(kitsui_score_search(s, "沒有這個人")), 0)
})

test_that("題目表：三種門檻（校準的量尺、內部連結後、D），通過年齡用這一回合的作答", {
  r8 <- round8()
  lk <- linked()
  tab <- kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36")
  expect_s3_class(tab, "kits_item_table")
  item <- r8$round$items[["lan M36"]]
  step1 <- \(x) {
    co <- coef(x)
    co[co$item_score == 1, ]
  }
  own <- step1(item)
  expect_equal(tab$D, own$thr[match(tab$item_id, own$item_id)])
  # 另外兩把尺：和分數同一組連結
  extra <- attr(tab, "thresholds")
  inner <- step1(kits::kits_convert(item, lk$internal))
  full <- step1(kits::kits_convert(item, lk$internal, lk$D))
  expect_equal(extra$ability_linked_threshold, inner$thr[match(extra$item_id, inner$item_id)])
  expect_equal(extra$d_threshold, full$thr[match(extra$item_id, full$item_id)])
  shown <- kitsui_item_table_shown(tab)
  expect_equal(names(shown)[2:4], KITSUI_ITEM_THRESHOLDS)
  expect_false(is.unsorted(shown$d_threshold, na.rm = TRUE))
  expect_true("age50" %in% names(tab))
  expect_true(any(!is.na(tab$age50)))
  # 沒有常模：沒有常模算的年齡
  expect_false("age_reach" %in% names(tab))
  # 沒有連結時留在校準的量尺；沒有 age_m 時只有 D
  plain <- kitsui_item_table(r8$round, dplyr::select(r8$df, -"age_m"), r8$round$fixed,
                             "motor M03")
  co <- coef(r8$round$items[["motor M03"]])
  co <- co[co$item_score == 1, ]
  expect_equal(plain$D, co$thr[match(plain$item_id, co$item_id)])
  expect_null(attr(plain, "thresholds"))
  expect_equal(names(kitsui_item_table_shown(plain))[2], "ability_threshold")
  expect_true(all(is.na(plain$age50)))
  expect_error(kitsui_item_table(r8$round, r8$df, lk$fixed, "soc M03"), "沒有 soc M03")
})

test_that("分數對照表：ability 在校準的量尺上、ability_linked 經過內部連結、d_score 換到 D", {
  r <- round8()
  lk <- linked()
  sc <- kitsui_score(r$round, r$df, lk$fixed)
  path <- kitsui_path_links(lk$fixed, kitsui_links(lk$fixed), "lan M36")
  internal <- kitsui_path_links(lk$fixed, kitsui_links(lk$fixed), "lan M36", stage = "internal")
  tab <- kitsui_raw_table(sc$own[["lan M36"]], path, internal)
  expect_true(all(c("ability", "ability_se", "ability_linked", "ability_linked_se", "d_score",
                    "d_score_se", "se_link", "se_total") %in% names(tab)))
  own <- kits::kits_subj_table(sc$own[["lan M36"]])
  expect_equal(tab$ability, as.data.frame(own)$theta)
  # ability_linked：經過內部連結（M36 -> M03）
  inner_t <- as.data.frame(kits::kits_convert(own, lk$internal))
  expect_equal(tab$ability_linked, inner_t$theta)
  expect_equal(tab$ability_linked_se, inner_t$se)
  # d：和換算後的分數再做一次 kits_subj_table() 每一欄都相同
  conv <- as.data.frame(kits::kits_subj_table(sc$subj[["lan M36"]]))
  expect_equal(tab$d_score, conv$theta)
  expect_equal(tab$d_score_se, conv$se)
  expect_equal(tab$se_link, conv$se_link)
  expect_equal(tab$se_total, conv$se_total)
  expect_equal(tab[c("n", "pct", "cum_pct", "wave_n", "form_pct")],
               conv[c("n", "pct", "cum_pct", "wave_n", "form_pct")])
  # 沒有連結時只有 ability（校準的量尺）
  plain <- kitsui_raw_table(sc$own[["lan M36"]])
  expect_false(any(c("d_score", "ability_linked") %in% names(plain)))
  expect_equal(plain$ability, as.data.frame(own)$theta)
  # 只做了內部連結：ability_linked 換到 M03，沒有 d_score
  inner <- kitsui_raw_table(sc$own[["lan M36"]], list(lk$internal), list(lk$internal))
  expect_equal(inner$ability_linked, inner_t$theta)
  expect_false("d_score" %in% names(inner))
  # 畫面上預設不列沒有人的原始分數
  expect_true(any(tab$n == 0))
  shown <- kitsui_raw_table_shown(tab)
  expect_true(all(shown$n > 0))
  expect_equal(nrow(kitsui_raw_table_shown(tab, TRUE)), nrow(tab))
})

test_that("分數只算一次：換算只要校準量尺上的分數與連結；傳進來的連結與列次資訊結果相同", {
  r8 <- round8()
  lk <- linked()
  persons <- kitsui_persons(r8$df)
  seen <- character()
  prog <- \(i, n, s) seen <<- c(seen, sprintf("%d/%d %s", i, n, s))
  links <- kitsui_links(lk$fixed)
  out <- kitsui_score(r8$round, r8$df, lk$fixed, links = links, persons = persons,
                      progress = prog)
  expect_equal(seen, c("1/2 motor M03", "2/2 lan M36"))
  expect_equal(out$scores, kitsui_score(r8$round, r8$df, lk$fixed)$scores)
  # 只有連結變了：用同一組校準量尺上的分數重新換算
  plain <- kitsui_score_convert(out$own, r8$round$fixed, round_label = "r8")
  expect_setequal(unique(plain$scores$scale), c("lan M36", "motor M03"))
  expect_true(all(c("ability", "ability_se") %in% names(plain$scores)))
  again <- kitsui_score_convert(plain$own, lk$fixed, links, "r8")
  expect_equal(again$scores, out$scores)
  # 校準量尺上的分數和直接 kits_subj() 相同
  own <- kitsui_own(r8$round, "lan M36", persons)
  direct <- kits::kits_subj(r8$round$data[["lan M36"]], r8$round$items[["lan M36"]],
                            persons = persons)
  expect_equal(own$theta, direct$theta)
})

test_that("分數表的欄：ability、ability_se 一直都在；不在 D 的量尺 d_score 是空白；每一欄一句說明", {
  r8 <- round8()
  lk <- linked()
  links <- kitsui_links(lk$fixed)
  # 只有內部連結（沒有外部連結）：lan M36 到 lan M03，motor M03 留在自己的量尺
  inner <- lk$fixed
  inner$links <- dplyr::filter(inner$links, kind == "internal")
  s <- kitsui_score(r8$round, r8$df, inner)$scores
  expect_setequal(unique(s$scale), c("lan M03", "motor M03"))
  expect_true(all(c("ability", "ability_se", "d_score", "d_score_se", "bank") %in% names(s)))
  expect_true(all(is.na(s$d_score)))
  expect_equal(unique(s$bank[s$scale == "lan M03"]), "lan M36")
  cols <- kitsui_score_columns(kitsui_score(r8$round, r8$df, lk$fixed, links)$scores)
  expect_named(cols, c("column", "meaning"))
  expect_true(all(c("ability", "ability_se", "d_score", "d_score_se", "se_link", "se_total", "bank",
                    "round") %in%
                    cols$column))
  expect_match(cols$meaning[cols$column == "bank"], "借用另一世代題庫")
})

test_that("借用另一世代題庫：路線表寫出是 M03 的資料、用 motor M36 的題庫", {
  bc <- borrow_chain()
  r2 <- bc$r2
  sc <- kitsui_score(r2, bc$df2, r2$fixed)
  routes <- kitsui_score_routes(r2$fixed, sc$subj, r2$plan)
  expect_equal(routes$bank, "motor M36")
  expect_equal(routes$data, "motor M03（60 個月）")
  expect_match(routes$note, "M03 的資料，借用另一世代題庫（motor M36）")
  expect_equal(unique(sc$scores$bank), "motor M36")
  expect_equal(unique(sc$scores$cohort), "M03")
})

test_that("題目表：每一列都沒有內容的欄不列出（domain、column、stem、DIF）", {
  r8 <- round8()
  lk <- linked()
  links <- kitsui_links(lk$fixed)
  tab <- kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36", links, kitsui_persons(r8$df))
  expect_equal(tab$D, kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36")$D)
  shown <- kitsui_item_table_shown(tab)
  expect_false(any(c("domain", "column", "stem", "DIF") %in% names(shown)))
  expect_setequal(attr(shown, "dropped"), c("domain", "column", "stem", "DIF"))
  expect_true(all(c("item_id", "d_threshold", "age50") %in% names(shown)))
  # 有內容時留著
  tab$DIF[1] <- "favors girls"
  tab$stem[1] <- "說出兩個字"
  kept <- kitsui_item_table_shown(tab)
  expect_true(all(c("DIF", "stem") %in% names(kept)))
})

test_that("② 的標記帶到 ④；新固定檔的檔名與說明文字", {
  r8 <- round8()
  expect_equal(nrow(kitsui_round_flags(r8$round)), 0)
  round <- r8$round
  round$log <- dplyr::bind_rows(round$log, tibble::tibble(
    round = "r8", scale = "lan M36", item_id = NA, check = "flag", detail = "偏移 5 題"
  ))
  flags <- kitsui_round_flags(round)
  expect_equal(flags$scale, "lan M36")
  expect_equal(flags$detail, "偏移 5 題")
  expect_equal(nrow(kitsui_round_flags(NULL)), 0)
  expect_equal(kitsui_fixed_name("r8"), "kitsui_fixed_r8.zip")
  text <- kitsui_fixed_readme(linked()$fixed)
  expect_type(text, "character")
  expect_length(text, 1)
  for (x in c("bank.csv", "links.csv", "versions.csv", "waves.csv", "checks.csv", "VERSION",
              "lan M36, D", "not recomputed")) {
    expect_match(text, x, fixed = TRUE)
  }
  expect_match(kitsui_fixed_readme(kitsui_fixed_empty()), "Links: (none yet)", fixed = TRUE)
})

test_that("題目表依最後一把尺的門檻由小到大；kits 的 D 欄叫 ability_threshold", {
  tab <- data.frame(item_id = c("a", "b", "c"), D = c(-1, -2, NA), age50 = c(9, 3, NA))
  out <- kitsui_item_table_shown(tab)
  expect_equal(out$item_id, c("b", "a", "c"))
  expect_equal(out$ability_threshold, c(-2, -1, NA))
  attr(tab, "thresholds") <- tibble::tibble(item_id = c("a", "b", "c"), d_threshold = c(20, 40, NA))
  out <- kitsui_item_table_shown(tab)
  expect_equal(out$item_id, c("a", "b", "c"))
  expect_equal(names(out), c("item_id", "ability_threshold", "d_threshold", "age50"))
})
