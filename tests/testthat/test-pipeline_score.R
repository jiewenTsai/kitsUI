test_that("這一輪的分數換算到 D，合成一張表", {
  r8 <- round8()
  lk <- linked()
  out <- kitsui_score(r8$round, r8$df, lk$fixed)
  s <- out$scores
  expect_equal(nrow(s), dplyr::n_distinct(r8$df$visit_id))
  cols <- c("visit_id", "subj_id", "domain", "round", "bank", "scale", "d", "se_d", "se_link",
            "se_total")
  expect_true(all(cols %in% names(s)))
  # 全部換到 D：theta、se 叫 d、se_d（和分數對照表相同）
  expect_false(any(c("theta", "se") %in% names(s)))
  expect_equal(unique(s$scale), "D")
  expect_equal(unique(s$round), "r8")
  expect_setequal(unique(s$bank), c("lan M36", "motor M03"))
  # 和直接呼叫 kits 相同
  m36 <- kits::kits_subj(r8$round$data[["lan M36"]], r8$round$items[["lan M36"]]) |>
    kits::kits_convert(lk$internal, lk$D)
  expect_equal(s$d[s$cohort == "M36"], m36$theta)
  expect_equal(s$se_d[s$cohort == "M36"], m36$se)
  expect_s3_class(kits::kits_subj_table(out$subj[["lan M36"]]), "kits_subj_table")
})

test_that("沒有連結時分數留在校準的量尺", {
  r8 <- round8()
  s <- kitsui_score(r8$round, r8$df)$scores
  expect_setequal(unique(s$scale), c("lan M36", "motor M03"))
  expect_false("se_link" %in% names(s))
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

test_that("題目表：門檻換到 D（和分數同一組連結），通過年齡用這一輪的作答", {
  r8 <- round8()
  lk <- linked()
  tab <- kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36")
  expect_s3_class(tab, "kits_item_table")
  item <- r8$round$items[["lan M36"]]
  thr <- coef(kits::kits_convert(item, lk$internal, lk$D))
  first <- thr[thr$item_score == 1, ]
  expect_equal(tab$D, first$thr[match(tab$item_id, first$item_id)])
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
  expect_true(all(is.na(plain$age50)))
  expect_error(kitsui_item_table(r8$round, r8$df, lk$fixed, "soc M03"), "沒有 soc M03")
})

test_that("兩個世代的垂直連結三個版本：未連結、內部連結後、外部連結後", {
  both <- both_cohorts()
  r <- both$round
  # motor M03 只有 3 個月時 kits 出錯（停下來），所以只有 lan 有兩個世代
  expect_equal(kitsui_vertical_domains(r), "lan")
  expect_equal(kitsui_vertical_domains(round8()$round), character())
  v <- kitsui_verticals(r, both$df, r$fixed, "lan")
  expect_named(v, c("none", "internal", "external"))
  # 未連結：各世代自己的量尺，kits_vertical() 不標量尺
  expect_s3_class(v$none$vertical, "kits_vertical")
  expect_true(is.na(attr(v$none$vertical, "scale")))
  expect_setequal(v$none$vertical$cohort, c("M03", "M36"))
  expect_s3_class(plot(v$none$vertical, type = "median"), "plotly")
  expect_null(v$internal$vertical)
  expect_match(v$internal$why, "lan M36 還沒有內部連結")
  expect_match(v$external$why, "lan M36 還沒有內部連結")

  fixed <- kitsui_link_internal(r$fixed, "lan")$fixed
  v <- kitsui_verticals(r, both$df, fixed, "lan")
  expect_null(v$internal$why)
  expect_equal(v$internal$scale, "lan M03")
  expect_equal(attr(v$internal$vertical, "scale"), "lan M03")
  expect_setequal(v$internal$vertical$cohort, c("M03", "M36"))
  # M03 不換算：未連結與內部連結後相同
  m03 <- \(x) x$median[x$cohort == "M03"]
  expect_equal(m03(v$internal$vertical), m03(v$none$vertical))
  expect_match(v$external$why, "還沒有外部連結")

  # 有外部連結時到 D（M03 要 3、6、12 個月，錨題才夠）
  wide <- two_cohorts_linked()
  expect_equal(kitsui_vertical_domains(wide$round), c("lan", "motor"))
  v <- kitsui_verticals(wide$round, wide$df, wide$fixed, "motor")
  expect_equal(v$external$scale, "D")
  expect_equal(nrow(v$external$vertical), 7)
  expect_equal(v$internal$scale, "motor M03")
  # 和評分（kitsui_score()）同一個換算
  s <- kitsui_score(wide$round, wide$df, wide$fixed)$scores
  s <- s[s$domain == "motor" & s$cohort == "M36" & s$wave_m == 48, ]
  ext <- v$external$vertical
  expect_equal(ext$median[ext$cohort == "M36" & ext$wave_m == 48], stats::median(s$d))
  # 兩個世代有共同的月齡（36、48）：沒有說明
  expect_null(v$external$note)
  one <- kitsui_verticals(round8()$round, round8()$df, linked()$fixed, "lan")
  expect_true(all(purrr::map_lgl(one, \(x) grepl("只有一個世代", x$why))))
})

test_that("分數對照表：theta 在校準的量尺上，d 是換算後的值（表直接換算，不重解）", {
  r <- round8()
  lk <- linked()
  sc <- kitsui_score(r$round, r$df, lk$fixed)
  path <- kitsui_path_links(lk$fixed, kitsui_links(lk$fixed), "lan M36")
  tab <- kitsui_raw_table(sc$own[["lan M36"]], path)
  expect_true(all(c("theta", "se", "d", "se_d", "se_link", "se_total") %in% names(tab)))
  own <- as.data.frame(kits::kits_subj_table(sc$own[["lan M36"]]))
  # 和換算後的分數再做一次 kits_subj_table() 每一欄都相同
  conv <- as.data.frame(kits::kits_subj_table(sc$subj[["lan M36"]]))
  expect_equal(tab$theta, own$theta)
  expect_equal(tab$d, conv$theta)
  expect_equal(tab$se_d, conv$se)
  expect_equal(tab$se_link, conv$se_link)
  expect_equal(tab$se_total, conv$se_total)
  expect_equal(tab[c("n", "pct", "cum_pct", "wave_n", "form_pct")],
               conv[c("n", "pct", "cum_pct", "wave_n", "form_pct")])
  # 沒有連結時只有 theta
  plain <- kitsui_raw_table(sc$own[["lan M36"]])
  expect_false("d" %in% names(plain))
  # 只做了內部連結：換算後的欄叫 theta_lan_M03
  inner <- kitsui_raw_table(sc$own[["lan M36"]], list(lk$internal))
  expect_true(all(c("theta_lan_M03", "se_lan_M03") %in% names(inner)))
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
  expect_true(all(c("theta", "se") %in% names(plain$scores)))
  again <- kitsui_score_convert(plain$own, lk$fixed, links, "r8")
  expect_equal(again$scores, out$scores)
  # 校準量尺上的分數和直接 kits_subj() 相同
  own <- kitsui_own(r8$round, "lan M36", persons)
  direct <- kits::kits_subj(r8$round$data[["lan M36"]], r8$round$items[["lan M36"]],
                            persons = persons)
  expect_equal(own$theta, direct$theta)
})

test_that("分數表的欄：全部到 D 時叫 d、se_d；還有量尺不在 D 時留 theta、se；每一欄一句說明", {
  r8 <- round8()
  lk <- linked()
  links <- kitsui_links(lk$fixed)
  # 只有內部連結（沒有外部連結）：lan M36 到 lan M03，motor M03 留在自己的量尺
  inner <- lk$fixed
  inner$links <- dplyr::filter(inner$links, kind == "internal")
  s <- kitsui_score(r8$round, r8$df, inner)$scores
  expect_setequal(unique(s$scale), c("lan M03", "motor M03"))
  expect_true(all(c("theta", "se", "bank") %in% names(s)))
  expect_equal(unique(s$bank[s$scale == "lan M03"]), "lan M36")
  cols <- kitsui_score_columns(kitsui_score(r8$round, r8$df, lk$fixed, links)$scores)
  expect_named(cols, c("column", "meaning"))
  expect_true(all(c("d", "se_d", "se_link", "se_total", "bank", "round") %in% cols$column))
  expect_false("theta" %in% cols$column)
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
  expect_true(all(c("item_id", "D", "age50") %in% names(shown)))
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
              "lan M36、D", "不會重算")) {
    expect_match(text, x, fixed = TRUE)
  }
  expect_match(kitsui_fixed_readme(kitsui_fixed_empty()), "連結：（還沒有）")
})

test_that("垂直連結：分數算一次（progress 每個世代一次），換算另外做；沒有共同月齡時一句說明", {
  wide <- two_cohorts_linked()
  persons <- kitsui_persons(wide$df)
  seen <- character()
  raw <- kitsui_vertical_scores(wide$round, "lan", persons, \(i, n, s) seen <<- c(seen, s))
  expect_equal(seen, c("lan M03", "lan M36"))
  expect_null(kitsui_vertical_scores(round8()$round, "lan", persons))
  links <- kitsui_links(wide$fixed)
  v <- kitsui_verticals(wide$round, wide$df, wide$fixed, "lan", links, persons, raw)
  expect_equal(v$external$vertical, kitsui_verticals(wide$round, wide$df, wide$fixed,
                                                     "lan")$external$vertical)
  # 連結還沒做時，同一份分數照樣用（只是換不過去）
  none <- kitsui_verticals(wide$round, wide$df, wide$round$fixed, "lan", list(), persons, raw)
  expect_match(none$internal$why, "還沒有內部連結")
  fake <- list(`lan M03` = data.frame(wave_m = c(3, 6)), `lan M36` = data.frame(wave_m = 84))
  note <- kitsui_vertical_no_common(fake)
  expect_match(note, "沒有共同的月齡")
  expect_match(note, "lan M03：3、6 個月")
  expect_null(kitsui_vertical_no_common(raw))
})
