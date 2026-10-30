# 垂直連結：每一回合的各波次摘要（固定檔的 wave_summary）、三個版本、之前回合的點

test_that("每一回合把各量尺 × 世代 × 波次的摘要寫進固定檔，之前的列都留著", {
  chain <- lan_chain()
  ws3 <- chain[["3"]]$fixed$wave_summary
  expect_equal(nrow(ws3), 1)
  expect_equal(ws3$scale, "lan M03")
  expect_equal(ws3$cohort, "M03")
  expect_equal(ws3$round, "3")
  expect_named(ws3, names(KITSUI_FIXED_COLS$wave_summary))
  expect_type(ws3$n, "integer")
  ws48 <- chain[["48"]]$fixed$wave_summary
  expect_equal(ws48$wave_m, c(3, 6, 12, 18, 24, 36, 48))
  expect_equal(ws48$round, c("3", "6", "12", "18", "24", "36", "48"))
  # 第 8 回合：lan M36（36、48 個月）與 motor M03（3 個月）
  ws8 <- round8()$round$fixed$wave_summary
  expect_equal(nrow(ws8), 7 + 3)
  new <- ws8[ws8$round == "r8", ]
  expect_setequal(paste(new$scale, new$cohort, new$wave_m),
                  c("lan M36 M36 36", "lan M36 M36 48", "motor M03 M03 3"))
  # 只有摘要：沒有個人資料的欄
  expect_false(any(c("visit_id", "subj_id", "theta") %in% names(ws8)))
  # 寫出再讀回相同（數字差在 1e-12 以內；見 expect_roundtrip()）
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(round8()$round$fixed, zip_path)
  expect_roundtrip(kitsui_read_fixed(zip_path)$wave_summary, ws8)
})

test_that("同一個波次再校準時兩列都留，畫圖用最新的回合", {
  fixed <- lan_chain()[["48"]]$fixed
  again <- suppressMessages(kitsui_round(kitsui_demo("M03", 3, "lan"), fixed, "again"))
  ws <- again$fixed$wave_summary
  three <- ws[ws$scale == "lan M03" & ws$wave_m == 3, ]
  expect_equal(three$round, c("3", "again"))
  # 下一回合看之前的點：同一個波次只留最新的回合
  fixed$version$rounds <- c(again$fixed$version$rounds, "next")
  fixed$wave_summary <- ws
  earlier <- kitsui_vertical_earlier(fixed, "lan", "next")
  expect_equal(nrow(earlier), 7)
  expect_equal(earlier$round[earlier$wave_m == 3], "again")
  expect_equal(earlier$p50[earlier$wave_m == 3], three$p50[2])
})

test_that("摘要和 kits_vertical() 的 n、P25、中位數、P75 完全相同；分數存在 memo", {
  r8 <- round8()
  persons <- kitsui_persons(r8$df)
  # kitsui_round() 算過的分數存在 memo，之後各分頁直接拿
  expect_true(exists("own_scores:lan M36", envir = r8$round$memo))
  own <- kitsui_own_scores(r8$round, "lan M36", persons)
  kv <- kits::kits_vertical(own)
  ws <- r8$round$fixed$wave_summary
  ws <- ws[ws$scale == "lan M36", ]
  expect_identical(ws$wave_m, kv$wave_m)
  expect_identical(ws$n, as.integer(kv$n))
  expect_identical(ws$p25, kv$P25)
  expect_identical(ws$p50, kv$median)
  expect_identical(ws$p75, kv$P75)
  # 實際月齡的中位數：和 kits_vertical() 的 age_med 相同
  expect_true(all(is.finite(ws$age_med)))
  expect_identical(ws$age_med, kv$age_med)
})

test_that("垂直圖畫在實際月齡的中位數；有一回合沒有實際月齡時，整張用名義月齡（不混用）", {
  r8 <- round8()
  lk <- linked()
  v <- kitsui_verticals(r8$round, r8$df, lk$fixed, "lan")$external
  tab <- v$table
  expect_true(all(is.finite(tab$age_med)))
  built <- plotly::plotly_build(kitsui_vertical_plot(v))$x
  xs <- unlist(purrr::map(purrr::keep(built$data, \(tr) tr$name %in% c("M03", "M36")), "x"))
  expect_setequal(round(xs, 8), round(tab$age_med, 8))
  expect_equal(built$layout$xaxis$title, "Age (months; median actual age of each wave)")
  expect_match(vertical_caption("external", v), "x 軸是實際月齡")
  # 舊的固定檔（0.3.11 以前）：之前回合沒有 age_med，整張圖退回名義月齡，軸與說明寫出來
  old_fixed <- lk$fixed
  old_fixed$wave_summary$age_med <- NA_real_
  w <- kitsui_verticals(r8$round, r8$df, old_fixed, "lan")$external
  t2 <- w$table
  expect_true(all(is.finite(t2$age_med[t2$source == "this"])))
  expect_true(all(is.na(kitsui_vertical_age(t2))))
  expect_false(kitsui_vertical_actual(t2))
  built2 <- plotly::plotly_build(kitsui_vertical_plot(w))$x
  xs2 <- unlist(purrr::map(purrr::keep(built2$data, \(tr) tr$name %in% c("M03", "M36")), "x"))
  expect_setequal(xs2, t2$wave_m)
  expect_equal(built2$layout$xaxis$title, "Age (months; nominal age of each wave)")
  expect_match(vertical_caption("external", w), "x 軸是名義月齡")
  expect_match(vertical_caption("external", w), "沒有實際月齡（kitsUI 0.3.11 以前做的）")
  # 全部都沒有實際月齡：整張用名義月齡
  expect_true(all(is.na(kitsui_vertical_age(data.frame(age_med = NA, wave_m = c(3, 6))))))
})

test_that("摘要照連結換算，等於換算後的分數再做摘要", {
  r8 <- round8()
  lk <- linked()
  links <- kitsui_links(lk$fixed)
  persons <- kitsui_persons(r8$df)
  own <- kitsui_own_scores(r8$round, "lan M36", persons)
  ws <- lk$fixed$wave_summary
  ws <- ws[ws$scale == "lan M36", ]
  for (stage in c("internal", "external")) {
    path <- kitsui_path_links(lk$fixed, links, "lan M36", stage)
    direct <- kitsui_wave_summary(kitsui_convert_path(own, path), "lan M36", "r8")
    conv <- kitsui_summary_convert(ws, lk$fixed, links, stage)
    expect_equal(attr(conv, "ends"), rep(if (stage == "internal") "lan M03" else "D", 2))
    for (col in c("p25", "p50", "p75")) {
      expect_equal(conv[[col]], direct[[col]], tolerance = 1e-10)
    }
  }
  # 沒有連結時原樣
  same <- kitsui_summary_convert(ws, r8$round$fixed, list(), "external")
  expect_equal(same$p50, ws$p50)
  expect_equal(attr(same, "ends"), c("lan M36", "lan M36"))
})

test_that("兩個世代的垂直連結三個版本：未連結、內部連結後、外部連結後", {
  both <- both_cohorts()
  r <- both$round
  # motor M03 只有 3 個月時 kits 出錯（停下來），所以只有 lan 有兩個世代
  expect_equal(kitsui_vertical_domains(r), "lan")
  v <- kitsui_verticals(r, both$df, r$fixed, "lan")
  expect_named(v, c("none", "internal", "external"))
  # 未連結：各世代自己的量尺，kits_vertical() 不標量尺
  expect_s3_class(v$none$vertical, "kits_vertical")
  expect_true(is.na(attr(v$none$vertical, "scale")))
  expect_setequal(v$none$vertical$cohort, c("M03", "M36"))
  # 第一個回合：沒有之前回合的點，圖和 plot(kits_vertical()) 相同
  expect_true(all(v$none$table$source == "this"))
  expect_s3_class(kitsui_vertical_plot(v$none), "plotly")
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
  expect_equal(ext$median[ext$cohort == "M36" & ext$wave_m == 48], stats::median(s$d_score))
  # 兩個世代有共同的月齡（36、48）：沒有說明
  expect_null(v$external$note)
})

test_that("這一回合只有一個世代：之前回合的點（固定檔的摘要）照現在的連結一起畫", {
  r8 <- round8()
  lk <- linked()
  # lan 這一回合只有 M36，固定檔裡有之前回合 lan M03 的摘要；motor 只有 M03、沒有之前的點
  expect_equal(kitsui_vertical_domains(r8$round), "lan")
  v <- kitsui_verticals(r8$round, r8$df, lk$fixed, "lan")
  expect_equal(v$none$from, "lan M36")
  expect_setequal(v$none$scales, c("lan M03", "lan M36"))
  tab <- v$external$table
  expect_equal(v$external$scale, "D")
  expect_setequal(tab$cohort[tab$source == "this"], "M36")
  expect_equal(sort(tab$wave_m[tab$source == "earlier"]), c(3, 6, 12, 18, 24, 36, 48))
  expect_equal(sort(unique(tab$round[tab$source == "earlier"])),
               sort(c("3", "6", "12", "18", "24", "36", "48")))
  # 之前回合的點：摘要換到 D，和 lan M03 的分數換到 D 再取中位數相同
  links <- kitsui_links(lk$fixed)
  ws <- lk$fixed$wave_summary
  ws <- ws[ws$scale == "lan M03", ]
  d03 <- kitsui_convert_path(structure(ws$p50, scale = "lan M03"),
                             kitsui_path_links(lk$fixed, links, "lan M03"))
  expect_equal(tab$median[tab$source == "earlier"], as.vector(d03), tolerance = 1e-10)
  # 這一回合的點：和 kits_vertical() 相同
  expect_equal(tab$median[tab$source == "this"], v$external$vertical$median)
  # 圖：之前回合的點是空心、較淡，滑鼠移上去看回合
  p <- kitsui_vertical_plot(v$external)
  expect_s3_class(p, "plotly")
  built <- plotly::plotly_build(p)$x$data
  m03 <- purrr::keep(built, \(tr) identical(tr$name, "M03"))[[1]]
  expect_true(all(grepl("-open$", m03$marker$symbol)))
  expect_true(all(m03$marker$opacity < 1))
  expect_match(m03$hovertext[1], "Earlier round \\(fixed file\\): 3")
  m36 <- purrr::keep(built, \(tr) identical(tr$name, "M36"))[[1]]
  expect_false(any(grepl("-open$", m36$marker$symbol)))
  expect_match(m36$hovertext[1], "This round: r8")
  # x 軸的刻度（格線）是圖上的每一個波次
  lay <- plotly::plotly_build(p)$x$layout
  expect_equal(unlist(lay$xaxis$tickvals), sort(unique(tab$wave_m)))
  # 96 個月以後的波次也有刻度（kits 預設的刻度只到 96）
  late <- v$external
  late$table$wave_m[late$table$source == "this"] <- c(108, 120)
  lay <- plotly::plotly_build(kitsui_vertical_plot(late))$x$layout
  expect_true(all(c(108, 120) %in% unlist(lay$xaxis$tickvals)))
  expect_equal(kitsui_vertical_ticks(c(132, 3, NA, 108, 3)), c(3, 108, 132))
  # 還沒有連結時：未連結畫得出來，內部連結後說明原因
  plain <- kitsui_verticals(r8$round, r8$df, r8$round$fixed, "lan")
  expect_s3_class(kitsui_vertical_plot(plain$none), "plotly")
  expect_match(plain$internal$why, "lan M36 還沒有內部連結")
  # motor：只有一個世代、沒有之前的點
  one <- kitsui_verticals(r8$round, r8$df, lk$fixed, "motor")
  expect_true(all(purrr::map_lgl(one, \(x) grepl("只有一個世代", x$why))))
})

test_that("借用另一世代題庫（M03 60 個月用 motor M36 的題庫）：之前回合兩個世代的點都畫得出來", {
  b <- borrow_chain()
  r2 <- b$r2
  expect_equal(names(r2$items), "motor M36")
  ws <- r2$fixed$wave_summary
  # 量尺是借出的題庫，世代是孩子的世代
  now <- ws[ws$round == "2", ]
  expect_equal(now$scale, "motor M36")
  expect_equal(now$cohort, "M03")
  expect_equal(now$wave_m, 60)
  expect_equal(kitsui_vertical_domains(r2), "motor")
  v <- kitsui_verticals(r2, b$df2, r2$fixed, "motor")
  tab <- v$external$table
  expect_equal(v$external$scale, "D")
  expect_equal(tab$wave_m[tab$source == "this"], 60)
  expect_setequal(tab$cohort[tab$source == "earlier"], c("M03", "M36"))
  expect_equal(v$internal$scale, "motor M03")
  expect_s3_class(kitsui_vertical_plot(v$internal), "plotly")
  # 未連結：借用 motor M36 題庫的 M03 60 個月另成一條線，不和 M03 自己量尺的波次連在一起
  none <- v$none$table
  expect_equal(none$cohort[none$source == "this"], "M03 (motor M36 bank)")
  expect_setequal(none$cohort[none$source == "earlier"], c("M03", "M36"))
  built <- plotly::plotly_build(kitsui_vertical_plot(v$none))$x$data
  names <- purrr::map_chr(built, \(tr) as.character(tr$name %||% NA)[1])
  expect_true("M03 (motor M36 bank)" %in% names)
  expect_match(vertical_caption("none", v$none), "另成一條線")
  # 換算後在同一把尺上：同一個世代仍是一條線
  expect_false(any(kitsui_vertical_borrowed(v$external$table$cohort)))
  expect_equal(kitsui_vertical_series(c("M03", "M36"), c("motor M36", "motor M36")),
               c("M03 (motor M36 bank)", "M36"))
})

test_that("舊的固定檔沒有摘要、第一個回合：一句清楚的說明", {
  b <- borrow_chain()
  r2 <- b$r2
  # 舊的固定檔：之前的回合沒有摘要（只有這一回合用新版本算的）
  r2$fixed$wave_summary <- r2$fixed$wave_summary[r2$fixed$wave_summary$round == "2", ]
  expect_equal(kitsui_vertical_domains(r2), character())
  why <- kitsui_vertical_alone(r2$fixed)
  expect_match(why, "只有一個世代")
  expect_match(why, "wave_summary.csv")
  expect_match(why, paste0("kitsUI ", KITSUI_SUMMARY_SINCE, " 開始"))
  expect_match(why, "重跑之前的回合")
  v <- kitsui_verticals(r2, b$df2, r2$fixed, "motor")
  expect_true(all(purrr::map_lgl(v, \(x) identical(x$why, why))))
  # 第一個回合
  first <- lan_chain()[["3"]]
  expect_equal(kitsui_vertical_domains(first), character())
  expect_match(kitsui_vertical_alone(first$fixed), "第一個回合")
})

test_that("垂直連結：分數算一次（progress 每個世代一次），換算另外做；沒有共同月齡時一句說明", {
  wide <- two_cohorts_linked()
  persons <- kitsui_persons(wide$df)
  seen <- character()
  raw <- kitsui_vertical_scores(wide$round, "lan", persons, \(i, n, s) seen <<- c(seen, s))
  expect_equal(seen, c("lan M03", "lan M36"))
  expect_named(kitsui_vertical_scores(round8()$round, "lan", persons), "lan M36")
  expect_null(kitsui_vertical_scores(round8()$round, "soc", persons))
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

test_that("之前回合的點的程式碼能跑：讀 wave_summary.csv、照連結換算，和 app 的點相同", {
  b <- borrow_chain()
  r2 <- b$r2
  v <- kitsui_verticals(r2, b$df2, r2$fixed, "motor")
  code <- kitsui_code_vertical_earlier(r2$fixed, v$none$scales, "external", "kitsui_fixed_2.zip")
  expect_match(code, 'unzip\\("kitsui_fixed_2.zip"')
  expect_match(code, "wave_summary.csv")
  expect_match(code, "`motor M03` = list\\(link_D\\)")
  dir <- withr::local_tempdir()
  kitsui_write_fixed(r2$fixed, file.path(dir, "kitsui_fixed_2.zip"))
  links <- kitsui_links(r2$fixed)
  env <- new.env(parent = globalenv())
  env$link_motor_M36 <- links[["motor M36"]]
  env$link_D <- links$D
  withr::with_dir(dir, {
    out <- eval(parse(text = paste("library(kits)", "library(dplyr)", code, sep = "\n")),
                envir = env)
  })
  expect_s3_class(out, "plotly")
  key <- \(x) paste(x$cohort, x$wave_m)
  tab <- v$external$table
  expect_equal(env$ws$p50[match(key(tab), key(env$ws))], tab$median, tolerance = 1e-10)
  expect_equal(env$ws$p25[match(key(tab), key(env$ws))], tab$P25, tolerance = 1e-10)
  # 未連結：不換算
  none <- kitsui_code_vertical_earlier(r2$fixed, v$none$scales, "none", "kitsui_fixed_2.zip")
  expect_false(grepl("kits_convert", none))
})
