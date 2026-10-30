# 在暫存資料夾跑一段產生的程式碼，回傳它的環境
run_code <- function(code, dir) {
  withr::local_dir(dir)
  env <- new.env(parent = globalenv())
  suppressMessages(eval(parse(text = code), envir = env))
  env
}

test_that("一回合的程式碼：自由校準與固定校準都能跑，結果相同", {
  chain <- lan_chain()
  dir <- withr::local_tempdir()
  code <- kitsui_code_round("lan M03", "3", waves = 3)
  expect_no_error(parse(text = code))
  env <- run_code(code, dir)
  expect_equal(coef(env$item)$beta, coef(chain[["3"]]$items[["lan M03"]])$beta)

  kitsui_write_fixed(chain[["6"]]$fixed, file.path(dir, "fixed_6.zip"))
  code <- kitsui_code_round("lan M03", "12", chain[["6"]]$fixed, "fixed_6.zip", waves = 12)
  env <- run_code(code, dir)
  expect_equal(coef(env$item), coef(chain[["12"]]$items[["lan M03"]]), ignore_attr = TRUE)
})

test_that("連結與評分的程式碼接在一回合之後能跑，結果相同", {
  r8 <- round8()
  lk <- linked()
  dir <- withr::local_tempdir()
  kitsui_write_fixed(lk$fixed, file.path(dir, "fixed_r8.zip"))
  code <- c(
    kitsui_code_round("lan M36", "r8", lan_chain()[["48"]]$fixed, waves = c(36, 48)),
    kitsui_code_link(lk$fixed, "lan M36", "fixed_r8.zip"),
    kitsui_code_link(lk$fixed, "D", "fixed_r8.zip"),
    kitsui_code_score(lk$fixed, "lan M36")
  )
  env <- run_code(paste(code, collapse = "\n"), dir)
  expect_equal(coef(env$link_D), coef(lk$D))
  expect_equal(coef(env$link_lan_M36), coef(lk$internal))
  sc <- kitsui_score(r8$round, r8$df, lk$fixed)
  score <- sc$scores
  m36 <- score[score$cohort == "M36", ]
  expect_equal(env$scores$theta[match(m36$visit_id, env$scores$visit_id)], m36$d_score)
  tab <- kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36")
  expect_equal(env$item_table$D, tab$D)
  expect_equal(env$item_table$age50, tab$age50)
  # 分數對照表：程式直接換算表，和 app 的 d 欄相同
  links <- kitsui_links(lk$fixed)
  raw <- kitsui_raw_table(sc$own[["lan M36"]],
                          kitsui_path_links(lk$fixed, links, "lan M36"),
                          kitsui_path_links(lk$fixed, links, "lan M36", stage = "internal"))
  expect_equal(as.data.frame(env$raw_d)$theta, raw$d_score)
  expect_equal(as.data.frame(env$raw_d)$se_total, raw$se_total)
  expect_equal(as.data.frame(env$raw_theta)$theta, raw$ability_linked)
})

test_that("連結的程式讀真的固定檔檔名：之前的回合做的讀上傳的 zip，這一回合做的讀新的固定檔", {
  expect_equal(kitsui_link_file("D", "D", "fixed_7.zip", "r8"), "fixed_7.zip")
  expect_equal(kitsui_link_file("D", character(), "fixed_7.zip", "r8"), "kitsui_fixed_r8.zip")
  # 這次連線交過來的固定檔（沒有上傳的檔名）：新的固定檔裡也有之前的連結
  expect_equal(kitsui_link_file("D", "D", NULL, "r8"), "kitsui_fixed_r8.zip")
  code <- kitsui_code_link(linked()$fixed, "D", "kitsui_fixed_r8.zip")
  expect_match(code, 'unzip\\("kitsui_fixed_r8.zip"')
  expect_false(grepl("fixed_set.zip", code))
})

test_that("兩個世代垂直連結三個版本的程式碼能跑，和 app 的結果相同", {
  wide <- two_cohorts_linked()
  r <- wide$round
  code <- kitsui_code_vertical(wide$fixed, c("lan M03", "lan M36"))
  expect_match(code, 'plot(v_external, type = "median", age_ticks = sort(unique(v_external$wave_m)))',
               fixed = TRUE)
  expect_match(code, "kits_convert\\(scores_M36, link_lan_M36, link_D\\)")
  # 程式需要的東西：兩個世代還沒換算的分數、③ 重建的連結
  env <- new.env(parent = globalenv())
  persons <- kitsui_persons(wide$df)
  subj <- \(s) kits::kits_subj(r$data[[s]], r$items[[s]], persons = persons)
  env$scores_M03 <- subj("lan M03")
  env$scores_M36 <- subj("lan M36")
  links <- kitsui_links(wide$fixed)
  env$link_lan_M36 <- links[["lan M36"]]
  env$link_D <- links$D
  eval(parse(text = paste("library(kits)", "library(dplyr)", code, sep = "\n")), envir = env)
  v <- kitsui_verticals(r, wide$df, wide$fixed, "lan")
  expect_equal(env$v_none, v$none$vertical, ignore_attr = TRUE)
  expect_equal(env$v_internal, v$internal$vertical, ignore_attr = TRUE)
  expect_equal(env$v_external, v$external$vertical, ignore_attr = TRUE)
  only <- kitsui_code_vertical(wide$fixed, c("lan M03", "lan M36"), "none")
  expect_false(grepl("v_internal", only))
})

test_that("① 的程式碼能跑：示範資料選波次、一個量尺的作答設計與檢查", {
  code <- kitsui_code_data("lan M03", waves = c("3", "6"))
  expect_match(code, "filter\\(wave_m %in% c\\(3, 6\\)\\)")
  env <- run_code(code, withr::local_tempdir())
  expect_s3_class(env$resp, "kits_resp")
  expect_setequal(unique(env$responses$wave_m), c(3, 6))
  up <- kitsui_code_data("lan M03", "my data.csv", "12")
  expect_match(up, 'read_csv\\("my data.csv"\\) \\|>\n  filter\\(wave_m %in% 12\\)')
})

test_that("① 的程式碼帶通過步驟：作答設計圖的通過比例和 app 相同；都是 1 時不變", {
  steps <- c(lana01 = 2L, lana02 = 1L, lana05 = 2L)
  code <- kitsui_code_data("lan M03", waves = c("3", "6"), steps = steps)
  expect_match(code, "steps <- c\\(\n  lana01 = 2, lana05 = 2\n\\)")
  expect_match(code, "plot(resp_pass)\nkits_check(resp = resp)", fixed = TRUE)
  expect_true(all(nchar(strsplit(code, "\n")[[1]]) <= 100))
  env <- run_code(code, withr::local_tempdir())
  app <- kitsui_resp_pass(env$resp, steps)
  expect_equal(env$resp_pass, app, ignore_attr = TRUE)
  z <- \(r) plotly::plotly_build(plot(r))$x$data[[1]]$z
  expect_equal(z(env$resp_pass), z(app))
  expect_false(identical(z(env$resp_pass), z(env$resp)))
  # 都是 1（或沒有通過步驟）時和原來的程式一樣
  plain <- kitsui_code_data("lan M03", waves = c("3", "6"))
  expect_identical(kitsui_code_data("lan M03", waves = c("3", "6"), steps = c(lana01 = 1L)),
                   plain)
  expect_identical(kitsui_code_data("lan M03", waves = c("3", "6"),
                                    steps = kitsui_steps_vec(NULL)), plain)
})

test_that("① 的程式碼：好幾個檔案一起讀（readr 接起來），和 app 讀的相同", {
  dir <- withr::local_tempdir()
  a <- kitsui_demo("M03", 3, "lan")
  b <- kitsui_demo("M36", 36, "lan")
  readr::write_csv(a, file.path(dir, "m03.csv"))
  readr::write_csv(b, file.path(dir, "m36.csv.gz"))
  code <- kitsui_code_data("lan M36", c("m03.csv", "m36.csv.gz"), c("3", "36"))
  expect_match(code, 'read_csv\\(c\\(\n  "m03.csv",\n  "m36.csv.gz"\n\\)\\)')
  env <- run_code(code, dir)
  expect_s3_class(env$resp, "kits_resp")
  files <- data.frame(name = c("m03.csv", "m36.csv.gz"),
                      datapath = file.path(dir, c("m03.csv", "m36.csv.gz")))
  up <- kitsui_read_uploads(files)
  expect_equal(nrow(env$responses), nrow(up$data))
  expect_equal(env$responses$item_score, up$data$item_score)
})

test_that("借用另一世代題庫的一回合的程式碼能跑，結果和 app 相同", {
  bc <- borrow_chain()
  dir <- withr::local_tempdir()
  kitsui_write_fixed(bc$fixed, file.path(dir, "fixed_1.zip"))
  r2 <- bc$r2
  code <- kitsui_code_round("motor M36", "2", bc$fixed, "fixed_1.zip", waves = 60,
                            plan = r2$plan[r2$plan$scale == "motor M36", ])
  expect_match(code, "借用另一世代題庫時人的 cohort 欄不變")
  expect_match(code, 'keys <- "motor M03 60"')
  env <- run_code(code, dir)
  expect_equal(coef(env$item), coef(r2$items[["motor M36"]]), ignore_attr = TRUE)
  score <- kitsui_code_score(r2$fixed, "motor M36", kitsui_round_cohorts(r2, "motor M36"))
  expect_match(score, 'cohort %in% "M03"')
})

test_that("程式照勾到的世代 × 波次選資料（只勾 M03 36 時不帶進 M36 36），和 app 相同", {
  # 示範資料的程式讀 kitm03、kitm36 的全部作答，所以 app 這邊也用全部
  data <- dplyr::bind_rows(kits::kitm03$responses, kits::kitm36$responses)
  sel <- kitsui_data_select(data, c("M03 3", "M03 36"))
  expect_setequal(unique(sel$df$cohort), "M03")
  code <- kitsui_code_data("lan M03", picks = sel$picks)
  expect_match(code, 'picks <- tibble\\(cohort = c\\("M03", "M03"\\)')
  expect_match(code, "semi_join\\(responses, picks")
  expect_false(grepl("filter\\(wave_m", code))
  env <- run_code(code, withr::local_tempdir())
  expect_equal(nrow(env$responses), nrow(sel$df))
  expect_setequal(unique(env$responses$cohort), "M03")
  # 一回合的程式：同樣的選法，自由校準的結果和 app 相同
  lan <- dplyr::filter(sel$df, domain == "lan")
  r <- suppressMessages(kitsui_round(lan, kitsui_fixed_empty(), "1"))
  code <- kitsui_code_round("lan M03", "1", picks = sel$picks)
  env <- run_code(code, withr::local_tempdir())
  expect_equal(coef(env$item)$beta, coef(r$items[["lan M03"]])$beta)
  # 沒有 picks 時照舊照波次選
  expect_match(kitsui_code_data("lan M03", waves = 3), "filter\\(wave_m %in% 3\\)")
})

test_that("題目表的程式帶通過步驟（step = steps）能跑，和 app 的題目表相同；每行不超過 100 字", {
  r8 <- round8()
  lk <- linked()
  dir <- withr::local_tempdir()
  kitsui_write_fixed(lk$fixed, file.path(dir, "fixed_r8.zip"))
  item <- r8$round$items[["lan M36"]]
  steps <- c(lanb08 = 2L, lanb09 = 3L, lanc01 = 2L)
  step <- kitsui_item_step_arg(item, steps, lk$fixed$versions)
  score <- kitsui_code_score(lk$fixed, "lan M36", step = step)
  expect_match(score, "steps <- c\\(\n  lanb08 = 2, lanb09 = 3, lanc01 = 2\n\\)")
  expect_match(score, "kits_convert\\(item, link_lan_M36, link_D\\), step = steps\\)")
  code <- c(
    kitsui_code_round("lan M36", "r8", lan_chain()[["48"]]$fixed, waves = c(36, 48)),
    kitsui_code_link(lk$fixed, "lan M36", "fixed_r8.zip"),
    kitsui_code_link(lk$fixed, "D", "fixed_r8.zip"),
    score
  )
  env <- run_code(paste(code, collapse = "\n"), dir)
  tab <- kitsui_item_table(r8$round, r8$df, lk$fixed, "lan M36", steps = steps)
  expect_equal(env$item_table$D, tab$D)
  expect_equal(env$item_table$age50, tab$age50)
  expect_equal(env$item_table$step, tab$step)
  # 很多題：一行放幾題，每行不超過 100 字
  many <- stats::setNames(rep(3L, 30), sprintf("cogD%02d_r12", 1:30))
  lines <- strsplit(kitsui_code_score(lk$fixed, "lan M36", step = many), "\n")[[1]]
  expect_true(all(nchar(lines) <= 100))
  expect_no_error(parse(text = kitsui_code_steps(many)))
  expect_null(kitsui_code_steps(1))
})
