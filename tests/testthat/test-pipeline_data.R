test_that("示範資料依世代、波次、領域篩選", {
  df <- kitsui_demo("M03", c(3, 6), "lan")
  expect_equal(unique(df$cohort), "M03")
  expect_setequal(unique(df$wave_m), c(3, 6))
  expect_equal(unique(df$domain), "lan")
  expect_setequal(unique(kitsui_demo(waves = 36)$cohort), c("M03", "M36"))
})

test_that("缺 domain 或 cohort 時以中文說明", {
  df <- kitsui_demo("M03", 3, "lan")
  expect_error(kitsui_check_data(dplyr::select(df, -domain)), "domain", class = "kit_user_error")
  df$cohort[1] <- NA
  expect_error(kitsui_check_data(df), "世代", class = "kit_user_error")
})

test_that("量尺清單與作答矩陣", {
  df <- dplyr::bind_rows(kitsui_demo("M03", 3, "lan"), kitsui_demo("M36", 36, "lan"))
  scales <- kitsui_scales(df)
  expect_equal(scales$scale, c("lan M03", "lan M36"))
  X <- kitsui_matrix(df, "lan M03")
  expect_equal(nrow(X), scales$n_visits[1])
  expect_equal(ncol(X), scales$n_items[1])
  expect_setequal(rownames(X), unique(df$visit_id[df$cohort == "M03"]))
})

test_that("這一回合的名稱：預設 R1、R2…（下一個沒用過的）；只能有英文字母與數字", {
  expect_equal(kitsui_default_label(), "R1")
  expect_equal(kitsui_default_label("R1"), "R2")
  expect_equal(kitsui_default_label(c("3", "6")), "R3")
  expect_equal(kitsui_default_label(c("R2", "x")), "R3")
  expect_equal(kitsui_default_label(c("a", "R2")), "R3")
  expect_length(kitsui_label_problem("r8", c("3", "6")), 0)
  expect_match(kitsui_label_problem("第一回合"), "英文字母與數字")
  expect_match(kitsui_label_problem("", "3"), "不能空白")
  expect_match(kitsui_label_problem("6", c("3", "6")), "重複")
})

test_that("這一回合的資料：選波次、每個量尺 kits_resp()，問題以中文列出", {
  data <- kitsui_demo("M03", c(3, 6), c("lan", "motor"))
  ck <- kitsui_data_check(data, "3", "3", kitsui_fixed_empty())
  expect_length(ck$problems, 0)
  expect_equal(unique(ck$df$wave_m), 3)
  expect_equal(ck$scales$scale, c("lan M03", "motor M03"))
  expect_null(ck$resps)
  expect_equal(ck$persons, kitsui_persons(ck$df))
  expect_equal(ck$plan$data_scale, c("lan M03", "motor M03"))
  # 還沒勾：不是問題，是中性的提示
  none <- kitsui_data_check(data, character(), "3", NULL)
  expect_length(none$problems, 0)
  expect_match(none$hint, "請勾這一回合的世代與波次")
  miss <- kitsui_data_check(dplyr::select(data, -"wave_m"), "3", "3", NULL)
  expect_match(miss$problems, "wave_m")
  fixed <- lan_chain()[["3"]]$fixed
  expect_match(kitsui_data_check(data, "3", "3", fixed)$problems, "重複")
})

# 上傳的檔案：寫到暫存資料夾（.csv.gz 由 readr 壓縮），回傳 fileInput 格式（name、datapath）
upload_files <- function(dfs, names, dir = withr::local_tempdir(.local_envir = parent.frame())) {
  paths <- file.path(dir, names)
  purrr::walk2(dfs, paths, readr::write_csv)
  data.frame(name = names, datapath = paths)
}

test_that("好幾個長格式檔案（含 .csv.gz）一起上傳：接成一張表，編號讀成文字", {
  a <- kitsui_demo("M03", 3, "lan")
  b <- kitsui_demo("M36", 36, "lan")
  # 數字樣子的編號：開頭的 0 不會掉
  a$subj_id <- sprintf("%05d", match(a$subj_id, unique(a$subj_id)))
  files <- upload_files(list(a, b), c("m03.csv", "m36.csv.gz"))
  expect_match(files$datapath[2], "[.]csv[.]gz$")
  up <- kitsui_read_uploads(files)
  expect_equal(nrow(up$data), nrow(a) + nrow(b))
  expect_equal(up$files$file, c("m03.csv", "m36.csv.gz"))
  expect_equal(up$files$rows, c(nrow(a), nrow(b)))
  expect_type(up$data$subj_id, "character")
  expect_equal(up$data$subj_id[1], "00001")
  expect_setequal(kitsui_scales(up$data)$scale, c("lan M03", "lan M36"))
  # shiny 的 datapath 不是 .gz 結尾時也讀得到
  plain <- withr::local_tempfile()
  file.copy(files$datapath[2], plain)
  gz <- kitsui_read_uploads(data.frame(name = "m36.csv.gz", datapath = plain))
  expect_equal(nrow(gz$data), nrow(b))
})

test_that("檔案的欄位或型別不一樣、數字欄有讀不懂的值時，說是哪一個檔案", {
  a <- kitsui_demo("M03", 3, "lan")
  files <- upload_files(list(a, dplyr::select(a, -"sex")), c("a.csv", "b.csv"))
  expect_error(kitsui_read_uploads(files), "b.csv 的欄位和 a.csv 不一樣。少了：sex",
               class = "kit_user_error")
  # 約定以外的欄讀檔時就拿掉，不比；約定的欄型別不一樣時（例如 SAV 的文字欄）說是哪一個檔案
  c2 <- dplyr::mutate(a, extra = 1)
  c1 <- dplyr::mutate(a, extra = "x")
  files <- upload_files(list(c1, c2), c("c1.csv", "c2.csv"))
  expect_equal(kitsui_read_uploads(files)$dropped, "extra")
  expect_error(kitsui_same_columns(list(c1.csv = a, c2.csv = dplyr::mutate(a, wave_m = "x"))),
               "c2.csv 的 wave_m 欄是文字", class = "kit_user_error")
  bad <- dplyr::mutate(a, wave_m = as.character(wave_m))
  bad$wave_m[2] <- "3a"
  files <- upload_files(list(bad), "bad.csv")
  # 列數和 Excel 一樣（第 1 列是欄名）
  expect_error(kitsui_read_uploads(files), "第 3 列的 wave_m 是「3a」，應該是數字",
               class = "kit_user_error")
  expect_error(kitsui_read_uploads(NULL), "請選擇資料檔", class = "kit_user_error")
  # 錯誤沒有被包起來：app 直接顯示這句中文
  e <- tryCatch(kitsui_read_uploads(upload_files(list(a, a[-1]), c("a.csv", "b.csv"))),
                error = identity)
  expect_s3_class(e, "kit_user_error")
})

test_that("欄位範例檔與欄位表：資料約定的全部欄位，範例檔可以直接用", {
  ex <- kitsui_example_data()
  expect_equal(nrow(ex), 24)
  expect_named(ex, KITSUI_COLUMNS$column)
  expect_setequal(unique(ex$domain), c("lan", "motor"))
  expect_s3_class(kitsui_resp(ex, "lan M03"), "kits_resp")
  tab <- kitsui_columns_table()
  expect_named(tab, c("column", "required", "meaning"))
  expect_equal(tab$column[tab$required == "是"], KITSUI_REQUIRED)
})

test_that("讀好幾個檔案：每個檔案讀之前回報進度", {
  paths <- c(tempfile(fileext = ".csv"), tempfile(fileext = ".csv"))
  readr::write_csv(kitsui_demo("M03", 3, "lan"), paths[1])
  readr::write_csv(kitsui_demo("M36", 36, "lan"), paths[2])
  seen <- character()
  step <- \(i, n, name) seen <<- c(seen, sprintf("%d/%d %s", i, n, name))
  up <- kitsui_read_uploads(data.frame(name = c("a.csv", "b.csv"), datapath = paths),
                            progress = step)
  expect_equal(seen, c("1/2 a.csv", "2/2 b.csv"))
  expect_equal(up$files$file, c("a.csv", "b.csv"))
})

test_that("很快的作答檢查：重複的列、不是整數的分數", {
  df <- kitsui_demo("M03", 3, "lan")
  expect_length(kitsui_quick_check(df), 0)
  expect_match(kitsui_quick_check(rbind(df, df[1, ])), "同一列次同一題有好幾列（共 2 列）")
  bad <- dplyr::mutate(df, item_score = ifelse(dplyr::row_number() == 1, 1.5, item_score))
  expect_match(kitsui_quick_check(bad), "0 以上的整數")
  ck <- kitsui_data_check(rbind(df, df[1, ]), "3", "r1", kitsui_fixed_empty())
  expect_match(ck$problems, "好幾列")
})

test_that("世代 × 波次：選項（參照世代在前）、勾選的值對到資料", {
  data <- kitsui_demo(waves = c(3, 36, 48))
  ch <- kitsui_pick_choices(data)
  expect_equal(ch$value, c("M03 3", "M03 36", "M03 48", "M36 36", "M36 48"))
  expect_equal(kitsui_parse_picks(c("M36 48", "M03 3"), ch)$value, c("M03 3", "M36 48"))
  # 只寫月齡：所有世代的這個月齡
  expect_equal(kitsui_parse_picks("36", ch)$value, c("M03 36", "M36 36"))
  expect_equal(nrow(kitsui_parse_picks(c("M99 3", "7"), ch)), 0)
  expect_equal(nrow(kitsui_pick_choices(data.frame(x = 1))), 0)
})

test_that("選資料：只拿勾到的組合；全部都勾時不複製資料；還沒勾時是提示", {
  data <- kitsui_demo(waves = c(3, 36, 48), domains = "lan")
  ck <- kitsui_data_select(data, c("M03 36", "M36 48"))
  got <- dplyr::count(ck$df, cohort, wave_m)
  expect_equal(paste(got$cohort, got$wave_m), c("M03 36", "M36 48"))
  expect_equal(ck$picks$value, c("M03 36", "M36 48"))
  expect_equal(ck$scales$scale, c("lan M03", "lan M36"))
  all <- kitsui_data_select(data, kitsui_pick_choices(data)$value)
  expect_identical(rlang::obj_address(all$df), rlang::obj_address(data))
  expect_match(kitsui_data_select(data, character())$hint, "請勾這一回合的世代與波次")
  miss <- kitsui_data_select(dplyr::select(data, -"cohort"), "M03 3")
  expect_match(miss$problems, "cohort")
})

test_that("橘色提醒：波次已經校準過、固定檔不是最新的一回合、資料檔用過、得分不完整的題", {
  fixed <- lan_chain()[["6"]]$fixed
  plan <- kitsui_round_plan(kitsui_demo("M03", c(6, 12), "lan"), fixed)
  expect_match(kitsui_note_waves(plan, fixed),
               "這些波次之前的回合已經校準過：lan M03 的 6 個月（第 6 回合）。")
  expect_length(kitsui_note_waves(kitsui_round_plan(kitsui_demo("M03", 12, "lan"), fixed),
                                  fixed), 0)
  expect_length(kitsui_note_stale(c("R1", "R2"), "R2"), 0)
  expect_length(kitsui_note_stale("R1", NULL), 0)
  expect_match(kitsui_note_stale("R1", "R2"), "這個固定檔只到第 R1 回合，這次連線已經做到第 R2 回合")
  expect_match(kitsui_note_stale(character(), "R2"), "現在沒有用固定檔")
  fixed$sources <- tibble::tibble(round = c("R1", "R2"), file = "a.csv", md5 = c("aa", "bb"))
  files <- tibble::tibble(file = c("new.csv", "old.csv"), rows = 1, md5 = c("cc", "bb"))
  expect_equal(kitsui_note_sources(files, fixed), "old.csv：這個檔案在第 R2 回合用過。")
  expect_length(kitsui_note_sources(NULL, fixed), 0)
  probs <- tibble::tibble(scale = "lan M03", item_id = paste0("i", 1:14), problem = "no 0 score",
                          detail_zh = paste0("i", 1:14, " 沒有人得 0 分"))
  out <- kitsui_note_categories(probs)
  expect_length(out, 13)
  expect_equal(out[1], "lan M03：i1 沒有人得 0 分。")
  expect_equal(out[13], "……還有 2 題。")
  expect_length(kitsui_note_categories(probs[0, ]), 0)
})

test_that("上傳的檔案記下 md5；CSV 不再轉一次 UTF-8，BIG5 的中文照樣讀對", {
  a <- kitsui_demo("M03", 3, "lan")
  files <- upload_files(list(a), "a.csv")
  up <- kitsui_read_uploads(files)
  expect_equal(up$files$md5, unname(tools::md5sum(files$datapath)))
  big5 <- withr::local_tempfile(fileext = ".csv")
  con <- file(big5, "wb")
  writeBin(iconv("visit_id,note\n1,語言\n", "UTF-8", "CP950", toRaw = TRUE)[[1]], con)
  close(con)
  r <- read_data_file(big5, "b.csv")
  expect_equal(r$encoding, "BIG5")
  expect_equal(r$data$note, "語言")
  expect_true(all(validUTF8(r$data$note)))
})

test_that("空檔、只有欄名的檔：清楚的錯誤", {
  dir <- withr::local_tempdir()
  writeLines(character(), file.path(dir, "empty.csv"))
  writeLines(paste(KITSUI_REQUIRED, collapse = ","), file.path(dir, "head.csv"))
  expect_error(kitsui_read_uploads(data.frame(name = "empty.csv",
                                              datapath = file.path(dir, "empty.csv"))),
               "empty.csv 是空的檔案", class = "kit_user_error")
  expect_error(kitsui_read_uploads(data.frame(name = "head.csv",
                                              datapath = file.path(dir, "head.csv"))),
               "head.csv 只有欄名，沒有作答的列", class = "kit_user_error")
})

test_that("缺欄位時看得出原因：分號分隔、欄名大寫", {
  a <- kitsui_example_data()
  dir <- withr::local_tempdir()
  readr::write_delim(a, file.path(dir, "semi.csv"), delim = ";")
  up <- kitsui_read_uploads(data.frame(name = "semi.csv", datapath = file.path(dir, "semi.csv")))
  expect_match(kitsui_data_select(up$data, "M03 3")$problems, "看起來是用分號（;）分隔")
  names(a) <- toupper(names(a))
  expect_match(kitsui_data_select(a, "M03 3")$problems, "欄名要全部小寫.*「VISIT_ID」")
  expect_equal(kitsui_missing_hint(c("visit_id", "x")), "")
})

test_that("約定以外的欄：讀檔與選資料時拿掉，校準不會因為它們停下來", {
  a <- kitsui_demo("M03", 3, "lan")
  a$note <- seq_len(nrow(a))
  files <- upload_files(list(a), "a.csv")
  up <- kitsui_read_uploads(files)
  expect_equal(up$dropped, "note")
  expect_false("note" %in% names(up$data))
  ck <- kitsui_data_select(a, "M03 3")
  expect_equal(ck$dropped, "note")
  expect_false("note" %in% names(ck$df))
  expect_match(kitsui_note_dropped("note"), "已經拿掉：note")
  expect_length(kitsui_note_dropped(character()), 0)
  r <- suppressMessages(kitsui_round(ck$df, kitsui_fixed_empty(), "R1"))
  expect_true(r$ok)
})

test_that("wave_m 空白的列：全部都勾時也不放進來，橘色提醒", {
  d <- kitsui_demo("M03", c(3, 6), "lan")
  d$wave_m[c(1, 51)] <- NA
  ck <- kitsui_data_select(d, kitsui_pick_choices(d)$value)
  expect_false(anyNA(ck$df$wave_m))
  expect_equal(nrow(ck$df), nrow(d) - 2)
  expect_match(ck$notes, "wave_m 有 2 列是空白")
  some <- kitsui_data_select(d, "M03 3")
  expect_false(anyNA(some$df$wave_m))
})

test_that("domain、cohort 是空字串時和空白一樣擋下來", {
  d <- kitsui_demo("M03", 3, "lan")
  d$domain[1] <- ""
  expect_error(kitsui_check_data(d), "domain 或 cohort 有空白", class = "kit_user_error")
})

test_that("看起來是遺漏碼的分數：簡短提醒，指到 kitJoin 的「遺漏值處理」", {
  d <- kitsui_demo("M03", 3, "lan")
  ids <- unique(d$item_id)
  d$item_score[d$item_id == ids[1]][1:3] <- c(98, 99, 98)
  d$item_score[d$item_id == ids[2]][1] <- 7777
  note <- kitsui_note_codes(d)
  expect_match(note, "^2 題有看起來是遺漏碼的分數")
  expect_match(note, "這些值是：98、99、7777。")
  expect_match(note, "kitJoin「遺漏值處理」")
  expect_length(kitsui_note_codes(kitsui_demo("M03", 3, "lan")), 0)
  # 0–100 分這種連續的分數不算
  e <- tibble::tibble(item_id = "p", item_score = 0:100)
  expect_length(kitsui_note_codes(e), 0)
  # 分數不連續的說明寫成一段，不列出 90 個數字
  expect_equal(kitsui_runs(c(4:97, 99)), "4–97、99")
  expect_equal(kitsui_runs(c(1, 2, 5)), "1、2、5")
  p <- kitsui_score_problem("x", c(0, 1, 2, 3, 98))
  expect_equal(p$detail, "x 分數不連續，沒有人得 4–97 分（分數 0、1、2、3、98）")
})

test_that("世代名稱有空白、領域名稱有逗號：① 與 ② 一致，校準、連結、固定檔、分數都可以做", {
  d <- kitsui_demo(waves = c(3, 36, 48), domains = "lan")
  d$cohort[d$cohort == "M36"] <- "old cohort"
  d$domain <- "lan, words"
  ck <- kitsui_data_check(d, kitsui_pick_choices(d)$value, "R1", NULL)
  expect_length(ck$problems, 0)
  old <- ck$scales$scale[2]
  expect_equal(scale_domain(old), "lan, words")
  expect_equal(scale_cohort(old), "old cohort")
  expect_equal(scale_label(old), "lan, words old cohort")
  # 沒有空白的世代：量尺名稱和以前一樣
  expect_equal(ck$scales$scale[1], "lan, words M03")
  expect_equal(scale_key("lan", "M36"), "lan M36")
  r <- suppressMessages(kitsui_round(ck$df, kitsui_fixed_empty(), "R1"))
  expect_true(r$ok)
  expect_setequal(names(r$items), ck$scales$scale)
  li <- kitsui_link_internal(r$fixed, "lan, words", "old cohort")
  expect_false(is.null(li$link))
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(li$fixed, zip_path)
  back <- kitsui_read_fixed(zip_path)
  expect_identical(back$version$scales, li$fixed$version$scales)
  expect_equal(kitsui_internal_scales(back), old)
  sc <- kitsui_score(r, ck$df, back)$scores
  # 分數表沒有 ability_linked 欄（在分數對照表裡）：舊世代的分數經過內部連結，最後在參照世代的量尺上
  old_sc <- sc[sc$cohort == "old cohort", ]
  expect_gt(nrow(old_sc), 0)
  expect_true(all(old_sc$scale == "lan, words M03"))
  expect_false(anyNA(old_sc$ability))
  expect_equal(kitsui_split_scales("lan, words M03, lan M36", back), c("lan, words M03", "lan M36"))
  # 產生的 R 程式：資料用一般的空白，量尺名稱的不斷行空白寫成  
  code <- kitsui_code_round(old, "R1")
  expect_match(code, 'paste(domain, cohort) == "lan, words old cohort"', fixed = TRUE)
  expect_match(code, 'scale = "lan, words old\\u00a0cohort"', fixed = TRUE)
})

test_that("量尺表：參照世代排在前面", {
  d <- kitsui_demo(waves = c(3, 36), domains = "lan")
  expect_equal(kitsui_data_select(d, kitsui_pick_choices(d)$value)$scales$scale,
               c("lan M03", "lan M36"))
  expect_equal(kitsui_data_select(d, kitsui_pick_choices(d)$value, reference = "M36")$scales$scale,
               c("lan M36", "lan M03"))
  fx <- kitsui_fixed_empty()
  fx$version$reference <- "M36"
  plan <- kitsui_round_plan(d, fx)
  expect_equal(data_scale_table(kitsui_scales(d), fx, plan)$scale, c("lan M36", "lan M03"))
})
