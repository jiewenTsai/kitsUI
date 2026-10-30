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
  c2 <- dplyr::mutate(a, extra = 1)
  c1 <- dplyr::mutate(a, extra = "x")
  files <- upload_files(list(c1, c2), c("c1.csv", "c2.csv"))
  expect_error(kitsui_read_uploads(files), "c2.csv 的 extra 欄是數字", class = "kit_user_error")
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
