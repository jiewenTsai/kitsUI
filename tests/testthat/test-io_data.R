# 讀 CSV 的編碼判斷

# 在另一個 R 行程讀檔：readr 遇到不合法的多位元組可能讓整個 R 當掉（segfault），
# 在這裡當掉只會讓這個測試失敗，不會讓整個測試停下來
read_in_subprocess <- function(path, name) {
  skip_if_not_installed("callr")
  root <- testthat::test_path("..", "..")
  dev <- file.exists(file.path(root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE)
  callr::r(function(root, dev, path, name) {
    if (dev) pkgload::load_all(root, quiet = TRUE, export_all = TRUE) else library(kitsUI)
    read <- get("kitsui_read_uploads", envir = asNamespace("kitsUI"))
    r <- tryCatch(read(data.frame(name = name, datapath = path)), error = function(e) e)
    if (inherits(r, "error")) list(error = conditionMessage(r), class = class(r)) else
      list(rows = nrow(r$data), cohort = unique(r$data$cohort))
  }, args = list(root = normalizePath(root), dev = dev, path = path, name = name))
}

latin1_csv <- function() {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  df <- kitsui_example_data()
  df$cohort <- "Café"
  lines <- c(paste(names(df), collapse = ","), do.call(paste, c(df, sep = ",")))
  writeLines(iconv(lines, "UTF-8", "latin1"), path, useBytes = TRUE)
  path
}

test_that("Latin-1 的 CSV：清楚的中文錯誤，R 不會當掉", {
  path <- latin1_csv()
  # 確實不是 UTF-8，也不是 CP950（以前就是在 CP950 那一步當掉）
  expect_true(is.na(csv_bytes_encoding(path)))
  r <- read_in_subprocess(path, "latin1.csv")
  expect_match(r$error, "編碼不是 UTF-8，也不是繁體中文的 Big5")
  expect_true("kit_user_error" %in% r$class)
  expect_error(read_data_file(path, "latin1.csv", KITSUI_TYPES), "CSV UTF-8",
               class = "kit_user_error")
})

test_that("Big5（CP950）與 UTF-8 的 CSV 照常讀進來", {
  df <- kitsui_example_data()
  df$cohort <- "國中"
  lines <- c(paste(names(df), collapse = ","), do.call(paste, c(df, sep = ",")))
  big5 <- withr::local_tempfile(fileext = ".csv")
  writeLines(iconv(lines, "UTF-8", "CP950"), big5, useBytes = TRUE)
  expect_equal(csv_bytes_encoding(big5), "CP950")
  r <- read_data_file(big5, "big5.csv", KITSUI_TYPES)
  expect_equal(r$encoding, "BIG5")
  expect_equal(unique(r$data$cohort), "國中")
  utf8 <- withr::local_tempfile(fileext = ".csv")
  writeLines(enc2utf8(lines), utf8, useBytes = TRUE)
  expect_equal(read_data_file(utf8, "utf8.csv", KITSUI_TYPES)$encoding, "UTF-8")
  sub <- read_in_subprocess(big5, "big5.csv")
  expect_equal(sub$rows, nrow(df))
})
