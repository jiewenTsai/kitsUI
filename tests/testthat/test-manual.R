test_that("使用手冊：最後一頁直接顯示手冊內容，另有新分頁的連結", {
  dir <- kit_manual_dir()
  expect_false(is.null(dir))
  html <- as.character(kit_manual_ui())
  expect_match(html, "class=\"kit-manual\"", fixed = TRUE)
  expect_false(grepl("<iframe", html, fixed = TRUE))
  expect_match(html, "href=\"kitsui-manual/kitsui_manual.html\" target=\"_blank\"", fixed = TRUE)
  # 截圖從 kitsui-manual/img/ 讀，檔案都在
  imgs <- regmatches(html, gregexpr("kitsui-manual/img/[A-Za-z0-9_]+\\.webp", html))[[1]]
  expect_gt(length(imgs), 10)
  expect_true(all(file.exists(file.path(dir, sub("^kitsui-manual/", "", imgs)))))
  # class 加了 km- 前綴（不會碰到 Bootstrap 的 .col）；錨點加了 man- 前綴
  expect_false(grepl("class=\"col\"", html, fixed = TRUE))
  expect_match(html, "class=\"km-col\"", fixed = TRUE)
  expect_match(html, "id=\"man-t1-data\"", fixed = TRUE)
  expect_match(html, "href=\"#man-t1-data\"", fixed = TRUE)
  tabs <- as.character(kit_ui(NULL))
  expect_match(tabs, "data-value=\"manual\"", fixed = TRUE)
  expect_true(regexpr("④ 受試者評分", tabs) < regexpr("使用手冊", tabs))
})

test_that("使用手冊：獨立網頁完整，沒有本機路徑", {
  dir <- kit_manual_dir()
  page <- paste(readLines(file.path(dir, KIT_MANUAL_FILE), encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  expect_match(page, "<meta charset=\"utf-8\">", fixed = TRUE)
  expect_match(page, "src=\"img/t1_data_pick.webp\"", fixed = TRUE)
  for (f in c(KIT_MANUAL_FILE, KIT_MANUAL_BODY)) {
    text <- paste(readLines(file.path(dir, f), encoding = "UTF-8", warn = FALSE), collapse = "\n")
    expect_false(grepl("{{", text, fixed = TRUE))
    expect_false(grepl("/home/|/tmp/|C:\\\\Users", text))
  }
})
