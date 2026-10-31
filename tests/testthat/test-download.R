# 下載按鈕：kitDesign::kit_download_button() 的機制在 kitDesign 測；這裡測 kitsUI 自己的部分：
# app 名稱（.onLoad）、CSV 與固定檔 zip 的寫檔函式與 MIME

test_that("app 名稱是 kitsUI：中文檔名的 ASCII 備用名是 kitsui，log 寫 kitsUI", {
  expect_equal(getOption("kitDesign.app"), "kitsUI")
  session <- shiny::MockShinySession$new()
  kitDesign::kit_download_button(session, "dl", "下載", "第一波_na.csv",
                                 function(file) writeLines("x", file))
  expect_equal(
    kit_test_download(session, "dl")$headers[["Content-Disposition"]],
    'attachment; filename="kitsui_na.csv"; filename*=UTF-8\'\'%E7%AC%AC%E4%B8%80%E6%B3%A2_na.csv'
  )
  kitDesign::kit_download_button(session, "dl", "下載", "x.csv", function(file) stop("壞掉"))
  expect_message(res <- kit_test_download(session, "dl"), "kitsUI 下載失敗.*壞掉")
  expect_equal(res$status, 500)
})

test_that("回應：檔案位元組和寫檔函式一樣；csv 與 zip 用自己的 MIME（CSV、ZIP）", {
  df <- data.frame(id = c("0012", "0013"), x = c(1, NA), name = c("王", "李"))
  fixed <- kitsui_fixed_empty()
  session <- shiny::MockShinySession$new()
  cases <- list(
    list(name = "第一波 資料.csv", writer = \(file) readr::write_excel_csv(df, file),
         bytes = file_bytes, type = "text/csv; charset=UTF-8"),
    list(name = "kitsui_fixed_R1.zip", writer = \(file) kitsui_write_fixed(fixed, file),
         bytes = zip_bytes, type = "application/zip")
  )
  for (cs in cases) {
    kitDesign::kit_download_button(session, "dl", "下載", cs$name, cs$writer)
    res <- kit_test_download(session, "dl")
    expect_equal(res$status, 200)
    expect_equal(res$content_type, cs$type)
    ext <- sub("^.*[.]", ".", cs$name)
    expect_true(endsWith(res$content$file, ext))
    old <- withr::local_tempfile(fileext = ext)
    cs$writer(old)
    expect_equal(cs$bytes(res$content$file), cs$bytes(old))
  }
  expect_identical(kitsui_read_fixed(res$content$file), fixed)
  expect_equal(res$headers[["Content-Disposition"]], 'attachment; filename="kitsui_fixed_R1.zip"')
})
