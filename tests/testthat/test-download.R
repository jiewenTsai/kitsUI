# 下載按鈕：檔名標頭（RFC 6266）、網址結尾是檔名、檔案內容和寫檔函式一樣

test_that("Content-Disposition：ASCII 檔名只有 filename", {
  expect_equal(
    kit_content_disposition("kitsui_fixed_R1.zip"),
    'attachment; filename="kitsui_fixed_R1.zip"'
  )
  expect_equal(kit_content_disposition('a"b\\c.csv'), 'attachment; filename="a\\"b\\\\c.csv"')
})

test_that("Content-Disposition：中文與空白 → ASCII 備用名 + filename*（UTF-8）", {
  expect_equal(
    kit_content_disposition("第一波 資料.csv"),
    paste0(
      'attachment; filename="kitsui.csv"; ',
      "filename*=UTF-8''%E7%AC%AC%E4%B8%80%E6%B3%A2%20%E8%B3%87%E6%96%99.csv"
    )
  )
  expect_equal(
    kit_content_disposition("kitsui_fixed_第一回.zip"),
    paste0(
      'attachment; filename="kitsui_fixed_.zip"; ',
      "filename*=UTF-8''kitsui_fixed_%E7%AC%AC%E4%B8%80%E5%9B%9E.zip"
    )
  )
  expect_match(kit_content_disposition("第一波_na.csv"), 'filename="kitsui_na.csv"', fixed = TRUE)
  expect_match(kit_content_disposition("w1 第一波.csv"), 'filename="w1.csv"', fixed = TRUE)
  # RFC 5987 attr-char 不編碼；其他（括號、引號）要編碼
  expect_match(kit_content_disposition("資料 (1)'.csv"), "%20%281%29%27.csv", fixed = TRUE)
})

test_that("副檔名與 Content-Type：csv、zip、html 各自的 MIME，其他是 octet-stream", {
  expect_equal(kit_download_ext("第一波.csv.gz"), "csv.gz")
  expect_equal(kit_download_ext("a.ZIP"), "ZIP")
  expect_equal(kit_download_ext("readme"), "")
  expect_equal(kit_ascii_filename("第一波.CSV.GZ"), "kitsui.CSV.GZ")
  expect_equal(kit_download_type("a.csv"), "text/csv; charset=UTF-8")
  expect_equal(kit_download_type("a.ZIP"), "application/zip")
  expect_equal(kit_download_type("item_lan_M03_fit.html"), "text/html; charset=UTF-8")
  expect_equal(kit_download_type("a.csv.gz"), "application/octet-stream")
  expect_equal(kit_download_type("readme"), "application/octet-stream")
})

test_that("網址最後一段是檔名（registerDataObj 的網址插入檔名），按鈕帶 download", {
  # 像模組的 session：registerDataObj() 自己加 namespace
  registered <- list()
  session <- list(
    ns = function(id) paste0("score-", id),
    registerDataObj = function(name, data, filterFunc) {
      registered[[paste0("score-", name)]] <<- list(data = data, filter = filterFunc)
      paste0("session/abc/dataobj/score-", name, "?w=&nonce=n1")
    }
  )
  html <- as.character(kit_download_button(
    session, "fixed_dl", "下載", "kitsui_fixed_R1.zip", function(file) writeLines("x", file),
    class = "btn-sm btn-primary"
  ))
  expect_equal(
    kit_link_attr(html, "score-fixed_dl", "href"),
    "session/abc/dataobj/score-fixed_dl/kitsui_fixed_R1.zip?w=&nonce=n1"
  )
  expect_equal(kit_link_attr(html, "score-fixed_dl", "download"), "kitsui_fixed_R1.zip")
  expect_match(html, "fa-download")
  expect_match(html, "btn-sm btn-primary")
  expect_named(registered, "score-fixed_dl")
  # 中文檔名：網址裡是百分比編碼
  html <- as.character(kit_download_button(
    session, "dl", "下載", "第一波_na.csv", function(file) writeLines("x", file)
  ))
  expect_equal(
    kit_link_attr(html, "score-dl", "href"),
    "session/abc/dataobj/score-dl/%E7%AC%AC%E4%B8%80%E6%B3%A2_na.csv?w=&nonce=n1"
  )
})

test_that("回應：200、標頭、檔案位元組和寫檔函式一樣（CSV、ZIP）", {
  df <- data.frame(id = c("0012", "0013"), x = c(1, NA), name = c("王", "李"))
  fixed <- kitsui_fixed_empty()
  session <- shiny::MockShinySession$new()
  cases <- list(
    list(name = "第一波 資料.csv", writer = \(file) readr::write_excel_csv(df, file),
         bytes = file_bytes),
    list(name = "kitsui_fixed_R1.zip", writer = \(file) kitsui_write_fixed(fixed, file),
         bytes = zip_bytes)
  )
  for (cs in cases) {
    kit_download_button(session, "dl", "下載", cs$name, cs$writer)
    res <- kit_test_download(session, "dl")
    expect_equal(res$status, 200)
    expect_equal(res$content_type, kit_download_type(cs$name))
    expect_equal(res$headers[["Content-Disposition"]], kit_content_disposition(cs$name))
    expect_equal(res$headers[["Cache-Control"]], "no-cache")
    expect_true(res$content$owned)
    expect_true(endsWith(res$content$file, paste0(".", kit_download_ext(cs$name))))
    old <- withr::local_tempfile(fileext = paste0(".", kit_download_ext(cs$name)))
    cs$writer(old)
    expect_equal(cs$bytes(res$content$file), cs$bytes(old))
  }
  expect_identical(kitsui_read_fixed(res$content$file), fixed)
})

test_that("回應：寫檔函式可以讀 reactive；出錯時回 500 並寫 log", {
  session <- shiny::MockShinySession$new()
  value <- shiny::reactiveVal("a")
  kit_download_button(session, "dl", "下載", "x.csv", function(file) writeLines(value(), file))
  value("b")
  expect_equal(readLines(kit_test_download_file(session, "dl")), "b")

  kit_download_button(session, "dl", "下載", "x.csv", function(file) stop("壞掉"))
  expect_message(res <- kit_test_download(session, "dl"), "kitsUI 下載失敗.*壞掉")
  expect_equal(res$status, 500)
  expect_match(res$content, "下載失敗")
})
