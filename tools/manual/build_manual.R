# 產生 app 的「使用手冊」：inst/manual/kitsui_manual.html
#
# 在 kitsUI 資料夾執行：Rscript tools/manual/build_manual.R
# 手冊原稿是 tools/manual/kitsui_manual_src.html；{{名稱}} 換成 tools/manual/img/名稱.webp
# （截圖用 app 內建的示範資料）。輸出是單一 html 檔，圖片內嵌，離線也看得到圖。

src <- "tools/manual/kitsui_manual_src.html"
out <- "inst/manual/kitsui_manual.html"
body <- paste(readLines(src, encoding = "UTF-8", warn = FALSE), collapse = "\n")

# 圖片內嵌成 data URI
keys <- unique(regmatches(body, gregexpr("\\{\\{[A-Za-z0-9_]+\\}\\}", body))[[1]])
for (key in keys) {
  img <- file.path("tools/manual/img", paste0(gsub("[{}]", "", key), ".webp"))
  if (!file.exists(img)) stop("找不到圖片：", img)
  uri <- paste0("data:image/webp;base64,", jsonlite::base64_enc(readBin(img, "raw", file.size(img))))
  body <- gsub(key, uri, body, fixed = TRUE)
}

page <- c(
  "<!doctype html>",
  "<html lang=\"zh-Hant\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "</head>",
  "<body>",
  body,
  "</body>",
  "</html>"
)
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
writeLines(enc2utf8(page), out, useBytes = TRUE)
cat("寫好", out, "（", round(file.size(out) / 1e6, 2), "MB，", length(keys), "張圖）\n")
