# 產生「使用手冊」的兩個檔案（在 kitsUI 資料夾執行：Rscript tools/manual/build_manual.R）
#
# 原稿：tools/manual/kitsui_manual_src.html；{{名稱}} 是截圖 tools/manual/img/名稱.webp
# （截圖用 app 內建的示範資料）。
#
# 1. inst/manual/kitsui_manual_body.html：app 最後一頁直接顯示的內容（用 app 的字型與配色）。
#    class 加上 km- 前綴（避免和 Bootstrap 的 .col 等撞名），錨點 id 加上 man- 前綴。
# 2. inst/manual/kitsui_manual.html：獨立網頁（「在新分頁開啟」），保留原稿自己的樣式。
# 兩者共用 inst/manual/img/ 的截圖。

src <- "tools/manual/kitsui_manual_src.html"
out_dir <- "inst/manual"
lines <- readLines(src, encoding = "UTF-8", warn = FALSE)
page <- paste(lines, collapse = "\n")

# 原稿 = 頭（title、meta、字型、style）＋ <div class="wrap"> 內容
start <- regexpr("<div class=\"wrap\">", page, fixed = TRUE)
if (start < 0) stop("原稿裡找不到 <div class=\"wrap\">。")
head <- substr(page, 1, start - 1)
body <- substr(page, start, nchar(page))

# 截圖：複製到 inst/manual/img/
keys <- unique(gsub("[{}]", "", regmatches(body, gregexpr("\\{\\{[A-Za-z0-9_]+\\}\\}", body))[[1]]))
imgs <- file.path("tools/manual/img", paste0(keys, ".webp"))
if (!all(file.exists(imgs))) stop("找不到圖片：", paste(imgs[!file.exists(imgs)], collapse = ", "))
dir.create(file.path(out_dir, "img"), showWarnings = FALSE, recursive = TRUE)
invisible(file.copy(imgs, file.path(out_dir, "img"), overwrite = TRUE))
with_imgs <- function(x, prefix) gsub("\\{\\{([A-Za-z0-9_]+)\\}\\}", paste0(prefix, "\\1.webp"), x)

# 1. app 內的內容
prefix_classes <- function(x) {
  m <- gregexpr("class=\"[^\"]*\"", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(cls) {
    vapply(cls, function(one) {
      tokens <- strsplit(sub("^class=\"(.*)\"$", "\\1", one), "\\s+")[[1]]
      paste0("class=\"", paste0("km-", tokens, collapse = " "), "\"")
    }, character(1), USE.NAMES = FALSE)
  })
  x
}
app_body <- body |>
  with_imgs("kitsui-manual/img/") |>
  prefix_classes()
app_body <- gsub(" id=\"([^\"]+)\"", " id=\"man-\\1\"", app_body)
app_body <- gsub(" href=\"#([^\"]+)\"", " href=\"#man-\\1\"", app_body)
app_body <- gsub("\\s*<!--[^>]*-->", "", app_body)
# 目錄的錨點要跳得準：截圖不延遲載入（延遲載入時高度未知，跳轉位置會偏）
app_body <- gsub(" loading=\"lazy\"", "", app_body, fixed = TRUE)
writeLines(enc2utf8(app_body), file.path(out_dir, "kitsui_manual_body.html"), useBytes = TRUE)

# 2. 獨立網頁
standalone <- c(
  "<!doctype html>",
  "<html lang=\"zh-Hant\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  head,
  "</head>",
  "<body>",
  with_imgs(body, "img/"),
  "</body>",
  "</html>"
)
writeLines(enc2utf8(standalone), file.path(out_dir, "kitsui_manual.html"), useBytes = TRUE)
cat("寫好", out_dir, "：app 內容、獨立網頁、", length(keys), "張圖\n")
