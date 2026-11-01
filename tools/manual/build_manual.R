# 產生「使用手冊」的兩個檔案（在 kitsUI 資料夾執行：Rscript tools/manual/build_manual.R）
#
# 原稿：tools/manual/kitsui_manual_src.html；{{名稱}} 是截圖 tools/manual/img/名稱.webp
# （截圖用 app 內建的示範資料）。
#
# 1. inst/manual/kitsui_manual_body.html：app 最後一頁直接顯示的內容（用 app 的字型與配色）。
#    class 加上 km- 前綴（避免和 Bootstrap 的 .col 等撞名），錨點 id 加上 man- 前綴。
# 2. inst/manual/kitsui_manual.html：獨立網頁（「在新分頁開啟」）。內容和 1. 相同，
#    套用 app 的主題（bslib 編譯好的 Bootstrap CSS）、頁首與手冊樣式，兩邊看起來一樣。
# 兩者共用 inst/manual/img/ 的截圖。
# 要讀 app 的主題與樣式（kitDesign），所以先載入套件（需要 kits、kitDesign 等相依套件）。

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

# 2. 獨立網頁：app 的主題、頁首與手冊樣式，內容和 app 裡的一樣
suppressMessages(pkgload::load_all(".", quiet = TRUE))
pick <- function(pattern) regmatches(head, regexpr(pattern, head))
deps <- bslib::bs_theme_dependencies(kitDesign::kit_theme())
fonts <- unlist(lapply(deps, function(d) if (!is.null(d$head)) as.character(d$head)))
css <- unlist(lapply(deps, function(d) {
  files <- unlist(d$stylesheet)
  if (length(files) == 0) return(NULL)
  vapply(file.path(d$src$file, files), function(f) {
    paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  }, character(1))
}))
# kitDesign 的樣式（inst/www/ 的三個 CSS，順序不能換）
kit_design_css <- paste(vapply(
  c("kit-base.css", "kit-brite.css", "kit-design.css"),
  function(f) paste(readLines(system.file("www", f, package = "kitDesign"), encoding = "UTF-8",
                              warn = FALSE), collapse = "\n"),
  character(1)
), collapse = "\n")
# 頁首：kitDesign 的頁首；標誌寫進頁面（獨立網頁沒有 kitDesign 的標誌檔）
ver <- kitDesign::kit_app_description("kitsUI")
header <- as.character(kitDesign::kit_header(
  "KIT 量尺建構工具", "臺灣幼兒發展調查資料庫", tagline = "使用手冊",
  version = ver$version, date = ver$date, package = "kitsUI",
  meta_extra = paste0("kits v", utils::packageVersion("kits")), logo = "inline"
))
standalone_css <- paste(
  "body { padding: 1rem 1.5rem 2rem; }",
  ".kit-manual .km-standalone { display: inline; }",
  sep = "\n"
)
standalone <- c(
  "<!doctype html>",
  "<html lang=\"zh-Hant\">",
  "<head>",
  "<meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  pick("<title>[^<]*</title>"),
  pick("<meta name=\"description\"[^>]*>"),
  fonts,
  paste0("<style>", css, "</style>"),
  paste0("<style>", kit_design_css, "</style>"),
  paste0("<style>", kit_manual_css(), "\n", standalone_css, "</style>"),
  "</head>",
  "<body>",
  header,
  "<div class=\"kit-manual\">",
  gsub("kitsui-manual/img/", "img/", app_body, fixed = TRUE),
  "</div>",
  "</body>",
  "</html>"
)
writeLines(enc2utf8(standalone), file.path(out_dir, "kitsui_manual.html"), useBytes = TRUE)
cat("寫好", out_dir, "：app 內容、獨立網頁、", length(keys), "張圖\n")
