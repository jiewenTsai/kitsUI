# 下載按鈕（R/download.R）測試共用的小工具（和 kitJoin 的一樣）

# 按下載：呼叫 kit_download_button() 註冊的回應函式
# （testServer 的假 session 不會真的註冊，註冊的東西存在 userData）
# id：模組裡用不含 namespace 的 id；kit_server 層用完整 id（如 "score-fixed_dl"）
kit_test_download <- function(session, id) {
  store <- session$userData$kit_downloads
  dl <- store[[id]] %||% store[[session$ns(id)]]
  if (is.null(dl)) stop("沒有註冊這個下載鈕：", id)
  dl$filter(dl$data, list())
}

# 下載回來的檔案路徑
kit_test_download_file <- function(session, id) kit_test_download(session, id)$content$file

# 下載的檔名（註冊時的檔名，也就是網址最後一段、標頭與 download 屬性用的名字）
kit_test_download_name <- function(session, id) {
  store <- session$userData$kit_downloads
  (store[[id]] %||% store[[session$ns(id)]])$data$filename
}

# HTML 裡某個 id 的 <a> 的屬性值
kit_link_attr <- function(html, id, attr) {
  tag <- regmatches(html, regexpr(paste0('<a [^>]*id="', id, '"[^>]*>'), html))
  value <- sub(paste0('.*\\s', attr, '="([^"]*)".*'), "\\1", tag)
  gsub("&amp;", "&", value, fixed = TRUE)
}

file_bytes <- function(path) readBin(path, "raw", file.size(path))

# zip 裡每個檔的位元組（zip 本身帶時間，不直接比）
zip_bytes <- function(path) {
  dir <- withr::local_tempdir()
  files <- utils::unzip(path, exdir = dir)
  stats::setNames(lapply(files, file_bytes), basename(files))
}

# testServer 裡：先畫出下載鈕（output$<id>_ui），再按下載，回傳檔案路徑
kit_test_dl_file <- function(output, session, id) {
  output[[paste0(id, "_ui")]]
  kit_test_download_file(session, id)
}

# 同上，讀成表
kit_test_dl_csv <- function(output, session, id) {
  readr::read_csv(kit_test_dl_file(output, session, id), show_col_types = FALSE)
}

# 測試時用 Quarto 附的 pandoc（有裝 Quarto 時），圖才能存成 html
kit_test_local_pandoc <- function(frame = parent.frame()) {
  pandoc <- "/opt/quarto/bin/tools/x86_64"
  if (dir.exists(pandoc)) withr::local_envvar(RSTUDIO_PANDOC = pandoc, .local_envir = frame)
  invisible(kit_pandoc_ok())
}
