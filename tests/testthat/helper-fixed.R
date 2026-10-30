# 模擬 kitsUI 0.3.12 以前的固定檔：VERSION 沒有 Checksums（舊的固定檔沒有核對碼）
old_version_file <- function(dir) {
  file <- file.path(dir, "VERSION")
  dcf <- read.dcf(file, keep.white = KITSUI_DCF_WHITE)
  write.dcf(dcf[, setdiff(colnames(dcf), "Checksums"), drop = FALSE], file,
            keep.white = KITSUI_DCF_WHITE)
}
