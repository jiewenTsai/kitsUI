# 共用小工具

#' 給使用者看的錯誤
#'
#' 訊息需是白話中文、說明原因與怎麼修；UI 會原文顯示，不加技術細節。
#' 其他未預期的錯誤則由 UI 包成「發生未預期的錯誤」並把原始訊息收在技術細節裡。
#' @noRd
kit_abort <- function(...) {
  stop(structure(
    class = c("kit_user_error", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
}

#' x 為 NULL 時改用 y（R 4.4 起 base 也有，這裡為了支援較舊的 R）
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' 每列的 ID 合成一個比對用字串；任一 ID 欄缺漏的列回傳 NA
#' @noRd
key_strings <- function(df, by_vars) {
  keys <- lapply(df[by_vars], function(x) as.character(unclass(x)))
  out <- do.call(paste, c(keys, sep = "\r"))
  has_na <- Reduce(`|`, lapply(keys, is.na))
  out[has_na] <- NA_character_
  out
}

#' 「t1（wave1.csv）」這種給使用者看的檔案稱呼
#' @noRd
file_display_name <- function(wave_label, file_name) {
  ifelse(nzchar(wave_label), paste0(wave_label, "（", file_name, "）"), file_name)
}

#' 錯誤轉成給使用者看的句子：kit_abort() 的訊息原文；其他錯誤前面加上在做什麼
#' @noRd
kit_error_text <- function(e, action) {
  if (inherits(e, "kit_user_error")) return(conditionMessage(e))
  paste0(action, "時出錯：", conditionMessage(e))
}

# pandoc（kits::kits_save_plot() 存 html 要用） ----

#' 常見的 pandoc 資料夾：RStudio 與 Quarto 附的（Linux、macOS、Windows）
#' @noRd
kit_pandoc_dirs <- function() {
  tools <- c(
    "/opt/quarto/bin/tools", "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Quarto/bin/tools"
  )
  c(rbind(file.path(tools, "x86_64"), file.path(tools, "aarch64"), tools),
    "/usr/lib/rstudio/bin/pandoc", "/Applications/RStudio.app/Contents/MacOS/pandoc")
}

#' 找 pandoc 的資料夾：PATH、RSTUDIO_PANDOC、rmarkdown::find_pandoc()、常見的資料夾
#' @param dirs 常見的資料夾
#' @param sys,env Sys.which("pandoc") 與 RSTUDIO_PANDOC（測試時可以換）
#' @param use_rmarkdown 有 rmarkdown 時也問它
#' @return 資料夾；找不到時 NULL
#' @noRd
kit_find_pandoc <- function(dirs = kit_pandoc_dirs(), sys = Sys.which("pandoc"),
                            env = Sys.getenv("RSTUDIO_PANDOC"), use_rmarkdown = TRUE) {
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  if (nzchar(sys)) return(dirname(sys))
  if (nzchar(env) && file.exists(file.path(env, exe))) return(env)
  if (use_rmarkdown && requireNamespace("rmarkdown", quietly = TRUE)) {
    dir <- rmarkdown::find_pandoc(cache = FALSE)$dir
    if (length(dir) == 1 && nzchar(dir)) return(dir)
  }
  hit <- dirs[file.exists(file.path(dirs, exe))]
  if (length(hit) > 0) hit[1] else NULL
}

#' 啟動時找 pandoc；不在 PATH 上時設定 RSTUDIO_PANDOC（rmarkdown 與 kits 都看它）
#' @return 不可見的 TRUE（找到）或 FALSE
#' @noRd
kit_setup_pandoc <- function() {
  dir <- kit_find_pandoc()
  if (is.null(dir)) return(invisible(FALSE))
  if (!nzchar(Sys.which("pandoc"))) Sys.setenv(RSTUDIO_PANDOC = dir)
  if (requireNamespace("rmarkdown", quietly = TRUE)) rmarkdown::find_pandoc(cache = FALSE)
  invisible(TRUE)
}

#' 現在找得到 pandoc 嗎（和 kits::kits_save_plot() 的判斷一樣）
#' @noRd
kit_pandoc_ok <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE)) return(rmarkdown::pandoc_available())
  nzchar(Sys.which("pandoc"))
}
