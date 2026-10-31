#' 啟動 KIT 量尺建構工具
#'
#' 在瀏覽器開啟 KIT 量尺建構工具（kitsUI）。資料只在您的電腦上處理，不會上傳到網路。
#'
#' @param launch.browser 是否自動開啟瀏覽器；也可以傳入開啟網址的函式
#' @param ... 傳給 [shiny::runApp()] 的其他參數，例如 `port`
#' @return 不可見的 NULL；app 關閉後才會回傳
#' @export
#' @examples
#' if (interactive()) {
#'   run_kitsui()
#' }
run_kitsui <- function(launch.browser = kitDesign::kit_open_browser, ...) {
  old <- Sys.getenv("KIT_DEPLOY", unset = NA)
  on.exit(
    if (is.na(old)) Sys.unsetenv("KIT_DEPLOY") else Sys.setenv(KIT_DEPLOY = old)
  )
  Sys.setenv(KIT_DEPLOY = "local")
  shiny::runApp(kit_app(), launch.browser = launch.browser, ...)
  invisible(NULL)
}
