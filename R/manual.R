# 使用手冊：最後一個分頁，用 iframe 顯示 inst/manual/kitsui_manual.html
# （手冊自己的樣式不會影響 app）。原稿與截圖在 tools/manual/。

KIT_MANUAL_FILE <- "kitsui_manual.html"
KIT_MANUAL_PREFIX <- "kitsui-manual"

#' 手冊所在的資料夾
#'
#' 從原始資料夾啟動（雲端版 app.R、run.bat）時用 inst/manual；
#' 安裝成套件後（run_kitsui()）用 system.file()。
#' @return 資料夾路徑；找不到手冊時是 NULL
#' @noRd
kit_manual_dir <- function() {
  dirs <- c(file.path("inst", "manual"), system.file("manual", package = "kitsUI"))
  dirs <- dirs[nzchar(dirs) & file.exists(file.path(dirs, KIT_MANUAL_FILE))]
  if (length(dirs) == 0) NULL else normalizePath(dirs[1])
}

#' 讓瀏覽器讀得到手冊（kit_app() 啟動前呼叫一次）
#' @return 不可見的 TRUE / FALSE：找到手冊了嗎
#' @noRd
kit_setup_manual <- function() {
  dir <- kit_manual_dir()
  if (!is.null(dir)) shiny::addResourcePath(KIT_MANUAL_PREFIX, dir)
  invisible(!is.null(dir))
}

#' 「使用手冊」分頁
#' @noRd
kit_manual_ui <- function() {
  if (is.null(kit_manual_dir())) {
    return(ui_callout(warning = TRUE, "找不到使用手冊（inst/manual/kitsui_manual.html）。"))
  }
  src <- paste0(KIT_MANUAL_PREFIX, "/", KIT_MANUAL_FILE)
  shiny::tagList(
    shiny::tags$p(
      class = "kit-hint mt-3",
      "操作手冊分成兩個任務：過去波次的基礎計分、新增波次的計分（截圖用示範資料）。",
      shiny::tags$a(href = src, target = "_blank", rel = "noopener", "在新分頁開啟"),
      "，就可以一邊看手冊一邊操作。"
    ),
    shiny::tags$iframe(src = src, title = "kitsUI 操作手冊", class = "kit-manual-frame")
  )
}
