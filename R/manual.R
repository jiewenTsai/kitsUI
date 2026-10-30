# 使用手冊：最後一個分頁直接顯示手冊內容（用 app 的字型與配色），並提供獨立網頁的連結。
# 原稿與截圖在 tools/manual/；Rscript tools/manual/build_manual.R 產生 inst/manual/ 的檔案。

KIT_MANUAL_FILE <- "kitsui_manual.html"
KIT_MANUAL_BODY <- "kitsui_manual_body.html"
KIT_MANUAL_PREFIX <- "kitsui-manual"

#' 手冊所在的資料夾
#'
#' 從原始資料夾啟動（雲端版 app.R、run.bat）時用 inst/manual；
#' 安裝成套件後（run_kitsui()）用 system.file()。
#' @return 資料夾路徑；找不到手冊時是 NULL
#' @noRd
kit_manual_dir <- function() {
  dirs <- c(file.path("inst", "manual"), system.file("manual", package = "kitsUI"))
  files <- c(KIT_MANUAL_FILE, KIT_MANUAL_BODY)
  ok <- vapply(dirs, function(d) nzchar(d) && all(file.exists(file.path(d, files))), logical(1))
  if (!any(ok)) NULL else normalizePath(dirs[ok][1])
}

#' 讓瀏覽器讀得到手冊與截圖（kit_app() 啟動前呼叫一次）
#' @return 不可見的 TRUE / FALSE：找到手冊了嗎
#' @noRd
kit_setup_manual <- function() {
  dir <- kit_manual_dir()
  if (!is.null(dir)) shiny::addResourcePath(KIT_MANUAL_PREFIX, dir)
  invisible(!is.null(dir))
}

#' 「使用手冊」分頁：手冊內容，加上「在新分頁開啟」的連結
#' @noRd
kit_manual_ui <- function() {
  dir <- kit_manual_dir()
  if (is.null(dir)) {
    return(ui_callout(warning = TRUE, "找不到使用手冊（inst/manual/）。"))
  }
  body <- readLines(file.path(dir, KIT_MANUAL_BODY), encoding = "UTF-8", warn = FALSE)
  src <- paste0(KIT_MANUAL_PREFIX, "/", KIT_MANUAL_FILE)
  shiny::tagList(
    shiny::tags$style(kit_manual_css()),
    shiny::tags$p(
      class = "kit-hint mt-3",
      "要一邊看手冊一邊操作，可以",
      shiny::tags$a(href = src, target = "_blank", rel = "noopener", "在新分頁開啟手冊"),
      "。"
    ),
    shiny::tags$div(class = "kit-manual", shiny::HTML(paste(body, collapse = "\n")))
  )
}

#' 手冊的樣式：只作用在 .kit-manual 裡（class 都有 km- 前綴）
#' @noRd
kit_manual_css <- function() {
  cols <- kit_colors()
  css <- "
    .kit-manual .km-wrap {
      display: grid; grid-template-columns: 13rem minmax(0, 1fr); gap: 0 2.5rem;
    }
    .kit-manual .km-top { grid-column: 1 / -1; border-bottom: 1px solid BORDER;
      padding-bottom: 1rem; margin-bottom: 0.5rem; }
    .kit-manual .km-top h1 { font-size: 1.6rem; color: PRIMARY; margin: 0.25rem 0 0.5rem; }
    .kit-manual .km-eyebrow { font-size: 0.8rem; letter-spacing: 0.06em; font-weight: 700;
      color: MUTED; }
    .kit-manual .km-lede { max-width: 48em; margin: 0; }
    .kit-manual .km-meta { display: flex; flex-wrap: wrap; gap: 0.25rem 1.25rem;
      font-size: 0.85rem; color: MUTED; margin-top: 0.5rem; }
    .kit-manual .km-standalone { display: none; }
    .kit-manual .km-toc { position: sticky; top: 1rem; align-self: start; font-size: 0.9rem;
      max-height: calc(100vh - 2rem); overflow-y: auto; padding-top: 1rem; }
    .kit-manual .km-toc .km-h { font-size: 0.75rem; font-weight: 700; letter-spacing: 0.06em;
      color: MUTED; margin: 1rem 0 0.25rem; }
    .kit-manual .km-toc a { display: block; padding: 0.15rem 0 0.15rem 0.6rem; color: TEXT;
      text-decoration: none; border-left: 2px solid transparent; line-height: 1.5; }
    .kit-manual .km-toc a:hover, .kit-manual .km-toc a:focus-visible { color: PRIMARY;
      border-left-color: BORDER; }
    .kit-manual .km-toc a.km-t1 { border-left-color: PRIMARY; }
    .kit-manual .km-toc a.km-t2 { border-left-color: INFO; }
    .kit-manual main { min-width: 0; }
    .kit-manual section { margin-top: 2.5rem; }
    .kit-manual [id^='man-'] { scroll-margin-top: 1rem; }
    .kit-manual h2 { font-size: 1.4rem; color: TEXT; margin-bottom: 0.75rem; }
    .kit-manual h3 { font-size: 1.15rem; margin-top: 2rem; display: flex; flex-wrap: wrap;
      gap: 0.5rem; align-items: baseline; }
    .kit-manual h4 { font-size: 1rem; margin-top: 1.25rem; }
    .kit-manual p, .kit-manual ul, .kit-manual ol { max-width: 48em; }
    .kit-manual .km-col { font-family: var(--bs-font-monospace);
      font-size: 0.85em; background: #eef2ec; padding: 0 0.3em; border-radius: 4px; }
    .kit-manual .km-req { color: PRIMARY; font-weight: 700; }
    .kit-manual .km-table-wrap { overflow-x: auto; margin: 1rem 0; }
    .kit-manual table { border-collapse: collapse; width: 100%; background: #fff;
      font-size: 0.92rem; }
    .kit-manual th, .kit-manual td { padding: 0.5rem 0.75rem; border-bottom: 1px solid BORDER;
      text-align: left; vertical-align: top; }
    .kit-manual th { font-size: 0.8rem; color: MUTED; }
    .kit-manual th.km-t1 { color: PRIMARY; }
    .kit-manual th.km-t2 { color: INFO; }
    .kit-manual .km-shot { margin: 1rem 0 1.5rem; }
    .kit-manual .km-shot img { display: block; width: 100%; max-width: 100%; height: auto;
      border: 1px solid BORDER; border-radius: 6px; background: #fff;
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.08); }
    .kit-manual .km-narrow img { max-width: 760px; }
    .kit-manual figcaption { font-size: 0.85rem; color: MUTED; margin-top: 0.4rem;
      max-width: 52em; }
    .kit-manual .km-note { background: #eef5ec; border-left: 4px solid PRIMARY;
      border-radius: 4px; padding: 0.75rem 1rem; margin: 1rem 0; max-width: 48em; }
    .kit-manual .km-note.km-warn { background: #fdf3e1; border-left-color: #e0a000; }
    .kit-manual .km-note.km-t2 { background: #e8f1fa; border-left-color: INFO; }
    .kit-manual .km-note p { margin: 0.25rem 0; }
    .kit-manual .km-task-head { border-top: 4px solid PRIMARY; padding-top: 1rem; }
    .kit-manual .km-task-head.km-t2 { border-top-color: INFO; }
    .kit-manual .km-task-tag { display: inline-block; font-size: 0.8rem; font-weight: 700;
      border-radius: 999px; padding: 0.1rem 0.7rem; color: #fff; background: PRIMARY; }
    .kit-manual .km-task-tag.km-t2 { background: INFO; }
    .kit-manual .km-step-no { font-size: 0.85rem; font-weight: 700; color: #fff;
      background: PRIMARY; border-radius: 4px; padding: 0 0.45rem; }
    .kit-manual section.km-t2 .km-step-no { background: INFO; }
    .kit-manual .km-keep { display: grid; grid-template-columns: repeat(auto-fit, minmax(15rem, 1fr));
      gap: 0.75rem; max-width: 48em; margin: 1rem 0; }
    .kit-manual .km-keep > div { background: #fff; border: 1px solid BORDER; border-radius: 6px;
      padding: 0.75rem 1rem; }
    .kit-manual .km-k { font-weight: 700; word-break: break-all; }
    .kit-manual .km-v { font-size: 0.9rem; color: MUTED; }
    .kit-manual .km-terms { display: grid; grid-template-columns: max-content 1fr;
      gap: 0.35rem 1.25rem; max-width: 48em; }
    .kit-manual .km-terms dd { margin: 0; color: MUTED; }
    .kit-manual footer { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid BORDER;
      font-size: 0.85rem; color: MUTED; }
    @media (max-width: 900px) {
      .kit-manual .km-wrap { grid-template-columns: minmax(0, 1fr); }
      .kit-manual .km-toc { display: none; }
      .kit-manual .km-terms { grid-template-columns: 1fr; }
    }
  "
  keys <- c(PRIMARY = cols$primary, INFO = cols$info, BORDER = cols$border,
            MUTED = "#616161", TEXT = cols$text_muted)
  for (k in names(keys)) css <- gsub(k, keys[[k]], css, fixed = TRUE)
  shiny::HTML(css)
}
