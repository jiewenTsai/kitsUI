#' KIT Shiny 主題與版面元件
#'
#' 配色參考 KIT 資料跨波次串連平臺（https://kitwaves.hdfs.ntnu.edu.tw/）。
#' Bootswatch Brite 尚未納入 bslib，改以 Lumen 為底並覆寫 KIT 品牌色。

#' @return A [bslib::bs_theme()] object.
#' @export
kit_theme <- function() {
  # CJK 字型切成上百個 unicode-range 片段，local = FALSE 改由瀏覽器向
  # Google CDN 按需下載，避免伺服器啟動時抓下全部字檔；
  # 未指定 wght 時只提供 400，需列出 CSS 用到的字重
  kit_font <- bslib::font_collection(
    bslib::font_google(
      "Chiron GoRound TC", local = FALSE, wght = c(400, 500, 700)
    ),
    "sans-serif"
  )

  bslib::bs_theme(
    bootswatch = "lumen",
    primary = "#2d7535",
    secondary = "#616161",
    success = "#2d7535",
    info = "#2778c4",
    warning = "#c6d033",
    danger = "#d9534f",
    base_font = kit_font,
    heading_font = kit_font,
    "navbar-bg" = "#2d7535",
    "body-bg" = "#f5f5f5"
  )
}

#' 讀取 DESCRIPTION 的版本與日期
#'
#' 雲端版、本機 run.bat、套件 run_kitsui() 都以 DESCRIPTION 的
#' Version / Date 為準，三者顯示一致（不看檔案修改時間，因為解壓、
#' 安裝、部署都會改變 mtime）。每次更新請同步遞增 Version 並改 Date。
#' @noRd
kit_app_description <- function() {
  if ("kitsUI" %in% loadedNamespaces()) {
    desc <- utils::packageDescription("kitsUI")
    return(list(version = desc$Version, date = desc$Date))
  }
  # 雲端版與 run.bat：工作目錄為專案根目錄；測試時在 tests/testthat，往上找
  paths <- c("DESCRIPTION", file.path("..", "DESCRIPTION"), file.path("..", "..", "DESCRIPTION"))
  for (path in paths[file.exists(paths)]) {
    desc <- read.dcf(path, fields = c("Package", "Version", "Date"))
    if (identical(unname(desc[1, "Package"]), "kitsUI")) {
      return(list(version = unname(desc[1, "Version"]), date = unname(desc[1, "Date"])))
    }
  }
  list(version = NA, date = NA)
}

#' KIT 品牌色常數（供自訂 CSS 或圖表使用）
#' @export
kit_colors <- function() {
  list(
    primary = "#2d7535",
    primary_light = "#3a8f45",
    accent = "#c6d033",
    subtitle = "#e8f0a0",
    body_bg = "#f5f5f5",
    text_muted = "#424242",
    border = "#e0e0e0",
    info = "#2778c4",
    danger = "#d9534f"
  )
}

#' KIT App 共用 CSS
#' @export
kit_css <- function() {
  cols <- kit_colors()
  shiny::HTML(sprintf("
    .kit-header {
      background: linear-gradient(135deg, %1$s 0%%, %2$s 100%%);
      color: #fff;
      padding: 1.25rem 1.5rem;
      border-radius: 0.5rem;
      border-bottom: 4px solid %3$s;
      box-shadow: 0 2px 8px rgba(45, 117, 53, 0.2);
    }
    .kit-header h1 {
      margin: 0;
      font-size: 1.75rem;
      font-weight: 500;
    }
    .kit-header p {
      margin: 0.35rem 0 0;
      opacity: 0.92;
      font-size: 0.95rem;
    }
    .kit-subtitle {
      color: %4$s;
      font-weight: 400;
    }
    .nav-tabs .nav-link.active {
      border-bottom: 3px solid %1$s;
      font-weight: 500;
    }
    .nav-tabs .nav-link {
      color: %5$s;
    }
    .table {
      background: #fff;
    }
    .well, pre {
      background-color: #fff;
      border: 1px solid %6$s;
    }
    .kit-table-scroll {
      overflow-x: auto;
      max-width: 100%%;
      margin-bottom: 0.5rem;
    }
    .kit-table-scroll table {
      font-size: 0.85rem;
      white-space: nowrap;
    }
    .kit-table-tall {
      max-height: 26rem;
      overflow-y: auto;
    }
    .kit-table-tall thead th {
      position: sticky;
      top: 0;
      background: #fff;
    }
    .kit-code-panel {
      margin-top: 2rem;
    }
    .kit-code-block {
      margin-bottom: 1rem;
    }
    .kit-code-head {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 0.3rem;
    }
    .kit-code-block pre {
      font-size: 0.8rem;
      padding: 0.75rem;
      margin: 0;
      white-space: pre;
      overflow-x: auto;
    }
    .kit-code-block pre code {
      padding: 0;
      background: none;
      color: inherit;
    }
    .alert.kit-cloud-banner {
      margin-bottom: 1rem;
      padding: 0.5rem 0.9rem;
      font-size: 0.9rem;
      background: #fff8e1;
      border: 1px solid #e6c65c;
      border-left: 4px solid #b38600;
      color: #3d2c00;
    }
    .alert.kit-cloud-banner a {
      color: #3d2c00;
    }
    .kit-pandoc-notice {
      margin-bottom: 1rem;
    }
    .kit-cloud-footer {
      margin: 2.5rem 0 1.5rem;
      padding-top: 1rem;
      border-top: 1px solid %6$s;
      font-size: 13px;
      line-height: 1.6;
      color: #4f4f4f;
    }
    .kit-cloud-footer p {
      margin: 0 0 0.45rem;
    }
    .kit-cloud-footer strong,
    .kit-cloud-footer-title {
      font-weight: 500;
      color: %5$s;
    }
    .kit-cloud-footer-title {
      margin-bottom: 0.4rem;
    }
    .kit-cloud-footer a {
      color: inherit;
      text-decoration: underline;
    }
    .kit-app-meta {
      margin-top: 0.35rem;
      font-size: 0.72rem;
      color: rgba(255, 255, 255, 0.65);
      line-height: 1.2;
      font-weight: 400;
      letter-spacing: 0.02em;
    }
    .kit-guide {
      background: #f1f7ee;
      border-left: 4px solid %1$s;
      border-radius: 0.375rem;
      padding: 0.9rem 1.1rem;
      margin: 1rem 0;
      color: #263238;
    }
    .kit-guide p {
      margin: 0 0 0.4rem;
    }
    .kit-guide-note {
      font-size: 0.85rem;
      color: #455a64;
    }
    .kit-guide details summary,
    .kit-example summary,
    .kit-status details summary {
      cursor: pointer;
      color: %1$s;
      font-size: 0.85rem;
    }
    .kit-steps {
      margin: 0.2rem 0 0.4rem 1.2rem;
      padding: 0;
    }
    .kit-hint {
      display: block;
      color: #546e7a;
      font-size: 0.85rem;
      margin: -0.4rem 0 0.9rem;
    }
    .kit-downloads .kit-hint {
      margin: 0.4rem 0 0;
    }
    .kit-hint.kit-tab-caption {
      margin: 0.7rem 0 0.4rem;
    }
    .kit-example {
      margin: -0.3rem 0 1rem;
    }
    .kit-example-table {
      max-width: 44rem;
      font-size: 0.85rem;
      margin-top: 0.4rem;
    }
    .kit-status {
      margin-top: 0.9rem;
    }
    .kit-status ul {
      margin: 0.4rem 0 0;
    }
    .kit-status-notes {
      margin-top: 0.4rem;
      font-size: 0.85rem;
    }
    .kit-downloads {
      margin: 0.9rem 0 1.2rem;
    }
    .kit-summary {
      background: #fff;
      border: 1px solid %6$s;
      border-radius: 0.375rem;
      padding: 0.8rem 1.1rem 0.3rem;
      margin-bottom: 1.2rem;
    }
    .kit-summary ul {
      margin: 0.4rem 0 0.5rem;
    }
    .kit-source-note {
      color: #455a64;
      font-size: 0.9rem;
    }
    .kit-callout {
      background: #eef5fc;
      border: 1px solid #c9dcf0;
      border-left: 4px solid #2778c4;
      border-radius: 0.375rem;
      color: #1f2d3a;
      padding: 0.8rem 1.1rem;
      margin: 0.8rem 0 1rem;
    }
    .kit-warn-text {
      color: #7a3a00;
      font-weight: 600;
    }
    .kit-callout-warn,
    .kit-callout-warning {
      background: #fff1e0;
      border-color: #f3c58f;
      border-left-color: #d9730d;
      color: #5c2b00;
    }
    .kit-callout-warn .kit-warn-text {
      color: #5c2b00;
    }
    .kit-columns {
      font-size: 0.8rem;
    }
    .kit-picks .kit-pick-row {
      display: flex;
      align-items: flex-start;
      gap: 0.6rem;
      margin-bottom: 0.35rem;
    }
    .kit-picks .kit-pick-cohort {
      font-weight: 600;
      flex: 0 0 2.8rem;
    }
    .kit-picks .kit-pick-boxes {
      display: flex;
      flex-wrap: wrap;
      gap: 0.15rem 0.9rem;
    }
    .kit-picks label.kit-pick {
      display: inline-flex;
      align-items: center;
      gap: 0.25rem;
      margin: 0;
      font-weight: 400;
      cursor: pointer;
    }
    .kit-callout ul {
      margin: 0.35rem 0 0;
    }
    .kit-callout a,
    .alert a {
      color: inherit;
      font-weight: 600;
      text-decoration: underline;
    }
    .kit-callout a {
      color: %1$s;
    }
    .kit-file-table {
      background: #fff;
      margin-bottom: 0.4rem;
    }
    .kit-file-table td {
      vertical-align: middle;
    }
    .kit-file-table .form-group {
      margin-bottom: 0;
    }
    .kit-row-buttons .btn {
      margin-right: 0.25rem;
    }
    .kit-audit {
      margin-top: 1.2rem;
    }
    .kit-codes-parsed {
      margin: -0.5rem 0 1rem;
    }
    .kit-manual-frame {
      display: block;
      width: 100%%;
      height: calc(100vh - 12rem);
      min-height: 32rem;
      border: 1px solid %6$s;
      border-radius: 8px;
      background: #fff;
    }
  ", cols$primary, cols$primary_light, cols$accent,
     cols$subtitle, cols$text_muted, cols$border))
}

#' 離開頁面前的提醒：伺服器送 kit_unload_guard（TRUE 或 FALSE）開關
#'
#' 有這一回合的結果時（② 校準過），關掉或重新整理分頁前，瀏覽器會先問一次。
#' @noRd
kit_js <- function() {
  shiny::HTML("
    (function() {
      var guard = false;
      window.addEventListener('beforeunload', function(e) {
        if (!guard) return;
        e.preventDefault();
        e.returnValue = '';
      });
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('kit_unload_guard', function(on) { guard = !!on; });
      });
      $(document).on('shiny:disconnected', function() { guard = false; });
    })();
  ")
}

#' 頁首版本、最後修改日期與 kits 的版本，例如「v0.1.0 · 2026-10-29 · kits 0.25.0」
#' @export
kit_app_meta <- function() {
  meta <- kit_app_description()
  shiny::div(
    class = "kit-app-meta",
    paste(c(
      if (!is.null(meta$version) && !is.na(meta$version)) {
        paste0("v", meta$version)
      },
      if (!is.null(meta$date) && !is.na(meta$date)) meta$date,
      paste("kits", utils::packageVersion("kits"))
    ), collapse = " · ")
  )
}

#' KIT 頁首區塊
#'
#' @param title 主標題
#' @param subtitle 副標題（顯示為淺黃綠色）
#' @param tagline 標語（接在副標題後）
#' @param meta 版本資訊（預設為 [kit_app_meta()]；傳 NULL 不顯示）
#' @export
kit_header <- function(
    title = "KIT 量尺建構工具",
    subtitle = "臺灣幼兒發展調查資料庫",
    tagline = "量尺：校準、連結、計分",
    meta = kit_app_meta()) {
  shiny::div(
    class = "kit-header mb-4",
    shiny::h1(title),
    shiny::p(
      shiny::tags$span(class = "kit-subtitle", subtitle),
      " · ", tagline
    ),
    meta
  )
}
