# 各分頁共用的小元件：說明框、提示框、檢查結果、R 程式、下載鈕

#' 分頁說明框（這一頁做什麼、步驟、會得到；note 是一行補充）
#' @noRd
mod_guide <- function(goal, steps, output, note = NULL) {
  shiny::tags$div(
    class = "kit-guide",
    shiny::tags$p(shiny::tags$strong("這一頁做什麼："), goal),
    shiny::tags$div(
      shiny::tags$strong("步驟："),
      shiny::tags$ol(class = "kit-steps", lapply(steps, shiny::tags$li))
    ),
    shiny::tags$p(shiny::tags$strong("會得到："), output),
    if (!is.null(note)) shiny::tags$p(class = "kit-guide-note", note)
  )
}

#' 提示框（warning = TRUE 時是橘色的提醒：.kit-callout-warn，深棕字、淺橘底）
#' @noRd
ui_callout <- function(..., warning = FALSE) {
  cls <- paste("kit-callout", if (warning) "kit-callout-warn")
  shiny::tags$div(class = cls, ...)
}

#' 橘色的提醒：一句標題與一串項目（不擋下一步）
#' @param items 中文句子；長度 0 時不顯示
#' @noRd
ui_warn_list <- function(head, items, ...) {
  if (length(items) == 0) return(NULL)
  ui_callout(shiny::tags$strong(head), shiny::tags$ul(lapply(items, shiny::tags$li)), ...,
             warning = TRUE)
}

#' 找不到 pandoc 時，頁面最上方的提醒
#' @noRd
kit_pandoc_notice <- function() {
  shiny::tags$div(class = "kit-callout kit-callout-warn kit-pandoc-notice", role = "alert",
                  KIT_PANDOC_MISSING)
}

#' 還沒有這一回合的題目校準：提醒，並連到 ②（按了切換分頁的是 input$go_items）
#' @noRd
ui_go_items <- function(ns) {
  ui_callout(
    "還沒有這一回合的題目校準。請先到 ",
    shiny::actionLink(ns("go_items"), "「② 題目校準」"), " 跑這一回合。",
    warning = TRUE
  )
}

#' 狀態列：type 是 "success"、"info"、"danger"；head 是【】裡的粗體字
#' @noRd
ui_alert <- function(type, head, ...) {
  shiny::tags$div(
    class = paste0("alert alert-", type, " kit-status"), role = "alert",
    shiny::tags$strong(head), ...
  )
}

#' 開啟 kitJoin 雲端版的按鈕（新分頁）
#' @noRd
ui_kitjoin_button <- function(label = "開啟 kitJoin", class = "btn-outline-primary") {
  shiny::tags$a(
    href = KITJOIN_URL, target = "_blank", rel = "noopener noreferrer",
    class = paste("btn kit-ext-btn", class), shiny::icon("arrow-up-right-from-square"), label
  )
}

#' 資料不合格式時的一句話：到 kitJoin ⑥ 轉好再上傳
#' @noRd
ui_kitjoin_hint <- function(class = "btn-sm btn-outline-primary", hint_class = "kit-hint") {
  shiny::tags$p(
    class = hint_class,
    "資料是寬格式（一題一欄）、欄名不一樣，或分數不是從 0 開始？",
    "可以先到 kitJoin 的「⑥ 轉成 kits 作答檔」轉好，再回來上傳。",
    ui_kitjoin_button(class = class)
  )
}

#' 下一步：一句話與一個切換分頁的按鈕
#' @noRd
ui_next_step <- function(text, button_id, label) {
  ui_callout(
    shiny::tags$p(text),
    shiny::actionButton(button_id, label, class = "btn-primary")
  )
}

#' 檢查結果（kits_check()）：UI 一格
#' @noRd
ui_check_panel <- function(id, title = "檢查結果") {
  shiny::tagList(
    shiny::h5(title),
    shiny::tableOutput(id)
  )
}

#' 畫面上的表格：小數點 2 位；整數欄不顯示小數（下載的 csv 保留完整數字）
#' @param expr 產生表格的運算式（和 shiny::renderTable() 一樣，每次重算）
#' @noRd
kit_table <- function(expr, digits = 2) {
  q <- substitute(expr)
  env <- parent.frame()
  shiny::renderTable(whole_as_integer(eval(q, env)), digits = digits, striped = TRUE,
                     spacing = "s", na = "")
}

#' 數值都是整數的欄改成 integer，表格上就不會顯示 3.00
#' @noRd
whole_as_integer <- function(df) {
  if (!is.data.frame(df)) return(df)
  is_whole <- \(x) is.double(x) && all(is.na(x) | (abs(x) < 1e9 & x == round(x)))
  out <- lapply(df, \(x) if (is_whole(x)) as.integer(x) else x)
  data.frame(out, check.names = FALSE)
}

#' 檢查結果：把一個或幾個 kits 物件交給 kits_check()，回傳表格
#' @noRd
check_table <- function(...) {
  objs <- Filter(Negate(is.null), list(...))
  if (length(objs) == 0) return(NULL)
  out <- do.call(kits::kits_check, objs)
  as.data.frame(out)
}

#' 檢查表沒有任何提醒時，回傳一列「都通過」（欄名 result）；沒有可以檢查的東西（NULL）時不顯示
#' @noRd
checks_or_ok <- function(tab) {
  if (is.null(tab) || nrow(tab) > 0) return(tab)
  data.frame(result = "kits_check()：全部通過，沒有提醒。")
}

#' R 程式：可以複製的程式碼區塊
#' @noRd
ui_code_panel <- function(id, title = "用 R 怎麼做") {
  shiny::tags$details(
    shiny::tags$summary(title),
    shiny::verbatimTextOutput(id)
  )
}

#' 下載鈕的位置：一個 uiOutput（id 後面加 _ui）
#'
#' 按鈕在 server 端用 render_download_csv()、render_download_plot() 畫（R/download.R 的
#' kit_download_button()：網址最後一段、檔名標頭、download 屬性都是同一個檔名）。
#' @param id 按鈕的 id（含 namespace）
#' @noRd
ui_download <- function(id) shiny::uiOutput(paste0(id, "_ui"), inline = TRUE)

# 下載鈕的樣式
KIT_DL_CLASS <- "btn-sm btn-outline-primary"

#' 下載鈕：一張表存成 UTF-8 csv（Excel 看得懂中文）
#'
#' 按鈕的字一律寫成「下載…（CSV）」「下載…（HTML）」。
#' 在 renderUI() 裡呼叫（或用 render_download_csv()）。檔名在畫按鈕時決定：
#' name 是函式時，裡面讀的 input 或 reactive 變了，按鈕就重畫。
#' 檔案在按下載時才寫，寫的是當下畫面上的內容；表還沒有內容時寫一列說明。
#' @param session 模組的 session；id 是按鈕的 id（不含 namespace）
#' @param table_fun 傳回要下載的表的函式（或 reactive）
#' @param name 檔名（不含副檔名）：字串，或傳回它的函式
#' @noRd
download_csv_ui <- function(session, id, label, table_fun, name) {
  write <- \(file) readr::write_excel_csv(csv_or_note(table_fun), file)
  download_button_ui(session, id, label, name, "csv", write)
}

#' 下載鈕：plotly 圖存成單一 html（要 pandoc）
#'
#' 找不到 pandoc 時，按鈕換成不能按的鈕與一句說明。
#' 圖還沒有內容或存不了時，畫面上跳出紅色的通知（plot_save()）。
#' @inheritParams download_csv_ui
#' @param plot_fun 傳回 plotly 圖的函式（或 reactive）
#' @noRd
download_plot_ui <- function(session, id, label, plot_fun, name) {
  if (!kit_pandoc_ok()) return(ui_download_off(label, KIT_PANDOC_MISSING))
  write <- \(file) plot_save(plot_fun, file)
  download_button_ui(session, id, label, name, "html", write)
}

#' 放在 ui_download(ns(id)) 的位置：output[[paste0(id, "_ui")]] <- render_download_csv(...)
#' @noRd
render_download_csv <- function(session, id, label, table_fun, name) {
  shiny::renderUI(download_csv_ui(session, id, label, table_fun, name))
}

#' 圖的版本（同上）
#' @noRd
render_download_plot <- function(session, id, label, plot_fun, name) {
  shiny::renderUI(download_plot_ui(session, id, label, plot_fun, name))
}

#' 一個下載鈕（在 renderUI() 裡呼叫）：檔名是 download_name(name) 加上副檔名 ext
#'
#' 檔名拿不到時（例如還沒有量尺，req() 停下來）先不畫；拿到時 renderUI() 會自己重畫。
#' @noRd
download_button_ui <- function(session, id, label, name, ext, content, class = KIT_DL_CLASS) {
  base <- tryCatch(download_name(name), error = \(e) character())
  if (length(base) != 1 || !nzchar(base)) return(NULL)
  kit_download_button(session, id, label, paste0(base, ".", ext), content, class = class)
}

#' 不能下載時：不能按的鈕與一句說明（例如找不到 pandoc）
#' @noRd
ui_download_off <- function(label, why) {
  shiny::tags$span(
    class = "kit-download-off",
    shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                       disabled = NA, shiny::icon("download"), label),
    shiny::tags$small(class = "kit-hint kit-warn-text", why)
  )
}

#' 存一張圖（kits::kits_save_plot()）；不行時通知使用者，再停下來
#' @param p plotly 圖，或傳回它的函式
#' @param pandoc_ok 找得到 pandoc 嗎
#' @noRd
plot_save <- function(p, file, pandoc_ok = kit_pandoc_ok()) {
  notify <- \(text) {
    if (!is.null(shiny::getDefaultReactiveDomain())) {
      shiny::showNotification(text, type = "error", duration = KIT_NOTIFY_ERROR_SECONDS)
    }
    kit_abort(text)
  }
  if (!pandoc_ok) notify(KIT_PANDOC_MISSING)
  if (is.function(p)) p <- tryCatch(p(), error = \(e) NULL)
  if (is.null(p)) notify("這張圖還沒有內容。請先完成這一頁的步驟。")
  out <- tryCatch(kits::kits_save_plot(p, file), error = \(e) e)
  if (inherits(out, "error")) notify(kit_error_text(out, "存圖檔（HTML）"))
  invisible(file)
}

#' 檔名：函式就呼叫它；只留英文字母、數字、底線與減號
#' @noRd
download_name <- function(name) {
  if (is.function(name)) name <- name()
  gsub("[^A-Za-z0-9_-]+", "_", name)
}

#' 要下載的表；拿不到時（還沒有內容、出錯）回傳一列說明
#' @noRd
csv_or_note <- function(table_fun) {
  tab <- tryCatch(table_fun(), error = \(e) e)
  if (inherits(tab, "error") || is.null(tab)) {
    why <- if (inherits(tab, "error")) conditionMessage(tab) else ""
    text <- "這張表還沒有內容。請先完成這一頁的步驟。"
    if (nzchar(why)) text <- paste0(text, "（", why, "）")
    return(data.frame(note = text))
  }
  as.data.frame(tab)
}

# 畫面與下載用的欄名（kits、固定檔裡仍用左邊的名字）：beta -> difficulty、thr -> threshold、
# theta -> ability、
# d -> d_score；標準誤加 _se
KITSUI_DISPLAY_NAMES <- c(
  beta = "difficulty", beta_se = "difficulty_se", thr = "threshold", thr_se = "threshold_se",
  theta = "ability", theta_se = "ability_se", se = "ability_se",
  d = "d_score", d_se = "d_score_se"
)

#' 把表的欄名換成畫面用的名字（只換表裡有的欄）
#' @noRd
kitsui_display_names <- function(df) {
  hit <- intersect(names(df), names(KITSUI_DISPLAY_NAMES))
  names(df)[match(hit, names(df))] <- KITSUI_DISPLAY_NAMES[hit]
  df
}
