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

#' 下載鈕：一個圖（HTML）或一張表（CSV）
#'
#' 按鈕的字一律寫成「下載…（CSV）」「下載…（HTML）」。
#' @noRd
ui_download <- function(id, label) {
  shiny::downloadButton(id, label, class = "btn-sm btn-outline-primary")
}

#' 下載處理：plotly 圖存成單一 html
#'
#' name 是字串，或按下載時才呼叫的函式（檔名跟著目前的選擇變）。
#' 找不到 pandoc 或存不了時，畫面上跳出紅色的通知（不是空白的錯誤頁）。
#' @noRd
download_plot <- function(plot_fun, name) {
  shiny::downloadHandler(
    filename = function() paste0(download_name(name), ".html"),
    content = function(file) plot_save(plot_fun, file)
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

#' 下載處理：表存成 UTF-8 csv（Excel 看得懂中文）
#'
#' 表還沒有內容時（例如還沒按按鈕），檔案裡寫一行說明，不讓瀏覽器下載失敗。
#' @inheritParams download_plot
#' @noRd
download_csv <- function(table_fun, name) {
  shiny::downloadHandler(
    filename = function() paste0(download_name(name), ".csv"),
    content = function(file) readr::write_excel_csv(csv_or_note(table_fun), file)
  )
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
