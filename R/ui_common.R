# 各分頁共用的小元件：說明框、提示框、檢查結果、R 程式、下載鈕

#' 分頁說明框：預設收起，只看到「這一頁做什麼」一句；按「看步驟」才展開步驟、會得到、補充
#' @noRd
mod_guide <- function(goal, steps, output, note = NULL) {
  shiny::tags$details(
    class = "kit-guide kit-guide-page",
    shiny::tags$summary(
      shiny::tags$strong("這一頁做什麼："), goal,
      shiny::tags$span(class = "kit-guide-more")
    ),
    shiny::tags$div(
      class = "kit-guide-body",
      shiny::tags$div(
        shiny::tags$strong("步驟："),
        shiny::tags$ol(class = "kit-steps", lapply(steps, shiny::tags$li))
      ),
      shiny::tags$p(shiny::tags$strong("會得到："), output),
      if (!is.null(note)) shiny::tags$p(class = "kit-guide-note", note)
    )
  )
}

#' 提示框（KIT 設計語言 3.7）：天藍的資訊；warning = TRUE 時是黃色的注意（.kit-callout-warning，
#' 和 kitJoin 同一個名字）
#' @noRd
ui_callout <- function(..., warning = FALSE) {
  cls <- paste("kit-callout", if (warning) "kit-callout-warning")
  shiny::tags$div(class = cls, ...)
}

#' 黃色的提醒（要注意）：一句標題與一串項目（不擋下一步）
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
  shiny::tags$div(class = "kit-callout kit-callout-warning kit-pandoc-notice", role = "alert",
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

# 狀態框的四種標籤與顏色（KIT 設計語言 3.6）；兩個 app 都只用這四種
KIT_STATUS <- list(
  done = list(label = "【完成】", class = "alert-success"),
  warn = list(label = "【完成，但請注意】", class = "alert-warning"),
  stop = list(label = "【停下來】", class = "alert-danger"),
  stale = list(label = "【要重新執行】", class = "alert-warning")
)

#' 狀態框：一個動作的結果，第一眼要看的東西
#'
#' 第一行是標籤與一句結論（大字粗體）；接著最多 3 點重點（points），再接其他內容（...），
#' 例如停下來時「怎麼辦」的按鈕、收起來的「kits 的原文」。
#' @param kind "done"（完成）、"warn"（完成，但請注意）、"stop"（停下來）、"stale"（要重新執行）
#' @param line 一句結論
#' @param points 重點（字串或 tag 的 list）；長度 0 時不顯示
#' @noRd
ui_status <- function(kind, line, ..., points = NULL) {
  st <- KIT_STATUS[[match.arg(kind, names(KIT_STATUS))]]
  shiny::tags$div(
    class = paste("alert kit-status", st$class), role = "alert",
    shiny::tags$div(class = "kit-status-line", shiny::tags$strong(st$label), " ", line),
    ui_status_points(points),
    ...
  )
}

# 狀態框第一層最多幾點（KIT 設計語言 3.6）；其他收在「還有 N 點」
KIT_STATUS_MAX_POINTS <- 3

#' 狀態框的重點：前 3 點直接看得到，其他收在「還有 N 點」（.kit-status-notes）
#' @param points 字串或 tag 的 list；長度 0 時 NULL
#' @noRd
ui_status_points <- function(points, max = KIT_STATUS_MAX_POINTS) {
  if (length(points) == 0) return(NULL)
  if (!is.list(points)) points <- as.list(points)
  shown <- utils::head(points, max)
  rest <- points[-seq_len(length(shown))]
  shiny::tagList(
    shiny::tags$ul(lapply(shown, shiny::tags$li)),
    if (length(rest) > 0) {
      shiny::tags$details(
        class = "kit-status-notes",
        shiny::tags$summary(paste0("還有 ", kit_num(length(rest)), " 點")),
        shiny::tags$ul(lapply(rest, shiny::tags$li))
      )
    }
  )
}

#' R 或 kits 的原文：收在「kits 的原文」裡（KIT 設計語言 3.18），第一層不放英文
#' @param raw 原文（英文）；長度 0 或 "" 時 NULL
#' @noRd
ui_raw_text <- function(raw, title = "kits 的原文") {
  if (length(raw) == 0 || !nzchar(raw[1])) return(NULL)
  shiny::tags$details(class = "kit-kits-says kit-status-raw", shiny::tags$summary(title),
                      shiny::tags$code(raw[1]))
}

# 按鈕的寫法（KIT 設計語言 3.4）：主要按鈕一頁一個；次要按鈕白底、小一號；
# 危險按鈕（這一回合不做、不用固定檔…）鮭紅底
KIT_BTN <- c(
  primary = "btn-primary",
  secondary = "btn-sm btn-outline-secondary",
  danger = "btn-sm btn-danger"
)

#' 一個按鈕（shiny::actionButton()），class 照 KIT_BTN
#' @param kind "primary"、"secondary" 或 "danger"
#' @noRd
ui_button <- function(id, label, kind = "secondary", ..., class = NULL) {
  shiny::actionButton(id, label, class = paste(c(KIT_BTN[[kind]], class), collapse = " "), ...)
}

#' 進階設定：預設收起（回合名稱、flag 的條件、用哪一個題庫、通過步驟…）
#' @param title 收合標題；可以是 tag（例如後面接現在的設定）
#' @noRd
ui_advanced <- function(..., title = "進階設定", open = FALSE) {
  shiny::tags$details(
    class = "kit-advanced", open = if (open) NA,
    shiny::tags$summary(title),
    shiny::tags$div(class = "kit-advanced-body", ...)
  )
}

#' 一排細節：結果下面一排收起來的區塊（題目參數、紀錄、kits 的檢查、用 R 怎麼做…）
#' @param ... ui_detail()
#' @noRd
ui_details_row <- function(...) {
  shiny::tags$div(class = "kit-details", ...)
}

#' 一個細節：預設收起，標題一行小字
#' @noRd
ui_detail <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    class = "kit-detail", open = if (open) NA,
    shiny::tags$summary(title),
    shiny::tags$div(class = "kit-detail-body", ...)
  )
}

#' 設定摘要：結果上面一行小字，例如「本回合 R1：M03、M36 · 3、6、36 個月 · 參照世代 M03」
#' @noRd
ui_settings_line <- function(text) {
  if (length(text) == 0 || !nzchar(text)) return(NULL)
  shiny::tags$p(class = "kit-settings-line", text)
}

#' 設定摘要的一句：回合名稱、世代、波次（帶單位）、參照世代、固定檔
#' @param d mod_data_server() 的回傳值（list）；NULL 時是 ""
#' @noRd
kitsui_settings_text <- function(d) {
  if (is.null(d)) return("")
  picks <- d$picks %||% d$df
  cohorts <- sort(unique(stats::na.omit(picks$cohort)))
  unit <- kitsui_wave_unit(d$fixed)
  waves <- sort(unique(stats::na.omit(picks$wave_m)))
  wave_text <- switch(
    unit,
    years = paste0(paste(format(waves, trim = TRUE), collapse = "、"), " 年"),
    waves = paste0("第 ", paste(format(waves, trim = TRUE), collapse = "、"), " 波"),
    paste0(paste(format(waves, trim = TRUE), collapse = "、"), " 個月")
  )
  ref <- d$fixed$version$reference
  rounds <- d$fixed$version$rounds
  fixed <- if (length(rounds) == 0) "沒有固定檔（第一回合）" else {
    paste0("固定檔：", d$fixed_file %||% paste0("到第 ", utils::tail(rounds, 1), " 回合"))
  }
  paste0(
    "本回合", if (length(d$round_label) == 1) paste0(" ", d$round_label), "：",
    paste(c(paste(cohorts, collapse = "、"), if (length(waves) > 0) wave_text,
            if (length(ref) == 1) paste0("參照世代 ", ref), fixed), collapse = " · ")
  )
}

#' 到 kitJoin：依本機版、雲端版給不同的東西（和 kitJoin 的 kit_kitsui_link() 同一個寫法）
#'
#' - 雲端版（kit_is_cloud()）：開新分頁連到 KITJOIN_URL，後面一行小字說明開的是雲端版。
#' - 本機版：不放連結（連結會把人帶到雲端版，資料就上傳到雲端了），
#'   改成一行說明：在 R 執行 kitJoin::run_kitjoin()。
#' @param label 連結（按鈕）的字
#' @param class 雲端版按鈕的 class（KIT_BTN）
#' @param cloud 是不是雲端版（預設 kit_is_cloud()；測試用）
#' @noRd
ui_kitjoin_button <- function(label = "開啟 kitJoin", class = KIT_BTN[["secondary"]],
                              cloud = kit_is_cloud()) {
  if (!isTRUE(cloud)) {
    return(shiny::tags$span(
      class = "kit-hint kit-kitjoin-local",
      "本機版：在 R 執行 ", shiny::tags$code("kitJoin::run_kitjoin()"),
      " 開啟 kitJoin，資料不會離開這台電腦。"
    ))
  }
  shiny::tags$span(
    class = "kit-kitjoin-link",
    shiny::tags$a(
      href = KITJOIN_URL, target = "_blank", rel = "noopener noreferrer",
      class = paste("btn kit-ext-btn", class), shiny::icon("arrow-up-right-from-square"), label
    ),
    " ",
    shiny::tags$small(class = "kit-hint", "（開啟網路上的雲端版 kitJoin）")
  )
}

#' 資料不合格式時的一句話：到 kitJoin 的「kits 作答檔」轉好再上傳
#' @noRd
ui_kitjoin_hint <- function(class = KIT_BTN[["secondary"]], hint_class = "kit-hint",
                            cloud = kit_is_cloud()) {
  shiny::tags$p(
    class = hint_class,
    "資料是寬格式（一題一欄）、欄名不一樣，或分數不是從 0 開始？",
    paste0("可以先到 kitJoin 的「", KITJOIN_KITS_TAB, "」轉好，再回來上傳。"),
    ui_kitjoin_button(class = class, cloud = cloud)
  )
}

#' 停下來的原因：中文在前；kits 的英文原文（「kits 的訊息：」後面）收在可以打開的 details 裡
#' @param detail kitsui_error_detail() 的一句
#' @noRd
kitsui_error_ui <- function(detail) {
  at <- regexpr(KITSUI_KITS_SAYS, detail, fixed = TRUE)
  if (at < 0) return(detail)
  head <- substr(detail, 1, at - 1)
  said <- substr(detail, at + nchar(KITSUI_KITS_SAYS), nchar(detail))
  shiny::tagList(head, shiny::tags$details(
    class = "d-inline kit-kits-says", shiny::tags$summary("kits 的原文"), shiny::tags$code(said)
  ))
}

#' 下一步：頁面最下面，一句小字與一個切換分頁的按鈕（和主要按鈕同一種樣式，靠右）
#' @param label 「下一步：題目校準」；後面自動加「 →」
#' @param kind 按鈕的種類（KIT_BTN）：預設是主要按鈕；這一頁另有主角時（④ 的下載）用次要按鈕
#' @noRd
ui_next_step <- function(text, button_id, label, kind = "primary") {
  shiny::tags$div(
    class = "kit-next",
    shiny::tags$p(class = "kit-hint", text),
    ui_button(button_id, paste0(label, " →"), kind)
  )
}

#' 檢查結果（kits_check()）：一排細節裡的一個收合區塊（ui_detail()）
#' @noRd
ui_check_panel <- function(id, title = "kits 的檢查") {
  ui_detail(title, shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(id)))
}

#' 畫面上的表格：小數點 2 位；整數欄不顯示小數（下載的 csv 保留完整數字）
#' @param expr 產生表格的運算式（和 shiny::renderTable() 一樣，每次重算）
#' @noRd
kit_table <- function(expr, digits = 2) {
  q <- substitute(expr)
  env <- parent.frame()
  # 人數、列次數這類計數欄加千分位（變成文字），對齊照原本的數字靠右
  last <- NULL
  shiny::renderTable({
    tab <- kit_count_marks(whole_as_integer(eval(q, env)))
    last <<- tab
    tab
  }, digits = digits, striped = TRUE, spacing = "s", na = "",
  align = \() kit_table_align(last))
}

# 計數的欄（人數、列次數、題數…）：畫面上加千分位（KIT 設計語言 5）。只換這些欄：
# 波次（2009 年）、編號這類數字不加
KIT_COUNT_COLS <- c("n", "n_visits", "n_items", "rows", "wave_n", "n_1", "n_2", "drift_n",
                    "fixed_n", "steps", "steps_judged", "n_anchors", "人數", "列次數", "題數",
                    "列數", "步驟數", "偏移（題）", "固定（題）")

#' 計數的欄（KIT_COUNT_COLS）是整數時，換成加千分位的文字
#' @noRd
kit_count_marks <- function(df) {
  if (!is.data.frame(df)) return(df)
  for (col in intersect(names(df), KIT_COUNT_COLS)) {
    x <- df[[col]]
    if (is.numeric(x) && all(is.na(x) | x == round(x))) {
      df[[col]] <- ifelse(is.na(x), NA_character_, kit_num(x))
      attr(df[[col]], "kit_count") <- TRUE
    }
  }
  df
}

#' 表格每一欄的對齊：數字與計數欄靠右，其他靠左（renderTable(align = )，第一個字是列名）
#' @noRd
kit_table_align <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(NULL)
  right <- vapply(df, \(x) is.numeric(x) || isTRUE(attr(x, "kit_count")), logical(1))
  paste(ifelse(right, "r", "l"), collapse = "")
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

#' R 程式：一排細節裡的一個收合區塊（ui_detail()），排法和 kitJoin 的 kit_code_panel() 相同
#'
#' 裡面是 server 端畫的 uiOutput（id 後面加 _ui，render_code_panel()）：有程式碼時是一句說明、
#' 版本名稱（粗體）與「複製」同一行（.kit-code-head），下面是程式碼框；沒有時只有一行字。
#' @param id 程式碼（renderText）的 id（含 namespace）
#' @noRd
ui_code_panel <- function(id, title = "用 R 怎麼做") {
  ui_detail(title, shiny::uiOutput(paste0(id, "_ui")))
}

# 程式碼的版本名稱（kitJoin 是「tidyverse 版」「base R 版」；kitsUI 只有一個版本）
KIT_CODE_LABEL <- "kits 版"
# 還沒有程式碼時的一行字
KIT_CODE_EMPTY <- "先選資料，這裡才會有程式碼。"

#' 有程式碼時的內容：一句說明、版本名稱與「複製」（同一行）、程式碼框
#'
#' 「複製」用 navigator.clipboard 複製下面的程式碼，按了顯示「已複製」1.5 秒（和 kitJoin 相同）。
#' @noRd
ui_code_block <- function(id, label = KIT_CODE_LABEL) {
  copy <- paste0(
    "var b=this;navigator.clipboard.writeText(",
    "b.parentElement.nextElementSibling.innerText).then(function(){",
    "b.textContent='已複製';setTimeout(function(){b.textContent='複製';},1500);});"
  )
  shiny::tagList(
    shiny::tags$p(class = "kit-hint", "以下程式碼依你目前的設定產生，是簡化的示範。"),
    shiny::tags$div(
      class = "kit-code-head",
      shiny::tags$strong(label),
      shiny::tags$button(type = "button", class = paste("btn", KIT_BTN[["secondary"]]),
                         onclick = copy, "複製")
    ),
    shiny::verbatimTextOutput(id)
  )
}

#' 放在 ui_code_panel() 的位置：output[[paste0(id, "_ui")]] <- render_code_panel(session, id, code)
#'
#' 打開「用 R 怎麼做」時才算（收起來時不畫）。code() 停下來（req）或出錯時只有一行 empty。
#' @param code 傳回程式碼（字串）的函式或 reactive
#' @noRd
render_code_panel <- function(session, id, code, empty = KIT_CODE_EMPTY) {
  shiny::renderUI({
    have <- tryCatch({
      x <- code()
      length(x) > 0 && any(nzchar(x))
    }, error = \(e) FALSE)
    if (have) ui_code_block(session$ns(id)) else shiny::tags$p(class = "kit-hint mb-0", empty)
  })
}

#' 下載鈕的位置：一個 uiOutput（id 後面加 _ui）
#'
#' 按鈕在 server 端用 render_download_csv()、render_download_plot() 畫（R/download.R 的
#' kit_download_button()：網址最後一段、檔名標頭、download 屬性都是同一個檔名）。
#' @param id 按鈕的 id（含 namespace）
#' @noRd
ui_download <- function(id) shiny::uiOutput(paste0(id, "_ui"), inline = TRUE)

# 下載鈕的樣式：次要按鈕
KIT_DL_CLASS <- KIT_BTN[["secondary"]]

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
download_csv_ui <- function(session, id, label, table_fun, name, class = KIT_DL_CLASS) {
  write <- \(file) readr::write_excel_csv(csv_or_note(table_fun), file)
  download_button_ui(session, id, label, name, "csv", write, class = class)
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
render_download_csv <- function(session, id, label, table_fun, name, class = KIT_DL_CLASS) {
  shiny::renderUI(download_csv_ui(session, id, label, table_fun, name, class = class))
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
    shiny::tags$button(type = "button", class = paste("btn", KIT_BTN[["secondary"]]),
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

# 畫面上 kitsUI 自己整理的摘要表用中文欄名（KIT 設計語言 3.8）。
# kits 函式直接算出來的表（coef(item)、kits_check()、適配度、kits_subj_table()、偏移表、連結係數、
# 分數表與題目表）維持英文欄名，方便和 kits 的說明與 R 的輸出對照；下載的 CSV 一律維持原欄名。
KITSUI_SCREEN_NAMES <- c(
  scale = "量尺", n_visits = "列次數", n_items = "題數", waves = "波次", calibration = "校準方式",
  data = "資料", drift_n = "偏移（題）", fixed_n = "固定（題）", item_id = "題號", status = "狀態",
  steps = "步驟數", detail = "說明", bank = "題庫", route = "換算路線", final_scale = "最後的量尺",
  n = "列次數", note = "說明", from = "從", to = "到", state = "狀態", link = "連結", kind = "種類",
  made = "什麼時候做的", round = "回合"
)

#' 摘要表的欄名換成中文（只換 KITSUI_SCREEN_NAMES 有的欄；只用在畫面上）
#' @noRd
kitsui_screen_names <- function(df) {
  if (!is.data.frame(df)) return(df)
  hit <- intersect(names(df), names(KITSUI_SCREEN_NAMES))
  names(df)[match(hit, names(df))] <- KITSUI_SCREEN_NAMES[hit]
  df
}
