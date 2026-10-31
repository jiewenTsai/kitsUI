# 各分頁共用、kitsUI 自己的小元件：檢查結果、表格、下載鈕、到 kitJoin
# （說明框、提示框、狀態框、按鈕、細節列、用 R 怎麼做都用 kitDesign 的 kit_*()）

#' 找不到 pandoc 時，頁面最上方的提醒
#' @noRd
kit_pandoc_notice <- function() {
  kitDesign::kit_callout(KIT_PANDOC_MISSING, warn = TRUE, class = "kit-pandoc-notice",
                         role = "alert")
}

#' 還沒有這一回合的題目校準：提醒，並連到 ②（按了切換分頁的是 input$go_items）
#' @noRd
ui_go_items <- function(ns) {
  kitDesign::kit_callout(
    "還沒有這一回合的題目校準。請先到 ",
    shiny::actionLink(ns("go_items"), "「② 題目校準」"), " 跑這一回合。",
    warn = TRUE
  )
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
#' - 雲端版（kitDesign::kit_is_cloud()）：開新分頁連到 KITJOIN_URL，後面一行小字說明開的是雲端版。
#' - 本機版：不放連結（連結會把人帶到雲端版，資料就上傳到雲端了），
#'   改成一行說明：在 R 執行 kitJoin::run_kitjoin()。
#' @param label 連結（按鈕）的字
#' @param class 雲端版按鈕的 class（kitDesign::kit_btn_class()）
#' @param cloud 是不是雲端版（預設 kitDesign::kit_is_cloud()；測試用）
#' @noRd
ui_kitjoin_button <- function(label = "開啟 kitJoin", class = kitDesign::kit_btn_class(),
                              cloud = kitDesign::kit_is_cloud()) {
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
ui_kitjoin_hint <- function(class = kitDesign::kit_btn_class(), hint_class = "kit-hint",
                            cloud = kitDesign::kit_is_cloud()) {
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

#' 檢查結果（kits_check()）：一排細節裡的一個收合區塊（kitDesign::kit_detail()）
#' @noRd
ui_check_panel <- function(id, title = "kits 的檢查") {
  kitDesign::kit_detail(title, shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(id)))
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
      df[[col]] <- ifelse(is.na(x), NA_character_, kitDesign::kit_num(x))
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

#' 下載鈕的位置：一個 uiOutput（id 後面加 _ui）
#'
#' 按鈕在 server 端用 render_download_csv()、render_download_plot() 畫（kitDesign 的
#' kit_download_button()：網址最後一段、檔名標頭、download 屬性都是同一個檔名）。
#' @param id 按鈕的 id（含 namespace）
#' @noRd
ui_download <- function(id) shiny::uiOutput(paste0(id, "_ui"), inline = TRUE)

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
download_csv_ui <- function(session, id, label, table_fun, name,
                            class = kitDesign::kit_btn_class()) {
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
render_download_csv <- function(session, id, label, table_fun, name,
                                class = kitDesign::kit_btn_class()) {
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
download_button_ui <- function(session, id, label, name, ext, content,
                               class = kitDesign::kit_btn_class()) {
  base <- tryCatch(download_name(name), error = \(e) character())
  if (length(base) != 1 || !nzchar(base)) return(NULL)
  kitDesign::kit_download_button(session, id, label, paste0(base, ".", ext), content, class = class)
}

#' 不能下載時：不能按的鈕與一句說明（例如找不到 pandoc）
#' @noRd
ui_download_off <- function(label, why) {
  shiny::tags$span(
    class = "kit-download-off",
    shiny::tags$button(type = "button", class = paste("btn", kitDesign::kit_btn_class()),
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
    kitDesign::kit_abort(text)
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

#' 按了主要按鈕之後：跳一個短通知、捲到狀態框（設計語言的「狀態流」，和 kitJoin 一樣）
#' @noRd
kitsui_after_run <- function(state, message) {
  kitDesign::kit_notify(kitDesign::kit_status(state, message))
  kitDesign::kit_scroll_to("status")
}
