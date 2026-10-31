# 步驟分頁的進度（KIT 設計語言 3.2、4.2）：這一回合做完的步驟打 ✓；還不能做的步驟灰色，
# 滑過時說明原因（仍然可以按，進去會看到怎麼辦）。結果過期時（資料改了、重新校準）✓ 拿掉。
# 規則是 kitsUI 自己的（kitsui_tab_states()）；送到瀏覽器交給 kitDesign 的
# kit_tab_states() 與 kit_watch_tab_states()（kitsui_tab_send()）。

# 步驟分頁（tabsetPanel 的 value）
KIT_STEP_TABS <- c("data", "items", "link", "score")

#' 每個步驟分頁的狀態
#'
#' - ① 資料：資料可以用時做完。
#' - ② 題目校準：還沒有資料時不能做；這一回合校準完、沒有量尺停下來時做完。
#' - ③ 連結：還沒有校準時不能做；② 做完（沒有量尺停下來），而且能建立的連結都建立了（或這一回合
#'   不用做連結）時做完。② 有量尺停下來時可以先看，但不打 ✓（② 都還沒有 ✓）。
#' - ④ 受試者評分：還沒有校準、或沒有一個量尺校準好時不能做；算好這一回合的分數時做完。
#' @param data_ok ① 的資料可以用嗎（mod_data_server() 不是 NULL）
#' @param items ② 的結果（kitsui_round()）或 NULL（還沒校準，或資料改了、結果清掉了）
#' @param link_done ③ 能建立的連結都建立了嗎
#' @param score ④ 的結果（kitsui_score()）或 NULL
#' @return data.frame：tab、state（"done"、"ready"、"locked"）、why（不能做的原因，其他是 ""）
#' @noRd
kitsui_tab_states <- function(data_ok, items = NULL, link_done = FALSE, score = NULL) {
  no_items <- is.null(items)
  items_done <- isTRUE(items$ok)
  # ② 的結果裡沒有一個量尺校準好（全部停下來）：④ 沒有東西可以評分
  none_ok <- !no_items && is.list(items$items) && length(items$items) == 0
  state <- c(
    data = if (isTRUE(data_ok)) "done" else "ready",
    items = if (!isTRUE(data_ok)) "locked" else if (items_done) "done" else "ready",
    link = if (no_items) "locked" else if (items_done && isTRUE(link_done)) "done" else "ready",
    score = if (no_items || none_ok) "locked" else if (!is.null(score)) "done" else "ready"
  )
  why <- c(data = "", items = "先在「① 資料」選好這一回合的資料",
           link = "先做「② 題目校準」",
           score = if (none_ok) "② 沒有一個量尺校準好，先處理「② 題目校準」停下來的量尺" else {
             "先做「② 題目校準」"
           })
  why[state != "locked"] <- ""
  data.frame(tab = KIT_STEP_TABS, state = unname(state[KIT_STEP_TABS]),
             why = unname(why[KIT_STEP_TABS]))
}

#' 送給瀏覽器的分頁狀態（kitDesign::kit_tab_states()）：做完 ✓、還不能做（滑過時是原因）
#' @param states kitsui_tab_states()
#' @noRd
kitsui_tab_send <- function(states) {
  locked <- states$state == "locked"
  kitDesign::kit_tab_states(
    stats::setNames(states$state, states$tab),
    locked = states$tab[locked],
    locked_tips = stats::setNames(states$why, states$tab)[locked],
    tabset = "tabs"
  )
}
