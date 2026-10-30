# 步驟分頁的進度（KIT 設計語言 3.2、4.2）：這一回合做完的步驟打 ✓；還不能做的步驟灰色，
# 滑過時說明原因（仍然可以按，進去會看到怎麼辦）。結果過期時（資料改了、重新校準）✓ 拿掉。
# 狀態在伺服器算（kit_tab_states()），用 kit_tab_state 訊息送到瀏覽器（kit_tabs_js()）。

# 步驟分頁（tabsetPanel 的 value）
KIT_STEP_TABS <- c("data", "items", "link", "score")

#' 每個步驟分頁的狀態
#'
#' - ① 資料：資料可以用時做完。
#' - ② 題目校準：還沒有資料時不能做；這一回合校準完、沒有量尺停下來時做完。
#' - ③ 連結：還沒有校準時不能做；能建立的連結都建立了（或這一回合不用做連結）時做完。
#' - ④ 受試者評分：還沒有校準時不能做；算好這一回合的分數時做完。
#' @param data_ok ① 的資料可以用嗎（mod_data_server() 不是 NULL）
#' @param items ② 的結果（kitsui_round()）或 NULL（還沒校準，或資料改了、結果清掉了）
#' @param link_done ③ 能建立的連結都建立了嗎
#' @param score ④ 的結果（kitsui_score()）或 NULL
#' @return data.frame：tab、state（"done"、"ready"、"locked"）、why（不能做的原因，其他是 ""）
#' @noRd
kit_tab_states <- function(data_ok, items = NULL, link_done = FALSE, score = NULL) {
  no_items <- is.null(items)
  state <- c(
    data = if (isTRUE(data_ok)) "done" else "ready",
    items = if (!isTRUE(data_ok)) "locked" else if (isTRUE(items$ok)) "done" else "ready",
    link = if (no_items) "locked" else if (isTRUE(link_done)) "done" else "ready",
    score = if (no_items) "locked" else if (!is.null(score)) "done" else "ready"
  )
  why <- c(data = "", items = "先在「① 資料」選好這一回合的資料",
           link = "先做「② 題目校準」", score = "先做「② 題目校準」")
  why[state != "locked"] <- ""
  data.frame(tab = KIT_STEP_TABS, state = unname(state[KIT_STEP_TABS]),
             why = unname(why[KIT_STEP_TABS]))
}

#' 送給瀏覽器的訊息：每個分頁一個 list(tab, state, why)
#' @noRd
kit_tab_message <- function(states) {
  lapply(seq_len(nrow(states)), \(i) as.list(states[i, ]))
}

#' 步驟分頁的進度與收合區塊（kit_tabs_js()）
#'
#' - kit_tab_state：在 #tabs 的分頁上加 .kit-tab-done（多一個 ✓）或 .kit-tab-locked（灰色，
#'   title 是原因）。
#' - 收合區塊（<details>）打開時，通知 Shiny 裡面的輸出看得到了（才會畫），並讓圖重新量大小。
#' @noRd
kit_tabs_js <- function() {
  shiny::HTML("
    (function() {
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('kit_tab_state', function(tabs) {
          tabs.forEach(function(t) {
            var a = document.querySelector('#tabs a[data-value=\"' + t.tab + '\"]');
            if (!a) return;
            a.classList.toggle('kit-tab-done', t.state === 'done');
            a.classList.toggle('kit-tab-locked', t.state === 'locked');
            var mark = a.querySelector('.kit-tab-mark');
            if (t.state === 'done' && !mark) {
              mark = document.createElement('span');
              mark.className = 'kit-tab-mark';
              mark.setAttribute('aria-label', '做完了');
              mark.textContent = '✓';
              a.appendChild(mark);
            }
            if (t.state !== 'done' && mark) mark.remove();
            if (t.why) {
              a.setAttribute('title', t.why);
              a.setAttribute('aria-description', t.why);
            } else {
              a.removeAttribute('title');
              a.removeAttribute('aria-description');
            }
          });
        });
      });
      document.addEventListener('toggle', function(e) {
        var d = e.target;
        if (!d || d.tagName !== 'DETAILS' || !d.open) return;
        $(d).trigger('shown');
        window.dispatchEvent(new Event('resize'));
      }, true);
    })();
  ")
}
