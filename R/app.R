# 組裝整個 app：雲端版（根目錄 app.R）、本機 run.bat、套件 run_kitsui() 共用

#' 建立 kitsUI Shiny app 物件
#'
#' 三種啟動方式都呼叫這個函式，所以看到的畫面一致；
#' 雲端版會在上方與頁尾多顯示雲端使用須知。
#'
#' @return [shiny::shinyApp()] 物件
#' @export
kit_app <- function() {
  kit_set_options()
  shiny::shinyApp(ui = kit_ui, server = kit_server)
}

#' 啟動前的 Shiny 設定（上傳上限）；找 pandoc（圖檔下載要用）
#' @noRd
kit_set_options <- function() {
  options(
    shiny.maxRequestSize = KIT_UPLOAD_MB * 1024^2,
    shiny.autoload.r = FALSE
  )
  kit_setup_pandoc()
  kit_setup_manual()
}

#' 在預設瀏覽器開啟網址
#' @noRd
kit_open_browser <- function(url) {
  if (.Platform$OS.type == "windows") {
    utils::browseURL(url, browser = NULL)
  } else {
    utils::browseURL(url)
  }
  invisible(NULL)
}

#' 頁面（每次連線時產生，雲端判斷才會反映目前環境）
#' @noRd
kit_ui <- function(request) {
  bslib::page_fluid(
    lang = "zh-Hant-TW",
    theme = kit_theme(),
    title = "KIT 量尺建構工具",
    shiny::tags$head(shiny::tags$style(kit_css()), shiny::tags$script(kit_js()),
                     shiny::tags$script(kit_tabs_js())),
    kit_header(),
    if (kit_is_cloud()) kit_cloud_banner(),
    if (!kit_pandoc_ok()) kit_pandoc_notice(),
    shiny::tabsetPanel(
      id = "tabs",
      shiny::tabPanel("① 資料", value = "data", mod_data_ui("data")),
      shiny::tabPanel("② 題目校準", value = "items", mod_items_ui("items")),
      shiny::tabPanel("③ 連結", value = "link", mod_link_ui("link")),
      shiny::tabPanel("④ 受試者評分", value = "score", mod_score_ui("score")),
      shiny::tabPanel("使用手冊", value = "manual", kit_manual_ui())
    ),
    if (kit_is_cloud()) kit_cloud_footer()
  )
}

#' 伺服器邏輯：各分頁模組之間只透過 reactive 傳資料
#' @noRd
kit_server <- function(input, output, session) {
  goto <- function(tab) {
    shiny::updateTabsetPanel(session, "tabs", selected = tab)
  }
  # ④ 交給 ① 的固定檔（這次連線接著做下一回合）；n 讓同一個固定檔再交一次也算數
  handover <- shiny::reactiveVal(NULL)
  next_round <- function(fixed, round) {
    n <- (shiny::isolate(handover())$n %||% 0) + 1
    handover(list(fixed = fixed, round = round, n = n))
    goto("data")
  }
  data <- mod_data_server("data", goto, handover)
  items <- mod_items_server("items", data, goto)
  links <- mod_link_server("link", data, items, goto)
  score <- mod_score_server("score", data, items, links, goto, next_round)
  # 有這一回合的結果時，離開頁面前先問一次（kit_js()）
  shiny::observe({
    session$sendCustomMessage("kit_unload_guard", !is.null(items()))
  })
  # 步驟分頁的進度：做完的打 ✓，還不能做的灰色（kit_tabs.R）
  tab_states <- shiny::reactive({
    ok <- \(f) tryCatch(f(), error = \(e) NULL)
    lk <- ok(links)
    kit_tab_states(!is.null(ok(data)), ok(items), !is.null(lk) && length(lk$todo) == 0,
                   ok(score))
  })
  shiny::observe(session$sendCustomMessage("kit_tab_state", kit_tab_message(tab_states())))
}
