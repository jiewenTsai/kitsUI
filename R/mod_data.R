# ① 資料：示範資料或上傳、這一回合的世代與波次、上一回合的固定檔、這一回合的名稱

#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：選這一回合要校準的資料。第二回合起，再放上一回合下載的固定檔。
      goal = "\u9078\u9019\u4e00\u56de\u5408\u8981\u6821\u6e96\u7684\u8cc7\u6599\u3002\u7b2c\u4e8c\u56de\u5408\u8d77\uff0c\u518d\u653e\u4e0a\u4e00\u56de\u5408\u4e0b\u8f09\u7684\u56fa\u5b9a\u6a94\u3002",
      steps = c(
        # 原文：選資料來源：示範資料，或上傳自己的長格式資料（可以一次選好幾個檔案）。
        "\u9078\u8cc7\u6599\u4f86\u6e90\uff1a\u793a\u7bc4\u8cc7\u6599\uff0c\u6216\u4e0a\u50b3\u81ea\u5df1\u7684\u9577\u683c\u5f0f\u8cc7\u6599\uff08\u53ef\u4ee5\u4e00\u6b21\u9078\u597d\u5e7e\u500b\u6a94\u6848\uff09\u3002",
        # 原文：勾這一回合的世代與波次（例如月齡）。一個檔案可以有好幾個波次。新收一波時只勾那一波；已經收完的好幾波可以一次勾。
        "\u52fe\u9019\u4e00\u56de\u5408\u7684\u4e16\u4ee3\u8207\u6ce2\u6b21\uff08\u4f8b\u5982\u6708\u9f61\uff09\u3002\u4e00\u500b\u6a94\u6848\u53ef\u4ee5\u6709\u597d\u5e7e\u500b\u6ce2\u6b21\u3002\u65b0\u6536\u4e00\u6ce2\u6642\u53ea\u52fe\u90a3\u4e00\u6ce2\uff1b\u5df2\u7d93\u6536\u5b8c\u7684\u597d\u5e7e\u6ce2\u53ef\u4ee5\u4e00\u6b21\u52fe\u3002",
        # 原文：第二回合起：上傳上一回合的固定檔（zip）。第一回合不用。
        "\u7b2c\u4e8c\u56de\u5408\u8d77\uff1a\u4e0a\u50b3\u4e0a\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\uff08zip\uff09\u3002\u7b2c\u4e00\u56de\u5408\u4e0d\u7528\u3002",
        # 原文：看右邊：沒有【停下來】；【完成，但請注意】時先看過裡面的每一點。量尺表確認每個量尺的列次數、題數與校準方式。
        "\u770b\u53f3\u908a\uff1a\u6c92\u6709\u3010\u505c\u4e0b\u4f86\u3011\uff1b\u3010\u5b8c\u6210\uff0c\u4f46\u8acb\u6ce8\u610f\u3011\u6642\u5148\u770b\u904e\u88e1\u9762\u7684\u6bcf\u4e00\u9ede\u3002\u91cf\u5c3a\u8868\u78ba\u8a8d\u6bcf\u500b\u91cf\u5c3a\u7684\u5217\u6b21\u6578\u3001\u984c\u6578\u8207\u6821\u6e96\u65b9\u5f0f\u3002",
        # 原文：按最下面的「下一步：題目校準」。
        "\u6309\u6700\u4e0b\u9762\u7684\u300c\u4e0b\u4e00\u6b65\uff1a\u984c\u76ee\u6821\u6e96\u300d\u3002"
      ),
      # 原文：這一回合的資料。還沒有校準；校準在 ② 題目校準。
      output = "\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u3002\u9084\u6c92\u6709\u6821\u6e96\uff1b\u6821\u6e96\u5728 \u2461 \u984c\u76ee\u6821\u6e96\u3002"
    ),
    data_new_wave_guide(),
    bslib::layout_columns(
      col_widths = c(4, 8),
      data_choices_ui(ns),
      # 右欄：設定摘要、狀態、要注意、量尺表（重點）；固定檔、作答設計、預覽、檢查、程式收起來
      shiny::tagList(
        shiny::uiOutput(ns("settings")),
        shiny::uiOutput(ns("problems")),
        shiny::uiOutput(ns("hint")),
        shiny::uiOutput(ns("data_status")),
        shiny::uiOutput(ns("notes")),
        shiny::uiOutput(ns("categories")),
        # 原文：量尺
        shiny::h4("\u91cf\u5c3a"),
        shiny::tags$p(class = "kit-hint",
                      # 原文：每個量尺分開校準。
                      shiny::textOutput(ns("scale_example"), inline = TRUE), "\u6bcf\u500b\u91cf\u5c3a\u5206\u958b\u6821\u6e96\u3002"),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("scales"))),
        shiny::uiOutput(ns("borrowed")),
        kitDesign::kit_details_row(
          # 原文：固定檔的內容
          kitDesign::kit_detail("\u56fa\u5b9a\u6a94\u7684\u5167\u5bb9", shiny::uiOutput(ns("fixed_info"))),
          kitDesign::kit_detail(
            # 原文：作答設計
            "\u4f5c\u7b54\u8a2d\u8a08",
            shiny::tags$p(class = "kit-hint",
                          # 原文：每格是一個波次、一題的通過比例（得分達到通過步驟以上；預設 1 分）。
                          "\u6bcf\u683c\u662f\u4e00\u500b\u6ce2\u6b21\u3001\u4e00\u984c\u7684\u901a\u904e\u6bd4\u4f8b\uff08\u5f97\u5206\u9054\u5230\u901a\u904e\u6b65\u9a5f\u4ee5\u4e0a\uff1b\u9810\u8a2d 1 \u5206\uff09\u3002",
                          # 原文：空白表示沒有施測。
                          "\u7a7a\u767d\u8868\u793a\u6c92\u6709\u65bd\u6e2c\u3002"),
            shiny::uiOutput(ns("design_steps")),
            shiny::uiOutput(ns("design_pick")),
            plotly::plotlyOutput(ns("design"), height = "420px"),
            shiny::uiOutput(ns("design_dl"))
          ),
          kitDesign::kit_detail(
            # 原文：預覽
            "\u9810\u89bd",
            shiny::textOutput(ns("preview_meta")),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview"))),
            shiny::tags$div(class = "kit-downloads", ui_download(ns("dl_data")))
          ),
          # 原文：kits 的檢查（作答設計選的量尺）
          ui_check_panel(ns("check"), "kits \u7684\u6aa2\u67e5\uff08\u4f5c\u7b54\u8a2d\u8a08\u9078\u7684\u91cf\u5c3a\uff09"),
          kitDesign::kit_code_panel(ns("code"))
        ),
        shiny::uiOutput(ns("next_step"))
      )
    )
  )
}

# 左欄：資料來源、世代與波次、固定檔；這一回合的名稱在進階設定
data_choices_ui <- function(ns) {
  shiny::tagList(
    # 原文：1. 資料來源
    shiny::h5("1. \u8cc7\u6599\u4f86\u6e90"),
    # 原文：示範資料 | 上傳檔案
    shiny::radioButtons(ns("source"), NULL, c("\u793a\u7bc4\u8cc7\u6599" = "demo", "\u4e0a\u50b3\u6a94\u6848" = "upload")),
    shiny::conditionalPanel(
      "input.source == 'demo'", ns = ns,
      # 原文：領域 | 語言 lan | 動作 motor
      shiny::checkboxGroupInput(ns("domains"), "\u9818\u57df", c("\u8a9e\u8a00 lan" = "lan", "\u52d5\u4f5c motor" = "motor"),
                                "lan"),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：示範資料（KIT 的 M03、M36）第一回合建議：M03 只勾 3 個月。
        "\u793a\u7bc4\u8cc7\u6599\uff08KIT \u7684 M03\u3001M36\uff09\u7b2c\u4e00\u56de\u5408\u5efa\u8b70\uff1aM03 \u53ea\u52fe 3 \u500b\u6708\u3002",
        # 原文：M36 的第一回合勾 36 與 48 個月。
        "M36 \u7684\u7b2c\u4e00\u56de\u5408\u52fe 36 \u8207 48 \u500b\u6708\u3002",
        # 原文：之後每一回合加下一個波次。
        "\u4e4b\u5f8c\u6bcf\u4e00\u56de\u5408\u52a0\u4e0b\u4e00\u500b\u6ce2\u6b21\u3002"
      )
    ),
    shiny::conditionalPanel(
      "input.source == 'upload'", ns = ns,
      # 原文：資料檔（CSV、CSV.GZ 或 SAV，可以一次選好幾個）
      shiny::fileInput(ns("file"), "\u8cc7\u6599\u6a94\uff08CSV\u3001CSV.GZ \u6216 SAV\uff0c\u53ef\u4ee5\u4e00\u6b21\u9078\u597d\u5e7e\u500b\uff09",
                       multiple = TRUE, accept = c(".csv", ".gz", ".csv.gz", ".sav", ".zsav"),
                       # 原文：選擇檔案… | 尚未選擇檔案
                       buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026", placeholder = "\u5c1a\u672a\u9078\u64c7\u6a94\u6848"),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：長格式：一列是一個列次（一個孩子的一個波次）的一題作答。
        "\u9577\u683c\u5f0f\uff1a\u4e00\u5217\u662f\u4e00\u500b\u5217\u6b21\uff08\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\uff09\u7684\u4e00\u984c\u4f5c\u7b54\u3002",
        # 原文：大的 CSV 可以先壓縮成 .csv.gz 再上傳。
        "\u5927\u7684 CSV \u53ef\u4ee5\u5148\u58d3\u7e2e\u6210 .csv.gz \u518d\u4e0a\u50b3\u3002",
        # 原文：好幾個檔案時，每個檔案的欄位要一樣。
        "\u597d\u5e7e\u500b\u6a94\u6848\u6642\uff0c\u6bcf\u500b\u6a94\u6848\u7684\u6b04\u4f4d\u8981\u4e00\u6a23\u3002"
      ),
      ui_kitjoin_hint(),
      shiny::uiOutput(ns("upload_info")),
      data_columns_guide(),
      shiny::tags$div(class = "kit-downloads",
                      ui_download(ns("dl_example")))
    ),
    # 原文：2. 這一回合的世代與波次
    shiny::h5("2. \u9019\u4e00\u56de\u5408\u7684\u4e16\u4ee3\u8207\u6ce2\u6b21"),
    shiny::uiOutput(ns("pick_ui")),
    shiny::tags$div(
      class = "d-flex gap-2 mb-1",
      # 原文：全選
      kitDesign::kit_button(ns("waves_all"), "\u5168\u9078"),
      # 原文：全不選
      kitDesign::kit_button(ns("waves_none"), "\u5168\u4e0d\u9078")
    ),
    shiny::tags$small(class = "kit-hint", shiny::textOutput(ns("wave_hint"), inline = TRUE),
                      # 原文：可以一次勾好幾個。 | 勾完停 1 秒才開始檢查。
                      "\u53ef\u4ee5\u4e00\u6b21\u52fe\u597d\u5e7e\u500b\u3002", "\u52fe\u5b8c\u505c 1 \u79d2\u624d\u958b\u59cb\u6aa2\u67e5\u3002"),
    shiny::uiOutput(ns("unit_ui")),
    # 參照世代：第一回合選（寫進固定檔）；之後的回合讀固定檔的，只顯示
    shiny::uiOutput(ns("reference_ui")),
    # 原文：3. 上一回合的固定檔（選用）
    shiny::h5("3. \u4e0a\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\uff08\u9078\u7528\uff09"),
    shiny::uiOutput(ns("fixed_choice")),
    shiny::uiOutput(ns("fixed_error")),
    shiny::tags$small(
      class = "kit-hint d-block",
      # 原文：固定檔是上一回合在 ④ 下載的 zip。
      "\u56fa\u5b9a\u6a94\u662f\u4e0a\u4e00\u56de\u5408\u5728 \u2463 \u4e0b\u8f09\u7684 zip\u3002",
      # 原文：有它，舊題的參數就固定不動。
      "\u6709\u5b83\uff0c\u820a\u984c\u7684\u53c3\u6578\u5c31\u56fa\u5b9a\u4e0d\u52d5\u3002"
    ),
    kitDesign::kit_advanced(
      # 原文：這一回合的名稱
      shiny::textInput(ns("round_label"), "\u9019\u4e00\u56de\u5408\u7684\u540d\u7a31", ""),
      shiny::tags$small(
        class = "kit-hint",
        # 原文：預設是 R1、R2…（第幾回合）。
        "\u9810\u8a2d\u662f R1\u3001R2\u2026\uff08\u7b2c\u5e7e\u56de\u5408\uff09\u3002",
        # 原文：只能用英文字母與數字。
        "\u53ea\u80fd\u7528\u82f1\u6587\u5b57\u6bcd\u8207\u6578\u5b57\u3002",
        # 原文：偏移的題會改名，加上「_r名稱」。
        "\u504f\u79fb\u7684\u984c\u6703\u6539\u540d\uff0c\u52a0\u4e0a\u300c_r\u540d\u7a31\u300d\u3002"
      ),
      # 原文：進階設定（這一回合的名稱）
      title = "\u9032\u968e\u8a2d\u5b9a\uff08\u9019\u4e00\u56de\u5408\u7684\u540d\u7a31\uff09"
    )
  )
}

#' 世代 × 波次的勾選框：一個世代一列（值是 "M03 3"）
#'
#' 用 Shiny 的 checkboxGroupInput 的 binding（找 name 是 id 的勾選框），全選、全不選可以用
#' updateCheckboxGroupInput(selected = )。
#' @param choices kitsui_pick_choices()
#' @noRd
data_pick_input <- function(id, choices, selected = character()) {
  rows <- split(choices, factor(choices$cohort, unique(choices$cohort))) |>
    purrr::map(\(ch) {
      boxes <- purrr::map2(ch$value, ch$wave_m, \(value, wave) shiny::tags$label(
        class = "kit-pick",
        shiny::tags$input(type = "checkbox", name = id, value = value,
                          checked = if (value %in% selected) NA),
        shiny::tags$span(format(wave))
      ))
      shiny::tags$div(class = "kit-pick-row", role = "group",
                      shiny::tags$span(class = "kit-pick-cohort", ch$cohort[1]),
                      shiny::tags$div(class = "kit-pick-boxes", boxes))
    })
  shiny::tags$div(
    id = id, class = "form-group shiny-input-checkboxgroup shiny-input-container kit-picks",
    shiny::tags$div(class = "shiny-options-group", unname(rows))
  )
}

#' @param goto function(tab) 切換分頁
#' @param handover NULL 或 reactive：④ 交過來的 list(fixed, round)（這次連線上一回合的固定檔）
#' @return reactive：資料沒問題時是 list(df, fixed, round_label, files, source, data_file,
#'   fixed_file, waves, picks, scales, plan, persons)；有問題或還沒勾時是 NULL。
#'   files 與 data_file 可以有好幾個檔案；waves 是勾到的月齡，picks 是勾到的世代 × 波次；
#'   persons 是 kitsui_persons(df)（算一次，給其他分頁用）
#' @noRd
mod_data_server <- function(id, goto, handover = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fixed_rv <- shiny::reactiveVal(NULL)
    # 固定檔來自這次連線的哪一回合（④ 交過來時）；NULL 時用上傳的 zip
    from_session <- shiny::reactiveVal(NULL)
    # 這次連線最新的一回合（④ 交過來的）；讀不進來的固定檔的訊息
    latest_round <- shiny::reactiveVal(NULL)
    fixed_error <- shiny::reactiveVal(NULL)

    # 上傳的資料（一個或好幾個檔案接起來；讀不了時是錯誤訊息）
    upload <- shiny::reactive({
      f <- input$file
      # 原文：請選擇資料檔（CSV、CSV.GZ 或 SAV）。
      if (is.null(f)) return(list(error = "\u8acb\u9078\u64c7\u8cc7\u6599\u6a94\uff08CSV\u3001CSV.GZ \u6216 SAV\uff09\u3002"))
      # 原文：讀取資料檔…
      up <- shiny::withProgress(message = "\u8b80\u53d6\u8cc7\u6599\u6a94\u2026", value = 0, {
        step <- \(i, n, name) {
          shiny::setProgress(value = (i - 1) / n,
                             # 原文：第 %d 個，共 %d 個：%s
                             detail = sprintf("\u7b2c %d \u500b\uff0c\u5171 %d \u500b\uff1a%s", i, n, name))
        }
        res <- tryCatch(kitsui_read_uploads(f, progress = step),
                        # 原文：讀取資料檔
                        error = \(e) list(error = kit_error_text(e, "\u8b80\u53d6\u8cc7\u6599\u6a94")))
        # 原文：讀好了
        shiny::setProgress(value = 1, detail = "\u8b80\u597d\u4e86")
        res
      })
      if (!is.null(up$error)) return(up)
      c(up, list(name = f$name, path = f$datapath))
    })
    output$upload_info <- shiny::renderUI({
      up <- upload()
      if (is.null(up$files)) return(NULL)
      # 匯入的結果是一個動作的結果：薄荷的【完成】（和 kitJoin 的上傳相同）
      kitDesign::kit_status_ui(kitDesign::kit_status(
        "done",
        # 原文：匯入完成：讀進 %s 個檔案，共 %s 列。
        sprintf("\u532f\u5165\u5b8c\u6210\uff1a\u8b80\u9032 %s \u500b\u6a94\u6848\uff0c\u5171 %s \u5217\u3002", kitDesign::kit_num(nrow(up$files)),
                kitDesign::kit_num(sum(up$files$rows))),
        # 原文：%s（%s 列）
        points = sprintf("%s\uff08%s \u5217\uff09", up$files$file, kitDesign::kit_num(up$files$rows))
      ))
    })
    output$dl_example_ui <- render_download_csv(
      # 原文：下載欄位範例檔（CSV）
      session, "dl_example", "\u4e0b\u8f09\u6b04\u4f4d\u7bc4\u4f8b\u6a94\uff08CSV\uff09", kitsui_example_data, "kitsui_example"
    )
    is_upload <- shiny::reactive(identical(input$source, "upload"))

    # 這個來源的全部資料（還沒選世代與波次）；示範資料是兩個世代、勾選的領域
    source_df <- shiny::reactive({
      if (is_upload()) {
        up <- upload()
        if (!is.null(up$error)) return(up)
        return(list(data = up$data))
      }
      # 原文：請至少勾選一個領域。
      if (length(input$domains) == 0) return(list(error = "\u8acb\u81f3\u5c11\u52fe\u9078\u4e00\u500b\u9818\u57df\u3002"))
      list(data = kitsui_demo(c("M03", "M36"), NULL, input$domains))
    })
    # 參照世代：有固定檔時用固定檔的（不能改）；第一回合用 ① 選的（預設 M03 或第一個世代）
    cohorts <- shiny::reactive(sort(unique(stats::na.omit(source_df()$data$cohort))))
    reference <- shiny::reactive({
      fx <- fixed_rv()$fixed
      if (!is.null(fx)) return(fx$version$reference)
      kitsui_ref_pick(input$reference, cohorts())
    })
    output$reference_ui <- shiny::renderUI({
      data_reference_ui(ns, cohorts(), fixed_rv()$fixed, shiny::isolate(input$reference))
    })
    choices <- shiny::reactive(kitsui_pick_choices(source_df()$data %||% data.frame()))

    # 世代 × 波次的勾選框跟著資料來源變；還勾著的留著。預設都不勾
    output$pick_ui <- shiny::renderUI({
      ch <- choices()
      # 原文：先選資料。
      if (nrow(ch) == 0) return(shiny::tags$p(class = "kit-hint", "\u5148\u9078\u8cc7\u6599\u3002"))
      # 參照世代排在最前面（只影響顯示的順序，不重跑資料的檢查）
      ch <- ch[order(ch$cohort != reference()), ]
      data_pick_input(ns("waves"), ch, intersect(shiny::isolate(input$waves), ch$value))
    })
    shiny::observeEvent(input$waves_all, {
      shiny::updateCheckboxGroupInput(session, "waves", selected = choices()$value)
    })
    shiny::observeEvent(input$waves_none, {
      shiny::updateCheckboxGroupInput(session, "waves", selected = character())
    })

    # 固定檔：讀不進來時用紅字說明，之前的固定檔留著。
    # 被改過的（和核對碼不符）先擋下來；按「仍然使用」才讀，之後一直提醒
    load_fixed <- function(allow_modified = FALSE) {
      path <- input$fixed_file$datapath
      fx <- tryCatch(kitsui_read_fixed(path, allow_modified), error = \(e) e)
      if (inherits(fx, "error")) {
        # 原文：讀取固定檔
        fixed_error(kit_error_text(fx, "\u8b80\u53d6\u56fa\u5b9a\u6a94"))
        fixed_modified(inherits(fx, "kit_fixed_modified"))
        return()
      }
      fixed_error(NULL)
      fixed_modified(FALSE)
      fixed_rv(list(fixed = fx, name = input$fixed_file$name,
                    integrity = kitsui_fixed_integrity(path)))
      from_session(NULL)
    }
    fixed_modified <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$fixed_file, load_fixed())
    shiny::observeEvent(input$fixed_force, load_fixed(allow_modified = TRUE))
    shiny::observeEvent(input$fixed_clear, {
      fixed_rv(NULL)
      from_session(NULL)
      fixed_error(NULL)
    })
    output$fixed_error <- shiny::renderUI({
      e <- fixed_error()
      if (is.null(e)) return(NULL)
      fr <- shiny::isolate(fixed_rv())
      # 原文：現在沒有固定檔。
      keep <- if (is.null(fr)) "\u73fe\u5728\u6c92\u6709\u56fa\u5b9a\u6a94\u3002" else {
        # 原文：仍然用之前的固定檔： | 這次連線的第  |  回合
        paste0("\u4ecd\u7136\u7528\u4e4b\u524d\u7684\u56fa\u5b9a\u6a94\uff1a", fr$name %||% paste0("\u9019\u6b21\u9023\u7dda\u7684\u7b2c ", from_session(), " \u56de\u5408"),
               # 原文：。
               "\u3002")
      }
      force <- if (isTRUE(fixed_modified())) {
        # 原文：仍然使用這個固定檔（我知道它被改過）
        shiny::tags$p(kitDesign::kit_button(ns("fixed_force"), "\u4ecd\u7136\u4f7f\u7528\u9019\u500b\u56fa\u5b9a\u6a94\uff08\u6211\u77e5\u9053\u5b83\u88ab\u6539\u904e\uff09", "danger"))
      }
      kitDesign::kit_status_ui(
        # 原文：固定檔讀不進來。
        kitDesign::kit_status("stop", "\u56fa\u5b9a\u6a94\u8b80\u4e0d\u9032\u4f86\u3002"),
        shiny::tags$p(kitsui_error_ui(e)), shiny::tags$p(keep),
        # 原文：怎麼辦：重新選上一回合在「④ 受試者評分」下載的 zip； | 第一回合不用固定檔。
        shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u91cd\u65b0\u9078\u4e0a\u4e00\u56de\u5408\u5728\u300c\u2463 \u53d7\u8a66\u8005\u8a55\u5206\u300d\u4e0b\u8f09\u7684 zip\uff1b", "\u7b2c\u4e00\u56de\u5408\u4e0d\u7528\u56fa\u5b9a\u6a94\u3002"),
        force
      )
    })

    # ④ 交過來的固定檔：不用上傳；世代與波次清空，等使用者勾新的
    if (!is.null(handover)) {
      shiny::observeEvent(handover(), {
        h <- handover()
        fixed_rv(list(fixed = h$fixed, name = NULL))
        from_session(h$round)
        latest_round(h$round)
        fixed_error(NULL)
        shiny::updateCheckboxGroupInput(session, "waves", selected = character())
      })
    }
    output$fixed_choice <- shiny::renderUI(data_fixed_choice_ui(ns, from_session()))
    # 第一回合（固定檔裡還沒有回合）：波次的單位用 ① 選的；之後沿用固定檔的
    fixed <- shiny::reactive({
      fx <- fixed_rv()$fixed %||% kitsui_fixed_empty(reference())
      if (length(fx$version$rounds) == 0) fx$version$wave_unit <- input$wave_unit %||% "months"
      fx
    })
    output$unit_ui <- shiny::renderUI({
      fx <- fixed_rv()$fixed
      if (!is.null(fx) && length(fx$version$rounds) > 0) {
        return(shiny::tags$small(class = "kit-hint d-block", data_unit_text(kitsui_wave_unit(fx))))
      }
      data_unit_ui(ns, shiny::isolate(input$wave_unit))
    })
    output$wave_hint <- shiny::renderText(
      # 原文：數字是 | 。
      paste0("\u6578\u5b57\u662f", data_wave_meaning(kitsui_wave_unit(fixed())), "\u3002")
    )

    # 勾世代與波次時停手 1 秒才檢查：一個一個勾時不用每勾一次就等一次
    # （測試時 options(kitsui.waves_delay = 0) 不等）
    delay <- getOption("kitsui.waves_delay", KIT_PICK_DELAY_MS)
    picks <- shiny::reactive(input$waves %||% character())
    if (delay > 0) picks <- shiny::debounce(picks, delay)

    # 預設的名稱 R1、R2…：只跟著固定檔變。名稱欄空白時用預設；打字時停手 0.5 秒才檢查
    default_label <- shiny::reactive(kitsui_default_label(fixed()$version$rounds))
    # 名稱欄只在空白、或還是之前放上去的預設名稱時，才換成新的預設；使用者自己打的名稱不動
    # （fixed() 也跟著波次的單位、參照世代變，換它們不能把名稱改回 R1）。固定檔本身換了
    # （上傳、清掉、④ 交過來）就是新的一回合，名稱一律換成新的預設
    shown_defaults <- character()
    put_label <- function(force = FALSE) {
      new <- default_label()
      now <- trimws(shiny::isolate(input$round_label) %||% "")
      if (force || !nzchar(now) || now %in% shown_defaults) {
        if (!identical(now, new)) shiny::updateTextInput(session, "round_label", value = new)
      }
      shown_defaults <<- union(shown_defaults, new)
    }
    shiny::observeEvent(default_label(), put_label())
    shiny::observeEvent(fixed_rv(), put_label(force = TRUE), ignoreInit = TRUE,
                        ignoreNULL = FALSE)
    label_in <- shiny::reactive(input$round_label %||% "")
    label_delay <- getOption("kitsui.label_delay", KIT_LABEL_DELAY_MS)
    if (label_delay > 0) label_in <- shiny::debounce(label_in, label_delay)
    round_label <- shiny::reactive({
      label <- trimws(label_in())
      if (nzchar(label)) label else default_label()
    })
    label_problem <- shiny::reactive(kitsui_label_problem(round_label(), fixed()$version$rounds))

    # 這一回合的資料與檢查：只跟著資料與勾選變（名稱、固定檔變了不重跑）
    checked <- shiny::reactive({
      src <- source_df()
      if (!is.null(src$error)) return(list(problems = src$error))
      p <- picks()
      if (length(p) == 0) return(kitsui_data_select(src$data, p, choices()))
      shiny::withProgress(
        # 原文：檢查資料… | 整理這一回合的世代、波次、量尺與作答
        message = "\u6aa2\u67e5\u8cc7\u6599\u2026", detail = "\u6574\u7406\u9019\u4e00\u56de\u5408\u7684\u4e16\u4ee3\u3001\u6ce2\u6b21\u3001\u91cf\u5c3a\u8207\u4f5c\u7b54", value = 0.5,
        kitsui_data_select(src$data, p, choices())
      )
    })
    # 每個量尺用誰的題庫：跟著固定檔變
    plan <- shiny::reactive({
      df <- checked()$df
      shiny::req(df)
      kitsui_round_plan(df, fixed())
    })

    result <- shiny::reactive({
      ck <- checked()
      if (is.null(ck$df) || length(label_problem()) > 0) return(NULL)
      files <- if (is_upload()) stats::setNames(upload()$path, upload()$name)
      list(
        df = ck$df, fixed = fixed(), round_label = round_label(), files = files,
        source = if (is_upload()) "upload" else "demo",
        data_file = if (is_upload()) upload()$name, fixed_file = fixed_rv()$name,
        waves = sort(unique(ck$picks$wave_m)), picks = ck$picks, scales = ck$scales,
        plan = plan(), persons = ck$persons
      )
    })

    output$problems <- shiny::renderUI({
      data_p <- checked()$problems
      p <- c(data_p, label_problem())
      if (length(p) == 0) return(NULL)
      # 上傳的檔案沒通過：提示可以到 kitJoin 轉
      to_kitjoin <- is_upload() && !is.null(input$file) && length(data_p) > 0
      # 原文：資料還不能用。
      kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u8cc7\u6599\u9084\u4e0d\u80fd\u7528\u3002"),
                shiny::tags$ul(lapply(p, \(x) shiny::tags$li(kitsui_error_ui(x)))),
                # 原文：怎麼辦：照上面的原因改左邊的設定，或改好資料檔再上傳。
                shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u7167\u4e0a\u9762\u7684\u539f\u56e0\u6539\u5de6\u908a\u7684\u8a2d\u5b9a\uff0c\u6216\u6539\u597d\u8cc7\u6599\u6a94\u518d\u4e0a\u50b3\u3002"),
                if (to_kitjoin) ui_kitjoin_hint(hint_class = "mb-0"))
    })
    output$hint <- shiny::renderUI({
      h <- checked()$hint
      if (is.null(h)) return(NULL)
      # 原文：左邊第 2 步，一個世代一列，數字是
      kitDesign::kit_callout(shiny::tags$strong(h), "\u5de6\u908a\u7b2c 2 \u6b65\uff0c\u4e00\u500b\u4e16\u4ee3\u4e00\u5217\uff0c\u6578\u5b57\u662f",
                             # 原文：。
                             data_wave_meaning(kitsui_wave_unit(fixed())), "\u3002")
    })

    # 要注意（黃色的提醒）：波次已經校準過、固定檔不是最新的一回合、資料檔之前用過、
    # 拿掉的約定以外的欄、wave_m 空白的列、像遺漏碼的分數
    notes <- shiny::reactive({
      ck <- checked()
      c(
        if (!is.null(ck$df)) kitsui_note_waves(plan(), fixed()),
        if (!is.null(ck$df)) kitsui_note_no_common(ck$df, plan(), fixed()),
        kitsui_note_stale(fixed()$version$rounds, latest_round()),
        if (is_upload()) kitsui_note_sources(upload()$files, fixed()),
        kitsui_note_dropped(unique(c(if (is_upload()) upload()$dropped, ck$dropped))),
        ck$notes
      )
    })
    # 題目沒有連在一起的量尺（② 會停下來）：在 ① 就說
    groups <- shiny::reactive({
      df <- checked()$df
      if (is.null(df)) return(NULL)
      tryCatch(kitsui_item_groups(df, plan(), fixed_rv()$fixed), error = \(e) NULL)
    })
    # 資料可以用時，要注意的事都放進狀態框（一個黃框）；資料還不能用時才另外放黃框
    output$notes <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      n <- notes()
      if (length(n) == 0) return(NULL)
      # 原文：要注意
      kitDesign::kit_callout(shiny::tags$strong("\u8981\u6ce8\u610f"), shiny::tags$ul(lapply(n, shiny::tags$li)),
                             warn = TRUE)
    })
    # 得分不完整的題（沒有 0 分、中間少了分數、只有一種分數）
    categories <- shiny::reactive({
      df <- checked()$df
      if (is.null(df) || !exists("kitsui_category_problems", mode = "function")) return(NULL)
      tryCatch(kitsui_category_problems(df), error = \(e) NULL)
    })
    output$categories <- shiny::renderUI({
      if (!is.null(result())) return(NULL)
      data_category_note(categories())
    })

    output$scales <- kit_table({
      ck <- checked()
      shiny::req(ck$scales)
      kitsui_screen_names(data_scale_table(ck$scales, fixed(), plan()))
    })
    output$borrowed <- shiny::renderUI(data_borrowed_note(plan(), fixed()))

    output$next_step <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      warned <- length(notes()) > 0 || NROW(categories()) > 0
      text <- if (NROW(groups()) > 0) {
        # 原文：有量尺的題目沒有連在一起，② 校準時這些量尺會停下來。
        paste0("\u6709\u91cf\u5c3a\u7684\u984c\u76ee\u6c92\u6709\u9023\u5728\u4e00\u8d77\uff0c\u2461 \u6821\u6e96\u6642\u9019\u4e9b\u91cf\u5c3a\u6703\u505c\u4e0b\u4f86\u3002",
               # 原文：請先看上面【完成，但請注意】的「怎麼辦」。
               "\u8acb\u5148\u770b\u4e0a\u9762\u3010\u5b8c\u6210\uff0c\u4f46\u8acb\u6ce8\u610f\u3011\u7684\u300c\u600e\u9ebc\u8fa6\u300d\u3002")
      } else if (warned) {
        # 原文：資料可以用，但有要注意的事，請先看過上面狀態框裡的「要注意」。下一步是校準。
        "\u8cc7\u6599\u53ef\u4ee5\u7528\uff0c\u4f46\u6709\u8981\u6ce8\u610f\u7684\u4e8b\uff0c\u8acb\u5148\u770b\u904e\u4e0a\u9762\u72c0\u614b\u6846\u88e1\u7684\u300c\u8981\u6ce8\u610f\u300d\u3002\u4e0b\u4e00\u6b65\u662f\u6821\u6e96\u3002"
      } else {
        # 原文：資料沒問題。下一步是校準。
        "\u8cc7\u6599\u6c92\u554f\u984c\u3002\u4e0b\u4e00\u6b65\u662f\u6821\u6e96\u3002"
      }
      # 原文：下一步：題目校準
      kitDesign::kit_next_step(text, ns("go_items"), "\u4e0b\u4e00\u6b65\uff1a\u984c\u76ee\u6821\u6e96")
    })
    # 設定摘要與狀態框：資料可以用時【完成】；有要注意的事時【完成，但請注意】
    output$settings <- shiny::renderUI(
      kitDesign::kit_settings_line(kitsui_settings_text(result()), prefix = NULL)
    )
    output$data_status <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      data_status_ui(r, notes(), categories(), fixed_rv()$integrity, groups())
    })
    shiny::observeEvent(input$go_items, goto("items"))

    output$fixed_info <- shiny::renderUI({
      fr <- fixed_rv()
      if (is.null(fr)) {
        # 原文：沒有固定檔：這一回合每個量尺都是自由校準。
        return(shiny::tags$p(class = "kit-hint", "\u6c92\u6709\u56fa\u5b9a\u6a94\uff1a\u9019\u4e00\u56de\u5408\u6bcf\u500b\u91cf\u5c3a\u90fd\u662f\u81ea\u7531\u6821\u6e96\u3002"))
      }
      # 原文：這次連線的第  |  回合（還沒下載）
      name <- fr$name %||% paste0("\u9019\u6b21\u9023\u7dda\u7684\u7b2c ", from_session(), " \u56de\u5408\uff08\u9084\u6c92\u4e0b\u8f09\uff09")
      data_fixed_summary(fr$fixed, name, fr$integrity)
    })
    output$scale_example <- shiny::renderText(data_scale_example(checked()$scales))

    # 作答設計：每個量尺一張
    output$design_pick <- shiny::renderUI({
      sc <- checked()$scales
      shiny::req(sc)
      # 原文：量尺
      shiny::selectInput(ns("design_scale"), "\u91cf\u5c3a", sc$scale)
    })
    # 選單收在細節裡：收起來時也要畫好（kits 的檢查與 R 程式用它選的量尺）
    shiny::outputOptions(output, "design_pick", suspendWhenHidden = FALSE)
    # 作答矩陣（kits_resp()）只做正在看的量尺；同一份資料算過的記起來。
    # 資料一變就換一個新的記憶（舊的丟掉）
    resp_memo <- shiny::reactive({
      checked()$df
      new.env()
    })
    design_resp <- shiny::reactive({
      ck <- checked()
      s <- input$design_scale
      shiny::req(ck$scales, s %in% ck$scales$scale)
      memo <- resp_memo()
      if (is.null(memo[[s]])) {
        memo[[s]] <- shiny::withProgress(
          # 原文：整理作答矩陣…
          message = "\u6574\u7406\u4f5c\u7b54\u77e9\u9663\u2026", detail = s, value = 0.5,
          tryCatch(kitsui_resp(ck$df, s), error = \(e) e)
        )
      }
      memo[[s]]
    })
    design_plot <- shiny::reactive({
      r <- design_resp()
      shiny::validate(shiny::need(!inherits(r, "error"), paste(
        # 原文：這個量尺的作答有問題：
        "\u9019\u500b\u91cf\u5c3a\u7684\u4f5c\u7b54\u6709\u554f\u984c\uff1a", if (inherits(r, "error")) {
          # 原文：（kits_resp 的訊息： | ）
          paste0(kitsui_kits_zh(conditionMessage(r)), "\uff08kits_resp \u7684\u8a0a\u606f\uff1a", conditionMessage(r), "\uff09")
        }
      )))
      # 通過比例照固定檔的通過步驟（示範資料、沒有固定檔時每題都是 1）
      p <- plot(kitsui_resp_pass(r, kitsui_steps_vec(fixed()$pass_steps)))
      # 圖的字照 kits 的英文；最後套上 KIT 的樣子（kitDesign::kit_plotly_style()）
      kitDesign::kit_plotly_style(kitsui_wave_axis(p, kitsui_wave_unit(fixed())))
    })
    output$design <- plotly::renderPlotly(design_plot())
    output$design_steps <- shiny::renderUI(data_steps_note(fixed()$pass_steps))
    # 下載鈕等圖畫好才出現；檔名跟著選的量尺（換量尺時按鈕重畫）
    output$design_dl <- shiny::renderUI({
      # 還沒有量尺（req）或 kits_resp 出錯時都是 error：不顯示
      if (inherits(tryCatch(design_resp(), error = \(e) e), "error")) return(NULL)
      shiny::tags$div(class = "kit-downloads", download_plot_ui(
        # 原文：下載作答設計圖（HTML）
        session, "dl_design", "\u4e0b\u8f09\u4f5c\u7b54\u8a2d\u8a08\u5716\uff08HTML\uff09", design_plot,
        \() paste0("design_", input$design_scale)
      ))
    })

    output$preview_meta <- shiny::renderText({
      df <- checked()$df
      shiny::req(df)
      # 原文：這一回合共 %s 列作答，以下前 %d 列。完整的請下載。
      sprintf("\u9019\u4e00\u56de\u5408\u5171 %s \u5217\u4f5c\u7b54\uff0c\u4ee5\u4e0b\u524d %d \u5217\u3002\u5b8c\u6574\u7684\u8acb\u4e0b\u8f09\u3002", kitDesign::kit_num(nrow(df)),
              KIT_PREVIEW_ROWS)
    })
    output$preview <- kit_table({
      df <- checked()$df
      shiny::req(df)
      df_for_display(df, KIT_PREVIEW_ROWS)
    })
    output$dl_data_ui <- render_download_csv(
      # 原文：下載這一回合的資料（CSV）
      session, "dl_data", "\u4e0b\u8f09\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\uff08CSV\uff09", \() shiny::req(checked()$df),
      \() paste0("responses_", round_label())
    )

    output$check <- kit_table({
      r <- design_resp()
      if (inherits(r, "error")) {
        return(data.frame(scale = input$design_scale, result = conditionMessage(r)))
      }
      tibble::add_column(check_table(resp = r), scale = input$design_scale, .before = 1) |>
        checks_or_ok()
    })

    code_text <- shiny::reactive({
      ck <- checked()
      shiny::req(ck$scales)
      upload_name <- if (is_upload()) upload()$name
      s <- input$design_scale %||% ck$scales$scale[1]
      # 通過步驟只寫這個量尺有的題（和作答設計圖相同）
      steps <- kitsui_steps_vec(fixed()$pass_steps)
      steps <- steps[names(steps) %in% ck$df$item_id[scale_key(ck$df$domain, ck$df$cohort) == s]]
      kitsui_code_data(s, upload_name, sort(unique(ck$picks$wave_m)), picks = ck$picks,
                       steps = steps)
    })
    # 用 R 怎麼做：打開時才算；每行最多 70 欄（kitDesign::kit_code()）
    # 原文：kits 版
    output$code <- kitDesign::kit_render_code_panel(\() list("kits \u7248" = code_text()))

    result
  })
}

#' ① 的狀態框：資料可以用時【完成】一句（幾個量尺、幾列作答）；有要注意的事、得分不完整的題、
#' 題目沒有連在一起的量尺、固定檔的核對碼不符時【完成，但請注意】
#'
#' 要注意的事都放在這一個框裡（一個黃框，不另外疊一個「要注意」）：最多 3 點，其他收在「還有 N 點」。
#' 題目沒有連在一起的量尺排在最前面（② 一定會停下來），後面接「怎麼辦」。
#' @param notes 要注意的提醒（中文句子）；categories 是 kitsui_category_problems()（得分不完整的題）
#' @param integrity NULL 或 kitsui_fixed_integrity()
#' @param groups NULL 或 kitsui_item_groups()（題目沒有連在一起的量尺）
#' @noRd
data_status_ui <- function(r, notes = character(), categories = NULL, integrity = NULL,
                           groups = NULL) {
  # 舊的寫法（則數）也接受
  # 原文：要注意的事。
  if (is.numeric(notes)) notes <- rep("\u8981\u6ce8\u610f\u7684\u4e8b\u3002", notes)
  if (is.numeric(categories)) categories <- NULL
  n_bad <- NROW(groups)
  line <- if (n_bad > 0) {
    # 原文：資料讀好了：%s 個量尺、%s 列作答；其中 %s 個量尺的題目沒有連在一起，② 校準時會停下來。
    sprintf("\u8cc7\u6599\u8b80\u597d\u4e86\uff1a%s \u500b\u91cf\u5c3a\u3001%s \u5217\u4f5c\u7b54\uff1b\u5176\u4e2d %s \u500b\u91cf\u5c3a\u7684\u984c\u76ee\u6c92\u6709\u9023\u5728\u4e00\u8d77\uff0c\u2461 \u6821\u6e96\u6642\u6703\u505c\u4e0b\u4f86\u3002",
            kitDesign::kit_num(nrow(r$scales)), kitDesign::kit_num(nrow(r$df)),
            kitDesign::kit_num(n_bad))
  } else {
    # 原文：資料可以用：%s 個量尺、%s 列作答。
    sprintf("\u8cc7\u6599\u53ef\u4ee5\u7528\uff1a%s \u500b\u91cf\u5c3a\u3001%s \u5217\u4f5c\u7b54\u3002", kitDesign::kit_num(nrow(r$scales)),
            kitDesign::kit_num(nrow(r$df)))
  }
  unit <- kitsui_wave_unit(r$fixed)
  cat_items <- kitsui_note_categories(categories, n = Inf)
  points <- c(
    kitsui_note_groups(groups, unit),
    if (!is.null(integrity) && !identical(integrity$status, "ok")) {
      # 原文：固定檔的核對碼：
      paste0("\u56fa\u5b9a\u6a94\u7684\u6838\u5c0d\u78bc\uff1a", integrity$text)
    },
    notes,
    if (length(cat_items) > 0) {
      # 原文：有 %s 題的得分不完整（沒有 0 分、中間少了分數，或只有一種分數），② 會照規則處理：
      c(sprintf("\u6709 %s \u984c\u7684\u5f97\u5206\u4e0d\u5b8c\u6574\uff08\u6c92\u6709 0 \u5206\u3001\u4e2d\u9593\u5c11\u4e86\u5206\u6578\uff0c\u6216\u53ea\u6709\u4e00\u7a2e\u5206\u6578\uff09\uff0c\u2461 \u6703\u7167\u898f\u5247\u8655\u7406\uff1a",
                kitDesign::kit_num(NROW(categories))), cat_items)
    }
  )
  if (length(points) == 0) return(kitDesign::kit_status_ui(kitDesign::kit_status("done", line)))
  kitDesign::kit_status_ui(kitDesign::kit_status(
    "warn", line, points = points),
    if (n_bad > 0) {
      shiny::tags$p(
        class = "kit-status-action",
        # 原文：怎麼辦：
        shiny::tags$strong("\u600e\u9ebc\u8fa6\uff1a"),
        # 原文：各波的同一題要用同一個題號。回 kitJoin 的「 | 」，
        paste0("\u5404\u6ce2\u7684\u540c\u4e00\u984c\u8981\u7528\u540c\u4e00\u500b\u984c\u865f\u3002\u56de kitJoin \u7684\u300c", KITJOIN_KITS_TAB, "\u300d\uff0c",
               # 原文：第 2 步「合成同一組題號」，把各波的同一題合成一個題號，再重新上傳。
               "\u7b2c 2 \u6b65\u300c\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f\u300d\uff0c\u628a\u5404\u6ce2\u7684\u540c\u4e00\u984c\u5408\u6210\u4e00\u500b\u984c\u865f\uff0c\u518d\u91cd\u65b0\u4e0a\u50b3\u3002"),
        ui_kitjoin_button()
      )
    },
    if (length(cat_items) > 0) {
      shiny::tags$p(
        class = "kit-status-action",
        # 原文：得分不完整的題：② 校準時，題庫裡有這一題，就固定在題庫的值；題庫裡沒有這一題，
        "\u5f97\u5206\u4e0d\u5b8c\u6574\u7684\u984c\uff1a\u2461 \u6821\u6e96\u6642\uff0c\u984c\u5eab\u88e1\u6709\u9019\u4e00\u984c\uff0c\u5c31\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\uff1b\u984c\u5eab\u88e1\u6c92\u6709\u9019\u4e00\u984c\uff0c",
        # 原文：這一回合先不估它。作答的分數不會被改動。
        "\u9019\u4e00\u56de\u5408\u5148\u4e0d\u4f30\u5b83\u3002\u4f5c\u7b54\u7684\u5206\u6578\u4e0d\u6703\u88ab\u6539\u52d5\u3002",
        # 原文：要平移或合併分數，可以在 kitJoin「 | 」的題目設定改好再上傳。
        paste0("\u8981\u5e73\u79fb\u6216\u5408\u4f75\u5206\u6578\uff0c\u53ef\u4ee5\u5728 kitJoin\u300c", KITJOIN_KITS_TAB, "\u300d\u7684\u984c\u76ee\u8a2d\u5b9a\u6539\u597d\u518d\u4e0a\u50b3\u3002"),
        if (n_bad == 0) ui_kitjoin_button()
      )
    }
  )
}

#' 波次的單位：第一回合可以選（之後沿用固定檔的）
#' @noRd
data_unit_ui <- function(ns, selected = NULL) {
  shiny::tagList(
    # 原文：波次（wave_m）的單位
    shiny::radioButtons(ns("wave_unit"), "\u6ce2\u6b21\uff08wave_m\uff09\u7684\u55ae\u4f4d", KITSUI_WAVE_UNITS,
                        selected %||% "months", inline = TRUE),
    shiny::tags$small(
      class = "kit-hint d-block",
      # 原文：第一回合選，寫進固定檔；之後的回合沿用，不能改。
      "\u7b2c\u4e00\u56de\u5408\u9078\uff0c\u5beb\u9032\u56fa\u5b9a\u6a94\uff1b\u4e4b\u5f8c\u7684\u56de\u5408\u6cbf\u7528\uff0c\u4e0d\u80fd\u6539\u3002",
      # 原文：KIT 的 wave_m 是施測月齡（月）。
      "KIT \u7684 wave_m \u662f\u65bd\u6e2c\u6708\u9f61\uff08\u6708\uff09\u3002",
      # 原文：單位是年或波次序號時，不照月齡借另一世代題庫，題目表也不算 age50（通過年齡）。
      "\u55ae\u4f4d\u662f\u5e74\u6216\u6ce2\u6b21\u5e8f\u865f\u6642\uff0c\u4e0d\u7167\u6708\u9f61\u501f\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\uff0c\u984c\u76ee\u8868\u4e5f\u4e0d\u7b97 age50\uff08\u901a\u904e\u5e74\u9f61\uff09\u3002"
    )
  )
}

#' 固定檔裡的波次單位（之後的回合：只顯示，不能改）
#' @noRd
data_unit_text <- function(unit) {
  name <- names(KITSUI_WAVE_UNITS)[match(unit, KITSUI_WAVE_UNITS)]
  # 原文：波次（wave_m）的單位： | （來自固定檔，之後的回合沿用）。
  paste0("\u6ce2\u6b21\uff08wave_m\uff09\u7684\u55ae\u4f4d\uff1a", name, "\uff08\u4f86\u81ea\u56fa\u5b9a\u6a94\uff0c\u4e4b\u5f8c\u7684\u56de\u5408\u6cbf\u7528\uff09\u3002",
         # 原文：不照月齡借另一世代題庫，題目表也不算 age50。
         if (unit != "months") "\u4e0d\u7167\u6708\u9f61\u501f\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\uff0c\u984c\u76ee\u8868\u4e5f\u4e0d\u7b97 age50\u3002")
}

#' 勾選框的數字是什麼
#' @noRd
data_wave_meaning <- function(unit) {
  # 原文：年 | 波次序號 | 施測月齡
  switch(unit, years = "\u5e74", waves = "\u6ce2\u6b21\u5e8f\u865f", "\u65bd\u6e2c\u6708\u9f61")
}

#' 作答設計圖：固定檔裡有通過步驟不是 1 的題時，一句說明
#' @param pass_steps 固定檔的 pass_steps
#' @noRd
data_steps_note <- function(pass_steps) {
  steps <- kitsui_steps_vec(pass_steps)
  steps <- steps[steps != 1]
  if (length(steps) == 0) return(NULL)
  shiny::tags$p(class = "kit-hint", paste0(
    # 原文：固定檔裡有  |  題的通過步驟不是 1：
    "\u56fa\u5b9a\u6a94\u88e1\u6709 ", length(steps), " \u984c\u7684\u901a\u904e\u6b65\u9a5f\u4e0d\u662f 1\uff1a",
    # 原文：（ |  分以上） | 。
    kitsui_some(paste0(names(steps), "\uff08", steps, " \u5206\u4ee5\u4e0a\uff09")), "\u3002"
  # 原文：可以在「④ 受試者評分」的題目表改。
  ), "\u53ef\u4ee5\u5728\u300c\u2463 \u53d7\u8a66\u8005\u8a55\u5206\u300d\u7684\u984c\u76ee\u8868\u6539\u3002")
}

#' 得分不完整的題：黃色的提醒，說 ② 怎麼處理（分數本身不改）
#' @param problems NULL 或 kitsui_category_problems()
#' @noRd
data_category_note <- function(problems) {
  items <- kitsui_note_categories(problems)
  if (length(items) == 0) return(NULL)
  kitDesign::kit_callout(
    shiny::tags$strong(sprintf(
      # 原文：有 %d 題的得分不完整（沒有 0 分、中間少了分數，或只有一種分數）
      "\u6709 %d \u984c\u7684\u5f97\u5206\u4e0d\u5b8c\u6574\uff08\u6c92\u6709 0 \u5206\u3001\u4e2d\u9593\u5c11\u4e86\u5206\u6578\uff0c\u6216\u53ea\u6709\u4e00\u7a2e\u5206\u6578\uff09", NROW(problems)
    )),
    shiny::tags$ul(lapply(items, shiny::tags$li)),
    # 原文：② 校準時，題庫裡有這一題，就固定在題庫的值。
    shiny::tags$p("\u2461 \u6821\u6e96\u6642\uff0c\u984c\u5eab\u88e1\u6709\u9019\u4e00\u984c\uff0c\u5c31\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\u3002",
                  # 原文：題庫裡沒有這一題，這一回合先不估它。
                  "\u984c\u5eab\u88e1\u6c92\u6709\u9019\u4e00\u984c\uff0c\u9019\u4e00\u56de\u5408\u5148\u4e0d\u4f30\u5b83\u3002"),
    # 原文：作答的分數不會被改動。
    shiny::tags$p("\u4f5c\u7b54\u7684\u5206\u6578\u4e0d\u6703\u88ab\u6539\u52d5\u3002",
                  # 原文：要平移或合併分數，可以在 kitJoin「
                  paste0("\u8981\u5e73\u79fb\u6216\u5408\u4f75\u5206\u6578\uff0c\u53ef\u4ee5\u5728 kitJoin\u300c", KITJOIN_KITS_TAB,
                         # 原文：」的題目設定改好再上傳。
                         "\u300d\u7684\u984c\u76ee\u8a2d\u5b9a\u6539\u597d\u518d\u4e0a\u50b3\u3002"),
                  ui_kitjoin_button()),
    warn = TRUE
  )
}

#' 量尺表：列次、題數、波次與校準方式（有題庫時固定校準；借用另一世代題庫時寫出借誰的）；
#' 欄名是英文（畫面上用 kitsui_screen_names() 換成中文）
#' @param plan NULL 或 kitsui_round_plan()
#' @noRd
data_scale_table <- function(scales, fixed, plan = NULL) {
  # 參照世代排在前面（固定檔的參照世代）
  ref <- fixed$version$reference %||% KITSUI_REF_COHORT
  scales <- scales[order(scale_cohort(scales$scale) != ref, scales$scale), ]
  # 原文：固定校準（有題庫） | 自由校準（沒有題庫）
  mode <- ifelse(scales$scale %in% fixed$bank$scale, "\u56fa\u5b9a\u6821\u6e96\uff08\u6709\u984c\u5eab\uff09", "\u81ea\u7531\u6821\u6e96\uff08\u6c92\u6709\u984c\u5eab\uff09")
  borrow <- purrr::map_chr(scales$scale, \(s) data_borrow_text(plan[plan$data_scale == s, ]))
  data.frame(
    scale = scales$scale, n_visits = scales$n_visits, n_items = scales$n_items,
    waves = scales$waves, calibration = dplyr::coalesce(borrow, mode)
  )
}

# 一個資料量尺借用另一世代題庫：全部波次都借時「固定校準（借用另一世代題庫：motor M36）」；
# 只有部分波次借時寫出哪些波次；沒有借時 NA
data_borrow_text <- function(rows) {
  b <- rows[rows$borrowed, ]
  if (nrow(b) == 0) return(NA_character_)
  # 原文：、
  to <- paste(unique(b$scale), collapse = "\u3001")
  # 原文：固定校準（借用另一世代題庫： | ）
  if (nrow(b) == nrow(rows)) return(paste0("\u56fa\u5b9a\u6821\u6e96\uff08\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\uff1a", to, "\uff09"))
  # 原文：固定校準（ | 、 |  個月借用另一世代題庫： | ；
  paste0("\u56fa\u5b9a\u6821\u6e96\uff08", paste(b$wave_m, collapse = "\u3001"), " \u500b\u6708\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\uff1a", to, "\uff1b",
         # 原文：其他波次用自己的題庫）
         "\u5176\u4ed6\u6ce2\u6b21\u7528\u81ea\u5df1\u7684\u984c\u5eab\uff09")
}

#' 借用另一世代題庫的說明：哪個波次、借誰的、分數怎麼換算
#' @noRd
data_borrowed_note <- function(plan, fixed) {
  b <- plan[plan$borrowed, ]
  if (is.null(b) || nrow(b) == 0) return(NULL)
  li <- purrr::pmap(b, \(data_scale, wave_m, scale, why, ...) {
    path <- kitsui_link_path(fixed, scale)
    route <- fixed$links$to_scale[match(path, kitsui_link_key(fixed$links))]
    shiny::tags$li(
      # 原文：%s 的 %s 個月：%s。
      sprintf("%s \u7684 %s \u500b\u6708\uff1a%s\u3002", data_scale, format(wave_m), why),
      # 原文：所以用 %s 的題庫校準。
      sprintf("\u6240\u4ee5\u7528 %s \u7684\u984c\u5eab\u6821\u6e96\u3002", scale),
      # 原文：分數經 %s 換算。 |  → 
      sprintf("\u5206\u6578\u7d93 %s \u63db\u7b97\u3002", paste(c(scale, route), collapse = " \u2192 "))
    )
  })
  # 原文：借用另一世代題庫
  kitDesign::kit_callout(shiny::tags$strong("\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab"), shiny::tags$ul(li),
                         # 原文：在 ② 題目校準可以改用別的題庫。
                         shiny::tags$p("\u5728 \u2461 \u984c\u76ee\u6821\u6e96\u53ef\u4ee5\u6539\u7528\u5225\u7684\u984c\u5eab\u3002"))
}

#' 參照世代：第一回合（沒有固定檔）選一個，寫進固定檔；有固定檔時只顯示固定檔裡的
#'
#' 預設：資料有 M03 時是 M03，沒有時是第一個世代（kitsui_ref_pick()）。
#' @param cohorts 資料裡的世代；fixed 是上一回合的固定檔（NULL 時是第一回合）；pick 是原本選的
#' @noRd
data_reference_ui <- function(ns, cohorts, fixed = NULL, pick = NULL) {
  if (!is.null(fixed)) {
    ref <- fixed$version$reference
    away <- if (length(cohorts) > 0 && !ref %in% cohorts) {
      # 原文：這份資料沒有  | ：其他世代的內部連結仍然連到題庫裡的  | 。
      paste0("\u9019\u4efd\u8cc7\u6599\u6c92\u6709 ", ref, "\uff1a\u5176\u4ed6\u4e16\u4ee3\u7684\u5167\u90e8\u9023\u7d50\u4ecd\u7136\u9023\u5230\u984c\u5eab\u88e1\u7684 ", ref, "\u3002")
    }
    return(shiny::tags$p(
      # 原文：參照世代：
      class = "kit-hint mt-2", shiny::tags$strong("\u53c3\u7167\u4e16\u4ee3\uff1a", ref),
      # 原文：（固定檔裡的。第一回合選好以後，之後的回合都一樣，不能改。）
      "\uff08\u56fa\u5b9a\u6a94\u88e1\u7684\u3002\u7b2c\u4e00\u56de\u5408\u9078\u597d\u4ee5\u5f8c\uff0c\u4e4b\u5f8c\u7684\u56de\u5408\u90fd\u4e00\u6a23\uff0c\u4e0d\u80fd\u6539\u3002\uff09", away
    ))
  }
  if (length(cohorts) == 0) return(NULL)
  shiny::tags$div(
    class = "mt-2",
    # 原文：參照世代
    shiny::selectInput(ns("reference"), "\u53c3\u7167\u4e16\u4ee3", cohorts, kitsui_ref_pick(pick, cohorts),
                       width = "12rem"),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：其他世代的量尺都用內部連結換到參照世代的量尺；
      "\u5176\u4ed6\u4e16\u4ee3\u7684\u91cf\u5c3a\u90fd\u7528\u5167\u90e8\u9023\u7d50\u63db\u5230\u53c3\u7167\u4e16\u4ee3\u7684\u91cf\u5c3a\uff1b",
      # 原文：參照世代有 D 分數的錨題時，外部連結也從它開始。
      "\u53c3\u7167\u4e16\u4ee3\u6709 D \u5206\u6578\u7684\u9328\u984c\u6642\uff0c\u5916\u90e8\u9023\u7d50\u4e5f\u5f9e\u5b83\u958b\u59cb\u3002",
      # 原文：參照世代會寫進這一回合的固定檔，之後的回合都用同一個，不能再改。
      "\u53c3\u7167\u4e16\u4ee3\u6703\u5beb\u9032\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\uff0c\u4e4b\u5f8c\u7684\u56de\u5408\u90fd\u7528\u540c\u4e00\u500b\uff0c\u4e0d\u80fd\u518d\u6539\u3002",
      # 原文：資料有 M03 時預設是 M03，沒有時是第一個世代。
      "\u8cc7\u6599\u6709 M03 \u6642\u9810\u8a2d\u662f M03\uff0c\u6c92\u6709\u6642\u662f\u7b2c\u4e00\u500b\u4e16\u4ee3\u3002"
    )
  )
}

#' 量尺的例子（量尺表上面的一句話）：用這一回合的第一個量尺；還沒有時用 lan M03
#' @noRd
data_scale_example <- function(scales) {
  ex <- if (NROW(scales) > 0) scales$scale[1] else "lan M03"
  # 原文：量尺 = 領域 × 世代，例如  | 。
  paste0("\u91cf\u5c3a = \u9818\u57df \u00d7 \u4e16\u4ee3\uff0c\u4f8b\u5982 ", ex, "\u3002")
}

#' 固定檔的內容：之前的回合、量尺、題庫、連結、核對碼
#' @param integrity NULL 或 kitsui_fixed_integrity()（這次連線交過來的固定檔沒有）
#' @noRd
data_fixed_summary <- function(fixed, name, integrity = NULL) {
  v <- fixed$version
  bank <- dplyr::count(fixed$bank, .data$scale, name = "steps")
  items <- dplyr::summarise(fixed$bank, items = dplyr::n_distinct(.data$item_id), .by = "scale")
  bank <- dplyr::left_join(bank, items, by = "scale")
  links <- dplyr::distinct(fixed$links, .data$kind, .data$from_scale, .data$to_scale)
  li <- function(...) shiny::tags$li(...)
  shiny::tags$div(
    class = "kit-callout",
    # 原文：固定檔：
    shiny::tags$strong("\u56fa\u5b9a\u6a94\uff1a", name),
    shiny::tags$ul(
      # 原文：之前的回合： | 、 | （沒有）
      li("\u4e4b\u524d\u7684\u56de\u5408\uff1a", if (length(v$rounds)) paste(v$rounds, collapse = "\u3001") else "\uff08\u6c92\u6709\uff09"),
      # 原文：建立於  | ，kits  | ，參照世代 
      li("\u5efa\u7acb\u65bc ", v$created, "\uff0ckits ", v$kits, "\uff0c\u53c3\u7167\u4e16\u4ee3 ", v$reference),
      if (!is.null(integrity)) {
        cls <- if (identical(integrity$status, "ok")) NULL else "kit-warn-text"
        # 原文：核對碼：
        li(shiny::tags$span(class = cls, "\u6838\u5c0d\u78bc\uff1a", integrity$text))
      },
      if (length(integrity$ignored) > 0) {
        # 原文：zip 裡多出來、沒有讀的檔案：
        li(shiny::tags$span(class = "kit-warn-text", "zip \u88e1\u591a\u51fa\u4f86\u3001\u6c92\u6709\u8b80\u7684\u6a94\u6848\uff1a",
                            # 原文：、
                            paste(integrity$ignored, collapse = "\u3001")))
      },
      # 原文：題庫： | （空的）
      li("\u984c\u5eab\uff1a", if (nrow(bank) == 0) "\uff08\u7a7a\u7684\uff09" else paste(
        # 原文：%s %d 題 %d 步 | ；
        sprintf("%s %d \u984c %d \u6b65", bank$scale, bank$items, bank$steps), collapse = "\uff1b"
      )),
      # 原文：新版本的題： |  題
      li("\u65b0\u7248\u672c\u7684\u984c\uff1a", nrow(fixed$versions), " \u984c"),
      # 原文：連結： | （還沒有）
      li("\u9023\u7d50\uff1a", if (nrow(links) == 0) "\uff08\u9084\u6c92\u6709\uff09" else paste(
        # 原文：%s → %s | ；
        sprintf("%s \u2192 %s", links$from_scale, links$to_scale), collapse = "\uff1b"
      ))
    )
  )
}

#' 固定檔的選擇：上傳 zip；或 ④ 交過來的（這次連線上一回合的）固定檔
#' @noRd
data_fixed_choice_ui <- function(ns, round) {
  if (!is.null(round)) {
    return(shiny::tagList(
      # 原文：上一回合的固定檔：來自這次連線的第  |  回合
      kitDesign::kit_callout(shiny::tags$strong(paste0("\u4e0a\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\uff1a\u4f86\u81ea\u9019\u6b21\u9023\u7dda\u7684\u7b2c ", round, " \u56de\u5408"))),
      # 原文：改用上傳的固定檔，或不用
      kitDesign::kit_button(ns("fixed_clear"), "\u6539\u7528\u4e0a\u50b3\u7684\u56fa\u5b9a\u6a94\uff0c\u6216\u4e0d\u7528")
    ))
  }
  shiny::tagList(
    # 原文：選擇 zip…
    shiny::fileInput(ns("fixed_file"), NULL, accept = ".zip", buttonLabel = "\u9078\u64c7 zip\u2026",
                     # 原文：沒有（第一回合）
                     placeholder = "\u6c92\u6709\uff08\u7b2c\u4e00\u56de\u5408\uff09"),
    # 原文：不用固定檔
    kitDesign::kit_button(ns("fixed_clear"), "\u4e0d\u7528\u56fa\u5b9a\u6a94")
  )
}

#' 資料檔的欄位：上傳旁邊可以打開的表（這一頁只有這一張）
#' @noRd
data_columns_guide <- function() {
  shiny::tags$details(
    class = "kit-guide",
    # 原文：資料檔要有哪些欄位
    shiny::tags$summary(shiny::tags$strong("\u8cc7\u6599\u6a94\u8981\u6709\u54ea\u4e9b\u6b04\u4f4d")),
    # 原文：一個檔案可以有好幾個波次。 | 也可以分成好幾個檔案一起上傳。
    shiny::tags$p("\u4e00\u500b\u6a94\u6848\u53ef\u4ee5\u6709\u597d\u5e7e\u500b\u6ce2\u6b21\u3002", "\u4e5f\u53ef\u4ee5\u5206\u6210\u597d\u5e7e\u500b\u6a94\u6848\u4e00\u8d77\u4e0a\u50b3\u3002",
                  # 原文：required 是「是」的欄一定要有。
                  "required \u662f\u300c\u662f\u300d\u7684\u6b04\u4e00\u5b9a\u8981\u6709\u3002"),
    data_columns_table()
  )
}

#' 資料約定的欄位表（欄名是英文）
#' @noRd
data_columns_table <- function() {
  tab <- kitsui_columns_table()
  cell <- \(tag, x) lapply(x, tag)
  shiny::tags$table(
    class = "table table-sm kit-columns",
    shiny::tags$thead(shiny::tags$tr(cell(shiny::tags$th, names(tab)))),
    shiny::tags$tbody(purrr::pmap(tab, \(...) shiny::tags$tr(cell(shiny::tags$td, c(...)))))
  )
}

#' 加一個新的波次：六個步驟（預設收起來）
#' @noRd
data_new_wave_guide <- function() {
  shiny::tags$details(
    class = "kit-guide",
    # 原文：加一個新的波次
    shiny::tags$summary(shiny::tags$strong("\u52a0\u4e00\u500b\u65b0\u7684\u6ce2\u6b21")),
    shiny::tags$ol(
      class = "kit-steps",
      # 原文：在 ④ 下載這一回合的固定檔（zip）；或按「用這一回合的固定檔開始下一回合」，留在這次連線裡。
      shiny::tags$li("\u5728 \u2463 \u4e0b\u8f09\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\uff08zip\uff09\uff1b\u6216\u6309\u300c\u7528\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\u958b\u59cb\u4e0b\u4e00\u56de\u5408\u300d\uff0c\u7559\u5728\u9019\u6b21\u9023\u7dda\u88e1\u3002"),
      # 原文：在這一頁勾新的世代與波次，或上傳新的資料。
      shiny::tags$li("\u5728\u9019\u4e00\u9801\u52fe\u65b0\u7684\u4e16\u4ee3\u8207\u6ce2\u6b21\uff0c\u6216\u4e0a\u50b3\u65b0\u7684\u8cc7\u6599\u3002"),
      # 原文：到 ② 題目校準：有固定檔時，舊題的參數不動，只估新的步驟。
      shiny::tags$li("\u5230 \u2461 \u984c\u76ee\u6821\u6e96\uff1a\u6709\u56fa\u5b9a\u6a94\u6642\uff0c\u820a\u984c\u7684\u53c3\u6578\u4e0d\u52d5\uff0c\u53ea\u4f30\u65b0\u7684\u6b65\u9a5f\u3002"),
      # 原文：到 ③ 連結：做好的內部連結與外部連結已經凍結，直接沿用。
      shiny::tags$li("\u5230 \u2462 \u9023\u7d50\uff1a\u505a\u597d\u7684\u5167\u90e8\u9023\u7d50\u8207\u5916\u90e8\u9023\u7d50\u5df2\u7d93\u51cd\u7d50\uff0c\u76f4\u63a5\u6cbf\u7528\u3002"),
      # 原文：到 ④ 算新的孩子的分數。
      shiny::tags$li("\u5230 \u2463 \u7b97\u65b0\u7684\u5b69\u5b50\u7684\u5206\u6578\u3002"),
      # 原文：存好這一回合的分數表與新的固定檔。
      shiny::tags$li("\u5b58\u597d\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u8868\u8207\u65b0\u7684\u56fa\u5b9a\u6a94\u3002")
    )
  )
}
