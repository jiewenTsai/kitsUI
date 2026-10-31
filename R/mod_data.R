# ① 資料：示範資料或上傳、這一回合的世代與波次、上一回合的固定檔、這一回合的名稱

#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_guide(
      goal = "選這一回合要校準的資料。第二回合起，再放上一回合下載的固定檔。",
      steps = c(
        "選資料來源：示範資料，或上傳自己的長格式資料（可以一次選好幾個檔案）。",
        "勾這一回合的世代與波次（例如月齡）。一個檔案可以有好幾個波次。新收一波時只勾那一波；已經收完的好幾波可以一次勾。",
        "第二回合起：上傳上一回合的固定檔（zip）。第一回合不用。",
        "看右邊：沒有【停下來】；有「要注意」時先看過。量尺表確認每個量尺的列次數、題數與校準方式。",
        "按最下面的「下一步：題目校準」。"
      ),
      output = "這一回合的資料。還沒有校準；校準在 ② 題目校準。"
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
        shiny::h4("量尺"),
        shiny::tags$p(class = "kit-hint",
                      shiny::textOutput(ns("scale_example"), inline = TRUE), "每個量尺分開校準。"),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("scales"))),
        shiny::uiOutput(ns("borrowed")),
        ui_details_row(
          ui_detail("固定檔的內容", shiny::uiOutput(ns("fixed_info"))),
          ui_detail(
            "作答設計",
            shiny::tags$p(class = "kit-hint",
                          "每格是一個波次、一題的通過比例（得分達到通過步驟以上；預設 1 分）。",
                          "空白表示沒有施測。"),
            shiny::uiOutput(ns("design_steps")),
            shiny::uiOutput(ns("design_pick")),
            plotly::plotlyOutput(ns("design"), height = "420px"),
            shiny::uiOutput(ns("design_dl"))
          ),
          ui_detail(
            "預覽",
            shiny::textOutput(ns("preview_meta")),
            shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("preview"))),
            shiny::tags$div(class = "kit-downloads", ui_download(ns("dl_data")))
          ),
          ui_check_panel(ns("check"), "kits 的檢查（作答設計選的量尺）"),
          ui_code_panel(ns("code"))
        ),
        shiny::uiOutput(ns("next_step"))
      )
    )
  )
}

# 左欄：資料來源、世代與波次、固定檔；這一回合的名稱在進階設定
data_choices_ui <- function(ns) {
  shiny::tagList(
    shiny::h5("1. 資料來源"),
    shiny::radioButtons(ns("source"), NULL, c("示範資料" = "demo", "上傳檔案" = "upload")),
    shiny::conditionalPanel(
      "input.source == 'demo'", ns = ns,
      shiny::checkboxGroupInput(ns("domains"), "領域", c("語言 lan" = "lan", "動作 motor" = "motor"),
                                "lan"),
      shiny::tags$small(
        class = "kit-hint",
        "示範資料（KIT 的 M03、M36）第一回合建議：M03 只勾 3 個月。",
        "M36 的第一回合勾 36 與 48 個月。",
        "之後每一回合加下一個波次。"
      )
    ),
    shiny::conditionalPanel(
      "input.source == 'upload'", ns = ns,
      shiny::fileInput(ns("file"), "資料檔（CSV、CSV.GZ 或 SAV，可以一次選好幾個）",
                       multiple = TRUE, accept = c(".csv", ".gz", ".csv.gz", ".sav", ".zsav"),
                       buttonLabel = "選擇檔案…", placeholder = "尚未選擇檔案"),
      shiny::tags$small(
        class = "kit-hint",
        "長格式：一列是一個列次（一個孩子的一個波次）的一題作答。",
        "大的 CSV 可以先壓縮成 .csv.gz 再上傳。",
        "好幾個檔案時，每個檔案的欄位要一樣。"
      ),
      ui_kitjoin_hint(),
      shiny::uiOutput(ns("upload_info")),
      data_columns_guide(),
      shiny::tags$div(class = "kit-downloads",
                      ui_download(ns("dl_example")))
    ),
    shiny::h5("2. 這一回合的世代與波次"),
    shiny::uiOutput(ns("pick_ui")),
    shiny::tags$div(
      class = "d-flex gap-2 mb-1",
      ui_button(ns("waves_all"), "全選"),
      ui_button(ns("waves_none"), "全不選")
    ),
    shiny::tags$small(class = "kit-hint", shiny::textOutput(ns("wave_hint"), inline = TRUE),
                      "可以一次勾好幾個。", "勾完停 1 秒才開始檢查。"),
    shiny::uiOutput(ns("unit_ui")),
    # 參照世代：第一回合選（寫進固定檔）；之後的回合讀固定檔的，只顯示
    shiny::uiOutput(ns("reference_ui")),
    shiny::h5("3. 上一回合的固定檔（選用）"),
    shiny::uiOutput(ns("fixed_choice")),
    shiny::uiOutput(ns("fixed_error")),
    shiny::tags$small(
      class = "kit-hint d-block",
      "固定檔是上一回合在 ④ 下載的 zip。",
      "有它，舊題的參數就固定不動。"
    ),
    ui_advanced(
      shiny::textInput(ns("round_label"), "這一回合的名稱", ""),
      shiny::tags$small(
        class = "kit-hint",
        "預設是 R1、R2…（第幾回合）。",
        "只能用英文字母與數字。",
        "偏移的題會改名，加上「_r名稱」。"
      ),
      title = "進階設定（這一回合的名稱）"
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
      if (is.null(f)) return(list(error = "請選擇資料檔（CSV、CSV.GZ 或 SAV）。"))
      up <- shiny::withProgress(message = "讀取資料檔…", value = 0, {
        step <- \(i, n, name) {
          shiny::setProgress(value = (i - 1) / n,
                             detail = sprintf("第 %d 個，共 %d 個：%s", i, n, name))
        }
        res <- tryCatch(kitsui_read_uploads(f, progress = step),
                        error = \(e) list(error = kit_error_text(e, "讀取資料檔")))
        shiny::setProgress(value = 1, detail = "讀好了")
        res
      })
      if (!is.null(up$error)) return(up)
      c(up, list(name = f$name, path = f$datapath))
    })
    output$upload_info <- shiny::renderUI({
      up <- upload()
      if (is.null(up$files)) return(NULL)
      ui_callout(
        shiny::tags$strong("匯入完成。"),
        sprintf("讀進 %d 個檔案，共 %s 列：", nrow(up$files), format(sum(up$files$rows),
                                                              big.mark = ",")),
        paste(sprintf("%s（%s 列）", up$files$file, format(up$files$rows, big.mark = ",",
                                                         trim = TRUE)), collapse = "、")
      )
    })
    output$dl_example_ui <- render_download_csv(
      session, "dl_example", "下載欄位範例檔（CSV）", kitsui_example_data, "kitsui_example"
    )
    is_upload <- shiny::reactive(identical(input$source, "upload"))

    # 這個來源的全部資料（還沒選世代與波次）；示範資料是兩個世代、勾選的領域
    source_df <- shiny::reactive({
      if (is_upload()) {
        up <- upload()
        if (!is.null(up$error)) return(up)
        return(list(data = up$data))
      }
      if (length(input$domains) == 0) return(list(error = "請至少勾選一個領域。"))
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
      if (nrow(ch) == 0) return(shiny::tags$p(class = "kit-hint", "先選資料。"))
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
        fixed_error(kit_error_text(fx, "讀取固定檔"))
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
      keep <- if (is.null(fr)) "現在沒有固定檔。" else {
        paste0("仍然用之前的固定檔：", fr$name %||% paste0("這次連線的第 ", from_session(), " 回合"),
               "。")
      }
      force <- if (isTRUE(fixed_modified())) {
        shiny::tags$p(ui_button(ns("fixed_force"), "仍然使用這個固定檔（我知道它被改過）", "danger"))
      }
      ui_status("stop", "固定檔讀不進來。", shiny::tags$p(kitsui_error_ui(e)), shiny::tags$p(keep),
                shiny::tags$p("怎麼辦：重新選上一回合在「④ 受試者評分」下載的 zip；",
                              "第一回合不用固定檔。"),
                force)
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
      paste0("數字是", data_wave_meaning(kitsui_wave_unit(fixed())), "。")
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
        message = "檢查資料…", detail = "整理這一回合的世代、波次、量尺與作答", value = 0.5,
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
      ui_status("stop", "資料還不能用。",
                shiny::tags$ul(lapply(p, \(x) shiny::tags$li(kitsui_error_ui(x)))),
                shiny::tags$p("怎麼辦：照上面的原因改左邊的設定，或改好資料檔再上傳。"),
                if (to_kitjoin) ui_kitjoin_hint(hint_class = "mb-0"))
    })
    output$hint <- shiny::renderUI({
      h <- checked()$hint
      if (is.null(h)) return(NULL)
      ui_callout(shiny::tags$strong(h), "左邊第 2 步，一個世代一列，數字是",
                 data_wave_meaning(kitsui_wave_unit(fixed())), "。")
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
    output$notes <- shiny::renderUI(ui_warn_list("要注意", notes()))
    # 得分不完整的題（沒有 0 分、中間少了分數、只有一種分數）
    categories <- shiny::reactive({
      df <- checked()$df
      if (is.null(df) || !exists("kitsui_category_problems", mode = "function")) return(NULL)
      tryCatch(kitsui_category_problems(df), error = \(e) NULL)
    })
    output$categories <- shiny::renderUI(data_category_note(categories()))

    output$scales <- kit_table({
      ck <- checked()
      shiny::req(ck$scales)
      kitsui_screen_names(data_scale_table(ck$scales, fixed(), plan()))
    })
    output$borrowed <- shiny::renderUI(data_borrowed_note(plan(), fixed()))

    output$next_step <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      warned <- length(notes()) > 0 || NROW(categories()) > 0
      text <- if (warned) "資料可以用，但有要注意的事，請先看過上面的「要注意」。下一步是校準。" else {
        "資料沒問題。下一步是校準。"
      }
      ui_next_step(text, ns("go_items"), "下一步：題目校準")
    })
    # 設定摘要與狀態框：資料可以用時【完成】；有要注意的事時【完成，但請注意】
    output$settings <- shiny::renderUI(ui_settings_line(kitsui_settings_text(result())))
    output$data_status <- shiny::renderUI({
      r <- result()
      if (is.null(r)) return(NULL)
      data_status_ui(r, length(notes()), NROW(categories()), fixed_rv()$integrity)
    })
    shiny::observeEvent(input$go_items, goto("items"))

    output$fixed_info <- shiny::renderUI({
      fr <- fixed_rv()
      if (is.null(fr)) {
        return(shiny::tags$p(class = "kit-hint", "沒有固定檔：這一回合每個量尺都是自由校準。"))
      }
      name <- fr$name %||% paste0("這次連線的第 ", from_session(), " 回合（還沒下載）")
      data_fixed_summary(fr$fixed, name, fr$integrity)
    })
    output$scale_example <- shiny::renderText(data_scale_example(checked()$scales))

    # 作答設計：每個量尺一張
    output$design_pick <- shiny::renderUI({
      sc <- checked()$scales
      shiny::req(sc)
      shiny::selectInput(ns("design_scale"), "量尺", sc$scale)
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
          message = "整理作答矩陣…", detail = s, value = 0.5,
          tryCatch(kitsui_resp(ck$df, s), error = \(e) e)
        )
      }
      memo[[s]]
    })
    design_plot <- shiny::reactive({
      r <- design_resp()
      shiny::validate(shiny::need(!inherits(r, "error"), paste(
        "這個量尺的作答有問題：", if (inherits(r, "error")) {
          paste0(kitsui_kits_zh(conditionMessage(r)), "（kits_resp 的訊息：", conditionMessage(r), "）")
        }
      )))
      # 通過比例照固定檔的通過步驟（示範資料、沒有固定檔時每題都是 1）
      p <- plot(kitsui_resp_pass(r, kitsui_steps_vec(fixed()$pass_steps)))
      kitsui_wave_axis(p, kitsui_wave_unit(fixed()))
    })
    output$design <- plotly::renderPlotly(design_plot())
    output$design_steps <- shiny::renderUI(data_steps_note(fixed()$pass_steps))
    # 下載鈕等圖畫好才出現；檔名跟著選的量尺（換量尺時按鈕重畫）
    output$design_dl <- shiny::renderUI({
      # 還沒有量尺（req）或 kits_resp 出錯時都是 error：不顯示
      if (inherits(tryCatch(design_resp(), error = \(e) e), "error")) return(NULL)
      shiny::tags$div(class = "kit-downloads", download_plot_ui(
        session, "dl_design", "下載作答設計圖（HTML）", design_plot,
        \() paste0("design_", input$design_scale)
      ))
    })

    output$preview_meta <- shiny::renderText({
      df <- checked()$df
      shiny::req(df)
      sprintf("這一回合共 %s 列作答，以下前 %d 列。完整的請下載。", format(nrow(df), big.mark = ","),
              KIT_PREVIEW_ROWS)
    })
    output$preview <- kit_table({
      df <- checked()$df
      shiny::req(df)
      df_for_display(df, KIT_PREVIEW_ROWS)
    })
    output$dl_data_ui <- render_download_csv(
      session, "dl_data", "下載這一回合的資料（CSV）", \() shiny::req(checked()$df),
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

    output$code <- shiny::renderText({
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

    result
  })
}

#' ① 的狀態框：資料可以用時【完成】一句（幾個量尺、幾列作答）；有要注意的事、固定檔的核對碼
#' 不符時【完成，但請注意】
#' @param n_notes 要注意的提醒有幾則；n_categories 得分不完整的題數
#' @param integrity NULL 或 kitsui_fixed_integrity()
#' @noRd
data_status_ui <- function(r, n_notes = 0, n_categories = 0, integrity = NULL) {
  line <- sprintf("資料可以用：%d 個量尺、%s 列作答。", nrow(r$scales), format(nrow(r$df), big.mark = ","))
  points <- c(
    if (!is.null(integrity) && !identical(integrity$status, "ok")) {
      paste0("固定檔的核對碼：", integrity$text)
    },
    if (n_notes > 0) sprintf("有 %d 則要注意的事，請看下面的「要注意」。", n_notes),
    if (n_categories > 0) sprintf("有 %d 題的得分不完整，② 會照規則處理。", n_categories)
  )
  if (length(points) == 0) return(ui_status("done", line))
  ui_status("warn", line, points = points)
}

#' 波次的單位：第一回合可以選（之後沿用固定檔的）
#' @noRd
data_unit_ui <- function(ns, selected = NULL) {
  shiny::tagList(
    shiny::radioButtons(ns("wave_unit"), "波次（wave_m）的單位", KITSUI_WAVE_UNITS,
                        selected %||% "months", inline = TRUE),
    shiny::tags$small(
      class = "kit-hint d-block",
      "第一回合選，寫進固定檔；之後的回合沿用，不能改。",
      "KIT 的 wave_m 是施測月齡（月）。",
      "單位是年或波次序號時，不照月齡借另一世代題庫，題目表也不算 age50（通過年齡）。"
    )
  )
}

#' 固定檔裡的波次單位（之後的回合：只顯示，不能改）
#' @noRd
data_unit_text <- function(unit) {
  name <- names(KITSUI_WAVE_UNITS)[match(unit, KITSUI_WAVE_UNITS)]
  paste0("波次（wave_m）的單位：", name, "（來自固定檔，之後的回合沿用）。",
         if (unit != "months") "不照月齡借另一世代題庫，題目表也不算 age50。")
}

#' 勾選框的數字是什麼
#' @noRd
data_wave_meaning <- function(unit) {
  switch(unit, years = "年", waves = "波次序號", "施測月齡")
}

#' 作答設計圖：固定檔裡有通過步驟不是 1 的題時，一句說明
#' @param pass_steps 固定檔的 pass_steps
#' @noRd
data_steps_note <- function(pass_steps) {
  steps <- kitsui_steps_vec(pass_steps)
  steps <- steps[steps != 1]
  if (length(steps) == 0) return(NULL)
  shiny::tags$p(class = "kit-hint", paste0(
    "固定檔裡有 ", length(steps), " 題的通過步驟不是 1：",
    kitsui_some(paste0(names(steps), "（", steps, " 分以上）")), "。"
  ), "可以在「④ 受試者評分」的題目表改。")
}

#' 得分不完整的題：黃色的提醒，說 ② 怎麼處理（分數本身不改）
#' @param problems NULL 或 kitsui_category_problems()
#' @noRd
data_category_note <- function(problems) {
  items <- kitsui_note_categories(problems)
  ui_warn_list(
    sprintf("有 %d 題的得分不完整（沒有 0 分、中間少了分數，或只有一種分數）", NROW(problems)),
    items,
    shiny::tags$p("② 校準時，題庫裡有這一題，就固定在題庫的值。",
                  "題庫裡沒有這一題，這一回合先不估它。"),
    shiny::tags$p("作答的分數不會被改動。",
                  "要平移或合併分數，可以在 kitJoin「⑥ 轉成 kits 作答檔」的題目設定改好再上傳。",
                  ui_kitjoin_button())
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
  mode <- ifelse(scales$scale %in% fixed$bank$scale, "固定校準（有題庫）", "自由校準（沒有題庫）")
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
  to <- paste(unique(b$scale), collapse = "、")
  if (nrow(b) == nrow(rows)) return(paste0("固定校準（借用另一世代題庫：", to, "）"))
  paste0("固定校準（", paste(b$wave_m, collapse = "、"), " 個月借用另一世代題庫：", to, "；",
         "其他波次用自己的題庫）")
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
      sprintf("%s 的 %s 個月：%s。", data_scale, format(wave_m), why),
      sprintf("所以用 %s 的題庫校準。", scale),
      sprintf("分數經 %s 換算。", paste(c(scale, route), collapse = " → "))
    )
  })
  ui_callout(shiny::tags$strong("借用另一世代題庫"), shiny::tags$ul(li),
             shiny::tags$p("在 ② 題目校準可以改用別的題庫。"))
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
      paste0("這份資料沒有 ", ref, "：其他世代的內部連結仍然連到題庫裡的 ", ref, "。")
    }
    return(shiny::tags$p(
      class = "kit-hint mt-2", shiny::tags$strong("參照世代：", ref),
      "（固定檔裡的。第一回合選好以後，之後的回合都一樣，不能改。）", away
    ))
  }
  if (length(cohorts) == 0) return(NULL)
  shiny::tags$div(
    class = "mt-2",
    shiny::selectInput(ns("reference"), "參照世代", cohorts, kitsui_ref_pick(pick, cohorts),
                       width = "12rem"),
    shiny::tags$small(
      class = "kit-hint",
      "其他世代的量尺都用內部連結換到參照世代的量尺；",
      "參照世代有 D 分數的錨題時，外部連結也從它開始。",
      "參照世代會寫進這一回合的固定檔，之後的回合都用同一個，不能再改。",
      "資料有 M03 時預設是 M03，沒有時是第一個世代。"
    )
  )
}

#' 量尺的例子（量尺表上面的一句話）：用這一回合的第一個量尺；還沒有時用 lan M03
#' @noRd
data_scale_example <- function(scales) {
  ex <- if (NROW(scales) > 0) scales$scale[1] else "lan M03"
  paste0("量尺 = 領域 × 世代，例如 ", ex, "。")
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
    shiny::tags$strong("固定檔：", name),
    shiny::tags$ul(
      li("之前的回合：", if (length(v$rounds)) paste(v$rounds, collapse = "、") else "（沒有）"),
      li("建立於 ", v$created, "，kits ", v$kits, "，參照世代 ", v$reference),
      if (!is.null(integrity)) {
        cls <- if (identical(integrity$status, "ok")) NULL else "kit-warn-text"
        li(shiny::tags$span(class = cls, "核對碼：", integrity$text))
      },
      if (length(integrity$ignored) > 0) {
        li(shiny::tags$span(class = "kit-warn-text", "zip 裡多出來、沒有讀的檔案：",
                            paste(integrity$ignored, collapse = "、")))
      },
      li("題庫：", if (nrow(bank) == 0) "（空的）" else paste(
        sprintf("%s %d 題 %d 步", bank$scale, bank$items, bank$steps), collapse = "；"
      )),
      li("新版本的題：", nrow(fixed$versions), " 題"),
      li("連結：", if (nrow(links) == 0) "（還沒有）" else paste(
        sprintf("%s → %s", links$from_scale, links$to_scale), collapse = "；"
      ))
    )
  )
}

#' 固定檔的選擇：上傳 zip；或 ④ 交過來的（這次連線上一回合的）固定檔
#' @noRd
data_fixed_choice_ui <- function(ns, round) {
  if (!is.null(round)) {
    return(shiny::tagList(
      ui_callout(shiny::tags$strong(paste0("上一回合的固定檔：來自這次連線的第 ", round, " 回合"))),
      ui_button(ns("fixed_clear"), "改用上傳的固定檔，或不用")
    ))
  }
  shiny::tagList(
    shiny::fileInput(ns("fixed_file"), NULL, accept = ".zip", buttonLabel = "選擇 zip…",
                     placeholder = "沒有（第一回合）"),
    ui_button(ns("fixed_clear"), "不用固定檔")
  )
}

#' 資料檔的欄位：上傳旁邊可以打開的表（這一頁只有這一張）
#' @noRd
data_columns_guide <- function() {
  shiny::tags$details(
    class = "kit-guide",
    shiny::tags$summary(shiny::tags$strong("資料檔要有哪些欄位")),
    shiny::tags$p("一個檔案可以有好幾個波次。", "也可以分成好幾個檔案一起上傳。",
                  "required 是「是」的欄一定要有。"),
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
    shiny::tags$summary(shiny::tags$strong("加一個新的波次")),
    shiny::tags$ol(
      class = "kit-steps",
      shiny::tags$li("在 ④ 下載這一回合的固定檔（zip）；或按「用這一回合的固定檔開始下一回合」，留在這次連線裡。"),
      shiny::tags$li("在這一頁勾新的世代與波次，或上傳新的資料。"),
      shiny::tags$li("到 ② 題目校準：有固定檔時，舊題的參數不動，只估新的步驟。"),
      shiny::tags$li("到 ③ 連結：做好的內部連結與外部連結已經凍結，直接沿用。"),
      shiny::tags$li("到 ④ 算新的孩子的分數。"),
      shiny::tags$li("存好這一回合的分數表與新的固定檔。")
    )
  )
}
