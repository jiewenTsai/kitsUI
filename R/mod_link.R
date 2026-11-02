# ③ 連結：內部連結（參照世代以外的每個世代 -> 參照世代，每個世代、每個領域各一條）與
# 外部連結（參照世代 -> D，用錨題；參照世代沒有錨題時不適用）
# 每條連結只做一次；做好就凍結在固定檔裡，之後每一回合沿用，早先的 D 分數不會變
# 計算（狀態、錨題數、獨立檢核、總表）在 pipeline_link.R

#' @noRd
mod_link_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    link_guide(),
    shiny::uiOutput(ns("settings")),
    shiny::uiOutput(ns("source")),
    # 1. 內部連結：標題與說明用固定檔裡的參照世代與其他世代的名稱
    shiny::uiOutput(ns("internal_head")),
    # 原文：哪一個量尺要做內部連結？
    shiny::selectInput(ns("internal_scale"), "\u54ea\u4e00\u500b\u91cf\u5c3a\u8981\u505a\u5167\u90e8\u9023\u7d50\uff1f", choices = NULL),
    shiny::uiOutput(ns("internal_overview_ui")),
    shiny::uiOutput(ns("internal_status")),
    shiny::uiOutput(ns("internal_result")),
    # 其他已經建好的內部連結：每條一列（公式），打開看圖
    shiny::uiOutput(ns("internal_list")),
    # 2. 外部連結：不適用時整段只有一行
    shiny::uiOutput(ns("d_section")),
    # 3. 垂直連結（校準以後才出現）
    shiny::uiOutput(ns("vertical_ui")),
    kitDesign::kit_details_row(
      # 原文：固定檔裡的連結
      kitDesign::kit_detail("\u56fa\u5b9a\u6a94\u88e1\u7684\u9023\u7d50",
                            # 原文：內部連結與外部連結，每條一列。
                            shiny::tags$p(class = "kit-hint", "\u5167\u90e8\u9023\u7d50\u8207\u5916\u90e8\u9023\u7d50\uff0c\u6bcf\u689d\u4e00\u5217\u3002",
                              # 原文：intercept、slope 是這條連結的係數（新的值 = intercept + slope × 舊的值）。
                              "intercept\u3001slope \u662f\u9019\u689d\u9023\u7d50\u7684\u4fc2\u6578\uff08\u65b0\u7684\u503c = intercept + slope \u00d7 \u820a\u7684\u503c\uff09\u3002"),
                            shiny::tags$div(class = "kit-table-scroll",
                                            shiny::tableOutput(ns("links_table")))),
      ui_check_panel(ns("checks")),
      kitDesign::kit_code_panel(ns("code"))
    ),
    shiny::uiOutput(ns("next_step"))
  )
}

#' 這一頁的說明
#' @noRd
link_guide <- function() {
  kitDesign::kit_tab_guide(
    # 原文：把各量尺的分數換到同一把尺上。
    goal = "\u628a\u5404\u91cf\u5c3a\u7684\u5206\u6578\u63db\u5230\u540c\u4e00\u628a\u5c3a\u4e0a\u3002",
    steps = c(
      # 原文：內部連結：選一個參照世代以外的量尺，按「建立內部連結」。每個世代、每個領域各做一條。
      "\u5167\u90e8\u9023\u7d50\uff1a\u9078\u4e00\u500b\u53c3\u7167\u4e16\u4ee3\u4ee5\u5916\u7684\u91cf\u5c3a\uff0c\u6309\u300c\u5efa\u7acb\u5167\u90e8\u9023\u7d50\u300d\u3002\u6bcf\u500b\u4e16\u4ee3\u3001\u6bcf\u500b\u9818\u57df\u5404\u505a\u4e00\u689d\u3002",
      # 原文：外部連結：錨題夠了，按「建立外部連結」。參照世代沒有 D 分數的錨題時，這一步不適用。
      "\u5916\u90e8\u9023\u7d50\uff1a\u9328\u984c\u5920\u4e86\uff0c\u6309\u300c\u5efa\u7acb\u5916\u90e8\u9023\u7d50\u300d\u3002\u53c3\u7167\u4e16\u4ee3\u6c92\u6709 D \u5206\u6578\u7684\u9328\u984c\u6642\uff0c\u9019\u4e00\u6b65\u4e0d\u9069\u7528\u3002",
      # 原文：看連結圖、連結前後的垂直連結與檢查，沒有紅字時，按「下一步：受試者評分」。
      "\u770b\u9023\u7d50\u5716\u3001\u9023\u7d50\u524d\u5f8c\u7684\u5782\u76f4\u9023\u7d50\u8207\u6aa2\u67e5\uff0c\u6c92\u6709\u7d05\u5b57\u6642\uff0c\u6309\u300c\u4e0b\u4e00\u6b65\uff1a\u53d7\u8a66\u8005\u8a55\u5206\u300d\u3002"
    ),
    output = paste0(
      # 原文：存進固定檔的連結。每條連結只做一次，做好就凍結。
      "\u5b58\u9032\u56fa\u5b9a\u6a94\u7684\u9023\u7d50\u3002\u6bcf\u689d\u9023\u7d50\u53ea\u505a\u4e00\u6b21\uff0c\u505a\u597d\u5c31\u51cd\u7d50\u3002",
      # 原文：之後每一回合都用同一條連結，所以早先算好的 D 分數永遠不會變。
      "\u4e4b\u5f8c\u6bcf\u4e00\u56de\u5408\u90fd\u7528\u540c\u4e00\u689d\u9023\u7d50\uff0c\u6240\u4ee5\u65e9\u5148\u7b97\u597d\u7684 D \u5206\u6578\u6c38\u9060\u4e0d\u6703\u8b8a\u3002"
    ),
    note = paste0(
      # 原文：內部連結把其他世代換到參照世代（在 ① 第一回合選的）的量尺；外部連結把參照世代換到 D。
      "\u5167\u90e8\u9023\u7d50\u628a\u5176\u4ed6\u4e16\u4ee3\u63db\u5230\u53c3\u7167\u4e16\u4ee3\uff08\u5728 \u2460 \u7b2c\u4e00\u56de\u5408\u9078\u7684\uff09\u7684\u91cf\u5c3a\uff1b\u5916\u90e8\u9023\u7d50\u628a\u53c3\u7167\u4e16\u4ee3\u63db\u5230 D\u3002",
      # 原文：兩種連結都用題庫裡的題目參數（kits_linking()），不是只用這一回合的資料。
      "\u5169\u7a2e\u9023\u7d50\u90fd\u7528\u984c\u5eab\u88e1\u7684\u984c\u76ee\u53c3\u6578\uff08kits_linking()\uff09\uff0c\u4e0d\u662f\u53ea\u7528\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u3002"
    )
  )
}

#' @param data mod_data_server() 的回傳值：reactive list(df, fixed, round_label, files, persons)
#' @param items mod_items_server() 的回傳值：reactive，kitsui_round() 的結果
#' @param goto function(tab) 切換分頁
#' @return reactive list(fixed, links)：fixed 是加上連結的固定檔；links 是 kitsui_links()
#'   的 kits_link 物件（具名：內部連結以起點量尺命名，外部連結叫 "D"）；還沒有題目校準時為 NULL
#' @noRd
mod_link_server <- function(id, data, items, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fixed <- shiny::reactiveVal(NULL)
    # 連結物件：一組固定檔只建一次（每條要重抽 2000 次）；新做的連結只建那一條
    link_objs <- shiny::reactiveVal(list())
    notes <- shiny::reactiveVal(list())
    check <- shiny::reactiveVal(NULL)

    # 題目校準重跑時，從它的固定檔重新開始（這一頁新做的連結要重做）
    shiny::observeEvent(items(), {
      fx <- items()$fixed
      fixed(fx)
      link_objs(if (is.null(fx)) list() else kitsui_links(fx))
      notes(list())
      check(NULL)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$go_items, goto("items"))
    shiny::observeEvent(input$go_score, goto("score"))

    links <- shiny::reactive({
      shiny::req(fixed())
      link_objs()
    })
    earlier <- shiny::reactive(kitsui_link_names(data()$fixed))
    # ① 算好的列次資訊；沒有時從資料算
    persons <- shiny::reactive(data()$persons %||% kitsui_persons(data()$df))
    # 建立一條連結：結果寫回 fixed，只重建這一條；訊息記在 notes[[name]]
    # 建好一條連結後，垂直連結圖跳到最後一個畫得出來的分頁（不留在「未連結」）
    jump_vertical <- FALSE
    make_link <- function(name, message, make) {
      shiny::withProgress(message = message, value = 0.1, {
        # 原文：用題庫的門檻估計，重抽 2000 次…
        shiny::setProgress(0.2, detail = "\u7528\u984c\u5eab\u7684\u9580\u6abb\u4f30\u8a08\uff0c\u91cd\u62bd 2000 \u6b21\u2026")
        out <- make(fixed())
        if (isTRUE(out$new)) {
          jump_vertical <<- TRUE
          # 原文：存進固定檔，從點重建這條連結…
          shiny::setProgress(0.8, detail = "\u5b58\u9032\u56fa\u5b9a\u6a94\uff0c\u5f9e\u9ede\u91cd\u5efa\u9019\u689d\u9023\u7d50\u2026")
          link_objs(c(link_objs(), kitsui_links(out$fixed, name)))
          fixed(out$fixed)
        }
      })
      notes(utils::modifyList(notes(), stats::setNames(list(out$message), name)))
    }

    output$source <- shiny::renderUI(link_source_ui(items(), ns))
    output$settings <- shiny::renderUI({
      if (is.null(items())) return(NULL)
      kitDesign::kit_settings_line(kitsui_settings_text(data()), prefix = NULL)
    })
    # 標題與說明：還沒有題目校準時用 ① 的固定檔（參照世代）
    head_fixed <- shiny::reactive(fixed() %||% tryCatch(data()$fixed, error = \(e) NULL))
    d_ok <- shiny::reactive({
      fx <- fixed()
      is.null(fx) || kitsui_d_applicable(fx, anchors())
    })
    output$internal_head <- shiny::renderUI({
      fx <- shiny::req(head_fixed())
      link_head_ui(kitsui_internal_title(fx), kitsui_internal_hint(fx))
    })
    output$d_head <- shiny::renderUI({
      fx <- shiny::req(head_fixed())
      link_head_ui(kitsui_d_title(fx, d_ok()), kitsui_d_hint(fx, d_ok()))
    })

    # 1. 內部連結 ----
    shiny::observeEvent(fixed(), {
      choices <- kitsui_internal_scales(fixed())
      sel <- if (isTRUE(input$internal_scale %in% choices)) input$internal_scale else choices[1]
      shiny::updateSelectInput(session, "internal_scale", choices = choices, selected = sel)
    })
    internal <- shiny::reactive({
      shiny::req(fixed(), input$internal_scale)
      kitsui_internal_state(fixed(), input$internal_scale, links(), earlier())
    })
    shiny::observeEvent(input$make_internal, {
      s <- shiny::req(input$internal_scale)
      # 原文：正在建立內部連結…
      make_link(s, "\u6b63\u5728\u5efa\u7acb\u5167\u90e8\u9023\u7d50\u2026", \(fx) {
        kitsui_link_internal(fx, scale_domain(s), scale_cohort(s))
      })
    })

    output$internal_status <- shiny::renderUI({
      if (is.null(fixed())) return(NULL)
      if (length(kitsui_internal_scales(fixed())) == 0) {
        # 原文：題庫裡還沒有參照世代（
        return(kitDesign::kit_callout("\u984c\u5eab\u88e1\u9084\u6c92\u6709\u53c3\u7167\u4e16\u4ee3\uff08", fixed()$version$reference,
                                      # 原文：）以外的世代的量尺，這一步不用做。
                                      "\uff09\u4ee5\u5916\u7684\u4e16\u4ee3\u7684\u91cf\u5c3a\uff0c\u9019\u4e00\u6b65\u4e0d\u7528\u505a\u3002"))
      }
      link_status_ui(internal(), notes()[[input$internal_scale]], ns("make_internal"),
                     # 原文：建立內部連結 | 內部連結
                     "\u5efa\u7acb\u5167\u90e8\u9023\u7d50", "\u5167\u90e8\u9023\u7d50")
    })

    # 好幾個量尺可以做內部連結時（例如 M06、M40 都連到 M03），列出每一條的狀態
    # 每一條的狀態收成一句話；表收在「每一條（N 條）」裡
    output$internal_overview_ui <- shiny::renderUI({
      if (is.null(fixed()) || length(kitsui_internal_scales(fixed())) < 2) return(NULL)
      ov <- kitsui_internal_overview(fixed(), links(), earlier())
      shiny::tagList(
        shiny::tags$p(class = "kit-hint", kitsui_internal_summary(fixed(), ov),
                      # 原文：每個世代、每個領域各有一條，都連到參照世代：在上面一條一條選、一條一條建立。
                      "\u6bcf\u500b\u4e16\u4ee3\u3001\u6bcf\u500b\u9818\u57df\u5404\u6709\u4e00\u689d\uff0c\u90fd\u9023\u5230\u53c3\u7167\u4e16\u4ee3\uff1a\u5728\u4e0a\u9762\u4e00\u689d\u4e00\u689d\u9078\u3001\u4e00\u689d\u4e00\u689d\u5efa\u7acb\u3002"),
        kitDesign::kit_details_row(kitDesign::kit_detail(
          # 原文：每一條（ |  條）
          paste0("\u6bcf\u4e00\u689d\uff08", kitDesign::kit_num(nrow(ov)), " \u689d\uff09"),
          shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("internal_overview")))
        ))
      )
    })
    output$internal_overview <- kit_table(
      kitsui_screen_names(kitsui_internal_overview(shiny::req(fixed()), links(), earlier()))
    )
    internal_link <- shiny::reactive(links()[[shiny::req(input$internal_scale)]])
    output$internal_result <- shiny::renderUI({
      if (is.null(internal()$link)) return(NULL)
      why <- kitsui_link_check_why(items(), internal()$link)
      link_result_ui(ns, "internal", internal()$link, internal_check_ui(ns, why))
    })
    link_outputs(output, session, "internal", internal_link,
                 shiny::reactive(kitsui_link_var(input$internal_scale)))
    # 其他已經建好的內部連結（不是上面選的那一條）：每條一個收合區塊，標題是公式
    internal_built <- shiny::reactive(setdiff(names(links()), "D"))
    made_outputs <- character()
    shiny::observe({
      for (n in setdiff(internal_built(), made_outputs)) {
        local({
          name <- n
          link_outputs(output, session, link_list_key(name), shiny::reactive(links()[[name]]),
                       shiny::reactive(kitsui_link_var(name)))
        })
        made_outputs <<- c(made_outputs, n)
      }
    })
    output$internal_list <- shiny::renderUI({
      others <- setdiff(internal_built(), input$internal_scale)
      if (length(others) == 0) return(NULL)
      link_list_ui(ns, links()[others])
    })

    # 獨立檢核（kits_link_check()）：要這一回合兩個世代的作答
    shiny::observeEvent(input$run_check, {
      link <- shiny::req(internal_link())
      # 原文：正在做獨立檢核（約需數秒）…
      out <- shiny::withProgress(message = "\u6b63\u5728\u505a\u7368\u7acb\u6aa2\u6838\uff08\u7d04\u9700\u6578\u79d2\uff09\u2026", value = 0.4, {
        tryCatch(kitsui_link_check(items(), data()$df, link, persons()), error = function(e) e)
      })
      check(list(scale = input$internal_scale, result = out))
    })
    check_now <- shiny::reactive({
      chk <- check()
      if (is.null(chk) || !identical(chk$scale, input$internal_scale)) NULL else chk$result
    })
    output$check_result <- shiny::renderUI(check_result_ui(ns, check_now()))
    output$check_table <- kit_table(kitsui_link_check_table(shiny::req(check_now())))
    output$check_print <- shiny::renderPrint(print(shiny::req(check_now())))
    output$check_dl_ui <- render_download_csv(
      # 原文：下載檢核結果（CSV）
      session, "check_dl", "\u4e0b\u8f09\u6aa2\u6838\u7d50\u679c\uff08CSV\uff09", \() check_now()$at,
      \() paste0(kitsui_link_var(input$internal_scale), "_check")
    )

    # 2. 外部連結 ----
    anchors <- shiny::reactive(kitsui_anchor_counts(shiny::req(fixed())))
    d_state <- shiny::reactive(kitsui_d_state(shiny::req(fixed()), links(), earlier(), anchors()))
    # 原文：正在建立外部連結…
    shiny::observeEvent(input$make_d, make_link("D", "\u6b63\u5728\u5efa\u7acb\u5916\u90e8\u9023\u7d50\u2026", kitsui_link_D))
    # 外部連結不適用：整段收成一行（說明再收一層）；適用時標題、狀態、結果
    output$d_section <- shiny::renderUI({
      fx <- shiny::req(head_fixed())
      if (!d_ok()) return(link_d_na_ui(fx))
      shiny::tagList(shiny::uiOutput(ns("d_head")), shiny::uiOutput(ns("d_status")),
                     shiny::uiOutput(ns("d_result")))
    })
    output$d_status <- shiny::renderUI({
      if (is.null(fixed())) return(NULL)
      # 主要按鈕一頁一個：內部連結都建立了，外部連結的按鈕才是主要按鈕
      primary <- length(tryCatch(internal_ready(), error = \(e) character())) == 0
      shiny::tagList(
        # 原文：建立外部連結 | 外部連結
        link_status_ui(d_state(), notes()[["D"]], ns("make_d"), "\u5efa\u7acb\u5916\u90e8\u9023\u7d50", "\u5916\u90e8\u9023\u7d50",
                       primary),
        if (d_ok()) link_anchor_ui(anchors(), ns("coverage"), fixed()$version$reference)
      )
    })
    output$coverage <- kit_table(kitsui_d_coverage(fixed(), anchors()))
    output$d_result <- shiny::renderUI({
      if (is.null(d_state()$link)) return(NULL)
      link_result_ui(ns, "d", d_state()$link)
    })
    link_outputs(output, session, "d", shiny::reactive(links()[["D"]]), shiny::reactive("link_D"))

    # 3. 兩個世代的垂直連結：未連結、內部連結後、外部連結後 ----
    # 分數只跟這一回合與領域有關（連結變了不重算）；換算跟連結有關（幾乎不花時間）
    v_domains <- shiny::reactive(kitsui_vertical_domains(shiny::req(items())))
    v_raw <- shiny::reactive({
      dom <- shiny::req(input$vertical_domain)
      shiny::req(dom %in% v_domains())
      # 原文：正在算這一回合的分數…
      shiny::withProgress(message = "\u6b63\u5728\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u2026", value = 0, tryCatch(
        kitsui_vertical_scores(items(), dom, persons(), vertical_progress),
        error = \(e) e
      ))
    })
    vertical <- shiny::reactive({
      dom <- shiny::req(input$vertical_domain)
      shiny::req(dom %in% v_domains(), fixed())
      raw <- v_raw()
      tryCatch({
        if (inherits(raw, "error")) stop(raw)
        kitsui_verticals(items(), data()$df, fixed(), dom, links(), persons(), raw)
      }, error = \(e) purrr::map(KITSUI_VERTICAL_STAGES, \(x) {
        # 第一層只有【停下來】與白話；R 的原文收在「R 的原文」裡
        # 原文：垂直連結圖畫不出來。
        list(why = "\u5782\u76f4\u9023\u7d50\u5716\u756b\u4e0d\u51fa\u4f86\u3002", error = TRUE, raw = conditionMessage(e))
      }))
    })
    output$vertical_ui <- shiny::renderUI({
      if (is.null(items())) return(NULL)
      shiny::tagList(
        vertical_head_ui(),
        vertical_pick_ui(ns, v_domains(), shiny::isolate(input$vertical_domain))
      )
    })
    output$vertical_body <- shiny::renderUI({
      vs <- vertical()
      pick <- shiny::isolate(input$vertical_tab)
      # 第一次畫、或剛建好一條連結：跳到最後一個畫得出來的分頁
      if (is.null(pick) || jump_vertical) pick <- vertical_last_stage(vs)
      jump_vertical <<- FALSE
      vertical_body_ui(ns, vs, pick)
    })
    purrr::walk(names(KITSUI_VERTICAL_STAGES), \(st) {
      vertical_outputs(output, session, st, vertical, \() input$vertical_domain)
    })
    # 檢查與 R 程式用的垂直連結：連結到最後的版本（有外部連結時是 D）。
    # 檢查只看這一回合兩個世代的比較（只有一個世代時不放進檢查）
    vertical_all <- shiny::reactive(tryCatch(vertical(), error = \(e) NULL))
    vertical_now <- shiny::reactive({
      v <- vertical_all()
      if (length(v$none$from) < 2) return(NULL)
      v$external$vertical %||% v$internal$vertical
    })

    # 總表、檢查、R 程式、下一步 ----
    output$links_table <- kit_table(
      kitsui_screen_names(kitsui_link_table(shiny::req(fixed()), links(), earlier()))
    )
    output$checks <- kit_table({
      chk <- if (!inherits(check_now(), "error")) check_now()
      more <- list(link_check = chk, vertical = vertical_now())
      checks_or_ok(do.call(check_table, c(links(), more))) |>
        kitsui_check_explain()
    })
    code_text <- shiny::reactive({
      d <- data()
      file <- \(n) kitsui_link_file(n, earlier(), d$fixed_file, score_round_label(d, fixed()))
      code <- link_code(shiny::req(fixed()), file)
      v <- vertical_all()
      drawn <- names(Filter(\(x) !is.null(x$vertical), v))
      if (length(drawn) == 0) return(code)
      earlier <- setdiff(v$none$scales, v$none$from)
      zip <- kitsui_fixed_name(score_round_label(d, fixed()))
      more <- if (any(v$none$table$source == "earlier")) {
        kitsui_code_vertical_earlier(fixed(), v$none$scales, utils::tail(drawn, 1), zip)
      }
      paste(c(code, kitsui_code_vertical(fixed(), v$none$from, drawn), more), collapse = "\n\n")
    })
    # 用 R 怎麼做：打開時才算；每行最多 70 欄（kitDesign::kit_code()）
    output$code <- kitDesign::kit_render_code_panel(
      # 原文：kits 版 | 先做「② 題目校準」，這裡才會有程式碼。
      \() list("kits \u7248" = code_text()), empty = "\u5148\u505a\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\uff0c\u9019\u88e1\u624d\u6703\u6709\u7a0b\u5f0f\u78bc\u3002"
    )
    # 還可以建立的內部連結
    internal_ready <- shiny::reactive({
      fx <- shiny::req(fixed())
      scales <- kitsui_internal_scales(fx)
      ready <- purrr::map_chr(scales, \(s) kitsui_internal_state(fx, s, links(), earlier())$state)
      scales[ready == "ready"]
    })
    output$next_step <- shiny::renderUI({
      if (is.null(fixed())) return(NULL)
      kitDesign::kit_next_step(link_next_text(fixed(), internal_ready(), d_state()$state),
                               # 原文：下一步：受試者評分
                               ns("go_score"), "\u4e0b\u4e00\u6b65\uff1a\u53d7\u8a66\u8005\u8a55\u5206")
    })

    # todo：還可以建立的連結（步驟分頁的 ✓：都建立了才算做完）
    shiny::reactive({
      if (is.null(fixed())) return(NULL)
      todo <- tryCatch(c(internal_ready(), if (identical(d_state()$state, "ready")) "D"),
                       error = \(e) character())
      list(fixed = fixed(), links = links(), todo = todo)
    })
  })
}

# 每個世代開始算分數前更新進度條（kitsui_vertical_scores() 的 progress）
vertical_progress <- function(i, n, scale) {
  # 原文：%s（第 %d 個，共 %d 個世代）
  shiny::setProgress((i - 1) / n, detail = sprintf("%s\uff08\u7b2c %d \u500b\uff0c\u5171 %d \u500b\u4e16\u4ee3\uff09", scale, i, n))
}

#' 下一步的一句話：還沒有連結時不說「做好了」
#' @param ready 還可以建立的內部連結；d 是外部連結的狀態（kitsui_d_state()$state）
#' @noRd
link_next_text <- function(fixed, ready, d) {
  n <- length(kitsui_link_names(fixed))
  # 原文：外部連結（到 D）
  todo <- c(ready, if (identical(d, "ready")) "\u5916\u90e8\u9023\u7d50\uff08\u5230 D\uff09")
  left <- if (length(todo) > 0) {
    # 原文：還可以建立： | 、 | 。不做也可以先評分。
    paste0("\u9084\u53ef\u4ee5\u5efa\u7acb\uff1a", paste(todo, collapse = "\u3001"), "\u3002\u4e0d\u505a\u4e5f\u53ef\u4ee5\u5148\u8a55\u5206\u3002")
  }
  na <- if (identical(d, "na")) {
    # 原文：外部連結不適用，分數停在參照世代（ | ）的量尺上。
    paste0("\u5916\u90e8\u9023\u7d50\u4e0d\u9069\u7528\uff0c\u5206\u6578\u505c\u5728\u53c3\u7167\u4e16\u4ee3\uff08", fixed$version$reference, "\uff09\u7684\u91cf\u5c3a\u4e0a\u3002")
  }
  if (n == 0) {
    # 原文：還沒有連結，分數會留在各自校準的量尺上。
    return(paste0("\u9084\u6c92\u6709\u9023\u7d50\uff0c\u5206\u6578\u6703\u7559\u5728\u5404\u81ea\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\u3002", na,
                  # 原文：這一回合不用做連結。 | 下一步是評分。
                  left %||% "\u9019\u4e00\u56de\u5408\u4e0d\u7528\u505a\u9023\u7d50\u3002", "\u4e0b\u4e00\u6b65\u662f\u8a55\u5206\u3002"))
  }
  # 原文：固定檔裡有  |  條連結。 | 下一步是評分。
  paste0("\u56fa\u5b9a\u6a94\u88e1\u6709 ", n, " \u689d\u9023\u7d50\u3002", na, left %||% "", "\u4e0b\u4e00\u6b65\u662f\u8a55\u5206\u3002")
}

#' 一條連結的輸出：圖、下載圖與點（key 是 "internal" 或 "d"）
#' @param link reactive：kits_link；name reactive：下載的檔名（畫按鈕時讀，變了按鈕重畫）
#' @noRd
link_outputs <- function(output, session, key, link, name) {
  out <- \(x) paste0(key, "_", x)
  # 原文：外部連結 | 內部連結
  kind <- if (key == "d") "\u5916\u90e8\u9023\u7d50" else "\u5167\u90e8\u9023\u7d50"
  # 畫面與下載的 HTML 是同一張圖：kits 的 plot()，套上 KIT 的樣子
  p <- shiny::reactive(kitDesign::kit_plotly_style(plot(shiny::req(link()))))
  output[[out("plot")]] <- plotly::renderPlotly(p())
  output[[out("plot_dl_ui")]] <- render_download_plot(
    # 原文：下載 | 圖（HTML）
    session, out("plot_dl"), paste0("\u4e0b\u8f09", kind, "\u5716\uff08HTML\uff09"), p, name
  )
  output[[out("points_dl_ui")]] <- render_download_csv(
    # 原文：下載 | 的點（CSV）
    session, out("points_dl"), paste0("\u4e0b\u8f09", kind, "\u7684\u9ede\uff08CSV\uff09"), \() link()$points,
    \() paste0(name(), "_points")
  )
  output[[out("checks")]] <- kit_table({
    # object 欄寫連結的名稱（不是 object 1）
    objs <- stats::setNames(list(shiny::req(link())), name())
    checks_or_ok(do.call(check_table, objs)) |>
      kitsui_check_explain()
  })
}

#' 一個版本的垂直連結：圖與下載（stage 是 KITSUI_VERTICAL_STAGES 的名稱）
#' @param vertical reactive：kitsui_verticals()；domain：下載檔名用的領域（變了按鈕重畫）
#' @noRd
vertical_outputs <- function(output, session, stage, vertical, domain) {
  force(stage)
  id <- \(x) paste0("vertical_", stage, "_", x)
  p <- shiny::reactive({
    v <- vertical()[[stage]]
    shiny::req(v$vertical)
    # 世代依序取 KIT 的類別色（三個分頁同一個世代同一個顏色）
    kitDesign::kit_plotly_style(kit_chart_categorical(kitsui_vertical_plot(v)))
  })
  output[[id("plot")]] <- plotly::renderPlotly(p())
  output[[id("dl_ui")]] <- render_download_plot(
    # 原文：下載這張垂直連結圖（HTML）
    session, id("dl"), "\u4e0b\u8f09\u9019\u5f35\u5782\u76f4\u9023\u7d50\u5716\uff08HTML\uff09", p,
    \() paste0("vertical_", domain(), "_", stage)
  )
}

#' 重建固定檔裡所有連結的 R 程式
#' @param file function(name)：存著這條連結的固定檔檔名（kitsui_link_file()）
#' @noRd
link_code <- function(fixed, file = \(n) "fixed_set.zip") {
  nm <- kitsui_link_names(fixed)
  # 原文：# 還沒有連結。做好連結後，這裡會出現重建它的 R 程式。
  if (length(nm) == 0) return("# \u9084\u6c92\u6709\u9023\u7d50\u3002\u505a\u597d\u9023\u7d50\u5f8c\uff0c\u9019\u88e1\u6703\u51fa\u73fe\u91cd\u5efa\u5b83\u7684 R \u7a0b\u5f0f\u3002")
  code <- purrr::map_chr(nm, \(n) kitsui_code_link(fixed, n, file(n)))
  paste(c("library(kits)", "library(dplyr)", code), collapse = "\n\n")
}

# 畫面小元件 ----

#' 一段連結的標題與說明（kitsui_internal_title()、kitsui_d_title() 等）
#' @noRd
link_head_ui <- function(title, hint) {
  shiny::tagList(shiny::h4(title), shiny::tags$small(class = "kit-hint", hint))
}

#' 還沒有題目校準時的提示；這一回合有量尺停下來時的提醒
#' @noRd
link_source_ui <- function(round, ns) {
  if (is.null(round)) return(ui_go_items(ns))
  if (isTRUE(round$ok)) return(NULL)
  kitDesign::kit_callout(
    # 原文：這一回合有量尺停下來（ | 、 | ）。
    "\u9019\u4e00\u56de\u5408\u6709\u91cf\u5c3a\u505c\u4e0b\u4f86\uff08", paste(round$stopped, collapse = "\u3001"), "\uff09\u3002",
    # 原文：連結仍可以先看，但這一回合不會產生新的固定檔。
    "\u9023\u7d50\u4ecd\u53ef\u4ee5\u5148\u770b\uff0c\u4f46\u9019\u4e00\u56de\u5408\u4e0d\u6703\u7522\u751f\u65b0\u7684\u56fa\u5b9a\u6a94\u3002",
    # 原文：可以在 ② 把它們改成「這一回合不做」再校準一次。
    "\u53ef\u4ee5\u5728 \u2461 \u628a\u5b83\u5011\u6539\u6210\u300c\u9019\u4e00\u56de\u5408\u4e0d\u505a\u300d\u518d\u6821\u6e96\u4e00\u6b21\u3002",
    warn = TRUE
  )
}

#' 連結的狀態：可以做（按鈕與「做了就凍結」）、還不能做（原因）、不適用（不是錯誤）、已凍結；
#' 這次連線按了按鈕以後是狀態框：建立好了是【完成】，沒有建立是【停下來】（原因與怎麼辦）
#' @param note 這次連線按了按鈕以後的訊息（沒有按過時 NULL）
#' @param kind 「內部連結」或「外部連結」
#' @param primary 按鈕是這一頁的主要按鈕嗎（不是時用次要按鈕）
#' @noRd
# 原文：連結
link_status_ui <- function(st, note, button_id, button_label, kind = "\u9023\u7d50", primary = TRUE) {
  tried <- !is.null(note)
  shiny::tagList(
    if (tried && st$state == "frozen") {
      # 原文：建立好了，已凍結在固定檔裡。
      kitDesign::kit_status_ui(kitDesign::kit_status("done", paste0(kind, "\u5efa\u7acb\u597d\u4e86\uff0c\u5df2\u51cd\u7d50\u5728\u56fa\u5b9a\u6a94\u88e1\u3002")),
                shiny::tags$p(kitsui_error_ui(st$text)))
    } else {
      # kits 的英文原文收起來（kitsui_error_ui()）
      kitDesign::kit_callout(kitsui_error_ui(st$text), warn = st$state == "missing")
    },
    if (tried && st$state != "frozen") {
      kitDesign::kit_status_ui(
        # 原文：沒有建立。
        kitDesign::kit_status("stop", paste0(kind, "\u6c92\u6709\u5efa\u7acb\u3002")),
        shiny::tags$p(kitsui_error_ui(note)),
        # 原文：怎麼辦：看上面的原因；題庫缺的題要在之後的回合校準，再回來建立。
        shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u770b\u4e0a\u9762\u7684\u539f\u56e0\uff1b\u984c\u5eab\u7f3a\u7684\u984c\u8981\u5728\u4e4b\u5f8c\u7684\u56de\u5408\u6821\u6e96\uff0c\u518d\u56de\u4f86\u5efa\u7acb\u3002")
      )
    },
    if (st$state == "ready") {
      shiny::tags$div(
        class = "kit-downloads",
        kitDesign::kit_button(button_id, button_label, if (primary) "primary" else "secondary"),
        # 原文：連結一做好就凍結在固定檔裡，不能復原，也不會重做。
        shiny::tags$small(class = "kit-hint", "\u9023\u7d50\u4e00\u505a\u597d\u5c31\u51cd\u7d50\u5728\u56fa\u5b9a\u6a94\u88e1\uff0c\u4e0d\u80fd\u5fa9\u539f\uff0c\u4e5f\u4e0d\u6703\u91cd\u505a\u3002")
      )
    }
  )
}

#' 外部連結不適用：整段只有一行，說明收起來
#' @noRd
link_d_na_ui <- function(fixed) {
  ref <- kitsui_link_cohorts(fixed)$ref
  shiny::tagList(
    shiny::h4(kitsui_d_title(fixed, FALSE)),
    kitDesign::kit_callout(
      # 原文：外部連結不適用：
      shiny::tags$strong("\u5916\u90e8\u9023\u7d50\u4e0d\u9069\u7528\uff1a"),
      # 原文：參照世代（ | ）沒有 D 分數的錨題，分數留在  |  的量尺上。這不是錯誤。
      paste0("\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u6c92\u6709 D \u5206\u6578\u7684\u9328\u984c\uff0c\u5206\u6578\u7559\u5728 ", ref, " \u7684\u91cf\u5c3a\u4e0a\u3002\u9019\u4e0d\u662f\u932f\u8aa4\u3002"),
      # 原文：為什麼
      shiny::tags$details(shiny::tags$summary("\u70ba\u4ec0\u9ebc"), kitsui_d_hint(fixed, FALSE))
    )
  )
}

#' 垂直連結的標題與說明
#' @noRd
vertical_head_ui <- function() {
  shiny::tagList(
    # 原文：3. 各世代的垂直連結：連結前後
    shiny::h4("3. \u5404\u4e16\u4ee3\u7684\u5782\u76f4\u9023\u7d50\uff1a\u9023\u7d50\u524d\u5f8c"),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：三個分頁：未連結、內部連結後、外部連結後。
      "\u4e09\u500b\u5206\u9801\uff1a\u672a\u9023\u7d50\u3001\u5167\u90e8\u9023\u7d50\u5f8c\u3001\u5916\u90e8\u9023\u7d50\u5f8c\u3002",
      # 原文：每個波次畫中位數，線段是 P25 到 P75（kits_vertical()）。
      "\u6bcf\u500b\u6ce2\u6b21\u756b\u4e2d\u4f4d\u6578\uff0c\u7dda\u6bb5\u662f P25 \u5230 P75\uff08kits_vertical()\uff09\u3002",
      # 原文：波次的單位是月時，連結後同一個月齡各世代的點靠在一起，表示連結合理。
      "\u6ce2\u6b21\u7684\u55ae\u4f4d\u662f\u6708\u6642\uff0c\u9023\u7d50\u5f8c\u540c\u4e00\u500b\u6708\u9f61\u5404\u4e16\u4ee3\u7684\u9ede\u9760\u5728\u4e00\u8d77\uff0c\u8868\u793a\u9023\u7d50\u5408\u7406\u3002",
      # 原文：單位是年或波次序號時，同一波各世代的年齡不同，點不一定會靠在一起。
      "\u55ae\u4f4d\u662f\u5e74\u6216\u6ce2\u6b21\u5e8f\u865f\u6642\uff0c\u540c\u4e00\u6ce2\u5404\u4e16\u4ee3\u7684\u5e74\u9f61\u4e0d\u540c\uff0c\u9ede\u4e0d\u4e00\u5b9a\u6703\u9760\u5728\u4e00\u8d77\u3002",
      # 原文：之前回合的點來自固定檔裡的各波次摘要，照現在的連結換算。
      "\u4e4b\u524d\u56de\u5408\u7684\u9ede\u4f86\u81ea\u56fa\u5b9a\u6a94\u88e1\u7684\u5404\u6ce2\u6b21\u6458\u8981\uff0c\u7167\u73fe\u5728\u7684\u9023\u7d50\u63db\u7b97\u3002",
      # 原文：所以這一回合只有一個世代時，也看得到連結得怎樣。
      "\u6240\u4ee5\u9019\u4e00\u56de\u5408\u53ea\u6709\u4e00\u500b\u4e16\u4ee3\u6642\uff0c\u4e5f\u770b\u5f97\u5230\u9023\u7d50\u5f97\u600e\u6a23\u3002",
      # 原文：只有一個世代、也沒有之前回合的點時，只畫未連結的圖。
      "\u53ea\u6709\u4e00\u500b\u4e16\u4ee3\u3001\u4e5f\u6c92\u6709\u4e4b\u524d\u56de\u5408\u7684\u9ede\u6642\uff0c\u53ea\u756b\u672a\u9023\u7d50\u7684\u5716\u3002"
    )
  )
}

#' 一條連結的結果：公式、檢查、圖、下載（key 是 "internal"（內部連結）或 "d"（外部連結））
#'
#' 公式與係數直接寫在畫面上（不用等另一個輸出），圖還沒畫好時有一句「正在畫」。
#' @noRd
link_result_ui <- function(ns, key, link, extra = NULL) {
  out <- \(x) ns(paste0(key, "_", x))
  # 原文：外部連結 | 內部連結
  kind <- if (key == "d") "\u5916\u90e8\u9023\u7d50" else "\u5167\u90e8\u9023\u7d50"
  if (startsWith(key, "il_")) {
    # 其他已經建好的連結（收合區塊裡）：公式在標題上，這裡只放係數、圖與下載
    return(shiny::tagList(
      link_coef_table(kitsui_link_formula(link)$table),
      # 原文：正在畫連結圖…
      link_plot_box(out("plot"), "\u6b63\u5728\u756b\u9023\u7d50\u5716\u2026"),
      shiny::tags$div(class = "kit-downloads", ui_download(out("plot_dl")),
                      ui_download(out("points_dl"))),
      # 原文：的檢查（kits_check）
      kitDesign::kit_details_row(kitDesign::kit_detail(paste0(kind, "\u7684\u6aa2\u67e5\uff08kits_check\uff09"),
                               shiny::tags$div(class = "kit-table-scroll",
                                               shiny::tableOutput(out("checks")))))
    ))
  }
  f <- kitsui_link_formula(link)
  shiny::tagList(
    # 原文：的公式
    shiny::h5(paste0(kind, "\u7684\u516c\u5f0f")),
    shiny::tags$p(shiny::tags$strong(f$formula)),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：新的值 = intercept + slope × 舊的值。 | linking_error 是連結本身的不確定性。
      "\u65b0\u7684\u503c = intercept + slope \u00d7 \u820a\u7684\u503c\u3002", "linking_error \u662f\u9023\u7d50\u672c\u8eab\u7684\u4e0d\u78ba\u5b9a\u6027\u3002"
    ),
    link_coef_table(f$table),
    # 原文：圖
    shiny::h5(paste0(kind, "\u5716")),
    shiny::tags$small(class = "kit-hint",
                      # 原文：每個點是一個共同的題目步驟。 | 點越貼近直線，連結越可靠。
                      "\u6bcf\u500b\u9ede\u662f\u4e00\u500b\u5171\u540c\u7684\u984c\u76ee\u6b65\u9a5f\u3002", "\u9ede\u8d8a\u8cbc\u8fd1\u76f4\u7dda\uff0c\u9023\u7d50\u8d8a\u53ef\u9760\u3002",
                      # 原文：圖例：Kept 是留下的點，Dropped 是拿掉的點。
                      "\u5716\u4f8b\uff1aKept \u662f\u7559\u4e0b\u7684\u9ede\uff0cDropped \u662f\u62ff\u6389\u7684\u9ede\u3002"),
    # 原文：正在畫連結圖…
    link_plot_box(out("plot"), "\u6b63\u5728\u756b\u9023\u7d50\u5716\u2026"),
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(out("plot_dl")),
      ui_download(out("points_dl"))
    ),
    kitDesign::kit_details_row(
      # 原文：的檢查（kits_check）
      kitDesign::kit_detail(paste0(kind, "\u7684\u6aa2\u67e5\uff08kits_check\uff09"),
                            shiny::tags$div(class = "kit-table-scroll",
                                            shiny::tableOutput(out("checks")))),
      extra
    )
  )
}

#' 一條連結的輸出用的 key（id 的一部分）：量尺名稱的 16 進位，換了選單也對得到同一條
#' @noRd
link_list_key <- function(name) {
  paste0("il_", paste(as.character(charToRaw(enc2utf8(name))), collapse = ""))
}

#' 其他已經建好的內部連結：每條一個收合區塊，標題是起點 → 終點與公式；打開看圖與下載
#' @param links 具名 list：kits_link（內部連結，以起點量尺命名）
#' @noRd
link_list_ui <- function(ns, links) {
  blocks <- purrr::imap(links, \(link, name) {
    f <- kitsui_link_formula(link)
    kitDesign::kit_detail(
      # 原文： →  | ：
      paste0(name, " \u2192 ", link$to_scale, "\uff1a", f$formula),
      link_result_ui(ns, link_list_key(name), link)
    )
  })
  shiny::tagList(
    # 原文：其他已經建好的內部連結（ |  條）
    shiny::h5(paste0("\u5176\u4ed6\u5df2\u7d93\u5efa\u597d\u7684\u5167\u90e8\u9023\u7d50\uff08", kitDesign::kit_num(length(links)), " \u689d\uff09")),
    # 原文：每條一列；打開看係數、連結圖與檢查。
    shiny::tags$small(class = "kit-hint", "\u6bcf\u689d\u4e00\u5217\uff1b\u6253\u958b\u770b\u4fc2\u6578\u3001\u9023\u7d50\u5716\u8207\u6aa2\u67e5\u3002"),
    kitDesign::kit_details_row(unname(blocks))
  )
}

#' 係數表（term、estimate、linking_error）直接寫成 HTML 表，小數 2 位
#' @noRd
link_coef_table <- function(tab) {
  cell <- \(x) if (is.numeric(x)) sprintf("%.2f", x) else x
  shiny::tags$table(
    class = "table table-sm table-striped w-auto",
    shiny::tags$thead(shiny::tags$tr(lapply(names(tab), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(tab)), \(i) {
      shiny::tags$tr(lapply(tab[i, ], \(x) shiny::tags$td(cell(x[[1]]))))
    }))
  )
}

#' 圖的位置：圖還沒畫好時，底下先顯示一句「正在畫」（畫好後被圖蓋住）
#' @noRd
link_plot_box <- function(id, wait, height = "420px") {
  shiny::tags$div(
    style = "position: relative;",
    shiny::tags$div(
      class = "kit-hint",
      style = "position: absolute; top: 40%; width: 100%; text-align: center;",
      wait
    ),
    shiny::tags$div(
      style = "position: relative; z-index: 1;",
      plotly::plotlyOutput(id, height = height)
    )
  )
}

#' 內部連結的獨立檢核：能做時給按鈕，不能做時說明原因（不是每次都要做，預設收起來）
#' @noRd
internal_check_ui <- function(ns, why) {
  kitDesign::kit_detail(
    # 原文：內部連結的獨立檢核（kits_link_check）
    "\u5167\u90e8\u9023\u7d50\u7684\u7368\u7acb\u6aa2\u6838\uff08kits_link_check\uff09",
    shiny::tags$small(
      class = "kit-hint",
      # 原文：用不靠共同題目的兩種方法再算一次：同年齡組比較、兩個世代一起校準。
      "\u7528\u4e0d\u9760\u5171\u540c\u984c\u76ee\u7684\u5169\u7a2e\u65b9\u6cd5\u518d\u7b97\u4e00\u6b21\uff1a\u540c\u5e74\u9f61\u7d44\u6bd4\u8f03\u3001\u5169\u500b\u4e16\u4ee3\u4e00\u8d77\u6821\u6e96\u3002",
      # 原文：結果和連結差不多，表示連結可信。
      "\u7d50\u679c\u548c\u9023\u7d50\u5dee\u4e0d\u591a\uff0c\u8868\u793a\u9023\u7d50\u53ef\u4fe1\u3002"
    ),
    if (is.null(why)) {
      shiny::tagList(
        # 原文：做獨立檢核
        kitDesign::kit_button(ns("run_check"), "\u505a\u7368\u7acb\u6aa2\u6838"),
        shiny::uiOutput(ns("check_result"))
      )
    } else {
      kitDesign::kit_callout(why)
    }
  )
}

#' 獨立檢核的結果：小表（方法、和連結的差距、一致不一致與規則），原始輸出收起來
#' @noRd
check_result_ui <- function(ns, chk) {
  if (is.null(chk)) return(NULL)
  if (inherits(chk, "error")) {
    # 原文：獨立檢核停下來了，沒有結果。
    return(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u7368\u7acb\u6aa2\u6838\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u7d50\u679c\u3002"),
                     # 原文：獨立檢核
                     shiny::tags$p(kitsui_error_ui(kit_error_text(chk, "\u7368\u7acb\u6aa2\u6838")))))
  }
  tab <- kitsui_link_check_table(chk)
  shiny::tagList(
    shiny::tags$small(
      class = "kit-hint kit-tab-caption",
      # 原文：max_diff 是在幾個能力值上，這個方法和連結換算結果的最大差距（目標量尺的單位）。
      "max_diff \u662f\u5728\u5e7e\u500b\u80fd\u529b\u503c\u4e0a\uff0c\u9019\u500b\u65b9\u6cd5\u548c\u9023\u7d50\u63db\u7b97\u7d50\u679c\u7684\u6700\u5927\u5dee\u8ddd\uff08\u76ee\u6a19\u91cf\u5c3a\u7684\u55ae\u4f4d\uff09\u3002",
      # 原文：max_z 是這個差距除以標準誤。
      if ("max_z" %in% names(tab)) "max_z \u662f\u9019\u500b\u5dee\u8ddd\u9664\u4ee5\u6a19\u6e96\u8aa4\u3002",
      attr(tab, "rule")
    ),
    shiny::tableOutput(ns("check_table")),
    shiny::tags$details(
      # 原文：kits 的完整輸出
      shiny::tags$summary("kits \u7684\u5b8c\u6574\u8f38\u51fa"),
      shiny::verbatimTextOutput(ns("check_print"))
    ),
    shiny::tags$div(class = "kit-downloads",
                    ui_download(ns("check_dl")))
  )
}

#' 參照世代各量尺：錨題數與外部連結涵蓋的方式（收起來的細節）
#' @param ref 參照世代
#' @noRd
link_anchor_ui <- function(anchors, table_id, ref = KITSUI_REF_COHORT) {
  if (nrow(anchors) == 0) return(NULL)
  kitDesign::kit_details_row(kitDesign::kit_detail(
    # 原文：錨題數與外部連結涵蓋的量尺
    "\u9328\u984c\u6578\u8207\u5916\u90e8\u9023\u7d50\u6db5\u84cb\u7684\u91cf\u5c3a",
    # 原文：參照世代各量尺在題庫裡的錨題數（kits_anchors()），與外部連結涵蓋哪些量尺：
    shiny::tags$p("\u53c3\u7167\u4e16\u4ee3\u5404\u91cf\u5c3a\u5728\u984c\u5eab\u88e1\u7684\u9328\u984c\u6578\uff08kits_anchors()\uff09\uff0c\u8207\u5916\u90e8\u9023\u7d50\u6db5\u84cb\u54ea\u4e9b\u91cf\u5c3a\uff1a"),
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(table_id)),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：外部連結是各領域共用的一條線。
      "\u5916\u90e8\u9023\u7d50\u662f\u5404\u9818\u57df\u5171\u7528\u7684\u4e00\u689d\u7dda\u3002",
      # 原文：沒有錨題、或之後才進題庫的  |  量尺，沿用同一條外部連結，不重新估計。
      paste0("\u6c92\u6709\u9328\u984c\u3001\u6216\u4e4b\u5f8c\u624d\u9032\u984c\u5eab\u7684 ", ref, " \u91cf\u5c3a\uff0c\u6cbf\u7528\u540c\u4e00\u689d\u5916\u90e8\u9023\u7d50\uff0c\u4e0d\u91cd\u65b0\u4f30\u8a08\u3002"),
      # 原文：之後各回合題庫裡的錨題數可能變多。
      "\u4e4b\u5f8c\u5404\u56de\u5408\u984c\u5eab\u88e1\u7684\u9328\u984c\u6578\u53ef\u80fd\u8b8a\u591a\u3002",
      # 原文：但外部連結凍結後，不會用新的錨題重新估計。
      "\u4f46\u5916\u90e8\u9023\u7d50\u51cd\u7d50\u5f8c\uff0c\u4e0d\u6703\u7528\u65b0\u7684\u9328\u984c\u91cd\u65b0\u4f30\u8a08\u3002"
    )
  ))
}

#' 垂直連結：選領域；沒有校準好的領域時，一句話說明
#' @noRd
vertical_pick_ui <- function(ns, domains, pick, why = NULL) {
  if (length(domains) == 0) {
    # 原文：這一回合沒有校準好的領域，所以沒有垂直連結圖。
    return(kitDesign::kit_callout(why %||% "\u9019\u4e00\u56de\u5408\u6c92\u6709\u6821\u6e96\u597d\u7684\u9818\u57df\uff0c\u6240\u4ee5\u6c92\u6709\u5782\u76f4\u9023\u7d50\u5716\u3002"))
  }
  if (!isTRUE(pick %in% domains)) pick <- domains[1]
  shiny::tagList(
    # 原文：哪一個領域？
    shiny::selectInput(ns("vertical_domain"), "\u54ea\u4e00\u500b\u9818\u57df\uff1f", domains, pick),
    shiny::uiOutput(ns("vertical_body"))
  )
}

#' 垂直連結本身：三個分頁（未連結、內部連結後、外部連結後）
#'
#' 只有一個世代、也沒有之前回合的點時（vs$none$alone），連結後的兩頁永遠畫不了：
#' 不放分頁，一句說明加上未連結的圖。
#' @param vs kitsui_verticals() 的結果；pick 是原本選的分頁（重畫時留在同一頁）
#' @noRd
vertical_body_ui <- function(ns, vs, pick = NULL) {
  if (!is.null(vs$none$alone)) {
    return(shiny::tagList(kitDesign::kit_callout(vs$none$alone),
                          vertical_tab_ui(ns, "none", vs$none)))
  }
  tabs <- purrr::imap(KITSUI_VERTICAL_STAGES, \(label, st) {
    shiny::tabPanel(label, value = st, vertical_tab_ui(ns, st, vs[[st]]))
  })
  do.call(shiny::tabsetPanel, c(list(id = ns("vertical_tab"), selected = pick), unname(tabs)))
}

#' 最後一個畫得出來的分頁（外部連結後 > 內部連結後 > 未連結）
#' @param vs kitsui_verticals() 的結果
#' @noRd
vertical_last_stage <- function(vs) {
  ok <- purrr::map_lgl(names(KITSUI_VERTICAL_STAGES), \(st) !is.null(vs[[st]]$vertical))
  if (!any(ok)) return(names(KITSUI_VERTICAL_STAGES)[1])
  utils::tail(names(KITSUI_VERTICAL_STAGES)[ok], 1)
}

#' 一個分頁：畫不了時一句原因；畫得了時一句說明、圖、沒有共同月齡時的說明與下載
#' @noRd
vertical_tab_ui <- function(ns, stage, v) {
  if (isTRUE(v$error)) {
    return(kitDesign::kit_status_ui(kitDesign::kit_status(
      "stop", v$why,
      # 原文：怎麼辦：連結本身不受影響，可以照常到「④ 受試者評分」。
      action = shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u9023\u7d50\u672c\u8eab\u4e0d\u53d7\u5f71\u97ff\uff0c\u53ef\u4ee5\u7167\u5e38\u5230\u300c\u2463 \u53d7\u8a66\u8005\u8a55\u5206\u300d\u3002",
                             # 原文：請把這個領域與下面的原文告訴維護 kitsUI 的人。
                             "\u8acb\u628a\u9019\u500b\u9818\u57df\u8207\u4e0b\u9762\u7684\u539f\u6587\u544a\u8a34\u7dad\u8b77 kitsUI \u7684\u4eba\u3002"),
      # 原文：R 的原文
      details = v$raw[1], details_title = "R \u7684\u539f\u6587"
    )))
  }
  # 外部連結不適用（na）不是錯誤，不用橘色
  if (!is.null(v$why)) return(kitDesign::kit_callout(v$why, warn = !isTRUE(v$na)))
  id <- \(x) ns(paste0("vertical_", stage, "_", x))
  shiny::tagList(
    shiny::tags$p(class = "kit-hint kit-tab-caption", vertical_caption(stage, v)),
    # 原文：正在畫垂直連結圖…
    link_plot_box(id("plot"), "\u6b63\u5728\u756b\u5782\u76f4\u9023\u7d50\u5716\u2026", "460px"),
    if (!is.null(v$note)) kitDesign::kit_callout(v$note, warn = TRUE),
    if (!is.null(kitsui_vertical_dropped_note(v$vertical, v$wave_unit %||% "months"))) {
      shiny::tags$p(class = "kit-hint",
                    kitsui_vertical_dropped_note(v$vertical, v$wave_unit %||% "months"))
    },
    shiny::tags$div(class = "kit-downloads",
                    ui_download(id("dl")))
  )
}

#' 每個版本一句說明；有之前回合的點時，再說哪些點是哪一回合
#' @noRd
vertical_caption <- function(stage, v) {
  scales <- v$scales %||% v$from
  unit <- v$wave_unit %||% "months"
  text <- switch(
    stage,
    none = paste0(if (length(scales) == 1) {
                    # 原文：未連結：一個世代，在自己校準的量尺上（ | ）。
                    paste0("\u672a\u9023\u7d50\uff1a\u4e00\u500b\u4e16\u4ee3\uff0c\u5728\u81ea\u5df1\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\uff08", scales, "\uff09\u3002")
                  } else {
                    # 原文：未連結：各世代在自己校準的量尺上（ | 、
                    paste0("\u672a\u9023\u7d50\uff1a\u5404\u4e16\u4ee3\u5728\u81ea\u5df1\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\uff08", paste(scales, collapse = "\u3001"),
                           # 原文：），數字不能直接比。
                           "\uff09\uff0c\u6578\u5b57\u4e0d\u80fd\u76f4\u63a5\u6bd4\u3002")
                  },
                  if (any(kitsui_vertical_borrowed(v$table$cohort))) {
                    # 原文：借用另一世代題庫的波次在那個題庫的量尺上，另成一條線，
                    paste0("\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\u7684\u6ce2\u6b21\u5728\u90a3\u500b\u984c\u5eab\u7684\u91cf\u5c3a\u4e0a\uff0c\u53e6\u6210\u4e00\u689d\u7dda\uff0c",
                           # 原文：不和自己量尺的波次連在一起。
                           "\u4e0d\u548c\u81ea\u5df1\u91cf\u5c3a\u7684\u6ce2\u6b21\u9023\u5728\u4e00\u8d77\u3002")
                  }),
    # 原文：內部連結後： | 、 |  換到 
    internal = paste0("\u5167\u90e8\u9023\u7d50\u5f8c\uff1a", paste(setdiff(scales, v$scale), collapse = "\u3001"), " \u63db\u5230 ",
                      # 原文： 的量尺，
                      v$scale, " \u7684\u91cf\u5c3a\uff0c",
                      if (unit == "months") {
                        # 原文：同一個月齡 | 各世代 | 兩個世代
                        paste0("\u540c\u4e00\u500b\u6708\u9f61", if (length(scales) > 2) "\u5404\u4e16\u4ee3" else "\u5169\u500b\u4e16\u4ee3",
                               # 原文：的點應該靠在一起。
                               "\u7684\u9ede\u61c9\u8a72\u9760\u5728\u4e00\u8d77\u3002")
                      } else {
                        # 原文：可以直接比。x 軸是 | ，同一
                        paste0("\u53ef\u4ee5\u76f4\u63a5\u6bd4\u3002x \u8ef8\u662f", kitsui_wave_word(unit), "\uff0c\u540c\u4e00",
                               # 原文：年 | 波 | 各世代的年齡不同，
                               if (unit == "years") "\u5e74" else "\u6ce2", "\u5404\u4e16\u4ee3\u7684\u5e74\u9f61\u4e0d\u540c\uff0c",
                               # 原文：所以點不一定會靠在一起（例如年紀較大的世代分數本來就可能比較高）。
                               "\u6240\u4ee5\u9ede\u4e0d\u4e00\u5b9a\u6703\u9760\u5728\u4e00\u8d77\uff08\u4f8b\u5982\u5e74\u7d00\u8f03\u5927\u7684\u4e16\u4ee3\u5206\u6578\u672c\u4f86\u5c31\u53ef\u80fd\u6bd4\u8f03\u9ad8\uff09\u3002",
                               # 原文：連結合不合理，請看上面的內部連結圖與檢查，或比較年齡相同的點。
                               "\u9023\u7d50\u5408\u4e0d\u5408\u7406\uff0c\u8acb\u770b\u4e0a\u9762\u7684\u5167\u90e8\u9023\u7d50\u5716\u8207\u6aa2\u67e5\uff0c\u6216\u6bd4\u8f03\u5e74\u9f61\u76f8\u540c\u7684\u9ede\u3002")
                      }),
    # 原文：外部連結後：各世代都換到 D，相對位置和內部連結後相同。
    external = "\u5916\u90e8\u9023\u7d50\u5f8c\uff1a\u5404\u4e16\u4ee3\u90fd\u63db\u5230 D\uff0c\u76f8\u5c0d\u4f4d\u7f6e\u548c\u5167\u90e8\u9023\u7d50\u5f8c\u76f8\u540c\u3002"
  )
  if (!any(v$table$source == "earlier")) return(text)
  rounds <- unique(v$table$round[v$table$source == "earlier"])
  where <- if (unit != "months") {
    # 原文：x 軸是波次（單位： | ）。
    paste0("x \u8ef8\u662f\u6ce2\u6b21\uff08\u55ae\u4f4d\uff1a", kitsui_wave_word(unit), "\uff09\u3002")
  } else if (kitsui_vertical_actual(v$table)) {
    # 原文：x 軸是實際月齡：點畫在各波次實際月齡的中位數。
    "x \u8ef8\u662f\u5be6\u969b\u6708\u9f61\uff1a\u9ede\u756b\u5728\u5404\u6ce2\u6b21\u5be6\u969b\u6708\u9f61\u7684\u4e2d\u4f4d\u6578\u3002"
  } else {
    age <- as.numeric(v$table$age_med %||% NA_real_)
    old <- unique(v$table$round[v$table$source == "earlier" & !is.finite(age)])
    if (length(old) > 0) {
      # 原文：x 軸是名義月齡（波次）：之前回合  | 、
      paste0("x \u8ef8\u662f\u540d\u7fa9\u6708\u9f61\uff08\u6ce2\u6b21\uff09\uff1a\u4e4b\u524d\u56de\u5408 ", paste(old, collapse = "\u3001"),
             # 原文： 的固定檔沒有實際月齡（kitsUI 0.3.11 以前做的），一張圖不混用兩種月齡。
             " \u7684\u56fa\u5b9a\u6a94\u6c92\u6709\u5be6\u969b\u6708\u9f61\uff08kitsUI 0.3.11 \u4ee5\u524d\u505a\u7684\uff09\uff0c\u4e00\u5f35\u5716\u4e0d\u6df7\u7528\u5169\u7a2e\u6708\u9f61\u3002",
             # 原文：要畫在實際月齡，用 kitsUI 0.3.11 以後重做那一回合，再用新的固定檔做這一回合。
             "\u8981\u756b\u5728\u5be6\u969b\u6708\u9f61\uff0c\u7528 kitsUI 0.3.11 \u4ee5\u5f8c\u91cd\u505a\u90a3\u4e00\u56de\u5408\uff0c\u518d\u7528\u65b0\u7684\u56fa\u5b9a\u6a94\u505a\u9019\u4e00\u56de\u5408\u3002")
    } else {
      # 原文：x 軸是名義月齡（波次）：這一回合的資料沒有實際月齡（age_m）。
      "x \u8ef8\u662f\u540d\u7fa9\u6708\u9f61\uff08\u6ce2\u6b21\uff09\uff1a\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u6c92\u6709\u5be6\u969b\u6708\u9f61\uff08age_m\uff09\u3002"
    }
  }
  # 原文：實心的點是這一回合。
  paste0(text, "\u5be6\u5fc3\u7684\u9ede\u662f\u9019\u4e00\u56de\u5408\u3002",
         # 原文：空心、較淡的點是之前的回合（ | 、 | ），來自固定檔。
         "\u7a7a\u5fc3\u3001\u8f03\u6de1\u7684\u9ede\u662f\u4e4b\u524d\u7684\u56de\u5408\uff08", paste(rounds, collapse = "\u3001"), "\uff09\uff0c\u4f86\u81ea\u56fa\u5b9a\u6a94\u3002",
         # 原文：滑鼠移到點上，可以看是哪一回合。
         "\u6ed1\u9f20\u79fb\u5230\u9ede\u4e0a\uff0c\u53ef\u4ee5\u770b\u662f\u54ea\u4e00\u56de\u5408\u3002", where)
}
