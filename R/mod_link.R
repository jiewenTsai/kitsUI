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
    shiny::selectInput(ns("internal_scale"), "哪一個量尺要做內部連結？", choices = NULL),
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
      kitDesign::kit_detail("固定檔裡的連結",
                            shiny::tags$p(class = "kit-hint", "內部連結與外部連結，每條一列。",
                              "intercept、slope 是這條連結的係數（新的值 = intercept + slope × 舊的值）。"),
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
    goal = "把各量尺的分數換到同一把尺上。",
    steps = c(
      "內部連結：選一個參照世代以外的量尺，按「建立內部連結」。每個世代、每個領域各做一條。",
      "外部連結：錨題夠了，按「建立外部連結」。參照世代沒有 D 分數的錨題時，這一步不適用。",
      "看連結圖、連結前後的垂直連結與檢查，沒有紅字時，按「下一步：受試者評分」。"
    ),
    output = paste0(
      "存進固定檔的連結。每條連結只做一次，做好就凍結。",
      "之後每一回合都用同一條連結，所以早先算好的 D 分數永遠不會變。"
    ),
    note = paste0(
      "內部連結把其他世代換到參照世代（在 ① 第一回合選的）的量尺；外部連結把參照世代換到 D。",
      "兩種連結都用題庫裡的題目參數（kits_linking()），不是只用這一回合的資料。"
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
        shiny::setProgress(0.2, detail = "用題庫的門檻估計，重抽 2000 次…")
        out <- make(fixed())
        if (isTRUE(out$new)) {
          jump_vertical <<- TRUE
          shiny::setProgress(0.8, detail = "存進固定檔，從點重建這條連結…")
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
      make_link(s, "正在建立內部連結…", \(fx) {
        kitsui_link_internal(fx, scale_domain(s), scale_cohort(s))
      })
    })

    output$internal_status <- shiny::renderUI({
      if (is.null(fixed())) return(NULL)
      if (length(kitsui_internal_scales(fixed())) == 0) {
        return(kitDesign::kit_callout("題庫裡還沒有參照世代（", fixed()$version$reference,
                                      "）以外的世代的量尺，這一步不用做。"))
      }
      link_status_ui(internal(), notes()[[input$internal_scale]], ns("make_internal"),
                     "建立內部連結", "內部連結")
    })

    # 好幾個量尺可以做內部連結時（例如 M06、M40 都連到 M03），列出每一條的狀態
    # 每一條的狀態收成一句話；表收在「每一條（N 條）」裡
    output$internal_overview_ui <- shiny::renderUI({
      if (is.null(fixed()) || length(kitsui_internal_scales(fixed())) < 2) return(NULL)
      ov <- kitsui_internal_overview(fixed(), links(), earlier())
      shiny::tagList(
        shiny::tags$p(class = "kit-hint", kitsui_internal_summary(fixed(), ov),
                      "每個世代、每個領域各有一條，都連到參照世代：在上面一條一條選、一條一條建立。"),
        kitDesign::kit_details_row(kitDesign::kit_detail(
          paste0("每一條（", kitDesign::kit_num(nrow(ov)), " 條）"),
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
      out <- shiny::withProgress(message = "正在做獨立檢核（約需數秒）…", value = 0.4, {
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
      session, "check_dl", "下載檢核結果（CSV）", \() check_now()$at,
      \() paste0(kitsui_link_var(input$internal_scale), "_check")
    )

    # 2. 外部連結 ----
    anchors <- shiny::reactive(kitsui_anchor_counts(shiny::req(fixed())))
    d_state <- shiny::reactive(kitsui_d_state(shiny::req(fixed()), links(), earlier(), anchors()))
    shiny::observeEvent(input$make_d, make_link("D", "正在建立外部連結…", kitsui_link_D))
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
        link_status_ui(d_state(), notes()[["D"]], ns("make_d"), "建立外部連結", "外部連結",
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
      shiny::withProgress(message = "正在算這一回合的分數…", value = 0, tryCatch(
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
        list(why = "垂直連結圖畫不出來。", error = TRUE, raw = conditionMessage(e))
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
      \() list("kits 版" = code_text()), empty = "先做「② 題目校準」，這裡才會有程式碼。"
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
                               ns("go_score"), "下一步：受試者評分")
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
  shiny::setProgress((i - 1) / n, detail = sprintf("%s（第 %d 個，共 %d 個世代）", scale, i, n))
}

#' 下一步的一句話：還沒有連結時不說「做好了」
#' @param ready 還可以建立的內部連結；d 是外部連結的狀態（kitsui_d_state()$state）
#' @noRd
link_next_text <- function(fixed, ready, d) {
  n <- length(kitsui_link_names(fixed))
  todo <- c(ready, if (identical(d, "ready")) "外部連結（到 D）")
  left <- if (length(todo) > 0) {
    paste0("還可以建立：", paste(todo, collapse = "、"), "。不做也可以先評分。")
  }
  na <- if (identical(d, "na")) {
    paste0("外部連結不適用，分數停在參照世代（", fixed$version$reference, "）的量尺上。")
  }
  if (n == 0) {
    return(paste0("還沒有連結，分數會留在各自校準的量尺上。", na,
                  left %||% "這一回合不用做連結。", "下一步是評分。"))
  }
  paste0("固定檔裡有 ", n, " 條連結。", na, left %||% "", "下一步是評分。")
}

#' 一條連結的輸出：圖、下載圖與點（key 是 "internal" 或 "d"）
#' @param link reactive：kits_link；name reactive：下載的檔名（畫按鈕時讀，變了按鈕重畫）
#' @noRd
link_outputs <- function(output, session, key, link, name) {
  out <- \(x) paste0(key, "_", x)
  kind <- if (key == "d") "外部連結" else "內部連結"
  # 畫面與下載的 HTML 是同一張圖：kits 的 plot()，套上 KIT 的樣子
  p <- shiny::reactive(kitDesign::kit_plotly_style(plot(shiny::req(link()))))
  output[[out("plot")]] <- plotly::renderPlotly(p())
  output[[out("plot_dl_ui")]] <- render_download_plot(
    session, out("plot_dl"), paste0("下載", kind, "圖（HTML）"), p, name
  )
  output[[out("points_dl_ui")]] <- render_download_csv(
    session, out("points_dl"), paste0("下載", kind, "的點（CSV）"), \() link()$points,
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
    session, id("dl"), "下載這張垂直連結圖（HTML）", p,
    \() paste0("vertical_", domain(), "_", stage)
  )
}

#' 重建固定檔裡所有連結的 R 程式
#' @param file function(name)：存著這條連結的固定檔檔名（kitsui_link_file()）
#' @noRd
link_code <- function(fixed, file = \(n) "fixed_set.zip") {
  nm <- kitsui_link_names(fixed)
  if (length(nm) == 0) return("# 還沒有連結。做好連結後，這裡會出現重建它的 R 程式。")
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
    "這一回合有量尺停下來（", paste(round$stopped, collapse = "、"), "）。",
    "連結仍可以先看，但這一回合不會產生新的固定檔。",
    "可以在 ② 把它們改成「這一回合不做」再校準一次。",
    warn = TRUE
  )
}

#' 連結的狀態：可以做（按鈕與「做了就凍結」）、還不能做（原因）、不適用（不是錯誤）、已凍結；
#' 這次連線按了按鈕以後是狀態框：建立好了是【完成】，沒有建立是【停下來】（原因與怎麼辦）
#' @param note 這次連線按了按鈕以後的訊息（沒有按過時 NULL）
#' @param kind 「內部連結」或「外部連結」
#' @param primary 按鈕是這一頁的主要按鈕嗎（不是時用次要按鈕）
#' @noRd
link_status_ui <- function(st, note, button_id, button_label, kind = "連結", primary = TRUE) {
  tried <- !is.null(note)
  shiny::tagList(
    if (tried && st$state == "frozen") {
      kitDesign::kit_status_ui(kitDesign::kit_status("done", paste0(kind, "建立好了，已凍結在固定檔裡。")),
                shiny::tags$p(kitsui_error_ui(st$text)))
    } else {
      # kits 的英文原文收起來（kitsui_error_ui()）
      kitDesign::kit_callout(kitsui_error_ui(st$text), warn = st$state == "missing")
    },
    if (tried && st$state != "frozen") {
      kitDesign::kit_status_ui(
        kitDesign::kit_status("stop", paste0(kind, "沒有建立。")),
        shiny::tags$p(kitsui_error_ui(note)),
        shiny::tags$p("怎麼辦：看上面的原因；題庫缺的題要在之後的回合校準，再回來建立。")
      )
    },
    if (st$state == "ready") {
      shiny::tags$div(
        class = "kit-downloads",
        kitDesign::kit_button(button_id, button_label, if (primary) "primary" else "secondary"),
        shiny::tags$small(class = "kit-hint", "連結一做好就凍結在固定檔裡，不能復原，也不會重做。")
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
      shiny::tags$strong("外部連結不適用："),
      paste0("參照世代（", ref, "）沒有 D 分數的錨題，分數留在 ", ref, " 的量尺上。這不是錯誤。"),
      shiny::tags$details(shiny::tags$summary("為什麼"), kitsui_d_hint(fixed, FALSE))
    )
  )
}

#' 垂直連結的標題與說明
#' @noRd
vertical_head_ui <- function() {
  shiny::tagList(
    shiny::h4("3. 各世代的垂直連結：連結前後"),
    shiny::tags$small(
      class = "kit-hint",
      "三個分頁：未連結、內部連結後、外部連結後。",
      "每個波次畫中位數，線段是 P25 到 P75（kits_vertical()）。",
      "波次的單位是月時，連結後同一個月齡各世代的點靠在一起，表示連結合理。",
      "單位是年或波次序號時，同一波各世代的年齡不同，點不一定會靠在一起。",
      "之前回合的點來自固定檔裡的各波次摘要，照現在的連結換算。",
      "所以這一回合只有一個世代時，也看得到連結得怎樣。",
      "只有一個世代、也沒有之前回合的點時，只畫未連結的圖。"
    )
  )
}

#' 一條連結的結果：公式、檢查、圖、下載（key 是 "internal"（內部連結）或 "d"（外部連結））
#'
#' 公式與係數直接寫在畫面上（不用等另一個輸出），圖還沒畫好時有一句「正在畫」。
#' @noRd
link_result_ui <- function(ns, key, link, extra = NULL) {
  out <- \(x) ns(paste0(key, "_", x))
  kind <- if (key == "d") "外部連結" else "內部連結"
  if (startsWith(key, "il_")) {
    # 其他已經建好的連結（收合區塊裡）：公式在標題上，這裡只放係數、圖與下載
    return(shiny::tagList(
      link_coef_table(kitsui_link_formula(link)$table),
      link_plot_box(out("plot"), "正在畫連結圖…"),
      shiny::tags$div(class = "kit-downloads", ui_download(out("plot_dl")),
                      ui_download(out("points_dl"))),
      kitDesign::kit_details_row(kitDesign::kit_detail(paste0(kind, "的檢查（kits_check）"),
                               shiny::tags$div(class = "kit-table-scroll",
                                               shiny::tableOutput(out("checks")))))
    ))
  }
  f <- kitsui_link_formula(link)
  shiny::tagList(
    shiny::h5(paste0(kind, "的公式")),
    shiny::tags$p(shiny::tags$strong(f$formula)),
    shiny::tags$small(
      class = "kit-hint",
      "新的值 = intercept + slope × 舊的值。", "linking_error 是連結本身的不確定性。"
    ),
    link_coef_table(f$table),
    shiny::h5(paste0(kind, "圖")),
    shiny::tags$small(class = "kit-hint",
                      "每個點是一個共同的題目步驟。", "點越貼近直線，連結越可靠。",
                      "圖例：Kept 是留下的點，Dropped 是拿掉的點。"),
    link_plot_box(out("plot"), "正在畫連結圖…"),
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(out("plot_dl")),
      ui_download(out("points_dl"))
    ),
    kitDesign::kit_details_row(
      kitDesign::kit_detail(paste0(kind, "的檢查（kits_check）"),
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
      paste0(name, " → ", link$to_scale, "：", f$formula),
      link_result_ui(ns, link_list_key(name), link)
    )
  })
  shiny::tagList(
    shiny::h5(paste0("其他已經建好的內部連結（", kitDesign::kit_num(length(links)), " 條）")),
    shiny::tags$small(class = "kit-hint", "每條一列；打開看係數、連結圖與檢查。"),
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
    "內部連結的獨立檢核（kits_link_check）",
    shiny::tags$small(
      class = "kit-hint",
      "用不靠共同題目的兩種方法再算一次：同年齡組比較、兩個世代一起校準。",
      "結果和連結差不多，表示連結可信。"
    ),
    if (is.null(why)) {
      shiny::tagList(
        kitDesign::kit_button(ns("run_check"), "做獨立檢核"),
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
    return(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "獨立檢核停下來了，沒有結果。"),
                     shiny::tags$p(kitsui_error_ui(kit_error_text(chk, "獨立檢核")))))
  }
  tab <- kitsui_link_check_table(chk)
  shiny::tagList(
    shiny::tags$small(
      class = "kit-hint kit-tab-caption",
      "max_diff 是在幾個能力值上，這個方法和連結換算結果的最大差距（目標量尺的單位）。",
      if ("max_z" %in% names(tab)) "max_z 是這個差距除以標準誤。",
      attr(tab, "rule")
    ),
    shiny::tableOutput(ns("check_table")),
    shiny::tags$details(
      shiny::tags$summary("kits 的完整輸出"),
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
    "錨題數與外部連結涵蓋的量尺",
    shiny::tags$p("參照世代各量尺在題庫裡的錨題數（kits_anchors()），與外部連結涵蓋哪些量尺："),
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(table_id)),
    shiny::tags$small(
      class = "kit-hint",
      "外部連結是各領域共用的一條線。",
      paste0("沒有錨題、或之後才進題庫的 ", ref, " 量尺，沿用同一條外部連結，不重新估計。"),
      "之後各回合題庫裡的錨題數可能變多。",
      "但外部連結凍結後，不會用新的錨題重新估計。"
    )
  ))
}

#' 垂直連結：選領域；沒有校準好的領域時，一句話說明
#' @noRd
vertical_pick_ui <- function(ns, domains, pick, why = NULL) {
  if (length(domains) == 0) {
    return(kitDesign::kit_callout(why %||% "這一回合沒有校準好的領域，所以沒有垂直連結圖。"))
  }
  if (!isTRUE(pick %in% domains)) pick <- domains[1]
  shiny::tagList(
    shiny::selectInput(ns("vertical_domain"), "哪一個領域？", domains, pick),
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
      action = shiny::tags$p("怎麼辦：連結本身不受影響，可以照常到「④ 受試者評分」。",
                             "請把這個領域與下面的原文告訴維護 kitsUI 的人。"),
      details = v$raw[1], details_title = "R 的原文"
    )))
  }
  # 外部連結不適用（na）不是錯誤，不用橘色
  if (!is.null(v$why)) return(kitDesign::kit_callout(v$why, warn = !isTRUE(v$na)))
  id <- \(x) ns(paste0("vertical_", stage, "_", x))
  shiny::tagList(
    shiny::tags$p(class = "kit-hint kit-tab-caption", vertical_caption(stage, v)),
    link_plot_box(id("plot"), "正在畫垂直連結圖…", "460px"),
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
                    paste0("未連結：一個世代，在自己校準的量尺上（", scales, "）。")
                  } else {
                    paste0("未連結：各世代在自己校準的量尺上（", paste(scales, collapse = "、"),
                           "），數字不能直接比。")
                  },
                  if (any(kitsui_vertical_borrowed(v$table$cohort))) {
                    paste0("借用另一世代題庫的波次在那個題庫的量尺上，另成一條線，",
                           "不和自己量尺的波次連在一起。")
                  }),
    internal = paste0("內部連結後：", paste(setdiff(scales, v$scale), collapse = "、"), " 換到 ",
                      v$scale, " 的量尺，",
                      if (unit == "months") {
                        paste0("同一個月齡", if (length(scales) > 2) "各世代" else "兩個世代",
                               "的點應該靠在一起。")
                      } else {
                        paste0("可以直接比。x 軸是", kitsui_wave_word(unit), "，同一",
                               if (unit == "years") "年" else "波", "各世代的年齡不同，",
                               "所以點不一定會靠在一起（例如年紀較大的世代分數本來就可能比較高）。",
                               "連結合不合理，請看上面的內部連結圖與檢查，或比較年齡相同的點。")
                      }),
    external = "外部連結後：各世代都換到 D，相對位置和內部連結後相同。"
  )
  if (!any(v$table$source == "earlier")) return(text)
  rounds <- unique(v$table$round[v$table$source == "earlier"])
  where <- if (unit != "months") {
    paste0("x 軸是波次（單位：", kitsui_wave_word(unit), "）。")
  } else if (kitsui_vertical_actual(v$table)) {
    "x 軸是實際月齡：點畫在各波次實際月齡的中位數。"
  } else {
    age <- as.numeric(v$table$age_med %||% NA_real_)
    old <- unique(v$table$round[v$table$source == "earlier" & !is.finite(age)])
    if (length(old) > 0) {
      paste0("x 軸是名義月齡（波次）：之前回合 ", paste(old, collapse = "、"),
             " 的固定檔沒有實際月齡（kitsUI 0.3.11 以前做的），一張圖不混用兩種月齡。",
             "要畫在實際月齡，用 kitsUI 0.3.11 以後重做那一回合，再用新的固定檔做這一回合。")
    } else {
      "x 軸是名義月齡（波次）：這一回合的資料沒有實際月齡（age_m）。"
    }
  }
  paste0(text, "實心的點是這一回合。",
         "空心、較淡的點是之前的回合（", paste(rounds, collapse = "、"), "），來自固定檔。",
         "滑鼠移到點上，可以看是哪一回合。", where)
}
