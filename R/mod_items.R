# ② 題目校準：跑一回合（自由校準或偏移把關 + 固定校準），每個量尺看結果

# 圖的種類
ITEMS_PLOTS <- c(
  "類別機率曲線" = "category", "題目與受試者分布（Wright map）" = "wright",
  "適配度（infit、outfit）" = "fit", "觀察與預期（依波次）" = "observed",
  "偏移把關圖" = "drift"
)
# 一次畫幾題（類別機率曲線、觀察與預期）
ITEMS_PAGE <- 9
# 要選題的圖
ITEMS_PAGED <- c("category", "observed")
# flag 的量尺怎麼辦（一句話，各處相同）
ITEMS_FLAG_NOTE <- "被 flag 的量尺照樣往下做；請看偏移的題，必要時在下一回合處理。"

#' @noRd
mod_items_ui <- function(id) {
  ns <- shiny::NS(id)
  hint <- \(...) shiny::tags$small(class = "kit-hint", ...)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      goal = "估計每一題的難度（校準）。有固定檔時，先檢查舊題有沒有偏移。",
      steps = c(
        "按「開始校準」。這一回合不做的量尺、題庫與 flag 的條件在「進階設定」，通常不用改。",
        "看狀態框：【完成】、【完成，但請注意】或【停下來】（停下來時寫了原因與怎麼辦）。",
        "看圖：預設是第一個校準好的量尺，可以換別的量尺、別種圖。",
        "每個量尺一列的表、要注意的細節、題目參數、固定了哪些題、偏移把關、適配度、紀錄都收在下面，要看再打開。",
        "沒有【停下來】時，按「下一步：連結」。"
      ),
      output = "這一回合每個量尺的題目參數。新的題目步驟會加進固定檔的題庫。",
      note = paste0(
        "偏移把關的順序：先不固定任何題、只用這一波試算一次；試算的門檻和題庫比，",
        "離線不遠的題固定成題庫的值；偏移的題換成新版本、用這一波重新估；最後一起做固定校準。"
      )
    ),
    kitDesign::kit_advanced(
      shiny::h5("這一回合不做的量尺"),
      shiny::uiOutput(ns("exclude_ui")),
      hint("有量尺停下來、或這一回合不想做某個量尺時，勾它再按「開始校準」。",
           "勾了的量尺不校準，題庫與連結都不會改變，固定檔的 checks.csv 會記一列 excluded；",
           "其他量尺照常做，也可以產生固定檔。之後的回合可以再做它。"),
      shiny::h5("用哪一個題庫"),
      shiny::tags$p(class = "kit-hint",
                    "預設照月齡的規則：自己的題庫沒有校準過這個月齡、另一個世代的有時，",
                    "借用另一世代題庫。波次的單位不是月時不借，用自己的題庫。",
                    "要改的話，在「題庫」欄選別的題庫，再按「開始校準」。"),
      shiny::uiOutput(ns("banks")),
      shiny::h5("flag 的條件"),
      shiny::numericInput(ns("flag_share"), "偏移題超過已判斷題的比例", 0.2, 0, 1, 0.05),
      shiny::numericInput(ns("flag_fixed"), "可固定的題少於幾題（空白：依量尺的題數）", NA, 1, 50,
                          1),
      hint(
        "偏移題太多，或可固定的題太少，這個量尺會被 flag。",
        "「可固定的題少於幾題」空白時，每個量尺用 5 與題數的一半（無條件進位）中小的那個：",
        "10 題以上是 5 題；4 題的量尺是 2 題、5 題的量尺是 3 題。",
        ITEMS_FLAG_NOTE,
        "只有 kits 出錯、沒有可固定的題，或沒有題可以估計時，量尺才會停下來。"
      ),
      title = "進階設定（這一回合不做的量尺、用哪一個題庫、flag 的條件）"
    ),
    # 進階設定收起來時也看得到：現在勾了哪些「這一回合不做」
    shiny::uiOutput(ns("exclude_line")),
    shiny::uiOutput(ns("settings")),
    kitDesign::kit_button(ns("run"), "開始校準", "primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("result")),
    shiny::uiOutput(ns("next_step"))
  )
}

#' @param data mod_data_server() 回傳的 reactive
#' @param goto function(tab) 切換分頁
#' @return reactive：kitsui_round() 的結果（$args 是這一次用的設定）；還沒校準、或校準以後
#'   改了設定還沒重新校準時是 NULL
#' @noRd
mod_items_server <- function(id, data, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    round <- shiny::reactiveVal(NULL)
    used <- shiny::reactiveVal(NULL)
    # 給使用者看的狀態框（kitDesign::kit_status_ui()）：結果以外的訊息（要重新執行、停下來）
    note <- shiny::reactiveVal(NULL)
    # 因為資訊不足停下來的資料量尺 -> kits 的原文：之後選「這一回合不做」時，checks.csv 寫原因
    no_info <- shiny::reactiveVal(character())

    # 資料或設定變了，舊結果作廢
    shiny::observeEvent(data(), {
      no_info(character())
      if (!is.null(shiny::isolate(round()))) {
        round(NULL)
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stale", "資料或設定改了，要重新校準。"),
                       shiny::tags$p("舊的結果已清除。請再按「開始校準」。")))
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # 用哪一個題庫：每個資料量尺一列，預設是規則選的
    banks <- shiny::reactive({
      d <- data()
      if (is.null(d$plan)) return(NULL)
      kitsui_bank_options(d$plan, d$fixed)
    })
    output$banks <- shiny::renderUI({
      b <- banks()
      if (is.null(b)) return(shiny::tags$p(class = "kit-hint", "先在「① 資料」選好這一回合的資料。"))
      items_bank_table(ns, b)
    })
    # 每一列選的題庫（還沒有選單時是規則選的）
    bank_picks <- shiny::reactive({
      b <- banks()
      if (is.null(b)) return(NULL)
      purrr::map_chr(seq_len(nrow(b)), \(i) input[[paste0("bank_", i)]] %||% b$rule[i])
    })
    # why 欄跟著選擇
    shiny::observe({
      b <- banks()
      if (is.null(b)) return()
      purrr::walk(seq_len(nrow(b)), \(i) {
        output[[paste0("why_", i)]] <- shiny::renderText(kitsui_bank_why(b, bank_picks())[i])
      })
    })
    # 和規則不同的選擇（kitsui_round(bank_choice = )）；照規則時是 NULL
    bank_choice <- shiny::reactive({
      b <- banks()
      if (is.null(b)) return(NULL)
      pick <- bank_picks()
      keep <- pick != b$rule & nzchar(pick)
      if (!any(keep)) return(NULL)
      stats::setNames(pick[keep], b$data_scale[keep])
    })

    # 這一回合不做的量尺：資料的量尺（領域 × 世代）；資料換了時還在的留著
    output$exclude_ui <- shiny::renderUI({
      d <- data()
      if (is.null(d$scales)) return(NULL)
      sc <- d$scales$scale
      shiny::checkboxGroupInput(ns("exclude"), NULL, stats::setNames(sc, scale_label(sc)),
                                intersect(shiny::isolate(input$exclude), sc), inline = TRUE)
    })
    # 勾選框收在進階設定裡：收起來時也要畫好（按「這一回合不做 …，再校準一次」時要改它的勾選）
    shiny::outputOptions(output, "exclude_ui", suspendWhenHidden = FALSE)
    output$exclude_line <- shiny::renderUI({
      ex <- exclude_now() %||% character()
      if (length(ex) == 0) return(NULL)
      shiny::tags$p(class = "kit-hint", paste0(
        "這一回合不做：", kitsui_first(scale_label(ex), 6), "（共 ", kitDesign::kit_num(length(ex)),
        " 個量尺；在上面的「進階設定」改）。"
      ))
    })

    run_round <- function(exclude) {
      d <- data()
      if (is.null(d)) {
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "資料還不能用，沒有校準。"),
                       shiny::tags$p("怎麼辦：回到「① 資料」，看【停下來】的原因，處理好再回來。")))
        return()
      }
      # 這一次用的設定：存進結果（res$args），程式碼與「設定改了」的比較都用它
      args <- items_args(input$flag_share, input$flag_fixed, bank_choice(), exclude)
      res <- shiny::withProgress(message = "題目校準中…", value = 0, {
        tryCatch(
          suppressMessages(kitsui_round(
            d$df, d$fixed, d$round_label, flag_share = args$flag_share,
            flag_fixed = args$flag_fixed, files = d$files,
            progress = items_progress, bank_choice = args$bank_choice, exclude = exclude,
            exclude_info = no_info()
          )),
          error = \(e) e
        )
      })
      if (inherits(res, "error")) {
        round(NULL)
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "校準停下來了，沒有結果。"),
                       shiny::tags$p(kitsui_error_ui(kit_error_text(res, "校準"))),
                       shiny::tags$p("怎麼辦：看上面的原因；資料有問題時回到「① 資料」處理，",
                                     "或勾「這一回合不做」出問題的量尺，再按「開始校準」。")))
        return()
      }
      note(NULL)
      used(d)
      res$args <- args
      round(res)
      seen <- kitsui_no_info_stopped(res)
      if (length(seen) > 0) no_info(c(seen, no_info()[setdiff(names(no_info()), names(seen))]))
    }
    shiny::observeEvent(input$run, run_round(input$exclude))
    # 現在勾的「這一回合不做」：跟著勾選框；按「這一回合不做 …，再校準一次」時先記下要勾的
    # （勾選框要等瀏覽器回報才會變，這段時間不算設定改了）
    exclude_now <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$exclude, exclude_now(input$exclude), ignoreNULL = FALSE,
                        priority = 10)
    # 停下來的量尺改成「這一回合不做」，再校準一次
    shiny::observeEvent(input$skip_stopped, {
      res <- round()
      shiny::req(res, length(res$stopped) > 0)
      skip <- union(input$exclude, items_stopped_data(res))
      shiny::updateCheckboxGroupInput(session, "exclude", selected = skip)
      exclude_now(skip)
      run_round(skip)
    })

    # 校準以後改了設定（flag 的條件、題庫、這一回合不做）：結果留著給使用者看，但標成
    # 【要重新執行】；③、④ 拿到 NULL（和資料改了一樣，要重新校準才能往下做）
    changed <- shiny::reactive({
      res <- round()
      if (is.null(res$args)) return(character())
      now <- items_args(input$flag_share, input$flag_fixed, bank_choice(), exclude_now())
      items_args_changes(res$args, now)
    })
    stale <- shiny::reactive(length(changed()) > 0)
    # 交給 ③、④ 的結果：reactiveVal 值一樣時不通知，展開進階設定等不會讓 ③、④ 重來
    current <- shiny::reactiveVal(NULL)
    shiny::observe(current(if (stale()) NULL else round()))

    # 設定摘要：這一回合的世代、波次、參照世代、固定檔（按下去以前就看得到）
    output$settings <- shiny::renderUI(
      kitDesign::kit_settings_line(kitsui_settings_text(data()), prefix = NULL)
    )
    output$status <- shiny::renderUI({
      if (!is.null(note())) return(note())
      res <- round()
      if (is.null(res)) {
        return(shiny::tags$p(class = "kit-hint", "還沒有校準。按「開始校準」。"))
      }
      if (stale()) return(items_stale_status(changed()))
      items_status(res, ns("skip_stopped"), checks_all(), items_n_visits(res, used()$scales))
    })

    # 每個量尺每一題的狀態（固定了哪些題）與核對：校準完算一次，表、要注意、細節都用它
    checks_all <- shiny::reactive({
      res <- round()
      shiny::req(res)
      bank_all <- used()$fixed$bank %||% kitsui_fixed_empty()$bank
      items_checks(res, bank_all)
    })
    # 重點：每個量尺一列（畫面用中文欄名）
    output$overview <- kit_table(kitsui_screen_names(items_overview(shiny::req(round()),
                                                                    checks_all())))
    output$attention <- shiny::renderUI(items_attention_ui(shiny::req(round()), checks_all()))

    scales <- shiny::reactive({
      res <- round()
      shiny::req(res)
      # 校準的量尺（借用另一世代題庫時是另一個世代的量尺）
      sc <- unique(res$plan$scale %||% used()$scales$scale)
      sc[sc %in% c(names(res$items), res$stopped)]
    })

    output$result <- shiny::renderUI({
      res <- shiny::req(round())
      sc <- scales()
      pick <- shiny::isolate(input$scale)
      # 預設看第一個校準好的量尺（停下來的量尺沒有圖）
      if (!isTRUE(pick %in% sc)) pick <- items_first_ok(sc, res)
      n_rows <- length(unique(res$plan$scale %||% sc)) + length(res$excluded)
      has_attention <- length(items_attention_parts(res, checks_all())) > 0
      items_result_ui(ns, sc, pick, n_rows, has_attention)
    })

    sel <- shiny::reactive({
      res <- round()
      shiny::req(res, input$scale %in% scales())
      s <- input$scale
      list(scale = s, item = res$items[[s]], drift = res$drift[[s]], X = res$data[[s]],
           log = dplyr::filter(res$log, .data$scale == s), stopped = s %in% res$stopped)
    })
    persons <- shiny::reactive(kitsui_persons(shiny::req(used())$df))
    # 這一回合這個量尺的分數：算一次存在 round()$memo（③、④ 也用）
    scores <- shiny::reactive({
      x <- sel()
      shiny::req(x$item)
      kitsui_own_scores(round(), x$scale, persons())
    })

    output$scale_info <- shiny::renderUI(items_scale_info(sel()))

    # 固定了哪些題：每一題的狀態，並核對固定的步驟和這一回合開始時的題庫相同
    status <- shiny::reactive(checks_all()[[sel()$scale]] %||% list(tab = NULL, check = NULL))
    output$fixed_status <- shiny::renderUI(items_fixed_ui(status()$tab, status()$check))
    status_csv <- shiny::reactive({
      tab <- status()$tab
      shiny::req(tab)
      items_with_scale(tab[c("item_id", "status", "steps", "detail")], sel()$scale)
    })

    # 偏移表：畫面上的 reason 是中文；csv 多了 scale 與 kits 的英文 reason
    drift_rows <- shiny::reactive(items_drift_rows(sel()))
    drift_csv <- shiny::reactive(items_with_scale(drift_rows(), sel()$scale))
    output$drift <- kit_table(items_drift_display(drift_rows()))
    output$drift_note <- shiny::renderUI(items_drift_note(sel()$drift))

    # 題目參數：畫面上固定的步驟寫「固定」；csv 保留數字
    coef_raw <- shiny::reactive({
      x <- sel()
      shiny::validate(shiny::need(x$item, "這個量尺停下來了，沒有題目參數。"))
      # 畫面與下載的欄名：beta -> difficulty（kits 與固定檔裡仍叫 beta）
      kitsui_display_names(as.data.frame(stats::coef(x$item)))
    })
    output$coef <- kit_table(items_coef_display(coef_raw()))
    coef_csv <- shiny::reactive(items_with_scale(coef_raw(), sel()$scale))

    fit_tab <- shiny::reactive({
      x <- sel()
      shiny::validate(shiny::need(x$item, "這個量尺停下來了，沒有適配度。"))
      as.data.frame(x$item$itemfit)
    })
    output$fit <- kit_table(fit_tab())
    fit_csv <- shiny::reactive(items_with_scale(fit_tab(), sel()$scale))

    output$log <- kit_table(items_log_table(sel()$log))

    output$plot_help <- shiny::renderUI({
      shiny::tags$p(class = "kit-hint", items_plot_help(input$plot_type %||% "category"))
    })

    # 要畫的題（類別機率曲線、觀察與預期）：預設前 9 題，可以換頁或自己選
    plot_ids <- shiny::reactive({
      x <- sel()
      shiny::req(x$item)
      items_plot_ids(x$item, x$X)
    })
    shown <- shiny::reactive(items_shown(plot_ids(), input$plot_items))
    output$plot_items_ui <- shiny::renderUI({
      if (!(input$plot_type %||% "category") %in% ITEMS_PAGED) return(NULL)
      items_picker_ui(ns, plot_ids(), shiny::isolate(shown()))
    })
    shiny::observeEvent(input$plot_page, {
      ids <- plot_ids()
      page <- items_page_ids(ids, as.integer(input$plot_page))
      if (!identical(page, shown())) {
        shiny::updateSelectizeInput(session, "plot_items", selected = page)
      }
    })
    output$plot_note <- shiny::renderUI({
      if (!(input$plot_type %||% "category") %in% ITEMS_PAGED) return(NULL)
      shiny::tags$p(class = "kit-hint", items_shown_text(plot_ids(), shown()))
    })

    plot_obj <- shiny::reactive({
      x <- sel()
      shiny::validate(shiny::need(x$item, "這個量尺停下來了，沒有校準結果可以畫。"))
      type <- input$plot_type %||% "category"
      if (type == "drift") {
        unjudged <- any(x$log$check == "drift not judged, kept fixed")
        shiny::validate(shiny::need(x$drift, if (unjudged) {
          "這一回合的資料不能判斷偏移，題庫的題都固定在題庫的值，沒有偏移把關圖。"
        } else {
          "這個量尺沒有題庫，是自由校準，沒有偏移把關圖。"
        }))
      }
      # 依類別上色的圖（分數、波次）換成 KIT 的類別色（kit_chart_categorical()）
      p <- shiny::withProgress(message = "畫圖中…", value = 0.5, tryCatch(switch(
        type,
        wright = plot(x$item, type = "wright", scores = scores()),
        observed = kit_chart_categorical(
          plot(x$item, type = "observed", data = x$X, group = "wave", persons = persons(),
               scores = scores(), items = shown()),
          skip = "model"
        ),
        fit = items_fit_plot(x$item$itemfit),
        drift = kitsui_drift_plot(x$drift, status()$tab),
        kit_chart_categorical(plot(x$item, type = "category", items = shown()))
      ), error = \(e) e))
      if (inherits(p, "error")) {
        shiny::validate("這張圖畫不出來。請換一種圖或別的量尺；R 的原文：", conditionMessage(p))
      }
      # 圖的字照 kits 的英文（資料與題號不動）；最後套上 KIT 的樣子
      kitDesign::kit_plotly_style(p)
    })
    output$plot <- plotly::renderPlotly(plot_obj())

    # 檔名跟著量尺、圖的種類（還沒選圖時是第一種）與畫的題
    name <- \(what) \() paste0(what, "_", sel()$scale)
    plot_name <- \() {
      type <- input$plot_type %||% "category"
      which <- if (type %in% ITEMS_PAGED) paste0("_", items_shown_label(plot_ids(), shown()))
      paste0("item_", sel()$scale, "_", type, which)
    }
    # 下載鈕：檔名變了（換量尺、換圖、換題）按鈕就重畫；按下載時寫當下畫面上的圖與表
    csv_dl <- \(id, label, table_fun, file) {
      output[[paste0(id, "_ui")]] <- render_download_csv(session, id, label, table_fun, file)
    }
    output$dl_plot_ui <- render_download_plot(
      session, "dl_plot", "下載這張圖（HTML，畫的是上面選的題）", plot_obj, plot_name
    )
    csv_dl("dl_coef", "下載題目參數（CSV）", coef_csv, name("item_coef"))
    csv_dl("dl_status", "下載每一題的狀態（CSV）", status_csv, name("fixed_status"))
    csv_dl("dl_drift", "下載偏移表（CSV）", drift_csv, name("drift"))
    csv_dl("dl_fit", "下載適配度（CSV）", fit_csv, name("item_fit"))
    csv_dl("dl_log", "下載這一回合的紀錄（CSV，所有量尺）", \() items_log_csv(round()$log),
           \() paste0("round_log_", used()$round_label))

    output$check <- kit_table({
      x <- sel()
      checks_or_ok(check_table(item = x$item, drift = x$drift))
    })

    code_text <- shiny::reactive({
      x <- sel()
      d <- used()
      plan <- round()$plan
      code <- kitsui_code_round(
        x$scale, d$round_label, fixed = d$fixed, fixed_file = d$fixed_file %||% "fixed_set.zip",
        data_file = d$data_file, waves = d$waves, flag_share = round()$args$flag_share,
        flag_fixed = round()$flag_fixed[x$scale] %|NA|% 5, plan = plan, picks = d$picks
      ) |>
        kitsui_code_drift_plot()
      info <- if (!is.null(x$item)) {
        paste("#", utils::capture.output(print(kits::kits_info(x$item))))
      }
      choice <- if (!is.null(plan) && any(plan$manual)) {
        c("# 這一次手動選了題庫（kitsui_round(bank_choice = )）：",
          paste("#", unique(plan$why[plan$manual])))
      }
      paste(c(code, "", choice, "# 這一次在 app 裡的設定（kits_info）", info), collapse = "\n")
    })
    # 用 R 怎麼做：打開時才算；每行最多 70 欄（kitDesign::kit_code()）
    output$code <- kitDesign::kit_render_code_panel(\() list("kits 版" = code_text()))

    output$next_step <- shiny::renderUI({
      res <- round()
      if (!isTRUE(res$ok) || stale()) return(NULL)
      done <- if (length(res$items) == 1) "這個量尺校準好了。" else "每個量尺都校準好了。"
      kitDesign::kit_next_step(paste0(done, "下一步是把各世代的分數連到同一把尺上。"), ns("go_link"),
                               "下一步：連結")
    })
    shiny::observeEvent(input$go_link, goto("link"))

    current
  })
}

#' ② 校準以後的畫面（狀態框下面）：先是一個量尺的圖（先結果、後細節），其他都收在一排收合區塊：
#' 每個量尺一列的表、要注意、題目參數、固定了哪些題、偏移把關、適配度、紀錄、檢查、R 程式
#' @param sc 可以看的量尺；pick 是預設選的（第一個校準好的量尺，items_first_ok()）
#' @param n_rows 「每個量尺」表有幾列（標題用）；has_attention 有沒有要注意的事
#' @noRd
items_result_ui <- function(ns, sc, pick, n_rows = length(sc), has_attention = TRUE) {
  hint <- \(...) shiny::tags$p(class = "kit-hint", ...)
  scroll <- \(id) shiny::tags$div(class = "kit-table-scroll kit-table-tall", shiny::tableOutput(id))
  dl <- \(id) shiny::tags$div(class = "kit-downloads", ui_download(id))
  cut <- kitsui_drift_cut()
  min_n <- eval(formals(kits::kits_drift)$min_n)
  shiny::tagList(
    shiny::h4("看一個量尺"),
    shiny::selectInput(ns("scale"), "量尺", sc, pick),
    shiny::uiOutput(ns("scale_info")),
    shiny::radioButtons(ns("plot_type"), "圖", ITEMS_PLOTS, inline = TRUE),
    shiny::uiOutput(ns("plot_help")),
    shiny::uiOutput(ns("plot_items_ui")),
    shiny::uiOutput(ns("plot_note")),
    plotly::plotlyOutput(ns("plot"), height = "520px"),
    dl(ns("dl_plot")),
    kitDesign::kit_details_row(
      kitDesign::kit_detail(
        paste0("每個量尺（", kitDesign::kit_num(n_rows), " 個）"),
        hint("一個量尺一列。偏移、固定是題數；被 flag 的量尺照常校準，但要看一下。"),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("overview")))
      ),
      if (has_attention) kitDesign::kit_detail("要注意", shiny::uiOutput(ns("attention"))),
      kitDesign::kit_detail(
        "題目參數",
        hint("這個量尺每一題每一個步驟一列（coef(item)）。"),
        scroll(ns("coef")),
        hint("difficulty、difficulty_se：步驟難度與標準誤（logit；kits 裡叫 beta、beta_se）。",
             paste0("threshold、threshold_se：Thurstonian 門檻與標準誤（kits 裡叫 thr）；",
                    "能力值（ability）到這裡時，有一半的機會得到這個分數以上。"),
             "固定在題庫的值的步驟沒有標準誤，表上寫「固定」（CSV 裡是 0）。"),
        dl(ns("dl_coef"))
      ),
      kitDesign::kit_detail(
        "固定了哪些題",
        hint("每一題一列：固定在題庫的值、換成新版本、新題，或這一回合拿掉。",
             "固定的題用題庫的參數，這一回合不重新估計。"),
        shiny::uiOutput(ns("fixed_status")),
        dl(ns("dl_status"))
      ),
      kitDesign::kit_detail(
        "偏移把關",
        hint("kits_drift() 先把這一回合的資料自由校準，得到每一步的門檻。",
             "再用 mean/sigma 畫一條線，把新的門檻換到題庫的量尺上，和題庫的門檻比。",
             sprintf("離線的距離大於 %s 個標準差（所有判斷的步驟算的）的步驟會被拿掉。", cut),
             "有步驟被拿掉的題就是 drift = TRUE，會換成新版本重新估計。",
             sprintf("步驟的上下各要有 %s 人才判斷；不判斷的題 drift 是空的，reason 寫原因。", min_n),
             "suggest_fix = TRUE 的題固定在題庫的值（沿用舊參數）。欄位說明在表的下面。"),
        scroll(ns("drift")),
        shiny::uiOutput(ns("drift_note")),
        dl(ns("dl_drift"))
      ),
      kitDesign::kit_detail(
        "適配度",
        hint("n 是用到的作答數。",
             "pvalue 是平均得分占最高分的比例；只有 0、1 分的題，就是通過的比例。",
             "rir 是這一題和其他題總分的相關；接近 0 或是負的，要看一下題目。",
             "infit、outfit 接近 1 最好。",
             "flag 是 kits 依 infit、outfit 給的判斷：兩個都在 0.6–1.4 之間是 ok。",
             "任一個大於 1.4 是 underfit（比模型預期亂），小於 0.6 是 overfit（比模型預期整齊）。"),
        scroll(ns("fit")),
        dl(ns("dl_fit"))
      ),
      kitDesign::kit_detail(
        "這一回合的紀錄",
        hint("新版本、併分數、分數類別的問題、借用另一世代題庫、停下來的原因，",
             "都會寫進固定檔的 checks.csv。status 是 warn 的列要特別看一下。"),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("log"))),
        dl(ns("dl_log"))
      ),
      ui_check_panel(ns("check")),
      kitDesign::kit_code_panel(ns("code"))
    )
  )
}

#' 進度條：kitsui_round() 每個量尺開始前呼叫一次
#' @noRd
items_progress <- function(i, n, scale) {
  shiny::incProgress(1 / n, detail = sprintf("校準第 %d 個量尺（共 %d 個）：%s", i, n, scale))
}

#' 一回合的狀態框：【完成】、【完成，但請注意】（flag、分數類別有問題、這一回合不做、
#' 固定的步驟和題庫不同；細節在下面的「要注意」）或【停下來】（有量尺停下來：原因、怎麼辦，
#' 與「這一回合不做停下來的量尺，再校準一次」的按鈕）
#' @param skip_id 「不做停下來的量尺」按鈕的 id（含 namespace）；NULL 時不給按鈕
#' @param checks items_checks()；NULL 時不看核對
#' @noRd
items_status <- function(res, skip_id = NULL, checks = NULL, n_visits = NULL) {
  n_ok <- length(res$items)
  if (isTRUE(res$ok)) {
    done <- if (n_ok == 1) "這個量尺校準好了。" else sprintf("%s 個量尺都校準好了。", kitDesign::kit_num(n_ok))
    points <- items_attention_points(res, checks)
    if (length(points) == 0) return(kitDesign::kit_status_ui(kitDesign::kit_status("done", done)))
    return(kitDesign::kit_status_ui(kitDesign::kit_status("warn", done, points = points),
                     shiny::tags$p("每一項的細節在下面的「▸ 要注意」。")))
  }
  why <- dplyr::filter(res$log, .data$check %in% c("stop", "error"))
  skip <- items_stopped_data(res)
  stopped <- unique(why$scale)
  line <- if (length(stopped) == 1) {
    paste0(scale_label(stopped), " 停下來了，這一回合還不能產生新的固定檔。")
  } else {
    sprintf("%s 個量尺停下來了，這一回合還不能產生新的固定檔。", kitDesign::kit_num(length(stopped)))
  }
  # 第一層：依原因分組的白話說明（每一種一點，含怎麼辦）；每個量尺的原因與 kits 的原文收起來
  per_scale <- lapply(seq_len(nrow(why)), \(i) {
    label <- scale_label(why$scale[i])
    # 資訊不足：「〈量尺〉這一回合資訊不足，無法估計（…）。按「這一回合不做」…」
    if (startsWith(why$detail[i], paste0(label, " 這一回合", KITSUI_NO_INFO_ZH))) {
      rest <- substring(why$detail[i], nchar(label) + 1)
      return(shiny::tags$li(shiny::tags$strong(label), kitsui_error_ui(rest)))
    }
    kind <- if (why$check[i] == "error") "kits 出錯" else "停下來"
    shiny::tags$li(shiny::tags$strong(why$scale[i]), "（", kind, "）：",
                   kitsui_error_ui(why$detail[i]))
  })
  skip_label <- if (length(skip) == 1) {
    paste0("這一回合不做 ", scale_label(skip), "，再校準一次")
  } else {
    sprintf("這一回合不做這 %s 個量尺，再校準一次", kitDesign::kit_num(length(skip)))
  }
  kitDesign::kit_status_ui(kitDesign::kit_status(
    "stop", line, points = items_stop_points(why, n_visits)),
    shiny::tags$details(
      class = "kit-status-notes",
      shiny::tags$summary(sprintf("每個量尺停下來的原因（%s 個）", kitDesign::kit_num(length(stopped)))),
      shiny::tags$ul(per_scale)
    ),
    shiny::tags$p(
      class = "kit-status-action",
      shiny::tags$strong("怎麼辦："),
      "照上面每一點的說明改好資料，再按「開始校準」。",
      if (n_ok > 0) sprintf("其他 %s 個量尺有校準結果，可以先看下面的圖。", kitDesign::kit_num(n_ok))
    ),
    if (!is.null(skip_id) && length(skip) > 0 && items_all_skipped(res, skip)) {
      # 停下來的就是這一回合全部的量尺：不做它們就沒有東西可以校準，不給這個選擇
      shiny::tags$p(class = "kit-status-action",
                    "這一回合只有這些量尺，先不做它們就沒有東西可以校準，所以請照上面的說明改好資料。")
    } else if (!is.null(skip_id) && length(skip) > 0) {
      shiny::tags$p(
        class = "kit-status-action",
        "也可以這一回合先不做停下來的量尺：其他量尺照常做、可以產生固定檔；",
        "這些量尺的題庫與連結不會改變，之後的回合可以再做。",
        shiny::tags$br(),
        kitDesign::kit_button(skip_id, skip_label, "secondary")
      )
    }
  )
}

# 停下來的原因分成幾種（照 kits 的訊息與 kitsUI 的說明比對；照順序，第一個符合的用）。
# 是函式：用到別的檔案（pipeline_round.R、utils.R）的常數，載入時還沒有
items_stop_causes <- function() tibble::tribble(
  ~cause, ~pattern,
  # 拿掉得分不完整的題以後沒有題可以估計：人很少時是人太少，其他是分數沒有從 0 開始
  "category", "分數類別有問題",
  "connect", paste("separate groups that share no item", "No item of the new data is in `ref`",
                   "沒有共同的題", sep = "|"),
  "zero", paste("have a 0 score", "no 0 score", "沒有人得 0 分", "gap in their observed scores",
                "Scores must be consecutive", "分數不連續", sep = "|"),
  "few", paste(c(KITSUI_NO_INFO_ZH, KITSUI_KITS_NO_INFO$pattern, "no finite CML estimate"),
               collapse = "|")
)

# 列次少於這個數的量尺，得分不完整多半是人太少（和 kits_drift() 每個步驟上下各要 30 人相同）
ITEMS_FEW_VISITS <- 30

#' 停下來的量尺依原因分組：每一種一句白話（原因＋怎麼辦）
#' @param why 這一回合紀錄裡 stop、error 的列（scale、detail）
#' @param n_visits NULL 或具名整數（校準的量尺 -> 列次數）：得分不完整而停下來時，用它分辨
#'   「人太少」和「分數沒有從 0 開始」
#' @return 中文句子（最多 4 句：題目沒有連在一起、分數沒有從 0 開始、人太少、其他）
#' @noRd
items_stop_points <- function(why, n_visits = NULL) {
  if (nrow(why) == 0) return(character())
  causes <- items_stop_causes()
  cause <- purrr::map_chr(why$detail, \(d) {
    hit <- which(purrr::map_lgl(causes$pattern, \(p) grepl(p, d)))
    if (length(hit) == 0) "other" else causes$cause[hit[1]]
  })
  visits <- if (is.null(n_visits)) rep(NA_real_, nrow(why)) else unname(n_visits[why$scale])
  few <- !is.na(visits) & visits < ITEMS_FEW_VISITS
  cause[cause == "category"] <- ifelse(few[cause == "category"], "few", "zero")
  who <- \(k) {
    sc <- unique(scale_label(why$scale[cause == k]))
    paste0(kitsui_first(sc, 5),
           if (length(sc) > 1) paste0("（共 ", kitDesign::kit_num(length(sc)), " 個）"))
  }
  out <- c(
    connect = paste0(
      "題目沒有連在一起：", who("connect"), "。各波次（或和題庫）之間沒有共同題，不能放在同一把尺上。",
      "怎麼辦：各波的同一題要用同一個題號，回 kitJoin 的「", KITJOIN_KITS_TAB,
      "」第 2 步「合成同一組題號」；有題庫時，題號要和之前的回合相同。"
    ),
    zero = paste0(
      "有題沒有人得 0 分、分數不連續，或只有一種分數：", who("zero"), "。",
      "分數要是連續的 0、1、2…；分數本來就從 0 開始、只是這一回合沒有人得到某些分數時，通常是人太少。",
      "怎麼辦：分數從 1 開始時，在 kitJoin 的「", KITJOIN_KITS_TAB, "」改成從 0 開始再上傳。"
    ),
    few = paste0(
      "人太少、資訊不足：", who("few"),
      if (any(few[cause == "few"])) paste0("（不到 ", ITEMS_FEW_VISITS, " 個列次）"), "。",
      "這一回合的人或題不夠，估不出來，資料本身沒有錯。",
      "怎麼辦：等資料多一些再做；這一回合可以先不做它們。"
    ),
    other = paste0(
      "其他原因：", who("other"), "。",
      sub(paste0(KITSUI_KITS_SAYS, ".*$"), "", why$detail[cause == "other"][1]),
      "每個量尺的原因見下面的「每個量尺停下來的原因」。"
    )
  )
  unname(out[intersect(names(out), unique(cause))])
}

#' 每個校準的量尺有幾個列次（借題庫時加起來）：資料量尺的 n_visits 照 plan 對到校準的量尺
#' @param scales 資料的量尺表（kitsui_scales()）；NULL 時 NULL
#' @noRd
items_n_visits <- function(res, scales) {
  if (is.null(scales)) return(NULL)
  plan <- res$plan
  if (is.null(plan)) return(stats::setNames(scales$n_visits, scales$scale))
  map <- dplyr::distinct(plan, .data$data_scale, .data$scale)
  map$n <- scales$n_visits[match(map$data_scale, scales$scale)]
  tot <- tapply(map$n, map$scale, sum)
  stats::setNames(as.vector(tot), names(tot))
}

#' 不做停下來的量尺以後，還有沒有量尺可以校準（沒有時是 TRUE）
#' @param skip 要不做的資料量尺（items_stopped_data()）
#' @noRd
items_all_skipped <- function(res, skip) {
  all <- unique(c(res$plan$data_scale %||% c(names(res$items), res$stopped), res$excluded))
  length(setdiff(all, union(skip, res$excluded))) == 0
}

#' 預設看哪一個量尺：第一個校準好的（沒有時第一個）
#' @noRd
items_first_ok <- function(sc, res) {
  ok <- sc[sc %in% names(res$items)]
  if (length(ok) > 0) ok[1] else sc[1]
}

#' 每個量尺每一題的狀態與核對（「固定了哪些題」）：校準的量尺 -> list(tab, check)
#' @param bank_all 這一回合開始時的題庫（全部量尺）
#' @noRd
items_checks <- function(res, bank_all) {
  sc <- names(res$items)
  purrr::map(stats::setNames(sc, sc), \(s) {
    bank <- dplyr::filter(bank_all, .data$scale == s)
    log <- dplyr::filter(res$log, .data$scale == s)
    tab <- kitsui_item_status(res$items[[s]], res$drift[[s]], log, bank)
    check <- if (!is.null(tab) && nrow(bank) > 0) kitsui_fixed_check(res$items[[s]], tab, bank)
    list(tab = tab, check = check)
  })
}

#' 固定的步驟和題庫不同的量尺（核對沒過）
#' @noRd
items_check_bad <- function(checks) {
  bad <- purrr::map_lgl(checks, \(x) !is.null(x$check) && nrow(x$check$bad) > 0)
  names(checks)[bad]
}

#' 狀態框裡要注意的事：每一種一句（最多 3 句；細節在 items_attention_ui()）
#' @noRd
items_attention_points <- function(res, checks = NULL) {
  log <- res$log
  flags <- unique(log$scale[log$check == "flag"])
  n_cat <- sum(log$check %in% c("category problem, kept fixed", "category problem, left out"))
  bad <- items_check_bad(checks)
  out <- c(
    if (length(bad) > 0) paste0("固定的步驟和題庫不同：", paste(bad, collapse = "、"), "。"),
    if (length(flags) > 0) paste0("被 flag 的量尺：", paste(flags, collapse = "、"), "。"),
    if (n_cat > 0) sprintf("有 %d 題分數類別有問題，已照規則處理；分數都沒有改。", n_cat),
    if (length(res$excluded) > 0) {
      paste0("這一回合不做：", paste(scale_label(res$excluded), collapse = "、"), "。")
    }
  )
  utils::head(out, 3)
}

#' 要注意：收在細節列「要注意」裡，每一種要注意的事一段（沒有時 NULL）
#' @noRd
items_attention_ui <- function(res, checks = NULL) {
  parts <- items_attention_parts(res, checks)
  if (length(parts) == 0) return(NULL)
  shiny::tags$div(class = "kit-attention", role = "status", parts)
}

#' 要注意的每一段（沒有時長度 0）
#' @noRd
items_attention_parts <- function(res, checks = NULL) {
  bad <- items_check_bad(checks)
  parts <- list(
    if (length(bad) > 0) {
      shiny::tags$p(class = "kit-warn-text",
                    "固定的步驟和題庫不同：", paste(bad, collapse = "、"), "。",
                    "這些步驟應該沿用題庫的值；請選這個量尺、打開「固定了哪些題」，",
                    "把量尺與步驟告訴維護 kitsUI 的人。")
    },
    items_flag_note(res$log),
    items_category_note(res$log),
    items_excluded_note(res$excluded, res$log)
  )
  Filter(Negate(is.null), parts)
}

#' 停下來的量尺用到的資料量尺（借題庫時是借的那個世代的資料量尺）
#' @noRd
items_stopped_data <- function(res) {
  plan <- res$plan
  if (is.null(plan)) return(res$stopped)
  unique(plan$data_scale[plan$scale %in% res$stopped])
}

#' 這一回合不做的量尺：一句話；因為資訊不足而不做的，每個量尺一列寫出原因（沒有時 NULL）
#' @param log 這一回合的紀錄（excluded 的列有原因）
#' @noRd
items_excluded_note <- function(excluded, log = NULL) {
  if (length(excluded) == 0) return(NULL)
  why <- kitsui_excluded_reasons(log)
  why <- why[names(why) %in% excluded & nzchar(why)]
  shiny::tags$div(
    shiny::tags$p(shiny::tags$strong("這一回合不做："), paste(scale_label(excluded), collapse = "、"),
                  "。這些量尺沒有校準，題庫與連結都不會改變；固定檔的 checks.csv 記為 excluded。"),
    if (length(why) > 0) {
      shiny::tags$ul(lapply(names(why), \(s) {
        shiny::tags$li(class = "kit-warn-text",
                       paste0(scale_label(s), " 這一回合", sub("：", "（", why[[s]]),
                              "），已跳過；題庫與連結沒有改變。"))
      }))
    }
  )
}

#' ② 的「可固定的題少於幾題」：空白（NA）或不是正數時依量尺題數（kitsui_flag_fixed()）
#' @noRd
items_flag_fixed <- function(x) {
  if (length(x) != 1 || is.na(x) || x < 1) NA else x
}

#' 一次校準用的設定（存在 kitsui_round() 結果的 $args）
#'
#' 和畫面上的輸入比較時要一樣的寫法：題庫依量尺排好，「這一回合不做」排序、沒有時是
#' character()。
#' @param flag_share,flag_fixed 輸入框的值（flag_fixed 空白時是 NA）
#' @param bank_choice 和規則不同的題庫（具名字元向量）或 NULL
#' @param exclude 這一回合不做的量尺
#' @noRd
items_args <- function(flag_share, flag_fixed, bank_choice, exclude) {
  bank <- if (length(bank_choice) > 0) bank_choice[order(names(bank_choice))]
  list(flag_share = flag_share %||% 0.2, flag_fixed = items_flag_fixed(flag_fixed),
       bank_choice = bank, exclude = sort(as.character(exclude %||% character())))
}

#' 上次校準以後改了哪些設定：每項一句（沒改時是 character()）
#' @param old,now items_args()
#' @noRd
items_args_changes <- function(old, now) {
  show <- \(x, none) if (length(x) == 0 || all(is.na(x))) none else paste(x, collapse = "、")
  bank <- \(x) if (length(x) == 0) "照規則" else paste0(names(x), " 用 ", x, collapse = "、")
  out <- c(
    flag_share = paste0("偏移題超過已判斷題的比例：", show(old$flag_share, "空白"), " → ",
                        show(now$flag_share, "空白")),
    flag_fixed = paste0("可固定的題少於幾題：", show(old$flag_fixed, "空白（依量尺的題數）"),
                        " → ", show(now$flag_fixed, "空白（依量尺的題數）")),
    bank_choice = paste0("用哪一個題庫：", bank(old$bank_choice), " → ", bank(now$bank_choice)),
    exclude = paste0("這一回合不做的量尺：", show(old$exclude, "沒有"), " → ",
                     show(now$exclude, "沒有"))
  )
  same <- vapply(names(out), \(k) identical(old[[k]], now[[k]]), logical(1))
  unname(out[!same])
}

#' 改了設定、還沒重新校準時的狀態框：下面的結果是舊設定算的
#' @param changes items_args_changes()
#' @noRd
items_stale_status <- function(changes) {
  kitDesign::kit_status_ui(kitDesign::kit_status(
    "stale", "設定改了，請再按一次「開始校準」。", points = changes),
    shiny::tags$p(paste0("下面的結果還是用上一次的設定算的。重新校準以前，",
                         "「③ 連結」和「④ 受試者評分」不會用它。"))
  )
}

#' NA 或長度 0 時用 y
#' @noRd
`%|NA|%` <- function(x, y) if (length(x) == 0 || is.na(x[1])) y else unname(x[1])

#' 分數類別有問題的題：一句話說怎麼處理（沒有時 NULL）
#' @noRd
items_category_note <- function(log) {
  kept <- sum(log$check == "category problem, kept fixed")
  out <- sum(log$check == "category problem, left out")
  if (kept + out == 0) return(NULL)
  shiny::tags$p(
    shiny::tags$strong("分數類別有問題："),
    sprintf("有 %d 題分數類別有問題（沒有人得 0 分、分數不連續或只有一種分數）。", kept + out),
    if (kept > 0) sprintf("題庫裡的 %d 題固定在題庫的值。", kept),
    if (out > 0) sprintf("不在題庫裡的 %d 題這一回合不放進來。", out),
    "分數都沒有改；每一題的原因見各量尺的「這一回合的紀錄」。"
  )
}

#' flag 的量尺：每個量尺一列，加上一句怎麼辦（沒有時 NULL）
#' @noRd
items_flag_note <- function(log) {
  flags <- dplyr::filter(log, .data$check == "flag")
  if (nrow(flags) == 0) return(NULL)
  shiny::tags$div(
    shiny::tags$ul(lapply(seq_len(nrow(flags)), \(i) {
      shiny::tags$li(class = "kit-warn-text", paste0(flags$scale[i], "（flag）：", flags$detail[i]))
    })),
    shiny::tags$p(ITEMS_FLAG_NOTE)
  )
}

#' 每個量尺一列：校準的量尺、哪些資料、校準方式、偏移與固定的題數、flag
#' （欄名是英文；畫面上用 kitsui_screen_names() 換成中文）
#' @param checks items_checks()；NULL 時固定的題數是空的
#' @noRd
items_overview <- function(res, checks = NULL) {
  plan <- res$plan
  out <- purrr::map(unique(plan$scale), \(s) {
    p <- plan[plan$scale == s, ]
    log <- res$log[res$log$scale == s, ]
    pick <- \(check) paste(log$detail[log$check == check], collapse = "；")
    stopped <- s %in% res$stopped
    tab <- checks[[s]]$tab
    tibble::tibble(
      scale = s, data = paste(p$data_scale, format(p$wave_m), collapse = "、"),
      calibration = if (stopped) "停下來" else pick("calibration"),
      drift_n = if (stopped) NA_integer_ else sum(log$check == "drift"),
      fixed_n = if (is.null(tab)) NA_integer_ else sum(tab$kind %in% KITSUI_STATUS_FIXED),
      flag = pick("flag")
    )
  }) |>
    purrr::list_rbind()
  if (length(res$excluded) == 0) return(out)
  why <- kitsui_excluded_reasons(res$log)[res$excluded]
  calibration <- ifelse(is.na(why) | !nzchar(why), "這一回合不做",
                        paste0("這一回合不做（", why, "）"))
  dplyr::bind_rows(out, tibble::tibble(scale = res$excluded, data = res$excluded,
                                       calibration = unname(calibration), flag = ""))
}

#' 選題庫的表：資料量尺、波次、題庫（選單）、原因（跟著選單變，why_i）
#' @noRd
items_bank_table <- function(ns, b) {
  th <- \(...) shiny::tags$th(...)
  rows <- lapply(seq_len(nrow(b)), \(i) {
    pick <- shiny::selectInput(ns(paste0("bank_", i)), NULL, b$choices[[i]], b$rule[i],
                               width = "100%")
    why <- shiny::textOutput(ns(paste0("why_", i)), inline = TRUE)
    shiny::tags$tr(shiny::tags$td(b$data_scale[i]), shiny::tags$td(b$waves[i]),
                   shiny::tags$td(pick), shiny::tags$td(why))
  })
  shiny::tags$table(
    class = "table table-sm kit-file-table",
    shiny::tags$thead(shiny::tags$tr(th("量尺"), th("波次"), th("題庫"), th("原因"))),
    shiny::tags$tbody(rows)
  )
}

#' 一個量尺的摘要（圖上面，沒有框）：校準方式、借用另一世代題庫、偏移、新版本、分數類別、併分數、flag
#' @noRd
items_scale_info <- function(x) {
  log <- x$log
  n <- \(check) sum(log$check == check)
  # 「：3 題（a1、a2、a3）」
  # 0 題的不列（每個量尺的偏移、固定題數在上面的表裡）
  count <- \(head, check) {
    it <- log$item_id[log$check == check]
    if (length(it) == 0) return(NULL)
    shiny::tags$li(paste0(sprintf("%s：%d 題", head, length(it)),
                          "（", paste(it, collapse = "、"), "）"))
  }
  some <- \(text, check) if (n(check) > 0) shiny::tags$li(sprintf(text, n(check)))
  stopped <- is.null(x$item)
  mode <- if (stopped) "停下來了（沒有校準結果）" else log$detail[log$check == "calibration"][1]
  flags <- log$detail[log$check == "flag"]
  shiny::tagList(
    shiny::tags$div(
      class = paste("kit-scale-info", if (stopped) "kit-warn-text"),
      shiny::tags$strong(x$scale, "："), mode,
      shiny::tags$ul(
        if (!stopped) {
          shiny::tags$li(sprintf("%s 列次、%s 題", kitDesign::kit_num(nrow(x$X)),
                                 kitDesign::kit_num(ncol(x$X))))
        },
        lapply(log$detail[log$check == "borrowed bank"],
               \(d) shiny::tags$li(paste0("借用另一世代題庫：", d))),
        some("偏移：%d 題", "drift"),
        some("換成新版本：%d 題", "new version"),
        count("估不出來、固定在題庫的值", "no CML estimate, kept fixed"),
        count("分數類別有問題、固定在題庫的值", "category problem, kept fixed"),
        count("分數類別有問題、這一回合不放進來", "category problem, left out"),
        count("不能判斷偏移、固定在題庫的值", "drift not judged, kept fixed"),
        some("併分數（新步驟有缺口）：%d 題", "capped")
      )
    ),
    if (length(flags) > 0) {
      shiny::tags$p(class = "kit-warn-text", paste0("flag：", flags, collapse = "；"), " ",
                    ITEMS_FLAG_NOTE)
    }
  )
}

# 這一回合的紀錄表：加上 status（flag、excluded 是 warn；stop、error 是 error；其他 ok）
# 畫面只放中文的 detail（detail_en 是寫進固定檔的英文）
items_log_table <- function(log) {
  log$detail_en <- NULL
  status <- dplyr::case_when(
    log$check %in% c("flag", "excluded") ~ "warn",
    log$check %in% c("stop", "error") ~ "error",
    .default = "ok"
  )
  as.data.frame(tibble::add_column(log, status = status, .after = "check"))
}

#' 下載的紀錄：和畫面一樣有 status；沒有題號的列 item_id 是 ""（不是 NA）
#' @noRd
items_log_csv <- function(log) {
  out <- items_log_table(log)
  out$item_id <- dplyr::coalesce(out$item_id, "")
  out
}

#' 下載的表前面加上 scale 欄
#' @noRd
items_with_scale <- function(tab, scale) {
  tab <- as.data.frame(tab)
  if ("scale" %in% names(tab)) tab$scale <- NULL
  data.frame(scale = rep(scale, nrow(tab)), tab, check.names = FALSE)
}

# kits_drift() 不判斷的原因（reason）-> 中文
ITEMS_REASON_ZH <- c(
  "score range differs" = "這一回合有比題庫更高的分數：舊的步驟固定，新的步驟另外估",
  "no 0 score or one score only" = "沒有人得 0 分或只有一種分數：固定在題庫的值",
  "score gap" = "分數不連續：不判斷",
  "too little information" = "步驟上下的人不夠：固定在題庫的值"
)

#' 偏移表的列：kits_drift() 的 items 加上步驟的資訊（kitsui_drift_items()），再加上沒有交給
#' kits_drift() 的題庫題（分數類別有問題、估不出來，固定在題庫的值）；reason_zh 是中文原因，
#' judging_limit 是判斷的界線（只在 csv）
#' @param x sel()：drift、log、item、stopped
#' @return data.frame；沒有偏移表時一列 note（自由校準，或停下來了）
#' @noRd
items_drift_rows <- function(x) {
  if (is.null(x$drift)) {
    unjudged <- x$log$detail[x$log$check == "drift not judged, kept fixed"]
    note <- if (isTRUE(x$stopped)) {
      "這個量尺停下來了，沒有偏移表。原因見上面的【停下來】與這一回合的紀錄。"
    } else if (length(unjudged) > 0) {
      paste0("沒有偏移表：", unjudged[1], "。每一題的狀態見「固定了哪些題」。")
    } else {
      "沒有題庫：自由校準，不用檢查偏移。"
    }
    return(data.frame(note = note))
  }
  items <- kitsui_drift_items(x$drift)
  limit <- attr(items, "judging")$limit %||% NA_real_
  attr(items, "judging") <- NULL
  checks <- c("category problem, kept fixed", "no CML estimate, kept fixed")
  kept <- x$log[x$log$check %in% checks & !x$log$item_id %in% items$item_id, ]
  k <- nrow(kept)
  # 沒有交給 kits_drift() 的題：步驟數與距離都是空的
  extra <- data.frame(item_id = kept$item_id, steps = rep(NA_integer_, k),
                      steps_judged = rep(NA_integer_, k), steps_dropped = rep("", k),
                      max_dist_judging = rep(NA_real_, k), max_dist_final = rep(NA_real_, k),
                      drift = rep(NA, k), suggest_fix = rep(TRUE, k), reason = kept$check)
  out <- dplyr::bind_rows(items, extra)
  zh <- unname(ITEMS_REASON_ZH[out$reason])
  zh[out$reason == ""] <- ""
  zh[out$reason %in% checks] <- kept$detail
  out$reason_zh <- dplyr::coalesce(zh, out$reason)
  # csv 才有：判斷的界線（cut × 判斷用的線的殘差標準差；畫面上寫在表的下面）
  out$judging_limit <- rep(limit, nrow(out))
  out
}

#' 偏移表下面的欄位說明：兩條線（判斷用的線、最後的線）與這個量尺的界線
#' @param drift kits_drift() 或 NULL（沒有偏移表時不顯示）
#' @noRd
items_drift_note <- function(drift) {
  if (is.null(drift)) return(NULL)
  jd <- kitsui_drift_judging(drift)
  cut <- drift$info$settings$cut %||% kitsui_drift_cut()
  p <- \(...) shiny::tags$p(class = "kit-hint", ...)
  limit <- if (is.null(jd)) {
    p("這個量尺重算不出 kits 判斷用的線，所以 max_dist_judging 是空的；",
      "被拿掉的步驟仍然看 steps_dropped。")
  } else {
    p(sprintf(paste0("這個量尺有 %d 個判斷的步驟，離判斷用的線的殘差標準差是 %.2f logit，",
                     "界線是 %s × %.2f = %.2f logit。"), jd$n, jd$sd, cut, jd$sd, jd$limit))
  }
  shiny::tagList(
    p("有兩條線。判斷用的線：用這個量尺所有判斷的步驟畫的線，離它超過界線",
      sprintf("（%s 個殘差標準差）的步驟被拿掉，那一題就是偏移。", cut),
      "最後的線：拿掉偏移的步驟以後重新畫的線，用來把這一回合的門檻換到題庫的量尺（偏移把關圖的實線）。"),
    limit,
    p("steps：這一題和題庫比較的步驟數；steps_judged：其中上下的人夠、有判斷的步驟數；",
      "steps_dropped：被拿掉的步驟（分數）。",
      "max_dist_judging：判斷的步驟離判斷用的線最遠的距離（logit），這是判斷用的數字：",
      "偏移的題大於界線，其他判斷過的題不大於界線。",
      "max_dist_final（kits 裡叫 max_abs_resid）：所有步驟（包括沒有判斷的）離最後的線最遠的距離，只供參考。",
      "沒有判斷的步驟人很少、門檻不準，常常離線很遠但不算偏移；而偏移的步驟被拿掉以後，最後的線會稍微移動。",
      "所以 max_dist_final 不能拿來和界線比，沒有偏移的題也可能比偏移的題大。")
  )
}

#' 畫面上的偏移表：reason 換成中文
#' @noRd
items_drift_display <- function(rows) {
  if (!"reason_zh" %in% names(rows)) return(rows)
  rows$reason <- rows$reason_zh
  rows$reason_zh <- NULL
  rows$judging_limit <- NULL
  rows
}

#' 畫面上的題目參數：固定在題庫的值的步驟（SE 是 0）寫「固定」，其他小數 2 位
#' @noRd
items_coef_display <- function(co) {
  se <- \(x) dplyr::case_when(is.na(x) ~ "", x == 0 ~ "固定",
                               .default = formatC(x, format = "f", digits = 2))
  co$difficulty_se <- se(co$difficulty_se)
  co$threshold_se <- se(co$threshold_se)
  co
}

# 偏移把關圖下面的一句話
ITEMS_DRIFT_HINT <- paste0(
  "這張圖用的是固定之前的試算：先不固定任何題，只用這一波的資料估一次，和題庫比。",
  "固定之後，固定的題門檻就等於題庫的值（見「固定了哪些題」的核對）。",
  "虛線是判斷用的線，灰色帶子是它上下的容許範圍（圖例寫幾個標準差）。",
  "落在帶子裡的步驟，這一波的作答和題庫一致，所以可以放心固定。",
  "帶子外的步驟偏移了，那一題換成新版本；同一題的其他步驟也一起換，所以也標成橘色。",
  "實線是拿掉偏移的步驟之後，重新畫的線。"
)

#' 圖的說明（每種圖一兩句）
#' @noRd
items_plot_help <- function(type) {
  switch(
    type,
    category = paste("每一題每個分數的機率，隨能力值（theta）變化。",
                     "圖例的 Score 0、Score 1… 是 0 分、1 分…。"),
    wright = paste("左邊是受試者能力值（ability）的分布，右邊是各題步驟的 Thurstonian 門檻（threshold）。",
                   "兩邊用同一個縱軸，要對得上。"),
    fit = paste("每一題的 infit（圓點）與 outfit（方塊）；灰色帶是 0.6–1.4。",
                "點的顏色只看它自己（infit 或 outfit）在不在帶子裡。"),
    observed = paste("點是各波次的孩子實際通過（得 1 分以上）的百分比，虛線是模型預期的通過百分比。",
                     "兩者要接近。"),
    drift = paste(ITEMS_DRIFT_HINT, "點的顏色是「固定了哪些題」的狀態。",
                  "圖例：Fixed (bank value) 是固定在題庫的值，",
                  "Drifted -> new version 是偏移、換成新版本。")
  )
}

#' 可以畫的題（和 kits 的 plot() 一樣排序）
#' @noRd
items_plot_ids <- function(item, X) {
  ids <- sort(unique(stats::coef(item)$item_id))
  if (is.null(X)) ids else ids[ids %in% colnames(X)]
}

#' 第 page 頁的題（每頁 ITEMS_PAGE 題）
#' @noRd
items_page_ids <- function(ids, page) {
  from <- (page - 1) * ITEMS_PAGE + 1
  ids[seq(from, min(length(ids), from + ITEMS_PAGE - 1))]
}

#' 要畫的題：選了的（照 ids 的順序）；沒有選或選的都不在這個量尺時是第 1 頁
#' @noRd
items_shown <- function(ids, picked) {
  hit <- ids[ids %in% picked]
  if (length(hit) == 0) items_page_ids(ids, 1) else hit
}

#' 畫的題在全部的第幾題：連續時 c(from, to)，不連續時 NULL
#' @noRd
items_shown_range <- function(ids, shown) {
  at <- match(shown, ids)
  if (length(at) > 0 && all(diff(at) == 1)) c(at[1], at[length(at)])
}

#' 圖上面的一句話，例如「這張圖畫了第 1–9 題（共 44 題）」
#' @noRd
items_shown_text <- function(ids, shown) {
  r <- items_shown_range(ids, shown)
  total <- sprintf("（共 %d 題）", length(ids))
  if (is.null(r)) {
    return(sprintf("這張圖畫了 %d 題%s：%s。", length(shown), total, paste(shown, collapse = "、")))
  }
  which <- if (r[1] == r[2]) sprintf("第 %d 題", r[1]) else sprintf("第 %d–%d 題", r[1], r[2])
  paste0("這張圖畫了", which, total, "。")
}

#' 檔名裡畫的題，例如 "1-9"；不連續時是題的位置（"1_3_5"），太多時是題數（"12items"）
#' @noRd
items_shown_label <- function(ids, shown) {
  r <- items_shown_range(ids, shown)
  if (!is.null(r)) return(if (r[1] == r[2]) as.character(r[1]) else paste0(r[1], "-", r[2]))
  at <- sort(match(shown, ids))
  if (length(at) <= 10) paste(at, collapse = "_") else paste0(length(at), "items")
}

#' 選題：換頁的按鈕（每頁 9 題）與自己選的選單
#' @noRd
items_picker_ui <- function(ns, ids, shown) {
  n_page <- ceiling(length(ids) / ITEMS_PAGE)
  pages <- purrr::map_chr(seq_len(n_page), \(p) {
    at <- range(match(items_page_ids(ids, p), ids))
    if (at[1] == at[2]) sprintf("第 %d 題", at[1]) else sprintf("第 %d–%d 題", at[1], at[2])
  })
  now <- which(purrr::map_lgl(seq_len(n_page), \(p) identical(items_page_ids(ids, p), shown)))
  shiny::tags$div(
    class = "kit-plot-items",
    if (n_page > 1) {
      shiny::radioButtons(ns("plot_page"), "看哪幾題", stats::setNames(seq_len(n_page), pages),
                          selected = if (length(now) == 1) now else character(), inline = TRUE)
    },
    shiny::selectizeInput(ns("plot_items"), "或自己選題（可以選好幾題）", ids, shown,
                          multiple = TRUE, width = "100%")
  )
}

#' 適配度圖：infit 與 outfit 各自上色（kits 的圖用整題的 flag，infit 好好的也會是橘色）
#'
#' 用 kits 的 plot()：只留 infit（outfit 設成 NA）畫一次，只留 outfit 畫一次，再把點放在一起。
#' 圖例是「infit: ok」「outfit: underfit」這樣，顏色只看那個統計量。
#' @param itemfit kits_item()$itemfit
#' @noRd
items_fit_plot <- function(itemfit) {
  one <- \(keep) {
    x <- itemfit
    x[[setdiff(c("infit", "outfit"), keep)]] <- NA_real_
    plot(x, type = keep)
  }
  p <- one("infit")
  p$x$data <- c(p$x$data, one("outfit")$x$data)
  p
}

#' 「固定了哪些題」：兩句話（題數、核對）、不是固定的題的表，全部固定的題收在 <details> 裡
#' @param tab kitsui_item_status()；NULL 是停下來的量尺
#' @param check kitsui_fixed_check()；自由校準時 NULL
#' @noRd
items_fixed_ui <- function(tab, check) {
  if (is.null(tab)) {
    return(shiny::tags$p(class = "kit-hint", "這個量尺停下來了，沒有每一題的狀態。"))
  }
  txt <- kitsui_status_text(tab, check)
  calm <- tab$kind %in% c(KITSUI_STATUS_FIXED, "free")
  top <- tab[!calm, ]
  rest <- tab[calm, ]
  what <- if (any(rest$kind == "free")) "自由校準" else "固定"
  verify <- if (is.null(txt$verify)) {
    NULL
  } else if (txt$ok) {
    kitDesign::kit_callout(shiny::tags$strong("核對："), txt$verify)
  } else {
    kitDesign::kit_callout(shiny::tags$strong("固定的步驟和題庫不同："), txt$verify,
                           "這些步驟應該沿用題庫的值。請把這個量尺與下面的步驟告訴維護 kitsUI 的人。",
                           shiny::tags$ul(lapply(kitsui_fixed_bad_lines(check$bad),
                                                 shiny::tags$li)),
                           warn = TRUE)
  }
  shiny::tagList(
    shiny::tags$p(txt$main),
    verify,
    if (nrow(top) > 0) {
      items_html_table(top)
    } else {
      shiny::tags$p(class = "kit-hint", "沒有偏移、新題或拿掉的題。")
    },
    if (nrow(rest) > 0) {
      shiny::tags$details(
        shiny::tags$summary(sprintf("看全部%s的題（%d 題）", what, nrow(rest))),
        items_html_table(rest, tall = TRUE)
      )
    }
  )
}

#' 每一題的狀態表（HTML，畫面用中文欄名）：題號、狀態、步驟數、說明；tall 時是可以捲動的長表
#' @noRd
items_html_table <- function(tab, tall = FALSE) {
  cols <- c("item_id", "status", "steps", "detail")
  cell <- \(x) if (is.na(x)) "" else as.character(x)
  rows <- lapply(seq_len(nrow(tab)), \(i) {
    shiny::tags$tr(lapply(cols, \(col) shiny::tags$td(cell(tab[[col]][i]))))
  })
  shiny::tags$div(
    class = paste("kit-table-scroll", if (tall) "kit-table-tall"),
    shiny::tags$table(
      class = "table table-sm table-striped",
      shiny::tags$thead(shiny::tags$tr(lapply(unname(KITSUI_SCREEN_NAMES[cols]), shiny::tags$th))),
      shiny::tags$tbody(rows)
    )
  )
}
