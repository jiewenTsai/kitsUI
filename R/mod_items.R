# ② 題目校準：跑一輪（自由校準或偏移把關 + 固定校準），每個量尺看結果

# 圖的種類
ITEMS_PLOTS <- c(
  "類別機率曲線" = "category", "題目與受試者分布（Wright map）" = "wright",
  "適配度（infit、outfit）" = "fit", "觀察與預期（依波次）" = "observed"
)
# 一次畫幾題（類別機率曲線、觀察與預期）
ITEMS_PAGE <- 9
# 要選題的圖
ITEMS_PAGED <- c("category", "observed")
# flag 的量尺怎麼辦（一句話，各處相同）
ITEMS_FLAG_NOTE <- "被 flag 的量尺照樣往下做；請看偏移的題，必要時在下一輪處理。"

#' @noRd
mod_items_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_guide(
      goal = "估計每一題的難度（校準）。有固定檔時，先檢查舊題有沒有偏移。",
      steps = c(
        "看「用哪一個題庫」：預設照月齡的規則，需要時可以改。",
        "按「開始校準」。",
        "選一個量尺，看它是自由校準還是固定校準。",
        "看偏移表：偏移的題會換成新版本，舊的參數不動。",
        "看圖與適配度表，確認題目合理。",
        "橘底的「flag」是要特別看一下的量尺；它照常校準。",
        "沒有紅字時，按「下一步：連結」。"
      ),
      output = "這一輪每個量尺的題目參數。新的題目步驟會加進固定檔的題庫。"
    ),
    shiny::tags$details(
      shiny::tags$summary("進階設定（flag 的條件）"),
      shiny::numericInput(ns("flag_share"), "偏移題超過已判斷題的比例", 0.2, 0, 1, 0.05),
      shiny::numericInput(ns("flag_fixed"), "可固定的題少於幾題", 5, 1, 50, 1),
      shiny::tags$small(
        class = "kit-hint",
        "偏移題太多，或可固定的題太少，這個量尺會被 flag。",
        ITEMS_FLAG_NOTE,
        "只有 kits 出錯、沒有可固定的題，或沒有題可以估計時，量尺才會停下來。"
      )
    ),
    shiny::h4("用哪一個題庫"),
    shiny::tags$p(class = "kit-hint",
                  "預設照月齡的規則。",
                  "自己的題庫沒有校準過這個月齡、另一個世代的有時，借用另一世代題庫。",
                  "要改的話，在 bank 欄選別的題庫，再按「開始校準」。"),
    shiny::uiOutput(ns("banks")),
    shiny::actionButton(ns("run"), "開始校準", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    shiny::tableOutput(ns("overview")),
    shiny::uiOutput(ns("result"))
  )
}

#' @param data mod_data_server() 回傳的 reactive
#' @param goto function(tab) 切換分頁
#' @return reactive：kitsui_round() 的結果；還沒校準時是 NULL
#' @noRd
mod_items_server <- function(id, data, goto) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    round <- shiny::reactiveVal(NULL)
    used <- shiny::reactiveVal(NULL)
    # 給使用者看的訊息：ui_alert()
    note <- shiny::reactiveVal(NULL)
    say <- \(type, head, text) note(ui_alert(type, head, " ", text))

    # 資料或設定變了，舊結果作廢
    shiny::observeEvent(data(), {
      if (!is.null(shiny::isolate(round()))) {
        round(NULL)
        say("info", "【需要重新校準】", "資料或設定改了，舊的結果已清除。請再按「開始校準」。")
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
      if (is.null(b)) return(shiny::tags$p(class = "kit-hint", "資料還不能用。"))
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

    shiny::observeEvent(input$run, {
      d <- data()
      if (is.null(d)) {
        say("danger", "【沒有完成】", "資料還不能用。請回到 ① 資料，看紅色的訊息。")
        return()
      }
      choice <- bank_choice()
      res <- shiny::withProgress(message = "題目校準中…", value = 0, {
        tryCatch(
          suppressMessages(kitsui_round(
            d$df, d$fixed, d$round_label, flag_share = input$flag_share %||% 0.2,
            flag_fixed = input$flag_fixed %||% 5, files = d$files, progress = items_progress,
            bank_choice = choice
          )),
          error = \(e) e
        )
      })
      if (inherits(res, "error")) {
        round(NULL)
        say("danger", "【沒有完成】", kit_error_text(res, "校準"))
        return()
      }
      note(NULL)
      used(d)
      round(res)
    })

    output$status <- shiny::renderUI({
      if (!is.null(note())) return(note())
      res <- round()
      if (is.null(res)) {
        return(shiny::tags$p(class = "kit-hint", "還沒有校準。按「開始校準」。"))
      }
      items_status(res)
    })
    output$overview <- kit_table({
      res <- round()
      shiny::req(res)
      items_overview(res)
    })

    scales <- shiny::reactive({
      res <- round()
      shiny::req(res)
      # 校準的量尺（借用另一世代題庫時是另一個世代的量尺）
      sc <- unique(res$plan$scale %||% used()$scales$scale)
      sc[sc %in% c(names(res$items), res$stopped)]
    })

    output$result <- shiny::renderUI({
      shiny::req(round())
      sc <- scales()
      pick <- shiny::isolate(input$scale)
      if (!isTRUE(pick %in% sc)) pick <- sc[1]
      items_result_ui(ns, sc, pick)
    })

    sel <- shiny::reactive({
      res <- round()
      shiny::req(res, input$scale %in% scales())
      s <- input$scale
      list(scale = s, item = res$items[[s]], drift = res$drift[[s]], X = res$data[[s]],
           log = dplyr::filter(res$log, .data$scale == s), stopped = s %in% res$stopped)
    })
    persons <- shiny::reactive(kitsui_persons(shiny::req(used())$df))
    # 這一輪這個量尺的分數：算一次存在 round()$memo（③、④ 也用）
    scores <- shiny::reactive({
      x <- sel()
      shiny::req(x$item)
      kitsui_own_scores(round(), x$scale, persons())
    })

    output$scale_info <- shiny::renderUI(items_scale_info(sel()))

    # 偏移表：畫面上的 reason 是中文；csv 多了 scale 與 kits 的英文 reason
    drift_rows <- shiny::reactive(items_drift_rows(sel()))
    drift_csv <- shiny::reactive(items_with_scale(drift_rows(), sel()$scale))
    output$drift <- kit_table(items_drift_display(drift_rows()))

    # 題目參數：畫面上固定的步驟寫「固定」；csv 保留數字
    coef_raw <- shiny::reactive({
      x <- sel()
      shiny::validate(shiny::need(x$item, "這個量尺停下來了，沒有題目參數。"))
      as.data.frame(stats::coef(x$item))
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
      p <- shiny::withProgress(message = "畫圖中…", value = 0.5, tryCatch(switch(
        type,
        wright = plot(x$item, type = "wright", scores = scores()),
        observed = plot(x$item, type = "observed", data = x$X, group = "wave",
                        persons = persons(), scores = scores(), items = shown()),
        fit = items_fit_plot(x$item$itemfit),
        plot(x$item, type = "category", items = shown())
      ), error = \(e) e))
      if (inherits(p, "error")) shiny::validate(paste("這張圖畫不出來：", conditionMessage(p)))
      p
    })
    output$plot <- plotly::renderPlotly(plot_obj())

    # 檔名跟著量尺、圖的種類（還沒選圖時是第一種）與畫的題
    name <- \(what) \() paste0(what, "_", sel()$scale)
    plot_name <- \() {
      type <- input$plot_type %||% "category"
      which <- if (type %in% ITEMS_PAGED) paste0("_", items_shown_label(plot_ids(), shown()))
      paste0("item_", sel()$scale, "_", type, which)
    }
    output$dl_plot <- download_plot(plot_obj, plot_name)
    output$dl_coef <- download_csv(coef_csv, name("item_coef"))
    output$dl_drift <- download_csv(drift_csv, name("drift"))
    output$dl_fit <- download_csv(fit_csv, name("item_fit"))
    output$dl_log <- download_csv(\() items_log_csv(round()$log),
                                  \() paste0("round_log_", used()$round_label))

    output$check <- kit_table({
      x <- sel()
      checks_or_ok(check_table(item = x$item, drift = x$drift))
    })

    output$code <- shiny::renderText({
      x <- sel()
      d <- used()
      plan <- round()$plan
      code <- kitsui_code_round(
        x$scale, d$round_label, fixed = d$fixed, fixed_file = d$fixed_file %||% "fixed_set.zip",
        data_file = d$data_file, waves = d$waves, flag_share = input$flag_share %||% 0.2,
        flag_fixed = input$flag_fixed %||% 5, plan = plan, picks = d$picks
      )
      info <- if (!is.null(x$item)) {
        paste("#", utils::capture.output(print(kits::kits_info(x$item))))
      }
      choice <- if (!is.null(plan) && any(plan$manual)) {
        c("# 這一次手動選了題庫（kitsui_round(bank_choice = )）：",
          paste("#", unique(plan$why[plan$manual])))
      }
      paste(c(code, "", choice, "# 這一次在 app 裡的設定（kits_info）", info), collapse = "\n")
    })

    output$next_step <- shiny::renderUI({
      res <- round()
      if (!isTRUE(res$ok)) return(NULL)
      done <- if (length(res$items) == 1) "這個量尺校準好了。" else "每個量尺都校準好了。"
      ui_next_step(paste0(done, "下一步是連結。"), ns("go_link"), "下一步：連結")
    })
    shiny::observeEvent(input$go_link, goto("link"))

    round
  })
}

#' ② 一個量尺的結果：摘要、偏移表、題目參數、紀錄、圖、適配度、檢查、程式
#' @noRd
items_result_ui <- function(ns, sc, pick) {
  hint <- \(...) shiny::tags$p(class = "kit-hint", ...)
  scroll <- \(id) shiny::tags$div(class = "kit-table-scroll kit-table-tall", shiny::tableOutput(id))
  dl <- \(id, label) shiny::tags$div(class = "kit-downloads", ui_download(id, label))
  cut <- kitsui_drift_cut()
  min_n <- eval(formals(kits::kits_drift)$min_n)
  shiny::tagList(
    shiny::hr(),
    shiny::selectInput(ns("scale"), "看哪一個量尺", sc, pick),
    shiny::uiOutput(ns("scale_info")),
    shiny::h4("偏移把關"),
    hint("kits_drift() 先把這一輪的資料自由校準，得到每一步的門檻。",
         "再用 mean/sigma 畫一條線，把新的門檻換到題庫的量尺上，和題庫的門檻比。",
         sprintf("離線的距離大於 %s 個標準差（所有判斷的步驟算的）的步驟會被拿掉。", cut),
         "有步驟被拿掉的題就是 drift = TRUE，會換成新版本重新估計。",
         "max_abs_resid 是這一題各步驟離線最遠的距離（logit，題庫的量尺）。",
         sprintf("步驟的上下各要有 %s 人才判斷；不判斷的題 drift 是空的，reason 寫原因。", min_n),
         "suggest_fix = TRUE 的題固定在題庫的值（沿用舊參數）。"),
    scroll(ns("drift")),
    dl(ns("dl_drift"), "下載偏移表（csv）"),
    shiny::h4("題目參數"),
    hint("每一題每一個步驟一列（coef(item)）。",
         "beta 是步驟難度（logit）。",
         "thr 是門檻：能力值（theta）到這裡時，有一半的機會得到這個分數以上。",
         "_se 是標準誤。",
         "固定在題庫的值的步驟沒有標準誤，表上寫「固定」（csv 裡是 0）。"),
    scroll(ns("coef")),
    dl(ns("dl_coef"), "下載題目參數（csv）"),
    shiny::h4("這一輪的紀錄"),
    hint("新版本、併分數、分數類別的問題、借用另一世代題庫、停下來的原因，都會寫進固定檔的 checks.csv。",
         "status 是 warn 的列要特別看一下。"),
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("log"))),
    dl(ns("dl_log"), "下載這一輪的紀錄（csv，所有量尺）"),
    shiny::h4("圖"),
    shiny::radioButtons(ns("plot_type"), NULL, ITEMS_PLOTS, inline = TRUE),
    shiny::uiOutput(ns("plot_help")),
    shiny::uiOutput(ns("plot_items_ui")),
    shiny::uiOutput(ns("plot_note")),
    plotly::plotlyOutput(ns("plot"), height = "520px"),
    dl(ns("dl_plot"), "下載這張圖（html，畫的是上面選的題）"),
    shiny::h4("題目適配度"),
    hint("n 是用到的作答數。",
         "pvalue 是平均得分占最高分的比例；只有 0、1 分的題，就是通過的比例。",
         "rir 是這一題和其他題總分的相關；接近 0 或是負的，要看一下題目。",
         "infit、outfit 接近 1 最好。",
         "flag 是 kits 依 infit、outfit 給的判斷：兩個都在 0.6–1.4 之間是 ok。",
         "任一個大於 1.4 是 underfit（比模型預期亂），小於 0.6 是 overfit（比模型預期整齊）。"),
    scroll(ns("fit")),
    dl(ns("dl_fit"), "下載適配度（csv）"),
    ui_check_panel(ns("check"), "kits 的檢查（kits_check）"),
    ui_code_panel(ns("code")),
    shiny::uiOutput(ns("next_step"))
  )
}

#' 進度條：kitsui_round() 每個量尺開始前呼叫一次
#' @noRd
items_progress <- function(i, n, scale) {
  shiny::incProgress(1 / n, detail = sprintf("校準第 %d 個量尺（共 %d 個）：%s", i, n, scale))
}

#' 一輪的整體狀態：全部完成（綠），或有量尺停下來（紅，不能產生新的固定檔）；
#' 有分數類別問題的題時說怎麼處理；有 flag 的量尺時另一個橘底的框
#' @noRd
items_status <- function(res) {
  n_ok <- length(res$items)
  extra <- shiny::tagList(items_category_note(res$log), items_flag_box(res$log))
  if (isTRUE(res$ok)) {
    done <- if (n_ok == 1) " 這個量尺校準好了。" else sprintf(" %d 個量尺都校準好了。", n_ok)
    return(shiny::tagList(ui_alert("success", "【完成】", done), extra))
  }
  why <- dplyr::filter(res$log, .data$check %in% c("stop", "error"))
  shiny::tagList(
    ui_alert(
      "danger", "【有量尺停下來】",
      " 這一輪不能產生新的固定檔。",
      "請看下面的原因，處理後重跑這一輪。",
      shiny::tags$ul(lapply(seq_len(nrow(why)), \(i) {
        kind <- if (why$check[i] == "error") "kits 出錯" else "停下來"
        shiny::tags$li(shiny::tags$strong(why$scale[i]), "（", kind, "）：", why$detail[i])
      })),
      if (n_ok > 0) shiny::tags$p(sprintf("其他 %d 個量尺有校準結果，可以先看。", n_ok))
    ),
    extra
  )
}

#' 分數類別有問題的題：一句話說怎麼處理（沒有時 NULL）
#' @noRd
items_category_note <- function(log) {
  kept <- sum(log$check == "category problem, kept fixed")
  out <- sum(log$check == "category problem, left out")
  if (kept + out == 0) return(NULL)
  shiny::tags$p(
    class = "kit-hint",
    sprintf("有 %d 題分數類別有問題（沒有人得 0 分、分數不連續或只有一種分數）。", kept + out),
    if (kept > 0) sprintf("題庫裡的 %d 題固定在題庫的值。", kept),
    if (out > 0) sprintf("不在題庫裡的 %d 題這一輪不放進來。", out),
    "分數都沒有改；每一題的原因見下面各量尺的紀錄。"
  )
}

#' flag 的量尺：橘底框，每個量尺一列，加上一句怎麼辦（沒有時 NULL）
#' @noRd
items_flag_box <- function(log) {
  flags <- dplyr::filter(log, .data$check == "flag")
  if (nrow(flags) == 0) return(NULL)
  shiny::tags$div(
    class = "kit-callout kit-callout-warn", role = "status",
    shiny::tags$strong("flag"),
    shiny::tags$ul(lapply(seq_len(nrow(flags)), \(i) {
      shiny::tags$li(class = "kit-warn-text", paste0(flags$scale[i], "（flag）：", flags$detail[i]))
    })),
    shiny::tags$p(ITEMS_FLAG_NOTE)
  )
}

#' 量尺一覽（欄名是英文）：校準的量尺、哪些資料、校準方式、flag
#' @noRd
items_overview <- function(res) {
  plan <- res$plan
  purrr::map(unique(plan$scale), \(s) {
    p <- plan[plan$scale == s, ]
    log <- res$log[res$log$scale == s, ]
    pick <- \(check) paste(log$detail[log$check == check], collapse = "；")
    tibble::tibble(
      scale = s, data = paste(p$data_scale, format(p$wave_m), collapse = "、"),
      calibration = if (s %in% res$stopped) "停下來" else pick("calibration"),
      flag = pick("flag")
    )
  }) |>
    purrr::list_rbind()
}

#' 選題庫的表：資料量尺、波次、題庫（選單）、原因（跟著選單變，why_i）；欄名是英文
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
    shiny::tags$thead(shiny::tags$tr(th("scale"), th("waves"), th("bank"), th("why"))),
    shiny::tags$tbody(rows)
  )
}

#' 一個量尺的摘要：校準方式、借用另一世代題庫、偏移、新版本、分數類別、併分數；flag 橘底
#' @noRd
items_scale_info <- function(x) {
  log <- x$log
  n <- \(check) sum(log$check == check)
  # 「：3 題（a1、a2、a3）」
  count <- \(head, check) {
    it <- log$item_id[log$check == check]
    which <- if (length(it) > 0) paste0("（", paste(it, collapse = "、"), "）")
    shiny::tags$li(paste0(sprintf("%s：%d 題", head, length(it)), which))
  }
  stopped <- is.null(x$item)
  mode <- if (stopped) "停下來了（沒有校準結果）" else log$detail[log$check == "calibration"][1]
  flags <- log$detail[log$check == "flag"]
  shiny::tagList(
    shiny::tags$div(
      class = if (stopped) "alert alert-danger" else "kit-callout",
      shiny::tags$strong(x$scale, "："), mode,
      shiny::tags$ul(
        if (!stopped) shiny::tags$li(sprintf("%d 列次、%d 題", nrow(x$X), ncol(x$X))),
        lapply(log$detail[log$check == "borrowed bank"],
               \(d) shiny::tags$li(paste0("借用另一世代題庫：", d))),
        shiny::tags$li(sprintf("偏移：%d 題", n("drift"))),
        shiny::tags$li(sprintf("換成新版本：%d 題", n("new version"))),
        count("估不出來、固定在題庫的值", "no CML estimate, kept fixed"),
        count("分數類別有問題、固定在題庫的值", "category problem, kept fixed"),
        count("分數類別有問題、這一輪不放進來", "category problem, left out"),
        shiny::tags$li(sprintf("併分數（新步驟有缺口）：%d 題", n("capped")))
      )
    ),
    if (length(flags) > 0) {
      shiny::tags$div(
        class = "kit-callout kit-callout-warn", role = "status",
        shiny::tags$p(class = "kit-warn-text", paste0("flag：", flags, collapse = "；")),
        shiny::tags$p(ITEMS_FLAG_NOTE)
      )
    }
  )
}

# 這一輪的紀錄表：加上 status（flag 是 warn；stop、error 是 error；其他 ok）
items_log_table <- function(log) {
  status <- dplyr::case_when(
    log$check == "flag" ~ "warn",
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
  "score range differs" = "這一輪有比題庫更高的分數：舊的步驟固定，新的步驟另外估",
  "no 0 score or one score only" = "沒有人得 0 分或只有一種分數：固定在題庫的值",
  "score gap" = "分數不連續：不判斷",
  "too little information" = "步驟上下的人不夠：固定在題庫的值"
)

#' 偏移表的列：kits_drift() 的 items，加上沒有交給 kits_drift() 的題庫題（分數類別有問題、
#' 估不出來，固定在題庫的值）；reason_zh 是中文原因
#' @param x sel()：drift、log、item、stopped
#' @return data.frame；沒有偏移表時一列 note（自由校準，或停下來了）
#' @noRd
items_drift_rows <- function(x) {
  if (is.null(x$drift)) {
    note <- if (isTRUE(x$stopped)) {
      "這個量尺停下來了，沒有偏移表。原因見上面的紅字與這一輪的紀錄。"
    } else {
      "沒有題庫：自由校準，不用檢查偏移。"
    }
    return(data.frame(note = note))
  }
  items <- as.data.frame(x$drift$items)
  checks <- c("category problem, kept fixed", "no CML estimate, kept fixed")
  kept <- x$log[x$log$check %in% checks & !x$log$item_id %in% items$item_id, ]
  k <- nrow(kept)
  extra <- data.frame(item_id = kept$item_id, max_abs_resid = rep(NA_real_, k),
                      drift = rep(NA, k), suggest_fix = rep(TRUE, k), reason = kept$check)
  out <- dplyr::bind_rows(items, extra)
  zh <- unname(ITEMS_REASON_ZH[out$reason])
  zh[out$reason == ""] <- ""
  zh[out$reason %in% checks] <- kept$detail
  out$reason_zh <- dplyr::coalesce(zh, out$reason)
  out
}

#' 畫面上的偏移表：reason 換成中文
#' @noRd
items_drift_display <- function(rows) {
  if (!"reason_zh" %in% names(rows)) return(rows)
  rows$reason <- rows$reason_zh
  rows$reason_zh <- NULL
  rows
}

#' 畫面上的題目參數：固定在題庫的值的步驟（SE 是 0）寫「固定」，其他小數 2 位
#' @noRd
items_coef_display <- function(co) {
  se <- \(x) dplyr::case_when(is.na(x) ~ "", x == 0 ~ "固定",
                               .default = formatC(x, format = "f", digits = 2))
  co$beta_se <- se(co$beta_se)
  co$thr_se <- se(co$thr_se)
  co
}

#' 圖的說明（每種圖一兩句）
#' @noRd
items_plot_help <- function(type) {
  switch(
    type,
    category = "每一題每個分數的機率，隨能力值（theta）變化。",
    wright = paste("左邊是受試者能力值（theta）的分布，右邊是各題步驟的門檻（thr）。",
                   "兩邊用同一個縱軸，要對得上。"),
    fit = paste("每一題的 infit（圓點）與 outfit（方塊）；灰色帶是 0.6–1.4。",
                "點的顏色只看它自己（infit 或 outfit）在不在帶子裡。"),
    observed = paste("點是各波次的孩子實際通過（得 1 分以上）的百分比，虛線是模型預期的通過百分比。",
                     "兩者要接近。")
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
