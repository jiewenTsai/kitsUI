# ④ 受試者評分：這一回合的分數（kits_subj() -> 內部連結 -> 外部連結）、分數對照表、新的固定檔
# 早先各回合的分數不重算：每一回合的分數表要在那一回合存下來

# 分數表一頁幾列
SCORE_PAGE_ROWS <- 20L

# 新固定檔的下載鈕
# 原文：下載新的固定檔（zip）
SCORE_FIXED_LABEL <- "\u4e0b\u8f09\u65b0\u7684\u56fa\u5b9a\u6a94\uff08zip\uff09"
# ④ 的兩個下載是這一頁做完後的主角（KIT 設計語言 2.1）：主要按鈕（.kit-download.btn-primary）
SCORE_DL_CLASS <- "btn-primary"
# 分數摘要畫面上最多幾列（其他收在「全部（N 列）」）
SCORE_SUMMARY_ROWS <- 20L

#' @noRd
mod_score_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    score_guide(),
    shiny::uiOutput(ns("source")),
    shiny::uiOutput(ns("flags")),
    shiny::uiOutput(ns("advanced")),
    shiny::uiOutput(ns("settings")),
    # 原文：計算這一回合的分數
    kitDesign::kit_button(ns("run"), "\u8a08\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578", "primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("done")),
    shiny::uiOutput(ns("result")),
    shiny::uiOutput(ns("next_step"))
  )
}

#' 這一頁的說明
#' @noRd
score_guide <- function() {
  kitDesign::kit_tab_guide(
    # 原文：算這一回合每個列次的分數。
    goal = "\u7b97\u9019\u4e00\u56de\u5408\u6bcf\u500b\u5217\u6b21\u7684\u5206\u6578\u3002",
    steps = c(
      # 原文：需要時在「進階設定」改通過步驟（只影響題目表與 age50）。
      "\u9700\u8981\u6642\u5728\u300c\u9032\u968e\u8a2d\u5b9a\u300d\u6539\u901a\u904e\u6b65\u9a5f\uff08\u53ea\u5f71\u97ff\u984c\u76ee\u8868\u8207 age50\uff09\u3002",
      # 原文：按「計算這一回合的分數」。
      "\u6309\u300c\u8a08\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u300d\u3002",
      # 原文：看分數摘要：每個量尺、世代、波次的人數、平均與標準差。
      "\u770b\u5206\u6578\u6458\u8981\uff1a\u6bcf\u500b\u91cf\u5c3a\u3001\u4e16\u4ee3\u3001\u6ce2\u6b21\u7684\u4eba\u6578\u3001\u5e73\u5747\u8207\u6a19\u6e96\u5dee\u3002",
      # 原文：下載這一回合的分數表。早先各回合的分數不會重算，所以請現在就存好。
      "\u4e0b\u8f09\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u8868\u3002\u65e9\u5148\u5404\u56de\u5408\u7684\u5206\u6578\u4e0d\u6703\u91cd\u7b97\uff0c\u6240\u4ee5\u8acb\u73fe\u5728\u5c31\u5b58\u597d\u3002",
      # 原文：下載新的固定檔（zip），下一回合要用。
      "\u4e0b\u8f09\u65b0\u7684\u56fa\u5b9a\u6a94\uff08zip\uff09\uff0c\u4e0b\u4e00\u56de\u5408\u8981\u7528\u3002",
      # 原文：需要時看每個量尺換到哪一把尺：校準的量尺 → 內部連結（參照世代以外的世代才有）→ 
      paste0("\u9700\u8981\u6642\u770b\u6bcf\u500b\u91cf\u5c3a\u63db\u5230\u54ea\u4e00\u628a\u5c3a\uff1a\u6821\u6e96\u7684\u91cf\u5c3a \u2192 \u5167\u90e8\u9023\u7d50\uff08\u53c3\u7167\u4e16\u4ee3\u4ee5\u5916\u7684\u4e16\u4ee3\u624d\u6709\uff09\u2192 ",
             # 原文：外部連結（到 D；參照世代有 D 分數的錨題時才有）。
             "\u5916\u90e8\u9023\u7d50\uff08\u5230 D\uff1b\u53c3\u7167\u4e16\u4ee3\u6709 D \u5206\u6578\u7684\u9328\u984c\u6642\u624d\u6709\uff09\u3002"),
      # 原文：要接著加新的波次，按「用這一回合的固定檔開始下一回合」。
      "\u8981\u63a5\u8457\u52a0\u65b0\u7684\u6ce2\u6b21\uff0c\u6309\u300c\u7528\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\u958b\u59cb\u4e0b\u4e00\u56de\u5408\u300d\u3002"
    ),
    output = paste0(
      # 原文：這一回合的分數表、分數對照表、題目表，與新的固定檔。
      "\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u8868\u3001\u5206\u6578\u5c0d\u7167\u8868\u3001\u984c\u76ee\u8868\uff0c\u8207\u65b0\u7684\u56fa\u5b9a\u6a94\u3002",
      # 原文：還沒有外部連結的量尺，分數留在自己的量尺上。
      "\u9084\u6c92\u6709\u5916\u90e8\u9023\u7d50\u7684\u91cf\u5c3a\uff0c\u5206\u6578\u7559\u5728\u81ea\u5df1\u7684\u91cf\u5c3a\u4e0a\u3002",
      # 原文：外部連結不適用時（沒有 D 分數的錨題），最後的分數是參照世代量尺上的 ref_score。
      "\u5916\u90e8\u9023\u7d50\u4e0d\u9069\u7528\u6642\uff08\u6c92\u6709 D \u5206\u6578\u7684\u9328\u984c\uff09\uff0c\u6700\u5f8c\u7684\u5206\u6578\u662f\u53c3\u7167\u4e16\u4ee3\u91cf\u5c3a\u4e0a\u7684 ref_score\u3002"
    ),
    note = paste0(
      # 原文：能力值（ability，kits 裡叫 theta）在校準的量尺上；換到 D 後是量尺分數（d_score）。
      "\u80fd\u529b\u503c\uff08ability\uff0ckits \u88e1\u53eb theta\uff09\u5728\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\uff1b\u63db\u5230 D \u5f8c\u662f\u91cf\u5c3a\u5206\u6578\uff08d_score\uff09\u3002",
      # 原文：ability_se、d_score_se 是它們的標準誤。
      "ability_se\u3001d_score_se \u662f\u5b83\u5011\u7684\u6a19\u6e96\u8aa4\u3002",
      # 原文：se_link 是連結的誤差，se_total 是兩者合起來。
      "se_link \u662f\u9023\u7d50\u7684\u8aa4\u5dee\uff0cse_total \u662f\u5169\u8005\u5408\u8d77\u4f86\u3002"
    )
  )
}

#' @param data mod_data_server() 的回傳值：reactive list(df, fixed, round_label, files, persons)
#' @param items mod_items_server() 的回傳值：reactive，kitsui_round() 的結果
#' @param links mod_link_server() 的回傳值：reactive list(fixed, links)；NULL 時用 items()$fixed
#' @param goto function(tab) 切換分頁
#' @param next_round NULL 或 function(fixed, label)：按「用這一回合的固定檔開始下一回合」時呼叫，
#'   把新的固定檔（在記憶體裡）交給 ① 資料
#' @return reactive：kitsui_score() 的結果（還沒計算時為 NULL）
#' @noRd
mod_score_server <- function(id, data, items, links, goto, next_round = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(NULL)
    # 校準量尺上的分數：題目校準沒變就不重算（連結變了只重做換算）
    own <- shiny::reactiveVal(NULL)

    fixed <- shiny::reactive({
      lk <- links()
      if (!is.null(lk$fixed)) lk$fixed else items()$fixed
    })
    # 連結：③ 已經建好時直接用；沒有時一組固定檔只建一次
    link_objs <- shiny::reactive({
      lk <- links()
      if (!is.null(lk$fixed) && !is.null(lk$links)) return(lk$links)
      kitsui_links(shiny::req(fixed()))
    })
    # ① 算好的列次資訊；沒有時從資料算
    persons <- shiny::reactive(data()$persons %||% kitsui_persons(data()$df))
    label <- shiny::reactive(score_round_label(data(), fixed()))
    # ② 沒有一個量尺校準好（全部停下來）：沒有東西可以評分，「計算」不能按
    nothing <- shiny::reactive({
      r <- items()
      !is.null(r) && length(r$items) == 0
    })
    output$source <- shiny::renderUI({
      if (is.null(items())) return(ui_go_items(ns))
      if (nothing()) score_nothing_status(items(), ns)
    })
    shiny::observe(shiny::updateActionButton(session, "run", disabled = nothing()))
    output$settings <- shiny::renderUI({
      if (is.null(items())) return(NULL)
      kitDesign::kit_settings_line(kitsui_settings_text(data()), prefix = NULL)
    })
    output$flags <- shiny::renderUI(score_flags_ui(kitsui_round_flags(items())))
    shiny::observeEvent(input$go_items, goto("items"))

    # 題目校準變了：舊的分數不算數，要重算。② 沒有結果了（資料或設定改了、還沒重新校準）
    # 也一樣，舊的分數不留在畫面上
    waiting_items <- FALSE
    shiny::observeEvent(items(), {
      own(NULL)
      if (!is.null(result()) || waiting_items) {
        result(NULL)
        waiting_items <<- is.null(items())
        todo <- if (waiting_items) {
          # 原文：請先到「② 題目校準」再按一次「開始校準」，再回來按「計算這一回合的分數」。
          "\u8acb\u5148\u5230\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\u518d\u6309\u4e00\u6b21\u300c\u958b\u59cb\u6821\u6e96\u300d\uff0c\u518d\u56de\u4f86\u6309\u300c\u8a08\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u300d\u3002"
        } else {
          # 原文：請按「計算這一回合的分數」。
          "\u8acb\u6309\u300c\u8a08\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u300d\u3002"
        }
        status(kitDesign::kit_status_ui(
          # 原文：題目校準有變動，分數要重新計算。
          kitDesign::kit_status("stale", "\u984c\u76ee\u6821\u6e96\u6709\u8b8a\u52d5\uff0c\u5206\u6578\u8981\u91cd\u65b0\u8a08\u7b97\u3002"), shiny::tags$p(todo)
        ))
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    # 只有連結變了：分數不重算，只用新的連結重新換算（幾乎不花時間）
    shiny::observeEvent(fixed(), {
      if (is.null(result()) || is.null(own())) return()
      if (identical(result()$links_rows, fixed()$links)) return()
      out <- tryCatch(kitsui_score_convert(own(), fixed(), link_objs(), label()),
                      error = function(e) e)
      if (inherits(out, "error")) {
        result(NULL)
        # 原文：連結有變動，分數要重新計算。
        status(kitDesign::kit_status_ui(kitDesign::kit_status("stale", "\u9023\u7d50\u6709\u8b8a\u52d5\uff0c\u5206\u6578\u8981\u91cd\u65b0\u8a08\u7b97\u3002"),
                         # 原文：請按「計算這一回合的分數」。
                         shiny::tags$p("\u8acb\u6309\u300c\u8a08\u7b97\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u300d\u3002")))
        return()
      }
      result(c(out, list(links_rows = fixed()$links)))
      # 原文：連結有變動，分數已用新的連結重新換算（沒有重算分數）。
      status(kitDesign::kit_status_ui(kitDesign::kit_status("done", "\u9023\u7d50\u6709\u8b8a\u52d5\uff0c\u5206\u6578\u5df2\u7528\u65b0\u7684\u9023\u7d50\u91cd\u65b0\u63db\u7b97\uff08\u6c92\u6709\u91cd\u7b97\u5206\u6578\uff09\u3002")))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      round <- items()
      if (is.null(round)) {
        # 原文：還沒有這一回合的題目校準，沒有計算分數。
        status(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u9084\u6c92\u6709\u9019\u4e00\u56de\u5408\u7684\u984c\u76ee\u6821\u6e96\uff0c\u6c92\u6709\u8a08\u7b97\u5206\u6578\u3002"),
                         # 原文：怎麼辦：先到「② 題目校準」按「開始校準」。
                         shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u5148\u5230\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002")))
        # 原文：還沒有這一回合的題目校準，沒有計算分數。
        kitsui_after_run("stop", "\u9084\u6c92\u6709\u9019\u4e00\u56de\u5408\u7684\u984c\u76ee\u6821\u6e96\uff0c\u6c92\u6709\u8a08\u7b97\u5206\u6578\u3002")
        return()
      }
      if (nothing()) {
        status(score_nothing_status(round, ns))
        # 原文：沒有可以評分的量尺，所以沒有分數。
        kitsui_after_run("stop", "\u6c92\u6709\u53ef\u4ee5\u8a55\u5206\u7684\u91cf\u5c3a\uff0c\u6240\u4ee5\u6c92\u6709\u5206\u6578\u3002")
        return()
      }
      # 原文：正在計算分數…
      out <- shiny::withProgress(message = "\u6b63\u5728\u8a08\u7b97\u5206\u6578\u2026", value = 0, {
        tryCatch({
          scores <- own() %||% kitsui_score_own(round, persons(), score_progress)
          own(scores)
          # 原文：換算到最後的量尺…
          shiny::setProgress(1, detail = "\u63db\u7b97\u5230\u6700\u5f8c\u7684\u91cf\u5c3a\u2026")
          kitsui_score_convert(scores, fixed(), link_objs(), label())
        }, error = function(e) e)
      })
      if (inherits(out, "error")) {
        # 原文：分數沒有算出來。
        status(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u5206\u6578\u6c92\u6709\u7b97\u51fa\u4f86\u3002"),
                         # 原文：計算分數
                         shiny::tags$p(kitsui_error_ui(kit_error_text(out, "\u8a08\u7b97\u5206\u6578"))),
                         # 原文：怎麼辦：看上面的原因；必要時回到「② 題目校準」重跑這一回合。
                         shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u770b\u4e0a\u9762\u7684\u539f\u56e0\uff1b\u5fc5\u8981\u6642\u56de\u5230\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\u91cd\u8dd1\u9019\u4e00\u56de\u5408\u3002")))
        # 原文：分數沒有算出來。
        kitsui_after_run("stop", "\u5206\u6578\u6c92\u6709\u7b97\u51fa\u4f86\u3002")
        return()
      }
      # 一列都沒有：不是【完成】，也不給空的下載
      if (NROW(out$scores) == 0) {
        status(score_nothing_status(round, ns))
        # 原文：沒有可以評分的量尺，所以沒有分數。
        kitsui_after_run("stop", "\u6c92\u6709\u53ef\u4ee5\u8a55\u5206\u7684\u91cf\u5c3a\uff0c\u6240\u4ee5\u6c92\u6709\u5206\u6578\u3002")
        return()
      }
      result(c(out, list(links_rows = fixed()$links)))
      status(score_status(out, round))
      # 原文：分數算好了。
      kitsui_after_run("done", "\u5206\u6578\u7b97\u597d\u4e86\u3002")
    })
    output$status <- shiny::renderUI(status())

    output$done <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      score_done_ui(ns, items(), ref_final(), label(), result()$scores)
    })
    summary_tab <- shiny::reactive(kitsui_score_summary(shiny::req(result())$scores, ref_final()))
    output$summary <- kit_table(kitsui_score_summary_display(utils::head(summary_tab(),
                                                                         SCORE_SUMMARY_ROWS)))
    output$summary_all <- kit_table(kitsui_score_summary_display(summary_tab()))
    output$result <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      score_result_ui(ns, names(result()$subj), fixed()$version$reference, ref_final())
    })
    # 進階設定（通過步驟）：有題不是 1 時打開，不把設定藏起來；算完分數時重畫
    output$advanced <- shiny::renderUI({
      shiny::req(items(), !nothing())
      result()
      shown <- shiny::isolate(tryCatch(shown_steps(), error = \(e) NULL))
      score_steps_ui(ns, shown)
    })
    # 下一步：用這一回合的固定檔開始下一回合（有量尺停下來時沒有）
    output$next_step <- shiny::renderUI({
      if (is.null(result()) || is.null(next_round) || !isTRUE(items()$ok)) return(NULL)
      # 這一頁的主角是兩個下載；「下一回合」是次要按鈕
      # 原文：這一回合做完了。要接著加新的波次，不用下載固定檔，直接開始下一回合。
      kitDesign::kit_next_step("\u9019\u4e00\u56de\u5408\u505a\u5b8c\u4e86\u3002\u8981\u63a5\u8457\u52a0\u65b0\u7684\u6ce2\u6b21\uff0c\u4e0d\u7528\u4e0b\u8f09\u56fa\u5b9a\u6a94\uff0c\u76f4\u63a5\u958b\u59cb\u4e0b\u4e00\u56de\u5408\u3002",
                               # 原文：下一步：用這一回合的固定檔開始下一回合
                               ns("next_round"), "\u4e0b\u4e00\u6b65\uff1a\u7528\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\u958b\u59cb\u4e0b\u4e00\u56de\u5408", kind = "secondary")
    })

    # 1. 分數表 ----
    output$routes <- kit_table({
      kitsui_screen_names(kitsui_score_routes(fixed(), shiny::req(result())$subj, items()$plan))
    })
    # 外部連結不適用時的參照世代（分數表是 ref_score，不是 d_score）；適用時 NULL
    ref_final <- shiny::reactive(kitsui_ref_final(shiny::req(fixed())))
    output$columns <- shiny::renderUI(score_columns_ui(shiny::req(result())$scores, ref_final()))
    shown <- shiny::reactive(kitsui_score_search(shiny::req(result())$scores, input$search))
    pages <- shiny::reactive(max(1, ceiling(nrow(shown()) / SCORE_PAGE_ROWS)))
    page <- shiny::reactive(score_page(input$page, pages()))
    output$page_info <- shiny::renderText({
      # 原文：符合 %s 列，共 %s 頁；目前第 %s 頁。
      sprintf("\u7b26\u5408 %s \u5217\uff0c\u5171 %s \u9801\uff1b\u76ee\u524d\u7b2c %s \u9801\u3002", kitDesign::kit_num(nrow(shown())),
              kitDesign::kit_num(pages()), kitDesign::kit_num(page()))
    })
    output$scores <- kit_table({
      dplyr::slice(shown(), (page() - 1) * SCORE_PAGE_ROWS + seq_len(SCORE_PAGE_ROWS))
    })
    output$scores_dl_ui <- render_download_csv(
      # 原文：下載這一回合的分數表（CSV）
      session, "scores_dl", "\u4e0b\u8f09\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u8868\uff08CSV\uff09", \() result()$scores,
      \() paste0("scores_round_", label()), class = SCORE_DL_CLASS
    )

    # 2. 分數對照表（kits_subj_table() 每個量尺只算一次，換算直接換這張表） ----
    raw_memo <- new.env()
    shiny::observeEvent(result(), rm(list = ls(raw_memo), envir = raw_memo))
    subj_table <- shiny::reactive({
      s <- shiny::req(input$subj_scale)
      res <- shiny::req(result())
      shiny::req(s %in% names(res$own))
      if (is.null(raw_memo[[s]])) {
        path <- kitsui_path_links(fixed(), link_objs(), s)
        internal <- kitsui_path_links(fixed(), link_objs(), s, stage = "internal")
        raw_memo[[s]] <- shiny::withProgress(
          # 原文：正在做分數對照表…
          message = "\u6b63\u5728\u505a\u5206\u6578\u5c0d\u7167\u8868\u2026", detail = s, value = 0.5,
          kitsui_raw_table(res$own[[s]], path, internal)
        )
      }
      raw_memo[[s]]
    })
    output$subj_table <- kit_table(kitsui_raw_table_shown(subj_table(), input$only_observed))
    output$subj_table_dl_ui <- render_download_csv(
      # 原文：下載對照表（CSV）
      session, "subj_table_dl", "\u4e0b\u8f09\u5c0d\u7167\u8868\uff08CSV\uff09", subj_table,
      \() paste0("raw_table_", input$subj_scale %||% "scale")
    )

    # 3. 題目表 ----
    # 通過步驟：預設用這一回合開始時固定檔的設定。使用者在畫面上改過或上傳之後用使用者的
    # （換了固定檔就回到那個固定檔的設定）
    base_steps <- shiny::reactive(items()$fixed$pass_steps)
    user_steps <- shiny::reactiveVal(NULL)
    steps <- shiny::reactive({
      u <- user_steps()
      if (!is.null(u) && identical(u$base, base_steps())) return(u$steps)
      kitsui_steps_vec(base_steps())
    })
    known <- shiny::reactive({
      shiny::req(!nothing())
      kitsui_round_item_steps(shiny::req(items())$items, fixed()$versions)
    })
    shown_steps <- shiny::reactive(kitsui_steps_shown(known(), steps()))
    # 訊息：選題目、改回 1 的放在表格上面（pick）；上傳的放在「用檔案設定」裡（file）
    steps_msg <- shiny::reactiveVal(NULL)
    say <- function(where, ui) steps_msg(list(where = where, ui = ui))
    set_steps <- function(new) user_steps(list(base = base_steps(), steps = new))
    output$steps_summary <- shiny::renderText(kitsui_steps_summary(shown_steps()))
    # 套用之後清單重畫：標籤換成新的 step，選的題清空；通過步驟留著剛才的值
    output$steps_picker <- shiny::renderUI({
      value <- shiny::isolate(input$steps_value) %||% 2
      steps_picker_ui(ns, kitsui_steps_choices(known(), steps()), value)
    })
    output$steps_table <- shiny::renderUI(steps_table_ui(ns, shown_steps()))
    output$steps_msg <- shiny::renderUI(steps_msg_at(steps_msg(), "pick"))
    output$steps_file_msg <- shiny::renderUI(steps_msg_at(steps_msg(), "file"))
    shiny::observeEvent(input$steps_apply, {
      chk <- kitsui_steps_pick(input$steps_pick, input$steps_value, known())
      if (length(chk$errors) > 0) {
        # 原文：選好再按「套用到選的題」。
        return(say("pick", steps_error_ui(chk$errors, "\u9078\u597d\u518d\u6309\u300c\u5957\u7528\u5230\u9078\u7684\u984c\u300d\u3002")))
      }
      set_steps(kitsui_steps_merge(steps(), chk$steps))
      say("pick", kitDesign::kit_callout(steps_applied_text(chk$steps)))
    })
    shiny::observeEvent(input$steps_file, {
      f <- input$steps_file
      tab <- tryCatch(kitsui_read_steps(f$datapath), error = \(e) e)
      chk <- if (inherits(tab, "error")) list(errors = conditionMessage(tab)) else {
        kitsui_steps_check(tab, known())
      }
      if (length(chk$errors) > 0) {
        # 原文：請改好再上傳。
        say("file", steps_error_ui(chk$errors, "\u8acb\u6539\u597d\u518d\u4e0a\u50b3\u3002"))
        return()
      }
      set_steps(kitsui_steps_merge(steps(), chk$steps))
      say("file", shiny::tagList(
        # 原文：已套用  | ：共  |  題。
        kitDesign::kit_callout(paste0("\u5df2\u5957\u7528 ", f$name, "\uff1a\u5171 ", length(chk$steps), " \u984c\u3002"),
                               # 原文：檔案裡沒列的題不變。
                               "\u6a94\u6848\u88e1\u6c92\u5217\u7684\u984c\u4e0d\u8b8a\u3002"),
        if (length(chk$warnings) > 0) {
          # 原文：請注意
          kitDesign::kit_callout(shiny::tags$strong("\u8acb\u6ce8\u610f"),
                                 shiny::tags$ul(lapply(chk$warnings, shiny::tags$li)), warn = TRUE)
        }
      ))
    })
    # 表格每一列的「改回 1」（按鈕把題號送到 steps_row_reset）
    shiny::observeEvent(input$steps_row_reset, {
      id <- input$steps_row_reset
      set_steps(kitsui_steps_merge(steps(), stats::setNames(1L, id)))
      # 原文： 改回 1 了。
      say("pick", kitDesign::kit_callout(paste0(id, " \u6539\u56de 1 \u4e86\u3002")))
    })
    shiny::observeEvent(input$steps_reset, {
      set_steps(kitsui_steps_vec(NULL))
      # 原文：已經全部改回 1。
      say("pick", kitDesign::kit_callout("\u5df2\u7d93\u5168\u90e8\u6539\u56de 1\u3002"))
    })
    output$steps_template_ui <- render_download_csv(
      # 原文：下載範本（CSV）
      session, "steps_template", "\u4e0b\u8f09\u7bc4\u672c\uff08CSV\uff09",
      \() kitsui_steps_template(known(), steps()), "pass_steps"
    )
    # 收起來時也要畫好（打開就看得到）
    for (id in c("steps_picker", "steps_table", "steps_msg", "steps_file_msg",
                 "steps_template_ui")) {
      shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
    }
    item_obj <- shiny::reactive({
      s <- shiny::req(input$item_scale)
      kitsui_item_table(shiny::req(items()), data()$df, fixed(), s, link_objs(), persons(),
                        steps())
    })
    item_table <- shiny::reactive(kitsui_item_table_shown(item_obj()))
    output$item_table <- kit_table(item_table())
    output$item_table_dl_ui <- render_download_csv(
      # 原文：下載題目表（CSV）
      session, "item_table_dl", "\u4e0b\u8f09\u984c\u76ee\u8868\uff08CSV\uff09", item_table,
      \() paste0("item_table_", input$item_scale %||% "scale")
    )
    output$item_table_scale <- shiny::renderUI({
      tab <- item_table()
      last <- utils::tail(intersect(KITSUI_ITEM_THRESHOLDS, names(tab)), 1)
      # 原文：題目依  |  由小到大排列。
      shiny::tags$p(class = "kit-hint", paste0("\u984c\u76ee\u4f9d ", last, " \u7531\u5c0f\u5230\u5927\u6392\u5217\u3002"))
    })
    output$item_table_note <- shiny::renderUI({
      item_table_note_ui(item_table(), fixed()$version$reference, kitsui_wave_unit(fixed()))
    })

    # 檢查、R 程式 ----
    output$checks <- kit_table({
      tab <- tryCatch(item_obj(), error = \(e) NULL)
      more <- if (inherits(tab, "kits_item_table")) list(item_table = tab)
      checks_or_ok(do.call(check_table, c(shiny::req(result())$subj, more))) |>
        kitsui_check_explain()
    })
    code_text <- shiny::reactive({
      s <- shiny::req(input$subj_scale)
      shiny::req(items(), fixed())
      step <- kitsui_item_step_arg(items()$items[[s]], steps(), fixed()$versions)
      score_code(fixed(), s, kitsui_round_cohorts(items(), s), kitsui_fixed_name(label()), step)
    })
    # 用 R 怎麼做：打開時才算；每行最多 70 欄（kitDesign::kit_code()）
    # 原文：kits 版
    output$code <- kitDesign::kit_render_code_panel(\() list("kits \u7248" = code_text()))

    # 新的固定檔 ----
    # 這一回合的通過步驟寫進固定檔（pass_steps）；沒有改時固定檔原樣
    fixed_out <- shiny::reactive(kitsui_fixed_pass_steps(fixed(), steps(), label()))
    # 下一回合：把新的固定檔（含這一回合的連結）交給 ①，不用下載
    shiny::observeEvent(input$next_round, {
      if (!isTRUE(items()$ok) || is.null(next_round)) return()
      next_round(fixed_out(), label())
    })
    # 檔名跟著這一回合的名稱；按下載時才寫出當下的固定檔
    output$fixed_dl_ui <- shiny::renderUI(kitDesign::kit_download_button(
      session, "fixed_dl", SCORE_FIXED_LABEL, kitsui_fixed_name(label()),
      function(file) {
        # 有量尺停下來時不寫出（kitsui_write_fixed() 也會擋）
        # 原文：這一回合有量尺停下來，不產生新的固定檔。
        if (!isTRUE(items()$ok)) kitDesign::kit_abort("\u9019\u4e00\u56de\u5408\u6709\u91cf\u5c3a\u505c\u4e0b\u4f86\uff0c\u4e0d\u7522\u751f\u65b0\u7684\u56fa\u5b9a\u6a94\u3002")
        kitsui_write_fixed(fixed_out(), file)
      },
      class = SCORE_DL_CLASS
    ))

    result
  })
}

# 每個量尺開始算分數前更新進度條（kitsui_score_own() 的 progress）
score_progress <- function(i, n, scale) {
  # 原文：%s（第 %d 個，共 %d 個量尺）
  shiny::setProgress((i - 1) / n, detail = sprintf("%s\uff08\u7b2c %d \u500b\uff0c\u5171 %d \u500b\u91cf\u5c3a\uff09", scale, i, n))
}

#' ④ 的 R 程式：從新的固定檔重建換算路線上的連結，再評分
#' @param fixed_file 新固定檔的檔名（④ 下載的 zip）
#' @param step 題目表的通過步驟（kitsui_item_step_arg()）
#' @noRd
score_code <- function(fixed, scale, cohorts, fixed_file, step = 1) {
  path <- kitsui_link_path(fixed, scale)
  # 原文：# 接在 ② 題目校準（ | ）的程式後面（X、item）
  head <- paste0("# \u63a5\u5728 \u2461 \u984c\u76ee\u6821\u6e96\uff08", scale, "\uff09\u7684\u7a0b\u5f0f\u5f8c\u9762\uff08X\u3001item\uff09")
  links <- purrr::map_chr(path, \(n) kitsui_code_link(fixed, n, fixed_file))
  if (length(path) > 0) {
    # 原文：# 連結從這一回合下載的新固定檔（ | ）重建
    head <- c(head, paste0("# \u9023\u7d50\u5f9e\u9019\u4e00\u56de\u5408\u4e0b\u8f09\u7684\u65b0\u56fa\u5b9a\u6a94\uff08", fixed_file, "\uff09\u91cd\u5efa"))
  }
  paste(c(head, links, kitsui_code_score(fixed, scale, cohorts, step)), collapse = "\n")
}

# 小工具：頁碼、檔名（分數的計算在 pipeline_score.R） ----

#' 頁碼限制在 1 到 pages
#' @noRd
score_page <- function(page, pages) {
  p <- suppressWarnings(as.integer(page %||% 1))
  if (length(p) == 0 || is.na(p)) p <- 1L
  min(max(p, 1L), pages)
}

#' 這一回合的名稱（檔名用）
#' @noRd
score_round_label <- function(data, fixed) {
  label <- data$round_label %||% utils::tail(fixed$version$rounds, 1)
  if (length(label) == 0) "round" else label
}

# 畫面小元件 ----

#' ② 的 flag（check = "flag"）帶到 ④：橘色提醒，說明是什麼意思
#' @param flags kitsui_round_flags()
#' @noRd
score_flags_ui <- function(flags) {
  if (is.null(flags) || nrow(flags) == 0) return(NULL)
  items <- purrr::map2(flags$scale, flags$detail, \(s, d) {
    # 原文：（flag）：
    shiny::tags$li(shiny::tags$span(class = "kit-warn-text", paste0(s, "\uff08flag\uff09\uff1a")), d)
  })
  kitDesign::kit_callout(
    # 原文：② 題目校準給這些量尺 flag：
    shiny::tags$strong("\u2461 \u984c\u76ee\u6821\u6e96\u7d66\u9019\u4e9b\u91cf\u5c3a flag\uff1a"),
    shiny::tags$ul(items),
    shiny::tags$p(
      # 原文：flag 表示這個量尺偏移的題比較多，或可以固定的題比較少。
      "flag \u8868\u793a\u9019\u500b\u91cf\u5c3a\u504f\u79fb\u7684\u984c\u6bd4\u8f03\u591a\uff0c\u6216\u53ef\u4ee5\u56fa\u5b9a\u7684\u984c\u6bd4\u8f03\u5c11\u3002",
      # 原文：這一回合照常校準，分數也照常算。
      "\u9019\u4e00\u56de\u5408\u7167\u5e38\u6821\u6e96\uff0c\u5206\u6578\u4e5f\u7167\u5e38\u7b97\u3002",
      # 原文：但這個量尺和題庫的尺可能沒有對得很準。
      "\u4f46\u9019\u500b\u91cf\u5c3a\u548c\u984c\u5eab\u7684\u5c3a\u53ef\u80fd\u6c92\u6709\u5c0d\u5f97\u5f88\u6e96\u3002",
      # 原文：請到「② 題目校準」看是哪些題，再決定要不要用這一回合的分數。
      "\u8acb\u5230\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\u770b\u662f\u54ea\u4e9b\u984c\uff0c\u518d\u6c7a\u5b9a\u8981\u4e0d\u8981\u7528\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u3002"
    ),
    warn = TRUE
  )
}

#' 算完分數的狀態框：【完成】；有量尺停下來或被 flag 時【完成，但請注意】
#' @param out kitsui_score_convert() 的結果；round 是 ② 的結果
#' @noRd
score_status <- function(out, round) {
  if (NROW(out$scores) == 0) return(score_nothing_status(round))
  # 原文：分數算好了：共 %s 列（每個量尺的每個列次一列）。
  line <- sprintf("\u5206\u6578\u7b97\u597d\u4e86\uff1a\u5171 %s \u5217\uff08\u6bcf\u500b\u91cf\u5c3a\u7684\u6bcf\u500b\u5217\u6b21\u4e00\u5217\uff09\u3002", kitDesign::kit_num(nrow(out$scores)))
  flags <- kitsui_round_flags(round)
  points <- c(
    if (!isTRUE(round$ok)) {
      # 原文：有量尺停下來（ | 、 | ），不能下載新的固定檔。
      paste0("\u6709\u91cf\u5c3a\u505c\u4e0b\u4f86\uff08", paste(round$stopped, collapse = "\u3001"), "\uff09\uff0c\u4e0d\u80fd\u4e0b\u8f09\u65b0\u7684\u56fa\u5b9a\u6a94\u3002")
    },
    if (NROW(flags) > 0) {
      # 原文：被 flag 的量尺： | 、 | ；見上面的提醒。
      paste0("\u88ab flag \u7684\u91cf\u5c3a\uff1a", paste(unique(flags$scale), collapse = "\u3001"), "\uff1b\u898b\u4e0a\u9762\u7684\u63d0\u9192\u3002")
    }
  )
  if (length(points) == 0) return(kitDesign::kit_status_ui(kitDesign::kit_status("done", line)))
  kitDesign::kit_status_ui(kitDesign::kit_status("warn", line, points = points))
}

#' 沒有可以評分的量尺（② 的量尺都停下來，或算出來 0 列）：【停下來】與怎麼辦，沒有下載
#' @param round ② 的結果；ns 有的時候給「到 ②」的連結
#' @noRd
score_nothing_status <- function(round, ns = NULL) {
  stopped <- round$stopped %||% character()
  why <- if (length(stopped) > 0) {
    # 原文：（② 有  |  個量尺停下來：
    paste0("\uff08\u2461 \u6709 ", kitDesign::kit_num(length(stopped)), " \u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\uff1a",
           # 原文：）
           kitsui_first(scale_label(stopped), 6), "\uff09")
  }
  kitDesign::kit_status_ui(kitDesign::kit_status(
    # 原文：沒有可以評分的量尺 | ，所以沒有分數。
    "stop", paste0("\u6c92\u6709\u53ef\u4ee5\u8a55\u5206\u7684\u91cf\u5c3a", why %||% "", "\uff0c\u6240\u4ee5\u6c92\u6709\u5206\u6578\u3002")),
    shiny::tags$p(
      # 原文：怎麼辦：
      class = "kit-status-action", shiny::tags$strong("\u600e\u9ebc\u8fa6\uff1a"),
      # 原文：回到 | 「② 題目校準」 | 「② 題目校準」
      "\u56de\u5230", if (!is.null(ns)) shiny::actionLink(ns("go_items"), "\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d") else "\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d",
      # 原文：，照【停下來】裡的說明改好資料再校準；至少要有一個量尺校準好，這裡才能算分數。
      "\uff0c\u7167\u3010\u505c\u4e0b\u4f86\u3011\u88e1\u7684\u8aaa\u660e\u6539\u597d\u8cc7\u6599\u518d\u6821\u6e96\uff1b\u81f3\u5c11\u8981\u6709\u4e00\u500b\u91cf\u5c3a\u6821\u6e96\u597d\uff0c\u9019\u88e1\u624d\u80fd\u7b97\u5206\u6578\u3002"
    )
  )
}

#' 重點：分數摘要（每個量尺 × 世代 × 波次的人數、平均、標準差）、現在就要存的兩個檔案、
#' 每個量尺換到哪一把尺（一張表）
#' @param ref_final 外部連結不適用時的參照世代（kitsui_ref_final()），適用時 NULL
#' @noRd
score_done_ui <- function(ns, round, ref_final = NULL, label = NULL, scores = NULL) {
  n_sum <- NROW(kitsui_score_summary(scores, ref_final))
  shiny::tagList(
    # 原文：分數摘要
    shiny::h4("\u5206\u6578\u6458\u8981"),
    shiny::tags$small(
      class = "kit-hint",
      # 原文：每個量尺、世代、波次一列：分數的人數、平均與標準差。
      "\u6bcf\u500b\u91cf\u5c3a\u3001\u4e16\u4ee3\u3001\u6ce2\u6b21\u4e00\u5217\uff1a\u5206\u6578\u7684\u4eba\u6578\u3001\u5e73\u5747\u8207\u6a19\u6e96\u5dee\u3002",
      if (n_sum > SCORE_SUMMARY_ROWS) {
        # 原文：共 %s 列，以下前 %s 列。
        sprintf("\u5171 %s \u5217\uff0c\u4ee5\u4e0b\u524d %s \u5217\u3002", kitDesign::kit_num(n_sum),
                kitDesign::kit_num(SCORE_SUMMARY_ROWS))
      }
    ),
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("summary"))),
    if (n_sum > SCORE_SUMMARY_ROWS) {
      kitDesign::kit_details_row(kitDesign::kit_detail(
        # 原文：全部（ |  列）
        paste0("\u5168\u90e8\uff08", kitDesign::kit_num(n_sum), " \u5217\uff09"),
        shiny::tags$div(class = "kit-table-scroll kit-table-tall", shiny::tableOutput(ns("summary_all")))
      ))
    },
    # 原文：請現在就存兩個檔案
    shiny::h4("\u8acb\u73fe\u5728\u5c31\u5b58\u5169\u500b\u6a94\u6848"),
    shiny::tags$p(class = "kit-warn-text",
                  # 原文：早先各回合的分數之後不會重算。所以這一回合的分數表要現在下載保存。
                  "\u65e9\u5148\u5404\u56de\u5408\u7684\u5206\u6578\u4e4b\u5f8c\u4e0d\u6703\u91cd\u7b97\u3002\u6240\u4ee5\u9019\u4e00\u56de\u5408\u7684\u5206\u6578\u8868\u8981\u73fe\u5728\u4e0b\u8f09\u4fdd\u5b58\u3002"),
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(ns("scores_dl")),
      shiny::tags$small(class = "kit-hint", kitsui_score_column_text(scores, ref_final),
                        # 原文：下載的是全部的列，不只畫面上這一頁。
                        "\u4e0b\u8f09\u7684\u662f\u5168\u90e8\u7684\u5217\uff0c\u4e0d\u53ea\u756b\u9762\u4e0a\u9019\u4e00\u9801\u3002")
    ),
    score_fixed_ui(round, ns("fixed_dl"), label),
    # 原文：每個量尺換到哪一把尺
    shiny::h4("\u6bcf\u500b\u91cf\u5c3a\u63db\u5230\u54ea\u4e00\u628a\u5c3a"),
    shiny::tags$small(
      class = "kit-hint",
      if (is.null(ref_final)) {
        # 原文：換算路線：校準的量尺 → 內部連結 → 外部連結到 D。
        "\u63db\u7b97\u8def\u7dda\uff1a\u6821\u6e96\u7684\u91cf\u5c3a \u2192 \u5167\u90e8\u9023\u7d50 \u2192 \u5916\u90e8\u9023\u7d50\u5230 D\u3002"
      } else {
        # 原文：換算路線：校準的量尺 → 內部連結到參照世代。
        "\u63db\u7b97\u8def\u7dda\uff1a\u6821\u6e96\u7684\u91cf\u5c3a \u2192 \u5167\u90e8\u9023\u7d50\u5230\u53c3\u7167\u4e16\u4ee3\u3002"
      },
      # 原文：題庫是校準用的題庫；資料是哪一個世代、哪些波次的資料。
      "\u984c\u5eab\u662f\u6821\u6e96\u7528\u7684\u984c\u5eab\uff1b\u8cc7\u6599\u662f\u54ea\u4e00\u500b\u4e16\u4ee3\u3001\u54ea\u4e9b\u6ce2\u6b21\u7684\u8cc7\u6599\u3002"
    ),
    if (!is.null(ref_final)) {
      kitDesign::kit_callout(
        # 原文：外部連結不適用：
        shiny::tags$strong("\u5916\u90e8\u9023\u7d50\u4e0d\u9069\u7528\uff1a"),
        # 原文：參照世代（ | ）沒有 D 分數的錨題，最後的分數是 ref_score，
        paste0("\u53c3\u7167\u4e16\u4ee3\uff08", ref_final, "\uff09\u6c92\u6709 D \u5206\u6578\u7684\u9328\u984c\uff0c\u6700\u5f8c\u7684\u5206\u6578\u662f ref_score\uff0c",
               # 原文：在  |  的量尺上；還沒有內部連結的世代，ref_score 是空白，
               "\u5728 ", ref_final, " \u7684\u91cf\u5c3a\u4e0a\uff1b\u9084\u6c92\u6709\u5167\u90e8\u9023\u7d50\u7684\u4e16\u4ee3\uff0cref_score \u662f\u7a7a\u767d\uff0c",
               # 原文：分數留在 ability（自己的量尺）。
               "\u5206\u6578\u7559\u5728 ability\uff08\u81ea\u5df1\u7684\u91cf\u5c3a\uff09\u3002")
      )
    },
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("routes")))
  )
}

#' 新的固定檔：這一回合沒問題時給下載鈕；有量尺停下來時鈕不能按，並用紅字說明
#' @noRd
score_fixed_ui <- function(round, button_id, label = NULL) {
  text <- SCORE_FIXED_LABEL
  # 原文：做完題目校準後，這裡可以下載新的固定檔。
  if (is.null(round)) return(shiny::tags$p("\u505a\u5b8c\u984c\u76ee\u6821\u6e96\u5f8c\uff0c\u9019\u88e1\u53ef\u4ee5\u4e0b\u8f09\u65b0\u7684\u56fa\u5b9a\u6a94\u3002"))
  if (!isTRUE(round$ok)) {
    return(shiny::tagList(
      shiny::tags$p(
        class = "text-danger fw-bold",
        # 原文：這一回合有量尺停下來（ | 、 | ），不能下載新的固定檔。
        "\u9019\u4e00\u56de\u5408\u6709\u91cf\u5c3a\u505c\u4e0b\u4f86\uff08", paste(round$stopped, collapse = "\u3001"), "\uff09\uff0c\u4e0d\u80fd\u4e0b\u8f09\u65b0\u7684\u56fa\u5b9a\u6a94\u3002"
      ),
      shiny::tags$p(class = "text-danger",
                    # 原文：請到「② 題目校準」看停下來的原因。 | 處理後重跑這一回合，
                    "\u8acb\u5230\u300c\u2461 \u984c\u76ee\u6821\u6e96\u300d\u770b\u505c\u4e0b\u4f86\u7684\u539f\u56e0\u3002", "\u8655\u7406\u5f8c\u91cd\u8dd1\u9019\u4e00\u56de\u5408\uff0c",
                    # 原文：或在 ② 把它們改成「這一回合不做」再校準一次。
                    "\u6216\u5728 \u2461 \u628a\u5b83\u5011\u6539\u6210\u300c\u9019\u4e00\u56de\u5408\u4e0d\u505a\u300d\u518d\u6821\u6e96\u4e00\u6b21\u3002"),
      shiny::tags$button(class = paste("btn", kitDesign::kit_btn_class()), disabled = NA,
                         shiny::icon("download"), text)
    ))
  }
  # 原文：（ | ）
  file <- if (length(label) > 0) paste0("\uff08", kitsui_fixed_name(label), "\uff09")
  shiny::tagList(
    shiny::tags$div(class = "kit-downloads", ui_download(button_id)),
    shiny::tags$ul(
      # 原文：新的固定檔 | 存著題庫、題目版本與連結。
      shiny::tags$li("\u65b0\u7684\u56fa\u5b9a\u6a94", file, "\u5b58\u8457\u984c\u5eab\u3001\u984c\u76ee\u7248\u672c\u8207\u9023\u7d50\u3002"),
      # 原文：下一回合要上傳這個 zip，才能接著用同一個題庫與連結。
      shiny::tags$li("\u4e0b\u4e00\u56de\u5408\u8981\u4e0a\u50b3\u9019\u500b zip\uff0c\u624d\u80fd\u63a5\u8457\u7528\u540c\u4e00\u500b\u984c\u5eab\u8207\u9023\u7d50\u3002"),
      # 原文：在這次連線裡接著做，可以直接按最下面的「下一步：用這一回合的固定檔開始下一回合」，
      shiny::tags$li("\u5728\u9019\u6b21\u9023\u7dda\u88e1\u63a5\u8457\u505a\uff0c\u53ef\u4ee5\u76f4\u63a5\u6309\u6700\u4e0b\u9762\u7684\u300c\u4e0b\u4e00\u6b65\uff1a\u7528\u9019\u4e00\u56de\u5408\u7684\u56fa\u5b9a\u6a94\u958b\u59cb\u4e0b\u4e00\u56de\u5408\u300d\uff0c",
                     # 原文：不用下載。
                     "\u4e0d\u7528\u4e0b\u8f09\u3002"),
      if (length(round$excluded) > 0) {
        why <- kitsui_excluded_reasons(round$log)[round$excluded]
        shown <- ifelse(is.na(why) | !nzchar(why), scale_label(round$excluded),
                        # 原文：：
                        paste0(scale_label(round$excluded), "\uff1a", why))
        shiny::tags$li(class = "kit-warn-text",
                       # 原文：這一回合不做的量尺（ | ；
                       "\u9019\u4e00\u56de\u5408\u4e0d\u505a\u7684\u91cf\u5c3a\uff08", paste(shown, collapse = "\uff1b"),
                       # 原文：）沒有校準，題庫不變；固定檔的 checks.csv 記為 excluded。
                       "\uff09\u6c92\u6709\u6821\u6e96\uff0c\u984c\u5eab\u4e0d\u8b8a\uff1b\u56fa\u5b9a\u6a94\u7684 checks.csv \u8a18\u70ba excluded\u3002")
      }
    )
  )
}

#' 分數表每一欄的一句說明
#' @param ref NULL，或外部連結不適用時的參照世代（kitsui_ref_final()）
#' @noRd
score_columns_ui <- function(scores, ref = NULL) {
  cols <- kitsui_score_columns(scores, ref)
  shiny::tags$details(
    # 原文：每一欄是什麼
    shiny::tags$summary("\u6bcf\u4e00\u6b04\u662f\u4ec0\u9ebc"),
    shiny::tags$ul(
      class = "kit-hint",
      # 原文：：
      purrr::map2(cols$column, cols$meaning, \(c, m) shiny::tags$li(shiny::tags$code(c), "\uff1a", m))
    )
  )
}

#' 題目表下面：拿掉了哪些沒有內容的欄
#' @param ref 參照世代
#' @param unit 波次的單位（kitsui_wave_unit()）
#' @noRd
item_table_note_ui <- function(tab, ref = KITSUI_REF_COHORT, unit = "months") {
  drop <- attr(tab, "dropped")
  li <- \(col, ...) if (col %in% names(tab)) shiny::tags$li(shiny::tags$code(col), ...)
  shiny::tags$details(
    # 原文：每一欄是什麼
    shiny::tags$summary("\u6bcf\u4e00\u6b04\u662f\u4ec0\u9ebc"),
    shiny::tags$ul(
      class = "kit-hint",
      # 原文：三種門檻都是通過步驟的 Thurstonian 門檻。
      shiny::tags$li("\u4e09\u7a2e\u9580\u6abb\u90fd\u662f\u901a\u904e\u6b65\u9a5f\u7684 Thurstonian \u9580\u6abb\u3002",
                     # 原文：能力到這裡時，得分達到通過步驟的機會是一半。
                     "\u80fd\u529b\u5230\u9019\u88e1\u6642\uff0c\u5f97\u5206\u9054\u5230\u901a\u904e\u6b65\u9a5f\u7684\u6a5f\u6703\u662f\u4e00\u534a\u3002",
                     # 原文：孩子的分數高於同一把尺上的門檻，通過的機會就超過一半。
                     "\u5b69\u5b50\u7684\u5206\u6578\u9ad8\u65bc\u540c\u4e00\u628a\u5c3a\u4e0a\u7684\u9580\u6abb\uff0c\u901a\u904e\u7684\u6a5f\u6703\u5c31\u8d85\u904e\u4e00\u534a\u3002"),
      # 原文：：通過步驟。得分至少這麼多分算通過。
      li("step", "\uff1a\u901a\u904e\u6b65\u9a5f\u3002\u5f97\u5206\u81f3\u5c11\u9019\u9ebc\u591a\u5206\u7b97\u901a\u904e\u3002",
         # 原文：門檻與 age50 都照它算。只有在有題不是 1 時才列出。
         "\u9580\u6abb\u8207 age50 \u90fd\u7167\u5b83\u7b97\u3002\u53ea\u6709\u5728\u6709\u984c\u4e0d\u662f 1 \u6642\u624d\u5217\u51fa\u3002"),
      # 原文：：在校準的量尺上，和 ability 同一把尺（kits 裡這一欄叫 D）。
      li("ability_threshold", "\uff1a\u5728\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\uff0c\u548c ability \u540c\u4e00\u628a\u5c3a\uff08kits \u88e1\u9019\u4e00\u6b04\u53eb D\uff09\u3002"),
      # 原文：：經過內部連結，在參照世代（ | ）的量尺上，
      li("ability_linked_threshold", paste0("\uff1a\u7d93\u904e\u5167\u90e8\u9023\u7d50\uff0c\u5728\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u91cf\u5c3a\u4e0a\uff0c"),
         # 原文：和 ability_linked 同一把尺。參照世代自己的量尺沒有這一欄。
         "\u548c ability_linked \u540c\u4e00\u628a\u5c3a\u3002\u53c3\u7167\u4e16\u4ee3\u81ea\u5df1\u7684\u91cf\u5c3a\u6c92\u6709\u9019\u4e00\u6b04\u3002"),
      # 原文：：再經過外部連結，在 D 上，和 d_score 同一把尺。
      li("d_threshold", "\uff1a\u518d\u7d93\u904e\u5916\u90e8\u9023\u7d50\uff0c\u5728 D \u4e0a\uff0c\u548c d_score \u540c\u4e00\u628a\u5c3a\u3002"),
      # 原文：：這一回合一半的孩子通過（得分達到通過步驟）的月齡。
      li("age50", "\uff1a\u9019\u4e00\u56de\u5408\u4e00\u534a\u7684\u5b69\u5b50\u901a\u904e\uff08\u5f97\u5206\u9054\u5230\u901a\u904e\u6b65\u9a5f\uff09\u7684\u6708\u9f61\u3002",
         # 原文：age50 要從這一回合的資料算：至少兩個波次、通過比例在波次之間跨過 50%，
         paste0("age50 \u8981\u5f9e\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u7b97\uff1a\u81f3\u5c11\u5169\u500b\u6ce2\u6b21\u3001\u901a\u904e\u6bd4\u4f8b\u5728\u6ce2\u6b21\u4e4b\u9593\u8de8\u904e 50%\uff0c",
                # 原文：而且落在資料的月齡範圍內；
                "\u800c\u4e14\u843d\u5728\u8cc7\u6599\u7684\u6708\u9f61\u7bc4\u570d\u5167\uff1b",
                # 原文：只有一個波次，或每一波都已經超過（或都還沒到）50% 時是空的。
                "\u53ea\u6709\u4e00\u500b\u6ce2\u6b21\uff0c\u6216\u6bcf\u4e00\u6ce2\u90fd\u5df2\u7d93\u8d85\u904e\uff08\u6216\u90fd\u9084\u6c92\u5230\uff0950% \u6642\u662f\u7a7a\u7684\u3002")),
      if (unit != "months") {
        # 原文：age50 不算：波次（wave_m）的單位不是月（在 ① 第一回合選的），
        shiny::tags$li("age50 \u4e0d\u7b97\uff1a\u6ce2\u6b21\uff08wave_m\uff09\u7684\u55ae\u4f4d\u4e0d\u662f\u6708\uff08\u5728 \u2460 \u7b2c\u4e00\u56de\u5408\u9078\u7684\uff09\uff0c",
                       # 原文：kitsUI 只在單位是月時算通過年齡。
                       "kitsUI \u53ea\u5728\u55ae\u4f4d\u662f\u6708\u6642\u7b97\u901a\u904e\u5e74\u9f61\u3002")
      },
      if (length(drop) > 0) {
        # 原文：每一題都沒有內容的欄不列出： | 、 | 。
        shiny::tags$li(paste0("\u6bcf\u4e00\u984c\u90fd\u6c92\u6709\u5167\u5bb9\u7684\u6b04\u4e0d\u5217\u51fa\uff1a", paste(drop, collapse = "\u3001"), "\u3002"),
                       # 原文：DIF（性別的差異試題功能）要另外做 kits_dif()，kitsUI 沒有做。
                       if ("DIF" %in% drop) "DIF\uff08\u6027\u5225\u7684\u5dee\u7570\u8a66\u984c\u529f\u80fd\uff09\u8981\u53e6\u5916\u505a kits_dif()\uff0ckitsUI \u6c92\u6709\u505a\u3002",
                       # 原文：stem 是題目內容，沒有給的時候和 item_id 一樣。
                       if ("stem" %in% drop) "stem \u662f\u984c\u76ee\u5167\u5bb9\uff0c\u6c92\u6709\u7d66\u7684\u6642\u5019\u548c item_id \u4e00\u6a23\u3002")
      }
    )
  )
}

#' 進階設定（題目表的通過步驟）：收合區塊，標題是現在的設定
#'
#' 裡面依序是：一句說明、選題目套用、不是 1 的題（可以改回 1）、用檔案設定（再收一層）。
#' 有題不是 1 時預設打開。
#' @param shown kitsui_steps_shown()；NULL 時當作每題都是 1
#' @noRd
score_steps_ui <- function(ns, shown = NULL) {
  here <- if (!is.null(shown)) shown[!is.na(shown$scale), ]
  # 原文：通過步驟（每題都是 1）
  title <- if (is.null(shown)) "\u901a\u904e\u6b65\u9a5f\uff08\u6bcf\u984c\u90fd\u662f 1\uff09" else kitsui_steps_summary(shown)
  shiny::tags$details(
    class = "kit-advanced",
    open = if (NROW(here) > 0) NA,
    # 標題跟著設定變（renderText 換掉裡面的字）
    # 原文：進階設定：
    shiny::tags$summary("\u9032\u968e\u8a2d\u5b9a\uff1a", shiny::tags$span(id = ns("steps_summary"),
                                                  class = "shiny-text-output", title)),
    # 原文：得分至少幾分算通過。預設每題都是 1。
    shiny::tags$p("\u5f97\u5206\u81f3\u5c11\u5e7e\u5206\u7b97\u901a\u904e\u3002\u9810\u8a2d\u6bcf\u984c\u90fd\u662f 1\u3002",
                  # 原文：只影響題目表的門檻與 age50；校準、連結、分數都不受影響。
                  "\u53ea\u5f71\u97ff\u984c\u76ee\u8868\u7684\u9580\u6abb\u8207 age50\uff1b\u6821\u6e96\u3001\u9023\u7d50\u3001\u5206\u6578\u90fd\u4e0d\u53d7\u5f71\u97ff\u3002"),
    shiny::uiOutput(ns("steps_picker")),
    shiny::uiOutput(ns("steps_msg")),
    shiny::uiOutput(ns("steps_table")),
    shiny::tags$details(
      # 原文：用檔案設定（題目很多時）
      shiny::tags$summary("\u7528\u6a94\u6848\u8a2d\u5b9a\uff08\u984c\u76ee\u5f88\u591a\u6642\uff09"),
      # 原文：上傳設定檔（CSV，兩欄：item_id、step）
      shiny::fileInput(ns("steps_file"), "\u4e0a\u50b3\u8a2d\u5b9a\u6a94\uff08CSV\uff0c\u5169\u6b04\uff1aitem_id\u3001step\uff09",
                       # 原文：選擇檔案…
                       width = "26rem", accept = ".csv", buttonLabel = "\u9078\u64c7\u6a94\u6848\u2026",
                       # 原文：尚未選擇檔案
                       placeholder = "\u5c1a\u672a\u9078\u64c7\u6a94\u6848"),
      shiny::tags$div(class = "kit-downloads", ui_download(ns("steps_template"))),
      shiny::tags$small(class = "kit-hint",
                        # 原文：範本列出這一回合的每一題與目前的 step。
                        "\u7bc4\u672c\u5217\u51fa\u9019\u4e00\u56de\u5408\u7684\u6bcf\u4e00\u984c\u8207\u76ee\u524d\u7684 step\u3002",
                        # 原文：改 step 欄後上傳。scale、max_score 兩欄只是參考。
                        "\u6539 step \u6b04\u5f8c\u4e0a\u50b3\u3002scale\u3001max_score \u5169\u6b04\u53ea\u662f\u53c3\u8003\u3002"),
      shiny::uiOutput(ns("steps_file_msg"))
    )
  )
}

#' 選題目、填通過步驟、套用
#' @param choices kitsui_steps_choices()
#' @noRd
steps_picker_ui <- function(ns, choices, value = 2) {
  shiny::tagList(
    shiny::tags$div(
      class = "kit-steps-pick",
      # 原文：題目
      shiny::selectizeInput(ns("steps_pick"), "\u984c\u76ee", choices = choices, multiple = TRUE,
                            width = "28rem",
                            # 原文：選一題或好幾題（可以打字找）
                            options = list(placeholder = "\u9078\u4e00\u984c\u6216\u597d\u5e7e\u984c\uff08\u53ef\u4ee5\u6253\u5b57\u627e\uff09")),
      # 原文：通過步驟
      shiny::numericInput(ns("steps_value"), "\u901a\u904e\u6b65\u9a5f", value = value, min = 1, step = 1,
                          width = "7rem"),
      # 原文：套用到選的題
      kitDesign::kit_button(ns("steps_apply"), "\u5957\u7528\u5230\u9078\u7684\u984c")
    ),
    shiny::tags$small(class = "kit-hint",
                      # 原文：清單是這一回合每個量尺的全部題目，依量尺分組。
                      "\u6e05\u55ae\u662f\u9019\u4e00\u56de\u5408\u6bcf\u500b\u91cf\u5c3a\u7684\u5168\u90e8\u984c\u76ee\uff0c\u4f9d\u91cf\u5c3a\u5206\u7d44\u3002",
                      # 原文：括號裡是目前的通過步驟與校準裡的最高分數。
                      "\u62ec\u865f\u88e1\u662f\u76ee\u524d\u7684\u901a\u904e\u6b65\u9a5f\u8207\u6821\u6e96\u88e1\u7684\u6700\u9ad8\u5206\u6578\u3002")
  )
}

#' 不是 1 的題：一題一列，每列可以改回 1；下面是「全部改回 1」
#' @param shown kitsui_steps_shown()
#' @noRd
steps_table_ui <- function(ns, shown) {
  here <- shown[!is.na(shown$scale), ]
  away <- shown[is.na(shown$scale), ]
  # 每一列的按鈕把題號送到 steps_row_reset
  click <- sprintf("Shiny.setInputValue('%s', this.dataset.item, {priority: 'event'})",
                   ns("steps_row_reset"))
  row <- \(id, step, top) {
    shiny::tags$tr(
      shiny::tags$td(id), shiny::tags$td(step), shiny::tags$td(top),
      shiny::tags$td(shiny::tags$button(type = "button",
                                        class = paste("btn", kitDesign::kit_btn_class()),
                                        # 原文：改回 1
                                        `data-item` = id, onclick = click, "\u6539\u56de 1"))
    )
  }
  # 原文：目前每一題都是 1。
  table <- if (nrow(here) == 0) shiny::tags$p("\u76ee\u524d\u6bcf\u4e00\u984c\u90fd\u662f 1\u3002") else {
    shiny::tags$table(
      class = "table table-sm kit-steps-table",
      # 原文：題號 | 通過步驟
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("\u984c\u865f"), shiny::tags$th("\u901a\u904e\u6b65\u9a5f"),
                                       # 原文：最高分數
                                       shiny::tags$th("\u6700\u9ad8\u5206\u6578"), shiny::tags$th())),
      shiny::tags$tbody(purrr::pmap(list(here$item_id, here$step, here$max_score), row))
    )
  }
  shiny::tagList(
    # 原文：不是 1 的題
    if (nrow(here) > 0) shiny::tags$p(class = "mb-1 fw-bold", "\u4e0d\u662f 1 \u7684\u984c"),
    table,
    if (nrow(away) > 0) {
      shiny::tags$small(class = "kit-hint", paste0(
        # 原文：另有  |  題這一回合沒有，設定照樣存進固定檔：
        "\u53e6\u6709 ", nrow(away), " \u984c\u9019\u4e00\u56de\u5408\u6c92\u6709\uff0c\u8a2d\u5b9a\u7167\u6a23\u5b58\u9032\u56fa\u5b9a\u6a94\uff1a",
        # 原文：（ | ） | 。
        kitsui_some(paste0(away$item_id, "\uff08", away$step, "\uff09")), "\u3002"
      ))
    },
    if (nrow(shown) > 0) {
      # 原文：全部改回 1
      shiny::tags$div(class = "mb-3", kitDesign::kit_button(ns("steps_reset"), "\u5168\u90e8\u6539\u56de 1"))
    }
  )
}

#' 套用了：一句話說哪幾題設成多少
#' @param steps 原來的題號 -> step（都一樣）
#' @noRd
steps_applied_text <- function(steps) {
  ids <- kitsui_some(names(steps))
  # 原文：已把  |  題改回 1： | 。
  if (all(steps == 1)) return(paste0("\u5df2\u628a ", length(steps), " \u984c\u6539\u56de 1\uff1a", ids, "\u3002"))
  # 原文：已把  |  題設成  | ： | 。
  paste0("\u5df2\u628a ", length(steps), " \u984c\u8a2d\u6210 ", steps[[1]], "\uff1a", ids, "\u3002")
}

#' 通過步驟的訊息放在哪裡：msg 是 list(where, ui)
#' @noRd
steps_msg_at <- function(msg, where) if (identical(msg$where, where)) msg$ui

#' 通過步驟沒有套用：紅字說明
#' @param todo 下一步怎麼做（「請改好再上傳。」等）
#' @noRd
steps_error_ui <- function(errors, todo) {
  # 原文：通過步驟沒有套用，設定沒有改。
  kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u901a\u904e\u6b65\u9a5f\u6c92\u6709\u5957\u7528\uff0c\u8a2d\u5b9a\u6c92\u6709\u6539\u3002"),
            shiny::tags$ul(lapply(errors, shiny::tags$li)),
            # 原文：怎麼辦：
            shiny::tags$p(class = "mb-0", "\u600e\u9ebc\u8fa6\uff1a", todo))
}

#' 算完分數之後的細節（一排收起來）：分數表（逐列）、分數對照表、題目表、檢查、R 程式
#' @param ref 參照世代；ref_final 是外部連結不適用時的參照世代（kitsui_ref_final()），適用時 NULL
#' @noRd
score_result_ui <- function(ns, scales, ref = KITSUI_REF_COHORT, ref_final = NULL) {
  kitDesign::kit_details_row(
    kitDesign::kit_detail(
      # 原文：分數表（逐列）
      "\u5206\u6578\u8868\uff08\u9010\u5217\uff09",
      # 原文：找人：輸入 visit_id 或 subj_id 的一部分
      shiny::textInput(ns("search"), "\u627e\u4eba\uff1a\u8f38\u5165 visit_id \u6216 subj_id \u7684\u4e00\u90e8\u5206",
                       # 原文：例如 c55df9b
                       placeholder = "\u4f8b\u5982 c55df9b"),
      # 原文：第幾頁
      shiny::numericInput(ns("page"), "\u7b2c\u5e7e\u9801", value = 1, min = 1, step = 1, width = "8rem"),
      shiny::textOutput(ns("page_info")),
      shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("scores"))),
      shiny::uiOutput(ns("columns"))
    ),
    kitDesign::kit_detail(
      # 原文：分數對照表
      "\u5206\u6578\u5c0d\u7167\u8868",
      shiny::tags$small(class = "kit-hint",
                        # 原文：這是所有可能的原始分數的對照表：
                        shiny::tags$strong("\u9019\u662f\u6240\u6709\u53ef\u80fd\u7684\u539f\u59cb\u5206\u6578\u7684\u5c0d\u7167\u8868\uff1a"),
                        # 原文：每個原始分數對應的能力值與人數（kits_subj_table()）；
                        "\u6bcf\u500b\u539f\u59cb\u5206\u6578\u5c0d\u61c9\u7684\u80fd\u529b\u503c\u8207\u4eba\u6578\uff08kits_subj_table()\uff09\uff1b",
                        # 原文：本次資料中沒有人得到的原始分數，n 是 0。欄位說明在表的下面。
                        "\u672c\u6b21\u8cc7\u6599\u4e2d\u6c92\u6709\u4eba\u5f97\u5230\u7684\u539f\u59cb\u5206\u6578\uff0cn \u662f 0\u3002\u6b04\u4f4d\u8aaa\u660e\u5728\u8868\u7684\u4e0b\u9762\u3002"),
      # 原文：哪一個量尺？
      shiny::selectInput(ns("subj_scale"), "\u54ea\u4e00\u500b\u91cf\u5c3a\uff1f", choices = scales),
      # 原文：只顯示本次資料中實際出現的原始分數
      shiny::checkboxInput(ns("only_observed"), "\u53ea\u986f\u793a\u672c\u6b21\u8cc7\u6599\u4e2d\u5be6\u969b\u51fa\u73fe\u7684\u539f\u59cb\u5206\u6578", value = FALSE),
      shiny::tags$div(class = "kit-table-scroll kit-table-tall",
                      shiny::tableOutput(ns("subj_table"))),
      shiny::tags$details(
        # 原文：每一欄是什麼
        shiny::tags$summary("\u6bcf\u4e00\u6b04\u662f\u4ec0\u9ebc"),
        shiny::tags$ul(
          class = "kit-hint",
          # 原文：、
          shiny::tags$li(shiny::tags$code("ability"), "\u3001", shiny::tags$code("ability_se"),
                         # 原文：：校準的量尺上的能力值與標準誤（logit；kits 裡叫 theta、se）。
                         "\uff1a\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\u7684\u80fd\u529b\u503c\u8207\u6a19\u6e96\u8aa4\uff08logit\uff1bkits \u88e1\u53eb theta\u3001se\uff09\u3002",
                         # 原文：和分數表的 ability 相同：各世代分開估計，沒有經過連結。
                         "\u548c\u5206\u6578\u8868\u7684 ability \u76f8\u540c\uff1a\u5404\u4e16\u4ee3\u5206\u958b\u4f30\u8a08\uff0c\u6c92\u6709\u7d93\u904e\u9023\u7d50\u3002"),
          # 原文：、
          shiny::tags$li(shiny::tags$code("ability_linked"), "\u3001",
                         shiny::tags$code("ability_linked_se"),
                         # 原文：：linked 表示經過內部連結，換到參照世代（ | ）的量尺上。
                         paste0("\uff1alinked \u8868\u793a\u7d93\u904e\u5167\u90e8\u9023\u7d50\uff0c\u63db\u5230\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u91cf\u5c3a\u4e0a\u3002"),
                         # 原文：參照世代自己的量尺沒有這兩欄。
                         "\u53c3\u7167\u4e16\u4ee3\u81ea\u5df1\u7684\u91cf\u5c3a\u6c92\u6709\u9019\u5169\u6b04\u3002"),
          if (is.null(ref_final)) {
            shiny::tagList(
              # 原文：、
              shiny::tags$li(shiny::tags$code("d_score"), "\u3001", shiny::tags$code("d_score_se"),
                             # 原文：：再經過外部連結，D 上的量尺分數與標準誤（kits 裡叫 d）。
                             "\uff1a\u518d\u7d93\u904e\u5916\u90e8\u9023\u7d50\uff0cD \u4e0a\u7684\u91cf\u5c3a\u5206\u6578\u8207\u6a19\u6e96\u8aa4\uff08kits \u88e1\u53eb d\uff09\u3002"),
              # 原文：、
              shiny::tags$li(shiny::tags$code("se_link"), "\u3001", shiny::tags$code("se_total"),
                             # 原文：：d_score 的連結誤差，以及測量誤差與連結誤差合起來。
                             "\uff1ad_score \u7684\u9023\u7d50\u8aa4\u5dee\uff0c\u4ee5\u53ca\u6e2c\u91cf\u8aa4\u5dee\u8207\u9023\u7d50\u8aa4\u5dee\u5408\u8d77\u4f86\u3002")
            )
          } else {
            # 原文：外部連結不適用，所以沒有 d_score。
            shiny::tags$li("\u5916\u90e8\u9023\u7d50\u4e0d\u9069\u7528\uff0c\u6240\u4ee5\u6c92\u6709 d_score\u3002")
          },
          # 原文：作答組合：這個波次最多孩子作答的那一組題目。表裡只算用這個作答組合的孩子。
          shiny::tags$li("\u4f5c\u7b54\u7d44\u5408\uff1a\u9019\u500b\u6ce2\u6b21\u6700\u591a\u5b69\u5b50\u4f5c\u7b54\u7684\u90a3\u4e00\u7d44\u984c\u76ee\u3002\u8868\u88e1\u53ea\u7b97\u7528\u9019\u500b\u4f5c\u7b54\u7d44\u5408\u7684\u5b69\u5b50\u3002"),
          # 原文：：這個原始分數有幾個孩子。
          shiny::tags$li(shiny::tags$code("n"), "\uff1a\u9019\u500b\u539f\u59cb\u5206\u6578\u6709\u5e7e\u500b\u5b69\u5b50\u3002",
                         # 原文：：佔這個作答組合的百分比。
                         shiny::tags$code("pct"), "\uff1a\u4f54\u9019\u500b\u4f5c\u7b54\u7d44\u5408\u7684\u767e\u5206\u6bd4\u3002"),
          shiny::tags$li(shiny::tags$code("cum_pct"),
                         # 原文：：累積百分比，原始分數到這一列為止的孩子佔幾 %。
                         "\uff1a\u7d2f\u7a4d\u767e\u5206\u6bd4\uff0c\u539f\u59cb\u5206\u6578\u5230\u9019\u4e00\u5217\u70ba\u6b62\u7684\u5b69\u5b50\u4f54\u5e7e %\u3002"),
          # 原文：：這個波次全部的孩子數。
          shiny::tags$li(shiny::tags$code("wave_n"), "\uff1a\u9019\u500b\u6ce2\u6b21\u5168\u90e8\u7684\u5b69\u5b50\u6578\u3002"),
          # 原文：：這個波次的孩子有幾 % 用這個作答組合。
          shiny::tags$li(shiny::tags$code("form_pct"), "\uff1a\u9019\u500b\u6ce2\u6b21\u7684\u5b69\u5b50\u6709\u5e7e % \u7528\u9019\u500b\u4f5c\u7b54\u7d44\u5408\u3002")
        )
      ),
      shiny::tags$div(
        class = "kit-downloads",
        ui_download(ns("subj_table_dl")),
        # 原文：下載的一律是所有可能的原始分數的對照表。
        shiny::tags$small(class = "kit-hint", "\u4e0b\u8f09\u7684\u4e00\u5f8b\u662f\u6240\u6709\u53ef\u80fd\u7684\u539f\u59cb\u5206\u6578\u7684\u5c0d\u7167\u8868\u3002")
      )
    ),
    kitDesign::kit_detail(
      # 原文：題目表
      "\u984c\u76ee\u8868",
      shiny::tags$small(
        class = "kit-hint",
        # 原文：每一題一列（kits_item_table()）。欄位說明在表的下面。
        "\u6bcf\u4e00\u984c\u4e00\u5217\uff08kits_item_table()\uff09\u3002\u6b04\u4f4d\u8aaa\u660e\u5728\u8868\u7684\u4e0b\u9762\u3002",
        # 原文：通過步驟在上面的「進階設定」改。
        "\u901a\u904e\u6b65\u9a5f\u5728\u4e0a\u9762\u7684\u300c\u9032\u968e\u8a2d\u5b9a\u300d\u6539\u3002"
      ),
      # 原文：哪一個量尺？
      shiny::selectInput(ns("item_scale"), "\u54ea\u4e00\u500b\u91cf\u5c3a\uff1f", choices = scales),
      shiny::uiOutput(ns("item_table_scale")),
      shiny::tags$div(class = "kit-table-scroll kit-table-tall",
                      shiny::tableOutput(ns("item_table"))),
      shiny::uiOutput(ns("item_table_note")),
      shiny::tags$div(class = "kit-downloads", ui_download(ns("item_table_dl"))),
      shiny::tags$p(
        class = "kit-hint",
        # 原文：用常模算的年齡（age_reach 等）要有常模（kits_norms()）；kitsUI 不做常模，所以這裡沒有。
        "\u7528\u5e38\u6a21\u7b97\u7684\u5e74\u9f61\uff08age_reach \u7b49\uff09\u8981\u6709\u5e38\u6a21\uff08kits_norms()\uff09\uff1bkitsUI \u4e0d\u505a\u5e38\u6a21\uff0c\u6240\u4ee5\u9019\u88e1\u6c92\u6709\u3002"
      )
    ),
    ui_check_panel(ns("checks")),
    kitDesign::kit_code_panel(ns("code"))
  )
}
