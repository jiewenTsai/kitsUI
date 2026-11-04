# ② 題目校準：跑一回合（自由校準或偏移把關 + 固定校準），每個量尺看結果

# 圖的種類
ITEMS_PLOTS <- c(
  # 原文：類別機率曲線 | 題目與受試者分布（Wright map）
  "\u985e\u5225\u6a5f\u7387\u66f2\u7dda" = "category", "\u984c\u76ee\u8207\u53d7\u8a66\u8005\u5206\u5e03\uff08Wright map\uff09" = "wright",
  # 原文：適配度（infit、outfit） | 觀察與預期（依波次）
  "\u9069\u914d\u5ea6\uff08infit\u3001outfit\uff09" = "fit", "\u89c0\u5bdf\u8207\u9810\u671f\uff08\u4f9d\u6ce2\u6b21\uff09" = "observed",
  # 原文：偏移把關圖
  "\u504f\u79fb\u628a\u95dc\u5716" = "drift"
)
# 一次畫幾題（類別機率曲線、觀察與預期）
ITEMS_PAGE <- 9
# 要選題的圖
ITEMS_PAGED <- c("category", "observed")
# flag 的量尺怎麼辦（一句話，各處相同）
# 原文：被 flag 的量尺照樣往下做；請看偏移的題，必要時在下一回合處理。
ITEMS_FLAG_NOTE <- "\u88ab flag \u7684\u91cf\u5c3a\u7167\u6a23\u5f80\u4e0b\u505a\uff1b\u8acb\u770b\u504f\u79fb\u7684\u984c\uff0c\u5fc5\u8981\u6642\u5728\u4e0b\u4e00\u56de\u5408\u8655\u7406\u3002"

#' @noRd
mod_items_ui <- function(id) {
  ns <- shiny::NS(id)
  hint <- \(...) shiny::tags$small(class = "kit-hint", ...)
  shiny::tagList(
    kitDesign::kit_tab_guide(
      # 原文：估計每一題的難度（校準）。有固定檔時，先檢查舊題有沒有偏移。
      goal = "\u4f30\u8a08\u6bcf\u4e00\u984c\u7684\u96e3\u5ea6\uff08\u6821\u6e96\uff09\u3002\u6709\u56fa\u5b9a\u6a94\u6642\uff0c\u5148\u6aa2\u67e5\u820a\u984c\u6709\u6c92\u6709\u504f\u79fb\u3002",
      steps = c(
        # 原文：按「開始校準」。這一回合不做的量尺、題庫與 flag 的條件在「進階設定」，通常不用改。
        "\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002\u9019\u4e00\u56de\u5408\u4e0d\u505a\u7684\u91cf\u5c3a\u3001\u984c\u5eab\u8207 flag \u7684\u689d\u4ef6\u5728\u300c\u9032\u968e\u8a2d\u5b9a\u300d\uff0c\u901a\u5e38\u4e0d\u7528\u6539\u3002",
        # 原文：看狀態框：【完成】、【完成，但請注意】或【停下來】（停下來時寫了原因與怎麼辦）。
        "\u770b\u72c0\u614b\u6846\uff1a\u3010\u5b8c\u6210\u3011\u3001\u3010\u5b8c\u6210\uff0c\u4f46\u8acb\u6ce8\u610f\u3011\u6216\u3010\u505c\u4e0b\u4f86\u3011\uff08\u505c\u4e0b\u4f86\u6642\u5beb\u4e86\u539f\u56e0\u8207\u600e\u9ebc\u8fa6\uff09\u3002",
        # 原文：看圖：預設是第一個校準好的量尺，可以換別的量尺、別種圖。
        "\u770b\u5716\uff1a\u9810\u8a2d\u662f\u7b2c\u4e00\u500b\u6821\u6e96\u597d\u7684\u91cf\u5c3a\uff0c\u53ef\u4ee5\u63db\u5225\u7684\u91cf\u5c3a\u3001\u5225\u7a2e\u5716\u3002",
        # 原文：每個量尺一列的表、要注意的細節、題目參數、固定了哪些題、偏移把關、適配度、紀錄都收在下面，要看再打開。
        "\u6bcf\u500b\u91cf\u5c3a\u4e00\u5217\u7684\u8868\u3001\u8981\u6ce8\u610f\u7684\u7d30\u7bc0\u3001\u984c\u76ee\u53c3\u6578\u3001\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c\u3001\u504f\u79fb\u628a\u95dc\u3001\u9069\u914d\u5ea6\u3001\u7d00\u9304\u90fd\u6536\u5728\u4e0b\u9762\uff0c\u8981\u770b\u518d\u6253\u958b\u3002",
        # 原文：沒有【停下來】時，按「下一步：連結」。
        "\u6c92\u6709\u3010\u505c\u4e0b\u4f86\u3011\u6642\uff0c\u6309\u300c\u4e0b\u4e00\u6b65\uff1a\u9023\u7d50\u300d\u3002"
      ),
      # 原文：這一回合每個量尺的題目參數。新的題目步驟會加進固定檔的題庫。
      output = "\u9019\u4e00\u56de\u5408\u6bcf\u500b\u91cf\u5c3a\u7684\u984c\u76ee\u53c3\u6578\u3002\u65b0\u7684\u984c\u76ee\u6b65\u9a5f\u6703\u52a0\u9032\u56fa\u5b9a\u6a94\u7684\u984c\u5eab\u3002",
      note = paste0(
        # 原文：偏移把關的順序：先不固定任何題、只用這一波試算一次；試算的門檻和題庫比，
        "\u504f\u79fb\u628a\u95dc\u7684\u9806\u5e8f\uff1a\u5148\u4e0d\u56fa\u5b9a\u4efb\u4f55\u984c\u3001\u53ea\u7528\u9019\u4e00\u6ce2\u8a66\u7b97\u4e00\u6b21\uff1b\u8a66\u7b97\u7684\u9580\u6abb\u548c\u984c\u5eab\u6bd4\uff0c",
        # 原文：離線不遠的題固定成題庫的值；偏移的題換成新版本、用這一波重新估；最後一起做固定校準。
        "\u96e2\u7dda\u4e0d\u9060\u7684\u984c\u56fa\u5b9a\u6210\u984c\u5eab\u7684\u503c\uff1b\u504f\u79fb\u7684\u984c\u63db\u6210\u65b0\u7248\u672c\u3001\u7528\u9019\u4e00\u6ce2\u91cd\u65b0\u4f30\uff1b\u6700\u5f8c\u4e00\u8d77\u505a\u56fa\u5b9a\u6821\u6e96\u3002"
      )
    ),
    kitDesign::kit_advanced(
      # 原文：這一回合不做的量尺
      shiny::h5("\u9019\u4e00\u56de\u5408\u4e0d\u505a\u7684\u91cf\u5c3a"),
      shiny::uiOutput(ns("exclude_ui")),
      # 原文：有量尺停下來、或這一回合不想做某個量尺時，勾它再按「開始校準」。
      hint("\u6709\u91cf\u5c3a\u505c\u4e0b\u4f86\u3001\u6216\u9019\u4e00\u56de\u5408\u4e0d\u60f3\u505a\u67d0\u500b\u91cf\u5c3a\u6642\uff0c\u52fe\u5b83\u518d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002",
           # 原文：勾了的量尺不校準，題庫與連結都不會改變，固定檔的 checks.csv 會記一列 excluded；
           "\u52fe\u4e86\u7684\u91cf\u5c3a\u4e0d\u6821\u6e96\uff0c\u984c\u5eab\u8207\u9023\u7d50\u90fd\u4e0d\u6703\u6539\u8b8a\uff0c\u56fa\u5b9a\u6a94\u7684 checks.csv \u6703\u8a18\u4e00\u5217 excluded\uff1b",
           # 原文：其他量尺照常做，也可以產生固定檔。之後的回合可以再做它。
           "\u5176\u4ed6\u91cf\u5c3a\u7167\u5e38\u505a\uff0c\u4e5f\u53ef\u4ee5\u7522\u751f\u56fa\u5b9a\u6a94\u3002\u4e4b\u5f8c\u7684\u56de\u5408\u53ef\u4ee5\u518d\u505a\u5b83\u3002"),
      # 原文：用哪一個題庫
      shiny::h5("\u7528\u54ea\u4e00\u500b\u984c\u5eab"),
      shiny::tags$p(class = "kit-hint",
                    # 原文：預設照月齡的規則：自己的題庫沒有校準過這個月齡、另一個世代的有時，
                    "\u9810\u8a2d\u7167\u6708\u9f61\u7684\u898f\u5247\uff1a\u81ea\u5df1\u7684\u984c\u5eab\u6c92\u6709\u6821\u6e96\u904e\u9019\u500b\u6708\u9f61\u3001\u53e6\u4e00\u500b\u4e16\u4ee3\u7684\u6709\u6642\uff0c",
                    # 原文：借用另一世代題庫。波次的單位不是月時不借，用自己的題庫。
                    "\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\u3002\u6ce2\u6b21\u7684\u55ae\u4f4d\u4e0d\u662f\u6708\u6642\u4e0d\u501f\uff0c\u7528\u81ea\u5df1\u7684\u984c\u5eab\u3002",
                    # 原文：要改的話，在「題庫」欄選別的題庫，再按「開始校準」。
                    "\u8981\u6539\u7684\u8a71\uff0c\u5728\u300c\u984c\u5eab\u300d\u6b04\u9078\u5225\u7684\u984c\u5eab\uff0c\u518d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002"),
      shiny::uiOutput(ns("banks")),
      # 原文：flag 的條件
      shiny::h5("flag \u7684\u689d\u4ef6"),
      # 原文：偏移題超過已判斷題的比例
      shiny::numericInput(ns("flag_share"), "\u504f\u79fb\u984c\u8d85\u904e\u5df2\u5224\u65b7\u984c\u7684\u6bd4\u4f8b", 0.2, 0, 1, 0.05),
      # 原文：可固定的題少於幾題（空白：依量尺的題數）
      shiny::numericInput(ns("flag_fixed"), "\u53ef\u56fa\u5b9a\u7684\u984c\u5c11\u65bc\u5e7e\u984c\uff08\u7a7a\u767d\uff1a\u4f9d\u91cf\u5c3a\u7684\u984c\u6578\uff09", NA, 1, 50,
                          1),
      hint(
        # 原文：偏移題太多，或可固定的題太少，這個量尺會被 flag。
        "\u504f\u79fb\u984c\u592a\u591a\uff0c\u6216\u53ef\u56fa\u5b9a\u7684\u984c\u592a\u5c11\uff0c\u9019\u500b\u91cf\u5c3a\u6703\u88ab flag\u3002",
        # 原文：「可固定的題少於幾題」空白時，每個量尺用 5 與題數的一半（無條件進位）中小的那個：
        "\u300c\u53ef\u56fa\u5b9a\u7684\u984c\u5c11\u65bc\u5e7e\u984c\u300d\u7a7a\u767d\u6642\uff0c\u6bcf\u500b\u91cf\u5c3a\u7528 5 \u8207\u984c\u6578\u7684\u4e00\u534a\uff08\u7121\u689d\u4ef6\u9032\u4f4d\uff09\u4e2d\u5c0f\u7684\u90a3\u500b\uff1a",
        # 原文：10 題以上是 5 題；4 題的量尺是 2 題、5 題的量尺是 3 題。
        "10 \u984c\u4ee5\u4e0a\u662f 5 \u984c\uff1b4 \u984c\u7684\u91cf\u5c3a\u662f 2 \u984c\u30015 \u984c\u7684\u91cf\u5c3a\u662f 3 \u984c\u3002",
        ITEMS_FLAG_NOTE,
        # 原文：只有 kits 出錯、沒有可固定的題，或沒有題可以估計時，量尺才會停下來。
        "\u53ea\u6709 kits \u51fa\u932f\u3001\u6c92\u6709\u53ef\u56fa\u5b9a\u7684\u984c\uff0c\u6216\u6c92\u6709\u984c\u53ef\u4ee5\u4f30\u8a08\u6642\uff0c\u91cf\u5c3a\u624d\u6703\u505c\u4e0b\u4f86\u3002"
      ),
      # 原文：進階設定（這一回合不做的量尺、用哪一個題庫、flag 的條件）
      title = "\u9032\u968e\u8a2d\u5b9a\uff08\u9019\u4e00\u56de\u5408\u4e0d\u505a\u7684\u91cf\u5c3a\u3001\u7528\u54ea\u4e00\u500b\u984c\u5eab\u3001flag \u7684\u689d\u4ef6\uff09"
    ),
    # 進階設定收起來時也看得到：現在勾了哪些「這一回合不做」
    shiny::uiOutput(ns("exclude_line")),
    shiny::uiOutput(ns("settings")),
    # 原文：開始校準
    kitDesign::kit_button(ns("run"), "\u958b\u59cb\u6821\u6e96", "primary"),
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
        # 原文：資料或設定改了，要重新校準。
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stale", "\u8cc7\u6599\u6216\u8a2d\u5b9a\u6539\u4e86\uff0c\u8981\u91cd\u65b0\u6821\u6e96\u3002"),
                       # 原文：舊的結果已清除。請再按「開始校準」。
                       shiny::tags$p("\u820a\u7684\u7d50\u679c\u5df2\u6e05\u9664\u3002\u8acb\u518d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002")))
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
      # 原文：先在「① 資料」選好這一回合的資料。
      if (is.null(b)) return(shiny::tags$p(class = "kit-hint", "\u5148\u5728\u300c\u2460 \u8cc7\u6599\u300d\u9078\u597d\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u3002"))
      items_bank_table(ns, b)
    })
    # 每一列選的題庫（還沒有選單時是規則選的）
    bank_picks <- shiny::reactive({
      b <- banks()
      if (is.null(b)) return(NULL)
      purrr::map_chr(seq_len(nrow(b)), \(i) input[[paste0("bank_", i)]] %||% b$rule[i])
    })
    # why 欄跟著選擇。每次讀最新的表：表變短時（例如下一回合從 4 個量尺變 2 個），
    # 之前建的 why_3、why_4 不再有對應的列，也還沒有新的選單值時，不算
    shiny::observe({
      b <- banks()
      if (is.null(b)) return()
      purrr::walk(seq_len(nrow(b)), \(i) {
        output[[paste0("why_", i)]] <- shiny::renderText({
          b <- banks()
          pick <- bank_picks()
          if (is.null(b) || i > nrow(b) || length(pick) != nrow(b)) return(NULL)
          kitsui_bank_why(b, pick)[i]
        })
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
        # 原文：這一回合不做： | （共 
        "\u9019\u4e00\u56de\u5408\u4e0d\u505a\uff1a", kitsui_first(scale_label(ex), 6), "\uff08\u5171 ", kitDesign::kit_num(length(ex)),
        # 原文： 個量尺；在上面的「進階設定」改）。
        " \u500b\u91cf\u5c3a\uff1b\u5728\u4e0a\u9762\u7684\u300c\u9032\u968e\u8a2d\u5b9a\u300d\u6539\uff09\u3002"
      ))
    })

    run_round <- function(exclude) {
      d <- data()
      if (is.null(d)) {
        # 原文：資料還不能用，沒有校準。
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u8cc7\u6599\u9084\u4e0d\u80fd\u7528\uff0c\u6c92\u6709\u6821\u6e96\u3002"),
                       # 原文：怎麼辦：回到「① 資料」，看【停下來】的原因，處理好再回來。
                       shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u56de\u5230\u300c\u2460 \u8cc7\u6599\u300d\uff0c\u770b\u3010\u505c\u4e0b\u4f86\u3011\u7684\u539f\u56e0\uff0c\u8655\u7406\u597d\u518d\u56de\u4f86\u3002")))
        # 原文：資料還不能用，沒有校準。
        kitsui_after_run("stop", "\u8cc7\u6599\u9084\u4e0d\u80fd\u7528\uff0c\u6c92\u6709\u6821\u6e96\u3002")
        return()
      }
      # 這一次用的設定：存進結果（res$args），程式碼與「設定改了」的比較都用它
      args <- items_args(input$flag_share, input$flag_fixed, bank_choice(), exclude)
      # 原文：題目校準中…
      res <- shiny::withProgress(message = "\u984c\u76ee\u6821\u6e96\u4e2d\u2026", value = 0, {
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
        # 原文：校準停下來了，沒有結果。
        note(kitDesign::kit_status_ui(kitDesign::kit_status("stop", "\u6821\u6e96\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u7d50\u679c\u3002"),
                       # 原文：校準
                       shiny::tags$p(kitsui_error_ui(kit_error_text(res, "\u6821\u6e96"))),
                       # 原文：怎麼辦：看上面的原因；資料有問題時回到「① 資料」處理，
                       shiny::tags$p("\u600e\u9ebc\u8fa6\uff1a\u770b\u4e0a\u9762\u7684\u539f\u56e0\uff1b\u8cc7\u6599\u6709\u554f\u984c\u6642\u56de\u5230\u300c\u2460 \u8cc7\u6599\u300d\u8655\u7406\uff0c",
                                     # 原文：或勾「這一回合不做」出問題的量尺，再按「開始校準」。
                                     "\u6216\u52fe\u300c\u9019\u4e00\u56de\u5408\u4e0d\u505a\u300d\u51fa\u554f\u984c\u7684\u91cf\u5c3a\uff0c\u518d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002")))
        # 原文：校準停下來了，沒有結果。
        kitsui_after_run("stop", "\u6821\u6e96\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u7d50\u679c\u3002")
        return()
      }
      note(NULL)
      used(d)
      res$args <- args
      round(res)
      seen <- kitsui_no_info_stopped(res)
      if (length(seen) > 0) no_info(c(seen, no_info()[setdiff(names(no_info()), names(seen))]))
      if (isTRUE(res$ok)) {
        # 原文：題目校準完成。
        kitsui_after_run("done", "\u984c\u76ee\u6821\u6e96\u5b8c\u6210\u3002")
      } else {
        # 原文：有量尺校準停下來了，原因在狀態框。
        kitsui_after_run("stop", "\u6709\u91cf\u5c3a\u6821\u6e96\u505c\u4e0b\u4f86\u4e86\uff0c\u539f\u56e0\u5728\u72c0\u614b\u6846\u3002")
      }
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
        # 原文：還沒有校準。按「開始校準」。
        return(shiny::tags$p(class = "kit-hint", "\u9084\u6c92\u6709\u6821\u6e96\u3002\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002"))
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
      # 原文：這個量尺停下來了，沒有題目參數。
      shiny::validate(shiny::need(x$item, "\u9019\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u984c\u76ee\u53c3\u6578\u3002"))
      # 畫面與下載的欄名：beta -> difficulty（kits 與固定檔裡仍叫 beta）
      kitsui_display_names(as.data.frame(stats::coef(x$item)))
    })
    output$coef <- kit_table(items_coef_display(coef_raw()))
    coef_csv <- shiny::reactive(items_with_scale(coef_raw(), sel()$scale))

    fit_tab <- shiny::reactive({
      x <- sel()
      # 原文：這個量尺停下來了，沒有適配度。
      shiny::validate(shiny::need(x$item, "\u9019\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u9069\u914d\u5ea6\u3002"))
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
      # 原文：這個量尺停下來了，沒有校準結果可以畫。
      shiny::validate(shiny::need(x$item, "\u9019\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u6821\u6e96\u7d50\u679c\u53ef\u4ee5\u756b\u3002"))
      type <- input$plot_type %||% "category"
      if (type == "drift") {
        unjudged <- any(x$log$check == "drift not judged, kept fixed")
        shiny::validate(shiny::need(x$drift, if (unjudged) {
          # 原文：這一回合的資料不能判斷偏移，題庫的題都固定在題庫的值，沒有偏移把關圖。
          "\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u4e0d\u80fd\u5224\u65b7\u504f\u79fb\uff0c\u984c\u5eab\u7684\u984c\u90fd\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\uff0c\u6c92\u6709\u504f\u79fb\u628a\u95dc\u5716\u3002"
        } else {
          # 原文：這個量尺沒有題庫，是自由校準，沒有偏移把關圖。
          "\u9019\u500b\u91cf\u5c3a\u6c92\u6709\u984c\u5eab\uff0c\u662f\u81ea\u7531\u6821\u6e96\uff0c\u6c92\u6709\u504f\u79fb\u628a\u95dc\u5716\u3002"
        }))
      }
      # 依類別上色的圖（分數、波次）換成 KIT 的類別色（kit_chart_categorical()）
      # 原文：畫圖中…
      p <- shiny::withProgress(message = "\u756b\u5716\u4e2d\u2026", value = 0.5, tryCatch(switch(
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
        # 原文：這張圖畫不出來。請換一種圖或別的量尺；R 的原文：
        shiny::validate("\u9019\u5f35\u5716\u756b\u4e0d\u51fa\u4f86\u3002\u8acb\u63db\u4e00\u7a2e\u5716\u6216\u5225\u7684\u91cf\u5c3a\uff1bR \u7684\u539f\u6587\uff1a", conditionMessage(p))
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
      # 原文：下載這張圖（HTML，畫的是上面選的題）
      session, "dl_plot", "\u4e0b\u8f09\u9019\u5f35\u5716\uff08HTML\uff0c\u756b\u7684\u662f\u4e0a\u9762\u9078\u7684\u984c\uff09", plot_obj, plot_name
    )
    # 原文：下載題目參數（CSV）
    csv_dl("dl_coef", "\u4e0b\u8f09\u984c\u76ee\u53c3\u6578\uff08CSV\uff09", coef_csv, name("item_coef"))
    # 原文：下載每一題的狀態（CSV）
    csv_dl("dl_status", "\u4e0b\u8f09\u6bcf\u4e00\u984c\u7684\u72c0\u614b\uff08CSV\uff09", status_csv, name("fixed_status"))
    # 原文：下載偏移表（CSV）
    csv_dl("dl_drift", "\u4e0b\u8f09\u504f\u79fb\u8868\uff08CSV\uff09", drift_csv, name("drift"))
    # 原文：下載適配度（CSV）
    csv_dl("dl_fit", "\u4e0b\u8f09\u9069\u914d\u5ea6\uff08CSV\uff09", fit_csv, name("item_fit"))
    # 原文：下載這一回合的紀錄（CSV，所有量尺）
    csv_dl("dl_log", "\u4e0b\u8f09\u9019\u4e00\u56de\u5408\u7684\u7d00\u9304\uff08CSV\uff0c\u6240\u6709\u91cf\u5c3a\uff09", \() items_log_csv(round()$log),
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
        flag_fixed = round()$flag_fixed[x$scale] %|NA|% 5, plan = plan, picks = d$picks,
        log = round()$log
      ) |>
        kitsui_code_drift_plot()
      info <- if (!is.null(x$item)) {
        paste("#", utils::capture.output(print(kits::kits_info(x$item))))
      }
      choice <- if (!is.null(plan) && any(plan$manual)) {
        # 原文：# 這一次手動選了題庫（kitsui_round(bank_choice = )）：
        c("# \u9019\u4e00\u6b21\u624b\u52d5\u9078\u4e86\u984c\u5eab\uff08kitsui_round(bank_choice = )\uff09\uff1a",
          paste("#", unique(plan$why[plan$manual])))
      }
      # 原文：# 這一次在 app 裡的設定（kits_info）
      paste(c(code, "", choice, "# \u9019\u4e00\u6b21\u5728 app \u88e1\u7684\u8a2d\u5b9a\uff08kits_info\uff09", info), collapse = "\n")
    })
    # 用 R 怎麼做：打開時才算；每行最多 70 欄（kitDesign::kit_code()）
    # 原文：kits 版
    output$code <- kitDesign::kit_render_code_panel(\() list("kits \u7248" = code_text()))

    output$next_step <- shiny::renderUI({
      res <- round()
      if (!isTRUE(res$ok) || stale()) return(NULL)
      # 原文：這個量尺校準好了。 | 每個量尺都校準好了。
      done <- if (length(res$items) == 1) "\u9019\u500b\u91cf\u5c3a\u6821\u6e96\u597d\u4e86\u3002" else "\u6bcf\u500b\u91cf\u5c3a\u90fd\u6821\u6e96\u597d\u4e86\u3002"
      # 原文：下一步是把各世代的分數連到同一把尺上。
      kitDesign::kit_next_step(paste0(done, "\u4e0b\u4e00\u6b65\u662f\u628a\u5404\u4e16\u4ee3\u7684\u5206\u6578\u9023\u5230\u540c\u4e00\u628a\u5c3a\u4e0a\u3002"), ns("go_link"),
                               # 原文：下一步：連結
                               "\u4e0b\u4e00\u6b65\uff1a\u9023\u7d50")
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
    # 原文：看一個量尺
    shiny::h4("\u770b\u4e00\u500b\u91cf\u5c3a"),
    # 原文：量尺
    shiny::selectInput(ns("scale"), "\u91cf\u5c3a", sc, pick),
    shiny::uiOutput(ns("scale_info")),
    # 原文：圖
    shiny::radioButtons(ns("plot_type"), "\u5716", ITEMS_PLOTS, inline = TRUE),
    shiny::uiOutput(ns("plot_help")),
    shiny::uiOutput(ns("plot_items_ui")),
    shiny::uiOutput(ns("plot_note")),
    plotly::plotlyOutput(ns("plot"), height = "520px"),
    dl(ns("dl_plot")),
    kitDesign::kit_details_row(
      kitDesign::kit_detail(
        # 原文：每個量尺（ |  個）
        paste0("\u6bcf\u500b\u91cf\u5c3a\uff08", kitDesign::kit_num(n_rows), " \u500b\uff09"),
        # 原文：一個量尺一列。偏移、固定是題數；被 flag 的量尺照常校準，但要看一下。
        hint("\u4e00\u500b\u91cf\u5c3a\u4e00\u5217\u3002\u504f\u79fb\u3001\u56fa\u5b9a\u662f\u984c\u6578\uff1b\u88ab flag \u7684\u91cf\u5c3a\u7167\u5e38\u6821\u6e96\uff0c\u4f46\u8981\u770b\u4e00\u4e0b\u3002"),
        shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("overview")))
      ),
      # 原文：要注意
      if (has_attention) kitDesign::kit_detail("\u8981\u6ce8\u610f", shiny::uiOutput(ns("attention"))),
      kitDesign::kit_detail(
        # 原文：題目參數
        "\u984c\u76ee\u53c3\u6578",
        # 原文：這個量尺每一題每一個步驟一列（coef(item)）。
        hint("\u9019\u500b\u91cf\u5c3a\u6bcf\u4e00\u984c\u6bcf\u4e00\u500b\u6b65\u9a5f\u4e00\u5217\uff08coef(item)\uff09\u3002"),
        scroll(ns("coef")),
        # 原文：difficulty、difficulty_se：步驟難度與標準誤（logit；kits 裡叫 beta、beta_se）。
        hint("difficulty\u3001difficulty_se\uff1a\u6b65\u9a5f\u96e3\u5ea6\u8207\u6a19\u6e96\u8aa4\uff08logit\uff1bkits \u88e1\u53eb beta\u3001beta_se\uff09\u3002",
             # 原文：threshold、threshold_se：Thurstonian 門檻與標準誤（kits 裡叫 thr）；
             paste0("threshold\u3001threshold_se\uff1aThurstonian \u9580\u6abb\u8207\u6a19\u6e96\u8aa4\uff08kits \u88e1\u53eb thr\uff09\uff1b",
                    # 原文：能力值（ability）到這裡時，有一半的機會得到這個分數以上。
                    "\u80fd\u529b\u503c\uff08ability\uff09\u5230\u9019\u88e1\u6642\uff0c\u6709\u4e00\u534a\u7684\u6a5f\u6703\u5f97\u5230\u9019\u500b\u5206\u6578\u4ee5\u4e0a\u3002"),
             # 原文：固定在題庫的值的步驟沒有標準誤，表上寫「固定」（CSV 裡是 0）。
             "\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\u7684\u6b65\u9a5f\u6c92\u6709\u6a19\u6e96\u8aa4\uff0c\u8868\u4e0a\u5beb\u300c\u56fa\u5b9a\u300d\uff08CSV \u88e1\u662f 0\uff09\u3002"),
        dl(ns("dl_coef"))
      ),
      kitDesign::kit_detail(
        # 原文：固定了哪些題
        "\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c",
        # 原文：每一題一列：固定在題庫的值、換成新版本、新題，或這一回合拿掉。
        hint("\u6bcf\u4e00\u984c\u4e00\u5217\uff1a\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\u3001\u63db\u6210\u65b0\u7248\u672c\u3001\u65b0\u984c\uff0c\u6216\u9019\u4e00\u56de\u5408\u62ff\u6389\u3002",
             # 原文：固定的題用題庫的參數，這一回合不重新估計。
             "\u56fa\u5b9a\u7684\u984c\u7528\u984c\u5eab\u7684\u53c3\u6578\uff0c\u9019\u4e00\u56de\u5408\u4e0d\u91cd\u65b0\u4f30\u8a08\u3002"),
        shiny::uiOutput(ns("fixed_status")),
        dl(ns("dl_status"))
      ),
      kitDesign::kit_detail(
        # 原文：偏移把關
        "\u504f\u79fb\u628a\u95dc",
        # 原文：kits_drift() 先把這一回合的資料自由校準，得到每一步的門檻。
        hint("kits_drift() \u5148\u628a\u9019\u4e00\u56de\u5408\u7684\u8cc7\u6599\u81ea\u7531\u6821\u6e96\uff0c\u5f97\u5230\u6bcf\u4e00\u6b65\u7684\u9580\u6abb\u3002",
             # 原文：再用 mean/sigma 畫一條線，把新的門檻換到題庫的量尺上，和題庫的門檻比。
             "\u518d\u7528 mean/sigma \u756b\u4e00\u689d\u7dda\uff0c\u628a\u65b0\u7684\u9580\u6abb\u63db\u5230\u984c\u5eab\u7684\u91cf\u5c3a\u4e0a\uff0c\u548c\u984c\u5eab\u7684\u9580\u6abb\u6bd4\u3002",
             # 原文：離線的距離大於 %s 個標準差（所有判斷的步驟算的）的步驟會被拿掉。
             sprintf("\u96e2\u7dda\u7684\u8ddd\u96e2\u5927\u65bc %s \u500b\u6a19\u6e96\u5dee\uff08\u6240\u6709\u5224\u65b7\u7684\u6b65\u9a5f\u7b97\u7684\uff09\u7684\u6b65\u9a5f\u6703\u88ab\u62ff\u6389\u3002", cut),
             # 原文：有步驟被拿掉的題就是 drift = TRUE，會換成新版本重新估計。
             "\u6709\u6b65\u9a5f\u88ab\u62ff\u6389\u7684\u984c\u5c31\u662f drift = TRUE\uff0c\u6703\u63db\u6210\u65b0\u7248\u672c\u91cd\u65b0\u4f30\u8a08\u3002",
             # 原文：步驟的上下各要有 %s 人才判斷；不判斷的題 drift 是空的，reason 寫原因。
             sprintf("\u6b65\u9a5f\u7684\u4e0a\u4e0b\u5404\u8981\u6709 %s \u4eba\u624d\u5224\u65b7\uff1b\u4e0d\u5224\u65b7\u7684\u984c drift \u662f\u7a7a\u7684\uff0creason \u5beb\u539f\u56e0\u3002", min_n),
             # 原文：suggest_fix = TRUE 的題固定在題庫的值（沿用舊參數）。欄位說明在表的下面。
             "suggest_fix = TRUE \u7684\u984c\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\uff08\u6cbf\u7528\u820a\u53c3\u6578\uff09\u3002\u6b04\u4f4d\u8aaa\u660e\u5728\u8868\u7684\u4e0b\u9762\u3002"),
        scroll(ns("drift")),
        shiny::uiOutput(ns("drift_note")),
        dl(ns("dl_drift"))
      ),
      kitDesign::kit_detail(
        # 原文：適配度
        "\u9069\u914d\u5ea6",
        # 原文：n 是用到的作答數。
        hint("n \u662f\u7528\u5230\u7684\u4f5c\u7b54\u6578\u3002",
             # 原文：pvalue 是平均得分占最高分的比例；只有 0、1 分的題，就是通過的比例。
             "pvalue \u662f\u5e73\u5747\u5f97\u5206\u5360\u6700\u9ad8\u5206\u7684\u6bd4\u4f8b\uff1b\u53ea\u6709 0\u30011 \u5206\u7684\u984c\uff0c\u5c31\u662f\u901a\u904e\u7684\u6bd4\u4f8b\u3002",
             # 原文：rir 是這一題和其他題總分的相關；接近 0 或是負的，要看一下題目。
             "rir \u662f\u9019\u4e00\u984c\u548c\u5176\u4ed6\u984c\u7e3d\u5206\u7684\u76f8\u95dc\uff1b\u63a5\u8fd1 0 \u6216\u662f\u8ca0\u7684\uff0c\u8981\u770b\u4e00\u4e0b\u984c\u76ee\u3002",
             # 原文：infit、outfit 接近 1 最好。
             "infit\u3001outfit \u63a5\u8fd1 1 \u6700\u597d\u3002",
             # 原文：flag 是 kits 依 infit、outfit 給的判斷：兩個都在 0.6–1.4 之間是 ok。
             "flag \u662f kits \u4f9d infit\u3001outfit \u7d66\u7684\u5224\u65b7\uff1a\u5169\u500b\u90fd\u5728 0.6\u20131.4 \u4e4b\u9593\u662f ok\u3002",
             # 原文：任一個大於 1.4 是 underfit（比模型預期亂），小於 0.6 是 overfit（比模型預期整齊）。
             "\u4efb\u4e00\u500b\u5927\u65bc 1.4 \u662f underfit\uff08\u6bd4\u6a21\u578b\u9810\u671f\u4e82\uff09\uff0c\u5c0f\u65bc 0.6 \u662f overfit\uff08\u6bd4\u6a21\u578b\u9810\u671f\u6574\u9f4a\uff09\u3002"),
        scroll(ns("fit")),
        dl(ns("dl_fit"))
      ),
      kitDesign::kit_detail(
        # 原文：這一回合的紀錄
        "\u9019\u4e00\u56de\u5408\u7684\u7d00\u9304",
        # 原文：新版本、併分數、分數類別的問題、借用另一世代題庫、停下來的原因，
        hint("\u65b0\u7248\u672c\u3001\u4f75\u5206\u6578\u3001\u5206\u6578\u985e\u5225\u7684\u554f\u984c\u3001\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\u3001\u505c\u4e0b\u4f86\u7684\u539f\u56e0\uff0c",
             # 原文：都會寫進固定檔的 checks.csv。status 是 warn 的列要特別看一下。
             "\u90fd\u6703\u5beb\u9032\u56fa\u5b9a\u6a94\u7684 checks.csv\u3002status \u662f warn \u7684\u5217\u8981\u7279\u5225\u770b\u4e00\u4e0b\u3002"),
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
  # 原文：校準第 %d 個量尺（共 %d 個）：%s
  shiny::incProgress(1 / n, detail = sprintf("\u6821\u6e96\u7b2c %d \u500b\u91cf\u5c3a\uff08\u5171 %d \u500b\uff09\uff1a%s", i, n, scale))
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
    # 原文：這個量尺校準好了。 | %s 個量尺都校準好了。
    done <- if (n_ok == 1) "\u9019\u500b\u91cf\u5c3a\u6821\u6e96\u597d\u4e86\u3002" else sprintf("%s \u500b\u91cf\u5c3a\u90fd\u6821\u6e96\u597d\u4e86\u3002", kitDesign::kit_num(n_ok))
    points <- items_attention_points(res, checks)
    if (length(points) == 0) return(kitDesign::kit_status_ui(kitDesign::kit_status("done", done)))
    return(kitDesign::kit_status_ui(kitDesign::kit_status("warn", done, points = points),
                     # 原文：每一項的細節在下面的「▸ 要注意」。
                     shiny::tags$p("\u6bcf\u4e00\u9805\u7684\u7d30\u7bc0\u5728\u4e0b\u9762\u7684\u300c\u25b8 \u8981\u6ce8\u610f\u300d\u3002")))
  }
  why <- dplyr::filter(res$log, .data$check %in% c("stop", "error"))
  skip <- items_stopped_data(res)
  stopped <- unique(why$scale)
  line <- if (length(stopped) == 1) {
    # 原文： 停下來了，這一回合還不能產生新的固定檔。
    paste0(scale_label(stopped), " \u505c\u4e0b\u4f86\u4e86\uff0c\u9019\u4e00\u56de\u5408\u9084\u4e0d\u80fd\u7522\u751f\u65b0\u7684\u56fa\u5b9a\u6a94\u3002")
  } else {
    # 原文：%s 個量尺停下來了，這一回合還不能產生新的固定檔。
    sprintf("%s \u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u9019\u4e00\u56de\u5408\u9084\u4e0d\u80fd\u7522\u751f\u65b0\u7684\u56fa\u5b9a\u6a94\u3002", kitDesign::kit_num(length(stopped)))
  }
  # 第一層：依原因分組的白話說明（每一種一點，含怎麼辦）；每個量尺的原因與 kits 的原文收起來
  per_scale <- lapply(seq_len(nrow(why)), \(i) {
    label <- scale_label(why$scale[i])
    # 資訊不足：「〈量尺〉這一回合資訊不足，無法估計（…）。按「這一回合不做」…」
    # 原文： 這一回合
    if (startsWith(why$detail[i], paste0(label, " \u9019\u4e00\u56de\u5408", KITSUI_NO_INFO_ZH))) {
      rest <- substring(why$detail[i], nchar(label) + 1)
      return(shiny::tags$li(shiny::tags$strong(label), kitsui_error_ui(rest)))
    }
    # 原文：kits 出錯 | 停下來
    kind <- if (why$check[i] == "error") "kits \u51fa\u932f" else "\u505c\u4e0b\u4f86"
    # 原文：（ | ）：
    shiny::tags$li(shiny::tags$strong(why$scale[i]), "\uff08", kind, "\uff09\uff1a",
                   kitsui_error_ui(why$detail[i]))
  })
  skip_label <- if (length(skip) == 1) {
    # 原文：這一回合不做  | ，再校準一次
    paste0("\u9019\u4e00\u56de\u5408\u4e0d\u505a ", scale_label(skip), "\uff0c\u518d\u6821\u6e96\u4e00\u6b21")
  } else {
    # 原文：這一回合不做這 %s 個量尺，再校準一次
    sprintf("\u9019\u4e00\u56de\u5408\u4e0d\u505a\u9019 %s \u500b\u91cf\u5c3a\uff0c\u518d\u6821\u6e96\u4e00\u6b21", kitDesign::kit_num(length(skip)))
  }
  kitDesign::kit_status_ui(kitDesign::kit_status(
    "stop", line, points = items_stop_points(why, n_visits)),
    shiny::tags$details(
      class = "kit-status-notes",
      # 原文：每個量尺停下來的原因（%s 個）
      shiny::tags$summary(sprintf("\u6bcf\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u7684\u539f\u56e0\uff08%s \u500b\uff09", kitDesign::kit_num(length(stopped)))),
      shiny::tags$ul(per_scale)
    ),
    shiny::tags$p(
      class = "kit-status-action",
      # 原文：怎麼辦：
      shiny::tags$strong("\u600e\u9ebc\u8fa6\uff1a"),
      # 原文：照上面每一點的說明改好資料，再按「開始校準」。
      "\u7167\u4e0a\u9762\u6bcf\u4e00\u9ede\u7684\u8aaa\u660e\u6539\u597d\u8cc7\u6599\uff0c\u518d\u6309\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002",
      # 原文：其他 %s 個量尺有校準結果，可以先看下面的圖。
      if (n_ok > 0) sprintf("\u5176\u4ed6 %s \u500b\u91cf\u5c3a\u6709\u6821\u6e96\u7d50\u679c\uff0c\u53ef\u4ee5\u5148\u770b\u4e0b\u9762\u7684\u5716\u3002", kitDesign::kit_num(n_ok))
    ),
    if (!is.null(skip_id) && length(skip) > 0 && items_all_skipped(res, skip)) {
      # 停下來的就是這一回合全部的量尺：不做它們就沒有東西可以校準，不給這個選擇
      shiny::tags$p(class = "kit-status-action",
                    # 原文：這一回合只有這些量尺，先不做它們就沒有東西可以校準，所以請照上面的說明改好資料。
                    "\u9019\u4e00\u56de\u5408\u53ea\u6709\u9019\u4e9b\u91cf\u5c3a\uff0c\u5148\u4e0d\u505a\u5b83\u5011\u5c31\u6c92\u6709\u6771\u897f\u53ef\u4ee5\u6821\u6e96\uff0c\u6240\u4ee5\u8acb\u7167\u4e0a\u9762\u7684\u8aaa\u660e\u6539\u597d\u8cc7\u6599\u3002")
    } else if (!is.null(skip_id) && length(skip) > 0) {
      shiny::tags$p(
        class = "kit-status-action",
        # 原文：也可以這一回合先不做停下來的量尺：其他量尺照常做、可以產生固定檔；
        "\u4e5f\u53ef\u4ee5\u9019\u4e00\u56de\u5408\u5148\u4e0d\u505a\u505c\u4e0b\u4f86\u7684\u91cf\u5c3a\uff1a\u5176\u4ed6\u91cf\u5c3a\u7167\u5e38\u505a\u3001\u53ef\u4ee5\u7522\u751f\u56fa\u5b9a\u6a94\uff1b",
        # 原文：這些量尺的題庫與連結不會改變，之後的回合可以再做。
        "\u9019\u4e9b\u91cf\u5c3a\u7684\u984c\u5eab\u8207\u9023\u7d50\u4e0d\u6703\u6539\u8b8a\uff0c\u4e4b\u5f8c\u7684\u56de\u5408\u53ef\u4ee5\u518d\u505a\u3002",
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
  # 原文：分數類別有問題
  "category", "\u5206\u6578\u985e\u5225\u6709\u554f\u984c",
  "connect", paste("separate groups that share no item", "No item of the new data is in `ref`",
                   # 原文：沒有共同的題
                   "\u6c92\u6709\u5171\u540c\u7684\u984c", sep = "|"),
  # 原文：沒有人得 0 分
  "zero", paste("have a 0 score", "no 0 score", "\u6c92\u6709\u4eba\u5f97 0 \u5206", "gap in their observed scores",
                # 原文：分數不連續
                "Scores must be consecutive", "\u5206\u6578\u4e0d\u9023\u7e8c", sep = "|"),
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
           # 原文：（共  |  個）
           if (length(sc) > 1) paste0("\uff08\u5171 ", kitDesign::kit_num(length(sc)), " \u500b\uff09"))
  }
  out <- c(
    connect = paste0(
      # 原文：題目沒有連在一起： | 。各波次（或和題庫）之間沒有共同題，不能放在同一把尺上。
      "\u984c\u76ee\u6c92\u6709\u9023\u5728\u4e00\u8d77\uff1a", who("connect"), "\u3002\u5404\u6ce2\u6b21\uff08\u6216\u548c\u984c\u5eab\uff09\u4e4b\u9593\u6c92\u6709\u5171\u540c\u984c\uff0c\u4e0d\u80fd\u653e\u5728\u540c\u4e00\u628a\u5c3a\u4e0a\u3002",
      # 原文：怎麼辦：各波的同一題要用同一個題號，回 kitJoin 的「
      "\u600e\u9ebc\u8fa6\uff1a\u5404\u6ce2\u7684\u540c\u4e00\u984c\u8981\u7528\u540c\u4e00\u500b\u984c\u865f\uff0c\u56de kitJoin \u7684\u300c", KITJOIN_KITS_TAB,
      # 原文：」第 2 步「合成同一組題號」；有題庫時，題號要和之前的回合相同。
      "\u300d\u7b2c 2 \u6b65\u300c\u5408\u6210\u540c\u4e00\u7d44\u984c\u865f\u300d\uff1b\u6709\u984c\u5eab\u6642\uff0c\u984c\u865f\u8981\u548c\u4e4b\u524d\u7684\u56de\u5408\u76f8\u540c\u3002"
    ),
    zero = paste0(
      # 原文：有題沒有人得 0 分、分數不連續，或只有一種分數： | 。
      "\u6709\u984c\u6c92\u6709\u4eba\u5f97 0 \u5206\u3001\u5206\u6578\u4e0d\u9023\u7e8c\uff0c\u6216\u53ea\u6709\u4e00\u7a2e\u5206\u6578\uff1a", who("zero"), "\u3002",
      # 原文：分數要是連續的 0、1、2…；分數本來就從 0 開始、只是這一回合沒有人得到某些分數時，通常是人太少。
      "\u5206\u6578\u8981\u662f\u9023\u7e8c\u7684 0\u30011\u30012\u2026\uff1b\u5206\u6578\u672c\u4f86\u5c31\u5f9e 0 \u958b\u59cb\u3001\u53ea\u662f\u9019\u4e00\u56de\u5408\u6c92\u6709\u4eba\u5f97\u5230\u67d0\u4e9b\u5206\u6578\u6642\uff0c\u901a\u5e38\u662f\u4eba\u592a\u5c11\u3002",
      # 原文：怎麼辦：分數從 1 開始時，在 kitJoin 的「 | 」改成從 0 開始再上傳。
      "\u600e\u9ebc\u8fa6\uff1a\u5206\u6578\u5f9e 1 \u958b\u59cb\u6642\uff0c\u5728 kitJoin \u7684\u300c", KITJOIN_KITS_TAB, "\u300d\u6539\u6210\u5f9e 0 \u958b\u59cb\u518d\u4e0a\u50b3\u3002"
    ),
    few = paste0(
      # 原文：人太少、資訊不足：
      "\u4eba\u592a\u5c11\u3001\u8cc7\u8a0a\u4e0d\u8db3\uff1a", who("few"),
      # 原文：（不到  |  個列次） | 。
      if (any(few[cause == "few"])) paste0("\uff08\u4e0d\u5230 ", ITEMS_FEW_VISITS, " \u500b\u5217\u6b21\uff09"), "\u3002",
      # 原文：這一回合的人或題不夠，估不出來，資料本身沒有錯。
      "\u9019\u4e00\u56de\u5408\u7684\u4eba\u6216\u984c\u4e0d\u5920\uff0c\u4f30\u4e0d\u51fa\u4f86\uff0c\u8cc7\u6599\u672c\u8eab\u6c92\u6709\u932f\u3002",
      # 原文：怎麼辦：等資料多一些再做；這一回合可以先不做它們。
      "\u600e\u9ebc\u8fa6\uff1a\u7b49\u8cc7\u6599\u591a\u4e00\u4e9b\u518d\u505a\uff1b\u9019\u4e00\u56de\u5408\u53ef\u4ee5\u5148\u4e0d\u505a\u5b83\u5011\u3002"
    ),
    other = paste0(
      # 原文：其他原因： | 。
      "\u5176\u4ed6\u539f\u56e0\uff1a", who("other"), "\u3002",
      sub(paste0(KITSUI_KITS_SAYS, ".*$"), "", why$detail[cause == "other"][1]),
      # 原文：每個量尺的原因見下面的「每個量尺停下來的原因」。
      "\u6bcf\u500b\u91cf\u5c3a\u7684\u539f\u56e0\u898b\u4e0b\u9762\u7684\u300c\u6bcf\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u7684\u539f\u56e0\u300d\u3002"
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
    # 原文：固定的步驟和題庫不同： | 、 | 。
    if (length(bad) > 0) paste0("\u56fa\u5b9a\u7684\u6b65\u9a5f\u548c\u984c\u5eab\u4e0d\u540c\uff1a", paste(bad, collapse = "\u3001"), "\u3002"),
    # 原文：被 flag 的量尺： | 、 | 。
    if (length(flags) > 0) paste0("\u88ab flag \u7684\u91cf\u5c3a\uff1a", paste(flags, collapse = "\u3001"), "\u3002"),
    # 原文：有 %d 題分數類別有問題，已照規則處理；分數都沒有改。
    if (n_cat > 0) sprintf("\u6709 %d \u984c\u5206\u6578\u985e\u5225\u6709\u554f\u984c\uff0c\u5df2\u7167\u898f\u5247\u8655\u7406\uff1b\u5206\u6578\u90fd\u6c92\u6709\u6539\u3002", n_cat),
    if (length(res$excluded) > 0) {
      # 原文：這一回合不做： | 、 | 。
      paste0("\u9019\u4e00\u56de\u5408\u4e0d\u505a\uff1a", paste(scale_label(res$excluded), collapse = "\u3001"), "\u3002")
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
                    # 原文：固定的步驟和題庫不同： | 、 | 。
                    "\u56fa\u5b9a\u7684\u6b65\u9a5f\u548c\u984c\u5eab\u4e0d\u540c\uff1a", paste(bad, collapse = "\u3001"), "\u3002",
                    # 原文：這些步驟應該沿用題庫的值；請選這個量尺、打開「固定了哪些題」，
                    "\u9019\u4e9b\u6b65\u9a5f\u61c9\u8a72\u6cbf\u7528\u984c\u5eab\u7684\u503c\uff1b\u8acb\u9078\u9019\u500b\u91cf\u5c3a\u3001\u6253\u958b\u300c\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c\u300d\uff0c",
                    # 原文：把量尺與步驟告訴維護 kitsUI 的人。
                    "\u628a\u91cf\u5c3a\u8207\u6b65\u9a5f\u544a\u8a34\u7dad\u8b77 kitsUI \u7684\u4eba\u3002")
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
    # 原文：這一回合不做： | 、
    shiny::tags$p(shiny::tags$strong("\u9019\u4e00\u56de\u5408\u4e0d\u505a\uff1a"), paste(scale_label(excluded), collapse = "\u3001"),
                  # 原文：。這些量尺沒有校準，題庫與連結都不會改變；固定檔的 checks.csv 記為 excluded。
                  "\u3002\u9019\u4e9b\u91cf\u5c3a\u6c92\u6709\u6821\u6e96\uff0c\u984c\u5eab\u8207\u9023\u7d50\u90fd\u4e0d\u6703\u6539\u8b8a\uff1b\u56fa\u5b9a\u6a94\u7684 checks.csv \u8a18\u70ba excluded\u3002"),
    if (length(why) > 0) {
      shiny::tags$ul(lapply(names(why), \(s) {
        shiny::tags$li(class = "kit-warn-text",
                       # 原文： 這一回合 | ： | （
                       paste0(scale_label(s), " \u9019\u4e00\u56de\u5408", sub("\uff1a", "\uff08", why[[s]]),
                              # 原文：），已跳過；題庫與連結沒有改變。
                              "\uff09\uff0c\u5df2\u8df3\u904e\uff1b\u984c\u5eab\u8207\u9023\u7d50\u6c92\u6709\u6539\u8b8a\u3002"))
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
  # 原文：、
  show <- \(x, none) if (length(x) == 0 || all(is.na(x))) none else paste(x, collapse = "\u3001")
  # 原文：照規則 |  用  | 、
  bank <- \(x) if (length(x) == 0) "\u7167\u898f\u5247" else paste0(names(x), " \u7528 ", x, collapse = "\u3001")
  out <- c(
    # 原文：偏移題超過已判斷題的比例： | 空白 |  → 
    flag_share = paste0("\u504f\u79fb\u984c\u8d85\u904e\u5df2\u5224\u65b7\u984c\u7684\u6bd4\u4f8b\uff1a", show(old$flag_share, "\u7a7a\u767d"), " \u2192 ",
                        # 原文：空白
                        show(now$flag_share, "\u7a7a\u767d")),
    # 原文：可固定的題少於幾題： | 空白（依量尺的題數）
    flag_fixed = paste0("\u53ef\u56fa\u5b9a\u7684\u984c\u5c11\u65bc\u5e7e\u984c\uff1a", show(old$flag_fixed, "\u7a7a\u767d\uff08\u4f9d\u91cf\u5c3a\u7684\u984c\u6578\uff09"),
                        # 原文： →  | 空白（依量尺的題數）
                        " \u2192 ", show(now$flag_fixed, "\u7a7a\u767d\uff08\u4f9d\u91cf\u5c3a\u7684\u984c\u6578\uff09")),
    # 原文：用哪一個題庫： |  → 
    bank_choice = paste0("\u7528\u54ea\u4e00\u500b\u984c\u5eab\uff1a", bank(old$bank_choice), " \u2192 ", bank(now$bank_choice)),
    # 原文：這一回合不做的量尺： | 沒有 |  → 
    exclude = paste0("\u9019\u4e00\u56de\u5408\u4e0d\u505a\u7684\u91cf\u5c3a\uff1a", show(old$exclude, "\u6c92\u6709"), " \u2192 ",
                     # 原文：沒有
                     show(now$exclude, "\u6c92\u6709"))
  )
  same <- vapply(names(out), \(k) identical(old[[k]], now[[k]]), logical(1))
  unname(out[!same])
}

#' 改了設定、還沒重新校準時的狀態框：下面的結果是舊設定算的
#' @param changes items_args_changes()
#' @noRd
items_stale_status <- function(changes) {
  kitDesign::kit_status_ui(kitDesign::kit_status(
    # 原文：設定改了，請再按一次「開始校準」。
    "stale", "\u8a2d\u5b9a\u6539\u4e86\uff0c\u8acb\u518d\u6309\u4e00\u6b21\u300c\u958b\u59cb\u6821\u6e96\u300d\u3002", points = changes),
    # 原文：下面的結果還是用上一次的設定算的。重新校準以前，
    shiny::tags$p(paste0("\u4e0b\u9762\u7684\u7d50\u679c\u9084\u662f\u7528\u4e0a\u4e00\u6b21\u7684\u8a2d\u5b9a\u7b97\u7684\u3002\u91cd\u65b0\u6821\u6e96\u4ee5\u524d\uff0c",
                         # 原文：「③ 連結」和「④ 受試者評分」不會用它。
                         "\u300c\u2462 \u9023\u7d50\u300d\u548c\u300c\u2463 \u53d7\u8a66\u8005\u8a55\u5206\u300d\u4e0d\u6703\u7528\u5b83\u3002"))
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
    # 原文：分數類別有問題：
    shiny::tags$strong("\u5206\u6578\u985e\u5225\u6709\u554f\u984c\uff1a"),
    # 原文：有 %d 題分數類別有問題（沒有人得 0 分、分數不連續或只有一種分數）。
    sprintf("\u6709 %d \u984c\u5206\u6578\u985e\u5225\u6709\u554f\u984c\uff08\u6c92\u6709\u4eba\u5f97 0 \u5206\u3001\u5206\u6578\u4e0d\u9023\u7e8c\u6216\u53ea\u6709\u4e00\u7a2e\u5206\u6578\uff09\u3002", kept + out),
    # 原文：題庫裡的 %d 題固定在題庫的值。
    if (kept > 0) sprintf("\u984c\u5eab\u88e1\u7684 %d \u984c\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\u3002", kept),
    # 原文：不在題庫裡的 %d 題這一回合不放進來。
    if (out > 0) sprintf("\u4e0d\u5728\u984c\u5eab\u88e1\u7684 %d \u984c\u9019\u4e00\u56de\u5408\u4e0d\u653e\u9032\u4f86\u3002", out),
    # 原文：分數都沒有改；每一題的原因見各量尺的「這一回合的紀錄」。
    "\u5206\u6578\u90fd\u6c92\u6709\u6539\uff1b\u6bcf\u4e00\u984c\u7684\u539f\u56e0\u898b\u5404\u91cf\u5c3a\u7684\u300c\u9019\u4e00\u56de\u5408\u7684\u7d00\u9304\u300d\u3002"
  )
}

#' flag 的量尺：每個量尺一列，加上一句怎麼辦（沒有時 NULL）
#' @noRd
items_flag_note <- function(log) {
  flags <- dplyr::filter(log, .data$check == "flag")
  if (nrow(flags) == 0) return(NULL)
  shiny::tags$div(
    shiny::tags$ul(lapply(seq_len(nrow(flags)), \(i) {
      # 原文：（flag）：
      shiny::tags$li(class = "kit-warn-text", paste0(flags$scale[i], "\uff08flag\uff09\uff1a", flags$detail[i]))
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
    # 原文：；
    pick <- \(check) paste(log$detail[log$check == check], collapse = "\uff1b")
    stopped <- s %in% res$stopped
    tab <- checks[[s]]$tab
    tibble::tibble(
      # 原文：、
      scale = s, data = paste(p$data_scale, format(p$wave_m), collapse = "\u3001"),
      # 原文：停下來
      calibration = if (stopped) "\u505c\u4e0b\u4f86" else pick("calibration"),
      drift_n = if (stopped) NA_integer_ else sum(log$check == "drift"),
      fixed_n = if (is.null(tab)) NA_integer_ else sum(tab$kind %in% KITSUI_STATUS_FIXED),
      flag = pick("flag")
    )
  }) |>
    purrr::list_rbind()
  if (length(res$excluded) == 0) return(out)
  why <- kitsui_excluded_reasons(res$log)[res$excluded]
  # 原文：這一回合不做
  calibration <- ifelse(is.na(why) | !nzchar(why), "\u9019\u4e00\u56de\u5408\u4e0d\u505a",
                        # 原文：這一回合不做（ | ）
                        paste0("\u9019\u4e00\u56de\u5408\u4e0d\u505a\uff08", why, "\uff09"))
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
    # 原文：量尺 | 波次 | 題庫 | 原因
    shiny::tags$thead(shiny::tags$tr(th("\u91cf\u5c3a"), th("\u6ce2\u6b21"), th("\u984c\u5eab"), th("\u539f\u56e0"))),
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
    # 原文：%s：%d 題
    shiny::tags$li(paste0(sprintf("%s\uff1a%d \u984c", head, length(it)),
                          # 原文：（ | 、 | ）
                          "\uff08", paste(it, collapse = "\u3001"), "\uff09"))
  }
  some <- \(text, check) if (n(check) > 0) shiny::tags$li(sprintf(text, n(check)))
  stopped <- is.null(x$item)
  # 原文：停下來了（沒有校準結果）
  mode <- if (stopped) "\u505c\u4e0b\u4f86\u4e86\uff08\u6c92\u6709\u6821\u6e96\u7d50\u679c\uff09" else log$detail[log$check == "calibration"][1]
  flags <- log$detail[log$check == "flag"]
  shiny::tagList(
    shiny::tags$div(
      class = paste("kit-scale-info", if (stopped) "kit-warn-text"),
      # 原文：：
      shiny::tags$strong(x$scale, "\uff1a"), mode,
      shiny::tags$ul(
        if (!stopped) {
          # 原文：%s 列次、%s 題
          shiny::tags$li(sprintf("%s \u5217\u6b21\u3001%s \u984c", kitDesign::kit_num(nrow(x$X)),
                                 kitDesign::kit_num(ncol(x$X))))
        },
        lapply(log$detail[log$check == "borrowed bank"],
               # 原文：借用另一世代題庫：
               \(d) shiny::tags$li(paste0("\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\uff1a", d))),
        # 原文：偏移：%d 題
        some("\u504f\u79fb\uff1a%d \u984c", "drift"),
        # 原文：換成新版本：%d 題
        some("\u63db\u6210\u65b0\u7248\u672c\uff1a%d \u984c", "new version"),
        # 原文：估不出來、固定在題庫的值
        count("\u4f30\u4e0d\u51fa\u4f86\u3001\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c", "no CML estimate, kept fixed"),
        # 原文：分數類別有問題、固定在題庫的值
        count("\u5206\u6578\u985e\u5225\u6709\u554f\u984c\u3001\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c", "category problem, kept fixed"),
        # 原文：分數類別有問題、這一回合不放進來
        count("\u5206\u6578\u985e\u5225\u6709\u554f\u984c\u3001\u9019\u4e00\u56de\u5408\u4e0d\u653e\u9032\u4f86", "category problem, left out"),
        # 原文：不能判斷偏移、固定在題庫的值
        count("\u4e0d\u80fd\u5224\u65b7\u504f\u79fb\u3001\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c", "drift not judged, kept fixed"),
        # 原文：併分數（新步驟有缺口）：%d 題
        some("\u4f75\u5206\u6578\uff08\u65b0\u6b65\u9a5f\u6709\u7f3a\u53e3\uff09\uff1a%d \u984c", "capped")
      )
    ),
    if (length(flags) > 0) {
      # 原文：flag： | ；
      shiny::tags$p(class = "kit-warn-text", paste0("flag\uff1a", flags, collapse = "\uff1b"), " ",
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
  # 原文：這一回合有比題庫更高的分數：舊的步驟固定，新的步驟另外估
  "score range differs" = "\u9019\u4e00\u56de\u5408\u6709\u6bd4\u984c\u5eab\u66f4\u9ad8\u7684\u5206\u6578\uff1a\u820a\u7684\u6b65\u9a5f\u56fa\u5b9a\uff0c\u65b0\u7684\u6b65\u9a5f\u53e6\u5916\u4f30",
  # 原文：沒有人得 0 分或只有一種分數：固定在題庫的值
  "no 0 score or one score only" = "\u6c92\u6709\u4eba\u5f97 0 \u5206\u6216\u53ea\u6709\u4e00\u7a2e\u5206\u6578\uff1a\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c",
  # 原文：分數不連續：不判斷
  "score gap" = "\u5206\u6578\u4e0d\u9023\u7e8c\uff1a\u4e0d\u5224\u65b7",
  # 原文：步驟上下的人不夠：固定在題庫的值
  "too little information" = "\u6b65\u9a5f\u4e0a\u4e0b\u7684\u4eba\u4e0d\u5920\uff1a\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c"
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
      # 原文：這個量尺停下來了，沒有偏移表。原因見上面的【停下來】與這一回合的紀錄。
      "\u9019\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u504f\u79fb\u8868\u3002\u539f\u56e0\u898b\u4e0a\u9762\u7684\u3010\u505c\u4e0b\u4f86\u3011\u8207\u9019\u4e00\u56de\u5408\u7684\u7d00\u9304\u3002"
    } else if (length(unjudged) > 0) {
      # 原文：沒有偏移表： | 。每一題的狀態見「固定了哪些題」。
      paste0("\u6c92\u6709\u504f\u79fb\u8868\uff1a", unjudged[1], "\u3002\u6bcf\u4e00\u984c\u7684\u72c0\u614b\u898b\u300c\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c\u300d\u3002")
    } else {
      # 原文：沒有題庫：自由校準，不用檢查偏移。
      "\u6c92\u6709\u984c\u5eab\uff1a\u81ea\u7531\u6821\u6e96\uff0c\u4e0d\u7528\u6aa2\u67e5\u504f\u79fb\u3002"
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
    # 原文：這個量尺重算不出 kits 判斷用的線，所以 max_dist_judging 是空的；
    p("\u9019\u500b\u91cf\u5c3a\u91cd\u7b97\u4e0d\u51fa kits \u5224\u65b7\u7528\u7684\u7dda\uff0c\u6240\u4ee5 max_dist_judging \u662f\u7a7a\u7684\uff1b",
      # 原文：被拿掉的步驟仍然看 steps_dropped。
      "\u88ab\u62ff\u6389\u7684\u6b65\u9a5f\u4ecd\u7136\u770b steps_dropped\u3002")
  } else {
    # 原文：這個量尺有 %d 個判斷的步驟，離判斷用的線的殘差標準差是 %.2f logit，
    p(sprintf(paste0("\u9019\u500b\u91cf\u5c3a\u6709 %d \u500b\u5224\u65b7\u7684\u6b65\u9a5f\uff0c\u96e2\u5224\u65b7\u7528\u7684\u7dda\u7684\u6b98\u5dee\u6a19\u6e96\u5dee\u662f %.2f logit\uff0c",
                     # 原文：界線是 %s × %.2f = %.2f logit。
                     "\u754c\u7dda\u662f %s \u00d7 %.2f = %.2f logit\u3002"), jd$n, jd$sd, cut, jd$sd, jd$limit))
  }
  shiny::tagList(
    # 原文：有兩條線。判斷用的線：用這個量尺所有判斷的步驟畫的線，離它超過界線
    p("\u6709\u5169\u689d\u7dda\u3002\u5224\u65b7\u7528\u7684\u7dda\uff1a\u7528\u9019\u500b\u91cf\u5c3a\u6240\u6709\u5224\u65b7\u7684\u6b65\u9a5f\u756b\u7684\u7dda\uff0c\u96e2\u5b83\u8d85\u904e\u754c\u7dda",
      # 原文：（%s 個殘差標準差）的步驟被拿掉，那一題就是偏移。
      sprintf("\uff08%s \u500b\u6b98\u5dee\u6a19\u6e96\u5dee\uff09\u7684\u6b65\u9a5f\u88ab\u62ff\u6389\uff0c\u90a3\u4e00\u984c\u5c31\u662f\u504f\u79fb\u3002", cut),
      # 原文：最後的線：拿掉偏移的步驟以後重新畫的線，用來把這一回合的門檻換到題庫的量尺（偏移把關圖的實線）。
      "\u6700\u5f8c\u7684\u7dda\uff1a\u62ff\u6389\u504f\u79fb\u7684\u6b65\u9a5f\u4ee5\u5f8c\u91cd\u65b0\u756b\u7684\u7dda\uff0c\u7528\u4f86\u628a\u9019\u4e00\u56de\u5408\u7684\u9580\u6abb\u63db\u5230\u984c\u5eab\u7684\u91cf\u5c3a\uff08\u504f\u79fb\u628a\u95dc\u5716\u7684\u5be6\u7dda\uff09\u3002"),
    limit,
    # 原文：steps：這一題和題庫比較的步驟數；steps_judged：其中上下的人夠、有判斷的步驟數；
    p("steps\uff1a\u9019\u4e00\u984c\u548c\u984c\u5eab\u6bd4\u8f03\u7684\u6b65\u9a5f\u6578\uff1bsteps_judged\uff1a\u5176\u4e2d\u4e0a\u4e0b\u7684\u4eba\u5920\u3001\u6709\u5224\u65b7\u7684\u6b65\u9a5f\u6578\uff1b",
      # 原文：steps_dropped：被拿掉的步驟（分數）。
      "steps_dropped\uff1a\u88ab\u62ff\u6389\u7684\u6b65\u9a5f\uff08\u5206\u6578\uff09\u3002",
      # 原文：max_dist_judging：判斷的步驟離判斷用的線最遠的距離（logit），這是判斷用的數字：
      "max_dist_judging\uff1a\u5224\u65b7\u7684\u6b65\u9a5f\u96e2\u5224\u65b7\u7528\u7684\u7dda\u6700\u9060\u7684\u8ddd\u96e2\uff08logit\uff09\uff0c\u9019\u662f\u5224\u65b7\u7528\u7684\u6578\u5b57\uff1a",
      # 原文：偏移的題大於界線，其他判斷過的題不大於界線。
      "\u504f\u79fb\u7684\u984c\u5927\u65bc\u754c\u7dda\uff0c\u5176\u4ed6\u5224\u65b7\u904e\u7684\u984c\u4e0d\u5927\u65bc\u754c\u7dda\u3002",
      # 原文：max_dist_final（kits 裡叫 max_abs_resid）：所有步驟（包括沒有判斷的）離最後的線最遠的距離，只供參考。
      "max_dist_final\uff08kits \u88e1\u53eb max_abs_resid\uff09\uff1a\u6240\u6709\u6b65\u9a5f\uff08\u5305\u62ec\u6c92\u6709\u5224\u65b7\u7684\uff09\u96e2\u6700\u5f8c\u7684\u7dda\u6700\u9060\u7684\u8ddd\u96e2\uff0c\u53ea\u4f9b\u53c3\u8003\u3002",
      # 原文：沒有判斷的步驟人很少、門檻不準，常常離線很遠但不算偏移；而偏移的步驟被拿掉以後，最後的線會稍微移動。
      "\u6c92\u6709\u5224\u65b7\u7684\u6b65\u9a5f\u4eba\u5f88\u5c11\u3001\u9580\u6abb\u4e0d\u6e96\uff0c\u5e38\u5e38\u96e2\u7dda\u5f88\u9060\u4f46\u4e0d\u7b97\u504f\u79fb\uff1b\u800c\u504f\u79fb\u7684\u6b65\u9a5f\u88ab\u62ff\u6389\u4ee5\u5f8c\uff0c\u6700\u5f8c\u7684\u7dda\u6703\u7a0d\u5fae\u79fb\u52d5\u3002",
      # 原文：所以 max_dist_final 不能拿來和界線比，沒有偏移的題也可能比偏移的題大。
      "\u6240\u4ee5 max_dist_final \u4e0d\u80fd\u62ff\u4f86\u548c\u754c\u7dda\u6bd4\uff0c\u6c92\u6709\u504f\u79fb\u7684\u984c\u4e5f\u53ef\u80fd\u6bd4\u504f\u79fb\u7684\u984c\u5927\u3002")
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
  # 原文：固定
  se <- \(x) dplyr::case_when(is.na(x) ~ "", x == 0 ~ "\u56fa\u5b9a",
                               .default = formatC(x, format = "f", digits = 2))
  co$difficulty_se <- se(co$difficulty_se)
  co$threshold_se <- se(co$threshold_se)
  co
}

# 偏移把關圖下面的一句話
ITEMS_DRIFT_HINT <- paste0(
  # 原文：這張圖用的是固定之前的試算：先不固定任何題，只用這一波的資料估一次，和題庫比。
  "\u9019\u5f35\u5716\u7528\u7684\u662f\u56fa\u5b9a\u4e4b\u524d\u7684\u8a66\u7b97\uff1a\u5148\u4e0d\u56fa\u5b9a\u4efb\u4f55\u984c\uff0c\u53ea\u7528\u9019\u4e00\u6ce2\u7684\u8cc7\u6599\u4f30\u4e00\u6b21\uff0c\u548c\u984c\u5eab\u6bd4\u3002",
  # 原文：固定之後，固定的題門檻就等於題庫的值（見「固定了哪些題」的核對）。
  "\u56fa\u5b9a\u4e4b\u5f8c\uff0c\u56fa\u5b9a\u7684\u984c\u9580\u6abb\u5c31\u7b49\u65bc\u984c\u5eab\u7684\u503c\uff08\u898b\u300c\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c\u300d\u7684\u6838\u5c0d\uff09\u3002",
  # 原文：虛線是判斷用的線，灰色帶子是它上下的容許範圍（圖例寫幾個標準差）。
  "\u865b\u7dda\u662f\u5224\u65b7\u7528\u7684\u7dda\uff0c\u7070\u8272\u5e36\u5b50\u662f\u5b83\u4e0a\u4e0b\u7684\u5bb9\u8a31\u7bc4\u570d\uff08\u5716\u4f8b\u5beb\u5e7e\u500b\u6a19\u6e96\u5dee\uff09\u3002",
  # 原文：落在帶子裡的步驟，這一波的作答和題庫一致，所以可以放心固定。
  "\u843d\u5728\u5e36\u5b50\u88e1\u7684\u6b65\u9a5f\uff0c\u9019\u4e00\u6ce2\u7684\u4f5c\u7b54\u548c\u984c\u5eab\u4e00\u81f4\uff0c\u6240\u4ee5\u53ef\u4ee5\u653e\u5fc3\u56fa\u5b9a\u3002",
  # 原文：帶子外的步驟偏移了，那一題換成新版本；同一題的其他步驟也一起換，所以也標成橘色。
  "\u5e36\u5b50\u5916\u7684\u6b65\u9a5f\u504f\u79fb\u4e86\uff0c\u90a3\u4e00\u984c\u63db\u6210\u65b0\u7248\u672c\uff1b\u540c\u4e00\u984c\u7684\u5176\u4ed6\u6b65\u9a5f\u4e5f\u4e00\u8d77\u63db\uff0c\u6240\u4ee5\u4e5f\u6a19\u6210\u6a58\u8272\u3002",
  # 原文：實線是拿掉偏移的步驟之後，重新畫的線。
  "\u5be6\u7dda\u662f\u62ff\u6389\u504f\u79fb\u7684\u6b65\u9a5f\u4e4b\u5f8c\uff0c\u91cd\u65b0\u756b\u7684\u7dda\u3002"
)

#' 圖的說明（每種圖一兩句）
#' @noRd
items_plot_help <- function(type) {
  switch(
    type,
    # 原文：每一題每個分數的機率，隨能力值（theta）變化。
    category = paste("\u6bcf\u4e00\u984c\u6bcf\u500b\u5206\u6578\u7684\u6a5f\u7387\uff0c\u96a8\u80fd\u529b\u503c\uff08theta\uff09\u8b8a\u5316\u3002",
                     # 原文：圖例的 Score 0、Score 1… 是 0 分、1 分…。
                     "\u5716\u4f8b\u7684 Score 0\u3001Score 1\u2026 \u662f 0 \u5206\u30011 \u5206\u2026\u3002"),
    # 原文：左邊是受試者能力值（ability）的分布，右邊是各題步驟的 Thurstonian 門檻（threshold）。
    wright = paste("\u5de6\u908a\u662f\u53d7\u8a66\u8005\u80fd\u529b\u503c\uff08ability\uff09\u7684\u5206\u5e03\uff0c\u53f3\u908a\u662f\u5404\u984c\u6b65\u9a5f\u7684 Thurstonian \u9580\u6abb\uff08threshold\uff09\u3002",
                   # 原文：兩邊用同一個縱軸，要對得上。
                   "\u5169\u908a\u7528\u540c\u4e00\u500b\u7e31\u8ef8\uff0c\u8981\u5c0d\u5f97\u4e0a\u3002"),
    # 原文：每一題的 infit（圓點）與 outfit（方塊）；灰色帶是 0.6–1.4。
    fit = paste("\u6bcf\u4e00\u984c\u7684 infit\uff08\u5713\u9ede\uff09\u8207 outfit\uff08\u65b9\u584a\uff09\uff1b\u7070\u8272\u5e36\u662f 0.6\u20131.4\u3002",
                # 原文：點的顏色只看它自己（infit 或 outfit）在不在帶子裡。
                "\u9ede\u7684\u984f\u8272\u53ea\u770b\u5b83\u81ea\u5df1\uff08infit \u6216 outfit\uff09\u5728\u4e0d\u5728\u5e36\u5b50\u88e1\u3002"),
    # 原文：點是各波次的孩子實際通過（得 1 分以上）的百分比，虛線是模型預期的通過百分比。
    observed = paste("\u9ede\u662f\u5404\u6ce2\u6b21\u7684\u5b69\u5b50\u5be6\u969b\u901a\u904e\uff08\u5f97 1 \u5206\u4ee5\u4e0a\uff09\u7684\u767e\u5206\u6bd4\uff0c\u865b\u7dda\u662f\u6a21\u578b\u9810\u671f\u7684\u901a\u904e\u767e\u5206\u6bd4\u3002",
                     # 原文：兩者要接近。
                     "\u5169\u8005\u8981\u63a5\u8fd1\u3002"),
    # 原文：點的顏色是「固定了哪些題」的狀態。
    drift = paste(ITEMS_DRIFT_HINT, "\u9ede\u7684\u984f\u8272\u662f\u300c\u56fa\u5b9a\u4e86\u54ea\u4e9b\u984c\u300d\u7684\u72c0\u614b\u3002",
                  # 原文：圖例：Fixed (bank value) 是固定在題庫的值，
                  "\u5716\u4f8b\uff1aFixed (bank value) \u662f\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c\uff0c",
                  # 原文：Drifted -> new version 是偏移、換成新版本。
                  "Drifted -> new version \u662f\u504f\u79fb\u3001\u63db\u6210\u65b0\u7248\u672c\u3002")
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
  # 原文：（共 %d 題）
  total <- sprintf("\uff08\u5171 %d \u984c\uff09", length(ids))
  if (is.null(r)) {
    # 原文：這張圖畫了 %d 題%s：%s。 | 、
    return(sprintf("\u9019\u5f35\u5716\u756b\u4e86 %d \u984c%s\uff1a%s\u3002", length(shown), total, paste(shown, collapse = "\u3001")))
  }
  # 原文：第 %d 題 | 第 %d–%d 題
  which <- if (r[1] == r[2]) sprintf("\u7b2c %d \u984c", r[1]) else sprintf("\u7b2c %d\u2013%d \u984c", r[1], r[2])
  # 原文：這張圖畫了 | 。
  paste0("\u9019\u5f35\u5716\u756b\u4e86", which, total, "\u3002")
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
    # 原文：第 %d 題 | 第 %d–%d 題
    if (at[1] == at[2]) sprintf("\u7b2c %d \u984c", at[1]) else sprintf("\u7b2c %d\u2013%d \u984c", at[1], at[2])
  })
  now <- which(purrr::map_lgl(seq_len(n_page), \(p) identical(items_page_ids(ids, p), shown)))
  shiny::tags$div(
    class = "kit-plot-items",
    if (n_page > 1) {
      # 原文：看哪幾題
      shiny::radioButtons(ns("plot_page"), "\u770b\u54ea\u5e7e\u984c", stats::setNames(seq_len(n_page), pages),
                          selected = if (length(now) == 1) now else character(), inline = TRUE)
    },
    # 原文：或自己選題（可以選好幾題）
    shiny::selectizeInput(ns("plot_items"), "\u6216\u81ea\u5df1\u9078\u984c\uff08\u53ef\u4ee5\u9078\u597d\u5e7e\u984c\uff09", ids, shown,
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
    # 原文：這個量尺停下來了，沒有每一題的狀態。
    return(shiny::tags$p(class = "kit-hint", "\u9019\u500b\u91cf\u5c3a\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u6bcf\u4e00\u984c\u7684\u72c0\u614b\u3002"))
  }
  txt <- kitsui_status_text(tab, check)
  calm <- tab$kind %in% c(KITSUI_STATUS_FIXED, "free")
  top <- tab[!calm, ]
  rest <- tab[calm, ]
  # 原文：自由校準 | 固定
  what <- if (any(rest$kind == "free")) "\u81ea\u7531\u6821\u6e96" else "\u56fa\u5b9a"
  verify <- if (is.null(txt$verify)) {
    NULL
  } else if (txt$ok) {
    # 原文：核對：
    kitDesign::kit_callout(shiny::tags$strong("\u6838\u5c0d\uff1a"), txt$verify)
  } else {
    # 原文：固定的步驟和題庫不同：
    kitDesign::kit_callout(shiny::tags$strong("\u56fa\u5b9a\u7684\u6b65\u9a5f\u548c\u984c\u5eab\u4e0d\u540c\uff1a"), txt$verify,
                           # 原文：這些步驟應該沿用題庫的值。請把這個量尺與下面的步驟告訴維護 kitsUI 的人。
                           "\u9019\u4e9b\u6b65\u9a5f\u61c9\u8a72\u6cbf\u7528\u984c\u5eab\u7684\u503c\u3002\u8acb\u628a\u9019\u500b\u91cf\u5c3a\u8207\u4e0b\u9762\u7684\u6b65\u9a5f\u544a\u8a34\u7dad\u8b77 kitsUI \u7684\u4eba\u3002",
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
      # 原文：沒有偏移、新題或拿掉的題。
      shiny::tags$p(class = "kit-hint", "\u6c92\u6709\u504f\u79fb\u3001\u65b0\u984c\u6216\u62ff\u6389\u7684\u984c\u3002")
    },
    if (nrow(rest) > 0) {
      shiny::tags$details(
        # 原文：看全部%s的題（%d 題）
        shiny::tags$summary(sprintf("\u770b\u5168\u90e8%s\u7684\u984c\uff08%d \u984c\uff09", what, nrow(rest))),
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
