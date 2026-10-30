# ④ 受試者評分：這一回合的分數（kits_subj() -> 內部連結 -> 外部連結）、分數對照表、新的固定檔
# 早先各回合的分數不重算：每一回合的分數表要在那一回合存下來

# 分數表一頁幾列
SCORE_PAGE_ROWS <- 20L

# 新固定檔的下載鈕
SCORE_FIXED_LABEL <- "下載新的固定檔（zip）"

#' @noRd
mod_score_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    score_guide(),
    shiny::uiOutput(ns("source")),
    shiny::uiOutput(ns("flags")),
    shiny::actionButton(ns("run"), "計算這一回合的分數", class = "btn-primary"),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("done")),
    shiny::uiOutput(ns("result"))
  )
}

#' 這一頁的說明
#' @noRd
score_guide <- function() {
  mod_guide(
    goal = "算這一回合每個列次的分數。",
    steps = c(
      "按「計算這一回合的分數」。",
      paste0("看每個量尺換到哪一把尺：校準的量尺 → 內部連結（參照世代以外的世代才有）→ ",
             "外部連結（到 D；參照世代有 D 分數的錨題時才有）。"),
      "下載這一回合的分數表。早先各回合的分數不會重算，所以請現在就存好。",
      "下載新的固定檔（zip），下一回合要用。",
      "要接著加新的波次，按「用這一回合的固定檔開始下一回合」。"
    ),
    output = paste0(
      "這一回合的分數表、分數對照表、題目表，與新的固定檔。",
      "還沒有外部連結的量尺，分數留在自己的量尺上。",
      "外部連結不適用時（沒有 D 分數的錨題），最後的分數是參照世代量尺上的 ref_score。"
    ),
    note = paste0(
      "能力值（ability，kits 裡叫 theta）在校準的量尺上；換到 D 後是量尺分數（d_score）。",
      "ability_se、d_score_se 是它們的標準誤。",
      "se_link 是連結的誤差，se_total 是兩者合起來。"
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
    output$source <- shiny::renderUI(if (is.null(items())) ui_go_items(ns))
    output$flags <- shiny::renderUI(score_flags_ui(kitsui_round_flags(items())))
    shiny::observeEvent(input$go_items, goto("items"))

    # 題目校準變了：舊的分數不算數，要重算
    shiny::observeEvent(items(), {
      own(NULL)
      if (!is.null(result())) {
        result(NULL)
        status(ui_callout("題目校準有變動。請按「計算這一回合的分數」重新計算。"))
      }
    }, ignoreInit = TRUE)
    # 只有連結變了：分數不重算，只用新的連結重新換算（幾乎不花時間）
    shiny::observeEvent(fixed(), {
      if (is.null(result()) || is.null(own())) return()
      if (identical(result()$links_rows, fixed()$links)) return()
      out <- tryCatch(kitsui_score_convert(own(), fixed(), link_objs(), label()),
                      error = function(e) e)
      if (inherits(out, "error")) {
        result(NULL)
        status(ui_callout("連結有變動。請按「計算這一回合的分數」重新計算。"))
        return()
      }
      result(c(out, list(links_rows = fixed()$links)))
      status(ui_callout("連結有變動，分數已用新的連結重新換算（沒有重算分數）。"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      result(NULL)
      round <- items()
      if (is.null(round)) {
        status(ui_callout("還沒有這一回合的題目校準。", warning = TRUE))
        return()
      }
      out <- shiny::withProgress(message = "正在計算分數…", value = 0, {
        tryCatch({
          scores <- own() %||% kitsui_score_own(round, persons(), score_progress)
          own(scores)
          shiny::setProgress(1, detail = "換算到最後的量尺…")
          kitsui_score_convert(scores, fixed(), link_objs(), label())
        }, error = function(e) e)
      })
      if (inherits(out, "error")) {
        status(ui_callout("分數沒有算出來：", conditionMessage(out), warning = TRUE))
        return()
      }
      result(c(out, list(links_rows = fixed()$links)))
      status(ui_callout("完成！共 ", nrow(out$scores), " 列（每個量尺的每個列次一列）。"))
    })
    output$status <- shiny::renderUI(status())

    output$done <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      next_id <- if (!is.null(next_round)) ns("next_round")
      score_done_ui(items(), ns("scores_dl"), ns("fixed_dl"), next_id, label())
    })
    output$result <- shiny::renderUI({
      if (is.null(result())) return(NULL)
      # 有題不是 1 時通過步驟的區塊打開，不把設定藏起來
      shown <- shiny::isolate(tryCatch(shown_steps(), error = \(e) NULL))
      score_result_ui(ns, names(result()$subj), shown, fixed()$version$reference, ref_final())
    })

    # 1. 分數表 ----
    output$routes <- kit_table({
      kitsui_score_routes(fixed(), shiny::req(result())$subj, items()$plan)
    })
    # 外部連結不適用時的參照世代（分數表是 ref_score，不是 d_score）；適用時 NULL
    ref_final <- shiny::reactive(kitsui_ref_final(shiny::req(fixed())))
    output$columns <- shiny::renderUI(score_columns_ui(shiny::req(result())$scores, ref_final()))
    shown <- shiny::reactive(kitsui_score_search(shiny::req(result())$scores, input$search))
    pages <- shiny::reactive(max(1, ceiling(nrow(shown()) / SCORE_PAGE_ROWS)))
    page <- shiny::reactive(score_page(input$page, pages()))
    output$page_info <- shiny::renderText({
      sprintf("符合 %d 列，共 %d 頁；目前第 %d 頁。", nrow(shown()), pages(), page())
    })
    output$scores <- kit_table({
      dplyr::slice(shown(), (page() - 1) * SCORE_PAGE_ROWS + seq_len(SCORE_PAGE_ROWS))
    })
    output$scores_dl_ui <- render_download_csv(
      session, "scores_dl", "下載這一回合的分數表（CSV）", \() result()$scores,
      \() paste0("scores_round_", label())
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
          message = "正在做分數對照表…", detail = s, value = 0.5,
          kitsui_raw_table(res$own[[s]], path, internal)
        )
      }
      raw_memo[[s]]
    })
    output$subj_table <- kit_table(kitsui_raw_table_shown(subj_table(), input$show_empty))
    output$subj_table_dl_ui <- render_download_csv(
      session, "subj_table_dl", "下載對照表（CSV）", subj_table,
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
    known <- shiny::reactive(kitsui_round_item_steps(shiny::req(items())$items, fixed()$versions))
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
        return(say("pick", steps_error_ui(chk$errors, "選好再按「套用到選的題」。")))
      }
      set_steps(kitsui_steps_merge(steps(), chk$steps))
      say("pick", ui_callout(steps_applied_text(chk$steps)))
    })
    shiny::observeEvent(input$steps_file, {
      f <- input$steps_file
      tab <- tryCatch(kitsui_read_steps(f$datapath), error = \(e) e)
      chk <- if (inherits(tab, "error")) list(errors = conditionMessage(tab)) else {
        kitsui_steps_check(tab, known())
      }
      if (length(chk$errors) > 0) {
        say("file", steps_error_ui(chk$errors, "請改好再上傳。"))
        return()
      }
      set_steps(kitsui_steps_merge(steps(), chk$steps))
      say("file", shiny::tagList(
        ui_callout(paste0("已套用 ", f$name, "：共 ", length(chk$steps), " 題。"),
                   "檔案裡沒列的題不變。"),
        ui_warn_list("請注意", chk$warnings)
      ))
    })
    # 表格每一列的「改回 1」（按鈕把題號送到 steps_row_reset）
    shiny::observeEvent(input$steps_row_reset, {
      id <- input$steps_row_reset
      set_steps(kitsui_steps_merge(steps(), stats::setNames(1L, id)))
      say("pick", ui_callout(paste0(id, " 改回 1 了。")))
    })
    shiny::observeEvent(input$steps_reset, {
      set_steps(kitsui_steps_vec(NULL))
      say("pick", ui_callout("已經全部改回 1。"))
    })
    output$steps_template_ui <- render_download_csv(
      session, "steps_template", "下載範本（CSV）",
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
      session, "item_table_dl", "下載題目表（CSV）", item_table,
      \() paste0("item_table_", input$item_scale %||% "scale")
    )
    output$item_table_scale <- shiny::renderUI({
      tab <- item_table()
      last <- utils::tail(intersect(KITSUI_ITEM_THRESHOLDS, names(tab)), 1)
      shiny::tags$p(class = "kit-hint", paste0("題目依 ", last, " 由小到大排列。"))
    })
    output$item_table_note <- shiny::renderUI({
      item_table_note_ui(item_table(), fixed()$version$reference)
    })

    # 檢查、R 程式 ----
    output$checks <- kit_table({
      tab <- tryCatch(item_obj(), error = \(e) NULL)
      more <- if (inherits(tab, "kits_item_table")) list(item_table = tab)
      checks_or_ok(do.call(check_table, c(shiny::req(result())$subj, more))) |>
        kitsui_check_explain()
    })
    output$code <- shiny::renderText({
      s <- shiny::req(input$subj_scale)
      shiny::req(items(), fixed())
      step <- kitsui_item_step_arg(items()$items[[s]], steps(), fixed()$versions)
      score_code(fixed(), s, kitsui_round_cohorts(items(), s), kitsui_fixed_name(label()), step)
    })

    # 新的固定檔 ----
    # 這一回合的通過步驟寫進固定檔（pass_steps）；沒有改時固定檔原樣
    fixed_out <- shiny::reactive(kitsui_fixed_pass_steps(fixed(), steps(), label()))
    # 下一回合：把新的固定檔（含這一回合的連結）交給 ①，不用下載
    shiny::observeEvent(input$next_round, {
      if (!isTRUE(items()$ok) || is.null(next_round)) return()
      next_round(fixed_out(), label())
    })
    # 檔名跟著這一回合的名稱；按下載時才寫出當下的固定檔
    output$fixed_dl_ui <- shiny::renderUI(kit_download_button(
      session, "fixed_dl", SCORE_FIXED_LABEL, kitsui_fixed_name(label()),
      function(file) {
        # 有量尺停下來時不寫出（kitsui_write_fixed() 也會擋）
        if (!isTRUE(items()$ok)) kit_abort("這一回合有量尺停下來，不產生新的固定檔。")
        kitsui_write_fixed(fixed_out(), file)
      },
      class = "btn-sm btn-primary"
    ))

    result
  })
}

# 每個量尺開始算分數前更新進度條（kitsui_score_own() 的 progress）
score_progress <- function(i, n, scale) {
  shiny::setProgress((i - 1) / n, detail = sprintf("%s（第 %d 個，共 %d 個量尺）", scale, i, n))
}

#' ④ 的 R 程式：從新的固定檔重建換算路線上的連結，再評分
#' @param fixed_file 新固定檔的檔名（④ 下載的 zip）
#' @param step 題目表的通過步驟（kitsui_item_step_arg()）
#' @noRd
score_code <- function(fixed, scale, cohorts, fixed_file, step = 1) {
  path <- kitsui_link_path(fixed, scale)
  head <- paste0("# 接在 ② 題目校準（", scale, "）的程式後面（X、item）")
  links <- purrr::map_chr(path, \(n) kitsui_code_link(fixed, n, fixed_file))
  if (length(path) > 0) {
    head <- c(head, paste0("# 連結從這一回合下載的新固定檔（", fixed_file, "）重建"))
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
    shiny::tags$li(shiny::tags$span(class = "kit-warn-text", paste0(s, "（flag）：")), d)
  })
  ui_callout(
    shiny::tags$strong("② 題目校準給這些量尺 flag："),
    shiny::tags$ul(items),
    shiny::tags$p(
      "flag 表示這個量尺偏移的題比較多，或可以固定的題比較少。",
      "這一回合照常校準，分數也照常算。",
      "但這個量尺和題庫的尺可能沒有對得很準。",
      "請到「② 題目校準」看是哪些題，再決定要不要用這一回合的分數。"
    ),
    warning = TRUE
  )
}

#' 完成：下載這一回合的分數表與新的固定檔（一個醒目的區塊，在結果的正下方）
#' @noRd
score_done_ui <- function(round, scores_id, fixed_id, next_id, label) {
  shiny::tags$div(
    class = "kit-summary kit-done",
    shiny::h4("這一回合做完了：請現在就存兩個檔案"),
    shiny::tags$p(class = "kit-warn-text",
                  "早先各回合的分數之後不會重算。所以這一回合的分數表要現在下載保存。"),
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(scores_id),
      shiny::tags$small(class = "kit-hint", "下載的是全部的列，不只畫面上這一頁。")
    ),
    score_fixed_ui(round, fixed_id, next_id, label)
  )
}

#' 新的固定檔：這一回合沒問題時給下載鈕；有量尺停下來時鈕不能按，並用紅字說明
#' @noRd
score_fixed_ui <- function(round, button_id, next_id = NULL, label = NULL) {
  text <- SCORE_FIXED_LABEL
  if (is.null(round)) return(shiny::tags$p("做完題目校準後，這裡可以下載新的固定檔。"))
  if (!isTRUE(round$ok)) {
    return(shiny::tagList(
      shiny::tags$p(
        class = "text-danger fw-bold",
        "這一回合有量尺停下來（", paste(round$stopped, collapse = "、"), "），不能下載新的固定檔。"
      ),
      shiny::tags$p(class = "text-danger",
                    "請到「② 題目校準」看停下來的原因。", "處理後重跑這一回合。"),
      shiny::tags$button(class = "btn btn-sm btn-outline-secondary", disabled = NA, text)
    ))
  }
  file <- if (length(label) > 0) paste0("（", kitsui_fixed_name(label), "）")
  shiny::tagList(
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(button_id),
      if (!is.null(next_id)) {
        shiny::actionButton(next_id, "用這一回合的固定檔開始下一回合",
                            class = "btn-sm btn-outline-primary ms-2")
      }
    ),
    shiny::tags$ul(
      shiny::tags$li("新的固定檔", file, "存著題庫、題目版本與連結。"),
      shiny::tags$li("下一回合要上傳這個 zip，才能接著用同一個題庫與連結。"),
      shiny::tags$li("在這次連線裡接著做，可以直接按「用這一回合的固定檔開始下一回合」，不用下載。")
    )
  )
}

#' 分數表每一欄的一句說明
#' @param ref NULL，或外部連結不適用時的參照世代（kitsui_ref_final()）
#' @noRd
score_columns_ui <- function(scores, ref = NULL) {
  cols <- kitsui_score_columns(scores, ref)
  shiny::tags$details(
    open = NA,
    shiny::tags$summary("每一欄是什麼"),
    shiny::tags$ul(
      class = "kit-hint",
      purrr::map2(cols$column, cols$meaning, \(c, m) shiny::tags$li(shiny::tags$code(c), "：", m))
    )
  )
}

#' 題目表下面：拿掉了哪些沒有內容的欄
#' @param ref 參照世代
#' @noRd
item_table_note_ui <- function(tab, ref = KITSUI_REF_COHORT) {
  drop <- attr(tab, "dropped")
  li <- \(col, ...) if (col %in% names(tab)) shiny::tags$li(shiny::tags$code(col), ...)
  shiny::tags$details(
    open = NA,
    shiny::tags$summary("每一欄是什麼"),
    shiny::tags$ul(
      class = "kit-hint",
      shiny::tags$li("三種門檻都是通過步驟的 Thurstonian 門檻。",
                     "能力到這裡時，得分達到通過步驟的機會是一半。",
                     "孩子的分數高於同一把尺上的門檻，通過的機會就超過一半。"),
      li("step", "：通過步驟。得分至少這麼多分算通過。",
         "門檻與 age50 都照它算。只有在有題不是 1 時才列出。"),
      li("ability_threshold", "：在校準的量尺上，和 ability 同一把尺（kits 裡這一欄叫 D）。"),
      li("ability_linked_threshold", paste0("：經過內部連結，在參照世代（", ref, "）的量尺上，"),
         "和 ability_linked 同一把尺。參照世代自己的量尺沒有這一欄。"),
      li("d_threshold", "：再經過外部連結，在 D 上，和 d_score 同一把尺。"),
      li("age50", "：這一回合一半的孩子通過（得分達到通過步驟）的月齡。",
         paste0("age50 要從這一回合的資料算：至少兩個波次、通過比例在波次之間跨過 50%，",
                "而且落在資料的月齡範圍內；",
                "只有一個波次，或每一波都已經超過（或都還沒到）50% 時是空的。")),
      if (length(drop) > 0) {
        shiny::tags$li(paste0("每一題都沒有內容的欄不列出：", paste(drop, collapse = "、"), "。"),
                       if ("DIF" %in% drop) "DIF（性別的差異試題功能）要另外做 kits_dif()，kitsUI 沒有做。",
                       if ("stem" %in% drop) "stem 是題目內容，沒有給的時候和 item_id 一樣。")
      }
    )
  )
}

#' 題目表的通過步驟：收合區塊，標題是現在的設定
#'
#' 裡面依序是：一句說明、選題目套用、不是 1 的題（可以改回 1）、用檔案設定（再收一層）。
#' 有題不是 1 時預設打開。
#' @param shown kitsui_steps_shown()；NULL 時當作每題都是 1
#' @noRd
score_steps_ui <- function(ns, shown = NULL) {
  here <- if (!is.null(shown)) shown[!is.na(shown$scale), ]
  title <- if (is.null(shown)) "通過步驟（每題都是 1）" else kitsui_steps_summary(shown)
  shiny::tags$details(
    class = "kit-fold-box",
    open = if (NROW(here) > 0) NA,
    # 標題跟著設定變（renderText 換掉裡面的字）
    shiny::tags$summary(shiny::tags$span(id = ns("steps_summary"), class = "shiny-text-output",
                                         title)),
    shiny::tags$p("得分至少幾分算通過。預設每題都是 1。",
                  "只影響題目表的門檻與 age50；校準、連結、分數都不受影響。"),
    shiny::uiOutput(ns("steps_picker")),
    shiny::uiOutput(ns("steps_msg")),
    shiny::uiOutput(ns("steps_table")),
    shiny::tags$details(
      shiny::tags$summary("用檔案設定（題目很多時）"),
      shiny::fileInput(ns("steps_file"), "上傳設定檔（CSV，兩欄：item_id、step）",
                       width = "26rem", accept = ".csv", buttonLabel = "選擇檔案…",
                       placeholder = "尚未選擇檔案"),
      shiny::tags$div(class = "kit-downloads", ui_download(ns("steps_template"))),
      shiny::tags$small(class = "kit-hint",
                        "範本列出這一回合的每一題與目前的 step。",
                        "改 step 欄後上傳。scale、max_score 兩欄只是參考。"),
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
      shiny::selectizeInput(ns("steps_pick"), "題目", choices = choices, multiple = TRUE,
                            width = "28rem",
                            options = list(placeholder = "選一題或好幾題（可以打字找）")),
      shiny::numericInput(ns("steps_value"), "通過步驟", value = value, min = 1, step = 1,
                          width = "7rem"),
      shiny::actionButton(ns("steps_apply"), "套用到選的題", class = "btn-sm btn-primary")
    ),
    shiny::tags$small(class = "kit-hint",
                      "清單是這一回合每個量尺的全部題目，依量尺分組。",
                      "括號裡是目前的通過步驟與校準裡的最高分數。")
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
      shiny::tags$td(shiny::tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
                                        `data-item` = id, onclick = click, "改回 1"))
    )
  }
  table <- if (nrow(here) == 0) shiny::tags$p("目前每一題都是 1。") else {
    shiny::tags$table(
      class = "table table-sm kit-steps-table",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("item_id"), shiny::tags$th("step"),
                                       shiny::tags$th("最高分數"), shiny::tags$th())),
      shiny::tags$tbody(purrr::pmap(list(here$item_id, here$step, here$max_score), row))
    )
  }
  shiny::tagList(
    if (nrow(here) > 0) shiny::tags$p(class = "mb-1 fw-bold", "不是 1 的題"),
    table,
    if (nrow(away) > 0) {
      shiny::tags$small(class = "kit-hint", paste0(
        "另有 ", nrow(away), " 題這一回合沒有，設定照樣存進固定檔：",
        kitsui_some(paste0(away$item_id, "（", away$step, "）")), "。"
      ))
    },
    if (nrow(shown) > 0) {
      shiny::tags$div(class = "mb-3", shiny::actionButton(ns("steps_reset"), "全部改回 1",
                                                          class = "btn-sm btn-outline-secondary"))
    }
  )
}

#' 套用了：一句話說哪幾題設成多少
#' @param steps 原來的題號 -> step（都一樣）
#' @noRd
steps_applied_text <- function(steps) {
  ids <- kitsui_some(names(steps))
  if (all(steps == 1)) return(paste0("已把 ", length(steps), " 題改回 1：", ids, "。"))
  paste0("已把 ", length(steps), " 題設成 ", steps[[1]], "：", ids, "。")
}

#' 通過步驟的訊息放在哪裡：msg 是 list(where, ui)
#' @noRd
steps_msg_at <- function(msg, where) if (identical(msg$where, where)) msg$ui

#' 通過步驟沒有套用：紅字說明
#' @param todo 下一步怎麼做（「請改好再上傳。」等）
#' @noRd
steps_error_ui <- function(errors, todo) {
  ui_alert("danger", "【通過步驟沒有套用】",
           shiny::tags$ul(lapply(errors, shiny::tags$li)),
           shiny::tags$p(class = "mb-0", "設定沒有改。", todo))
}

#' 算完分數之後的畫面：分數表、分數對照表、題目表、檢查、R 程式
#' @param steps kitsui_steps_shown()：有題不是 1 時通過步驟的區塊打開
#' @param ref 參照世代；ref_final 是外部連結不適用時的參照世代（kitsui_ref_final()），適用時 NULL
#' @noRd
score_result_ui <- function(ns, scales, steps = NULL, ref = KITSUI_REF_COHORT, ref_final = NULL) {
  shiny::tagList(
    shiny::h4("1. 分數表"),
    shiny::tags$small(
      class = "kit-hint",
      if (is.null(ref_final)) {
        "每個量尺的分數換到哪一把尺上（route：內部連結，再外部連結到 D）。"
      } else {
        "每個量尺的分數換到哪一把尺上（route：內部連結到參照世代）。"
      },
      "bank 是校準用的題庫；data 是哪一個世代、哪些波次的資料。"
    ),
    if (!is.null(ref_final)) {
      ui_callout(paste0(
        "外部連結不適用：參照世代（", ref_final, "）沒有 D 分數的錨題，所以分數表沒有 d_score。",
        "最後的分數是 ref_score，在參照世代（", ref_final, "）的量尺上；",
        "還沒有內部連結的世代，ref_score 是空白，分數留在 ability（自己的量尺）。"
      ))
    },
    shiny::tableOutput(ns("routes")),
    shiny::textInput(ns("search"), "找人：輸入 visit_id 或 subj_id 的一部分",
                     placeholder = "例如 c55df9b"),
    shiny::numericInput(ns("page"), "第幾頁", value = 1, min = 1, step = 1, width = "8rem"),
    shiny::textOutput(ns("page_info")),
    shiny::tags$div(class = "kit-table-scroll", shiny::tableOutput(ns("scores"))),
    shiny::uiOutput(ns("columns")),
    shiny::h4("2. 分數對照表"),
    shiny::tags$small(class = "kit-hint",
                      "每個原始分數對應的能力值與人數（kits_subj_table()）。欄位說明在表的下面。"),
    shiny::selectInput(ns("subj_scale"), "哪一個量尺？", choices = scales),
    shiny::checkboxInput(ns("show_empty"), "顯示沒有人的原始分數", value = FALSE),
    shiny::tags$div(class = "kit-table-scroll kit-table-tall",
                    shiny::tableOutput(ns("subj_table"))),
    shiny::tags$details(
      open = NA,
      shiny::tags$summary("每一欄是什麼"),
      shiny::tags$ul(
        class = "kit-hint",
        shiny::tags$li(shiny::tags$code("ability"), "、", shiny::tags$code("ability_se"),
                       "：校準的量尺上的能力值與標準誤（logit；kits 裡叫 theta、se）。",
                       "和上面分數表的 ability 相同：各世代分開估計，沒有經過連結。"),
        shiny::tags$li(shiny::tags$code("ability_linked"), "、", shiny::tags$code("ability_linked_se"),
                       paste0("：linked 表示經過內部連結，換到參照世代（", ref, "）的量尺上。"),
                       "參照世代自己的量尺沒有這兩欄。"),
        if (is.null(ref_final)) {
          shiny::tagList(
            shiny::tags$li(shiny::tags$code("d_score"), "、", shiny::tags$code("d_score_se"),
                           "：再經過外部連結，D 上的量尺分數與標準誤（kits 裡叫 d）。"),
            shiny::tags$li(shiny::tags$code("se_link"), "、", shiny::tags$code("se_total"),
                           "：d_score 的連結誤差，以及測量誤差與連結誤差合起來。")
          )
        } else {
          shiny::tags$li("外部連結不適用，所以沒有 d_score。")
        },
        shiny::tags$li("作答組合：這個波次最多孩子作答的那一組題目。表裡只算用這個作答組合的孩子。"),
        shiny::tags$li(shiny::tags$code("n"), "：這個原始分數有幾個孩子。",
                       shiny::tags$code("pct"), "：佔這個作答組合的百分比。"),
        shiny::tags$li(shiny::tags$code("cum_pct"), "：累積百分比，原始分數到這一列為止的孩子佔幾 %。"),
        shiny::tags$li(shiny::tags$code("wave_n"), "：這個波次全部的孩子數。"),
        shiny::tags$li(shiny::tags$code("form_pct"), "：這個波次的孩子有幾 % 用這個作答組合。")
      )
    ),
    shiny::tags$div(
      class = "kit-downloads",
      ui_download(ns("subj_table_dl")),
      shiny::tags$small(class = "kit-hint", "下載的是完整的表，包括沒有人的原始分數。")
    ),
    shiny::h4("3. 題目表"),
    shiny::tags$small(
      class = "kit-hint",
      "每一題一列（kits_item_table()）。欄位說明在表的下面。"
    ),
    score_steps_ui(ns, steps),
    shiny::selectInput(ns("item_scale"), "哪一個量尺？", choices = scales),
    shiny::uiOutput(ns("item_table_scale")),
    shiny::tags$div(class = "kit-table-scroll kit-table-tall",
                    shiny::tableOutput(ns("item_table"))),
    shiny::uiOutput(ns("item_table_note")),
    shiny::tags$div(class = "kit-downloads",
                    ui_download(ns("item_table_dl"))),
    shiny::tags$p(
      class = "kit-hint",
      "用常模算的年齡（age_reach 等）要有常模（kits_norms()）；kitsUI 不做常模，所以這裡沒有。"
    ),
    ui_check_panel(ns("checks")),
    ui_code_panel(ns("code"))
  )
}
