# 資料：示範資料、kits 不知道的欄位（domain、cohort）、量尺清單、作答矩陣、這一回合的名稱
# 其他檢查（作答分數、重複列…）交給 kits_resp() 與 kits_check()

# 預設的參照世代（內部連結的目標、外部連結的起點）：資料有 M03 時（KIT）；
# 第一回合在 ① 選（kitsui_ref_pick()），寫進固定檔的 VERSION
KITSUI_REF_COHORT <- "M03"

# 波次（wave_m）的單位：第一回合在 ① 選，寫進固定檔的 VERSION（WaveUnit），之後的回合沿用。
# 之前的固定檔沒有 WaveUnit：當成月（KIT 的 wave_m 是施測月齡）。
# 借題庫的規則（比對同一個月齡）與 age50 只在單位是月時用
# 原文：月（施測月齡） | 年 | 波次序號
KITSUI_WAVE_UNITS <- c("\u6708\uff08\u65bd\u6e2c\u6708\u9f61\uff09" = "months", "\u5e74" = "years", "\u6ce2\u6b21\u5e8f\u865f" = "waves")

#' 固定檔的波次單位："months"、"years" 或 "waves"（沒有寫時是 "months"）
#' @noRd
kitsui_wave_unit <- function(fixed) {
  u <- fixed$version$wave_unit
  if (length(u) != 1 || is.na(u) || !u %in% KITSUI_WAVE_UNITS) "months" else u
}

#' 一個波次給使用者看的寫法：月是「36 個月」，年是「2013 年」，波次序號是「第 3 波」
#' @noRd
kitsui_wave_text <- function(w, unit = "months") {
  w <- format(w, trim = TRUE)
  # 原文： 年 | 第  |  波 |  個月
  switch(unit, years = paste0(w, " \u5e74"), waves = paste0("\u7b2c ", w, " \u6ce2"), paste0(w, " \u500b\u6708"))
}

#' 好幾個波次一起寫：「3、6 個月」「2009、2011 年」「第 1、2 波」
#' @noRd
kitsui_waves_text <- function(w, unit = "months") {
  # 原文：、
  w <- paste(format(w, trim = TRUE), collapse = "\u3001")
  # 原文： 年 | 第  |  波 |  個月
  switch(unit, years = paste0(w, " \u5e74"), waves = paste0("\u7b2c ", w, " \u6ce2"), paste0(w, " \u500b\u6708"))
}

#' 句子裡的「月齡」：月是「月齡」，年是「年」，波次序號是「波次」
#' @noRd
kitsui_wave_word <- function(unit = "months") {
  # 原文：年 | 波次 | 月齡
  switch(unit, years = "\u5e74", waves = "\u6ce2\u6b21", "\u6708\u9f61")
}

#' 圖上 x 軸（英文）的波次：月是 "months"，年是 "years"，波次序號是 "wave number"
#' @noRd
kitsui_wave_unit_en <- function(unit = "months") {
  switch(unit, years = "years", waves = "wave number", "months")
}

#' kits 的圖（plotly）上寫著 "Wave (months)" 的軸，照波次單位改成 "Wave (years)" 等（例如 ① 的作答設計圖）
#' @noRd
kitsui_wave_axis <- function(p, unit = "months") {
  if (unit == "months") return(p)
  new <- paste0("Wave (", kitsui_wave_unit_en(unit), ")")
  for (ax in c("xaxis", "yaxis")) {
    title <- p$x$layout[[ax]]$title %||% p$x$layoutAttrs[[1]][[ax]]$title
    if (is.list(title)) title <- title$text
    if (identical(title, "Wave (months)")) {
      p <- do.call(plotly::layout, c(list(p), stats::setNames(list(list(title = new)), ax)))
    }
  }
  p
}

# 資料一定要有的欄位（資料約定）
KITSUI_REQUIRED <- c("visit_id", "cohort", "wave_m", "item_id", "item_score", "domain")

# 資料約定的欄位：讀檔時的型別（readr 的縮寫：c 文字、d 數字）、一定要有嗎、意思
KITSUI_COLUMNS <- tibble::tribble(
  ~column, ~type, ~meaning,
  # 原文：列次編號：一個孩子的一個波次一個編號。不同世代也不能重複。
  "visit_id", "c", "\u5217\u6b21\u7de8\u865f\uff1a\u4e00\u500b\u5b69\u5b50\u7684\u4e00\u500b\u6ce2\u6b21\u4e00\u500b\u7de8\u865f\u3002\u4e0d\u540c\u4e16\u4ee3\u4e5f\u4e0d\u80fd\u91cd\u8907\u3002",
  # 原文：孩子編號：同一個孩子的各波次相同。
  "subj_id", "c", "\u5b69\u5b50\u7de8\u865f\uff1a\u540c\u4e00\u500b\u5b69\u5b50\u7684\u5404\u6ce2\u6b21\u76f8\u540c\u3002",
  # 原文：世代，例如 M03、M36。參照世代在第一回合選（有 M03 時預設 M03）。
  "cohort", "c", "\u4e16\u4ee3\uff0c\u4f8b\u5982 M03\u3001M36\u3002\u53c3\u7167\u4e16\u4ee3\u5728\u7b2c\u4e00\u56de\u5408\u9078\uff08\u6709 M03 \u6642\u9810\u8a2d M03\uff09\u3002",
  # 原文：性別，例如 boy、girl。
  "sex", "c", "\u6027\u5225\uff0c\u4f8b\u5982 boy\u3001girl\u3002",
  # 原文：波次，例如施測月齡 3、6、12。也可以是年或波次序號（在 ① 選單位）。
  "wave_m", "d", "\u6ce2\u6b21\uff0c\u4f8b\u5982\u65bd\u6e2c\u6708\u9f61 3\u30016\u300112\u3002\u4e5f\u53ef\u4ee5\u662f\u5e74\u6216\u6ce2\u6b21\u5e8f\u865f\uff08\u5728 \u2460 \u9078\u55ae\u4f4d\uff09\u3002",
  # 原文：實際月齡。題目表的通過年齡要用它。
  "age_m", "d", "\u5be6\u969b\u6708\u9f61\u3002\u984c\u76ee\u8868\u7684\u901a\u904e\u5e74\u9f61\u8981\u7528\u5b83\u3002",
  # 原文：題號，例如 lana01。
  "item_id", "c", "\u984c\u865f\uff0c\u4f8b\u5982 lana01\u3002",
  # 原文：得分：0、1、2…。沒有作答的題不用放。
  "item_score", "d", "\u5f97\u5206\uff1a0\u30011\u30012\u2026\u3002\u6c92\u6709\u4f5c\u7b54\u7684\u984c\u4e0d\u7528\u653e\u3002",
  # 原文：領域，例如 lan、motor、cog。外部連結的錨題用 lan、motor、cog 這三個名稱。
  "domain", "c", "\u9818\u57df\uff0c\u4f8b\u5982 lan\u3001motor\u3001cog\u3002\u5916\u90e8\u9023\u7d50\u7684\u9328\u984c\u7528 lan\u3001motor\u3001cog \u9019\u4e09\u500b\u540d\u7a31\u3002"
) |>
  dplyr::mutate(required = .data$column %in% KITSUI_REQUIRED, .after = "type")

# 讀檔時的型別：編號一定讀成文字（不會掉開頭的 0），月齡與分數讀成數字
KITSUI_TYPES <- purrr::set_names(KITSUI_COLUMNS$type, KITSUI_COLUMNS$column)

#' 示範資料：kitm03、kitm36 的作答（長格式）
#'
#' @param cohorts 世代，"M03"、"M36"
#' @param waves 波次（月）；NULL 為全部
#' @param domains 領域，例如 "lan"、"motor"
#' @return 長格式 tibble，欄位同 kits::kitm03$responses
#' @noRd
kitsui_demo <- function(cohorts = c("M03", "M36"), waves = NULL, domains = c("lan", "motor")) {
  dplyr::bind_rows(kits::kitm03$responses, kits::kitm36$responses) |>
    dplyr::filter(
      .data$cohort %in% cohorts, .data$domain %in% domains,
      is.null(waves) | .data$wave_m %in% waves
    )
}

#' 檢查 kits 不知道的欄位：domain、cohort 要有、不可缺漏
#'
#' 作答本身（visit_id、item_id、item_score）由 kits_resp() 檢查。
#' @return 不可見的 df
#' @noRd
kitsui_check_data <- function(df) {
  miss <- setdiff(c("domain", "cohort"), names(df))
  if (length(miss) > 0) {
    # 原文：資料缺少  | 、 |  欄位。請加上後再上傳。
    kitDesign::kit_abort("\u8cc7\u6599\u7f3a\u5c11 ", paste(miss, collapse = "\u3001"), " \u6b04\u4f4d\u3002\u8acb\u52a0\u4e0a\u5f8c\u518d\u4e0a\u50b3\u3002")
  }
  blank <- \(x) anyNA(x) || any(!nzchar(trimws(x)))
  if (blank(df$domain) || blank(df$cohort)) {
    # 原文：domain 或 cohort 有空白。每一列都要填領域與世代。
    kitDesign::kit_abort("domain \u6216 cohort \u6709\u7a7a\u767d\u3002\u6bcf\u4e00\u5217\u90fd\u8981\u586b\u9818\u57df\u8207\u4e16\u4ee3\u3002")
  }
  invisible(df)
}

#' 這一回合的名稱能不能用：只能有英文字母與數字，且不能和之前的回合重複
#'
#' ① 資料頁與 kitsui_round() 共用這一條規則。
#' @return 中文訊息；可以用時長度 0
#' @noRd
kitsui_label_problem <- function(label, rounds = character()) {
  if (!grepl("^[A-Za-z0-9]+$", label)) {
    # 原文：這一回合的名稱只能用英文字母與數字，不能空白。
    return("\u9019\u4e00\u56de\u5408\u7684\u540d\u7a31\u53ea\u80fd\u7528\u82f1\u6587\u5b57\u6bcd\u8207\u6578\u5b57\uff0c\u4e0d\u80fd\u7a7a\u767d\u3002")
  }
  if (label %in% rounds) {
    # 原文：這一回合的名稱「 | 」和固定檔裡之前的回合重複了。請換一個。
    return(paste0("\u9019\u4e00\u56de\u5408\u7684\u540d\u7a31\u300c", label, "\u300d\u548c\u56fa\u5b9a\u6a94\u88e1\u4e4b\u524d\u7684\u56de\u5408\u91cd\u8907\u4e86\u3002\u8acb\u63db\u4e00\u500b\u3002"))
  }
  character()
}

#' 預設的這一回合名稱："R" 加第幾回合（固定檔裡已有 2 回合時是 "R3"）；被用過時往後找沒用過的
#' @param rounds 固定檔裡之前的回合
#' @noRd
kitsui_default_label <- function(rounds = character()) {
  n <- length(rounds) + 1
  while (paste0("R", n) %in% rounds) n <- n + 1
  paste0("R", n)
}

# 量尺的名稱："lan M36" -> 領域 "lan"、世代 "M36"。
# 世代名稱裡的空白，在量尺名稱裡換成不斷行空白（U+00A0，畫面上看起來一樣），所以量尺名稱
# 最後一個空白一定是領域與世代的分界（領域名稱可以有空白）。世代名稱沒有空白時（例如 KIT 的
# M03）量尺名稱和以前一樣，之前的固定檔照樣用
KITSUI_NBSP <- "\u00a0"
scale_key <- function(domain, cohort) {
  paste(domain, gsub(" ", KITSUI_NBSP, cohort, fixed = TRUE))
}
scale_domain <- function(scale) sub(" [^ ]+$", "", scale)
scale_cohort <- function(scale) gsub(KITSUI_NBSP, " ", sub("^.* ", "", scale), fixed = TRUE)
# 用 ", " 接起來的量尺名稱（外部連結的 from_scale）拆回來。領域名稱有 ", " 時，照固定檔裡有的
# 量尺（題庫與 VERSION 的 Scales）接回去
kitsui_split_scales <- function(x, fixed = NULL) {
  parts <- strsplit(x, ", ", fixed = TRUE)[[1]]
  known <- unique(c(fixed$bank$scale, fixed$version$scales))
  if (length(known) == 0 || all(parts %in% known)) return(parts)
  out <- character()
  now <- NULL
  for (p in parts) {
    now <- if (is.null(now)) p else paste(now, p, sep = ", ")
    if (now %in% known) {
      out <- c(out, now)
      now <- NULL
    }
  }
  c(out, now)
}
# 給 R 程式與 csv 用的量尺名稱：不斷行空白換回一般的空白（和 paste(domain, cohort) 一樣）
scale_label <- function(scale) gsub(KITSUI_NBSP, " ", scale, fixed = TRUE)

#' 量尺清單：領域 x 世代，各有幾列次、幾題、哪些波次（參照世代在前）
#' @param ref 參照世代；預設照資料的世代（kitsui_ref_default()）
#' @return tibble：scale、domain、cohort、n_visits、n_items、waves
#' @noRd
kitsui_scales <- function(df, ref = kitsui_ref_default(df$cohort)) {
  df |>
    dplyr::summarise(
      n_visits = dplyr::n_distinct(.data$visit_id), n_items = dplyr::n_distinct(.data$item_id),
      waves = paste(sort(unique(.data$wave_m)), collapse = ", "), .by = c("domain", "cohort")
    ) |>
    dplyr::mutate(scale = scale_key(.data$domain, .data$cohort), .before = 1) |>
    dplyr::arrange(.data$cohort != ref, .data$scale)
}

#' 一個量尺的作答（kits_resp()）
#'
#' @param scale 資料的量尺（領域 × 世代）；借用另一世代題庫時可以有兩個世代，例如 c("lan M03", "lan M36")
#' @noRd
kitsui_resp <- function(df, scale) {
  part <- dplyr::filter(df, scale_key(.data$domain, .data$cohort) %in% scale)
  if (length(scale) > 1) kitsui_check_ids(part)
  kits::kits_resp(part)
}

#' 作答設計圖用：通過步驟不是 1 的題，分數換成通過（1）或沒通過（0）
#'
#' plot() 畫的通過比例是「得分 1 以上」；換過以後就是「得分達到通過步驟以上」。
#' 只給圖用，不改校準用的作答。
#' @param steps 題號 -> step（kitsui_steps_vec()）
#' @noRd
kitsui_resp_pass <- function(resp, steps) {
  hit <- intersect(attr(resp, "items"), names(steps)[steps != 1])
  for (it in hit) resp[[it]] <- as.integer(resp[[it]] >= steps[[it]])
  resp
}

# 兩個世代放進同一個矩陣時，visit_id 不能重複
kitsui_check_ids <- function(df) {
  ids <- dplyr::distinct(df, .data$visit_id, .data$cohort)
  dup <- ids$visit_id[duplicated(ids$visit_id)]
  if (length(dup) > 0) {
    # 原文：兩個世代有相同的 visit_id（例如  | ），不能放進同一個作答矩陣。
    kitDesign::kit_abort("\u5169\u500b\u4e16\u4ee3\u6709\u76f8\u540c\u7684 visit_id\uff08\u4f8b\u5982 ", dup[1], "\uff09\uff0c\u4e0d\u80fd\u653e\u9032\u540c\u4e00\u500b\u4f5c\u7b54\u77e9\u9663\u3002",
                         # 原文：請讓每個列次的 visit_id 都不一樣，例如前面加上世代。
                         "\u8acb\u8b93\u6bcf\u500b\u5217\u6b21\u7684 visit_id \u90fd\u4e0d\u4e00\u6a23\uff0c\u4f8b\u5982\u524d\u9762\u52a0\u4e0a\u4e16\u4ee3\u3002")
  }
  invisible(df)
}

#' 一個量尺的作答矩陣（列名 visit_id；由 kits_resp() 轉寬）
#' @inheritParams kitsui_resp
#' @noRd
kitsui_matrix <- function(df, scale) {
  resp <- kitsui_resp(df, scale)
  X <- as.matrix(resp[attr(resp, "items")])
  rownames(X) <- resp$visit_id
  X
}

#' 每個列次的資訊（給 kits_subj(persons = )）
#' @noRd
kitsui_persons <- function(df) {
  cols <- intersect(c("visit_id", "subj_id", "wave_m", "age_m", "sex", "cohort"), names(df))
  dplyr::distinct(df, dplyr::pick(dplyr::all_of(cols)))
}

# 這一回合的資料 ----

#' 資料裡有哪些世代 × 波次（給 ① 勾選）：參照世代在前，再依世代名稱、月齡
#' @param ref 參照世代；預設照資料的世代（kitsui_ref_default()）
#' @return tibble：cohort、wave_m、value（勾選框的值，例如 "M03 3"）
#' @noRd
kitsui_pick_choices <- function(data, ref = kitsui_ref_default(data$cohort)) {
  if (!all(c("cohort", "wave_m") %in% names(data))) {
    return(tibble::tibble(cohort = character(), wave_m = numeric(), value = character()))
  }
  dplyr::distinct(data, .data$cohort, .data$wave_m) |>
    dplyr::filter(!is.na(.data$cohort), !is.na(.data$wave_m)) |>
    dplyr::arrange(.data$cohort != ref, .data$cohort, .data$wave_m) |>
    dplyr::mutate(value = paste(.data$cohort, .data$wave_m)) |>
    tibble::as_tibble()
}

#' 勾選的世代 × 波次（"M03 3"）對到資料裡有的組合
#'
#' 只寫月齡（"3"）時是所有世代的這個月齡。
#' @param picks 勾選框的值
#' @param choices kitsui_pick_choices()
#' @return choices 裡勾到的列
#' @noRd
kitsui_parse_picks <- function(picks, choices) {
  picks <- as.character(picks)
  has_cohort <- grepl(" ", picks)
  waves <- as.numeric(sub("^.* ", "", picks))
  hit <- choices$value %in% picks[has_cohort] | choices$wave_m %in% waves[!has_cohort]
  choices[hit, ]
}

#' 這一回合的資料：照勾選的世代 × 波次篩選、檢查欄位與作答（幾百萬列也只要幾秒）
#'
#' 名稱與固定檔不在這裡檢查：改名稱或換固定檔時不用重跑。
#' 全部組合都勾、沒有 wave_m 空白的列、沒有約定以外的欄時不複製資料。
#' @param data 這個來源的全部資料（還沒選）
#' @param picks 勾選的世代 × 波次（"M03 3"）；只寫月齡時是所有世代的這個月齡
#' @param choices kitsui_pick_choices(data)；NULL 時在這裡算
#' @param reference 參照世代：量尺表把它排在前面
#' @return list(df, scales, persons, picks, problems, hint, notes, dropped)：persons 是
#'   kitsui_persons(df)；picks 是勾到的 choices；problems 是紅色的中文訊息；hint 是還沒勾時的
#'   提示；notes 是橘色的提醒（wave_m 空白的列、像遺漏碼的分數）；dropped 是拿掉的約定以外的欄
#' @noRd
kitsui_data_select <- function(data, picks, choices = NULL, reference = KITSUI_REF_COHORT) {
  miss <- setdiff(KITSUI_REQUIRED, names(data))
  if (length(miss) > 0) {
    # 原文：資料缺少這些欄位： | 、
    return(list(problems = paste0("\u8cc7\u6599\u7f3a\u5c11\u9019\u4e9b\u6b04\u4f4d\uff1a", paste(miss, collapse = "\u3001"),
                                  # 原文：。請加上後再上傳。
                                  "\u3002\u8acb\u52a0\u4e0a\u5f8c\u518d\u4e0a\u50b3\u3002", kitsui_missing_hint(names(data)))))
  }
  # 約定以外的欄拿掉：kits_resp() 要一個列次裡每一欄都一樣，多的欄會讓每個量尺都停下來
  dropped <- setdiff(names(data), KITSUI_COLUMNS$column)
  if (length(dropped) > 0) data <- data[intersect(names(data), KITSUI_COLUMNS$column)]
  choices <- choices %||% kitsui_pick_choices(data)
  sel <- kitsui_parse_picks(picks, choices)
  # 原文：請勾這一回合的世代與波次。
  if (nrow(sel) == 0) return(list(problems = character(), hint = "\u8acb\u52fe\u9019\u4e00\u56de\u5408\u7684\u4e16\u4ee3\u8207\u6ce2\u6b21\u3002"))
  # wave_m 空白的列不知道是哪一個波次：勾選框沒有它們，全部都勾時也不放進來
  no_wave <- sum(is.na(data$wave_m))
  df <- if (nrow(sel) == nrow(choices) && no_wave == 0) data else {
    dplyr::semi_join(data, sel[c("cohort", "wave_m")], by = c("cohort", "wave_m"))
  }
  problems <- tryCatch({
    kitsui_check_data(df)
    character()
  # 原文：檢查資料
  }, error = \(e) kit_error_text(e, "\u6aa2\u67e5\u8cc7\u6599"))
  if (length(problems) == 0) problems <- kitsui_quick_check(df)
  if (length(problems) > 0) return(list(problems = problems))
  notes <- c(
    if (no_wave > 0) {
      # 原文：wave_m 有 %s 列是空白：不知道是哪一個波次，這些列不放進這一回合。
      sprintf("wave_m \u6709 %s \u5217\u662f\u7a7a\u767d\uff1a\u4e0d\u77e5\u9053\u662f\u54ea\u4e00\u500b\u6ce2\u6b21\uff0c\u9019\u4e9b\u5217\u4e0d\u653e\u9032\u9019\u4e00\u56de\u5408\u3002",
              format(no_wave, big.mark = ","))
    },
    kitsui_note_codes(df)
  )
  scales <- dplyr::arrange(kitsui_scales(df), .data$cohort != reference, .data$scale)
  list(df = df, scales = scales, persons = kitsui_persons(df), picks = sel,
       problems = character(), notes = notes, dropped = dropped)
}

#' 前 n 個，用「、」接起來；還有更多時加「…」
#' @noRd
kitsui_first <- function(x, n = 10) {
  # 原文：、 | …
  paste0(paste(utils::head(x, n), collapse = "\u3001"), if (length(x) > n) "\u2026")
}

#' 缺欄位時多一句：看起來是分號或 Tab 分隔、欄名是大寫或前後有空白
#' @param cols 檔案的欄名
#' @return 一句中文；看不出原因時 ""
#' @noRd
kitsui_missing_hint <- function(cols) {
  # 原文：請另存成逗號分隔的 CSV（Excel 的「CSV UTF-8（逗號分隔）」）再上傳。
  save_as <- "\u8acb\u53e6\u5b58\u6210\u9017\u865f\u5206\u9694\u7684 CSV\uff08Excel \u7684\u300cCSV UTF-8\uff08\u9017\u865f\u5206\u9694\uff09\u300d\uff09\u518d\u4e0a\u50b3\u3002"
  if (length(cols) == 1 && grepl(";", cols)) {
    # 原文：看起來是用分號（;）分隔的檔案。
    return(paste0("\u770b\u8d77\u4f86\u662f\u7528\u5206\u865f\uff08;\uff09\u5206\u9694\u7684\u6a94\u6848\u3002", save_as))
  }
  if (length(cols) == 1 && grepl("\t", cols)) {
    # 原文：看起來是用 Tab 分隔的檔案。
    return(paste0("\u770b\u8d77\u4f86\u662f\u7528 Tab \u5206\u9694\u7684\u6a94\u6848\u3002", save_as))
  }
  near <- cols[!cols %in% KITSUI_COLUMNS$column & tolower(trimws(cols)) %in% KITSUI_COLUMNS$column]
  if (length(near) == 0) return("")
  # 原文：欄名要全部小寫、前後不能有空白： | 「 | 」 |  要改成 
  paste0("\u6b04\u540d\u8981\u5168\u90e8\u5c0f\u5beb\u3001\u524d\u5f8c\u4e0d\u80fd\u6709\u7a7a\u767d\uff1a", kitsui_first(paste0("\u300c", near, "\u300d"), 4), " \u8981\u6539\u6210 ",
         # 原文：。
         kitsui_first(tolower(trimws(near)), 4), "\u3002")
}

#' 看起來是遺漏碼的分數：一題裡比其他分數大很多（差 50 以上）、而且是 90 以上的分數，
#' 例如 0–3 分的題有 95–99，或有 7777、9999
#'
#' 只提醒（橘色），分數不改：kits 會把它們當成很高的分數，那一題會分數不連續，
#' 量尺通常會停下來。遺漏碼要先在 kitJoin「遺漏值處理」轉成空白。
#' @return 一句中文；沒有時長度 0
#' @noRd
kitsui_note_codes <- function(df) {
  sc <- dplyr::distinct(df, .data$item_id, .data$item_score) |>
    dplyr::filter(!is.na(.data$item_score))
  hits <- purrr::map(split(sc$item_score, sc$item_id), \(x) {
    x <- sort(x)
    if (length(x) < 2) return(numeric())
    jump <- which(diff(x) >= 50 & x[-1] >= 90)
    if (length(jump) == 0) numeric() else x[x > x[jump[1]]]
  }) |>
    purrr::compact()
  if (length(hits) == 0) return(character())
  codes <- sort(unique(unlist(hits)))
  # 原文：（ | ）
  each <- purrr::imap_chr(hits, \(x, id) paste0(id, "\uff08", kitsui_first(format(x, trim = TRUE), 4), "\uff09"))
  paste0(
    # 原文： 題有看起來是遺漏碼的分數（比其他分數大很多）：
    length(hits), " \u984c\u6709\u770b\u8d77\u4f86\u662f\u907a\u6f0f\u78bc\u7684\u5206\u6578\uff08\u6bd4\u5176\u4ed6\u5206\u6578\u5927\u5f88\u591a\uff09\uff1a",
    # 原文：。這些值是： | 。
    kitsui_some(each, 3), "\u3002\u9019\u4e9b\u503c\u662f\uff1a", kitsui_first(format(codes, trim = TRUE), 8), "\u3002",
    # 原文：kits 會把它們當成真的分數，這些題會分數不連續，量尺可能停下來。
    "kits \u6703\u628a\u5b83\u5011\u7576\u6210\u771f\u7684\u5206\u6578\uff0c\u9019\u4e9b\u984c\u6703\u5206\u6578\u4e0d\u9023\u7e8c\uff0c\u91cf\u5c3a\u53ef\u80fd\u505c\u4e0b\u4f86\u3002",
    # 原文：請先在 kitJoin「 | 」把遺漏碼改成空白，再上傳。
    "\u8acb\u5148\u5728 kitJoin\u300c", KITJOIN_MISSING_TAB, "\u300d\u628a\u907a\u6f0f\u78bc\u6539\u6210\u7a7a\u767d\uff0c\u518d\u4e0a\u50b3\u3002"
  )
}

#' 這一回合的資料與檢查（kitsui_data_select() 加上名稱與題庫的計畫）
#'
#' @param waves 勾選的世代 × 波次（"M03 3"），或只寫月齡（"3"）
#' @param fixed 上一回合的固定檔（名稱不能和之前的回合重複）
#' @return kitsui_data_select() 加上 plan（kitsui_round_plan()，每個量尺用誰的題庫）；
#'   名稱有問題時 problems 是中文訊息
#' @noRd
kitsui_data_check <- function(data, waves, round_label, fixed) {
  fixed <- fixed %||% kitsui_fixed_empty()
  ck <- kitsui_data_select(data, waves)
  if (length(ck$problems) > 0 || !is.null(ck$hint)) return(ck)
  problems <- kitsui_label_problem(round_label, fixed$version$rounds)
  if (length(problems) > 0) return(list(problems = problems))
  c(ck, list(plan = kitsui_round_plan(ck$df, fixed)))
}

# 提醒（橘色，不擋下一步） ----

#' 勾到的波次之前的回合已經校準過（固定檔的 waves.csv 裡，同一個量尺的同一個波次）
#' @param plan kitsui_round_plan()
#' @return 中文訊息；沒有時長度 0
#' @noRd
kitsui_note_waves <- function(plan, fixed) {
  unit <- kitsui_wave_unit(fixed)
  done <- dplyr::filter(fixed$waves, !is.na(.data$wave_m))
  hit <- dplyr::inner_join(dplyr::distinct(plan, .data$data_scale, .data$wave_m), done,
                           by = c(data_scale = "scale", "wave_m"))
  if (nrow(hit) == 0) return(character())
  # 原文：這些波次之前的回合已經校準過：
  paste0("\u9019\u4e9b\u6ce2\u6b21\u4e4b\u524d\u7684\u56de\u5408\u5df2\u7d93\u6821\u6e96\u904e\uff1a",
         # 原文：%s 的 %s（第 %s 回合）
         paste(sprintf("%s \u7684 %s\uff08\u7b2c %s \u56de\u5408\uff09", hit$data_scale, kitsui_wave_text(hit$wave_m, unit),
                       # 原文：、
                       hit$round), collapse = "\u3001"),
         # 原文：。再校準一次，同一批孩子會有兩份分數。
         "\u3002\u518d\u6821\u6e96\u4e00\u6b21\uff0c\u540c\u4e00\u6279\u5b69\u5b50\u6703\u6709\u5169\u4efd\u5206\u6578\u3002")
}

#' 要用題庫固定校準，但這一回合的題沒有一題在那個題庫裡（題號都不一樣）：② 一定停下來
#' （kits："No item of the new data is in `ref`"），在 ① 就先說
#' @param plan kitsui_round_plan()
#' @return 中文訊息；沒有時長度 0
#' @noRd
kitsui_note_no_common <- function(df, plan, fixed) {
  use <- dplyr::distinct(plan[plan$scale %in% fixed$bank$scale, ], .data$data_scale, .data$scale)
  if (nrow(use) == 0) return(character())
  have <- dplyr::distinct(df, .data$domain, .data$cohort, .data$item_id)
  have$data_scale <- scale_key(have$domain, have$cohort)
  bad <- purrr::map_lgl(seq_len(nrow(use)), \(i) {
    items <- have$item_id[have$data_scale == use$data_scale[i]]
    bank <- fixed$bank$item_id[fixed$bank$scale == use$scale[i]]
    vers <- fixed$versions$current_id[fixed$versions$scale == use$scale[i]]
    !any(items %in% c(bank, vers))
  })
  if (!any(bad)) return(character())
  # 原文：%s：%s 的題庫裡沒有這一回合的任何一題（題號都不一樣），② 會停下來。
  sprintf(paste0("%s\uff1a%s \u7684\u984c\u5eab\u88e1\u6c92\u6709\u9019\u4e00\u56de\u5408\u7684\u4efb\u4f55\u4e00\u984c\uff08\u984c\u865f\u90fd\u4e0d\u4e00\u6a23\uff09\uff0c\u2461 \u6703\u505c\u4e0b\u4f86\u3002",
                 # 原文：請確認題號和之前的回合相同；或在 ② 把它選成「這一回合不做」。
                 "\u8acb\u78ba\u8a8d\u984c\u865f\u548c\u4e4b\u524d\u7684\u56de\u5408\u76f8\u540c\uff1b\u6216\u5728 \u2461 \u628a\u5b83\u9078\u6210\u300c\u9019\u4e00\u56de\u5408\u4e0d\u505a\u300d\u3002"),
          use$data_scale[bad], use$scale[bad])
}

#' 每個校準的量尺，題目有沒有連在一起：題目 × （世代、波次）的圖分成幾組
#'
#' 同一個（世代、波次）作答的題連在一起；兩個（世代、波次）有共同題，就連在一起。分成好幾組、
#' 組和組之間沒有共同題時，kits 不能把它們放在同一把尺上，② 會停下來（kits："separate groups
#' that share no item"）。所以在 ① 就先說。
#' 校準的單位照 plan（借用另一世代題庫的波次放在那個量尺上）。有題庫時（固定校準），有題庫的題
#' 的組由題庫定位，只有沒有題庫題的組才是問題；每一組都沒有題庫的題時，是 kitsui_note_no_common()
#' 說的情況，這裡不重複。
#' @param plan NULL 或 kitsui_round_plan()；fixed 是上一回合的固定檔（NULL 時沒有題庫）
#' @return tibble：scale（校準的量尺）、n_groups（幾組）、groups（list：每組的世代 × 波次，
#'   list 的每個元素是 tibble cohort、wave_m）；只列出不連在一起的量尺（都連在一起時 0 列）
#' @noRd
kitsui_item_groups <- function(df, plan = NULL, fixed = NULL) {
  empty <- tibble::tibble(scale = character(), n_groups = integer(), groups = list())
  if (NROW(df) == 0) return(empty)
  cells <- dplyr::distinct(df, .data$domain, .data$cohort, .data$wave_m, .data$item_id)
  cells$data_scale <- scale_key(cells$domain, cells$cohort)
  cells$scale <- cells$data_scale
  if (!is.null(plan) && nrow(plan) > 0) {
    at <- match(paste(cells$data_scale, cells$wave_m), paste(plan$data_scale, plan$wave_m))
    cells$scale <- dplyr::coalesce(plan$scale[at], cells$data_scale)
  }
  out <- purrr::map(split(cells, cells$scale), \(part) {
    comp <- kitsui_cell_components(paste(part$cohort, part$wave_m), part$item_id)
    n <- length(unique(comp$cell))
    if (n < 2) return(NULL)
    s <- part$scale[1]
    bank <- if (!is.null(fixed) && s %in% fixed$bank$scale) {
      c(fixed$bank$item_id[fixed$bank$scale == s],
        fixed$versions$current_id[fixed$versions$scale == s])
    }
    group <- comp$cell[match(paste(part$cohort, part$wave_m), comp$key)]
    if (length(bank) > 0) {
      anchored <- unique(group[part$item_id %in% bank])
      # 每一組都沒有題庫的題：kitsui_note_no_common() 說；每一組都有：題庫把它們放在同一把尺上
      if (length(anchored) == 0 || length(anchored) == n) return(NULL)
    }
    waves <- dplyr::distinct(tibble::tibble(cohort = part$cohort, wave_m = part$wave_m,
                                            group = group))
    waves <- waves[order(waves$group, waves$cohort, waves$wave_m), ]
    groups <- unname(split(waves[c("cohort", "wave_m")], factor(waves$group, unique(waves$group))))
    tibble::tibble(scale = s, n_groups = n, groups = list(groups))
  })
  out <- purrr::list_rbind(purrr::compact(out))
  if (nrow(out) == 0) return(empty)
  order <- unique(plan$scale %||% character())
  out[order(match(out$scale, order), out$scale), ]
}

#' 連通的組：cell（世代 × 波次）與 item 的二部圖，每個 cell 屬於哪一組
#' @param cell,item 每一列是「這個 cell 有這一題」
#' @return list(key = 各 cell 的名字, cell = 各 cell 的組號（1、2…，照第一次出現的順序）)
#' @noRd
kitsui_cell_components <- function(cell, item) {
  keys <- unique(cell)
  ci <- match(cell, keys)
  ii <- match(item, unique(item))
  comp <- seq_along(keys)
  repeat {
    by_item <- as.vector(tapply(comp[ci], ii, min))
    new <- as.vector(tapply(by_item[ii], ci, min))
    if (identical(new, comp)) break
    comp <- new
  }
  list(key = keys, cell = match(comp, unique(comp)))
}

#' 題目沒有連在一起的量尺（kitsui_item_groups()）：每個量尺一句
#' @param unit 波次的單位（kitsui_wave_unit()）
#' @return 中文句子；沒有時長度 0
#' @noRd
kitsui_note_groups <- function(groups, unit = "months") {
  if (NROW(groups) == 0) return(character())
  purrr::map2_chr(groups$scale, groups$groups, \(s, gs) {
    one_cohort <- length(unique(unlist(purrr::map(gs, "cohort")))) == 1
    each <- purrr::map_chr(gs, \(g) {
      w <- kitsui_waves_text(g$wave_m, unit)
      if (one_cohort) w else paste(paste0(g$cohort, " ", kitsui_wave_text(g$wave_m, unit)),
                                   # 原文：、
                                   collapse = "\u3001")
    })
    # 原文：…
    shown <- if (length(each) > 4) c(each[1:4], "\u2026") else each
    # 原文：：這個量尺的題目分成  |  組（ | ；
    paste0(s, "\uff1a\u9019\u500b\u91cf\u5c3a\u7684\u984c\u76ee\u5206\u6210 ", kitDesign::kit_num(length(gs)), " \u7d44\uff08", paste(shown, collapse = "\uff1b"),
           # 原文：），各組之間沒有共同題，不能放在同一把尺上。
           "\uff09\uff0c\u5404\u7d44\u4e4b\u9593\u6c92\u6709\u5171\u540c\u984c\uff0c\u4e0d\u80fd\u653e\u5728\u540c\u4e00\u628a\u5c3a\u4e0a\u3002")
  })
}

#' 固定檔不是這次連線最新的一回合（④ 交過更新的一回合時）
#' @param rounds 現在的固定檔裡的回合
#' @param latest NULL 或這次連線最新的一回合
#' @noRd
kitsui_note_stale <- function(rounds, latest) {
  if (is.null(latest) || latest %in% rounds) return(character())
  # 原文：現在沒有用固定檔
  have <- if (length(rounds) == 0) "\u73fe\u5728\u6c92\u6709\u7528\u56fa\u5b9a\u6a94" else {
    # 原文：這個固定檔只到第  |  回合
    paste0("\u9019\u500b\u56fa\u5b9a\u6a94\u53ea\u5230\u7b2c ", utils::tail(rounds, 1), " \u56de\u5408")
  }
  # 原文：，這次連線已經做到第  |  回合。
  paste0(have, "\uff0c\u9019\u6b21\u9023\u7dda\u5df2\u7d93\u505a\u5230\u7b2c ", latest, " \u56de\u5408\u3002",
         # 原文：要接著做，請用第  |  回合的固定檔。
         "\u8981\u63a5\u8457\u505a\uff0c\u8acb\u7528\u7b2c ", latest, " \u56de\u5408\u7684\u56fa\u5b9a\u6a94\u3002")
}

#' 資料檔之前的回合用過（md5 在固定檔的 sources 裡）
#' @param files tibble：file、md5（kitsui_read_uploads()$files）
#' @noRd
kitsui_note_sources <- function(files, fixed) {
  if (is.null(files) || nrow(files) == 0) return(character())
  hit <- dplyr::inner_join(files[c("file", "md5")], fixed$sources[c("round", "md5")],
                           by = "md5", relationship = "many-to-many")
  if (nrow(hit) == 0) return(character())
  # 原文：、
  hit <- dplyr::summarise(hit, round = paste(unique(.data$round), collapse = "\u3001"), .by = "file")
  # 原文：%s：這個檔案在第 %s 回合用過。
  sprintf("%s\uff1a\u9019\u500b\u6a94\u6848\u5728\u7b2c %s \u56de\u5408\u7528\u904e\u3002", hit$file, hit$round)
}

#' 約定以外的欄（kitsui_read_uploads()、kitsui_data_select() 拿掉的）
#' @noRd
kitsui_note_dropped <- function(dropped) {
  if (length(dropped) == 0) return(character())
  # 原文：資料約定以外的欄不用，已經拿掉： | 。
  paste0("\u8cc7\u6599\u7d04\u5b9a\u4ee5\u5916\u7684\u6b04\u4e0d\u7528\uff0c\u5df2\u7d93\u62ff\u6389\uff1a", kitsui_first(dropped, 8), "\u3002",
         # 原文：kitsUI 只用「資料檔要有哪些欄位」表上的欄。
         "kitsUI \u53ea\u7528\u300c\u8cc7\u6599\u6a94\u8981\u6709\u54ea\u4e9b\u6b04\u4f4d\u300d\u8868\u4e0a\u7684\u6b04\u3002")
}

#' 得分不完整的題（kitsui_category_problems()）：每題一句；太多時只列前 n 題
#' @param problems tibble：scale、item_id、problem、detail_zh（detail_zh 已經有題號）
#' @noRd
kitsui_note_categories <- function(problems, n = 12) {
  if (is.null(problems) || nrow(problems) == 0) return(character())
  # 原文：%s：%s。 | 。$
  out <- sprintf("%s\uff1a%s\u3002", problems$scale, sub("\u3002$", "", problems$detail_zh))
  if (length(out) <= n) return(out)
  # 原文：……還有 %d 題。
  c(out[seq_len(n)], sprintf("\u2026\u2026\u9084\u6709 %d \u984c\u3002", length(out) - n))
}

#' 很快的作答檢查（幾百萬列也只要一兩秒）：同一列次同一題只能一列、分數是 0 以上的整數
#'
#' 每個量尺的作答矩陣（kits_resp()）要等到看那個量尺、或校準時才做，因為大的資料要好幾秒。
#' @return 中文訊息；沒問題時長度 0
#' @noRd
kitsui_quick_check <- function(df) {
  out <- character()
  dup <- vctrs::vec_duplicate_detect(df[c("visit_id", "item_id")])
  if (any(dup)) {
    i <- which(dup)[1]
    # 原文：同一列次同一題有好幾列（共  |  列），例如 
    out <- c(out, paste0("\u540c\u4e00\u5217\u6b21\u540c\u4e00\u984c\u6709\u597d\u5e7e\u5217\uff08\u5171 ", sum(dup), " \u5217\uff09\uff0c\u4f8b\u5982 ",
                         # 原文： 的  | 。每個列次每一題只能有一列。
                         df$visit_id[i], " \u7684 ", df$item_id[i], "\u3002\u6bcf\u500b\u5217\u6b21\u6bcf\u4e00\u984c\u53ea\u80fd\u6709\u4e00\u5217\u3002"))
  }
  x <- df$item_score[!is.na(df$item_score)]
  if (any(x < 0 | x != round(x))) {
    # 原文：item_score 要是 0 以上的整數（0、1、2…）。請檢查資料。
    out <- c(out, "item_score \u8981\u662f 0 \u4ee5\u4e0a\u7684\u6574\u6578\uff080\u30011\u30012\u2026\uff09\u3002\u8acb\u6aa2\u67e5\u8cc7\u6599\u3002")
  }
  ids <- tryCatch({
    kitsui_check_ids(df)
    character()
  # 原文：檢查資料
  }, error = \(e) kit_error_text(e, "\u6aa2\u67e5\u8cc7\u6599"))
  c(out, ids)
}

# 上傳 ----

#' 上傳的資料檔（一個或好幾個長格式的檔案）讀進來，接成一張表
#'
#' 每個檔案的欄位要一樣（順序可以不同），同一欄的型別也要一樣；不一樣時說是哪一個檔案。
#' 資料約定的欄位照 KITSUI_TYPES 讀：編號是文字，月齡與分數是數字。
#' @param files data.frame：name（檔名）、datapath（路徑），就是 shiny::fileInput() 的值
#' @param progress NULL，或 function(i, n, name)：讀第 i 個（共 n 個）檔案之前呼叫
#' 約定以外的欄拿掉（kits_resp() 要一個列次裡每一欄都一樣），記在 dropped。
#' @return list(data, files, dropped)：files 是 tibble（file、rows、md5）；dropped 是拿掉的欄
#' @noRd
kitsui_read_uploads <- function(files, progress = NULL) {
  # 原文：請選擇資料檔（CSV、CSV.GZ 或 SAV）。
  if (is.null(files) || nrow(files) == 0) kitDesign::kit_abort("\u8acb\u9078\u64c7\u8cc7\u6599\u6a94\uff08CSV\u3001CSV.GZ \u6216 SAV\uff09\u3002")
  n <- nrow(files)
  # 用 for：purrr 會把錯誤包起來，使用者就看不到原來的中文訊息
  datas <- vector("list", n)
  for (i in seq_len(n)) {
    if (!is.null(progress)) progress(i, n, files$name[i])
    datas[[i]] <- kitsui_read_upload(files$datapath[i], files$name[i])
  }
  datas <- purrr::set_names(datas, files$name)
  dropped <- unique(unlist(purrr::map(datas, \(d) attr(d, "dropped"))))
  kitsui_same_columns(datas)
  list(data = dplyr::bind_rows(unname(datas)),
       files = tibble::tibble(file = files$name, rows = unname(purrr::map_int(datas, nrow)),
                              md5 = unname(tools::md5sum(files$datapath))),
       dropped = as.character(dropped))
}

# 一個檔案：讀成數字的欄有讀不懂的值時停下來（不默默變成空白）。
# 列數和 Excel 一樣算（第 1 列是欄名）；readr 的提醒由這裡的中文訊息取代
kitsui_read_upload <- function(path, name) {
  r <- suppressWarnings(read_data_file(path, name, col_types = KITSUI_TYPES))
  if (ncol(r$data) == 0 || nrow(r$data) == 0) {
    # 原文： 是空的檔案 |  只有欄名，沒有作答的列
    kitDesign::kit_abort(name, if (ncol(r$data) == 0) " \u662f\u7a7a\u7684\u6a94\u6848" else " \u53ea\u6709\u6b04\u540d\uff0c\u6c92\u6709\u4f5c\u7b54\u7684\u5217",
                         # 原文：。請確認匯出的檔案有資料再上傳。
                         "\u3002\u8acb\u78ba\u8a8d\u532f\u51fa\u7684\u6a94\u6848\u6709\u8cc7\u6599\u518d\u4e0a\u50b3\u3002")
  }
  if (r$n_problems > 0) {
    p <- r$problems[1, ]
    # 原文： 有  |  個值讀不懂。
    kitDesign::kit_abort(name, " \u6709 ", r$n_problems, " \u500b\u503c\u8b80\u4e0d\u61c2\u3002",
                         # 原文：例如第  |  列的  |  是「 | 」，
                         "\u4f8b\u5982\u7b2c ", p$row, " \u5217\u7684 ", names(r$data)[p$col], " \u662f\u300c", p$actual, "\u300d\uff0c",
                         # 原文：應該是 | 數字 | 。
                         "\u61c9\u8a72\u662f", if (grepl("double|number", p$expected)) "\u6578\u5b57" else p$expected, "\u3002",
                         # 原文：請修正後再上傳。
                         "\u8acb\u4fee\u6b63\u5f8c\u518d\u4e0a\u50b3\u3002")
  }
  # 約定以外的欄拿掉（記在 attr(, "dropped")）；缺約定的欄時留著，① 才說得出缺什麼
  data <- r$data
  dropped <- setdiff(names(data), KITSUI_COLUMNS$column)
  if (length(dropped) > 0 && all(KITSUI_REQUIRED %in% names(data))) {
    data <- data[intersect(names(data), KITSUI_COLUMNS$column)]
  } else {
    dropped <- character()
  }
  # SAV 的編號可能是數字：轉成文字，檔案之間才接得起來
  text <- intersect(names(KITSUI_TYPES)[KITSUI_TYPES == "c"], names(data))
  out <- haven::zap_labels(data) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(text), as.character))
  attr(out, "dropped") <- dropped
  out
}

# 每個檔案的欄位與第一個檔案相同；同一欄的型別也相同（全部空白的欄不算）。
# 用 for：purrr 會把錯誤包起來，使用者就看不到原來的中文訊息
kitsui_same_columns <- function(datas) {
  first <- names(datas)[1]
  ref <- datas[[1]]
  # 原文：數字 | 文字
  kind <- \(x) if (all(is.na(x))) NA_character_ else if (is.numeric(x)) "\u6578\u5b57" else "\u6587\u5b57"
  for (name in names(datas)[-1]) {
    d <- datas[[name]]
    miss <- setdiff(names(ref), names(d))
    extra <- setdiff(names(d), names(ref))
    if (length(miss) + length(extra) > 0) {
      # 原文：檔案  |  的欄位和  |  不一樣。
      kitDesign::kit_abort("\u6a94\u6848 ", name, " \u7684\u6b04\u4f4d\u548c ", first, " \u4e0d\u4e00\u6a23\u3002",
                           # 原文：少了： | 、 | 。
                           if (length(miss) > 0) paste0("\u5c11\u4e86\uff1a", paste(miss, collapse = "\u3001"), "\u3002"),
                           # 原文：多了： | 、 | 。
                           if (length(extra) > 0) paste0("\u591a\u4e86\uff1a", paste(extra, collapse = "\u3001"), "\u3002"),
                           # 原文：每個檔案的欄位要一樣。
                           "\u6bcf\u500b\u6a94\u6848\u7684\u6b04\u4f4d\u8981\u4e00\u6a23\u3002")
    }
    a <- purrr::map_chr(ref, kind)
    b <- purrr::map_chr(d[names(ref)], kind)
    bad <- which(a != b)
    if (length(bad) > 0) {
      col <- names(ref)[bad[1]]
      # 原文：檔案  |  的  |  欄是 | ， |  的是
      kitDesign::kit_abort("\u6a94\u6848 ", name, " \u7684 ", col, " \u6b04\u662f", b[[bad[1]]], "\uff0c", first, " \u7684\u662f",
                           # 原文：。每個檔案同一欄的型別要一樣。
                           a[[bad[1]]], "\u3002\u6bcf\u500b\u6a94\u6848\u540c\u4e00\u6b04\u7684\u578b\u5225\u8981\u4e00\u6a23\u3002")
    }
  }
  invisible(datas)
}

#' 欄位範例檔：kitm03 兩個列次、兩個領域，各 6 題（24 列，資料約定的全部欄位）
#' @noRd
kitsui_example_data <- function() {
  r <- kits::kitm03$responses
  r |>
    dplyr::filter(.data$visit_id %in% unique(r$visit_id)[1:2]) |>
    dplyr::slice_head(n = 6, by = c("visit_id", "domain")) |>
    dplyr::select(dplyr::all_of(KITSUI_COLUMNS$column))
}

#' 資料約定的欄位表（欄名是英文）：column、required、meaning
#' @noRd
kitsui_columns_table <- function() {
  # 原文：是 | 否
  dplyr::transmute(KITSUI_COLUMNS, .data$column, required = ifelse(.data$required, "\u662f", "\u5426"),
                   .data$meaning)
}
