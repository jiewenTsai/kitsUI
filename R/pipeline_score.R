# 受試者評分：這一回合的資料，kits_subj() -> kits_convert(內部連結) -> kits_convert(外部連結)
# 早先各回合的分數不重算（各回合自己存）
# 校準量尺上的分數（kits_subj()）每回合只算一次（kitsui_own_scores()）；換算（kits_convert()）
# 只是一條直線，幾乎不花時間。所以做了新的連結，只重做換算，不重算分數

#' 一個量尺校準量尺上的分數（kits_subj()，還沒換算）
#'
#' 有 kitsui_own_scores()（pipeline_memo.R，每回合算一次存在 round$memo）時用它；
#' 沒有時直接算。
#' @param persons kitsui_persons(df)（① 算好的）
#' @noRd
kitsui_own <- function(round, scale, persons) {
  if (exists("kitsui_own_scores", mode = "function")) {
    return(kitsui_own_scores(round, scale, persons))
  }
  kits::kits_subj(round$data[[scale]], round$items[[scale]], persons = persons)
}

#' 這一回合各量尺校準量尺上的分數
#' @param progress NULL 或 function(i, n, scale)：每個量尺開始前呼叫一次（同 kitsui_round()）
#' @return 具名 list（量尺）：kits_subj
#' @noRd
kitsui_score_own <- function(round, persons, progress = NULL, scales = names(round$items)) {
  n <- length(scales)
  purrr::map(seq_len(n), \(i) {
    if (!is.null(progress)) progress(i, n, scales[i])
    kitsui_own(round, scales[i], persons)
  }) |>
    purrr::set_names(scales)
}

#' 一個量尺的換算路線上的連結（kits_link 物件，依序）
#' @param links kitsui_links(fixed)（一組固定檔只建一次）
#' @param stage "external"（換到最後，有外部連結時是 D）或 "internal"（只做內部連結）
#' @noRd
kitsui_path_links <- function(fixed, links, scale, stage = "external") {
  path <- kitsui_link_path(fixed, scale)
  if (stage == "internal") path <- setdiff(path, "D")
  unname(links[path])
}

# 依序換算（沒有連結時原樣）
kitsui_convert_path <- function(x, path) {
  if (length(path) == 0) x else do.call(kits::kits_convert, c(list(x), path))
}

#' 這一回合各量尺的分數，合成一張表
#'
#' @param round kitsui_round() 的結果（用 items 與 data：改名、併過的矩陣）
#' @param df 這一回合的長格式資料（persons 是 NULL 時用來算列次資訊）
#' @param fixed 固定檔（含已做的連結）
#' @param links NULL 或 kitsui_links(fixed)（已經建好時傳進來，不重建）
#' @param persons NULL 或 ① 算好的 kitsui_persons(df)
#' @param progress NULL 或 function(i, n, scale)：每個量尺算分數前呼叫一次
#' @param round_label 這一回合的名稱（分數表的 round 欄）
#' @return list(scores, subj, own)：scores 一列一列次（kitsui_score_rows()）；subj 是各量尺換算後的
#'   kits_subj；own 是換算前（校準量尺上）的 kits_subj
#' @noRd
kitsui_score <- function(round, df, fixed = round$fixed, links = NULL, persons = NULL,
                         progress = NULL, round_label = utils::tail(fixed$version$rounds, 1)) {
  persons <- persons %||% kitsui_persons(df)
  own <- kitsui_score_own(round, persons, progress)
  kitsui_score_convert(own, fixed, links, round_label)
}

#' 換算（不重算分數）：校準量尺上的分數 -> 內部連結 -> 外部連結
#' @param own kitsui_score_own() 的結果
#' @inheritParams kitsui_score
#' @noRd
kitsui_score_convert <- function(own, fixed, links = NULL, round_label = NULL) {
  links <- links %||% kitsui_links(fixed)
  subj <- purrr::imap(own, \(x, s) kitsui_convert_path(x, kitsui_path_links(fixed, links, s)))
  list(scores = kitsui_score_rows(subj, round_label, own), subj = subj, own = own)
}

#' 分數表：一列一列次一個量尺
#'
#' bank 是校準用的題庫（借用另一世代題庫時是另一個世代的量尺，人的 cohort 不變）；scale 是換算後的量尺。
#' ability、ability_se（kits 的 theta、se）是 bank 那把尺上的能力值（各世代分開校準，沒有經過連結）；
#' d_score、d_score_se 是換到 D 之後的量尺分數（還沒換到 D 的量尺是空白），
#' se_total 是 d_score_se 與 se_link 合起來。
#' @param subj 具名 list（校準的量尺）：換算後的 kits_subj
#' @param own 具名 list（同 subj）：換算前（校準量尺上）的 kits_subj；NULL 時用 subj
#' @noRd
kitsui_score_rows <- function(subj, round_label = NULL, own = NULL) {
  own <- own %||% subj
  cols <- c("visit_id", "subj_id", "cohort", "wave_m", "age_m", "sex", "domain", "round", "bank",
            "scale", "raw", "ability", "ability_se", "d_score", "d_score_se", "se_link", "se_total")
  label <- if (length(round_label) == 0) NA_character_ else round_label
  purrr::imap(subj, \(x, s) {
    base <- tibble::as_tibble(own[[s]])
    conv <- tibble::as_tibble(x)
    on_d <- identical(attr(x, "scale"), "D")
    base |>
      dplyr::mutate(
        domain = scale_domain(s), round = label, bank = s, scale = attr(x, "scale"),
        ability = .data$theta, ability_se = .data$se,
        d_score = if (on_d) conv$theta else NA_real_, d_score_se = if (on_d) conv$se else NA_real_,
        se_link = if (on_d && "se_link" %in% names(conv)) conv$se_link else NA_real_,
        se_total = if (on_d && "se_total" %in% names(conv)) conv$se_total else NA_real_
      ) |>
      dplyr::select(dplyr::any_of(cols))
  }) |>
    unname() |>
    purrr::list_rbind()
}

# 分數表各欄的一句說明（畫面上列在表的下面）
KITSUI_SCORE_COLUMNS <- c(
  visit_id = "列次的編號（一個孩子的一次施測）。",
  subj_id = "孩子的編號。",
  cohort = "孩子的世代（借用另一世代題庫時也不變）。",
  wave_m = "波次（月齡）。",
  age_m = "施測時的實際月齡。",
  sex = "性別。",
  domain = "領域。",
  round = "這一回合的名稱。",
  bank = "校準用的題庫（量尺）。借用另一世代題庫時，這裡是那個世代的量尺。",
  scale = "分數最後在哪一把尺上。",
  raw = "原始分數（答對或通過的步驟數）。",
  ability = paste0("能力值（ability，kits 裡叫 theta；WLE，logit）：在 bank 那把尺上，",
                   "各世代分開校準，沒有經過內部、外部連結。"),
  ability_se = "ability 的標準誤（測量誤差）。",
  d_score = "量尺分數（d_score）：經過內部、外部連結換到 D 之後的值；還沒換到 D 時是空白。",
  d_score_se = "d_score 的標準誤（測量誤差，D 的單位）。",
  se_link = "連結的誤差（換算本身的不確定性，D 的單位）。",
  se_total = "d_score 的測量誤差與連結誤差合起來：sqrt(d_score_se² + se_link²)。"
)

#' 分數表有的欄，各一句說明
#' @return tibble（欄名英文）：column、meaning
#' @noRd
kitsui_score_columns <- function(scores) {
  have <- intersect(names(KITSUI_SCORE_COLUMNS), names(scores))
  tibble::tibble(column = have, meaning = unname(KITSUI_SCORE_COLUMNS[have]))
}

#' 每個量尺的換算路線：校準的題庫 → 內部連結的終點 → 外部連結（D）
#'
#' 借用另一世代題庫時 data 寫出是哪一個世代的資料（例如「motor M03（60 個月）」），note 說明
#' 「M03 的資料，用 motor M36 的題庫」，讓人看得出世代沒有變。
#' @param subj kitsui_score() 的 subj
#' @param plan NULL 或 round$plan（kitsui_round_plan()）
#' @return tibble（欄名是英文）：bank（校準的題庫）、data（資料的量尺與波次）、route（換算路線）、
#'   final_scale、n（列次）、note（借用另一世代題庫時一句話，其他是 NA）
#' @noRd
kitsui_score_routes <- function(fixed, subj, plan = NULL) {
  key <- kitsui_link_key(fixed$links %||% tibble::tibble(kind = character()))
  purrr::imap(subj, \(x, s) {
    steps <- fixed$links$to_scale[match(kitsui_link_path(fixed, s), key)]
    data <- kitsui_route_data(plan, s)
    other <- setdiff(data$data_scale, s)
    note <- if (length(other) > 0) {
      paste0(paste(scale_cohort(other), collapse = "、"), " 的資料，借用另一世代題庫（", s, "）。",
             "孩子的世代（cohort）不變。")
    }
    tibble::tibble(bank = s, data = paste(data$text, collapse = "；"),
                   route = paste(c(s, steps), collapse = " → "), final_scale = attr(x, "scale"),
                   n = nrow(x), note = note %||% NA_character_)
  }) |>
    purrr::list_rbind()
}

# 校準到這個題庫的資料量尺與波次（plan 沒有時就是題庫自己）
kitsui_route_data <- function(plan, scale) {
  part <- if (!is.null(plan)) plan[plan$scale == scale, ]
  if (is.null(part) || nrow(part) == 0) return(tibble::tibble(data_scale = scale, text = scale))
  part |>
    dplyr::summarise(waves = paste(sort(unique(.data$wave_m)), collapse = "、"),
                     .by = "data_scale") |>
    dplyr::mutate(text = paste0(.data$data_scale, "（", .data$waves, " 個月）"))
}

#' 這一回合 ② 的 flag（check = "flag"）：量尺與原因
#' @return tibble：scale、detail（沒有 flag 時 0 列）
#' @noRd
kitsui_round_flags <- function(round) {
  log <- round$log
  none <- tibble::tibble(scale = character(), detail = character())
  if (is.null(log) || nrow(log) == 0) return(none)
  dplyr::filter(log, .data$check == "flag") |>
    dplyr::select("scale", "detail")
}

#' 搜尋：visit_id 或 subj_id 含有這段文字的列（空白時全部）
#' @noRd
kitsui_score_search <- function(scores, text) {
  text <- trimws(text %||% "")
  if (!nzchar(text)) return(scores)
  hit <- \(x) !is.na(x) & grepl(text, x, fixed = TRUE)
  ids <- intersect(c("visit_id", "subj_id"), names(scores))
  keep <- Reduce(`|`, purrr::map(ids, \(v) hit(as.character(scores[[v]]))))
  scores[keep, , drop = FALSE]
}

#' 一個量尺的題目表：kits_item_table()
#'
#' 表本身在校準的量尺上（D 欄是 ability 那把尺的門檻）。同一題的門檻再用和分數同一組連結
#' 換到另外兩把尺：內部連結後（ability_linked）、再經過外部連結（D）；放在 attr(, "thresholds")。
#' 通過年齡（age50）用這一回合的作答與月齡算，和量尺無關。kitsUI 不做常模（kits_norms()），
#' 所以沒有常模算的年齡（age_reach 等）。
#' @param round kitsui_round() 的結果；df 是這一回合的長格式資料；fixed 是含連結的固定檔
#' @param links NULL 或 kitsui_links(fixed)；persons NULL 或 kitsui_persons(df)
#' @noRd
kitsui_item_table <- function(round, df, fixed, scale, links = NULL, persons = NULL) {
  item <- round$items[[scale]]
  if (is.null(item)) kit_abort("這一回合沒有 ", scale, " 的校準結果。")
  links <- links %||% kitsui_links(fixed, kitsui_link_path(fixed, scale))
  persons <- persons %||% kitsui_persons(df)
  ages <- kitsui_item_ages(round$data[[scale]], persons, kitsui_round_cohorts(round, scale))
  tab <- kits::kits_item_table(item, data = ages)
  attr(tab, "thresholds") <- kitsui_item_thresholds(item, fixed, links, scale)
  tab
}

#' 同一題的門檻換到內部連結後、外部連結後的量尺
#' @return NULL 或 tibble：item_id，有內部連結時 ability_linked_threshold，換得到 D 時 d_threshold
#' @noRd
kitsui_item_thresholds <- function(item, fixed, links, scale) {
  first <- function(path, name) {
    tab <- kits::kits_item_table(kitsui_convert_path(item, path))
    stats::setNames(tibble::tibble(item_id = tab$item_id, threshold = tab$D), c("item_id", name))
  }
  inner <- kitsui_path_links(fixed, links, scale, "internal")
  full <- kitsui_path_links(fixed, links, scale)
  parts <- list(
    if (length(inner) > 0) first(inner, "ability_linked_threshold"),
    if ("D" %in% kitsui_link_path(fixed, scale)) first(full, "d_threshold")
  ) |>
    purrr::compact()
  if (length(parts) == 0) return(NULL)
  purrr::reduce(parts, dplyr::full_join, by = "item_id")
}

#' 畫面上的題目表：三種門檻、拿掉每一列都沒有內容的欄
#'
#' 門檻：ability_threshold（校準的量尺，kits 的 D 欄）、ability_linked_threshold（內部連結後）、
#' d_threshold（D）；依最後一把尺的門檻由小到大。
#' domain、column 全是空白；stem 和 item_id 一樣（沒有給題目內容）；
#' DIF 全是 "none" 或空白（kitsUI 不做 DIF）時拿掉。
#' @return data.frame；attr(, "dropped") 是拿掉的欄
#' @noRd
kitsui_item_table_shown <- function(tab) {
  extra <- attr(tab, "thresholds")
  tab <- as.data.frame(tab)
  empty <- \(x) all(is.na(x) | x == "")
  drop <- names(tab)[purrr::map_lgl(tab, empty)]
  if ("stem" %in% names(tab) && all(is.na(tab$stem) | tab$stem == tab$item_id)) {
    drop <- c(drop, "stem")
  }
  if ("DIF" %in% names(tab) && all(is.na(tab$DIF) | tab$DIF == "none")) drop <- c(drop, "DIF")
  drop <- unique(setdiff(drop, c("item_id", "D")))
  tab <- tab[setdiff(names(tab), drop)]
  names(tab)[names(tab) == "D"] <- "ability_threshold"
  if (!is.null(extra)) {
    tab <- dplyr::left_join(tab, extra, by = "item_id") |>
      dplyr::relocate(dplyr::any_of(KITSUI_ITEM_THRESHOLDS), .after = "ability_threshold")
  }
  last <- utils::tail(intersect(KITSUI_ITEM_THRESHOLDS, names(tab)), 1)
  if (length(last) == 1) tab <- tab[order(tab[[last]], tab$item_id, na.last = TRUE), , drop = FALSE]
  rownames(tab) <- NULL
  structure(tab, dropped = drop)
}

# 題目表的三種門檻（由校準的量尺到 D）
KITSUI_ITEM_THRESHOLDS <- c("ability_threshold", "ability_linked_threshold", "d_threshold")

#' 這一回合校準到這個量尺的資料是哪些世代（借用另一世代題庫時不只是量尺自己的世代）
#' @noRd
kitsui_round_cohorts <- function(round, scale) {
  plan <- round$plan
  if (is.null(plan) || !scale %in% plan$scale) return(scale_cohort(scale))
  unique(scale_cohort(plan$data_scale[plan$scale == scale]))
}

# 作答矩陣加上每個列次的 visit_id、age_m、wave_m（kits_item_table(data = ) 的寬格式）；
# 沒有 age_m 時是 NULL。兩個世代的 visit_id 可能重複，所以只看這個量尺的資料的世代。
# persons 是 kitsui_persons(df)
kitsui_item_ages <- function(X, persons, cohorts) {
  if (!all(c("age_m", "wave_m") %in% names(persons))) return(NULL)
  if ("cohort" %in% names(persons)) {
    persons <- dplyr::filter(persons, .data$cohort %in% cohorts)
  }
  ages <- persons[match(rownames(X), persons$visit_id), c("visit_id", "age_m", "wave_m")]
  cbind(as.data.frame(ages), X)
}

#' 這一回合有兩個以上世代的領域（兩個世代的垂直連結用）
#' @noRd
kitsui_vertical_domains <- function(round) {
  domains <- scale_domain(names(round$items))
  sort(unique(domains[duplicated(domains)]))
}

# 垂直連結的三個版本（③ 的分頁標籤）
KITSUI_VERTICAL_STAGES <- c(none = "未連結", internal = "內部連結後", external = "外部連結後")

#' 一個領域各世代校準量尺上的分數（垂直連結用；只跟這一回合與領域有關，和連結無關）
#' @param progress NULL 或 function(i, n, scale)：每個世代開始前呼叫一次
#' @return 具名 list（量尺）：kits_subj；這一回合只有一個世代時是 NULL
#' @noRd
kitsui_vertical_scores <- function(round, domain, persons, progress = NULL) {
  scales <- names(round$items)[scale_domain(names(round$items)) == domain]
  if (length(scales) < 2) return(NULL)
  kitsui_score_own(round, persons, progress, scales)
}

#' 一個領域的兩個世代：連結前後三個版本的 kits_vertical()
#'
#' 分數只算一次（kitsui_vertical_scores()），再依版本換算：
#' - none（未連結）：各世代留在自己校準的量尺上。kits 的 rbind() 不讓不同量尺的分數合在一起，
#'   所以去掉量尺的標記，當成沒有量尺的分數交給 kits_vertical()（y 軸不標量尺）。
#' - internal（內部連結後）：M36 用內部連結換到參照世代的量尺，兩個世代 rbind()。
#' - external（外部連結後）：再用外部連結換到 D（和 kitsui_score() 的換算相同）。
#' @param links NULL 或 kitsui_links(fixed)；persons NULL 或 kitsui_persons(df)
#' @param raw NULL 或 kitsui_vertical_scores() 的結果（已經算好時傳進來，連結變了不用重算）
#' @return 具名 list（none、internal、external），各是 list(vertical, scale, from, why, note)：
#'   scale 是畫出來的量尺（未連結時是 NA），from 是這個領域的量尺；
#'   畫不了時 vertical 是 NULL，why 是一句原因；note 是兩個世代沒有共同月齡時的一句說明
#' @noRd
kitsui_verticals <- function(round, df, fixed, domain, links = NULL, persons = NULL,
                             raw = NULL) {
  scales <- names(round$items)[scale_domain(names(round$items)) == domain]
  if (length(scales) < 2) {
    why <- paste0("這一回合的 ", domain, " 只有一個世代，要兩個世代都在這一回合才畫得出來。")
    return(purrr::map(KITSUI_VERTICAL_STAGES, \(x) kitsui_vertical_none(scales, why)))
  }
  raw <- raw %||% kitsui_vertical_scores(round, domain, persons %||% kitsui_persons(df))
  links <- links %||% kitsui_links(fixed)
  out <- list(
    none = kitsui_vertical_raw(raw),
    internal = kitsui_vertical_linked(raw, fixed, links, "internal"),
    external = kitsui_vertical_linked(raw, fixed, links, "external")
  )
  note <- kitsui_vertical_no_common(raw)
  purrr::map(out, \(x) c(x, list(note = note)))
}

# 畫不了的版本
kitsui_vertical_none <- function(scales, why) {
  list(vertical = NULL, scale = NULL, from = scales, why = why)
}

# 未連結：去掉量尺的標記（各世代的量尺不同），kits_vertical() 照樣算各波次的中位數
kitsui_vertical_raw <- function(raw) {
  s <- dplyr::bind_rows(purrr::map(unname(raw), tibble::as_tibble))
  attr(s, "scale") <- NULL
  list(vertical = kits::kits_vertical(s), scale = NA_character_, from = names(raw), why = NULL)
}

# 內部連結後（到參照世代的量尺）或外部連結後（到 D）：換算後用 kits 的 rbind() 合起來
kitsui_vertical_linked <- function(raw, fixed, links, stage) {
  scales <- names(raw)
  ref <- paste(scale_domain(scales[1]), fixed$version$reference)
  target <- if (stage == "internal") ref else "D"
  converted <- purrr::imap(raw, \(x, s) {
    kitsui_convert_path(x, kitsui_path_links(fixed, links, s, stage))
  })
  ends <- purrr::map_chr(converted, \(x) attr(x, "scale"))
  if (any(ends != target)) {
    return(kitsui_vertical_none(scales, kitsui_vertical_why(fixed, scales, links, stage)))
  }
  s <- do.call(rbind, unname(converted))
  list(vertical = kits::kits_vertical(s), scale = target, from = scales, why = NULL)
}

# 畫不了的原因：缺內部連結，或（外部連結後）還沒有外部連結
kitsui_vertical_why <- function(fixed, scales, links, stage) {
  off <- setdiff(scales[scale_cohort(scales) != fixed$version$reference], names(links))
  if (length(off) > 0) {
    rest <- if (stage == "internal") "兩個世代不在同一把尺上。" else "所以換不到 D。"
    return(paste0(paste(off, collapse = "、"), " 還沒有內部連結，", rest))
  }
  if (is.null(links[["D"]])) {
    return("還沒有外部連結（到 D）。在上面建立外部連結後，這裡會畫出兩個世代在 D 上的垂直連結。")
  }
  paste0(paste(scales, collapse = "、"), " 還有量尺不在外部連結裡，換不到 D。")
}

#' 分數對照表：theta 在校準的量尺上，d 是換算後的值
#'
#' kits_subj_table() 只算一次（校準量尺上的分數）；換算後的欄直接用 kits_convert() 換這張表
#' （和換算後的分數再做一次 kits_subj_table() 每一欄都相同，但不用重解每個原始分數的 WLE）。
#' @param own 換算前的 kits_subj
#' @param path 換算到最後的連結（kitsui_path_links()）；空的 list 時不換算
#' @param internal 只有內部連結的那一段（kitsui_path_links(stage = "internal")）
#' @return data.frame：ability、ability_se 在校準的量尺上（kits 的 theta、se）；有內部連結時多
#'   ability_linked、ability_linked_se（參照世代的量尺）；換到 D 時多 d_score、d_score_se、
#'   se_link、se_total（se_link、se_total 是 d_score 的）
#' @noRd
kitsui_raw_table <- function(own, path = list(), internal = list()) {
  keys <- c("wave_m", "n_items", "raw")
  counts <- c("n", "pct", "cum_pct", "wave_n", "form_pct")
  table <- kits::kits_subj_table(own)
  # ability：校準的量尺上（和分數表的 ability 相同，各世代分開估計）
  out <- as.data.frame(table) |>
    dplyr::select(dplyr::all_of(c(keys, "theta", "se", counts))) |>
    dplyr::rename(ability = "theta", ability_se = "se")
  # ability_linked：經過內部連結（換到參照世代的量尺）
  if (length(internal) > 0) {
    inner <- as.data.frame(kitsui_convert_path(table, internal)) |>
      dplyr::select(dplyr::all_of(c(keys, "theta", "se"))) |>
      dplyr::rename(ability_linked = "theta", ability_linked_se = "se")
    out <- dplyr::left_join(out, inner, by = keys)
  }
  conv <- kitsui_convert_path(table, path)
  if (identical(attr(conv, "scale"), "D")) {
    # d_score：再經過外部連結換到 D
    d <- as.data.frame(conv) |>
      dplyr::select(dplyr::all_of(c(keys, "theta", "se")),
                    dplyr::any_of(c("se_link", "se_total"))) |>
      dplyr::rename(d_score = "theta", d_score_se = "se")
    out <- dplyr::left_join(out, d, by = keys)
  }
  dplyr::relocate(out, dplyr::all_of(counts), .after = dplyr::last_col())
}

#' 畫面上的分數對照表：預設不列沒有人的原始分數（n = 0）
#' @noRd
kitsui_raw_table_shown <- function(tab, show_empty = FALSE) {
  if (isTRUE(show_empty) || !"n" %in% names(tab)) return(tab)
  tab[tab$n > 0, , drop = FALSE]
}

#' 新固定檔的檔名（④ 下載的 zip，也是 R 程式裡讀的檔名）
#' @noRd
kitsui_fixed_name <- function(round_label) {
  label <- if (length(round_label) == 0) "round" else round_label
  paste0("kitsui_fixed_", label, ".zip")
}

#' 固定檔的中文說明（README.txt 的內容）
#'
#' 給寫固定檔的函式放進 zip（kitsUI 讀固定檔時不看它）。
#' @return 一個字串（多行）
#' @noRd
kitsui_fixed_readme <- function(fixed) {
  v <- fixed$version
  links <- kitsui_link_names(fixed)
  both <- \(x) if (length(x) == 0) "（還沒有）" else paste(x, collapse = "、")
  paste(c(
    "kitsUI 固定檔",
    "",
    "這個 zip 是 kitsUI 的固定檔。",
    "它存著到這一回合為止的題庫、題目版本與連結。",
    "下一回合要在 kitsUI 的「① 資料」上傳這個 zip，才能接著用同一個題庫與連結。",
    "請不要修改或重新壓縮裡面的檔案。",
    "固定檔寫了就不再改；每一回合會產生一個新的固定檔。",
    "",
    paste0("回合：", both(v$rounds)),
    paste0("參照世代：", v$reference %||% ""),
    paste0("量尺：", both(v$scales)),
    paste0("連結：", both(links)),
    paste0("kits 版本：", v$kits %||% "", "；kitsUI 版本：", v$kitsUI %||% ""),
    "",
    "裡面的檔案：",
    "VERSION：版本、建立日期、各回合名稱、參照世代、量尺，與每一回合資料檔的 md5。",
    "bank.csv：題庫。每個量尺每一題每一步的 beta 與標準誤，和加進來的那一回合。",
    "versions.csv：題目版本。偏移的題換成新版本時，原來的題號與目前的題號。",
    "waves.csv：每個題庫校準過的波次（月齡）。",
    "links.csv：連結的點與設定。從這些點可以重建一模一樣的連結。",
    "checks.csv：每一回合的檢查紀錄（偏移、新版本、flag、借用另一世代題庫等）。",
    "",
    "提醒：",
    "舊的 beta 永遠不動。",
    "連結做好就凍結，之後每一回合都用同一條。",
    "早先各回合的分數不會重算。",
    "每一回合的分數表請在那一回合下載保存。"
  ), collapse = "\n")
}
