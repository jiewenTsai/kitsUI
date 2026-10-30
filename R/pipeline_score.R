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
  # 外部連結不適用（參照世代沒有錨題）：最後的分數在參照世代的量尺上（ref_score），沒有 d_score
  ref <- kitsui_ref_final(fixed)
  list(scores = kitsui_score_rows(subj, round_label, own, ref), subj = subj, own = own)
}

#' 分數表：一列一列次一個量尺
#'
#' bank 是校準用的題庫（借用另一世代題庫時是另一個世代的量尺，人的 cohort 不變）；scale 是換算後的量尺。
#' ability、ability_se（kits 的 theta、se）是 bank 那把尺上的能力值（各世代分開校準，沒有經過連結）；
#' d_score、d_score_se 是換到 D 之後的量尺分數（還沒換到 D 的量尺是空白），
#' se_total 是 d_score_se 與 se_link 合起來。
#' 外部連結不適用時（ref 不是 NULL），d_score、d_score_se 換成 ref_score、ref_score_se：
#' 參照世代的量尺上的分數（參照世代自己就是 ability；其他世代經過內部連結；還沒有內部連結時空白），
#' se_link、se_total 是 ref_score 的。
#' @param subj 具名 list（校準的量尺）：換算後的 kits_subj
#' @param own 具名 list（同 subj）：換算前（校準量尺上）的 kits_subj；NULL 時用 subj
#' @param ref NULL，或外部連結不適用時的參照世代（kitsui_ref_final()）
#' @noRd
kitsui_score_rows <- function(subj, round_label = NULL, own = NULL, ref = NULL) {
  own <- own %||% subj
  last <- if (is.null(ref)) c("d_score", "d_score_se") else c("ref_score", "ref_score_se")
  cols <- c("visit_id", "subj_id", "cohort", "wave_m", "age_m", "sex", "domain", "round", "bank",
            "scale", "raw", "ability", "ability_se", last, "se_link", "se_total")
  label <- if (length(round_label) == 0) NA_character_ else round_label
  purrr::imap(subj, \(x, s) {
    base <- tibble::as_tibble(own[[s]])
    conv <- tibble::as_tibble(x)
    to <- attr(x, "scale")
    # 最後的量尺：D，或（外部連結不適用時）參照世代的量尺
    on_d <- if (is.null(ref)) identical(to, "D") else identical(scale_cohort(to), ref)
    base |>
      dplyr::mutate(
        domain = scale_domain(s), round = label, bank = s, scale = to,
        ability = .data$theta, ability_se = .data$se,
        d_score = if (on_d) conv$theta else NA_real_, d_score_se = if (on_d) conv$se else NA_real_,
        se_link = if (on_d && "se_link" %in% names(conv)) conv$se_link else NA_real_,
        se_total = if (on_d && "se_total" %in% names(conv)) conv$se_total else NA_real_
      ) |>
      dplyr::rename(dplyr::all_of(stats::setNames(c("d_score", "d_score_se"), last))) |>
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

#' 外部連結不適用時（參照世代沒有錨題）的欄：最後的分數在參照世代的量尺上
#' @noRd
kitsui_score_columns_ref <- function(ref) {
  c(
    ref_score = paste0("參照世代（", ref, "）的量尺上的分數，也就是最後的分數：", ref,
                       " 自己就是 ability；其他世代經過內部連結換過來；還沒有內部連結的世代是空白。",
                       "這份資料沒有 D 分數的錨題，外部連結不適用，所以沒有 d_score。"),
    ref_score_se = "ref_score 的標準誤（測量誤差，參照世代量尺的單位）。",
    se_link = "內部連結的誤差（換算本身的不確定性）；參照世代自己沒有換算，是空白。",
    se_total = "ref_score 的測量誤差與連結誤差合起來：sqrt(ref_score_se² + se_link²)。"
  )
}

#' 分數表有的欄，各一句說明
#' @param ref NULL，或外部連結不適用時的參照世代（分數表有 ref_score）
#' @return tibble（欄名英文）：column、meaning
#' @noRd
kitsui_score_columns <- function(scores, ref = NULL) {
  words <- KITSUI_SCORE_COLUMNS
  if (!is.null(ref)) {
    more <- kitsui_score_columns_ref(ref)
    words <- c(words[setdiff(names(words), c(names(more), "d_score", "d_score_se"))], more)
  }
  have <- intersect(names(words), names(scores))
  tibble::tibble(column = have, meaning = unname(words[have]))
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
#' 門檻與 age50 都照每一題的通過步驟（得分至少 step 算通過；kitsui_item_step_arg()）。
#' @param round kitsui_round() 的結果；df 是這一回合的長格式資料；fixed 是含連結的固定檔
#' @param links NULL 或 kitsui_links(fixed)；persons NULL 或 kitsui_persons(df)
#' @param steps NULL 或通過步驟（原來的題號 -> step，kitsui_steps_vec()）；NULL 時用固定檔的
#'   pass_steps
#' @noRd
kitsui_item_table <- function(round, df, fixed, scale, links = NULL, persons = NULL,
                              steps = NULL) {
  item <- round$items[[scale]]
  if (is.null(item)) kit_abort("這一回合沒有 ", scale, " 的校準結果。")
  links <- links %||% kitsui_links(fixed, kitsui_link_path(fixed, scale))
  persons <- persons %||% kitsui_persons(df)
  steps <- steps %||% kitsui_steps_vec(fixed$pass_steps)
  step <- kitsui_item_step_arg(item, steps, fixed$versions)
  ages <- kitsui_item_ages(round$data[[scale]], persons, kitsui_round_cohorts(round, scale))
  tab <- kits::kits_item_table(item, data = ages, step = step)
  attr(tab, "thresholds") <- kitsui_item_thresholds(item, fixed, links, scale, step)
  tab
}

#' 同一題的門檻換到內部連結後、外部連結後的量尺
#' @param step kits_item_table() 的 step（kitsui_item_step_arg()）
#' @return NULL 或 tibble：item_id，有內部連結時 ability_linked_threshold，換得到 D 時 d_threshold
#' @noRd
kitsui_item_thresholds <- function(item, fixed, links, scale, step = 1) {
  first <- function(path, name) {
    tab <- kits::kits_item_table(kitsui_convert_path(item, path), step = step)
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
  # 通過步驟（有題不是 1 時 kits 才給這一欄）放在題號旁邊
  tab <- dplyr::relocate(tab, dplyr::any_of("step"), .after = "item_id")
  last <- utils::tail(intersect(KITSUI_ITEM_THRESHOLDS, names(tab)), 1)
  if (length(last) == 1) tab <- tab[order(tab[[last]], tab$item_id, na.last = TRUE), , drop = FALSE]
  rownames(tab) <- NULL
  structure(tab, dropped = drop)
}

# 題目表的三種門檻（由校準的量尺到 D）
KITSUI_ITEM_THRESHOLDS <- c("ability_threshold", "ability_linked_threshold", "d_threshold")

# 通過步驟 ----
# 得分至少 step 算通過（kits_item_table(step = )）：題目表的門檻與 age50、① 的作答設計圖用。
# 設定用原來的題號；偏移後的新版本（題號_r回合）沿用原來題號的設定。沒列的題用 1。
# 校準、偏移、連結、分數都用全部的步驟，和通過步驟無關

#' 通過步驟的設定表（固定檔的 pass_steps）轉成具名整數向量：原來的題號 -> step
#' @noRd
kitsui_steps_vec <- function(tab) {
  if (NROW(tab) == 0) return(stats::setNames(integer(), character()))
  tab <- dplyr::slice_tail(tibble::as_tibble(tab), n = 1, by = "item_id")
  stats::setNames(as.integer(tab$step), as.character(tab$item_id))
}

#' 目前的題號換回原來的題號（versions 的 current_id -> item_id）；不是新版本的題不變
#' @noRd
kitsui_base_ids <- function(ids, versions) {
  if (NROW(versions) == 0) return(ids)
  dplyr::coalesce(versions$item_id[match(ids, versions$current_id)], ids)
}

#' 這一回合校準的每一題：量尺、目前的題號、原來的題號、校準裡最高的分數
#' @param items round$items（具名 list，量尺 -> kits_item）
#' @return tibble：scale、current_id、item_id、top
#' @noRd
kitsui_round_item_steps <- function(items, versions = NULL) {
  none <- tibble::tibble(scale = character(), current_id = character(), item_id = character(),
                         top = integer())
  out <- purrr::imap(items, \(item, s) {
    co <- stats::coef(item)
    top <- tapply(co$item_score, as.character(co$item_id), max)
    tibble::tibble(scale = s, current_id = names(top), top = as.integer(top))
  }) |>
    purrr::list_rbind()
  if (NROW(out) == 0) return(none)
  out |>
    dplyr::mutate(item_id = kitsui_base_ids(.data$current_id, versions), .after = "current_id")
}

#' 一個量尺 kits_item_table(step = ) 的值
#'
#' 目前的題號當名字，只列不是 1 的題；每一題都是 1 時是 1。
#' 校準裡沒有這個分數的題用 1（上傳時已經擋下來，這裡只是保險）。
#' @param steps 原來的題號 -> step（kitsui_steps_vec()）
#' @noRd
kitsui_item_step_arg <- function(item, steps, versions = NULL) {
  if (length(steps) == 0) return(1)
  co <- stats::coef(item)
  ids <- unique(as.character(co$item_id))
  st <- unname(steps[kitsui_base_ids(ids, versions)])
  has <- purrr::map2_lgl(ids, st, \(i, s) !is.na(s) && any(co$item_id == i & co$item_score == s))
  ok <- has & st != 1
  if (!any(ok)) return(1)
  stats::setNames(as.integer(st[ok]), ids[ok])
}

#' 讀上傳的通過步驟（CSV）：每一欄都讀成文字，檢查在 kitsui_steps_check()
#' @noRd
kitsui_read_steps <- function(path) {
  out <- tryCatch(
    readr::read_csv(path, col_types = readr::cols(.default = "c"), na = c("", "NA"),
                    lazy = FALSE, progress = FALSE),
    error = \(e) NULL
  )
  if (is.null(out)) kit_abort("讀不進這個檔案。請上傳 CSV 檔（逗號分隔）。")
  tibble::as_tibble(out)
}

#' 上傳的通過步驟：檢查並整理
#'
#' 擋下來（errors）：缺 item_id 或 step 欄、step 不是 1 以上的整數、同一題列了兩次而 step 不一樣、
#' 校準裡沒有這麼高的分數。只提醒（warnings）：這一回合的校準裡沒有的題（設定照樣存著）。
#' 題號可以用新版本的題號（例如 lana02_r12），會換回原來的題號。
#' @param tab kitsui_read_steps() 的結果
#' @param known kitsui_round_item_steps()
#' @return list(steps, errors, warnings)：steps 是原來的題號 -> step（有 errors 時 NULL）
#' @noRd
kitsui_steps_check <- function(tab, known) {
  fail <- \(...) list(steps = NULL, errors = c(...), warnings = character())
  miss <- setdiff(c("item_id", "step"), names(tab))
  if (length(miss) > 0) {
    return(fail(paste0("檔案要有 item_id 與 step 兩欄。這個檔案缺少：",
                       paste(miss, collapse = "、"), "。")))
  }
  id <- trimws(as.character(tab$item_id))
  raw <- trimws(as.character(tab$step))
  keep <- !is.na(id) & nzchar(id)
  id <- id[keep]
  raw <- raw[keep]
  if (length(id) == 0) return(fail("檔案裡沒有任何一題。"))
  id <- dplyr::coalesce(known$item_id[match(id, known$current_id)], id)
  num <- suppressWarnings(as.numeric(raw))
  bad <- is.na(num) | num < 1 | num != round(num)
  shown <- ifelse(is.na(raw) | raw == "", "空白", raw)
  errors <- c(
    if (any(bad)) {
      paste0("step 要是 1 以上的整數。這幾題不是：",
             kitsui_some(paste0(id[bad], "（", shown[bad], "）")), "。")
    },
    kitsui_steps_twice(id[!bad], num[!bad])
  )
  ok <- tibble::tibble(item_id = id[!bad], step = as.integer(num[!bad])) |>
    dplyr::distinct()
  tops <- dplyr::summarise(known, top = min(.data$top), .by = "item_id")
  over <- dplyr::inner_join(ok, tops, by = "item_id") |>
    dplyr::filter(.data$step > .data$top)
  if (nrow(over) > 0) {
    errors <- c(errors, paste0(
      "這幾題在校準裡沒有這麼高的分數：",
      kitsui_some(sprintf("%s（設定 %d，最高 %d 分）", over$item_id, over$step, over$top)), "。"
    ))
  }
  if (length(errors) > 0) return(fail(errors))
  unknown <- setdiff(ok$item_id, known$item_id)
  warnings <- if (length(unknown) > 0) {
    c(paste0("這一回合的校準裡沒有這幾題：", kitsui_some(unknown), "。"),
      "這些設定照樣存進固定檔，之後的回合用得到。",
      "如果是打錯了，請改好再上傳。")
  }
  list(steps = stats::setNames(ok$step, ok$item_id), errors = character(),
       warnings = warnings %||% character())
}

# 同一題列了兩次以上而 step 不一樣
kitsui_steps_twice <- function(id, step) {
  n <- tapply(step, id, \(x) length(unique(x)))
  twice <- names(n)[n > 1]
  if (length(twice) == 0) return(NULL)
  paste0("同一題列了兩次以上，step 不一樣：", kitsui_some(twice), "。每一題只列一次。")
}

# 列出前幾個，太多時加「等 n 題」
kitsui_some <- function(x, n = 10) {
  more <- if (length(x) > n) paste0(" 等 ", length(x), " 題")
  paste0(paste(utils::head(x, n), collapse = "、"), more)
}

#' 套用新的設定：檔案裡列的題換成新的 step，沒列的題不變；只留不是 1 的題
#' @noRd
kitsui_steps_merge <- function(steps, new) {
  steps[names(new)] <- new
  steps <- steps[steps != 1]
  stats::setNames(as.integer(steps), names(steps) %||% character())
}

#' 範本：這一回合的每一題（原來的題號）與目前的 step；scale、max_score 只是參考
#' @noRd
kitsui_steps_template <- function(known, steps) {
  known |>
    dplyr::summarise(scale = paste(unique(.data$scale), collapse = "; "),
                     max_score = min(.data$top), .by = "item_id") |>
    dplyr::mutate(step = dplyr::coalesce(unname(steps[.data$item_id]), 1L), .after = "item_id") |>
    dplyr::arrange(.data$scale, .data$item_id)
}

#' 目前不是 1 的題：這一回合有的題列出量尺與最高分數；沒有的題 scale 是 NA
#' @return tibble：item_id、step、scale、max_score（這一回合的題在前，依題號排）
#' @noRd
kitsui_steps_shown <- function(known, steps) {
  scales <- dplyr::summarise(known, scale = paste(unique(.data$scale), collapse = "、"),
                             max_score = min(.data$top), .by = "item_id")
  tibble::tibble(item_id = names(steps), step = unname(steps)) |>
    dplyr::filter(.data$step != 1) |>
    dplyr::left_join(scales, by = "item_id") |>
    dplyr::arrange(is.na(.data$scale), .data$item_id)
}

#' 收合區塊的標題：每題都是 1，或哪幾題不是 1（只算這一回合的題，最多列 3 題）
#' @param shown kitsui_steps_shown()
#' @noRd
kitsui_steps_summary <- function(shown) {
  here <- shown[!is.na(shown$scale), ]
  if (nrow(here) == 0) return("通過步驟（每題都是 1）")
  some <- paste0(here$item_id, "＝", here$step)
  more <- if (length(some) > 3) "、…"
  paste0("通過步驟（", nrow(here), " 題不是 1：", paste(utils::head(some, 3), collapse = "、"),
         more, "）")
}

#' 選題目的清單：這一回合的每一題（原來的題號），依量尺分組；標籤帶目前的 step 與最高分數
#' @return 具名 list：量尺 -> 具名文字向量（標籤 -> 原來的題號）
#' @noRd
kitsui_steps_choices <- function(known, steps) {
  tpl <- kitsui_steps_template(known, steps)
  if (nrow(tpl) == 0) return(list())
  label <- sprintf("%s（目前 %d，最高 %d 分）", tpl$item_id, tpl$step, tpl$max_score)
  split(stats::setNames(tpl$item_id, label), tpl$scale)
}

#' 在畫面上套用：選的題都設成同一個 step
#'
#' 要選了題、step 是 1 以上的整數；其餘的檢查（最高分數）和上傳的設定檔相同。
#' @param ids 選的題（原來的題號）
#' @param value 通過步驟（numericInput 的值）
#' @return list(steps, errors)：steps 是原來的題號 -> step（有 errors 時 NULL）
#' @noRd
kitsui_steps_pick <- function(ids, value, known) {
  fail <- \(...) list(steps = NULL, errors = c(...))
  if (length(ids) == 0) return(fail("請先選題目。"))
  num <- suppressWarnings(as.numeric(value))
  if (length(num) != 1 || is.na(num)) return(fail("通過步驟是空白。請填 1 以上的整數。"))
  if (num < 1 || num != round(num)) {
    return(fail(paste0("通過步驟要是 1 以上的整數。現在是 ", format(num), "。")))
  }
  tab <- tibble::tibble(item_id = ids, step = as.character(num))
  kitsui_steps_check(tab, known)[c("steps", "errors")]
}

#' 新固定檔的 pass_steps：一題一列，最新的設定
#'
#' 這一回合新設或改了的題，round 是這一回合；沒改的題留原來的回合。
#' 原來列著、這一回合改回 1 的題留一列 step = 1。都沒有改時固定檔原樣回傳。
#' @param steps 原來的題號 -> step（kitsui_steps_vec()；沒列的題是 1）
#' @noRd
kitsui_fixed_pass_steps <- function(fixed, steps, round_label) {
  old <- fixed$pass_steps %||% kitsui_fixed_empty()$pass_steps
  old <- dplyr::slice_tail(old, n = 1, by = "item_id")
  ids <- union(old$item_id, names(steps))
  now <- dplyr::coalesce(unname(steps[ids]), 1L)
  at <- match(ids, old$item_id)
  same <- !is.na(at) & dplyr::coalesce(old$step[at] == now, FALSE)
  if (all(same) && length(ids) == nrow(fixed$pass_steps %||% old)) return(fixed)
  keep <- !is.na(at) | now != 1
  fixed$pass_steps <- tibble::tibble(
    item_id = ids, step = as.integer(now),
    round = ifelse(same, old$round[at], round_label %||% NA_character_)
  )[keep, ]
  fixed
}

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

# 垂直連結（kitsui_verticals() 等）在 pipeline_vertical.R

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

#' 固定檔的說明（README.txt 的內容）：英文（純 ASCII），解壓縮後不會是亂碼
#'
#' 給寫固定檔的函式放進 zip（kitsUI 讀固定檔時不看它）。
#' 量尺、回合、連結的名稱是使用者給的，照原樣放（可能有中文）。
#' @return 一個字串（多行）
#' @noRd
kitsui_fixed_readme <- function(fixed) {
  v <- fixed$version
  links <- kitsui_link_names(fixed)
  both <- \(x) if (length(x) == 0) "(none yet)" else paste(x, collapse = ", ")
  paste(c(
    "kitsUI fixed file",
    "",
    "This zip is a kitsUI fixed file. It holds the item bank, item versions and links",
    "of every round up to this one (each fixed file includes all earlier rounds).",
    "In the next round, upload this zip in kitsUI tab 1 (Data) to keep using",
    "the same item bank and links.",
    "Do not modify the files in it or re-zip them.",
    "A fixed file is never changed after it is written; each round makes a new one.",
    "",
    paste0("Rounds: ", both(v$rounds)),
    paste0("Reference cohort: ", v$reference %||% ""),
    paste0("Scales: ", both(v$scales)),
    paste0("Links: ", both(links)),
    paste0("kits version: ", v$kits %||% "", "; kitsUI version: ", v$kitsUI %||% ""),
    "",
    "Files:",
    paste0("VERSION: versions, date created, round names, reference cohort, scales,",
           " and the md5 of each round's data file."),
    paste0("bank.csv: item bank. Beta and its SE for each step of each item in each scale,",
           " and the round that added it."),
    "versions.csv: item versions. Original and current item IDs of drifted items.",
    "waves.csv: the waves (age in months) each item bank was calibrated on.",
    "links.csv: link points and settings. The links can be rebuilt exactly from them.",
    "checks.csv: check log of each round (drift, new versions, flags, borrowed banks, etc.).",
    paste0("wave_summary.csv: per scale x cohort x wave, n and the P25, median and P75 of",
           " ability on the calibrated scale (aggregates only, no individual data).",
           " Used for the vertical plot."),
    paste0("pass_steps.csv: pass step of each item (a score >= step passes);",
           " items not listed use 1. Used for the item table and the design plot."),
    "",
    "Reminders:",
    "Old betas never change.",
    "A link is frozen once made; every later round uses the same one.",
    "Scores of earlier rounds are not recomputed.",
    "Download and keep each round's scores in that round."
  ), collapse = "\n")
}
