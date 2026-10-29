# 一輪（一波或多波新資料）：每個量尺
#   沒有題庫 -> 自由校準；有題庫 -> 改成目前版本、併掉缺口、偏移把關、新版本、固定校準
#   用哪一個題庫（自己的或借另一個世代的）見 pipeline_bank.R
# 舊的 beta 永遠不動；新的步驟才加進題庫
# 分數類別有問題的題（pipeline_categories.R）不改分數：題庫裡的題固定在題庫的值，
#   不在題庫的題這一輪不放進來

# 寫進 checks.csv 的檢查
KITSUI_CHECKS <- c("drift", "new version", "capped", "flag", "stop", "error", "D extended",
                   "borrowed bank", "bank choice", "no CML estimate, kept fixed",
                   "category problem, kept fixed", "category problem, left out")

#' 跑一輪
#'
#' 已有外部連結時，這一輪新進題庫的參照世代量尺沿用同一條外部連結（kitsui_link_D_extend()）。
#' @param df 這一輪的長格式資料（可含多個量尺）
#' @param fixed 上一輪的固定檔（第一輪用 kitsui_fixed_empty()）
#' @param round_label 這一輪的名稱（只能有英文字母與數字，例如 "12"）；新版本題號加上 "_r" 與它
#' @param flag_share,flag_fixed flag（不停下來）：偏移題多於 max(1, flag_share × 已判斷題)，
#'   或可固定的題少於 flag_fixed 時，記下 "flag"（status warn），照常換新版本、固定校準、
#'   更新題庫與產生固定檔。只有技術上做不了時才停下來：kits 出錯，沒有可固定的題
#'   （量尺放不上題庫的尺），或拿掉分數類別有問題的題以後沒有題可以估計
#' @param files 這一輪資料檔的路徑（記下 md5）；NULL 時記資料本身的 md5
#' @param bank_choice NULL（照規則）或具名字元向量：資料量尺 -> 用哪一個量尺的題庫，
#'   例如 c("motor M03" = "motor M36")（見 kitsui_round_plan()）
#' @param progress NULL 或 function(i, n, scale)：每個量尺開始前呼叫一次（第 i 個，共 n 個），
#'   例如在 app 裡更新進度條
#' @return list(items, drift, data, fixed, stopped, ok, log, plan, memo)：items、data（改名、
#'   併過的矩陣）只有沒停下來的量尺；kits 出錯的量尺也算停下來（check = "error"）。fixed 是
#'   更新後的固定檔（還沒寫出）；有量尺停下來時 ok = FALSE，fixed 帶 attr(, "stopped")，
#'   不能寫出。各量尺以校準的量尺命名（借題庫時是另一個世代的量尺）；plan 是
#'   kitsui_round_plan()；memo 是存算過的東西的 environment（kitsui_own_scores()）
#' @noRd
kitsui_round <- function(df, fixed, round_label, flag_share = 0.2, flag_fixed = 5, files = NULL,
                         progress = NULL, bank_choice = NULL) {
  kitsui_check_data(df)
  problem <- kitsui_label_problem(round_label, fixed$version$rounds)
  if (length(problem) > 0) kit_abort(problem)
  plan <- kitsui_round_plan(df, fixed, bank_choice)
  # 資料只切一次（領域 × 世代 × 波次），每個量尺拿它要的幾塊
  pieces <- vctrs::vec_split(df, df[c("domain", "cohort", "wave_m")])
  scales <- unique(plan$scale)
  n <- length(scales)
  res <- purrr::map(seq_len(n), \(i) {
    if (!is.null(progress)) progress(i, n, scales[i])
    part <- plan[plan$scale == scales[i], ]
    kitsui_round_try(kitsui_round_rows(pieces, part), part, fixed, round_label, flag_share,
                     flag_fixed)
  }) |>
    purrr::set_names(scales)
  ok <- purrr::discard(res, "stopped")
  log <- purrr::list_rbind(purrr::map(res, "log"))
  stopped <- names(res)[purrr::map_lgl(res, "stopped")]
  fixed <- kitsui_fixed_update(fixed, ok, log, kitsui_sources(round_label, df, files))
  # 新的參照世代量尺：已凍結的外部連結延伸過去（同一條線）
  ext <- kitsui_link_D_extend(fixed, round_label)
  fixed <- ext$fixed
  log <- dplyr::bind_rows(log, ext$log)
  # 有量尺停下來時，這一輪不產生新的固定檔
  if (length(stopped) > 0) attr(fixed, "stopped") <- stopped
  list(
    items = purrr::map(ok, "item"), drift = purrr::compact(purrr::map(res, "drift")),
    data = purrr::map(ok, "X"), fixed = fixed, stopped = stopped,
    ok = length(stopped) == 0, log = log, plan = plan, memo = new.env(parent = emptyenv())
  )
}

#' 一個量尺要的列：plan 裡這個量尺的資料量尺與波次（vctrs::vec_split() 切好的塊）
#' @param pieces vctrs::vec_split() 依 domain、cohort、wave_m 切好的塊
#' @noRd
kitsui_round_rows <- function(pieces, part) {
  keys <- data.frame(domain = scale_domain(part$data_scale),
                     cohort = scale_cohort(part$data_scale), wave_m = part$wave_m)
  hit <- vctrs::vec_match(keys, vctrs::vec_cast(pieces$key, keys))
  vctrs::vec_rbind(!!!pieces$val[hit[!is.na(hit)]])
}

# 一個量尺的一輪；kits 的錯誤只停這個量尺，其他量尺繼續
# rows 是這個量尺的作答；part 是 kitsui_round_plan() 裡校準到這個量尺的列
# （借題庫時可能有兩個世代的資料）
kitsui_round_try <- function(rows, part, fixed, round_label, flag_share, flag_fixed) {
  scale <- part$scale[1]
  # 借題庫、手動選題庫：記進 checks.csv
  noted <- part[!is.na(part$detail), ]
  borrow <- tibble::tibble(round = round_label, scale = scale, item_id = NA_character_,
                           check = dplyr::if_else(noted$borrowed, "borrowed bank", "bank choice"),
                           detail = noted$detail)
  out <- tryCatch({
    rows <- kitsui_current_ids(rows, fixed, scale)
    X <- kitsui_round_matrix(rows, unique(part$data_scale))
    row_cohorts <- rows$cohort[match(rownames(X), rows$visit_id)]
    borrowed <- unique(scale_cohort(part$data_scale[part$borrowed]))
    kitsui_round_scale(X, scale, fixed, round_label, flag_share, flag_fixed, row_cohorts, borrowed)
  }, error = \(e) list(stopped = TRUE, log = tibble::tibble(
    round = round_label, scale = scale, item_id = NA_character_, check = "error",
    detail = kitsui_error_detail(e)
  )))
  out$log <- dplyr::bind_rows(borrow, out$log)
  out$waves <- tibble::tibble(scale = scale, wave_m = sort(unique(part$wave_m)),
                              round = round_label)
  out
}

#' 停下來的原因（中文）：kitsui 自己的訊息原文；kits 的訊息前面加一句中文
#' @noRd
kitsui_error_detail <- function(e) {
  if (inherits(e, "kit_user_error")) return(conditionMessage(e))
  paste0("kits 出錯，這個量尺停下來。kits 的訊息：", conditionMessage(e))
}

#' 一個量尺的作答矩陣（rows 已經只有這個量尺的列；兩個世代時 visit_id 不能重複）
#' @noRd
kitsui_round_matrix <- function(rows, data_scales) {
  if (length(data_scales) > 1) kitsui_check_ids(rows)
  resp <- kits::kits_resp(rows)
  X <- as.matrix(resp[attr(resp, "items")])
  rownames(X) <- resp$visit_id
  X
}

#' 題號改成目前的版本：只用同一個世代的資料做的版本（versions.csv 的 cohort）
#'
#' 例如 M03 的資料借 motor M36 的題庫時做的新版本（bodybf09 -> bodybf09_r60），只給之後 M03 的
#' 資料用；M36 的資料還是用 bodybf09（除非 M36 的資料自己做了新版本）。
#' @param df 長格式資料；scale 是校準的量尺
#' @noRd
kitsui_current_ids <- function(df, fixed, scale) {
  vers <- fixed$versions |>
    dplyr::filter(.data$scale == !!scale) |>
    dplyr::select("cohort", "item_id", "current_id")
  if (nrow(vers) == 0) return(df)
  df |>
    dplyr::left_join(vers, by = c("cohort", "item_id")) |>
    dplyr::mutate(item_id = dplyr::coalesce(.data$current_id, .data$item_id)) |>
    dplyr::select(-"current_id")
}

# 一個量尺的一輪（X 的題號已經是目前的版本）；row_cohorts 是 X 每一列的世代（新版本記在
# 有作答的世代名下），borrowed 是借這個題庫的世代（沒有借時長度 0）
kitsui_round_scale <- function(X, scale, fixed, round_label, flag_share, flag_fixed,
                               row_cohorts = rep(scale_cohort(scale), nrow(X)),
                               borrowed = character()) {
  row <- \(item_id, check, detail) tibble::tibble(round = round_label, scale = scale,
                                                  item_id = item_id, check = check, detail = detail)
  bank <- dplyr::filter(fixed$bank, .data$scale == !!scale)
  if (nrow(bank) == 0) return(kitsui_round_free(X, scale, bank, row))
  vers <- dplyr::filter(fixed$versions, .data$scale == !!scale)
  # 併掉新步驟的缺口；分數類別有問題的題：題庫裡的固定，不在題庫的拿掉
  cat <- kitsui_cap_categories(X, bank, scale)
  X <- cat$X
  log <- dplyr::bind_rows(row(cat$capped$item_id, "capped", cat$capped$detail),
                          row(cat$events$item_id, cat$events$check, cat$events$detail))

  # 偏移把關：題庫的 beta 當參照（kits_drift() 從 beta 算門檻）。
  # 這一輪估不出來的題、分數類別有問題的題（kept）不判斷偏移，固定在題庫的值
  ref <- structure(bank[c("item_id", "item_score", "beta")], scale = scale)
  dr <- kitsui_drift_safe(X, ref, cat$kept)
  X <- X[, !colnames(X) %in% dr$out, drop = FALSE]
  log <- dplyr::bind_rows(log, row(dr$events$item_id, dr$events$check, dr$events$detail))
  drift <- dr$drift
  moved <- drift$items$item_id[drift$items$drift %in% TRUE]
  judged <- sum(!is.na(drift$items$drift))
  n_fixed <- dplyr::n_distinct(drift$fix$item_id)
  resid <- drift$items$max_abs_resid[match(moved, drift$items$item_id)]
  log <- dplyr::bind_rows(log, row(moved, "drift", sprintf(
    "離連結線最遠的步驟差 %.2f logit（大於 %s 個殘差標準差）", resid, kitsui_drift_cut()
  )))
  counts <- sprintf("偏移 %d 題（已判斷 %d 題）；可固定 %d 題", length(moved), judged, n_fixed)
  # 沒有可固定的題：量尺放不上題庫的尺，停下來
  if (n_fixed < 1) {
    log <- dplyr::bind_rows(log, row(NA_character_, "stop", paste0(counts, "：沒有可固定的題")))
    return(list(stopped = TRUE, drift = drift, log = log))
  }
  # 偏移題太多或可固定的題太少：flag，照常校準
  if (length(moved) > max(1, flag_share * judged) || n_fixed < flag_fixed) {
    why <- c(if (length(moved) > max(1, flag_share * judged)) "偏移題太多",
             if (n_fixed < flag_fixed) sprintf("可固定的題少於 %s 題", flag_fixed))
    detail <- paste0(counts, "（", paste(why, collapse = "、"), "），照常校準")
    log <- dplyr::bind_rows(log, row(NA_character_, "flag", detail))
  }

  fit <- kitsui_fit_fixed(X, scale, round_label, drift, ref, moved, dr$kept, vers, row_cohorts)
  new <- fit$versions
  new_ids <- dplyr::distinct(new, .data$item_id, .data$current_id)
  log <- dplyr::bind_rows(
    log, row(fit$events$item_id, fit$events$check, fit$events$detail),
    row(new_ids$item_id, "new version", paste(fit$moved, "->", new_ids$current_id))
  )
  detail <- sprintf("固定校準：固定 %d 題", n_fixed)
  if (length(borrowed) > 0) {
    detail <- paste0(detail, "（", paste(borrowed, collapse = "、"), " 的資料借用另一世代題庫）")
  }
  log <- dplyr::bind_rows(log, row(NA_character_, "calibration", detail))
  kitsui_scale_done(fit$X, fit$item, drift, bank, new, log)
}

#' kits_drift() 的 cut（殘差大於幾個標準差算偏移；kitsui_round() 用 kits 的預設）
#' @noRd
kitsui_drift_cut <- function() eval(formals(kits::kits_drift)$cut)

#' 沒有題庫：自由校準。分數類別有問題的題這一輪不放進來；拿掉以後不到 2 題時停下來
#' @noRd
kitsui_round_free <- function(X, scale, bank, row) {
  probs <- kitsui_matrix_problems(X, scale)
  log <- row(probs$item_id, "category problem, left out",
             paste0(probs$detail_zh, KITSUI_LEFT_OUT))
  free <- kitsui_item_free(X[, !colnames(X) %in% probs$item_id, drop = FALSE], scale)
  log <- dplyr::bind_rows(log, row(free$events$item_id, free$events$check, free$events$detail))
  if (is.null(free$item)) {
    stop_why <- sprintf("拿掉分數類別有問題的題以後只剩 %d 題，沒有題可以估計", ncol(free$X))
    return(list(stopped = TRUE, log = dplyr::bind_rows(log, row(NA_character_, "stop", stop_why))))
  }
  log <- dplyr::bind_rows(log, row(NA_character_, "calibration", "自由校準"))
  kitsui_scale_done(free$X, free$item, NULL, bank, NULL, log)
}

# 寫進紀錄的說明（接在分數類別的問題後面）
KITSUI_LEFT_OUT <- "：不在題庫裡，這一輪不放進來"
KITSUI_KEPT_FIXED <- "：不判斷偏移，固定在題庫的值"
KITSUI_NO_CML <- "這一輪的資料估不出有限的 CML 估計值（有資訊的人都通過或都沒通過）"

#' 自由校準；kits 說某些題估不出來（分數類別的問題、沒有 CML 估計值）時，拿掉它們再估
#' @return list(item, X, events)：item 是 NULL 時表示剩不到 2 題
#' @noRd
kitsui_item_free <- function(X, scale) {
  events <- kitsui_events()
  repeat {
    if (ncol(X) < 2) return(list(item = NULL, X = X, events = events))
    item <- tryCatch(kits::kits_item(X, scale = scale), error = \(e) e)
    if (!inherits(item, "error")) return(list(item = item, X = X, events = events))
    bad <- kitsui_error_items(item, colnames(X))
    if (length(bad) == 0) stop(item)
    events <- dplyr::bind_rows(events, kitsui_error_events(item, bad, rep(FALSE, length(bad))))
    X <- X[, !colnames(X) %in% bad, drop = FALSE]
  }
}

#' 併分數與分數類別的檢查（兩個規則一致）
#'
#' 1. kitsui_cap()：題庫裡的題，超過題庫最高步、又不能當新步驟的分數併為能用的最高分；
#' 2. 併過以後還有分數類別問題的題（沒有 0 分、不連續、只有一種分數）：在題庫裡的不判斷
#'    偏移，固定在題庫的值，也不加新的步驟（超過題庫最高步的分數併為最高步）；
#'    不在題庫裡的這一輪不放進來（從 X 拿掉）。分數本身不改。
#' @return list(X, capped, kept, out, events)：capped 是 kitsui_cap() 的紀錄；kept 是固定的題，
#'   out 是拿掉的題，events 是要寫進紀錄的列（item_id、check、detail）
#' @noRd
kitsui_cap_categories <- function(X, bank, scale) {
  first <- kitsui_cap(X, bank)
  probs <- kitsui_matrix_problems(first$X, scale)
  in_bank <- probs$item_id %in% bank$item_id
  kept <- probs$item_id[in_bank]
  out <- probs$item_id[!in_bank]
  capped <- if (length(kept) > 0) kitsui_cap(X, bank, no_new = kept) else first
  events <- tibble::tibble(
    item_id = probs$item_id,
    check = dplyr::if_else(in_bank, "category problem, kept fixed", "category problem, left out"),
    detail = paste0(probs$detail_zh, dplyr::if_else(in_bank, KITSUI_KEPT_FIXED, KITSUI_LEFT_OUT))
  )
  list(X = capped$X[, !colnames(capped$X) %in% out, drop = FALSE], capped = capped$log,
       kept = kept, out = out, events = events)
}

#' 紀錄的列（item_id、check、detail），還沒有時 0 列
#' @noRd
kitsui_events <- function(item_id = character(), check = character(), detail = character()) {
  tibble::tibble(item_id = item_id, check = check, detail = detail)
}

#' kits 說「某些題估不出來」的錯誤裡的題號（不是這種錯誤時長度 0）
#'
#' 這種錯誤：沒有有限的 CML 估計值（"no finite CML estimate"）、沒有 0 分（"no 0 score"）、
#' 分數不連續（"missing"、"consecutive"）。
#' @param ids 可能的題號（X 的欄名）
#' @noRd
kitsui_error_items <- function(e, ids) {
  msg <- conditionMessage(e)
  if (length(kitsui_no_cml_items(e)) > 0) return(intersect(kitsui_no_cml_items(e), ids))
  if (!grepl("no 0 score|missing|consecutive", msg)) return(character())
  # 題號要整個字出現（a1 不會比對到 a12）
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", ids)
  after <- "($|[^A-Za-z0-9_.]|[.]($|[^A-Za-z0-9_]))"
  ids[purrr::map_lgl(esc, \(id) grepl(paste0("(^|[^A-Za-z0-9_.])", id, after), msg))]
}

#' 這種錯誤寫進紀錄的列：in_bank 的題固定在題庫的值，其他題這一輪不放進來
#' @noRd
kitsui_error_events <- function(e, bad, in_bank) {
  no_cml <- length(kitsui_no_cml_items(e)) > 0
  why <- if (no_cml) KITSUI_NO_CML else {
    paste0("kits 說分數類別有問題（kits 的訊息：", conditionMessage(e), "）")
  }
  kept_check <- if (no_cml) "no CML estimate, kept fixed" else "category problem, kept fixed"
  kept_how <- if (no_cml) "，不換新版本，固定在題庫的值" else KITSUI_KEPT_FIXED
  kitsui_events(
    bad, dplyr::if_else(in_bank, kept_check, "category problem, left out"),
    paste0(why, dplyr::if_else(in_bank, kept_how, KITSUI_LEFT_OUT))
  )
}

#' kits 說「沒有有限的 CML 估計值」的題號（不是這個錯誤時長度 0）
#' @noRd
kitsui_no_cml_items <- function(e) {
  msg <- conditionMessage(e)
  if (!grepl("no finite CML estimate", msg, fixed = TRUE)) return(character())
  hits <- regmatches(msg, gregexpr("[^ ,:]+:[0-9]+ \\((reached|missed) by none\\)", msg))[[1]]
  unique(sub(":.*$", "", hits))
}

#' 偏移把關；kits 說某些題估不出來時：題庫裡的題拿掉再判斷一次（之後固定在題庫的值），
#' 不在題庫的題這一輪不放進來
#' @param kept 一開始就不判斷的題（分數類別有問題的題庫題）
#' @return list(drift, kept, out, events)：kept 是固定在題庫的值的題（含一開始的），
#'   out 是拿掉的題，events 是這裡新加的紀錄
#' @noRd
kitsui_drift_safe <- function(X, ref, kept = character()) {
  out <- character()
  events <- kitsui_events()
  repeat {
    drift <- tryCatch(kits::kits_drift(X[, !colnames(X) %in% c(kept, out), drop = FALSE], ref),
                      error = \(e) e)
    if (!inherits(drift, "error")) {
      return(list(drift = drift, kept = kept, out = out, events = events))
    }
    bad <- setdiff(kitsui_error_items(drift, colnames(X)), c(kept, out))
    if (length(bad) == 0) stop(drift)
    in_bank <- bad %in% ref$item_id
    events <- dplyr::bind_rows(events, kitsui_error_events(drift, bad, in_bank))
    kept <- c(kept, bad[in_bank])
    out <- c(out, bad[!in_bank])
  }
}

#' 固定校準：偏移的題換成新版本（原來的題號 + "_r" + 輪；再偏移時也從原來的題號做，
#' 例如 socb13_r72，不會是 socb13_r60_r72）；kits 說新版本或題庫的題估不出來時，
#' 不換新版本，固定在題庫的值；不在題庫的題估不出來時，這一輪不放進來；再校準一次
#' @param vers 這個量尺的 versions（各世代）；row_cohorts 是 X 每一列的世代
#' @return list(X, item, versions, moved, kept, events)：versions 是新版本（scale、item_id、
#'   current_id、cohort；這一題有作答的每個世代一列），moved 是換了新版本的題（換之前的
#'   題號），kept 是固定在題庫的值的題，events 是這裡新加的紀錄
#' @noRd
kitsui_fit_fixed <- function(X, scale, round_label, drift, ref, moved, kept, vers, row_cohorts) {
  events <- kitsui_events()
  repeat {
    base <- dplyr::coalesce(vers$item_id[match(moved, vers$current_id)], moved)
    new_id <- paste0(base, "_r", round_label)
    Xn <- X
    colnames(Xn)[match(moved, colnames(Xn))] <- new_id
    fix <- dplyr::bind_rows(as.data.frame(drift$fix), as.data.frame(ref[ref$item_id %in% kept, ]))
    item <- tryCatch(kits::kits_item(Xn, fixed_params = fix, scale = scale), error = \(e) e)
    if (!inherits(item, "error")) break
    bad <- kitsui_error_items(item, colnames(Xn))
    back <- setdiff(c(moved[new_id %in% bad], intersect(bad, ref$item_id)), kept)
    gone <- setdiff(bad, c(new_id, ref$item_id))
    if (length(back) == 0 && length(gone) == 0) stop(item)
    events <- dplyr::bind_rows(
      events, kitsui_error_events(item, c(back, gone), rep(c(TRUE, FALSE), c(length(back),
                                                                             length(gone))))
    )
    kept <- c(kept, back)
    moved <- setdiff(moved, back)
    X <- X[, !colnames(X) %in% gone, drop = FALSE]
  }
  # 新版本記在這一題有作答的世代名下
  versions <- purrr::pmap(list(moved, base, new_id), \(m, b, n) {
    co <- sort(unique(row_cohorts[!is.na(X[, m])]))
    tibble::tibble(scale = scale, item_id = b, current_id = n, cohort = co)
  }) |>
    purrr::list_rbind()
  if (length(moved) == 0) versions <- dplyr::slice(vers, 0)
  list(X = Xn, item = item, versions = versions, moved = moved, kept = kept, events = events)
}

# 一個量尺完成：題庫只加新的步驟（題號與分數都沒有的）
kitsui_scale_done <- function(X, item, drift, bank, new_vers, log) {
  added <- tibble::as_tibble(stats::coef(item)) |>
    dplyr::anti_join(bank, by = c("item_id", "item_score")) |>
    dplyr::transmute(scale = item$scale, .data$item_id, item_score = as.integer(.data$item_score),
                     .data$beta, .data$beta_se, round = log$round[1])
  list(stopped = FALSE, X = X, item = item, drift = drift, added = added, versions = new_vers,
       log = log)
}

#' 新步驟的缺口：分數要從題庫最高步往上連續；超過連續最高分的併為那個分數
#'
#' 連續是指每一個分數都有人得到：題庫最高步是 1 分、資料有 0、2 分（沒有人得 1 分）時，
#' 2 分不能當新步驟，併為 1 分。
#' @param no_new 不加新步驟的題（分數類別有問題、固定在題庫的值）：超過題庫最高步的分數
#'   都併為最高步
#' @return list(X, log)：log 每題一列（item_id、detail）
#' @noRd
kitsui_cap <- function(X, bank, no_new = character()) {
  top <- tapply(bank$item_score, bank$item_id, max)
  items <- intersect(colnames(X), names(top))
  n <- k <- purrr::set_names(integer(length(items)), items)
  for (it in items) {
    seen <- unique(stats::na.omit(X[, it]))
    k[it] <- top[[it]]
    grow <- !it %in% no_new
    while (grow && (k[it] + 1) %in% seen && k[it] %in% seen) k[it] <- k[it] + 1
    over <- which(X[, it] > k[it])
    n[it] <- length(over)
    X[over, it] <- k[it]
  }
  hit <- n > 0
  detail <- sprintf("%d 列次超過 %d 分，併為 %d 分", n[hit], k[hit], k[hit])
  why <- dplyr::if_else(items[hit] %in% no_new, "（分數類別有問題，不加新的步驟）", "")
  list(X = X, log = tibble::tibble(item_id = items[hit], detail = paste0(detail, why)))
}

# 把這一輪沒停下來的量尺寫進固定檔
kitsui_fixed_update <- function(fixed, ok, log, sources) {
  stack <- \(x, name) dplyr::bind_rows(x, purrr::list_rbind(purrr::map(ok, name)))
  # 同一題、同一個世代有新版本時，只留最新的一列
  fixed$versions <- stack(fixed$versions, "versions") |>
    dplyr::slice_tail(n = 1, by = c("scale", "item_id", "cohort"))
  fixed$bank <- stack(fixed$bank, "added")
  # 各題庫校準過的波次（借題庫時記在借出的題庫）：只加還沒有的
  waves <- purrr::list_rbind(purrr::map(ok, "waves"))
  if (NROW(waves) > 0) {
    waves <- dplyr::anti_join(waves, fixed$waves, by = c("scale", "wave_m"))
    fixed$waves <- dplyr::bind_rows(fixed$waves, waves)
  }
  checks <- dplyr::filter(log, .data$check %in% KITSUI_CHECKS)
  fixed$checks <- dplyr::bind_rows(fixed$checks, checks[names(KITSUI_FIXED_COLS$checks)])
  fixed$sources <- dplyr::bind_rows(fixed$sources, sources)
  fixed$version <- utils::modifyList(fixed$version, list(
    kits = as.character(utils::packageVersion("kits")), kitsUI = kitsui_version(),
    created = as.character(Sys.Date()), rounds = c(fixed$version$rounds, sources$round[1]),
    scales = union(fixed$version$scales, names(ok))
  ))
  fixed
}
