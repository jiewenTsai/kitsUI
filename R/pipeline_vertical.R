# 垂直連結（③ 的第 3 段）：各世代、各波次的中位數與 P25、P75，連結前後三個版本
# 這一回合的點：這一回合的分數（kits_vertical()）
# 之前回合的點：固定檔的 wave_summary（每一回合校準時存下的摘要，校準的量尺上），照現在的連結換算
# 連結是斜率為正的直線，所以 P25、中位數、P75 換算後仍然是 P25、中位數、P75

# 垂直連結的三個版本（③ 的分頁標籤）
KITSUI_VERTICAL_STAGES <- c(none = "未連結", internal = "內部連結後", external = "外部連結後")

# 摘要從這一版開始存進固定檔（舊的固定檔沒有時的說明用）
KITSUI_SUMMARY_SINCE <- "0.3.8"

# 摘要 ----

#' 一個量尺的各世代、各波次的能力值摘要（wave_summary 的列）
#'
#' 和 kits_vertical() 同一個算法：拿掉 theta 不是有限值與沒有波次的列，
#' P25、P75 是 quantile() 的預設（type 7），中位數是 median()；age_med 是實際月齡的中位數
#' （沒有 age_m 時 NA）。只有摘要，沒有個人資料。
#' @param own 校準的量尺上的 kits_subj（還沒換算）；scale 是校準的量尺
#' @return tibble：scale、cohort、wave_m、n、p25、p50、p75、age_med、round
#' @noRd
kitsui_wave_summary <- function(own, scale, round_label = NA_character_) {
  d <- tibble::as_tibble(own)
  if (!"wave_m" %in% names(d)) return(kitsui_fixed_empty()$wave_summary)
  if (!"cohort" %in% names(d)) d$cohort <- scale_cohort(scale)
  if (!"age_m" %in% names(d)) d$age_m <- NA_real_
  q <- \(x, p) stats::quantile(x, p, names = FALSE)
  d |>
    dplyr::filter(is.finite(.data$theta), !is.na(.data$wave_m)) |>
    dplyr::mutate(cohort = as.character(.data$cohort)) |>
    dplyr::summarise(n = dplyr::n(), p25 = q(.data$theta, 0.25), p50 = stats::median(.data$theta),
                     p75 = q(.data$theta, 0.75), age_med = stats::median(.data$age_m, na.rm = TRUE),
                     .by = c("cohort", "wave_m")) |>
    dplyr::mutate(scale = scale, round = round_label) |>
    dplyr::arrange(.data$cohort, .data$wave_m) |>
    dplyr::select(dplyr::all_of(names(KITSUI_FIXED_COLS$wave_summary)))
}

#' 這一回合各量尺的摘要（kitsui_round() 用）
#'
#' 分數用 kitsui_own_scores() 算，存在 memo：之後 ②、③、④ 直接拿，不重算。
#' 一個量尺算不出分數時，那個量尺沒有摘要（不影響這一回合）。
#' @param ok 沒停下來的量尺（kitsui_round_try() 的結果，有 item 與 X）
#' @noRd
kitsui_round_summary <- function(ok, persons, round_label, memo) {
  round <- list(items = purrr::map(ok, "item"), data = purrr::map(ok, "X"), memo = memo)
  purrr::map(names(ok), \(s) {
    own <- tryCatch(kitsui_own_scores(round, s, persons), error = \(e) NULL)
    if (!is.null(own)) kitsui_wave_summary(own, s, round_label)
  }) |>
    purrr::list_rbind()
}

#' 之前回合的摘要：這個領域、不是這一回合的列；同一個量尺 × 世代 × 波次只留最新的回合
#' @param label 這一回合的名稱
#' @noRd
kitsui_vertical_earlier <- function(fixed, domain, label) {
  ws <- fixed$wave_summary
  if (NROW(ws) == 0) return(kitsui_fixed_empty()$wave_summary)
  order <- match(ws$round, fixed$version$rounds)
  ws[order(dplyr::coalesce(order, 0L)), ] |>
    dplyr::filter(scale_domain(.data$scale) == domain, !.data$round %in% label) |>
    dplyr::slice_tail(n = 1, by = c("scale", "cohort", "wave_m"))
}

#' 摘要的 P25、中位數、P75 照現在的連結換算（和 kits_convert() 同一個計算）
#' @param ws kitsui_vertical_earlier() 的列
#' @return ws 換算後；attr(, "ends") 是各列換到的量尺
#' @noRd
kitsui_summary_convert <- function(ws, fixed, links, stage = "external") {
  if (nrow(ws) == 0) return(structure(ws, ends = character()))
  parts <- purrr::map(split(ws, ws$scale), \(p) {
    s <- p$scale[1]
    x <- structure(c(p$p25, p$p50, p$p75), scale = s)
    y <- kitsui_convert_path(x, kitsui_path_links(fixed, links, s, stage))
    q <- matrix(as.vector(y), ncol = 3)
    # 斜率為正時順序不變；保險起見 P25、P75 取小的與大的
    p$p25 <- pmin(q[, 1], q[, 3])
    p$p50 <- q[, 2]
    p$p75 <- pmax(q[, 1], q[, 3])
    p$end <- attr(y, "scale") %||% s
    p
  })
  out <- dplyr::bind_rows(unname(parts))
  structure(dplyr::select(out, -"end"), ends = out$end)
}

# 領域 ----

#' 這一回合可以畫垂直連結的領域
#'
#' 這一回合有兩個以上量尺（世代）的領域，或固定檔裡有這個領域之前回合的摘要。
#' @param fixed 這一回合的固定檔（預設是 round$fixed）
#' @noRd
kitsui_vertical_domains <- function(round, fixed = round$fixed) {
  domains <- scale_domain(names(round$items))
  label <- utils::tail(fixed$version$rounds, 1)
  ws <- fixed$wave_summary
  before <- if (NROW(ws) > 0) scale_domain(ws$scale[!ws$round %in% label])
  sort(unique(c(domains[duplicated(domains)], intersect(domains, before))))
}

#' 沒有可以畫的領域（或這個領域畫不了）時的一句說明
#'
#' 第一個回合、舊的固定檔（沒有摘要）、固定檔裡沒有這些領域的摘要，各說一句。
#' @noRd
kitsui_vertical_alone <- function(fixed) {
  label <- utils::tail(fixed$version$rounds, 1)
  ws <- fixed$wave_summary
  head <- "這一回合每個領域只有一個世代。"
  if (length(setdiff(fixed$version$rounds, label)) == 0) {
    return(paste0(head, "這是第一個回合，固定檔裡還沒有之前回合的點，所以還沒有垂直連結。"))
  }
  if (NROW(ws) == 0 || all(ws$round %in% label)) {
    return(paste0(
      head, "上傳的固定檔裡沒有之前回合的各波次摘要（wave_summary.csv），所以畫不出之前回合的點。",
      "各波次摘要從 kitsUI ", KITSUI_SUMMARY_SINCE, " 開始存進固定檔。",
      "用新版本重跑之前的回合，產生新的固定檔，這裡就會畫出之前回合的點。"
    ))
  }
  paste0(head, "固定檔裡也沒有這些領域之前回合的點，所以沒有垂直連結。")
}

# 三個版本 ----

#' 一個領域各量尺校準量尺上的分數（只跟這一回合與領域有關，和連結無關）
#' @param progress NULL 或 function(i, n, scale)：每個量尺開始前呼叫一次
#' @return 具名 list（量尺）：kits_subj；這一回合沒有這個領域時是 NULL
#' @noRd
kitsui_vertical_scores <- function(round, domain, persons, progress = NULL) {
  scales <- names(round$items)[scale_domain(names(round$items)) == domain]
  if (length(scales) == 0) return(NULL)
  kitsui_score_own(round, persons, progress, scales)
}

#' 一個領域：連結前後三個版本的垂直連結
#'
#' 這一回合的分數只算一次（kitsui_vertical_scores()），再依版本換算：
#' - none（未連結）：各世代留在自己校準的量尺上。kits 的 rbind() 不讓不同量尺的分數合在一起，
#'   所以去掉量尺的標記，當成沒有量尺的分數交給 kits_vertical()（y 軸不標量尺）。
#' - internal（內部連結後）：M36 用內部連結換到參照世代的量尺，兩個世代 rbind()。
#' - external（外部連結後）：再用外部連結換到 D（和 kitsui_score() 的換算相同）。
#' 之前回合的點（固定檔的摘要）照同樣的連結換算，和這一回合的點畫在一起。
#' @param links NULL 或 kitsui_links(fixed)；persons NULL 或 kitsui_persons(df)
#' @param raw NULL 或 kitsui_vertical_scores() 的結果（已經算好時傳進來，連結變了不用重算）
#' @return 具名 list（none、internal、external），各是
#'   list(vertical, table, scale, from, scales, why, note)：vertical 是這一回合的 kits_vertical()；
#'   table 是畫圖的列（這一回合與之前回合，source 是 "this" 或 "earlier"，round 是回合）；
#'   scale 是畫出來的量尺（未連結時是 NA）；from 是這一回合這個領域的量尺，scales 再加上之前回合
#'   的量尺；畫不了時 vertical 是 NULL，why 是一句原因；note 是兩個世代沒有共同月齡時的一句說明
#' @noRd
kitsui_verticals <- function(round, df, fixed, domain, links = NULL, persons = NULL,
                             raw = NULL) {
  scales <- names(round$items)[scale_domain(names(round$items)) == domain]
  label <- utils::tail(fixed$version$rounds, 1)
  earlier <- kitsui_vertical_earlier(fixed, domain, label)
  if (length(scales) == 0 || (length(scales) < 2 && nrow(earlier) == 0)) {
    why <- kitsui_vertical_alone(fixed)
    return(purrr::map(KITSUI_VERTICAL_STAGES, \(x) kitsui_vertical_none(scales, why)))
  }
  raw <- raw %||% kitsui_vertical_scores(round, domain, persons %||% kitsui_persons(df))
  links <- links %||% kitsui_links(fixed)
  # 這一回合也校準的量尺 × 世代 × 波次：用這一回合的點
  now <- purrr::imap(raw, \(x, s) kitsui_wave_summary(x, s)) |>
    purrr::list_rbind()
  earlier <- dplyr::anti_join(earlier, now, by = c("scale", "cohort", "wave_m"))
  out <- list(
    none = kitsui_vertical_raw(raw, earlier, label),
    internal = kitsui_vertical_linked(raw, earlier, fixed, links, "internal", label),
    external = kitsui_vertical_linked(raw, earlier, fixed, links, "external", label)
  )
  note <- kitsui_vertical_no_common(raw)
  every <- union(scales, earlier$scale)
  purrr::map(out, \(x) utils::modifyList(x, list(from = scales, scales = every, note = note)))
}

# 畫不了的版本
kitsui_vertical_none <- function(scales, why) {
  list(vertical = NULL, table = NULL, scale = NULL, from = scales, scales = scales, why = why)
}

# 未連結：去掉量尺的標記（各世代的量尺不同），kits_vertical() 照樣算各波次的中位數；
# 之前回合的點也留在各自校準的量尺上
kitsui_vertical_raw <- function(raw, earlier, label) {
  # 借用另一世代題庫的波次在別的量尺上：另成一條線，不和自己量尺的波次連在一起
  s <- purrr::imap(raw, \(x, sc) {
    tibble::as_tibble(x) |>
      dplyr::mutate(cohort = kitsui_vertical_series(.data$cohort, sc))
  }) |>
    unname() |>
    dplyr::bind_rows()
  attr(s, "scale") <- NULL
  v <- kits::kits_vertical(s)
  earlier$cohort <- kitsui_vertical_series(earlier$cohort, earlier$scale)
  list(vertical = v, table = kitsui_vertical_table(v, earlier, label), scale = NA_character_,
       why = NULL)
}

#' 未連結圖的一條線：世代；用另一世代的題庫校準的波次寫成「M03 (motor M36 bank)」（圖上用英文）
#' @noRd
kitsui_vertical_series <- function(cohort, scale) {
  if (length(cohort) == 0) return(as.character(cohort))
  own <- scale_cohort(scale) == cohort
  ifelse(own, cohort, paste0(cohort, " (", scale, " bank)"))
}

# 這條線是借用另一世代題庫的波次嗎（kitsui_vertical_series() 寫的名字）
kitsui_vertical_borrowed <- function(series) grepl(" bank)", series, fixed = TRUE)

# 內部連結後（到參照世代的量尺）或外部連結後（到 D）：換算後用 kits 的 rbind() 合起來
kitsui_vertical_linked <- function(raw, earlier, fixed, links, stage, label) {
  scales <- union(names(raw), earlier$scale)
  ref <- paste(scale_domain(scales[1]), fixed$version$reference)
  target <- if (stage == "internal") ref else "D"
  converted <- purrr::imap(raw, \(x, s) {
    kitsui_convert_path(x, kitsui_path_links(fixed, links, s, stage))
  })
  before <- kitsui_summary_convert(earlier, fixed, links, stage)
  ends <- c(purrr::map_chr(converted, \(x) attr(x, "scale")), attr(before, "ends"))
  if (any(ends != target)) {
    why <- kitsui_vertical_why(fixed, scales, links, stage)
    return(kitsui_vertical_none(names(raw), why))
  }
  v <- kits::kits_vertical(do.call(rbind, unname(converted)))
  list(vertical = v, table = kitsui_vertical_table(v, before, label), scale = target, why = NULL)
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

#' 這一回合兩個世代沒有共同的月齡時，一句說明（有共同月齡時是 NULL）
#' @param raw 具名 list：各量尺換算前的 kits_subj（有 wave_m）
#' @noRd
kitsui_vertical_no_common <- function(raw) {
  waves <- purrr::map(raw, \(x) sort(unique(x$wave_m)))
  if (any(purrr::map_lgl(waves, is.null))) return(NULL)
  if (length(Reduce(intersect, waves)) > 0) return(NULL)
  each <- purrr::imap_chr(waves, \(w, s) paste0(s, "：", paste(w, collapse = "、"), " 個月"))
  paste0("這一回合兩個世代沒有共同的月齡（", paste(each, collapse = "；"), "）。",
         "所以圖上沒有同一個月齡可以直接比的兩個點。")
}

# 圖 ----

#' 畫圖的列：這一回合（kits_vertical() 的表）與之前回合（摘要），依世代、波次排好
#' @param v 這一回合的 kits_vertical()；earlier 是（換算過的）之前回合的摘要
#' @return tibble：cohort、wave_m、n、median、P25、P75、age_med、source、round
#' @noRd
kitsui_vertical_table <- function(v, earlier, label) {
  now <- tibble::as_tibble(as.data.frame(v)) |>
    dplyr::select(dplyr::any_of(c("cohort", "wave_m", "n", "median", "P25", "P75", "age_med"))) |>
    dplyr::mutate(source = "this", round = label %||% NA_character_)
  before <- tibble::as_tibble(earlier) |>
    dplyr::transmute(.data$cohort, .data$wave_m, n = as.integer(.data$n), median = .data$p50,
                     P25 = .data$p25, P75 = .data$p75, age_med = as.numeric(.data$age_med),
                     source = "earlier", .data$round)
  dplyr::bind_rows(now, before) |>
    dplyr::arrange(.data$cohort, .data$wave_m, .data$source == "this")
}

#' 一個版本的垂直連結圖：中位數，線段是 P25 到 P75，顏色是世代
#'
#' 只有這一回合時就是 plot(kits_vertical(), type = "median")。有之前回合的點時，用同一張圖畫
#' 全部的列（x 軸是波次，因為摘要沒有實際月齡）：之前回合的點是空心、較淡，滑鼠移上去看回合。
#' @param v kitsui_verticals() 的一個版本
#' @noRd
kitsui_vertical_plot <- function(v) {
  tab <- v$table
  if (is.null(tab) || !any(tab$source == "earlier")) {
    ticks <- kitsui_vertical_ticks(as.data.frame(v$vertical)$wave_m)
    return(kitsui_vertical_axis(plot(v$vertical, type = "median", age_ticks = ticks)))
  }
  keep <- c("cohort", "wave_m", "n", "median", "P25", "P75")
  obj <- as.data.frame(tab[keep])
  # 點畫在實際月齡的中位數；沒有的列（舊的固定檔）退回名義月齡
  obj$age_med <- kitsui_vertical_age(tab)
  obj <- structure(obj, class = class(v$vertical), scale = attr(v$vertical, "scale"))
  p <- plot(obj, type = "median", age_ticks = kitsui_vertical_ticks(tab$wave_m))
  p <- kitsui_vertical_axis(p)
  # kits 的圖可能已經 build 過（p$x$data）；沒有時改還沒 build 的 p$x$attrs
  slot <- if (length(p$x$data) > 0) "data" else "attrs"
  p$x[[slot]] <- purrr::map(p$x[[slot]], \(tr) kitsui_vertical_marks(tr, tab))
  p
}

# 點的 x：實際月齡的中位數。一張圖只用一種月齡：有一列沒有實際月齡（0.3.11 以前的固定檔、
# 資料沒有 age_m）時，整張圖都用名義月齡（全部 NA，kits 的圖就畫在波次上）
kitsui_vertical_age <- function(tab) {
  age <- as.numeric(tab$age_med %||% NA_real_)
  if (!all(is.finite(age))) return(rep(NA_real_, nrow(tab)))
  age
}

# 這張圖用實際月齡嗎（每一列都有實際月齡的中位數）
kitsui_vertical_actual <- function(tab) {
  age <- as.numeric(tab$age_med %||% NA_real_)
  length(age) > 0 && all(is.finite(age))
}

# x 軸的標題寫清楚是實際月齡還是名義月齡（圖上用英文）。
# kits 的圖用實際月齡時寫 "Age (months)"，用名義月齡（波次）時寫 "Wave (months)"
KITSUI_AGE_AXIS <- c(
  "Age (months)" = "Age (months; median actual age of each wave)",
  "Wave (months)" = "Age (months; nominal age of each wave)"
)
kitsui_vertical_axis <- function(p) {
  title <- p$x$layout$xaxis$title %||% p$x$layoutAttrs[[1]]$xaxis$title
  if (is.list(title)) title <- title$text
  if (!is.character(title) || !title %in% names(KITSUI_AGE_AXIS)) return(p)
  plotly::layout(p, xaxis = list(title = unname(KITSUI_AGE_AXIS[title])))
}

# x 軸的刻度（也是格線）：圖上有的每一個波次。kits 預設的刻度只到 96 個月，
# 之後的波次（例如 108、120、132）會沒有刻度與格線
kitsui_vertical_ticks <- function(waves) {
  sort(unique(waves[is.finite(waves)]))
}

# 一個世代的線：之前回合的點空心、較淡，滑鼠移上去多一行回合（點的順序和 tab 裡這個世代的列相同）
kitsui_vertical_marks <- function(tr, tab) {
  if (is.null(tr$name)) return(tr)
  rows <- tab[tab$cohort %in% tr$name, ]
  if (nrow(rows) != length(tr$x)) return(tr)
  old <- rows$source == "earlier"
  tr$marker$symbol <- paste0(tr$marker$symbol, ifelse(old, "-open", ""))
  tr$marker$opacity <- ifelse(old, 0.6, 1)
  tr$hovertext <- paste0(tr$hovertext, ifelse(
    old, paste0("<br>Earlier round (fixed file): ", rows$round),
    paste0("<br>This round: ", rows$round)
  ))
  tr
}
