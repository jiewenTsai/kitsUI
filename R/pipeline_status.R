# ② 固定了哪些題：一個量尺這一回合每一題的狀態，並核對固定的步驟真的等於題庫的值
# 只用一回合的結果（kitsui_round()：items、drift、log）與這一回合開始時的題庫

# 每一題的狀態（kind -> 畫面上的字），照這個順序排：問題先
KITSUI_STATUS <- c(
  moved = "偏移 → 新版本",
  out = "這一回合拿掉",
  new = "新題（這一回合估計）",
  kept = "固定（不判斷偏移，沿用題庫）",
  fixed = "固定（沿用題庫）",
  free = "自由校準"
)
# 圖上的字（圖的標題、軸、圖例、滑鼠提示都用英文）
KITSUI_STATUS_EN <- c(
  moved = "Drifted -> new version",
  out = "Dropped this round",
  new = "New item (estimated this round)",
  kept = "Fixed (not judged, bank value)",
  fixed = "Fixed (bank value)",
  free = "Free calibration"
)
# 固定在題庫的值的狀態
KITSUI_STATUS_FIXED <- c("kept", "fixed")
# 固定的步驟和題庫差多少算不同
KITSUI_STATUS_TOL <- 1e-8

#' 一個量尺這一回合每一題的狀態
#'
#' 新版本照紀錄的 "new version"（"舊題號 -> 新題號"）；拿掉的照 "category problem, left out"；
#' 不在題庫的是新題；其他題庫的題固定在題庫的值：判斷過、沒有偏移的是「固定（沿用題庫）」，
#' 其他是「固定（不判斷偏移，沿用題庫）」，原因取自紀錄或偏移表的 reason。
#' @param item 這一回合的 kits_item()；NULL（停下來）時回傳 NULL
#' @param drift kits_drift() 或 NULL（自由校準）
#' @param log 這個量尺這一回合的紀錄（kitsui_round()$log 的列）
#' @param bank 這一回合開始時這個量尺的題庫（item_id、item_score、beta）；0 列是自由校準
#' @return tibble：key（這一回合開始時的題號）、item_id（畫面上的；新版本是「a → a_r2」）、
#'   status、kind（KITSUI_STATUS 的名稱）、steps（這一回合的步驟數）、detail
#' @noRd
kitsui_item_status <- function(item, drift, log, bank) {
  if (is.null(item)) return(NULL)
  co <- as.data.frame(stats::coef(item))
  n_steps <- \(ids) as.integer(table(co$item_id)[ids])
  pick <- \(check, ids) {
    hit <- log[log$check %in% check, ]
    hit$detail[match(ids, hit$item_id)]
  }
  row <- \(key, shown, kind, steps, detail) tibble::tibble(
    key = key, item_id = shown, status = unname(KITSUI_STATUS[kind]), kind = kind,
    steps = steps, detail = dplyr::coalesce(detail, "")
  )
  gone <- unique(log$item_id[log$check == "category problem, left out"])
  out <- row(gone, gone, rep("out", length(gone)), rep(NA_integer_, length(gone)),
             pick("category problem, left out", gone))
  ids <- unique(co$item_id)
  if (nrow(bank) == 0) {
    free <- row(ids, ids, rep("free", length(ids)), n_steps(ids), NA_character_)
    return(kitsui_status_sort(dplyr::bind_rows(out, free)))
  }
  # 新版本：紀錄的 "舊題號 -> 新題號"
  nv <- log$detail[log$check == "new version"]
  from <- sub(" -> .*$", "", nv)
  to <- sub("^.* -> ", "", nv)
  moved <- row(from, paste(from, "→", to), rep("moved", length(from)), n_steps(to),
               pick("drift", from))
  rest <- setdiff(ids, to)
  fresh <- rest[!rest %in% bank$item_id]
  new <- row(fresh, fresh, rep("new", length(fresh)), n_steps(fresh),
             "不在題庫裡，這一回合第一次估計")
  dplyr::bind_rows(moved, out, new, kitsui_status_fixed(setdiff(rest, fresh), co, drift, log,
                                                         bank)) |>
    kitsui_status_sort()
}

#' 固定在題庫的值的題：判斷過、沒有偏移的，與沒有判斷的（原因）；有新的步驟時也寫出來
#' @noRd
kitsui_status_fixed <- function(ids, co, drift, log, bank) {
  # 不能判斷偏移時（drift 是 NULL）沒有偏移表：每一題的原因都在紀錄裡
  items <- as.data.frame(drift$items %||% data.frame(item_id = character(), drift = logical(),
                                                     reason = character()))
  at <- match(ids, items$item_id)
  drifted <- items$drift[at]
  checks <- c("category problem, kept fixed", "no CML estimate, kept fixed",
              "drift not judged, kept fixed")
  kept_log <- log[log$check %in% checks, ]
  logged <- kept_log$detail[match(ids, kept_log$item_id)]
  judged <- drifted %in% FALSE & is.na(logged)
  reason <- unname(ITEMS_REASON_ZH[items$reason[at]])
  why <- dplyr::coalesce(logged, reason)
  # 偏移了，但新版本估不出來：固定在題庫的值（紀錄寫原因）
  why <- dplyr::if_else(drifted %in% TRUE & !is.na(why), paste0("偏移了，但", why), why)
  detail <- dplyr::if_else(judged, "已判斷，沒有偏移", dplyr::coalesce(why, ""))
  # 題庫沒有的步驟：這一回合另外估（例如這一回合有比題庫更高的分數）
  extra <- dplyr::anti_join(co[co$item_id %in% ids, ], bank, by = c("item_id", "item_score"))
  more <- purrr::map_chr(ids, \(id) {
    s <- extra$item_score[extra$item_id == id]
    if (length(s) == 0) "" else paste0("（這一回合估的步驟：", paste(s, collapse = "、"), "）")
  })
  tibble::tibble(
    key = ids, item_id = ids, kind = dplyr::if_else(judged, "fixed", "kept"),
    steps = as.integer(table(co$item_id)[ids]), detail = paste0(detail, more)
  ) |>
    dplyr::mutate(status = unname(KITSUI_STATUS[.data$kind]), .after = "item_id")
}

#' 照 KITSUI_STATUS 的順序排（同一種狀態裡保持原來的順序）
#' @noRd
kitsui_status_sort <- function(tab) {
  tab[order(match(tab$kind, names(KITSUI_STATUS))), ]
}

#' 核對：固定的題，這一回合的參數（coef(item) 的 beta）和題庫的值差多少
#' @param status kitsui_item_status()
#' @param bank 這一回合開始時這個量尺的題庫
#' @return list(n_items, n_steps, max_diff, bad)：bad 是差大於 tol 的步驟
#'   （item_id、item_score、beta、beta_bank、diff）
#' @noRd
kitsui_fixed_check <- function(item, status, bank, tol = KITSUI_STATUS_TOL) {
  keys <- status$key[status$kind %in% KITSUI_STATUS_FIXED]
  co <- as.data.frame(stats::coef(item))[c("item_id", "item_score", "beta")]
  hit <- co[co$item_id %in% keys, ] |>
    dplyr::inner_join(dplyr::select(bank, "item_id", "item_score", beta_bank = "beta"),
                      by = c("item_id", "item_score")) |>
    dplyr::mutate(diff = abs(.data$beta - .data$beta_bank))
  bad <- hit[!(hit$diff <= tol), ]
  list(n_items = length(keys), n_steps = nrow(hit), max_diff = max(c(0, hit$diff)),
       bad = tibble::as_tibble(bad))
}

#' 最大差的寫法：0 就寫 0，其他用科學記號（例如 3.6e-15）
#' @noRd
kitsui_diff_text <- function(x) {
  if (x == 0) "0" else formatC(x, format = "e", digits = 1)
}

#' 「固定了哪些題」的兩句話：固定與其他狀態的題數；核對的結果（沒有固定的題時 NULL）
#' @param check kitsui_fixed_check()；自由校準時 NULL
#' @return list(main, verify, ok)：ok 是固定的步驟都和題庫相同
#' @noRd
kitsui_status_text <- function(status, check = NULL) {
  n <- \(kind) sum(status$kind == kind)
  dropped <- if (n("out") > 0) sprintf("另外 %d 題這一回合拿掉。", n("out"))
  if (n("free") > 0 || is.null(check)) {
    main <- sprintf("沒有題庫，這個量尺是自由校準，沒有固定的題：%d 題都是這一回合估計的。",
                    n("free"))
    return(list(main = paste0(main, dropped), verify = NULL, ok = TRUE))
  }
  main <- sprintf(
    "這一回合固定 %d 題（沿用題庫的參數），換成新版本 %d 題，新題 %d 題，拿掉 %d 題。",
    n("fixed") + n("kept"), n("moved"), n("new"), n("out")
  )
  same <- if (check$max_diff <= 1e-12) "完全相同" else "相同"
  verify <- sprintf("固定的 %d 題、%d 個步驟，參數和題庫%s（最大差 %s）。", check$n_items,
                    check$n_steps, same, kitsui_diff_text(check$max_diff))
  if (nrow(check$bad) > 0) {
    verify <- sprintf("固定的 %d 題、%d 個步驟裡，有 %d 個步驟和題庫不同（最大差 %s）。",
                      check$n_items, check$n_steps, nrow(check$bad),
                      kitsui_diff_text(check$max_diff))
  }
  list(main = main, verify = verify, ok = nrow(check$bad) == 0)
}

#' 和題庫不同的步驟，一步一句（紅框裡列出來）
#' @noRd
kitsui_fixed_bad_lines <- function(bad) {
  sprintf("%s 步驟 %d：這一回合 %.4f，題庫 %.4f（差 %s）", bad$item_id, as.integer(bad$item_score),
          bad$beta, bad$beta_bank, purrr::map_chr(bad$diff, kitsui_diff_text))
}

# 偏移把關圖：每一種狀態的點（顏色和 kits 的圖一樣：固定藍、偏移橘、不判斷灰）；
# unjudged 是沒有判斷的步驟（上下的人不夠），不管是哪一題
KITSUI_DRIFT_LOOKS <- list(
  fixed = list(col = "#0072B2", symbol = "circle"),
  moved = list(col = "#D55E00", symbol = "x"),
  kept = list(col = "#7F7F7F", symbol = "diamond"),
  other = list(col = "#000000", symbol = "square"),
  unjudged = list(col = "#7F7F7F", symbol = "circle-open",
                  name = "Step not judged (too few people above/below)")
)

#' 偏移把關圖：kits 的 plot(drift)（新的門檻對題庫的門檻、連結線），點改成 kitsUI 的狀態上色，
#' 加上灰色帶子（判斷用的線 ± cut 個殘差標準差；帶子外的步驟被拿掉）
#'
#' 沒有判斷的步驟（上下的人不夠）另外畫成灰色空心圓：它們在帶子外也不算偏移。
#' 帶子用 kits::algo_screen() 重算（和 kits_drift() 的判斷相同）；算出來和 drift 的判斷
#' 不一樣時不畫帶子。
#' @param status kitsui_item_status()（key、status、kind）
#' @noRd
kitsui_drift_plot <- function(drift, status) {
  p <- plot(drift, type = "scatter")
  pts <- as.data.frame(drift$points)
  at <- match(pts$item_id, status$key)
  pts$kind <- dplyr::coalesce(status$kind[at], "other")
  pts$status <- dplyr::coalesce(unname(KITSUI_STATUS_EN[pts$kind]), "Other")
  pts$look <- dplyr::if_else(pts$judged %in% TRUE, pts$kind, "unjudged")
  q <- plotly::plot_ly()
  band <- kitsui_drift_band(drift, pts)
  if (!is.null(band)) {
    q <- plotly::add_trace(
      q, x = band$x, y = band$y, type = "scatter", mode = "lines", fill = "toself",
      fillcolor = "rgba(127,127,127,0.18)", line = list(width = 0), hoverinfo = "skip",
      name = sprintf("Grey band: judging line \u00b1 %s SD", band$cut)
    )
    # 帶子的中線：判斷偏移用的線（拿掉偏移的步驟之前）；實線是拿掉之後的線
    q <- plotly::add_trace(
      q, x = band$ends, y = band$mid, type = "scatter", mode = "lines", hoverinfo = "skip",
      line = list(color = "rgba(90,90,90,0.9)", width = 1.5, dash = "dash"),
      name = "Judging line (all judged steps)"
    )
  }
  for (k in intersect(names(KITSUI_DRIFT_LOOKS), unique(pts$look))) {
    s <- pts[pts$look == k, ]
    not_judged <- dplyr::if_else(s$judged %in% TRUE, "",
                                 "<br>Step not judged (too few people above/below)")
    hover <- sprintf(
      "%s step %d<br>%s<br>Bank threshold (fixed value): %.2f<br>Free trial threshold: %.2f%s",
      s$item_id, as.integer(s$item_score), s$status, s$thr_ref, s$thr_new, not_judged
    )
    look <- KITSUI_DRIFT_LOOKS[[k]]
    q <- plotly::add_markers(q, x = s$thr_new, y = s$thr_ref, name = look$name %||% s$status[1],
                             marker = list(color = look$col, symbol = look$symbol, size = 8),
                             hovertext = hover, hoverinfo = "text")
  }
  mine <- plotly::plotly_build(q)$x$data
  lines <- purrr::keep(p$x$data, \(tr) identical(tr$mode, "lines"))
  # 帶子在最下面，線其次，點在最上面
  is_band <- purrr::map_lgl(mine, \(tr) {
    identical(tr$fill, "toself") || identical(tr$mode, "lines")
  })
  p$x$data <- c(mine[is_band], lines, mine[!is_band])
  # 軸與標題寫清楚：點是固定之前的試算，不是固定之後的參數（圖上的字都用英文）
  plotly::layout(
    p,
    title = list(text = "Drift check before fixing"),
    xaxis = list(title = list(
      text = "Threshold, free trial this round (before fixing, own scale)"
    )),
    yaxis = list(title = list(text = "Bank threshold (fixed value)"))
  )
}

#' 灰色帶子的四個角：判斷用的線（全部判斷的步驟的 mean/sigma）± cut 個殘差標準差
#' @return list(x, y, cut)；判斷的步驟不到 3 個或重算不出 drift 的判斷時 NULL
#' @noRd
kitsui_drift_band <- function(drift, pts) {
  use <- pts[pts$judged %in% TRUE, ]
  cut <- drift$info$settings$cut %||% kitsui_drift_cut()
  if (nrow(use) < 3) return(NULL)
  sc <- tryCatch(kits::algo_screen(use$thr_new, use$thr_ref, use$item_id, cut = cut),
                 error = \(e) NULL)
  if (is.null(sc) || !identical(as.logical(sc$dropped), as.logical(use$dropped))) return(NULL)
  h <- cut * stats::sd(sc$resid)
  # 和 kits 的線一樣寬
  ends <- range(c(pts$thr_new, pts$thr_ref), na.rm = TRUE)
  mid <- sc$intercept0 + sc$slope0 * ends
  list(x = c(ends, rev(ends), ends[1]), y = c(mid + h, rev(mid) - h, mid[1] + h), cut = cut,
       ends = ends, mid = mid)
}
