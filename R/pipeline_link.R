# 連結：每條連結只做一次（kits_linking()，用題庫的門檻），點存進固定檔的 links.csv，之後從點重建
#   內部：參照世代以外的每個世代 -> 參照世代（每個世代、每個領域各一條；KIT 是 M36 -> M03）
#   外部：參照世代 -> D（各領域共用一條；參照世代有 GSED 的錨題時才適用）

#' 內部連結：一個領域的 cohort -> 參照世代（兩邊都用題庫到目前為止的所有步驟）
#'
#' @param cohort 要連結的世代；NULL 時用題庫裡這個領域唯一一個不是參照世代的世代
#'   （有好幾個時要指定）
#' @return list(fixed, link, new, message)：已有這條連結時沿用（new = FALSE）；
#'   做不了時 link = NULL，message 說明原因（kits 的錯誤訊息）
#' @noRd
kitsui_link_internal <- function(fixed, domain, cohort = NULL, n_boot = 2000, seed = 1) {
  if (is.null(cohort)) {
    others <- kitsui_internal_scales(fixed)
    cohort <- scale_cohort(others[scale_domain(others) == domain])
    if (length(cohort) == 0) {
      # 原文：題庫裡還沒有  |  參照世代（
      text <- paste0("\u984c\u5eab\u88e1\u9084\u6c92\u6709 ", domain, " \u53c3\u7167\u4e16\u4ee3\uff08", fixed$version$reference,
                     # 原文：）以外的量尺。
                     "\uff09\u4ee5\u5916\u7684\u91cf\u5c3a\u3002")
      return(list(fixed = fixed, link = NULL, new = FALSE, message = text))
    }
    if (length(cohort) > 1) {
      # 原文： 有好幾個世代可以做內部連結（ | 、 | ），
      kitDesign::kit_abort(domain, " \u6709\u597d\u5e7e\u500b\u4e16\u4ee3\u53ef\u4ee5\u505a\u5167\u90e8\u9023\u7d50\uff08", paste(cohort, collapse = "\u3001"), "\uff09\uff0c",
                           # 原文：請指定是哪一個世代。
                           "\u8acb\u6307\u5b9a\u662f\u54ea\u4e00\u500b\u4e16\u4ee3\u3002")
    }
  }
  from <- scale_key(domain, cohort)
  to <- scale_key(domain, fixed$version$reference)
  kitsui_link_once(fixed, from, "internal", function() {
    kits::kits_linking(kitsui_bank_thr(fixed, from), kitsui_bank_thr(fixed, to),
                       n_boot = n_boot, seed = seed)
  })
}

#' 外部連結：參照世代的各領域 -> D（錨題；kits 要求至少 3 個錨題）
#' @inherit kitsui_link_internal return
#' @noRd
kitsui_link_D <- function(fixed, anchors = kits::kits_anchors(), n_boot = 2000,
                          seed = 1) {
  ref <- kitsui_ref_scales(fixed)
  kitsui_link_once(fixed, "D", "D", function() {
    thr <- purrr::map(purrr::set_names(ref, scale_domain(ref)), \(s) kitsui_bank_thr(fixed, s))
    kits::kits_linking(thr, anchors, n_boot = n_boot, seed = seed)
  })
}

# 固定檔裡參照世代的量尺（外部連結的起點）
kitsui_ref_scales <- function(fixed) {
  scales <- fixed$version$scales
  scales[scale_cohort(scales) == fixed$version$reference]
}

# 題庫的門檻：kits 匯出的 algo_thresholds()（吃 coef 與 vcov）。題庫沒有存 beta 的共變數，
# vcov 用 beta_se^2 的對角：thr 和 coef(kits_item) 完全相同，thr_se 是近似（連結用 sd 篩點，不用它）
kitsui_bank_thr <- function(fixed, scale) {
  b <- fixed$bank |>
    dplyr::filter(.data$scale == !!scale) |>
    dplyr::arrange(.data$item_id, .data$item_score)
  # 原文：固定檔裡沒有  |  的題目參數，請先做這個量尺的題目校準。
  if (nrow(b) == 0) kitDesign::kit_abort("\u56fa\u5b9a\u6a94\u88e1\u6c92\u6709 ", scale, " \u7684\u984c\u76ee\u53c3\u6578\uff0c\u8acb\u5148\u505a\u9019\u500b\u91cf\u5c3a\u7684\u984c\u76ee\u6821\u6e96\u3002")
  coef <- data.frame(item_id = b$item_id, item_score = b$item_score, beta = b$beta, se = b$beta_se)
  thr <- kits::algo_thresholds(list(coef = coef, vcov = diag(b$beta_se^2, nrow(b))))
  structure(thr, scale = scale)
}

# 已有就沿用；沒有就做一次，把點寫進 fixed$links（round 是這一回合）。
# 有沒有先看名稱（kitsui_link_names()），已有時只重建這一條（不重建全部）。
# kits 的錯誤（例如錨題不到 3 個）變成 message
kitsui_link_once <- function(fixed, name, kind, make) {
  if (name %in% kitsui_link_names(fixed)) {
    old <- kitsui_links(fixed, name)[[name]]
    # 原文：沿用固定檔裡的連結。
    return(list(fixed = fixed, link = old, new = FALSE, message = "\u6cbf\u7528\u56fa\u5b9a\u6a94\u88e1\u7684\u9023\u7d50\u3002"))
  }
  link <- tryCatch(make(), error = function(e) e)
  if (inherits(link, "error")) {
    return(list(fixed = fixed, link = NULL, new = FALSE, message = conditionMessage(link)))
  }
  round <- utils::tail(fixed$version$rounds, 1)
  fixed$links <- dplyr::bind_rows(fixed$links, kitsui_link_rows(link, kind, round))
  # 原文：已建立連結，存進固定檔。
  list(fixed = fixed, link = link, new = TRUE, message = "\u5df2\u5efa\u7acb\u9023\u7d50\uff0c\u5b58\u9032\u56fa\u5b9a\u6a94\u3002")
}

# 一條連結的點與設定（links.csv 的列）；from_scale 多個時以 ", " 接起來
kitsui_link_rows <- function(link, kind, round) {
  pts <- as.data.frame(link$points)
  st <- link$settings
  tibble::tibble(
    kind = kind, from_scale = paste(link$from_scale, collapse = ", "), to_scale = link$to_scale,
    id = pts$id, item = pts$item, from = pts$from, to = pts$to,
    domain = pts$domain %||% NA_character_, cut = st$cut, spread = st$spread, unit = st$unit,
    n_boot = as.integer(st$n_boot), seed = as.integer(st$seed),
    round = if (length(round) == 0) NA_character_ else round
  )
}

#' links.csv 每一列屬於哪一條連結：內部連結以起點量尺命名（"lan M36"），外部連結叫 "D"
#' @noRd
kitsui_link_key <- function(rows) ifelse(rows$kind == "D", "D", rows$from_scale)

#' 固定檔裡的連結名稱（同 kitsui_links() 的名稱）；沒有固定檔時為空
#' @noRd
kitsui_link_names <- function(fixed) {
  rows <- fixed$links
  if (is.null(rows) || nrow(rows) == 0) return(character())
  unique(kitsui_link_key(rows))
}

#' 從 links.csv 的點重建所有連結
#'
#' kits_link_screen(points) |> kits_link_line() |> kits_link_error()，和原本的 kits_linking()
#' 同一段計算，係數完全相同。點附上 kits_link_points() 會記下的資訊（種類、題數、兩邊的題數），
#' 所以 kits_link_check() 能用，診斷也和第一次相同。
#' 每條連結要重抽 n_boot 次（約 1 秒），所以一組固定檔只建一次，再傳給要用的函式。
#' @param names NULL（全部）或只重建這幾條（kitsui_link_names() 的名稱）
#' @return 具名 list：內部連結以起點量尺命名（"lan M36"），外部連結叫 "D"
#' @noRd
kitsui_links <- function(fixed, names = NULL) {
  rows <- fixed$links
  if (is.null(rows) || nrow(rows) == 0) return(list())
  if (!is.null(names)) rows <- rows[kitsui_link_key(rows) %in% names, ]
  purrr::map(split(rows, kitsui_link_key(rows)), \(r) kitsui_link_rebuild(r, fixed))
}

# 一條連結：links.csv 的幾列 -> kits_link
kitsui_link_rebuild <- function(r, fixed) {
  meta <- kitsui_link_meta(r, fixed)
  cols <- c("id", "item", "from", "to", if (!all(is.na(r$domain))) "domain")
  pts <- as.data.frame(r[cols])
  # 外部連結的點是錨題的通過步（kits_linking() 接錨題時 step = 1）
  if (meta$kind == "anchors") pts$step <- meta$step
  attr(pts, "link") <- meta
  link <- kits::kits_link_screen(pts, cut = r$cut[1], spread = r$spread[1], unit = r$unit[1]) |>
    kits::kits_link_line()
  if (r$n_boot[1] > 0) link <- kits::kits_link_error(link, r$n_boot[1], r$seed[1])
  link
}

# kits_link_points() 記在點上的資訊，從存下來的點與題庫補回來：
#   n_items 是點裡的題數；items_total 是做連結那一回合為止，題庫裡兩個量尺各有幾題；
#   外部連結裡沒有錨題的領域（no_anchors）用同一條線
kitsui_link_meta <- function(r, fixed) {
  from <- kitsui_split_scales(r$from_scale[1], fixed)
  to <- r$to_scale[1]
  if (r$kind[1] == "D") {
    no_anchors <- setdiff(scale_domain(from), r$domain)
    return(list(kind = "anchors", step = 1L, no_anchors = if (length(no_anchors)) no_anchors,
                from_scale = from, to_scale = to, n_items = dplyr::n_distinct(r$item)))
  }
  list(kind = "calibration", step = "all", from_scale = from, to_scale = to,
       n_items = dplyr::n_distinct(r$item),
       items_total = kitsui_items_total(fixed, c(from = from, to = to), r$round[1]))
}

# 做連結那一回合為止（含），題庫裡各量尺的題數；round 不明時用整個題庫
kitsui_items_total <- function(fixed, scales, round) {
  rounds <- fixed$version$rounds
  upto <- if (isTRUE(round %in% rounds)) rounds[seq_len(match(round, rounds))] else rounds
  bank <- dplyr::filter(fixed$bank, .data$round %in% upto)
  purrr::map_int(scales, \(s) dplyr::n_distinct(bank$item_id[bank$scale == s]))
}

#' 已凍結的外部連結（到 D）延伸到之後才進題庫的參照世代量尺
#'
#' kits 的外部連結是各領域共用的一條線，沒有錨題的領域也用同一條線。所以新的參照世代量尺
#' （例如之後才校準的 "soc M03"）直接加進外部連結的起點：截距、斜率、點都不變，不重新估計。
#' @return list(fixed, log)：fixed$links 的 from_scale 加上新的量尺，fixed$checks 與 log 各一列
#'   （check = "D extended"；log 的 detail 是中文、detail_en 是英文，checks 寫英文）；
#'   沒有外部連結或沒有新量尺時不變
#' @noRd
kitsui_link_D_extend <- function(fixed, round) {
  d <- fixed$links$kind == "D"
  from <- if (any(d)) kitsui_split_scales(fixed$links$from_scale[which(d)[1]], fixed)
  new <- setdiff(kitsui_ref_scales(fixed), from)
  if (!any(d) || length(new) == 0) return(list(fixed = fixed, log = NULL))
  fixed$links$from_scale[d] <- paste(c(from, new), collapse = ", ")
  log <- tibble::tibble(
    round = round, scale = new, item_id = NA_character_, check = "D extended",
    # 原文：外部連結延伸到  | ：同一條線（截距、斜率、點都不變），沒有重新估計
    detail = paste0("\u5916\u90e8\u9023\u7d50\u5ef6\u4f38\u5230 ", new, "\uff1a\u540c\u4e00\u689d\u7dda\uff08\u622a\u8ddd\u3001\u659c\u7387\u3001\u9ede\u90fd\u4e0d\u8b8a\uff09\uff0c\u6c92\u6709\u91cd\u65b0\u4f30\u8a08"),
    detail_en = paste0("external link extended to ", new,
                       ": same line (intercept, slope and points unchanged), not re-estimated")
  )
  # checks.csv 寫英文
  checks <- dplyr::mutate(log, detail = .data$detail_en)
  fixed$checks <- dplyr::bind_rows(fixed$checks, checks[names(KITSUI_FIXED_COLS$checks)])
  list(fixed = fixed, log = log)
}

#' 參照世代各量尺：錨題數、外部連結涵蓋的方式（建立時、第幾回合延伸、還沒有外部連結）
#' @param anchors kitsui_anchor_counts(fixed)
#' @return tibble：scale、n_anchors、external_link
#' @noRd
kitsui_d_coverage <- function(fixed, anchors) {
  d <- fixed$links[fixed$links$kind == "D", ]
  ext <- dplyr::filter(fixed$checks, .data$check == "D extended")
  to_d <- dplyr::case_when(
    # 原文：還沒有外部連結
    nrow(d) == 0 ~ "\u9084\u6c92\u6709\u5916\u90e8\u9023\u7d50",
    anchors$scale %in% ext$scale ~
      # 原文：第  |  回合延伸（同一條線）
      paste0("\u7b2c ", ext$round[match(anchors$scale, ext$scale)], " \u56de\u5408\u5ef6\u4f38\uff08\u540c\u4e00\u689d\u7dda\uff09"),
    # 原文：建立外部連結時（第  |  回合）
    TRUE ~ paste0("\u5efa\u7acb\u5916\u90e8\u9023\u7d50\u6642\uff08\u7b2c ", d$round[1], " \u56de\u5408\uff09")
  )
  dplyr::mutate(anchors, external_link = to_d)
}

#' 一個量尺的換算路線：先內部連結（不是參照世代時），再外部連結到 D（有外部連結、且起點包含時）
#' @return 連結的名稱（kitsui_links() 的名稱），例如 c("lan M36", "D")
#' @noRd
kitsui_link_path <- function(fixed, scale) {
  # 沒有固定檔、或固定檔裡還沒有連結：不換算
  if (is.null(fixed$links) || nrow(fixed$links) == 0) return(character())
  rows <- dplyr::distinct(fixed$links, .data$kind, .data$from_scale, .data$to_scale)
  internal <- dplyr::filter(rows, .data$kind == "internal", .data$from_scale == !!scale)
  now <- if (nrow(internal) > 0) internal$to_scale[1] else scale
  d <- dplyr::filter(rows, .data$kind == "D")
  to_d <- nrow(d) > 0 && now %in% kitsui_split_scales(d$from_scale[1], fixed)
  as.character(c(if (nrow(internal) > 0) scale, if (to_d) "D"))
}

# ③ 連結頁的狀態與檢核 ----

#' 可以做內部連結的量尺：題庫裡不是參照世代的量尺（每個世代、每個領域各一條，都連到參照世代）
#' @noRd
kitsui_internal_scales <- function(fixed) {
  scales <- union(fixed$version$scales, unique(fixed$bank$scale))
  sort(scales[scale_cohort(scales) != fixed$version$reference])
}

#' 所有內部連結一覽：起點、終點（參照世代的同一個領域）、狀態（一句話）
#' @param links kitsui_links(fixed)；earlier 是這一回合開始時固定檔裡的連結名稱
#' @return tibble（欄名英文）：from、to、state
#' @noRd
kitsui_internal_overview <- function(fixed, links, earlier) {
  scales <- kitsui_internal_scales(fixed)
  # 原文：已凍結 | 可以建立 | 還不能做（題庫缺一邊）
  words <- c(frozen = "\u5df2\u51cd\u7d50", ready = "\u53ef\u4ee5\u5efa\u7acb", missing = "\u9084\u4e0d\u80fd\u505a\uff08\u984c\u5eab\u7f3a\u4e00\u908a\uff09")
  state <- purrr::map_chr(scales, \(s) kitsui_internal_state(fixed, s, links, earlier)$state)
  tibble::tibble(from = scales, to = scale_key(scale_domain(scales), fixed$version$reference),
                 state = unname(words[state]))
}

# ③ 的標題與說明：用固定檔裡的參照世代與其他世代的名稱（不寫死 M03、M36） ----

#' 內部連結的狀態收成一句話（kitsui_internal_overview() 的表）
#'
#' 例如「內部連結共 36 條：還不能做 36 條（題庫裡還沒有 distress B72、tsupport B72 的題目參數）。」
#' @param ov kitsui_internal_overview()
#' @noRd
kitsui_internal_summary <- function(fixed, ov) {
  if (NROW(ov) == 0) return("")
  # 原文：已凍結 | 可以建立 | 還不能做（題庫缺一邊）
  words <- c("\u5df2\u51cd\u7d50", "\u53ef\u4ee5\u5efa\u7acb", "\u9084\u4e0d\u80fd\u505a\uff08\u984c\u5eab\u7f3a\u4e00\u908a\uff09")
  n <- purrr::map_int(words, \(w) sum(ov$state == w))
  # 原文：已凍結  | 可以建立  | 還不能做  |  條
  parts <- paste0(c("\u5df2\u51cd\u7d50 ", "\u53ef\u4ee5\u5efa\u7acb ", "\u9084\u4e0d\u80fd\u505a "), kitDesign::kit_num(n), " \u689d")[n > 0]
  miss <- ov[ov$state == words[3], ]
  why <- if (nrow(miss) > 0) {
    lack <- unique(setdiff(c(miss$from, miss$to), fixed$bank$scale))
    # 原文：（題庫裡還沒有  |  的題目參數，兩邊都校準過才能建立）
    paste0("\uff08\u984c\u5eab\u88e1\u9084\u6c92\u6709 ", kitsui_first(lack, 6), " \u7684\u984c\u76ee\u53c3\u6578\uff0c\u5169\u908a\u90fd\u6821\u6e96\u904e\u624d\u80fd\u5efa\u7acb\uff09")
  }
  # 原文：內部連結共  |  條： | 、
  paste0("\u5167\u90e8\u9023\u7d50\u5171 ", kitDesign::kit_num(nrow(ov)), " \u689d\uff1a", paste(parts, collapse = "\u3001"),
         # 原文：。
         why %||% "", "\u3002")
}

#' 參照世代，與題庫裡（含這一回合的量尺）其他的世代
#' @return list(ref, others)
#' @noRd
kitsui_link_cohorts <- function(fixed) {
  ref <- fixed$version$reference %||% KITSUI_REF_COHORT
  scales <- union(fixed$version$scales, unique(fixed$bank$scale))
  list(ref = ref, others = sort(setdiff(unique(scale_cohort(scales)), ref)))
}

#' 內部連結的標題，例如「1. 內部連結：M36 → M03」「1. 內部連結：M06、M40 → M03」
#' @noRd
kitsui_internal_title <- function(fixed) {
  co <- kitsui_link_cohorts(fixed)
  # 原文：其他世代 | 、
  from <- if (length(co$others) == 0) "\u5176\u4ed6\u4e16\u4ee3" else paste(co$others, collapse = "\u3001")
  # 原文：1. 內部連結： |  → 
  paste0("1. \u5167\u90e8\u9023\u7d50\uff1a", from, " \u2192 ", co$ref)
}

#' 內部連結的說明（一段話）
#' @noRd
kitsui_internal_hint <- function(fixed) {
  co <- kitsui_link_cohorts(fixed)
  # 原文：參照世代以外的世代 | 、
  from <- if (length(co$others) == 0) "\u53c3\u7167\u4e16\u4ee3\u4ee5\u5916\u7684\u4e16\u4ee3" else paste(co$others, collapse = "\u3001")
  # 原文：每個世代、每個領域各一條。 | 每個領域一條。
  each <- if (length(co$others) > 1) "\u6bcf\u500b\u4e16\u4ee3\u3001\u6bcf\u500b\u9818\u57df\u5404\u4e00\u689d\u3002" else "\u6bcf\u500b\u9818\u57df\u4e00\u689d\u3002"
  # 原文：各世代的題目參數各自在自己的量尺上。
  paste0("\u5404\u4e16\u4ee3\u7684\u984c\u76ee\u53c3\u6578\u5404\u81ea\u5728\u81ea\u5df1\u7684\u91cf\u5c3a\u4e0a\u3002",
         # 原文：內部連結用兩邊共同的題目，把  |  的量尺換到參照世代  |  的量尺。
         "\u5167\u90e8\u9023\u7d50\u7528\u5169\u908a\u5171\u540c\u7684\u984c\u76ee\uff0c\u628a ", from, " \u7684\u91cf\u5c3a\u63db\u5230\u53c3\u7167\u4e16\u4ee3 ", co$ref, " \u7684\u91cf\u5c3a\u3002", each)
}

#' 外部連結的標題，例如「2. 外部連結：M03 → D」；不適用時加「（不適用）」
#' @param applicable kitsui_d_applicable()
#' @noRd
kitsui_d_title <- function(fixed, applicable = TRUE) {
  # 原文：2. 外部連結： |  → D | （不適用）
  paste0("2. \u5916\u90e8\u9023\u7d50\uff1a", kitsui_link_cohorts(fixed)$ref, " \u2192 D", if (!applicable) "\uff08\u4e0d\u9069\u7528\uff09")
}

#' 外部連結的說明（一段話）
#' @noRd
kitsui_d_hint <- function(fixed, applicable = TRUE) {
  ref <- kitsui_link_cohorts(fixed)$ref
  if (!applicable) {
    # 原文：D 分數是嬰幼兒發展的國際量尺（GSED），錨題只有 lan、motor、cog 的題目。
    return(paste0("D \u5206\u6578\u662f\u5b30\u5e7c\u5152\u767c\u5c55\u7684\u570b\u969b\u91cf\u5c3a\uff08GSED\uff09\uff0c\u9328\u984c\u53ea\u6709 lan\u3001motor\u3001cog \u7684\u984c\u76ee\u3002",
                  # 原文：參照世代（ | ）的題目裡沒有錨題，所以這份資料不做外部連結。
                  "\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u984c\u76ee\u88e1\u6c92\u6709\u9328\u984c\uff0c\u6240\u4ee5\u9019\u4efd\u8cc7\u6599\u4e0d\u505a\u5916\u90e8\u9023\u7d50\u3002",
                  # 原文：分數留在參照世代（ | ）的量尺上。這不是錯誤。
                  "\u5206\u6578\u7559\u5728\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u91cf\u5c3a\u4e0a\u3002\u9019\u4e0d\u662f\u932f\u8aa4\u3002"))
  }
  # 原文：D 分數是國際共用的發展量尺。
  paste0("D \u5206\u6578\u662f\u570b\u969b\u5171\u7528\u7684\u767c\u5c55\u91cf\u5c3a\u3002",
         # 原文：參照世代（ | ）的題目裡，有些是錨題：它們在 D 量尺上的位置已知。
         "\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u984c\u76ee\u88e1\uff0c\u6709\u4e9b\u662f\u9328\u984c\uff1a\u5b83\u5011\u5728 D \u91cf\u5c3a\u4e0a\u7684\u4f4d\u7f6e\u5df2\u77e5\u3002",
         # 原文：外部連結用錨題把  |  的量尺換到 D。所有領域共用一條。
         "\u5916\u90e8\u9023\u7d50\u7528\u9328\u984c\u628a ", ref, " \u7684\u91cf\u5c3a\u63db\u5230 D\u3002\u6240\u6709\u9818\u57df\u5171\u7528\u4e00\u689d\u3002",
         # 原文：之後才進題庫的  |  量尺，沿用同一條外部連結。
         "\u4e4b\u5f8c\u624d\u9032\u984c\u5eab\u7684 ", ref, " \u91cf\u5c3a\uff0c\u6cbf\u7528\u540c\u4e00\u689d\u5916\u90e8\u9023\u7d50\u3002")
}

#' 外部連結（到 D）適不適用
#'
#' 不適用：還沒有外部連結、題庫裡已經有參照世代的量尺，而且這些量尺沒有任何錨題、
#' 領域也不是錨題的領域（kits_anchors() 的 lan、motor、cog）。例如 TDCYP 的家庭量表。
#' 題庫裡還沒有參照世代的量尺時還不知道，算適用（照原來的說明）。
#' @param anchors kitsui_anchor_counts(fixed)
#' @noRd
kitsui_d_applicable <- function(fixed, anchors = kitsui_anchor_counts(fixed),
                                domains = unique(kits::kits_anchors()$domain)) {
  if ("D" %in% kitsui_link_names(fixed)) return(TRUE)
  if (nrow(anchors) == 0 || sum(anchors$n_anchors) > 0) return(TRUE)
  any(scale_domain(anchors$scale) %in% domains)
}

#' 外部連結不適用時，分數停在參照世代的量尺：回傳參照世代；適用時 NULL
#' @noRd
kitsui_ref_final <- function(fixed) {
  if (kitsui_d_applicable(fixed)) NULL else fixed$version$reference
}

#' 一條內部連結的狀態
#'
#' @param links kitsui_links(fixed)
#' @param earlier 這一回合開始時固定檔裡的連結名稱（kitsui_link_names()）
#' @return list(state, link, text)：state 是 "frozen"（已有）、"ready"（可以做）、
#'   "missing"（缺題庫，text 說明原因）
#' @noRd
kitsui_internal_state <- function(fixed, scale, links, earlier) {
  to <- scale_key(scale_domain(scale), fixed$version$reference)
  link <- links[[scale]]
  if (!is.null(link)) {
    return(list(state = "frozen", link = link, text = kitsui_frozen_text(scale %in% earlier)))
  }
  miss <- setdiff(c(scale, to), fixed$bank$scale)
  if (length(miss) > 0) {
    # 原文：題庫裡還沒有  | 、 |  的題目參數。
    text <- paste0("\u984c\u5eab\u88e1\u9084\u6c92\u6709 ", paste(miss, collapse = "\u3001"), " \u7684\u984c\u76ee\u53c3\u6578\u3002",
                   # 原文：兩個世代都做過題目校準後，才能建立這條連結。
                   "\u5169\u500b\u4e16\u4ee3\u90fd\u505a\u904e\u984c\u76ee\u6821\u6e96\u5f8c\uff0c\u624d\u80fd\u5efa\u7acb\u9019\u689d\u9023\u7d50\u3002")
    return(list(state = "missing", link = NULL, text = text))
  }
  list(state = "ready", link = NULL,
       # 原文：題庫裡  |  與  |  都有了，可以建立連結。
       text = paste0("\u984c\u5eab\u88e1 ", scale, " \u8207 ", to, " \u90fd\u6709\u4e86\uff0c\u53ef\u4ee5\u5efa\u7acb\u9023\u7d50\u3002"))
}

kitsui_frozen_text <- function(from_earlier) {
  # 原文：之前的回合就做好了（在這一回合開始時的固定檔裡） | 這一回合剛做好
  when <- if (from_earlier) "\u4e4b\u524d\u7684\u56de\u5408\u5c31\u505a\u597d\u4e86\uff08\u5728\u9019\u4e00\u56de\u5408\u958b\u59cb\u6642\u7684\u56fa\u5b9a\u6a94\u88e1\uff09" else "\u9019\u4e00\u56de\u5408\u525b\u505a\u597d"
  # 原文：這條連結 | ，已經凍結。這一回合直接沿用，不會重做。
  paste0("\u9019\u689d\u9023\u7d50", when, "\uff0c\u5df2\u7d93\u51cd\u7d50\u3002\u9019\u4e00\u56de\u5408\u76f4\u63a5\u6cbf\u7528\uff0c\u4e0d\u6703\u91cd\u505a\u3002")
}

#' 參照世代各量尺的錨題數（kits_anchors() 的題號出現在題庫裡的）
#' @return tibble：scale、n_anchors
#' @noRd
kitsui_anchor_counts <- function(fixed, anchors = kits::kits_anchors()) {
  ref <- kitsui_ref_scales(fixed)
  n <- purrr::map_int(ref, \(s) {
    sum(anchors$item_id %in% fixed$bank$item_id[fixed$bank$scale == s])
  })
  tibble::tibble(scale = ref, n_anchors = n)
}

#' 外部連結（到 D）的狀態
#' @param anchors kitsui_anchor_counts(fixed)
#' @return list(state, link, text)：state 是 "frozen"、"ready"、"missing"，或 "na"
#'   （不適用：參照世代沒有錨題，kitsui_d_applicable()；不是錯誤）
#' @noRd
kitsui_d_state <- function(fixed, links, earlier, anchors) {
  link <- links[["D"]]
  n <- sum(anchors$n_anchors)
  if (!is.null(link)) {
    return(list(state = "frozen", link = link, text = kitsui_frozen_text("D" %in% earlier)))
  }
  if (!kitsui_d_applicable(fixed, anchors)) {
    ref <- fixed$version$reference
    # 原文：不適用：參照世代（ | ）的量尺（ | 、
    text <- paste0("\u4e0d\u9069\u7528\uff1a\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u91cf\u5c3a\uff08", paste(anchors$scale, collapse = "\u3001"),
                   # 原文：）沒有 D 分數的錨題（kits_anchors()），所以不做外部連結。
                   "\uff09\u6c92\u6709 D \u5206\u6578\u7684\u9328\u984c\uff08kits_anchors()\uff09\uff0c\u6240\u4ee5\u4e0d\u505a\u5916\u90e8\u9023\u7d50\u3002",
                   # 原文：分數留在參照世代（ | ）的量尺上；這一回合照常完成。
                   "\u5206\u6578\u7559\u5728\u53c3\u7167\u4e16\u4ee3\uff08", ref, "\uff09\u7684\u91cf\u5c3a\u4e0a\uff1b\u9019\u4e00\u56de\u5408\u7167\u5e38\u5b8c\u6210\u3002")
    return(list(state = "na", link = NULL, text = text))
  }
  if (nrow(anchors) == 0) {
    # 原文：題庫裡還沒有參照世代（ | ）的量尺。
    text <- paste0("\u984c\u5eab\u88e1\u9084\u6c92\u6709\u53c3\u7167\u4e16\u4ee3\uff08", fixed$version$reference, "\uff09\u7684\u91cf\u5c3a\u3002",
                   # 原文：先做參照世代的題目校準。
                   "\u5148\u505a\u53c3\u7167\u4e16\u4ee3\u7684\u984c\u76ee\u6821\u6e96\u3002")
    return(list(state = "missing", link = NULL, text = text))
  }
  if (n < 3) {
    # 原文：錨題只有  |  個，kits 至少要 3 個，所以還不能做外部連結。
    text <- paste0("\u9328\u984c\u53ea\u6709 ", n, " \u500b\uff0ckits \u81f3\u5c11\u8981 3 \u500b\uff0c\u6240\u4ee5\u9084\u4e0d\u80fd\u505a\u5916\u90e8\u9023\u7d50\u3002",
                   # 原文：kits 的訊息：
                   "kits \u7684\u8a0a\u606f\uff1a", kitsui_link_D(fixed)$message)
    return(list(state = "missing", link = NULL, text = text))
  }
  # 原文：錨題共  |  個，可以建立外部連結。
  text <- paste0("\u9328\u984c\u5171 ", n, " \u500b\uff0c\u53ef\u4ee5\u5efa\u7acb\u5916\u90e8\u9023\u7d50\u3002",
                 # 原文：外部連結會用上面所有量尺： | 、 | 。
                 "\u5916\u90e8\u9023\u7d50\u6703\u7528\u4e0a\u9762\u6240\u6709\u91cf\u5c3a\uff1a", paste(anchors$scale, collapse = "\u3001"), "\u3002",
                 # 原文：之後才進題庫的參照世代量尺，會沿用同一條外部連結，不重新估計。
                 "\u4e4b\u5f8c\u624d\u9032\u984c\u5eab\u7684\u53c3\u7167\u4e16\u4ee3\u91cf\u5c3a\uff0c\u6703\u6cbf\u7528\u540c\u4e00\u689d\u5916\u90e8\u9023\u7d50\uff0c\u4e0d\u91cd\u65b0\u4f30\u8a08\u3002")
  list(state = "ready", link = NULL, text = text)
}

#' 獨立檢核做不了的原因：要這一回合兩個世代的作答與校準；NULL 表示可以做
#' @noRd
kitsui_link_check_why <- function(round, link) {
  scales <- c(link$from_scale, link$to_scale)
  have <- scales %in% names(round$items) & scales %in% names(round$data)
  if (all(have)) return(NULL)
  # 原文：這一回合的題目校準沒有  | 、 | 。
  paste0("\u9019\u4e00\u56de\u5408\u7684\u984c\u76ee\u6821\u6e96\u6c92\u6709 ", paste(scales[!have], collapse = "\u3001"), "\u3002",
         # 原文：獨立檢核要用兩個世代這一回合的作答，所以這次不能做。
         "\u7368\u7acb\u6aa2\u6838\u8981\u7528\u5169\u500b\u4e16\u4ee3\u9019\u4e00\u56de\u5408\u7684\u4f5c\u7b54\uff0c\u6240\u4ee5\u9019\u6b21\u4e0d\u80fd\u505a\u3002",
         # 原文：下次兩個世代的資料一起進來時就能做。
         "\u4e0b\u6b21\u5169\u500b\u4e16\u4ee3\u7684\u8cc7\u6599\u4e00\u8d77\u9032\u4f86\u6642\u5c31\u80fd\u505a\u3002")
}

#' 獨立檢核：kits_link_check()，兩個世代這一回合的校準與作答
#'
#' 兩個世代的 visit_id 可能重複，所以列次資訊分開給（persons_from、persons_to）。
#' @param round kitsui_round() 的結果；df 是這一回合的長格式資料
#' @param persons ① 算好的 kitsui_persons(df)；NULL 時從 df 算
#' @noRd
kitsui_link_check <- function(round, df, link, persons = NULL) {
  from <- link$from_scale
  to <- link$to_scale
  persons <- persons %||% kitsui_persons(df)
  pick <- \(X) dplyr::filter(persons, .data$visit_id %in% rownames(X))
  kits::kits_link_check(
    link, round$items[[from]], round$items[[to]], round$data[[from]], round$data[[to]],
    persons_from = pick(round$data[[from]]), persons_to = pick(round$data[[to]])
  )
}

#' 固定檔裡所有連結的總表：內部或外部連結、係數、哪一回合做的（欄名是英文）
#' @noRd
kitsui_link_table <- function(fixed, links, earlier) {
  if (length(links) == 0) return(NULL)
  made_in <- fixed$links$round[match(names(links), kitsui_link_key(fixed$links))]
  purrr::imap(links, \(l, n) {
    co <- stats::coef(l)
    tibble::tibble(
      # 原文：外部連結 | 內部連結
      link = n, kind = if (n == "D") "\u5916\u90e8\u9023\u7d50" else "\u5167\u90e8\u9023\u7d50",
      # 原文：、
      from = paste(l$from_scale, collapse = "\u3001"), to = l$to_scale,
      intercept = co[["intercept"]], slope = co[["slope"]],
      # 原文：之前的回合 | 這一回合
      made = if (n %in% earlier) "\u4e4b\u524d\u7684\u56de\u5408" else "\u9019\u4e00\u56de\u5408"
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(round = made_in)
}

#' 一條連結的公式：新的值 = intercept + slope × 舊的值，和兩個係數的 linking error
#' @return list(formula = 一行文字, table = tibble（term、estimate、linking_error）)
#' @noRd
kitsui_link_formula <- function(link) {
  # 原文： 或 
  from <- paste(unique(link$from_scale), collapse = " \u6216 ")
  # 原文：%s = %.2f + %.2f × %s
  formula <- sprintf("%s = %.2f + %.2f \u00d7 %s", link$to_scale, link$intercept, link$slope, from)
  table <- tibble::tibble(
    term = c("intercept", "slope"),
    estimate = c(link$intercept, link$slope),
    linking_error = c(link$intercept_error, link$slope_error)
  )
  list(formula = formula, table = table)
}

#' 獨立檢核的小表：每個方法一列，和連結差多少、一致不一致（判斷用中文寫，附規則）
#'
#' 有重抽（n_boot > 0）時：差距除以標準誤（連結誤差與這個方法的抽樣誤差合起來，kits 的 z_both）
#' 不超過 2 算一致，和 kits_check() 的規則相同；沒有重抽時：差距不超過 tol。
#' @param chk kits_link_check()
#' @return tibble（欄名英文）：method、intercept、slope、max_diff、max_z、judgement；
#'   attr(, "rule") 是判斷規則（一句中文）
#' @noRd
kitsui_link_check_table <- function(chk) {
  at <- as.data.frame(chk$at)
  tol <- chk$settings$tol %||% 0.2
  draws <- "z_both" %in% names(at) && any(!is.na(at$z_both))
  worst <- \(x) if (all(is.na(x))) NA_real_ else max(abs(x), na.rm = TRUE)
  tab <- tibble::as_tibble(as.data.frame(chk$methods)[c("method", "intercept", "slope")]) |>
    dplyr::mutate(
      max_diff = purrr::map_dbl(.data$method, \(m) worst(at$diff[at$method == m])),
      max_z = purrr::map_dbl(.data$method, \(m) worst(at$z_both[at$method == m] %||% NA))
    )
  ok <- if (draws) tab$max_z <= 2 else tab$max_diff <= tol
  main <- grepl("^common items", tab$method)
  tab$judgement <- dplyr::case_when(
    # 原文：這就是連結本身（比較的基準）
    main ~ "\u9019\u5c31\u662f\u9023\u7d50\u672c\u8eab\uff08\u6bd4\u8f03\u7684\u57fa\u6e96\uff09",
    # 原文：一致：差距在誤差範圍內
    ok ~ "\u4e00\u81f4\uff1a\u5dee\u8ddd\u5728\u8aa4\u5dee\u7bc4\u570d\u5167",
    # 原文：不一致：差距比誤差大
    TRUE ~ "\u4e0d\u4e00\u81f4\uff1a\u5dee\u8ddd\u6bd4\u8aa4\u5dee\u5927"
  )
  tab$method <- kitsui_check_method_zh(tab$method)
  if (!draws) tab$max_z <- NULL
  rule <- if (draws) {
    # 原文：判斷規則：和連結的差距不超過 2 個標準誤（連結誤差加上這個方法的抽樣誤差）時，算一致。
    "\u5224\u65b7\u898f\u5247\uff1a\u548c\u9023\u7d50\u7684\u5dee\u8ddd\u4e0d\u8d85\u904e 2 \u500b\u6a19\u6e96\u8aa4\uff08\u9023\u7d50\u8aa4\u5dee\u52a0\u4e0a\u9019\u500b\u65b9\u6cd5\u7684\u62bd\u6a23\u8aa4\u5dee\uff09\u6642\uff0c\u7b97\u4e00\u81f4\u3002"
  } else {
    # 原文：判斷規則：這條連結沒有重抽，差距不超過  | （目標量尺的單位）時，算一致。
    paste0("\u5224\u65b7\u898f\u5247\uff1a\u9019\u689d\u9023\u7d50\u6c92\u6709\u91cd\u62bd\uff0c\u5dee\u8ddd\u4e0d\u8d85\u904e ", tol, "\uff08\u76ee\u6a19\u91cf\u5c3a\u7684\u55ae\u4f4d\uff09\u6642\uff0c\u7b97\u4e00\u81f4\u3002")
  }
  structure(tab, rule = rule)
}

# kits_link_check() 的方法名稱翻成中文（月齡照原樣）
kitsui_check_method_zh <- function(method) {
  waves <- sub("^same-age groups, waves? ", "", method)
  dplyr::case_when(
    # 原文：共同題目（連結本身）
    grepl("^common items", method) ~ "\u5171\u540c\u984c\u76ee\uff08\u9023\u7d50\u672c\u8eab\uff09",
    # 原文：同年齡組（ |  個月）
    grepl("^same-age groups", method) ~ paste0("\u540c\u5e74\u9f61\u7d44\uff08", waves, " \u500b\u6708\uff09"),
    # 原文：兩個世代一起校準（concurrent CML）
    grepl("^concurrent CML", method) ~ "\u5169\u500b\u4e16\u4ee3\u4e00\u8d77\u6821\u6e96\uff08concurrent CML\uff09",
    TRUE ~ method
  )
}

# kits_check() 各項檢查的一句中文說明（照 check 的開頭比對；不認得的是 NA）
KITSUI_CHECK_EXPLAIN <- c(
  # 原文：保留的錨題點超過三分之二來自同一個領域。
  "^domain balance" = paste0("\u4fdd\u7559\u7684\u9328\u984c\u9ede\u8d85\u904e\u4e09\u5206\u4e4b\u4e8c\u4f86\u81ea\u540c\u4e00\u500b\u9818\u57df\u3002",
                             # 原文：這條共用的線主要由那個領域決定。
                             "\u9019\u689d\u5171\u7528\u7684\u7dda\u4e3b\u8981\u7531\u90a3\u500b\u9818\u57df\u6c7a\u5b9a\u3002"),
  # 原文：有領域單獨算的截距，和共用的截距差超過 2 個連結誤差。
  "^max \\|domain intercept" = paste0("\u6709\u9818\u57df\u55ae\u7368\u7b97\u7684\u622a\u8ddd\uff0c\u548c\u5171\u7528\u7684\u622a\u8ddd\u5dee\u8d85\u904e 2 \u500b\u9023\u7d50\u8aa4\u5dee\u3002",
                                     # 原文：一條共用的線不一定適合每個領域。
                                     "\u4e00\u689d\u5171\u7528\u7684\u7dda\u4e0d\u4e00\u5b9a\u9069\u5408\u6bcf\u500b\u9818\u57df\u3002"),
  # 原文：篩選拿掉的點超過一成。請在連結圖上看被拿掉的是哪些題。
  "^share dropped" = "\u7be9\u9078\u62ff\u6389\u7684\u9ede\u8d85\u904e\u4e00\u6210\u3002\u8acb\u5728\u9023\u7d50\u5716\u4e0a\u770b\u88ab\u62ff\u6389\u7684\u662f\u54ea\u4e9b\u984c\u3002",
  # 原文：篩選後留下來的共同題目步驟數。
  "^points kept" = "\u7be9\u9078\u5f8c\u7559\u4e0b\u4f86\u7684\u5171\u540c\u984c\u76ee\u6b65\u9a5f\u6578\u3002",
  # 原文：留下來的點在兩把尺上的相關，越接近 1 越好。
  "^correlation of kept points" = "\u7559\u4e0b\u4f86\u7684\u9ede\u5728\u5169\u628a\u5c3a\u4e0a\u7684\u76f8\u95dc\uff0c\u8d8a\u63a5\u8fd1 1 \u8d8a\u597d\u3002",
  # 原文：連結誤差：重抽時截距的標準差。
  "^linking error" = "\u9023\u7d50\u8aa4\u5dee\uff1a\u91cd\u62bd\u6642\u622a\u8ddd\u7684\u6a19\u6e96\u5dee\u3002",
  # 原文：同年齡組的檢核假設兩個世代同月齡的孩子來自同一個母群。
  "^same-age groups: assumption" = "\u540c\u5e74\u9f61\u7d44\u7684\u6aa2\u6838\u5047\u8a2d\u5169\u500b\u4e16\u4ee3\u540c\u6708\u9f61\u7684\u5b69\u5b50\u4f86\u81ea\u540c\u4e00\u500b\u6bcd\u7fa4\u3002",
  # 原文：同月齡的兩個世代對齊能力分布算出的線，和連結的差距。
  "^same-age groups" = "\u540c\u6708\u9f61\u7684\u5169\u500b\u4e16\u4ee3\u5c0d\u9f4a\u80fd\u529b\u5206\u5e03\u7b97\u51fa\u7684\u7dda\uff0c\u548c\u9023\u7d50\u7684\u5dee\u8ddd\u3002",
  # 原文：兩個世代一起校準算出的線，和連結差超過 2 個連結誤差。
  "^concurrent CML" = paste0("\u5169\u500b\u4e16\u4ee3\u4e00\u8d77\u6821\u6e96\u7b97\u51fa\u7684\u7dda\uff0c\u548c\u9023\u7d50\u5dee\u8d85\u904e 2 \u500b\u9023\u7d50\u8aa4\u5dee\u3002",
                             # 原文：兩種方法不一致。
                             "\u5169\u7a2e\u65b9\u6cd5\u4e0d\u4e00\u81f4\u3002"),
  # 原文：這個波次的測量誤差至少和觀察到的變異一樣大。
  "^sd_true is 0" = paste0("\u9019\u500b\u6ce2\u6b21\u7684\u6e2c\u91cf\u8aa4\u5dee\u81f3\u5c11\u548c\u89c0\u5bdf\u5230\u7684\u8b8a\u7570\u4e00\u6a23\u5927\u3002",
                          # 原文：這個波次分不出孩子之間的差別。
                          "\u9019\u500b\u6ce2\u6b21\u5206\u4e0d\u51fa\u5b69\u5b50\u4e4b\u9593\u7684\u5dee\u5225\u3002"),
  # 原文：同月齡兩個世代的平均差。
  "^cohorts at wave" = paste0("\u540c\u6708\u9f61\u5169\u500b\u4e16\u4ee3\u7684\u5e73\u5747\u5dee\u3002",
                              # 原文：接近 0 表示連結後兩個世代對得上。
                              "\u63a5\u8fd1 0 \u8868\u793a\u9023\u7d50\u5f8c\u5169\u500b\u4e16\u4ee3\u5c0d\u5f97\u4e0a\u3002"),
  # 原文：最大波次的真分數標準差，除以之前波次最大的。
  "^scale shrinkage" = paste0("\u6700\u5927\u6ce2\u6b21\u7684\u771f\u5206\u6578\u6a19\u6e96\u5dee\uff0c\u9664\u4ee5\u4e4b\u524d\u6ce2\u6b21\u6700\u5927\u7684\u3002",
                              # 原文：明顯小於 1 時，量尺在高年齡被壓縮。
                              "\u660e\u986f\u5c0f\u65bc 1 \u6642\uff0c\u91cf\u5c3a\u5728\u9ad8\u5e74\u9f61\u88ab\u58d3\u7e2e\u3002"),
  # 原文：信度低：這個波次的題目對孩子太容易或太難。
  "^reliability" = paste0("\u4fe1\u5ea6\u4f4e\uff1a\u9019\u500b\u6ce2\u6b21\u7684\u984c\u76ee\u5c0d\u5b69\u5b50\u592a\u5bb9\u6613\u6216\u592a\u96e3\u3002",
                           # 原文：很多孩子全對或全錯。
                           "\u5f88\u591a\u5b69\u5b50\u5168\u5c0d\u6216\u5168\u932f\u3002"),
  # 原文：不適配的列次比例超過 5%。
  "^share of misfit" = "\u4e0d\u9069\u914d\u7684\u5217\u6b21\u6bd4\u4f8b\u8d85\u904e 5%\u3002"
)

#' 檢查表加上一欄中文說明（explain）；沒有 check 欄時原樣回傳
#' @noRd
kitsui_check_explain <- function(tab) {
  if (!is.data.frame(tab) || !"check" %in% names(tab)) return(tab)
  hit <- purrr::map_chr(tab$check, \(x) {
    m <- which(purrr::map_lgl(names(KITSUI_CHECK_EXPLAIN), \(p) grepl(p, x)))
    if (length(m) == 0) NA_character_ else unname(KITSUI_CHECK_EXPLAIN[m[1]])
  })
  tab$explain <- hit
  tab
}
