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
      text <- paste0("題庫裡還沒有 ", domain, " 參照世代（", fixed$version$reference,
                     "）以外的量尺。")
      return(list(fixed = fixed, link = NULL, new = FALSE, message = text))
    }
    if (length(cohort) > 1) {
      kit_abort(domain, " 有好幾個世代可以做內部連結（", paste(cohort, collapse = "、"), "），",
                "請指定是哪一個世代。")
    }
  }
  from <- paste(domain, cohort)
  to <- paste(domain, fixed$version$reference)
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
  if (nrow(b) == 0) kit_abort("固定檔裡沒有 ", scale, " 的題目參數，請先做這個量尺的題目校準。")
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
    return(list(fixed = fixed, link = old, new = FALSE, message = "沿用固定檔裡的連結。"))
  }
  link <- tryCatch(make(), error = function(e) e)
  if (inherits(link, "error")) {
    return(list(fixed = fixed, link = NULL, new = FALSE, message = conditionMessage(link)))
  }
  round <- utils::tail(fixed$version$rounds, 1)
  fixed$links <- dplyr::bind_rows(fixed$links, kitsui_link_rows(link, kind, round))
  list(fixed = fixed, link = link, new = TRUE, message = "已建立連結，存進固定檔。")
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
  from <- strsplit(r$from_scale[1], ", ")[[1]]
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
  from <- if (any(d)) strsplit(fixed$links$from_scale[which(d)[1]], ", ")[[1]]
  new <- setdiff(kitsui_ref_scales(fixed), from)
  if (!any(d) || length(new) == 0) return(list(fixed = fixed, log = NULL))
  fixed$links$from_scale[d] <- paste(c(from, new), collapse = ", ")
  log <- tibble::tibble(
    round = round, scale = new, item_id = NA_character_, check = "D extended",
    detail = paste0("外部連結延伸到 ", new, "：同一條線（截距、斜率、點都不變），沒有重新估計"),
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
    nrow(d) == 0 ~ "還沒有外部連結",
    anchors$scale %in% ext$scale ~
      paste0("第 ", ext$round[match(anchors$scale, ext$scale)], " 回合延伸（同一條線）"),
    TRUE ~ paste0("建立外部連結時（第 ", d$round[1], " 回合）")
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
  to_d <- nrow(d) > 0 && now %in% strsplit(d$from_scale[1], ", ")[[1]]
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
  words <- c(frozen = "已凍結", ready = "可以建立", missing = "還不能做（題庫缺一邊）")
  state <- purrr::map_chr(scales, \(s) kitsui_internal_state(fixed, s, links, earlier)$state)
  tibble::tibble(from = scales, to = paste(scale_domain(scales), fixed$version$reference),
                 state = unname(words[state]))
}

# ③ 的標題與說明：用固定檔裡的參照世代與其他世代的名稱（不寫死 M03、M36） ----

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
  from <- if (length(co$others) == 0) "其他世代" else paste(co$others, collapse = "、")
  paste0("1. 內部連結：", from, " → ", co$ref)
}

#' 內部連結的說明（一段話）
#' @noRd
kitsui_internal_hint <- function(fixed) {
  co <- kitsui_link_cohorts(fixed)
  from <- if (length(co$others) == 0) "參照世代以外的世代" else paste(co$others, collapse = "、")
  each <- if (length(co$others) > 1) "每個世代、每個領域各一條。" else "每個領域一條。"
  paste0("各世代的題目參數各自在自己的量尺上。",
         "內部連結用兩邊共同的題目，把 ", from, " 的量尺換到參照世代 ", co$ref, " 的量尺。", each)
}

#' 外部連結的標題，例如「2. 外部連結：M03 → D」；不適用時加「（不適用）」
#' @param applicable kitsui_d_applicable()
#' @noRd
kitsui_d_title <- function(fixed, applicable = TRUE) {
  paste0("2. 外部連結：", kitsui_link_cohorts(fixed)$ref, " → D", if (!applicable) "（不適用）")
}

#' 外部連結的說明（一段話）
#' @noRd
kitsui_d_hint <- function(fixed, applicable = TRUE) {
  ref <- kitsui_link_cohorts(fixed)$ref
  if (!applicable) {
    return(paste0("D 分數是嬰幼兒發展的國際量尺（GSED），錨題只有 lan、motor、cog 的題目。",
                  "參照世代（", ref, "）的題目裡沒有錨題，所以這份資料不做外部連結。",
                  "分數留在參照世代（", ref, "）的量尺上。這不是錯誤。"))
  }
  paste0("D 分數是國際共用的發展量尺。",
         "參照世代（", ref, "）的題目裡，有些是錨題：它們在 D 量尺上的位置已知。",
         "外部連結用錨題把 ", ref, " 的量尺換到 D。所有領域共用一條。",
         "之後才進題庫的 ", ref, " 量尺，沿用同一條外部連結。")
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
  to <- paste(scale_domain(scale), fixed$version$reference)
  link <- links[[scale]]
  if (!is.null(link)) {
    return(list(state = "frozen", link = link, text = kitsui_frozen_text(scale %in% earlier)))
  }
  miss <- setdiff(c(scale, to), fixed$bank$scale)
  if (length(miss) > 0) {
    text <- paste0("題庫裡還沒有 ", paste(miss, collapse = "、"), " 的題目參數。",
                   "兩個世代都做過題目校準後，才能建立這條連結。")
    return(list(state = "missing", link = NULL, text = text))
  }
  list(state = "ready", link = NULL,
       text = paste0("題庫裡 ", scale, " 與 ", to, " 都有了，可以建立連結。"))
}

kitsui_frozen_text <- function(from_earlier) {
  when <- if (from_earlier) "之前的回合就做好了（在這一回合開始時的固定檔裡）" else "這一回合剛做好"
  paste0("這條連結", when, "，已經凍結。這一回合直接沿用，不會重做。")
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
    text <- paste0("不適用：參照世代（", ref, "）的量尺（", paste(anchors$scale, collapse = "、"),
                   "）沒有 D 分數的錨題（kits_anchors()），所以不做外部連結。",
                   "分數留在參照世代（", ref, "）的量尺上；這一回合照常完成。")
    return(list(state = "na", link = NULL, text = text))
  }
  if (nrow(anchors) == 0) {
    text <- paste0("題庫裡還沒有參照世代（", fixed$version$reference, "）的量尺。",
                   "先做參照世代的題目校準。")
    return(list(state = "missing", link = NULL, text = text))
  }
  if (n < 3) {
    text <- paste0("錨題只有 ", n, " 個，kits 至少要 3 個，所以還不能做外部連結。",
                   "kits 的訊息：", kitsui_link_D(fixed)$message)
    return(list(state = "missing", link = NULL, text = text))
  }
  text <- paste0("錨題共 ", n, " 個，可以建立外部連結。",
                 "外部連結會用上面所有量尺：", paste(anchors$scale, collapse = "、"), "。",
                 "之後才進題庫的參照世代量尺，會沿用同一條外部連結，不重新估計。")
  list(state = "ready", link = NULL, text = text)
}

#' 獨立檢核做不了的原因：要這一回合兩個世代的作答與校準；NULL 表示可以做
#' @noRd
kitsui_link_check_why <- function(round, link) {
  scales <- c(link$from_scale, link$to_scale)
  have <- scales %in% names(round$items) & scales %in% names(round$data)
  if (all(have)) return(NULL)
  paste0("這一回合的題目校準沒有 ", paste(scales[!have], collapse = "、"), "。",
         "獨立檢核要用兩個世代這一回合的作答，所以這次不能做。",
         "下次兩個世代的資料一起進來時就能做。")
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
      link = n, kind = if (n == "D") "外部連結" else "內部連結",
      from = paste(l$from_scale, collapse = "、"), to = l$to_scale,
      intercept = co[["intercept"]], slope = co[["slope"]],
      made = if (n %in% earlier) "之前的回合" else "這一回合"
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(round = made_in)
}

#' 一條連結的公式：新的值 = intercept + slope × 舊的值，和兩個係數的 linking error
#' @return list(formula = 一行文字, table = tibble（term、estimate、linking_error）)
#' @noRd
kitsui_link_formula <- function(link) {
  from <- paste(unique(link$from_scale), collapse = " 或 ")
  formula <- sprintf("%s = %.2f + %.2f × %s", link$to_scale, link$intercept, link$slope, from)
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
    main ~ "這就是連結本身（比較的基準）",
    ok ~ "一致：差距在誤差範圍內",
    TRUE ~ "不一致：差距比誤差大"
  )
  tab$method <- kitsui_check_method_zh(tab$method)
  if (!draws) tab$max_z <- NULL
  rule <- if (draws) {
    "判斷規則：和連結的差距不超過 2 個標準誤（連結誤差加上這個方法的抽樣誤差）時，算一致。"
  } else {
    paste0("判斷規則：這條連結沒有重抽，差距不超過 ", tol, "（目標量尺的單位）時，算一致。")
  }
  structure(tab, rule = rule)
}

# kits_link_check() 的方法名稱翻成中文（月齡照原樣）
kitsui_check_method_zh <- function(method) {
  waves <- sub("^same-age groups, waves? ", "", method)
  dplyr::case_when(
    grepl("^common items", method) ~ "共同題目（連結本身）",
    grepl("^same-age groups", method) ~ paste0("同年齡組（", waves, " 個月）"),
    grepl("^concurrent CML", method) ~ "兩個世代一起校準（concurrent CML）",
    TRUE ~ method
  )
}

# kits_check() 各項檢查的一句中文說明（照 check 的開頭比對；不認得的是 NA）
KITSUI_CHECK_EXPLAIN <- c(
  "^domain balance" = paste0("保留的錨題點超過三分之二來自同一個領域。",
                             "這條共用的線主要由那個領域決定。"),
  "^max \\|domain intercept" = paste0("有領域單獨算的截距，和共用的截距差超過 2 個連結誤差。",
                                     "一條共用的線不一定適合每個領域。"),
  "^share dropped" = "篩選拿掉的點超過一成。請在連結圖上看被拿掉的是哪些題。",
  "^points kept" = "篩選後留下來的共同題目步驟數。",
  "^correlation of kept points" = "留下來的點在兩把尺上的相關，越接近 1 越好。",
  "^linking error" = "連結誤差：重抽時截距的標準差。",
  "^same-age groups: assumption" = "同年齡組的檢核假設兩個世代同月齡的孩子來自同一個母群。",
  "^same-age groups" = "同月齡的兩個世代對齊能力分布算出的線，和連結的差距。",
  "^concurrent CML" = paste0("兩個世代一起校準算出的線，和連結差超過 2 個連結誤差。",
                             "兩種方法不一致。"),
  "^sd_true is 0" = paste0("這個波次的測量誤差至少和觀察到的變異一樣大。",
                          "這個波次分不出孩子之間的差別。"),
  "^cohorts at wave" = paste0("同月齡兩個世代的平均差。",
                              "接近 0 表示連結後兩個世代對得上。"),
  "^scale shrinkage" = paste0("最大波次的真分數標準差，除以之前波次最大的。",
                              "明顯小於 1 時，量尺在高年齡被壓縮。"),
  "^reliability" = paste0("信度低：這個波次的題目對孩子太容易或太難。",
                           "很多孩子全對或全錯。"),
  "^share of misfit" = "不適配的列次比例超過 5%。"
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
