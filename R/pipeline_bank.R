# 這一回合每個資料量尺（領域 × 世代）的每個波次，用哪一個量尺的題庫校準
#   預設照月齡的規則（kitsui_plan_wave()）；② 可以手動改（bank_choice）

#' 這一回合每個資料量尺的每個波次，用哪一個量尺的題庫
#'
#' 題庫「涵蓋」一個波次：固定檔的 waves.csv 裡，這個題庫校準過的波次最小到最大之間。
#' 規則：
#' 1. 自己的題庫是空的：自由校準；
#' 2. 自己的題庫涵蓋這個波次，或不知道它校準過哪些波次（舊的固定檔）：用自己的題庫；
#' 3. 否則，同一個領域另一個世代的題庫涵蓋這個波次、而且它到參照世代的內部連結已經凍結
#'    （參照世代自己的題庫不用連結）：借它的題庫。好幾個都可以時，先選參照世代的，再依名稱；
#' 4. 都不行：用自己的題庫。
#' 3. 只在波次的單位是月時用（比對的是同一個月齡）；單位是年或波次序號時，同一個數字的兩個
#'    世代年齡不一定一樣（例如調查年），所以不借，用自己的題庫（② 仍然可以手動選）。
#' 借的時候，這個波次的資料放到另一個世代的量尺上校準（例如 M03 的 60 個月放到 "motor M36"），
#' 新的步驟加進那個題庫，分數走那個量尺的換算路線；人的 cohort 欄不變。
#' @param bank_choice NULL 或具名字元向量（資料量尺 -> 題庫的量尺）：手動改這個資料量尺所有
#'   波次的題庫；只能選自己的，或 kitsui_bank_candidates() 裡的。空字串或 NA 表示照規則
#' @return tibble：data_scale（資料的量尺）、wave_m、scale（用哪一個量尺的題庫）、rule（規則選的）、
#'   borrowed、manual（手動改過、和規則不同）、why（給使用者看的一句話）、
#'   detail（借題庫或手動選擇時寫進 checks.csv 的說明；其他是 NA）
#' @noRd
kitsui_round_plan <- function(df, fixed, bank_choice = NULL) {
  order <- kitsui_scales(df)$scale
  dplyr::distinct(df, .data$domain, .data$cohort, .data$wave_m) |>
    dplyr::mutate(data_scale = scale_key(.data$domain, .data$cohort)) |>
    dplyr::arrange(match(.data$data_scale, order), .data$wave_m) |>
    dplyr::select("data_scale", "wave_m") |>
    purrr::pmap(\(data_scale, wave_m) kitsui_plan_wave(fixed, data_scale, wave_m)) |>
    purrr::list_rbind() |>
    kitsui_apply_choice(fixed, bank_choice)
}

# 一個資料量尺的一個波次：照規則選題庫
kitsui_plan_wave <- function(fixed, scale, wave) {
  co <- scale_cohort(scale)
  unit <- kitsui_wave_unit(fixed)
  # 月時寫「60 月」（和以前一樣）；年、波次序號是「2013 年」「第 3 波」
  w <- if (unit == "months") paste(format(wave), "月") else kitsui_wave_text(wave, unit)
  # tibble() 裡後面的欄會看到前面的欄，所以 borrowed 先算好
  out <- \(why, to = scale, detail = NA_character_) {
    borrowed <- to != scale
    tibble::tibble(data_scale = scale, wave_m = wave, scale = to, rule = to,
                   borrowed = borrowed, manual = FALSE, why = why, detail = detail)
  }
  if (!scale %in% fixed$bank$scale) return(out("自己的題庫是空的：自由校準"))
  own <- kitsui_covers(fixed, scale, wave)
  word <- kitsui_wave_word(unit)
  if (is.na(own)) return(out(sprintf("不知道自己的題庫校準過哪些%s（舊的固定檔）：用自己的題庫", word)))
  if (own) {
    return(out(sprintf("%s 題庫有校準過這個%s（%s）", co, word,
                       kitsui_wave_range_text(fixed, scale, unit))))
  }
  if (unit != "months") {
    return(out(sprintf(paste0("自己的題庫沒有校準過%s；波次的單位是%s，",
                              "借另一世代題庫的規則只用在月齡：用自己的題庫"), w, word)))
  }
  same <- unique(fixed$bank$scale[scale_domain(fixed$bank$scale) == scale_domain(scale)])
  covering <- same[same != scale & purrr::map_lgl(same, \(o) isTRUE(kitsui_covers(fixed, o, wave)))]
  hit <- intersect(kitsui_bank_candidates(fixed, scale), covering)
  if (length(covering) == 0) return(out(sprintf("沒有題庫校準過 %s：用自己的題庫", w)))
  if (length(hit) == 0) {
    return(out(sprintf("%s 題庫有校準過 %s，但還沒有到參照世代的內部連結：用自己的題庫",
                       paste(scale_cohort(covering), collapse = "、"), w)))
  }
  to <- hit[1]
  why <- sprintf("%s 題庫沒有校準過 %s；%s 題庫有（%s）", co, w, scale_cohort(to),
                 kitsui_wave_range_text(fixed, to, unit))
  detail <- sprintf("%s %s months on %s bank (it covers %s months); rule default", co,
                    format(wave), to, kitsui_wave_range(fixed, to))
  out(why, to, detail)
}

#' 可以借的題庫：同一個領域、別的世代、題庫不是空的，而且到參照世代的內部連結已經凍結
#' （參照世代自己的題庫不用連結）。參照世代的排第一，其他依名稱
#' @noRd
kitsui_bank_candidates <- function(fixed, scale) {
  ref <- scale_key(scale_domain(scale), fixed$version$reference)
  banks <- sort(unique(fixed$bank$scale))
  banks <- banks[scale_domain(banks) == scale_domain(scale) & banks != scale]
  internal <- fixed$links[fixed$links$kind == "internal", ]
  linked <- banks == ref | banks %in% internal$from_scale[internal$to_scale == ref]
  banks <- banks[linked]
  c(intersect(ref, banks), setdiff(banks, ref))
}

#' 題庫有沒有涵蓋這個波次（校準過的波次最小到最大之間）；不知道時 NA
#' @noRd
kitsui_covers <- function(fixed, scale, wave) {
  w <- fixed$waves$wave_m[fixed$waves$scale == scale]
  if (length(w) == 0 || anyNA(w)) return(NA)
  wave >= min(w) && wave <= max(w)
}

#' 題庫校準過的波次範圍，例如 "36-96"（sep 是中間的符號）
#' @noRd
kitsui_wave_range <- function(fixed, scale, sep = "-") {
  w <- fixed$waves$wave_m[fixed$waves$scale == scale]
  if (length(w) == 0 || anyNA(w)) return("unknown")
  paste(unique(format(range(w), trim = TRUE)), collapse = sep)
}

#' 題庫校準過的波次範圍給使用者看的寫法，例如「36–96 月」「2009–2013 年」「第 1–3 波」
#' @noRd
kitsui_wave_range_text <- function(fixed, scale, unit = kitsui_wave_unit(fixed)) {
  r <- kitsui_wave_range(fixed, scale, "–")
  switch(unit, years = paste0(r, " 年"), waves = paste0("第 ", r, " 波"), paste0(r, " 月"))
}

#' 手動選的題庫：只能選自己的，或可以借的題庫；和規則不同時記下來
#' @noRd
kitsui_apply_choice <- function(plan, fixed, bank_choice) {
  bank_choice <- bank_choice[!is.na(bank_choice) & nzchar(bank_choice)]
  if (length(bank_choice) == 0) return(plan)
  extra <- setdiff(names(bank_choice), plan$data_scale)
  if (length(extra) > 0) {
    kitDesign::kit_abort("這一回合的資料沒有 ", paste(extra, collapse = "、"), "，不能選它的題庫。")
  }
  for (s in names(bank_choice)) {
    to <- bank_choice[[s]]
    ok <- c(s, kitsui_bank_candidates(fixed, s))
    if (!to %in% ok) {
      kitDesign::kit_abort(s, " 不能用 ", to, " 的題庫。可以選：", paste(ok, collapse = "、"), "。")
    }
    i <- which(plan$data_scale == s & plan$rule != to)
    plan$scale[i] <- to
    plan$borrowed[i] <- to != s
    plan$manual[i] <- TRUE
    plan$why[i] <- sprintf("手動選擇：用 %s 的題庫（規則是 %s）", to, plan$rule[i])
    plan$detail[i] <- sprintf("%s %s %s on %s bank; manual choice (rule: %s)",
                              scale_cohort(s), format(plan$wave_m[i]),
                              kitsui_wave_unit_en(kitsui_wave_unit(fixed)), to, plan$rule[i])
  }
  plan
}

#' ② 選題庫的表：每個資料量尺一列（波次、規則選的、原因、可以選的題庫）
#'
#' @param plan kitsui_round_plan()（照規則）
#' @return tibble：data_scale、waves、rule（各波次規則都一樣時是那個量尺，不一樣時是 ""）、why、
#'   choices（list；具名字元向量，名稱是給使用者看的文字）
#' @noRd
kitsui_bank_options <- function(plan, fixed) {
  purrr::map(unique(plan$data_scale), \(s) {
    rows <- plan[plan$data_scale == s, ]
    rule <- unique(rows$rule)
    own <- if (s %in% fixed$bank$scale) paste0(s, "（自己的題庫）") else "自由校準（自己的題庫是空的）"
    other <- kitsui_bank_candidates(fixed, s)
    choices <- stats::setNames(c(s, other), c(own, sprintf("%s（借用另一世代題庫）", other)))
    if (length(rule) > 1) choices <- c("依月齡的規則（各波次不同）" = "", choices)
    tibble::tibble(data_scale = s, waves = paste(format(rows$wave_m), collapse = ", "),
                   rule = if (length(rule) == 1) rule else "",
                   why = paste(unique(rows$why), collapse = "；"), choices = list(choices))
  }) |>
    purrr::list_rbind()
}

#' ② 選題庫的表的 why 欄：跟著使用者的選擇（和規則不同時說是手動選的，規則是什麼）
#'
#' @param b kitsui_bank_options()
#' @param pick 每一列選的題庫（和 b 一樣長；"" 或 NA 是照規則）
#' @return 字元向量，每列一句
#' @noRd
kitsui_bank_why <- function(b, pick) {
  pick <- dplyr::coalesce(pick, "")
  manual <- nzchar(pick) & pick != b$rule
  rule <- dplyr::if_else(nzchar(b$rule), paste("規則是", b$rule), "規則依月齡，各波次不同")
  free <- purrr::map2_lgl(b$choices, pick, \(ch, p) startsWith(names(ch)[match(p, ch)] %||% "",
                                                              "自由校準"))
  text <- dplyr::if_else(free, sprintf("手動選擇：自由校準（%s）", rule),
                         sprintf("手動選擇：用 %s 的題庫（%s）", pick, rule))
  dplyr::if_else(manual, text, b$why)
}
