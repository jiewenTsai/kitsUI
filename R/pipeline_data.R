# 資料：示範資料、kits 不知道的欄位（domain、cohort）、量尺清單、作答矩陣、這一回合的名稱
# 其他檢查（作答分數、重複列…）交給 kits_resp() 與 kits_check()

# 預設的參照世代（內部連結的目標、外部連結的起點）：資料有 M03 時（KIT）；
# 第一回合在 ① 選（kitsui_ref_pick()），寫進固定檔的 VERSION
KITSUI_REF_COHORT <- "M03"

# 波次（wave_m）的單位：第一回合在 ① 選，寫進固定檔的 VERSION（WaveUnit），之後的回合沿用。
# 之前的固定檔沒有 WaveUnit：當成月（KIT 的 wave_m 是施測月齡）。
# 借題庫的規則（比對同一個月齡）與 age50 只在單位是月時用
KITSUI_WAVE_UNITS <- c("月（施測月齡）" = "months", "年" = "years", "波次序號" = "waves")

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
  switch(unit, years = paste0(w, " 年"), waves = paste0("第 ", w, " 波"), paste0(w, " 個月"))
}

#' 好幾個波次一起寫：「3、6 個月」「2009、2011 年」「第 1、2 波」
#' @noRd
kitsui_waves_text <- function(w, unit = "months") {
  w <- paste(format(w, trim = TRUE), collapse = "、")
  switch(unit, years = paste0(w, " 年"), waves = paste0("第 ", w, " 波"), paste0(w, " 個月"))
}

#' 句子裡的「月齡」：月是「月齡」，年是「年」，波次序號是「波次」
#' @noRd
kitsui_wave_word <- function(unit = "months") {
  switch(unit, years = "年", waves = "波次", "月齡")
}

#' 圖上 x 軸（英文）的波次：月是 "months"，年是 "years"，波次序號是 "wave number"
#' @noRd
kitsui_wave_unit_en <- function(unit = "months") {
  switch(unit, years = "years", waves = "wave number", "months")
}

# 資料一定要有的欄位（資料約定）
KITSUI_REQUIRED <- c("visit_id", "cohort", "wave_m", "item_id", "item_score", "domain")

# 資料約定的欄位：讀檔時的型別（readr 的縮寫：c 文字、d 數字）、一定要有嗎、意思
KITSUI_COLUMNS <- tibble::tribble(
  ~column, ~type, ~meaning,
  "visit_id", "c", "列次編號：一個孩子的一個波次一個編號。不同世代也不能重複。",
  "subj_id", "c", "孩子編號：同一個孩子的各波次相同。",
  "cohort", "c", "世代，例如 M03、M36。參照世代在第一回合選（有 M03 時預設 M03）。",
  "sex", "c", "性別，例如 boy、girl。",
  "wave_m", "d", "波次，例如施測月齡 3、6、12。也可以是年或波次序號（在 ① 選單位）。",
  "age_m", "d", "實際月齡。題目表的通過年齡要用它。",
  "item_id", "c", "題號，例如 lana01。",
  "item_score", "d", "得分：0、1、2…。沒有作答的題不用放。",
  "domain", "c", "領域，例如 lan、motor、cog。外部連結的錨題用 lan、motor、cog 這三個名稱。"
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
    kit_abort("資料缺少 ", paste(miss, collapse = "、"), " 欄位。請加上後再上傳。")
  }
  blank <- \(x) anyNA(x) || any(!nzchar(trimws(x)))
  if (blank(df$domain) || blank(df$cohort)) {
    kit_abort("domain 或 cohort 有空白。每一列都要填領域與世代。")
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
    return("這一回合的名稱只能用英文字母與數字，不能空白。")
  }
  if (label %in% rounds) {
    return(paste0("這一回合的名稱「", label, "」和固定檔裡之前的回合重複了。請換一個。"))
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
    kit_abort("兩個世代有相同的 visit_id（例如 ", dup[1], "），不能放進同一個作答矩陣。",
              "請讓每個列次的 visit_id 都不一樣，例如前面加上世代。")
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
    return(list(problems = paste0("資料缺少這些欄位：", paste(miss, collapse = "、"),
                                  "。請加上後再上傳。", kitsui_missing_hint(names(data)))))
  }
  # 約定以外的欄拿掉：kits_resp() 要一個列次裡每一欄都一樣，多的欄會讓每個量尺都停下來
  dropped <- setdiff(names(data), KITSUI_COLUMNS$column)
  if (length(dropped) > 0) data <- data[intersect(names(data), KITSUI_COLUMNS$column)]
  choices <- choices %||% kitsui_pick_choices(data)
  sel <- kitsui_parse_picks(picks, choices)
  if (nrow(sel) == 0) return(list(problems = character(), hint = "請勾這一回合的世代與波次。"))
  # wave_m 空白的列不知道是哪一個波次：勾選框沒有它們，全部都勾時也不放進來
  no_wave <- sum(is.na(data$wave_m))
  df <- if (nrow(sel) == nrow(choices) && no_wave == 0) data else {
    dplyr::semi_join(data, sel[c("cohort", "wave_m")], by = c("cohort", "wave_m"))
  }
  problems <- tryCatch({
    kitsui_check_data(df)
    character()
  }, error = \(e) kit_error_text(e, "檢查資料"))
  if (length(problems) == 0) problems <- kitsui_quick_check(df)
  if (length(problems) > 0) return(list(problems = problems))
  notes <- c(
    if (no_wave > 0) {
      sprintf("wave_m 有 %s 列是空白：不知道是哪一個波次，這些列不放進這一回合。",
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
  paste0(paste(utils::head(x, n), collapse = "、"), if (length(x) > n) "…")
}

#' 缺欄位時多一句：看起來是分號或 Tab 分隔、欄名是大寫或前後有空白
#' @param cols 檔案的欄名
#' @return 一句中文；看不出原因時 ""
#' @noRd
kitsui_missing_hint <- function(cols) {
  save_as <- "請另存成逗號分隔的 CSV（Excel 的「CSV UTF-8（逗號分隔）」）再上傳。"
  if (length(cols) == 1 && grepl(";", cols)) {
    return(paste0("看起來是用分號（;）分隔的檔案。", save_as))
  }
  if (length(cols) == 1 && grepl("\t", cols)) {
    return(paste0("看起來是用 Tab 分隔的檔案。", save_as))
  }
  near <- cols[!cols %in% KITSUI_COLUMNS$column & tolower(trimws(cols)) %in% KITSUI_COLUMNS$column]
  if (length(near) == 0) return("")
  paste0("欄名要全部小寫、前後不能有空白：", kitsui_first(paste0("「", near, "」"), 4), " 要改成 ",
         kitsui_first(tolower(trimws(near)), 4), "。")
}

#' 看起來是遺漏碼的分數：一題裡比其他分數大很多（差 50 以上）、而且是 90 以上的分數，
#' 例如 0–3 分的題有 95–99，或有 7777、9999
#'
#' 只提醒（橘色），分數不改：kits 會把它們當成很高的分數，那一題會分數不連續，
#' 量尺通常會停下來。遺漏碼要先在 kitJoin ② 轉成空白。
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
  each <- purrr::imap_chr(hits, \(x, id) paste0(id, "（", kitsui_first(format(x, trim = TRUE), 4), "）"))
  paste0(
    length(hits), " 題有看起來是遺漏碼的分數（比其他分數大很多）：",
    kitsui_some(each, 3), "。這些值是：", kitsui_first(format(codes, trim = TRUE), 8), "。",
    "kits 會把它們當成真的分數，這些題會分數不連續，量尺可能停下來。",
    "請先在 kitJoin「② 遺漏值轉 NA」把遺漏碼改成空白，再上傳。"
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
  paste0("這些波次之前的回合已經校準過：",
         paste(sprintf("%s 的 %s（第 %s 回合）", hit$data_scale, kitsui_wave_text(hit$wave_m, unit),
                       hit$round), collapse = "、"),
         "。再校準一次，同一批孩子會有兩份分數。")
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
  sprintf(paste0("%s：%s 的題庫裡沒有這一回合的任何一題（題號都不一樣），② 會停下來。",
                 "請確認題號和之前的回合相同；或在 ② 把它選成「這一回合不做」。"),
          use$data_scale[bad], use$scale[bad])
}

#' 固定檔不是這次連線最新的一回合（④ 交過更新的一回合時）
#' @param rounds 現在的固定檔裡的回合
#' @param latest NULL 或這次連線最新的一回合
#' @noRd
kitsui_note_stale <- function(rounds, latest) {
  if (is.null(latest) || latest %in% rounds) return(character())
  have <- if (length(rounds) == 0) "現在沒有用固定檔" else {
    paste0("這個固定檔只到第 ", utils::tail(rounds, 1), " 回合")
  }
  paste0(have, "，這次連線已經做到第 ", latest, " 回合。",
         "要接著做，請用第 ", latest, " 回合的固定檔。")
}

#' 資料檔之前的回合用過（md5 在固定檔的 sources 裡）
#' @param files tibble：file、md5（kitsui_read_uploads()$files）
#' @noRd
kitsui_note_sources <- function(files, fixed) {
  if (is.null(files) || nrow(files) == 0) return(character())
  hit <- dplyr::inner_join(files[c("file", "md5")], fixed$sources[c("round", "md5")],
                           by = "md5", relationship = "many-to-many")
  if (nrow(hit) == 0) return(character())
  hit <- dplyr::summarise(hit, round = paste(unique(.data$round), collapse = "、"), .by = "file")
  sprintf("%s：這個檔案在第 %s 回合用過。", hit$file, hit$round)
}

#' 約定以外的欄（kitsui_read_uploads()、kitsui_data_select() 拿掉的）
#' @noRd
kitsui_note_dropped <- function(dropped) {
  if (length(dropped) == 0) return(character())
  paste0("資料約定以外的欄不用，已經拿掉：", kitsui_first(dropped, 8), "。",
         "kitsUI 只用「資料檔要有哪些欄位」表上的欄。")
}

#' 得分不完整的題（kitsui_category_problems()）：每題一句；太多時只列前 n 題
#' @param problems tibble：scale、item_id、problem、detail_zh（detail_zh 已經有題號）
#' @noRd
kitsui_note_categories <- function(problems, n = 12) {
  if (is.null(problems) || nrow(problems) == 0) return(character())
  out <- sprintf("%s：%s。", problems$scale, sub("。$", "", problems$detail_zh))
  if (length(out) <= n) return(out)
  c(out[seq_len(n)], sprintf("……還有 %d 題。", length(out) - n))
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
    out <- c(out, paste0("同一列次同一題有好幾列（共 ", sum(dup), " 列），例如 ",
                         df$visit_id[i], " 的 ", df$item_id[i], "。每個列次每一題只能有一列。"))
  }
  x <- df$item_score[!is.na(df$item_score)]
  if (any(x < 0 | x != round(x))) {
    out <- c(out, "item_score 要是 0 以上的整數（0、1、2…）。請檢查資料。")
  }
  ids <- tryCatch({
    kitsui_check_ids(df)
    character()
  }, error = \(e) kit_error_text(e, "檢查資料"))
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
  if (is.null(files) || nrow(files) == 0) kit_abort("請選擇資料檔（CSV、CSV.GZ 或 SAV）。")
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
    kit_abort(name, if (ncol(r$data) == 0) " 是空的檔案" else " 只有欄名，沒有作答的列",
              "。請確認匯出的檔案有資料再上傳。")
  }
  if (r$n_problems > 0) {
    p <- r$problems[1, ]
    kit_abort(name, " 有 ", r$n_problems, " 個值讀不懂。",
              "例如第 ", p$row, " 列的 ", names(r$data)[p$col], " 是「", p$actual, "」，",
              "應該是", if (grepl("double|number", p$expected)) "數字" else p$expected, "。",
              "請修正後再上傳。")
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
  kind <- \(x) if (all(is.na(x))) NA_character_ else if (is.numeric(x)) "數字" else "文字"
  for (name in names(datas)[-1]) {
    d <- datas[[name]]
    miss <- setdiff(names(ref), names(d))
    extra <- setdiff(names(d), names(ref))
    if (length(miss) + length(extra) > 0) {
      kit_abort("檔案 ", name, " 的欄位和 ", first, " 不一樣。",
                if (length(miss) > 0) paste0("少了：", paste(miss, collapse = "、"), "。"),
                if (length(extra) > 0) paste0("多了：", paste(extra, collapse = "、"), "。"),
                "每個檔案的欄位要一樣。")
    }
    a <- purrr::map_chr(ref, kind)
    b <- purrr::map_chr(d[names(ref)], kind)
    bad <- which(a != b)
    if (length(bad) > 0) {
      col <- names(ref)[bad[1]]
      kit_abort("檔案 ", name, " 的 ", col, " 欄是", b[[bad[1]]], "，", first, " 的是",
                a[[bad[1]]], "。每個檔案同一欄的型別要一樣。")
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
  dplyr::transmute(KITSUI_COLUMNS, .data$column, required = ifelse(.data$required, "是", "否"),
                   .data$meaning)
}
