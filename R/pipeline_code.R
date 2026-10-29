# 重現每一步的 R 程式碼（kits 的呼叫，用使用者的設定與檔名）
# 每個 kitsui_code_*() 回傳一段字串；三段依序貼上就能跑（round -> link -> score）

#' 字串轉成 R 的寫法："a" 或 c("a", "b")
#' @noRd
kitsui_r <- function(x) {
  q <- if (is.character(x)) paste0('"', x, '"') else format(x, trim = TRUE)
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

# 連結在程式裡的物件名稱："lan M36" -> link_lan_M36
kitsui_link_var <- function(name) paste0("link_", gsub("[^A-Za-z0-9]", "_", name))

# 讀資料的程式：上傳的檔案（一個或好幾個，readr 會接起來）或示範資料（kitm03、kitm36），
# 再選這一輪：picks（勾到的世代 × 波次，cohort、wave_m）有給時照組合選（semi_join），
# 沒有時照波次選（waves）
kitsui_code_read <- function(data_file = NULL, waves = NULL, picks = NULL) {
  read <- if (is.null(data_file)) {
    "responses <- bind_rows(kitm03$responses, kitm36$responses)"
  } else if (length(data_file) == 1) {
    sprintf("responses <- readr::read_csv(%s)", kitsui_r(data_file))
  } else {
    files <- paste0('  "', data_file, '"', c(rep(",", length(data_file) - 1), ""))
    paste(c("responses <- readr::read_csv(c(", files, "))"), collapse = "\n")
  }
  if (NROW(picks) > 0) return(paste(read, kitsui_code_picks(picks), sep = "\n"))
  pick <- if (length(waves) > 0) sprintf(" |>\n  filter(wave_m %%in%% %s)", kitsui_r(waves))
  paste0(read, pick)
}

# 勾到的世代 × 波次寫成一張表，再 semi_join（只勾 M03 36 時不會帶進 M36 36）
kitsui_code_picks <- function(picks) {
  picks <- dplyr::distinct(as.data.frame(picks)[c("cohort", "wave_m")])
  c(
    "# 這一輪勾的世代 × 波次",
    sprintf("picks <- tibble(cohort = %s,", kitsui_r(as.character(picks$cohort))),
    sprintf("                wave_m = %s)", kitsui_r(as.numeric(picks$wave_m))),
    'responses <- semi_join(responses, picks, by = c("cohort", "wave_m"))'
  ) |>
    paste(collapse = "\n")
}

#' ① 的程式碼：讀資料、選波次、一個量尺的作答設計與檢查
#' @param data_file 上傳的檔名；NULL 時用示範資料
#' @param picks NULL 或勾到的世代 × 波次（cohort、wave_m 的表）；有給時照組合選
#' @noRd
kitsui_code_data <- function(scale, data_file = NULL, waves = NULL, picks = NULL) {
  waves <- as.numeric(waves)
  paste(c(
    "library(kits)", "library(dplyr)", kitsui_code_read(data_file, waves, picks),
    "count(responses, domain, cohort)",
    sprintf("resp <- kits_resp(filter(responses, paste(domain, cohort) == %s))", kitsui_r(scale)),
    "plot(resp)", "kits_check(resp = resp)"
  ), collapse = "\n")
}

#' 一輪的程式碼：讀資料、一個量尺的矩陣，自由校準或（有題庫時）偏移把關 + 固定校準
#'
#' @param fixed 上一輪的固定檔（決定要自由還是固定校準）；fixed_file 是它的 zip 檔名
#' @param data_file 資料檔名（一個或好幾個）；NULL 時用示範資料 kitm03、kitm36；
#'   waves 是這一輪的波次；picks（cohort、wave_m 的表）有給時照勾到的世代 × 波次選
#' @param plan NULL 或這一輪的 kitsui_round_plan()：有借用另一世代題庫（或一個資料量尺的波次分到
#'   不同題庫）時，矩陣用校準到這個量尺的資料量尺與波次
#' @noRd
kitsui_code_round <- function(scale, round_label, fixed = NULL, fixed_file = "fixed_set.zip",
                              data_file = NULL, waves = NULL, flag_share = 0.2, flag_fixed = 5,
                              plan = NULL, picks = NULL) {
  s <- kitsui_r(scale)
  head <- c("library(kits)", "library(dplyr)", kitsui_code_read(data_file, waves, picks))
  matrix <- c(kitsui_code_pick(scale, plan), 'X <- as.matrix(resp[attr(resp, "items")])',
              "rownames(X) <- resp$visit_id")
  if (is.null(fixed) || !scale %in% fixed$bank$scale) {
    free <- sprintf("item <- kits_item(X, scale = %s)", s)
    return(paste(c(head, matrix, "# 沒有題庫：自由校準", free, "coef(item)"), collapse = "\n"))
  }
  paste(c(
    head,
    sprintf('unzip(%s, exdir = "fixed_set", junkpaths = TRUE)', kitsui_r(fixed_file)),
    sprintf('bank <- filter(read.csv("fixed_set/bank.csv"), scale == %s)', s),
    'versions <- read.csv("fixed_set/versions.csv", colClasses = "character")',
    sprintf("versions <- filter(versions, scale == %s)", s),
    "# 1 題號改成目前的版本：只用同一個世代的資料做的版本",
    "responses <- responses |>",
    '  left_join(select(versions, cohort, item_id, current_id), by = c("cohort", "item_id")) |>',
    "  mutate(item_id = coalesce(current_id, item_id), .keep = \"unused\")",
    matrix,
    "# 2 新步驟的缺口：超過「從題庫最高步往上連續」的分數，併為那個分數",
    "for (it in intersect(colnames(X), bank$item_id)) {",
    "  k <- max(bank$item_score[bank$item_id == it])",
    "  while ((k + 1) %in% X[, it]) k <- k + 1",
    "  X[which(X[, it] > k), it] <- k",
    "}",
    "# 3 偏移把關：題庫的 beta 當參照",
    'ref <- bank[c("item_id", "item_score", "beta")]',
    sprintf("drift <- kits_drift(X, structure(ref, scale = %s))", s),
    "moved <- drift$items$item_id[drift$items$drift %in% TRUE]",
    "judged <- sum(!is.na(drift$items$drift))",
    "n_fixed <- n_distinct(drift$fix$item_id)",
    "stopifnot(n_fixed >= 1) # 沒有可固定的題時放不上題庫的尺",
    "# flag（照常校準）：偏移題太多，或可固定的題太少",
    sprintf("flag <- length(moved) > max(1, %s * judged) || n_fixed < %s", flag_share,
            flag_fixed),
    "# 4 偏移的題換成新版本（從原來的題號做）",
    "base <- coalesce(versions$item_id[match(moved, versions$current_id)], moved)",
    sprintf('new_id <- paste0(base, "_r", %s)', kitsui_r(round_label)),
    "colnames(X)[match(moved, colnames(X))] <- new_id",
    "# 5 固定校準：舊的 beta 不動，只估新的步驟",
    sprintf("item <- kits_item(X, fixed_params = drift$fix, scale = %s)", s),
    "coef(item)"
  ), collapse = "\n")
}

# 一個量尺的作答：照資料量尺選；借用另一世代題庫、或一個資料量尺的波次分到不同題庫時，
# 照 kitsui_round_plan() 的資料量尺與波次選
kitsui_code_pick <- function(scale, plan = NULL) {
  part <- plan[plan$scale == scale, ]
  split <- any(plan$data_scale %in% part$data_scale & plan$scale != scale)
  if (is.null(plan) || (!any(part$borrowed) && !split)) {
    return(sprintf("resp <- kits_resp(filter(responses, paste(domain, cohort) == %s))",
                   kitsui_r(scale)))
  }
  keys <- paste(part$data_scale, part$wave_m)
  c(sprintf("# 用 %s 的題庫校準的資料（借用另一世代題庫時人的 cohort 欄不變）", scale),
    sprintf("keys <- %s", kitsui_r(keys)),
    "resp <- kits_resp(filter(responses, paste(domain, cohort, wave_m) %in% keys))")
}

#' 一條連結（內部或外部）的程式碼：從 links.csv 的點重建（和第一次的 kits_linking() 係數相同）
#' @param name kitsui_links() 的名稱，例如 "lan M36"、"D"
#' @param fixed_file 存著這條連結的固定檔（zip）的檔名（kitsui_link_file()）
#' @noRd
kitsui_code_link <- function(fixed, name, fixed_file = "fixed_set.zip") {
  r <- fixed$links[kitsui_link_key(fixed$links) == name, ][1, ]
  cols <- c("id", "item", "from", "to", if (r$kind == "D") "domain")
  sel <- if (r$kind == "D") 'kind == "D"' else sprintf("from_scale == %s", kitsui_r(name))
  kind <- if (r$kind == "D") {
    "# 外部連結：參照世代的各領域 -> D（錨題）"
  } else {
    sprintf("# 內部連結：%s -> %s", name, r$to_scale)
  }
  paste(c(
    kind,
    "# 第一次用 kits_linking() 做（題庫的門檻）；點存在 links.csv，這裡從點重建",
    sprintf('unzip(%s, exdir = "fixed_set", junkpaths = TRUE)', kitsui_r(fixed_file)),
    sprintf('points <- filter(read.csv("fixed_set/links.csv"), %s)', sel),
    sprintf("%s <- points[%s] |>", kitsui_link_var(name), kitsui_r(cols)),
    sprintf("  kits_link_screen(cut = %s, spread = %s, unit = %s,", r$cut, kitsui_r(r$spread),
            kitsui_r(r$unit)),
    sprintf("                   from_scale = %s, to_scale = %s) |>",
            kitsui_r(strsplit(r$from_scale, ", ")[[1]]), kitsui_r(r$to_scale)),
    if (r$n_boot > 0) {
      c("  kits_link_line() |>",
        sprintf("  kits_link_error(n_boot = %d, seed = %d)", r$n_boot, r$seed))
    } else {
      "  kits_link_line()"
    },
    sprintf("coef(%s)", kitsui_link_var(name))
  ), collapse = "\n")
}

#' 一條連結存在哪一個固定檔：之前的輪做的在這一輪開始時的固定檔（上傳的 zip）；
#' 這一輪做的（或固定檔是這次連線交過來的）在 ④ 下載的新固定檔
#' @param earlier 這一輪開始時固定檔裡的連結名稱；fixed_file 是上傳的 zip 檔名（沒有時 NULL）
#' @noRd
kitsui_link_file <- function(name, earlier, fixed_file, round_label) {
  if (name %in% earlier && length(fixed_file) == 1) fixed_file else kitsui_fixed_name(round_label)
}

#' 評分的程式碼：接在一輪（X、item）與連結的程式碼後面
#' @param cohorts 校準到這個量尺的資料的世代（借用另一世代題庫時不只量尺自己的世代）
#' @noRd
kitsui_code_score <- function(fixed, scale, cohorts = scale_cohort(scale)) {
  path <- kitsui_link_var(kitsui_link_path(fixed, scale))
  links <- paste(path, collapse = ", ")
  convert <- if (length(path) > 0) sprintf("scores <- kits_convert(scores, %s)", links)
  thr <- if (length(path) > 0) sprintf("thr <- kits_convert(item, %s)", links) else "thr <- item"
  # 對照表：在校準的量尺上做一次，換算直接換這張表（不用重解每個原始分數）
  inner <- setdiff(path, kitsui_link_var("D"))
  table <- c(
    if (length(inner) > 0) {
      sprintf("raw_theta <- kits_convert(raw_table, %s) # theta：經過內部連結",
              paste(inner, collapse = ", "))
    } else {
      "raw_theta <- raw_table # theta：參照世代自己的量尺，不用換"
    },
    "raw_theta",
    if (kitsui_link_var("D") %in% path) {
      c(sprintf("raw_d <- kits_convert(raw_table, %s) # d：再經過外部連結", links), "raw_d")
    }
  )
  paste(c(
    "persons <- distinct(responses, visit_id, subj_id, cohort, sex, wave_m, age_m)",
    "scores <- kits_subj(X, item, persons = persons) # theta：各世代分開估計，沒有經過連結",
    "raw_table <- kits_subj_table(scores)",
    table,
    convert,
    "# 題目表：門檻和分數用同一組連結；通過年齡用這一輪的作答（沒有常模）",
    thr,
    sprintf("ages <- filter(persons, cohort %%in%% %s)", kitsui_r(cohorts)),
    'ages <- ages[match(rownames(X), ages$visit_id), c("visit_id", "age_m", "wave_m")]',
    "item_table <- kits_item_table(thr, data = cbind(ages, X))",
    "item_table"
  ), collapse = "\n")
}

#' 兩個世代的垂直連結（未連結、內部連結後、外部連結後）的程式碼
#'
#' 接在兩個世代各自的評分程式後面：scores_M03、scores_M36 是 kits_subj() 的結果（還沒換算），
#' 連結是 ③ 的程式重建的 link_*。只寫畫得出來的版本。
#' @param scales 這個領域的量尺，例如 c("lan M03", "lan M36")
#' @param stages 畫得出來的版本：KITSUI_VERTICAL_STAGES 的名稱
#' @noRd
kitsui_code_vertical <- function(fixed, scales, stages = names(KITSUI_VERTICAL_STAGES)) {
  vars <- paste0("scores_", scale_cohort(scales))
  # 一個世代的分數換到這個版本的量尺
  convert <- \(stage) purrr::map2_chr(scales, vars, \(s, v) {
    path <- kitsui_link_path(fixed, s)
    if (stage == "internal") path <- setdiff(path, "D")
    if (length(path) == 0) return(v)
    sprintf("kits_convert(%s, %s)", v, paste(kitsui_link_var(path), collapse = ", "))
  })
  linked <- \(stage, title) c(
    title,
    sprintf("v_%s <- kits_vertical(rbind(", stage),
    paste0("  ", convert(stage), c(rep(",", length(scales) - 1), "")),
    "))",
    sprintf('plot(v_%s, type = "median")', stage)
  )
  code <- list(
    none = c(
      "# 未連結：各世代留在自己校準的量尺上",
      "# kits 的 rbind() 不讓不同量尺的分數合在一起，所以去掉 scale 屬性",
      sprintf("raw <- bind_rows(%s)", paste0("as_tibble(", vars, ")", collapse = ", ")),
      'attr(raw, "scale") <- NULL',
      "v_none <- kits_vertical(raw)",
      'plot(v_none, type = "median")'
    ),
    internal = linked("internal", "# 內部連結後：M36 換到參照世代的量尺"),
    external = linked("external", "# 外部連結後：兩個世代都換到 D")
  )
  head <- sprintf("# %s：%s 用 ④ 的程式算好的 kits_subj()（還沒換算）", vars, scales)
  paste(c(head, unlist(purrr::map(code[stages], \(x) c("", x)))), collapse = "\n")
}
