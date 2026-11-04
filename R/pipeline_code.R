# 重現每一步的 R 程式碼（kits 的呼叫，用使用者的設定與檔名）
# 每個 kitsui_code_*() 回傳一段字串；三段依序貼上就能跑（round -> link -> score）

#' 字串轉成 R 的寫法："a" 或 c("a", "b")
#' @noRd
kitsui_r <- function(x) {
  # 量尺名稱裡的不斷行空白（世代名稱有空白時）寫成 \u00a0，複製貼上也不會變
  q <- if (is.character(x)) paste0('"', gsub(KITSUI_NBSP, "\\u00a0", x, fixed = TRUE), '"') else {
    format(x, trim = TRUE)
  }
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

# 連結在程式裡的物件名稱："lan M36" -> link_lan_M36
kitsui_link_var <- function(name) paste0("link_", gsub("[^A-Za-z0-9]", "_", name))

# 讀資料的程式：上傳的檔案（一個或好幾個，readr 會接起來）或示範資料（kitm03、kitm36），
# 再選這一回合：picks（勾到的世代 × 波次，cohort、wave_m）有給時照組合選（semi_join），
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
    # 原文：# 這一回合勾的世代 × 波次
    "# \u9019\u4e00\u56de\u5408\u52fe\u7684\u4e16\u4ee3 \u00d7 \u6ce2\u6b21",
    sprintf("picks <- tibble(cohort = %s,", kitsui_r(as.character(picks$cohort))),
    sprintf("                wave_m = %s)", kitsui_r(as.numeric(picks$wave_m))),
    'responses <- semi_join(responses, picks, by = c("cohort", "wave_m"))'
  ) |>
    paste(collapse = "\n")
}

#' ① 的程式碼：讀資料、選波次、一個量尺的作答設計與檢查
#' @param data_file 上傳的檔名；NULL 時用示範資料
#' @param picks NULL 或勾到的世代 × 波次（cohort、wave_m 的表）；有給時照組合選
#' @param steps NULL 或題號 -> step（固定檔的通過步驟，kitsui_steps_vec()）：有不是 1 的題時，
#'   作答設計圖先把這些題換成通過（1）或沒通過（0），和 kitsui_resp_pass() 相同
#' @noRd
kitsui_code_data <- function(scale, data_file = NULL, waves = NULL, picks = NULL, steps = NULL) {
  waves <- as.numeric(waves)
  steps <- steps[steps != 1]
  plot <- "plot(resp)"
  if (length(steps) > 0) {
    plot <- c(kitsui_code_steps(steps),
              # 原文：# 作答設計圖畫通過比例：這些題換成通過（1）或沒通過（0）；檢查仍用原來的分數
              "# \u4f5c\u7b54\u8a2d\u8a08\u5716\u756b\u901a\u904e\u6bd4\u4f8b\uff1a\u9019\u4e9b\u984c\u63db\u6210\u901a\u904e\uff081\uff09\u6216\u6c92\u901a\u904e\uff080\uff09\uff1b\u6aa2\u67e5\u4ecd\u7528\u539f\u4f86\u7684\u5206\u6578",
              "resp_pass <- resp |>",
              "  mutate(across(any_of(names(steps)),",
              "                \\(x) as.integer(x >= steps[[cur_column()]])))",
              "plot(resp_pass)")
  }
  paste(c(
    "library(kits)", "library(dplyr)", kitsui_code_read(data_file, waves, picks),
    "count(responses, domain, cohort)",
    sprintf("resp <- kits_resp(filter(responses, paste(domain, cohort) == %s))",
            kitsui_r(scale_label(scale))),
    plot, "kits_check(resp = resp)"
  ), collapse = "\n")
}

#' 一回合的程式碼：讀資料、一個量尺的矩陣，自由校準或（有題庫時）偏移把關 + 固定校準
#'
#' @param fixed 上一回合的固定檔（決定要自由還是固定校準）；fixed_file 是它的 zip 檔名
#' @param data_file 資料檔名（一個或好幾個）；NULL 時用示範資料 kitm03、kitm36；
#'   waves 是這一回合的波次；picks（cohort、wave_m 的表）有給時照勾到的世代 × 波次選
#' @param plan NULL 或這一回合的 kitsui_round_plan()：有借用另一世代題庫（或一個資料量尺的波次分到
#'   不同題庫）時，矩陣用校準到這個量尺的資料量尺與波次
#' @param log NULL 或這一回合的紀錄（kitsui_round() 的 log）：app 拿掉或固定在題庫的值的題
#'   （分數類別有問題、估不出 CML）照紀錄寫進程式，結果才和 app 相同
#' @noRd
kitsui_code_round <- function(scale, round_label, fixed = NULL, fixed_file = "fixed_set.zip",
                              data_file = NULL, waves = NULL, flag_share = 0.2, flag_fixed = 5,
                              plan = NULL, picks = NULL, log = NULL) {
  s <- kitsui_r(scale)
  log <- log[log$scale %in% scale, ]
  out <- unique(log$item_id[log$check == "category problem, left out"])
  kept <- unique(log$item_id[log$check %in% c("category problem, kept fixed",
                                              "no CML estimate, kept fixed")])
  head <- c("library(kits)", "library(dplyr)", kitsui_code_read(data_file, waves, picks))
  matrix <- c(kitsui_code_pick(scale, plan), 'X <- as.matrix(resp[attr(resp, "items")])',
              "rownames(X) <- resp$visit_id")
  drop <- if (length(out) > 0) {
    # 原文：# 分數類別有問題、不在題庫裡的題（app 的紀錄）：這一回合不放進來
    c("# \u5206\u6578\u985e\u5225\u6709\u554f\u984c\u3001\u4e0d\u5728\u984c\u5eab\u88e1\u7684\u984c\uff08app \u7684\u7d00\u9304\uff09\uff1a\u9019\u4e00\u56de\u5408\u4e0d\u653e\u9032\u4f86",
      sprintf("out <- %s", kitsui_r(out)),
      "X <- X[, !colnames(X) %in% out, drop = FALSE]")
  }
  if (is.null(fixed) || !scale %in% fixed$bank$scale) {
    free <- sprintf("item <- kits_item(X, scale = %s)", s)
    # 原文：# 沒有題庫：自由校準
    return(paste(c(head, matrix, drop, "# \u6c92\u6709\u984c\u5eab\uff1a\u81ea\u7531\u6821\u6e96", free, "coef(item)"), collapse = "\n"))
  }
  keep <- if (length(kept) > 0) {
    # 原文：# 分數類別有問題（或估不出來）、在題庫裡的題（app 的紀錄）：不判斷偏移、不加新步驟，固定在題庫的值
    c("# \u5206\u6578\u985e\u5225\u6709\u554f\u984c\uff08\u6216\u4f30\u4e0d\u51fa\u4f86\uff09\u3001\u5728\u984c\u5eab\u88e1\u7684\u984c\uff08app \u7684\u7d00\u9304\uff09\uff1a\u4e0d\u5224\u65b7\u504f\u79fb\u3001\u4e0d\u52a0\u65b0\u6b65\u9a5f\uff0c\u56fa\u5b9a\u5728\u984c\u5eab\u7684\u503c",
      sprintf("kept <- %s", kitsui_r(kept)))
  } else {
    "kept <- character()"
  }
  paste(c(
    head,
    sprintf('unzip(%s, exdir = "fixed_set")', kitsui_r(fixed_file)),
    sprintf('bank <- filter(read.csv("fixed_set/bank.csv"), scale == %s)', s),
    'versions <- read.csv("fixed_set/versions.csv", colClasses = "character")',
    sprintf("versions <- filter(versions, scale == %s)", s),
    # 原文：# 1 題號改成目前的版本：只用同一個世代的資料做的版本
    "# 1 \u984c\u865f\u6539\u6210\u76ee\u524d\u7684\u7248\u672c\uff1a\u53ea\u7528\u540c\u4e00\u500b\u4e16\u4ee3\u7684\u8cc7\u6599\u505a\u7684\u7248\u672c",
    "responses <- responses |>",
    '  left_join(select(versions, cohort, item_id, current_id), by = c("cohort", "item_id")) |>',
    "  mutate(item_id = coalesce(current_id, item_id), .keep = \"unused\")",
    matrix, drop, keep,
    # 原文：# 2 新步驟的缺口：超過「從題庫最高步往上連續」的分數，併為那個分數
    "# 2 \u65b0\u6b65\u9a5f\u7684\u7f3a\u53e3\uff1a\u8d85\u904e\u300c\u5f9e\u984c\u5eab\u6700\u9ad8\u6b65\u5f80\u4e0a\u9023\u7e8c\u300d\u7684\u5206\u6578\uff0c\u4f75\u70ba\u90a3\u500b\u5206\u6578",
    # 原文：#   （kept 的題不往上加，超過題庫最高步的都併為最高步）
    "#   \uff08kept \u7684\u984c\u4e0d\u5f80\u4e0a\u52a0\uff0c\u8d85\u904e\u984c\u5eab\u6700\u9ad8\u6b65\u7684\u90fd\u4f75\u70ba\u6700\u9ad8\u6b65\uff09",
    "for (it in intersect(colnames(X), bank$item_id)) {",
    "  k <- max(bank$item_score[bank$item_id == it])",
    "  while (!it %in% kept && (k + 1) %in% X[, it] && k %in% X[, it]) k <- k + 1",
    "  X[which(X[, it] > k), it] <- k",
    "}",
    # 原文：# 3 偏移把關：題庫的 beta 當參照
    "# 3 \u504f\u79fb\u628a\u95dc\uff1a\u984c\u5eab\u7684 beta \u7576\u53c3\u7167",
    'ref <- bank[c("item_id", "item_score", "beta")]',
    "X_judge <- X[, !colnames(X) %in% kept, drop = FALSE]",
    sprintf("drift <- kits_drift(X_judge, structure(ref, scale = %s))", s),
    "moved <- drift$items$item_id[drift$items$drift %in% TRUE]",
    "judged <- sum(!is.na(drift$items$drift))",
    "n_fixed <- n_distinct(drift$fix$item_id)",
    # 原文：stopifnot(n_fixed >= 1) # 沒有可固定的題時放不上題庫的尺
    "stopifnot(n_fixed >= 1) # \u6c92\u6709\u53ef\u56fa\u5b9a\u7684\u984c\u6642\u653e\u4e0d\u4e0a\u984c\u5eab\u7684\u5c3a",
    # 原文：# flag（照常校準）：偏移題太多，或可固定的題太少
    "# flag\uff08\u7167\u5e38\u6821\u6e96\uff09\uff1a\u504f\u79fb\u984c\u592a\u591a\uff0c\u6216\u53ef\u56fa\u5b9a\u7684\u984c\u592a\u5c11",
    sprintf("flag <- length(moved) > max(1, %s * judged) || n_fixed < %s", flag_share,
            flag_fixed),
    # 原文：# 4 偏移的題換成新版本（從原來的題號做）
    "# 4 \u504f\u79fb\u7684\u984c\u63db\u6210\u65b0\u7248\u672c\uff08\u5f9e\u539f\u4f86\u7684\u984c\u865f\u505a\uff09",
    "base <- coalesce(versions$item_id[match(moved, versions$current_id)], moved)",
    sprintf('new_id <- paste0(base, "_r", %s)', kitsui_r(round_label)),
    "colnames(X)[match(moved, colnames(X))] <- new_id",
    # 原文：# 5 固定校準：舊的 beta 不動，只估新的步驟
    "# 5 \u56fa\u5b9a\u6821\u6e96\uff1a\u820a\u7684 beta \u4e0d\u52d5\uff0c\u53ea\u4f30\u65b0\u7684\u6b65\u9a5f",
    "fix <- bind_rows(as.data.frame(drift$fix), ref[ref$item_id %in% kept, ])",
    sprintf("item <- kits_item(X, fixed_params = fix, scale = %s)", s),
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
                   kitsui_r(scale_label(scale))))
  }
  keys <- paste(scale_label(part$data_scale), part$wave_m)
  # 原文：# 用 %s 的題庫校準的資料（借用另一世代題庫時人的 cohort 欄不變）
  c(sprintf("# \u7528 %s \u7684\u984c\u5eab\u6821\u6e96\u7684\u8cc7\u6599\uff08\u501f\u7528\u53e6\u4e00\u4e16\u4ee3\u984c\u5eab\u6642\u4eba\u7684 cohort \u6b04\u4e0d\u8b8a\uff09", scale),
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
    # 原文：# 外部連結：參照世代的各領域 -> D（錨題）
    "# \u5916\u90e8\u9023\u7d50\uff1a\u53c3\u7167\u4e16\u4ee3\u7684\u5404\u9818\u57df -> D\uff08\u9328\u984c\uff09"
  } else {
    # 原文：# 內部連結：%s -> %s
    sprintf("# \u5167\u90e8\u9023\u7d50\uff1a%s -> %s", name, r$to_scale)
  }
  paste(c(
    kind,
    # 原文：# 第一次用 kits_linking() 做（題庫的門檻）；點存在 links.csv，這裡從點重建
    "# \u7b2c\u4e00\u6b21\u7528 kits_linking() \u505a\uff08\u984c\u5eab\u7684\u9580\u6abb\uff09\uff1b\u9ede\u5b58\u5728 links.csv\uff0c\u9019\u88e1\u5f9e\u9ede\u91cd\u5efa",
    sprintf('unzip(%s, exdir = "fixed_set")', kitsui_r(fixed_file)),
    sprintf('points <- filter(read.csv("fixed_set/links.csv"), %s)', sel),
    sprintf("%s <- points[%s] |>", kitsui_link_var(name), kitsui_r(cols)),
    sprintf("  kits_link_screen(cut = %s, spread = %s, unit = %s,", r$cut, kitsui_r(r$spread),
            kitsui_r(r$unit)),
    # from_scale、to_scale 各一行：「用 R 怎麼做」每行最多 70 欄
    sprintf("                   from_scale = %s,", kitsui_r(kitsui_split_scales(r$from_scale, fixed))),
    sprintf("                   to_scale = %s) |>", kitsui_r(r$to_scale)),
    if (r$n_boot > 0) {
      c("  kits_link_line() |>",
        sprintf("  kits_link_error(n_boot = %d, seed = %d)", r$n_boot, r$seed))
    } else {
      "  kits_link_line()"
    },
    sprintf("coef(%s)", kitsui_link_var(name))
  ), collapse = "\n")
}

#' 一條連結存在哪一個固定檔：之前的回合做的在這一回合開始時的固定檔（上傳的 zip）；
#' 這一回合做的（或固定檔是這次連線交過來的）在 ④ 下載的新固定檔
#' @param earlier 這一回合開始時固定檔裡的連結名稱；fixed_file 是上傳的 zip 檔名（沒有時 NULL）
#' @noRd
kitsui_link_file <- function(name, earlier, fixed_file, round_label) {
  if (name %in% earlier && length(fixed_file) == 1) fixed_file else kitsui_fixed_name(round_label)
}

#' 評分的程式碼：接在一回合（X、item）與連結的程式碼後面
#' @param cohorts 校準到這個量尺的資料的世代（借用另一世代題庫時不只量尺自己的世代）
#' @param step 題目表的通過步驟（kitsui_item_step_arg()）：1，或目前的題號 -> step
#' @noRd
kitsui_code_score <- function(fixed, scale, cohorts = scale_cohort(scale), step = 1) {
  path <- kitsui_link_var(kitsui_link_path(fixed, scale))
  links <- paste(path, collapse = ", ")
  step_arg <- if (!is.null(names(step))) ", step = steps" else ""
  convert <- if (length(path) > 0) sprintf("scores <- kits_convert(scores, %s)", links)
  # 對照表：在校準的量尺上做一次，換算直接換這張表（不用重解每個原始分數）
  inner <- setdiff(path, kitsui_link_var("D"))
  table <- c(
    if (length(inner) > 0) {
      # 原文：raw_theta <- kits_convert(raw_table, %s) # ability_linked：經過內部連結
      sprintf("raw_theta <- kits_convert(raw_table, %s) # ability_linked\uff1a\u7d93\u904e\u5167\u90e8\u9023\u7d50",
              paste(inner, collapse = ", "))
    } else {
      # 原文：raw_theta <- raw_table # 參照世代自己的量尺，不用換
      "raw_theta <- raw_table # \u53c3\u7167\u4e16\u4ee3\u81ea\u5df1\u7684\u91cf\u5c3a\uff0c\u4e0d\u7528\u63db"
    },
    "raw_theta",
    if (kitsui_link_var("D") %in% path) {
      # 原文：raw_d <- kits_convert(raw_table, %s) # d_score：再經過外部連結
      c(sprintf("raw_d <- kits_convert(raw_table, %s) # d_score\uff1a\u518d\u7d93\u904e\u5916\u90e8\u9023\u7d50", links), "raw_d")
    }
  )
  paste(c(
    "persons <- distinct(responses, visit_id, subj_id, cohort, sex, wave_m, age_m)",
    # 原文：scores <- kits_subj(X, item, persons = persons) # theta：各世代分開估計，沒有經過連結
    "scores <- kits_subj(X, item, persons = persons) # theta\uff1a\u5404\u4e16\u4ee3\u5206\u958b\u4f30\u8a08\uff0c\u6c92\u6709\u7d93\u904e\u9023\u7d50",
    "raw_table <- kits_subj_table(scores)",
    table,
    convert,
    # 原文：# 題目表：D 欄在校準的量尺上（ability_threshold）；通過年齡用這一回合的作答（沒有常模）
    "# \u984c\u76ee\u8868\uff1aD \u6b04\u5728\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a\uff08ability_threshold\uff09\uff1b\u901a\u904e\u5e74\u9f61\u7528\u9019\u4e00\u56de\u5408\u7684\u4f5c\u7b54\uff08\u6c92\u6709\u5e38\u6a21\uff09",
    sprintf("ages <- filter(persons, cohort %%in%% %s)", kitsui_r(cohorts)),
    'ages <- ages[match(rownames(X), ages$visit_id), c("visit_id", "age_m", "wave_m")]',
    kitsui_code_steps(step),
    sprintf("item_table <- kits_item_table(item, data = cbind(ages, X)%s)", step_arg),
    "item_table",
    # 同一題的門檻用分數的連結換到另外兩把尺
    if (length(inner) > 0) {
      # 原文：kits_item_table(kits_convert(item, %s)%s) # D 欄：ability_linked_threshold
      sprintf("kits_item_table(kits_convert(item, %s)%s) # D \u6b04\uff1aability_linked_threshold",
              paste(inner, collapse = ", "), step_arg)
    },
    if (kitsui_link_var("D") %in% path) {
      # 原文：kits_item_table(kits_convert(item, %s)%s) # D 欄：d_threshold
      sprintf("kits_item_table(kits_convert(item, %s)%s) # D \u6b04\uff1ad_threshold", links, step_arg)
    }
  ), collapse = "\n")
}

# 通過步驟不是 1 的題寫成 steps <- c(...)，一行放幾題（每行不超過 100 字）；都是 1 時不寫
kitsui_code_steps <- function(step) {
  if (is.null(names(step))) return(NULL)
  ids <- names(step)
  ids <- ifelse(make.names(ids) == ids, ids, paste0("`", ids, "`"))
  parts <- paste0(ids, " = ", step, c(rep(",", length(step) - 1), ""))
  lines <- character()
  now <- " "
  for (p in parts) {
    if (nchar(now) + 1 + nchar(p) > 100) {
      lines <- c(lines, now)
      now <- " "
    }
    now <- paste(now, p)
  }
  # 原文：# 通過步驟：得分至少 step 算通過（沒列的題用 1）
  c("# \u901a\u904e\u6b65\u9a5f\uff1a\u5f97\u5206\u81f3\u5c11 step \u7b97\u901a\u904e\uff08\u6c92\u5217\u7684\u984c\u7528 1\uff09", "steps <- c(", c(lines, now), ")")
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
    sprintf('plot(v_%s, type = "median", age_ticks = sort(unique(v_%s$wave_m)))', stage, stage)
  )
  code <- list(
    none = c(
      # 原文：# 未連結：各世代留在自己校準的量尺上
      "# \u672a\u9023\u7d50\uff1a\u5404\u4e16\u4ee3\u7559\u5728\u81ea\u5df1\u6821\u6e96\u7684\u91cf\u5c3a\u4e0a",
      # 原文：# kits 的 rbind() 不讓不同量尺的分數合在一起，所以去掉 scale 屬性
      "# kits \u7684 rbind() \u4e0d\u8b93\u4e0d\u540c\u91cf\u5c3a\u7684\u5206\u6578\u5408\u5728\u4e00\u8d77\uff0c\u6240\u4ee5\u53bb\u6389 scale \u5c6c\u6027",
      sprintf("raw <- bind_rows(%s)", paste0("as_tibble(", vars, ")", collapse = ", ")),
      'attr(raw, "scale") <- NULL',
      "v_none <- kits_vertical(raw)",
      # 原文：# age_ticks：每一個波次都有刻度（kits 預設的刻度只到 96 個月）
      "# age_ticks\uff1a\u6bcf\u4e00\u500b\u6ce2\u6b21\u90fd\u6709\u523b\u5ea6\uff08kits \u9810\u8a2d\u7684\u523b\u5ea6\u53ea\u5230 96 \u500b\u6708\uff09",
      'plot(v_none, type = "median", age_ticks = sort(unique(v_none$wave_m)))'
    ),
    internal = linked("internal", kitsui_code_internal_title(fixed, scales)),
    # 原文：# 外部連結後：各世代都換到 D
    external = linked("external", "# \u5916\u90e8\u9023\u7d50\u5f8c\uff1a\u5404\u4e16\u4ee3\u90fd\u63db\u5230 D")
  )
  # 原文：# %s：%s 用 ④ 的程式算好的 kits_subj()（還沒換算）
  head <- sprintf("# %s\uff1a%s \u7528 \u2463 \u7684\u7a0b\u5f0f\u7b97\u597d\u7684 kits_subj()\uff08\u9084\u6c92\u63db\u7b97\uff09", vars, scales)
  paste(c(head, unlist(purrr::map(code[stages], \(x) c("", x)))), collapse = "\n")
}

# 「內部連結後」那一段的註解：哪些世代換到參照世代的量尺（例如「M36 換到參照世代 M03 的量尺」）
kitsui_code_internal_title <- function(fixed, scales) {
  ref <- fixed$version$reference
  others <- setdiff(unique(scale_cohort(scales)), ref)
  # 原文：其他世代 | 、
  from <- if (length(others) == 0) "\u5176\u4ed6\u4e16\u4ee3" else paste(others, collapse = "\u3001")
  # 原文：# 內部連結後：%s 換到參照世代 %s 的量尺
  sprintf("# \u5167\u90e8\u9023\u7d50\u5f8c\uff1a%s \u63db\u5230\u53c3\u7167\u4e16\u4ee3 %s \u7684\u91cf\u5c3a", from, ref)
}

#' 垂直連結加上之前回合的點：讀固定檔的 wave_summary.csv，照現在的連結換算後一起畫
#'
#' 新固定檔的 wave_summary.csv 有每一回合的摘要（這一回合的列和 kits_vertical() 的相同）。
#' 同一個量尺 × 世代 × 波次校準過兩次時，用最新的回合。
#' @param scales 這個領域的量尺（這一回合與之前回合的）；stage 是畫的版本；zip 是新固定檔
#' @noRd
kitsui_code_vertical_earlier <- function(fixed, scales, stage, zip) {
  label <- utils::tail(fixed$version$rounds, 1)
  paths <- purrr::map_chr(scales, \(s) {
    path <- kitsui_link_path(fixed, s)
    if (stage == "internal") path <- setdiff(path, "D")
    sprintf("  `%s` = list(%s)", s, paste(kitsui_link_var(path), collapse = ", "))
  })
  convert <- if (stage != "none") {
    # 原文：# 照現在的連結換算：連結是斜率為正的直線，換過去還是 P25、中位數、P75
    c("# \u7167\u73fe\u5728\u7684\u9023\u7d50\u63db\u7b97\uff1a\u9023\u7d50\u662f\u659c\u7387\u70ba\u6b63\u7684\u76f4\u7dda\uff0c\u63db\u904e\u53bb\u9084\u662f P25\u3001\u4e2d\u4f4d\u6578\u3001P75",
      "paths <- list(", paste0(paths, c(rep(",", length(paths) - 1), "")), ")",
      "to <- \\(x, s) {",
      "  if (length(paths[[s]]) == 0) return(x)",
      "  as.vector(do.call(kits_convert, c(list(x), paths[[s]], from = s)))",
      "}",
      "ws <- mutate(ws, across(c(p25, p50, p75), \\(x) to(x, scale[1])), .by = scale)")
  }
  paste(c(
    # 原文：# 之前回合的點：固定檔的 wave_summary.csv（各波次的人數、P25、中位數、P75、實際月齡的中位數）
    "# \u4e4b\u524d\u56de\u5408\u7684\u9ede\uff1a\u56fa\u5b9a\u6a94\u7684 wave_summary.csv\uff08\u5404\u6ce2\u6b21\u7684\u4eba\u6578\u3001P25\u3001\u4e2d\u4f4d\u6578\u3001P75\u3001\u5be6\u969b\u6708\u9f61\u7684\u4e2d\u4f4d\u6578\uff09",
    sprintf('unzip(%s, exdir = "fixed_set")', kitsui_r(zip)),
    'ws <- read.csv("fixed_set/wave_summary.csv", colClasses = c(round = "character")) |>',
    sprintf("  filter(scale %%in%% %s) |>", kitsui_r(scales)),
    # 原文：  slice_tail(n = 1, by = c(scale, cohort, wave_m)) # 校準過兩次的波次：用最新的回合
    "  slice_tail(n = 1, by = c(scale, cohort, wave_m)) # \u6821\u6e96\u904e\u5169\u6b21\u7684\u6ce2\u6b21\uff1a\u7528\u6700\u65b0\u7684\u56de\u5408",
    convert,
    # 原文：# 點是中位數，線段是 P25 到 P75；空心的點是之前的回合
    "# \u9ede\u662f\u4e2d\u4f4d\u6578\uff0c\u7dda\u6bb5\u662f P25 \u5230 P75\uff1b\u7a7a\u5fc3\u7684\u9ede\u662f\u4e4b\u524d\u7684\u56de\u5408",
    sprintf('now <- if_else(ws$round == %s, "this round", "earlier round")', kitsui_r(label)),
    'ws$source <- factor(now, c("earlier round", "this round"))',
    # 原文：# x 是實際月齡的中位數；舊的固定檔沒有 age_med 時用名義月齡
    "# x \u662f\u5be6\u969b\u6708\u9f61\u7684\u4e2d\u4f4d\u6578\uff1b\u820a\u7684\u56fa\u5b9a\u6a94\u6c92\u6709 age_med \u6642\u7528\u540d\u7fa9\u6708\u9f61",
    "if (is.null(ws$age_med)) ws$age_med <- NA_real_",
    "ws$age <- coalesce(ws$age_med, ws$wave_m)",
    "plotly::plot_ly(ws, x = ~age, y = ~p50, color = ~cohort, symbol = ~source,",
    '                symbols = c("circle-open", "circle"), type = "scatter", mode = "markers",',
    "                error_y = ~list(array = p75 - p50, arrayminus = p50 - p25, symmetric = FALSE))"
  ), collapse = "\n")
}

#' ② 的程式碼加上偏移把關圖：kits_drift() 那一行後面加 plot(drift)（自由校準時不變）
#' @param code kitsui_code_round() 的字串
#' @noRd
kitsui_code_drift_plot <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  at <- grep("^drift <- kits_drift\\(", lines)
  if (length(at) == 0) return(code)
  # 原文：plot(drift) # 偏移把關圖：這一回合的門檻對題庫的門檻，與判斷用的線
  plot_line <- "plot(drift) # \u504f\u79fb\u628a\u95dc\u5716\uff1a\u9019\u4e00\u56de\u5408\u7684\u9580\u6abb\u5c0d\u984c\u5eab\u7684\u9580\u6abb\uff0c\u8207\u5224\u65b7\u7528\u7684\u7dda"
  paste(append(lines, plot_line, after = at[1]), collapse = "\n")
}
