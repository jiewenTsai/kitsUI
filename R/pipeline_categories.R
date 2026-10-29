# 分數類別的問題：沒有人得 0 分、分數不連續、只有一種分數
#   不改分數；只列出來。② 校準時：題庫裡的題固定在題庫的值，不在題庫的題這一輪不放進來

#' 每個資料量尺（領域 × 世代）裡分數類別有問題的題
#'
#' 只看給的這些列（例如這一輪勾選的波次）。一題可以有好幾個問題，放在同一列。
#' @param df 長格式資料（domain、cohort、item_id、item_score）
#' @return tibble：scale（資料的量尺）、item_id、problem（英文："no 0 score"、"score gap"、
#'   "one score only"，好幾個時以 ", " 分開）、detail_zh（一句中文，例如
#'   「lanc01 沒有人得 0 分（分數 1、2、3）」）。沒有問題時 0 列
#' @noRd
kitsui_category_problems <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$item_score)) |>
    dplyr::summarise(scores = list(sort(unique(.data$item_score))),
                     .by = c("domain", "cohort", "item_id")) |>
    dplyr::mutate(scale = paste(.data$domain, .data$cohort)) |>
    dplyr::arrange(match(.data$scale, kitsui_scales(df)$scale), .data$item_id) |>
    dplyr::select("scale", "item_id", "scores") |>
    kitsui_problem_rows()
}

#' 作答矩陣每一欄（題）的分數類別問題
#' @param X 作答矩陣（每一列是一個列次 visit_id，每一欄是一題）
#' @param scale 寫進結果的量尺
#' @return 和 kitsui_category_problems() 一樣的 tibble
#' @noRd
kitsui_matrix_problems <- function(X, scale) {
  scores <- purrr::map(colnames(X), \(it) sort(unique(stats::na.omit(X[, it]))))
  tibble::tibble(scale = scale, item_id = colnames(X), scores = scores) |>
    dplyr::filter(lengths(.data$scores) > 0) |>
    kitsui_problem_rows()
}

# 每題一列（scale、item_id、scores 是排好的分數）-> 只留有問題的題，加上 problem、detail_zh
kitsui_problem_rows <- function(tab) {
  found <- purrr::map2(tab$item_id, tab$scores, kitsui_score_problem)
  tab |>
    dplyr::mutate(problem = purrr::map_chr(found, "problem"),
                  detail_zh = purrr::map_chr(found, "detail")) |>
    dplyr::filter(!is.na(.data$problem)) |>
    dplyr::select("scale", "item_id", "problem", "detail_zh")
}

#' 一題的分數有沒有問題
#' @param scores 這一題出現過的分數（排好、不重複）
#' @return list(problem, detail)：沒有問題時都是 NA
#' @noRd
kitsui_score_problem <- function(item_id, scores) {
  seen <- paste0("（分數 ", paste(scores, collapse = "、"), "）")
  if (length(scores) == 1) {
    return(list(problem = "one score only",
                detail = sprintf("%s 每個人都得 %s 分，只有一種分數", item_id, scores)))
  }
  missing <- setdiff(seq(min(scores), max(scores)), scores)
  no_zero <- min(scores) > 0
  problem <- c(if (no_zero) "no 0 score", if (length(missing) > 0) "score gap")
  if (length(problem) == 0) return(list(problem = NA_character_, detail = NA_character_))
  gap <- sprintf("分數不連續，沒有人得 %s 分", paste(missing, collapse = "、"))
  what <- c(if (no_zero) "沒有人得 0 分", if (length(missing) > 0) gap)
  list(problem = paste(problem, collapse = ", "),
       detail = paste0(item_id, " ", paste(what, collapse = "；"), seen))
}
