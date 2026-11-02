# 一回合裡算過的東西存起來，各分頁共用（round$memo 是 kitsui_round() 建的 environment）

#' 這一回合一個量尺的受試者分數（校準量尺上，還沒換算）：每一回合每個量尺只算一次
#'
#' kits::kits_subj(round$data[[scale]], round$items[[scale]], persons = persons)，
#' 第一次算好存在 round$memo，之後直接拿（② 的 Wright map 與觀察圖、③、④ 共用）。
#' persons 每一回合都一樣（這一回合資料的 kitsui_persons()），所以只用量尺當 key。
#' round 沒有 memo 時（例如舊的結果）每次都算，不存。
#' @param round kitsui_round() 的結果
#' @param scale 校準的量尺（names(round$items) 之一）
#' @param persons kitsui_persons(df)，或 NULL
#' @return kits_subj()
#' @noRd
kitsui_own_scores <- function(round, scale, persons = NULL) {
  memo <- round$memo
  key <- paste0("own_scores:", scale)
  if (is.environment(memo) && exists(key, envir = memo, inherits = FALSE)) {
    return(get(key, envir = memo, inherits = FALSE))
  }
  item <- round$items[[scale]]
  # 原文： 這一回合停下來了，沒有校準結果，不能算分數。
  if (is.null(item)) kitDesign::kit_abort(scale, " \u9019\u4e00\u56de\u5408\u505c\u4e0b\u4f86\u4e86\uff0c\u6c92\u6709\u6821\u6e96\u7d50\u679c\uff0c\u4e0d\u80fd\u7b97\u5206\u6578\u3002")
  # kits_info()$call 和 ④ 的 R 程式一樣：kits_subj(X, item, persons = persons)
  X <- round$data[[scale]]
  scores <- kits::kits_subj(X, item, persons = persons)
  if (is.environment(memo)) assign(key, scores, envir = memo)
  scores
}
