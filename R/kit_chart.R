# 圖的顏色（KIT 設計語言 3.9）：類別色用色盲友善的 Okabe–Ito，世代、量尺、分數依序取色。
# 顏色只在這裡定義；kits 畫好的圖再用 kit_chart_categorical() 換色（kits 本身不改）。

# 類別色，依序取色：藍、橘、綠、紫紅、天藍、朱紅
KIT_CHART_COLORS <- c(
  blue = "#0072B2", orange = "#E69F00", green = "#009E73", purple = "#CC79A7",
  sky = "#56B4E9", vermillion = "#D55E00"
)
# 不分類別的中性色：參考線、帶子、沒有判斷的點
KIT_CHART_NEUTRAL <- "#7F7F7F"
KIT_CHART_INK <- "#000000"

#' 類別色
#' @param n NULL 時回傳全部（具名）；n 個類別時依序取 n 個（超過 6 個從頭再來）
#' @noRd
kit_chart_colors <- function(n = NULL) {
  if (is.null(n)) return(KIT_CHART_COLORS)
  unname(rep_len(KIT_CHART_COLORS, n))
}

#' 有意思的顏色（不是依序取色）：偏移把關圖的點
#'
#' 固定在題庫的值是藍、偏移換成新版本是朱紅、其他原因固定在題庫的值是綠；
#' 沒有判斷的步驟是中性的灰；其他是黑。
#' @noRd
kit_chart_role <- function(role) {
  switch(
    role,
    fixed = KIT_CHART_COLORS[["blue"]],
    moved = KIT_CHART_COLORS[["vermillion"]],
    kept = KIT_CHART_COLORS[["green"]],
    unjudged = KIT_CHART_NEUTRAL,
    KIT_CHART_INK
  )
}

#' 一張 plotly 圖的類別換成 KIT 的類別色：同一組（legendgroup，沒有時用 name）同一個顏色，
#' 依第一次出現的順序取色
#'
#' 用在 kits 依類別上色的圖：類別機率曲線（分數）、觀察與預期（波次）、垂直連結（世代）。
#' 只換線與點的顏色；點的形狀、透明度（之前的回合較淡）都不動。
#' @param skip 不換色的組（例如觀察與預期圖的 "model"：灰色的模型預期線）
#' @noRd
kit_chart_categorical <- function(p, skip = character()) {
  p <- plotly::plotly_build(p)
  group <- purrr::map_chr(p$x$data, \(tr) {
    g <- as.character(tr$legendgroup %||% tr$name %||% "")
    if (length(g) == 0) "" else g[1]
  })
  levels <- unique(group[nzchar(group) & !group %in% skip])
  cols <- stats::setNames(kit_chart_colors(length(levels)), levels)
  p$x$data <- purrr::map2(p$x$data, group, \(tr, g) {
    if (!g %in% levels) return(tr)
    # 只換畫得出來的部分（mode 沒有 markers 時加了 marker，plotly 會自己加上點）
    mode <- tr$mode %||% "lines+markers"
    if (grepl("lines", mode)) tr$line$color <- cols[[g]]
    if (grepl("markers", mode)) tr$marker$color <- cols[[g]]
    if (!is.null(tr$error_y)) tr$error_y$color <- cols[[g]]
    tr
  })
  p
}
