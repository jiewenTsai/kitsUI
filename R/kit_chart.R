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

# 圖上的字換成中文 ----
# kits 與 kitsUI 畫的圖，標題、軸標題、圖例是英文。畫面與下載的圖用 kit_chart_zh() 換成中文；
# 資料的值（題號、世代、量尺、infit 的 ok 等）與變項名稱（theta、P25、infit）不換。
# 只換認得的字：kits 改了字時，那個字就照原樣（英文）顯示，不會出錯。

# 整句一樣時換（標題、軸標題、圖例）
KIT_CHART_ZH <- c(
  "Category probability curves of each item" = "每一題的類別機率曲線",
  "Wright map: abilities (left) and step thresholds (right)" =
    "Wright map：能力值（左）與步驟門檻（右）",
  "Observed share passing each item against theta, by wave_m (dashed: model)" =
    "各波次每一題的通過比例（實際）對能力值（虛線：模型預期）",
  "Item fit (conditional): mean squares (shaded: 0.6 to 1.4)" =
    "題目適配度（條件）：均方（灰色帶：0.6 到 1.4）",
  "Design: the items of each wave, coloured by the share who pass" =
    "作答設計：每個波次的題目，顏色是通過比例",
  "Link: every point, the line before screening with its band, and the final line" =
    "連結：每個點、篩選前的線與帶子、最後的線",
  "Vertical scale: median (bars = P25 to P75) of each wave, by cohort" =
    "垂直連結：各世代每個波次的中位數（線段：P25 到 P75）",
  "Drift check before fixing" = "固定之前的偏移把關（試算）",
  "Probability" = "機率", "Person-waves" = "列次", "Item" = "題目", "Mean square" = "均方",
  "Pass (%)" = "通過（%）", "theta (logit)" = "能力值 theta（logit）",
  "From (scale to be converted)" = "從（要換算的量尺）", "To (target scale)" = "到（目標量尺）",
  "Median, P25 to P75" = "中位數、P25 到 P75",
  "Age (months; median actual age of each wave)" = "月齡（各波次實際月齡的中位數）",
  "Age (months; nominal age of each wave)" = "月齡（各波次的名義月齡）",
  "Age (months)" = "月齡", "Wave (months)" = "波次（月）", "Wave (years)" = "波次（年）",
  "Wave (wave number)" = "波次（序號）",
  "Threshold, free trial this round (before fixing, own scale)" =
    "這一回合試算的門檻（固定之前，自己的量尺）",
  "Bank threshold (fixed value)" = "題庫的門檻（固定的值）",
  "Model" = "模型預期", "Kept" = "留下", "Dropped" = "拿掉",
  "Judging line (all judged steps)" = "判斷用的線（所有判斷的步驟）",
  "Step not judged (too few people above/below)" = "沒有判斷的步驟（上下的人不夠）",
  "Drifted -> new version" = "偏移，換成新版本", "Dropped this round" = "這一回合拿掉",
  "New item (estimated this round)" = "新題（這一回合估計）",
  "Fixed (not judged, bank value)" = "固定在題庫的值（沒有判斷）",
  "Fixed (bank value)" = "固定在題庫的值", "Free calibration" = "自由校準", "Other" = "其他"
)

# 一部分一樣時換（正規表示式 -> 取代，照順序）：軸標題、圖例裡有數字或量尺名稱的
KIT_CHART_ZH_PATTERNS <- c(
  "^Score ([0-9]+)$" = "\\1 分",
  "^Step ([0-9]+)$" = "步驟 \\1",
  "^theta \\((.+)\\)$" = "能力值 theta（\\1）",
  "^Median, P25 to P75 \\((.+)\\)$" = "中位數、P25 到 P75（\\1）",
  "^Final line \\(kept points\\): " = "最後的線（留下的點）：",
  "^Before screening: " = "篩選前：",
  "^Screening band: \\+/- " = "篩選帶：± ",
  "^Grey band: judging line " = "灰色帶：判斷用的線 ",
  "^(infit|outfit): " = "\\1：",
  # 未連結的垂直連結圖：借用另一世代題庫的波次（kitsui_vertical_series()）
  "^(.+) \\((.+) bank\\)$" = "\\1（\\2 題庫）"
)

#' 一個字串換成中文（認得的才換）
#' @noRd
kit_chart_zh_text <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(x)
  # 標題下面有小標（"…<br><sup>…</sup>"）：兩段各自換
  if (grepl("<br>", x, fixed = TRUE)) {
    parts <- strsplit(x, "<br>", fixed = TRUE)[[1]]
    out <- paste(vapply(parts, kit_chart_zh_text, character(1), USE.NAMES = FALSE),
                 collapse = "<br>")
    return(gsub("actual median age: ", "各波次實際月齡的中位數：", out, fixed = TRUE))
  }
  if (x %in% names(KIT_CHART_ZH)) return(unname(KIT_CHART_ZH[[x]]))
  for (p in names(KIT_CHART_ZH_PATTERNS)) {
    if (grepl(p, x)) return(sub(p, KIT_CHART_ZH_PATTERNS[[p]], x))
  }
  x
}

#' plotly 圖的標題、各軸標題、圖例（trace 的名字）與色條標題換成中文
#'
#' 在 kitsUI 畫好圖之後呼叫（畫面與下載的 HTML 都用它）；資料、題號、世代名稱不動。
#' @noRd
kit_chart_zh <- function(p) {
  p <- plotly::plotly_build(p)
  title <- \(t) {
    if (is.list(t)) {
      t$text <- kit_chart_zh_text(t$text)
      t
    } else {
      kit_chart_zh_text(t)
    }
  }
  lay <- p$x$layout
  if (!is.null(lay$title)) lay$title <- title(lay$title)
  for (ax in grep("^[xy]axis[0-9]*$", names(lay), value = TRUE)) {
    if (!is.null(lay[[ax]]$title)) lay[[ax]]$title <- title(lay[[ax]]$title)
  }
  p$x$layout <- lay
  p$x$data <- purrr::map(p$x$data, \(tr) {
    if (is.character(tr$name) && length(tr$name) == 1) tr$name <- kit_chart_zh_text(tr$name)
    if (!is.null(tr$marker$colorbar$title)) {
      tr$marker$colorbar$title <- title(tr$marker$colorbar$title)
    }
    if (!is.null(tr$colorbar$title)) tr$colorbar$title <- title(tr$colorbar$title)
    tr
  })
  p
}
