# 共用小工具

#' x 為 NULL 時改用 y（R 4.4 起 base 也有，這裡為了支援較舊的 R）
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' 每列的 ID 合成一個比對用字串；任一 ID 欄缺漏的列回傳 NA
#' @noRd
key_strings <- function(df, by_vars) {
  keys <- lapply(df[by_vars], function(x) as.character(unclass(x)))
  out <- do.call(paste, c(keys, sep = "\r"))
  has_na <- Reduce(`|`, lapply(keys, is.na))
  out[has_na] <- NA_character_
  out
}

#' 錯誤轉成給使用者看的句子：kitDesign::kit_abort() 的訊息原文；其他錯誤前面加上在做什麼
#' @noRd
kit_error_text <- function(e, action) {
  if (inherits(e, "kit_user_error")) return(conditionMessage(e))
  # 原文收在 kitsui_error_ui() 的「kits 的原文」裡
  paste0(action, "時停下來了。", KITSUI_KITS_SAYS, conditionMessage(e))
}

# pandoc（kits::kits_save_plot() 存 html 要用） ----

#' 常見的 pandoc 資料夾：RStudio 與 Quarto 附的（Linux、macOS、Windows）
#' @noRd
kit_pandoc_dirs <- function() {
  tools <- c(
    "/opt/quarto/bin/tools", "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Quarto/bin/tools"
  )
  c(rbind(file.path(tools, "x86_64"), file.path(tools, "aarch64"), tools),
    "/usr/lib/rstudio/bin/pandoc", "/Applications/RStudio.app/Contents/MacOS/pandoc")
}

#' 找 pandoc 的資料夾：PATH、RSTUDIO_PANDOC、rmarkdown::find_pandoc()、常見的資料夾
#' @param dirs 常見的資料夾
#' @param sys,env Sys.which("pandoc") 與 RSTUDIO_PANDOC（測試時可以換）
#' @param use_rmarkdown 有 rmarkdown 時也問它
#' @return 資料夾；找不到時 NULL
#' @noRd
kit_find_pandoc <- function(dirs = kit_pandoc_dirs(), sys = Sys.which("pandoc"),
                            env = Sys.getenv("RSTUDIO_PANDOC"), use_rmarkdown = TRUE) {
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  if (nzchar(sys)) return(dirname(sys))
  if (nzchar(env) && file.exists(file.path(env, exe))) return(env)
  if (use_rmarkdown && requireNamespace("rmarkdown", quietly = TRUE)) {
    dir <- rmarkdown::find_pandoc(cache = FALSE)$dir
    if (length(dir) == 1 && nzchar(dir)) return(dir)
  }
  hit <- dirs[file.exists(file.path(dirs, exe))]
  if (length(hit) > 0) hit[1] else NULL
}

#' 啟動時找 pandoc；不在 PATH 上時設定 RSTUDIO_PANDOC（rmarkdown 與 kits 都看它）
#' @return 不可見的 TRUE（找到）或 FALSE
#' @noRd
kit_setup_pandoc <- function() {
  dir <- kit_find_pandoc()
  if (is.null(dir)) return(invisible(FALSE))
  if (!nzchar(Sys.which("pandoc"))) Sys.setenv(RSTUDIO_PANDOC = dir)
  if (requireNamespace("rmarkdown", quietly = TRUE)) rmarkdown::find_pandoc(cache = FALSE)
  invisible(TRUE)
}

#' 現在找得到 pandoc 嗎（和 kits::kits_save_plot() 的判斷一樣）
#' @noRd
kit_pandoc_ok <- function() {
  if (requireNamespace("rmarkdown", quietly = TRUE)) return(rmarkdown::pandoc_available())
  nzchar(Sys.which("pandoc"))
}

# kits 的英文錯誤 -> 白話中文 ----

# 常見的 kits 錯誤（照順序比對 conditionMessage()，第一個符合的用）：pattern 是正規表示式，
# zh 是給使用者看的一兩句（說原因與怎麼辦）。kits 的原文另外收在「kits 的原文」裡
KITSUI_KITS_ERRORS <- tibble::tribble(
  ~pattern, ~zh,
  "Fewer than 2 items of the new data have a 0 score",
  "這一回合能自由校準的題不到 2 題（要有人得 0 分、至少兩種分數），不能判斷偏移。",
  "^Only [0-9]+ item step\\(s\\) have at least `min_n`",
  "每個步驟上下各要有 30 人才判斷偏移，這一回合夠的步驟不到 3 個（人太少）。",
  "Every person-wave answered only one item",
  paste0("每個列次都只剩一題可以估計（其他題固定在題庫的值或拿掉了），CML 估不出來：",
         "每個人至少要有兩題。可以在 ② 把這個量尺選成「這一回合不做」，等資料多一些再做。"),
  "At least 2 item steps are needed",
  "可以估計的題步驟不到 2 個（其他題都固定在題庫的值或拿掉了），沒有東西可以校準。",
  "Every item is fixed and has no 0 score or only one score",
  "每一題都固定在題庫的值，而且沒有人得 0 分或只有一種分數，沒有東西可以校準。",
  "No item of the new data is in `ref`",
  "題庫和這一回合的資料沒有共同的題（題號都不一樣），不能固定校準。請確認題號和之前的回合相同。",
  "change within a person-wave",
  paste0("同一個列次（visit_id）的幾列，某些欄（例如 wave_m、cohort、age_m、sex）的值不一樣。",
         "同一個列次的這些欄要相同。"),
  "same item more than once",
  "同一個列次同一題有好幾列。每個列次每一題只能有一列。",
  "no finite CML estimate",
  "有題估不出有限的 CML 估計值（有資訊的人都通過，或都沒通過）。",
  "gap in their observed scores|Scores must be consecutive",
  "有題的分數不連續（中間少了分數），要先把分數改成連續的 0、1、2…。",
  "separate groups that share no item",
  "作答設計沒有連在一起：有幾組題沒有被同一批人一起作答，不能放在同一把尺上。",
  "Hessian of the calibration is singular",
  "資訊不夠，算不出標準誤（人或題太少，或分數太集中）。",
  "undefined columns selected",
  "kitsUI 找不到要用的欄或題。請把這個量尺與資料告訴維護 kitsUI 的人。"
)

# 資訊不足、無法估計的 kits 錯誤（資料本身沒有錯，只是這一回合的人或題不夠）-> 一句簡短的原因。
# 這些錯誤停下來時，紅字直接說「資訊不足」，請使用者按「這一回合不做」跳過；不強迫估計，
# 也不把偏移的題或新題改成題庫的值。前兩種在偏移把關時遇到會走 kitsui_drift_safe() 的退路
# （題庫的題固定在題庫的值），只有在別的地方（例如固定校準）遇到時才會停下來。
# 其他錯誤（資料有問題，要人看）不在這裡。
KITSUI_KITS_NO_INFO <- tibble::tribble(
  ~pattern, ~zh, ~en,
  "Fewer than 2 items of the new data have a 0 score",
  "能自由校準的題不到 2 題", "fewer than 2 items can be calibrated freely",
  "^Only [0-9]+ item step\\(s\\) have at least `min_n`",
  "上下各有 30 人的步驟不到 3 個", "fewer than 3 steps have 30 persons below and above",
  "Every person-wave answered only one item",
  "每個列次只剩一題可以估計", "every person-wave has only one item left to estimate",
  "At least 2 item steps are needed",
  "可以估計的步驟不到 2 個", "fewer than 2 item steps can be estimated",
  "Every item is fixed and has no 0 score or only one score",
  "每一題都固定，沒有可以估計的分數", "every item is fixed and none can be estimated",
  "Hessian of the calibration is singular",
  "人或題太少，算不出標準誤", "too few persons or items to compute standard errors"
)

#' kits 的錯誤是不是「資訊不足、無法估計」
#' @return list(zh, en)（簡短的原因）；不是時 NULL
#' @noRd
kitsui_kits_no_info <- function(msg) {
  hit <- which(purrr::map_lgl(KITSUI_KITS_NO_INFO$pattern, \(p) grepl(p, msg)))
  if (length(hit) == 0) return(NULL)
  list(zh = KITSUI_KITS_NO_INFO$zh[hit[1]], en = KITSUI_KITS_NO_INFO$en[hit[1]])
}

#' kits 的英文錯誤翻成白話中文
#' @param msg kits 的錯誤訊息（英文）
#' @return 一兩句中文；不是常見的錯誤時 NULL
#' @noRd
kitsui_kits_zh <- function(msg) {
  hit <- which(purrr::map_lgl(KITSUI_KITS_ERRORS$pattern, \(p) grepl(p, msg)))
  if (length(hit) == 0) return(NULL)
  KITSUI_KITS_ERRORS$zh[hit[1]]
}
