# 固定檔（fixed set）：一個 zip，寫了就不再改
#   VERSION（DCF）、bank.csv、versions.csv、links.csv、checks.csv、waves.csv
#   bank.csv、links.csv 與 waves.csv 的 round 是加進來的那一回合（題目步驟、連結、波次）

# 各表的欄位與型別（readr 的縮寫：c 文字、i 整數、d 數字）
KITSUI_FIXED_COLS <- list(
  bank = c(scale = "c", item_id = "c", item_score = "i", beta = "d", beta_se = "d", round = "c"),
  versions = c(scale = "c", item_id = "c", current_id = "c", cohort = "c"),
  links = c(kind = "c", from_scale = "c", to_scale = "c", id = "c", item = "c", from = "d",
            to = "d", domain = "c", cut = "d", spread = "c", unit = "c", n_boot = "i", seed = "i",
            round = "c"),
  checks = c(round = "c", scale = "c", item_id = "c", check = "c", detail = "c"),
  waves = c(scale = "c", wave_m = "d", round = "c")
)

# 舊的固定檔（kitsUI 0.2.0 以前）沒有 waves.csv：讀進來時，題庫的波次記成不知道（NA）；
# versions.csv 沒有 cohort 欄：當成量尺自己的世代的資料做的版本
KITSUI_FIXED_OPTIONAL <- "waves"

#' 空的固定檔
#' @return list(version, sources, bank, versions, links, checks)
#' @noRd
kitsui_fixed_empty <- function(reference = KITSUI_REF_COHORT) {
  tables <- purrr::map(KITSUI_FIXED_COLS, \(cols) {
    kitsui_read_csv(I(paste(names(cols), collapse = ",")), cols)
  })
  version <- list(
    kits = as.character(utils::packageVersion("kits")), kitsUI = kitsui_version(),
    created = as.character(Sys.Date()), rounds = character(), reference = reference,
    scales = character()
  )
  sources <- tibble::tibble(round = character(), file = character(), md5 = character())
  c(list(version = version, sources = sources), tables)
}

#' kitsUI 的版本：和頁首一樣讀 DESCRIPTION（kit_app_description()）；找不到時 "dev"
#' @noRd
kitsui_version <- function() {
  v <- kit_app_description()$version
  if (is.null(v) || is.na(v)) "dev" else as.character(v)
}

#' 寫出固定檔（zip）
#'
#' 有量尺停下來的一回合（attr(fixed, "stopped")）不能寫出。
#' 數字以 17 位有效數字寫出，讀回來與原值完全相同（連結才能一模一樣地重建）。
#' @noRd
kitsui_write_fixed <- function(fixed, zip_path) {
  stopped <- attr(fixed, "stopped")
  if (length(stopped) > 0) {
    kit_abort("這一回合有量尺停下來（", paste(stopped, collapse = "、"), "），不產生新的固定檔。",
              "請看檢查表的原因，處理後重跑這一回合。")
  }
  dir <- file.path(tempfile("fixed"), "fixed_set")
  dir.create(dir, recursive = TRUE)
  write.dcf(kitsui_dcf(fixed), file.path(dir, "VERSION"), keep.white = "Sources")
  # 給人看的說明（每個檔案是什麼、下一回合怎麼用）；讀固定檔時不讀它
  writeLines(kitsui_fixed_readme(fixed), file.path(dir, "README.txt"), useBytes = TRUE)
  for (name in names(KITSUI_FIXED_COLS)) {
    exact <- dplyr::mutate(fixed[[name]], dplyr::across(
      dplyr::where(is.double), \(x) ifelse(is.na(x), NA, sprintf("%.17g", x))
    ))
    readr::write_csv(exact, file.path(dir, paste0(name, ".csv")), na = "")
  }
  zip::zip(zip_path, list.files(dir, full.names = TRUE), mode = "cherry-pick")
  invisible(zip_path)
}

#' 讀入固定檔（zip）
#' @noRd
kitsui_read_fixed <- function(zip_path) {
  ok <- tryCatch(nrow(suppressWarnings(zip::zip_list(zip_path))) > 0, error = \(e) FALSE)
  if (!ok) {
    kit_abort("這不是固定檔（zip）。請上傳 kitsUI 在 ④ 下載的固定檔（kitsui_fixed_…zip）。")
  }
  dir <- tempfile("fixed")
  utils::unzip(zip_path, exdir = dir, junkpaths = TRUE)
  need <- c("VERSION", paste0(setdiff(names(KITSUI_FIXED_COLS), KITSUI_FIXED_OPTIONAL), ".csv"))
  miss <- need[!file.exists(file.path(dir, need))]
  if (length(miss) > 0) {
    kit_abort("這個 zip 不是完整的固定檔：缺少 ", paste(miss, collapse = "、"), "。",
              "請上傳 kitsUI 在 ④ 下載的固定檔。")
  }
  tables <- purrr::imap(KITSUI_FIXED_COLS, \(cols, name) {
    file <- file.path(dir, paste0(name, ".csv"))
    if (file.exists(file)) kitsui_read_csv(file, cols)
  })
  if (is.null(tables$waves)) {
    tables$waves <- tibble::tibble(scale = unique(tables$bank$scale), wave_m = NA_real_,
                                   round = NA_character_)
  }
  tables$versions$cohort <- dplyr::coalesce(tables$versions$cohort,
                                            scale_cohort(tables$versions$scale))
  dcf <- read.dcf(file.path(dir, "VERSION"), keep.white = "Sources")[1, ]
  c(kitsui_from_dcf(dcf), tables)
}

# 照固定的欄位型別讀。數字先讀成文字再用 as.numeric()：readr 的數字解析不一定回到同一個
# double，as.numeric() 讀 17 位有效數字才完全相同；as_tibble() 拿掉 readr 的 spec。
# 舊版的檔案少了的欄（例如 versions.csv 的 cohort）補成 NA
kitsui_read_csv <- function(file, cols) {
  head <- names(readr::read_csv(file, n_max = 0, col_types = readr::cols(.default = "c")))
  have <- names(cols)[names(cols) %in% head]
  types <- do.call(readr::cols_only, as.list(sub("d", "c", cols[have])))
  out <- readr::read_csv(file, col_types = types, na = "", lazy = FALSE)
  for (col in setdiff(names(cols), have)) out[[col]] <- NA_character_
  out[names(cols)] |>
    dplyr::mutate(dplyr::across(dplyr::all_of(names(cols)[cols == "d"]), as.numeric)) |>
    tibble::as_tibble()
}

# VERSION 的欄位；Sources 一行一個檔案：「波次 md5 檔名」
kitsui_dcf <- function(fixed) {
  v <- fixed$version
  src <- with(fixed$sources, paste(round, md5, file))
  waves <- purrr::map_chr(unique(fixed$waves$scale), \(s) {
    paste0(s, ": ", kitsui_wave_range(fixed, s))
  })
  cbind(
    Kits = v$kits, KitsUI = v$kitsUI, Created = v$created,
    Rounds = paste(v$rounds, collapse = ", "), Reference = v$reference,
    Scales = paste(v$scales, collapse = ", "), Waves = paste(waves, collapse = "; "),
    Sources = paste(c("", src), collapse = "\n")
  )
}

kitsui_from_dcf <- function(dcf) {
  split <- \(x) if (is.na(x) || x == "") character() else strsplit(x, ", ")[[1]]
  src <- setdiff(trimws(strsplit(dcf[["Sources"]], "\n")[[1]]), "")
  pick <- \(i) sub("^(\\S+) (\\S+) (.*)$", i, src)
  version <- list(
    kits = dcf[["Kits"]], kitsUI = dcf[["KitsUI"]], created = dcf[["Created"]],
    rounds = split(dcf[["Rounds"]]), reference = dcf[["Reference"]],
    scales = split(dcf[["Scales"]])
  )
  sources <- tibble::tibble(round = pick("\\1"), file = pick("\\3"), md5 = pick("\\2"))
  list(version = version, sources = sources)
}

#' 這一回合資料檔的 md5（給 VERSION）；示範資料沒有檔案，算資料本身的 md5
#'
#' @param files 檔案路徑，名稱為顯示的檔名；NULL 時用 df
#' @noRd
kitsui_sources <- function(round_label, df = NULL, files = NULL) {
  if (is.null(files)) {
    files <- c(data = tempfile(fileext = ".csv"))
    readr::write_csv(df, files)
  }
  names(files) <- names(files) %||% basename(files)
  tibble::tibble(round = round_label, file = names(files), md5 = unname(tools::md5sum(files)))
}
