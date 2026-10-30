# 固定檔（fixed set）：一個 zip，寫了就不再改
#   VERSION（DCF）、bank.csv、versions.csv、links.csv、checks.csv、waves.csv、
#   wave_summary.csv、pass_steps.csv
#   bank.csv、links.csv 與 waves.csv 的 round 是加進來的那一回合（題目步驟、連結、波次）
#   VERSION 的 Checksums 是每個檔案的 SHA-256（kitsUI 0.4.0 起），上傳時核對有沒有被改過

# 各表的欄位與型別（readr 的縮寫：c 文字、i 整數、d 數字）
KITSUI_FIXED_COLS <- list(
  bank = c(scale = "c", item_id = "c", item_score = "i", beta = "d", beta_se = "d", round = "c"),
  versions = c(scale = "c", item_id = "c", current_id = "c", cohort = "c"),
  links = c(kind = "c", from_scale = "c", to_scale = "c", id = "c", item = "c", from = "d",
            to = "d", domain = "c", cut = "d", spread = "c", unit = "c", n_boot = "i", seed = "i",
            round = "c"),
  checks = c(round = "c", scale = "c", item_id = "c", check = "c", detail = "c"),
  waves = c(scale = "c", wave_m = "d", round = "c"),
  # 每個量尺、每個世代、每個波次的能力值摘要（校準的量尺上，還沒換算）與實際月齡的中位數
  # （age_med，0.3.11 起；之前的固定檔沒有時是 NA）：畫垂直圖用。
  # 連結是斜率為正的直線，中位數與 P25、P75 換算後仍然是中位數與 P25、P75
  wave_summary = c(scale = "c", cohort = "c", wave_m = "d", n = "i", p25 = "d", p50 = "d",
                   p75 = "d", age_med = "d", round = "c"),
  # 題目的通過步驟（得分至少 step 算通過）：題目表的門檻與 age50、作答設計圖用。
  # 沒列的題用 1；偏移後的新版本沿用原題號的設定
  pass_steps = c(item_id = "c", step = "i", round = "c")
)

# 舊的固定檔（kitsUI 0.2.0 以前）沒有 waves.csv：讀進來時，題庫的波次記成不知道（NA）；
# versions.csv 沒有 cohort 欄：當成量尺自己的世代的資料做的版本。
# kitsUI 0.3.8 以前沒有 wave_summary.csv、pass_steps.csv：讀進來是空表
KITSUI_FIXED_OPTIONAL <- c("waves", "wave_summary", "pass_steps")

#' 空的固定檔
#' @param reference 參照世代：第一回合在 ① 選（kitsui_ref_pick()），寫進 VERSION 以後不能改
#' @return list(version, sources, bank, versions, links, checks)
#' @noRd
kitsui_fixed_empty <- function(reference = KITSUI_REF_COHORT) {
  tables <- purrr::map(KITSUI_FIXED_COLS, \(cols) {
    kitsui_read_csv(I(paste(names(cols), collapse = ",")), cols)
  })
  version <- list(
    kits = as.character(utils::packageVersion("kits")), kitsUI = kitsui_version(),
    created = kitsui_today(), rounds = character(), reference = reference,
    scales = character()
  )
  sources <- tibble::tibble(round = character(), file = character(), md5 = character())
  c(list(version = version, sources = sources), tables)
}

#' 今天的日期（固定檔的 Created）：用台灣的日期，不用伺服器的時區（雲端版的伺服器在美國）。
#' 別的時區用 options(kitsui.tz = "Europe/London") 之類設定
#' @noRd
kitsui_today <- function() {
  format(Sys.time(), "%Y-%m-%d", tz = getOption("kitsui.tz", "Asia/Taipei"))
}

#' 預設的參照世代：資料有 M03 時是 M03（KIT），沒有時是第一個世代（依名稱）
#' @param cohorts 資料裡的世代
#' @noRd
kitsui_ref_default <- function(cohorts) {
  cohorts <- sort(unique(stats::na.omit(as.character(cohorts))))
  if (length(cohorts) == 0 || KITSUI_REF_COHORT %in% cohorts) return(KITSUI_REF_COHORT)
  cohorts[1]
}

#' ① 選的參照世代：選的世代在資料裡時用它，不然用預設（kitsui_ref_default()）
#' @noRd
kitsui_ref_pick <- function(pick, cohorts) {
  if (length(pick) == 1 && !is.na(pick) && pick %in% cohorts) return(pick)
  kitsui_ref_default(cohorts)
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
              "請看檢查表的原因，處理後重跑這一回合；",
              "或在 ② 把它們選成「這一回合不做」再校準一次，其他量尺就可以產生固定檔。")
  }
  dir <- file.path(tempfile("fixed"), "fixed_set")
  dir.create(dir, recursive = TRUE)
  # 給人看的說明（每個檔案是什麼、下一回合怎麼用）；讀固定檔時不讀它
  # 說明是英文；使用者給的名稱有中文時加 UTF-8 BOM，記事本打開才不會是亂碼
  readme <- enc2utf8(kitsui_fixed_readme(fixed))
  bom <- if (all(utf8ToInt(readme) < 128)) "" else "﻿"
  writeLines(paste0(bom, readme), file.path(dir, "README.txt"), useBytes = TRUE)
  csvs <- paste0(names(KITSUI_FIXED_COLS), ".csv")
  for (name in names(KITSUI_FIXED_COLS)) {
    exact <- dplyr::mutate(fixed[[name]], dplyr::across(
      dplyr::where(is.double), \(x) ifelse(is.na(x), NA, sprintf("%.17g", x))
    ))
    kitsui_write_fixed_csv(exact, file.path(dir, paste0(name, ".csv")))
  }
  # VERSION 先寫一次（沒有 Checksums），讀回來算它的 SHA-256；再加上每個 csv 的 SHA-256
  version <- file.path(dir, "VERSION")
  dcf <- kitsui_dcf(fixed)
  white <- kitsui_dcf_white(dcf[, "Scales"])
  write.dcf(dcf, version, keep.white = white)
  sums <- c(VERSION = kitsui_version_sha256(kitsui_read_dcf(version)),
            stats::setNames(kitsui_sha256(file.path(dir, csvs)), csvs))
  write.dcf(cbind(dcf, Checksums = kitsui_checksums_text(sums)), version, keep.white = white)
  zip::zip(zip_path, list.files(dir, full.names = TRUE), mode = "cherry-pick")
  invisible(zip_path)
}

#' 固定檔裡的一個 csv：內容都是英文時用 write_csv()；有中文（例如使用者的題號、領域）時
#' 用 write_excel_csv()（加 BOM），Excel 打開才不會是亂碼。讀回來時 readr 會略過 BOM
#' @noRd
kitsui_write_fixed_csv <- function(df, file) {
  text <- unlist(lapply(df, \(x) if (is.character(x)) x), use.names = FALSE)
  ascii <- all(!grepl("[^ -~]", c(names(df), stats::na.omit(text))))
  if (ascii) readr::write_csv(df, file, na = "") else readr::write_excel_csv(df, file, na = "")
}

#' 讀入固定檔（zip）
#'
#' 只讀 zip 根目錄（或唯一的一層資料夾）裡的檔案，不用 junkpaths：子資料夾裡同名的檔案
#' （例如 old/bank.csv）不會蓋掉真的檔案，而是擋下來。VERSION 有 Checksums 時核對每個檔案；
#' 有檔案被改過時擋下來（allow_modified = TRUE 時照樣讀，畫面上一直提醒）。
#' 核對的結果用 kitsui_fixed_integrity() 拿。
#' @param allow_modified 被改過的固定檔也讀（使用者在 ① 按了「仍然使用」）
#' @noRd
kitsui_read_fixed <- function(zip_path, allow_modified = FALSE) {
  open <- kitsui_open_fixed(zip_path)
  dir <- open$dir
  if (identical(open$integrity$status, "modified") && !isTRUE(allow_modified)) {
    stop(structure(
      class = c("kit_fixed_modified", "kit_user_error", "error", "condition"),
      list(message = open$integrity$text, call = NULL)
    ))
  }
  tables <- purrr::imap(KITSUI_FIXED_COLS, \(cols, name) {
    file <- file.path(dir, paste0(name, ".csv"))
    if (file.exists(file)) kitsui_read_csv(file, cols)
  })
  if (is.null(tables$waves)) {
    tables$waves <- tibble::tibble(scale = unique(tables$bank$scale), wave_m = NA_real_,
                                   round = NA_character_)
  }
  empty <- kitsui_fixed_empty()
  for (name in c("wave_summary", "pass_steps")) tables[[name]] <- tables[[name]] %||% empty[[name]]
  tables$versions$cohort <- dplyr::coalesce(tables$versions$cohort,
                                            scale_cohort(tables$versions$scale))
  dcf <- kitsui_read_dcf(file.path(dir, "VERSION"))
  c(kitsui_from_dcf(dcf), tables)
}

#' 固定檔有沒有被改過（不讀內容）
#' @return list(status, text, files, ignored)：status 是 "ok"（核對過，都沒有改）、
#'   "modified"（有檔案和 Checksums 不符，files 是哪些）或 "none"（kitsUI 0.3.12 以前的
#'   固定檔，沒有 Checksums，不能核對）；text 是給人看的一句話；ignored 是 zip 裡多出來、
#'   沒有讀的檔案
#' @noRd
kitsui_fixed_integrity <- function(zip_path) {
  kitsui_open_fixed(zip_path)$integrity
}

# 固定檔裡 kitsUI 會讀的檔案
kitsui_fixed_files <- function() c("VERSION", paste0(names(KITSUI_FIXED_COLS), ".csv"))

# zip 裡不算數的項目：資料夾本身、macOS 壓縮時加的 __MACOSX/ 與 ._ 檔、.DS_Store
kitsui_zip_junk <- function(names) {
  grepl("/$", names) | grepl("(^|/)__MACOSX/", names) | grepl("(^|/)\\._", names) |
    basename(names) %in% c(".DS_Store", "Thumbs.db")
}

#' 打開固定檔：檢查 zip 裡有什麼、解出要讀的檔案、核對 Checksums
#'
#' 根目錄沒有檔案、全部都在同一個資料夾裡時（解壓縮後再壓縮的），讀那個資料夾。
#' 其他資料夾裡的檔案不讀：和固定檔的檔案同名時擋下來（不知道哪一個才是真的），
#' 其他的列在 integrity$ignored。
#' @return list(dir, integrity)：dir 是解出來的資料夾
#' @noRd
kitsui_open_fixed <- function(zip_path) {
  listed <- tryCatch(suppressWarnings(zip::zip_list(zip_path))$filename, error = \(e) NULL)
  if (length(listed) == 0) {
    kit_abort("這不是固定檔（zip）。請上傳 kitsUI 在 ④ 下載的固定檔（kitsui_fixed_…zip）。")
  }
  names <- listed[!kitsui_zip_junk(listed)]
  # 根目錄沒有檔案、只有一層資料夾時，讀那個資料夾
  prefix <- ""
  top <- unique(sub("/.*$", "", names[grepl("/", names)]))
  if (length(names) > 0 && all(grepl("/", names)) && length(top) == 1 && !top %in% c(".", "..")) {
    prefix <- paste0(top, "/")
  }
  inside <- startsWith(names, prefix)
  rel <- substring(names, nchar(prefix) + 1)
  root <- inside & !grepl("/", rel)
  known <- kitsui_fixed_files()
  extra <- names[!root]
  twice <- extra[basename(extra) %in% c(known, "README.txt")]
  if (length(twice) > 0) {
    kit_abort("這個 zip 裡除了固定檔，還有同名的檔案：", paste(twice, collapse = "、"), "。",
              "不知道哪一個才是真的，所以不讀。",
              "請上傳 kitsUI 在 ④ 下載的原始固定檔，不要解壓縮後再壓縮。")
  }
  read <- rel[root & rel %in% known]
  ignored <- c(extra, names[root & !rel %in% c(known, "README.txt")])
  need <- c("VERSION", paste0(setdiff(names(KITSUI_FIXED_COLS), KITSUI_FIXED_OPTIONAL), ".csv"))
  miss <- setdiff(need, read)
  if (length(miss) > 0) {
    kit_abort("這個 zip 不是完整的固定檔：缺少 ", paste(miss, collapse = "、"), "。",
              "請上傳 kitsUI 在 ④ 下載的固定檔。")
  }
  dir <- tempfile("fixed")
  utils::unzip(zip_path, files = paste0(prefix, read), exdir = dir)
  dir <- file.path(dir, sub("/$", "", prefix))
  list(dir = dir, integrity = kitsui_check_sums(dir, read, ignored))
}

#' 核對 VERSION 的 Checksums
#' @param read 解出來的檔案（kitsui_fixed_files() 裡有的）
#' @noRd
kitsui_check_sums <- function(dir, read, ignored = character()) {
  dcf <- kitsui_read_dcf(file.path(dir, "VERSION"))
  sums <- kitsui_checksums_parse(if ("Checksums" %in% names(dcf)) dcf[["Checksums"]])
  out <- \(status, text, files = character()) {
    list(status = status, text = text, files = files, ignored = ignored)
  }
  if (length(sums) == 0) {
    return(out("none", paste0("這個固定檔是 kitsUI 0.3.12 以前做的，沒有核對碼，",
                              "不能確認有沒有被改過。")))
  }
  csvs <- setdiff(read, "VERSION")
  now <- c(VERSION = kitsui_version_sha256(dcf),
           stats::setNames(kitsui_sha256(file.path(dir, csvs)), csvs))
  files <- union(names(sums), names(now))
  bad <- files[is.na(sums[files]) | is.na(now[files]) | sums[files] != now[files]]
  if (length(bad) > 0) {
    return(out("modified", paste0(
      "這個固定檔被改過：", paste(bad, collapse = "、"), " 和 kitsUI 寫出時的核對碼（SHA-256）不同。",
      "改過的題庫或連結會讓舊題的參數、早先的分數對不上，",
      "②「核對」比對的也會是改過以後的值。",
      "請用 kitsUI 在 ④ 下載的原始固定檔。"
    ), bad))
  }
  out("ok", paste0("核對過：", length(now), " 個檔案和 kitsUI 寫出時完全相同（SHA-256）。"))
}

# VERSION 的欄位裡保留空白與換行的（一行一個檔案）
KITSUI_DCF_WHITE <- c("Sources", "Checksums")

# Scales 一行一個（領域名稱有逗號）時，Scales 也要保留換行
kitsui_read_dcf <- function(file) {
  lined <- any(grepl("^Scales:[[:space:]]*$", readLines(file, warn = FALSE)))
  read.dcf(file, keep.white = c(KITSUI_DCF_WHITE, if (lined) "Scales"))[1, ]
}

#' 檔案的 SHA-256：R 4.5 起 tools::sha256sum()；之前用 digest（plotly 會裝）
#' @noRd
kitsui_sha256 <- function(files) {
  tools_ns <- asNamespace("tools")
  if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
    return(unname(get("sha256sum", envir = tools_ns)(files)))
  }
  digest <- getExportedValue("digest", "digest")
  unname(vapply(files, \(f) digest(f, algo = "sha256", file = TRUE), ""))
}

# VERSION 本身的 SHA-256：除了 Checksums 以外的欄位（讀進來的值），一行「欄位: 值」
kitsui_version_sha256 <- function(dcf) {
  dcf <- dcf[setdiff(names(dcf), "Checksums")]
  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file))
  writeBin(charToRaw(enc2utf8(paste0(names(dcf), ": ", dcf, collapse = "\n"))), file)
  kitsui_sha256(file)
}

# Checksums 一行一個檔案：「sha256 雜湊值 檔名」
kitsui_checksums_text <- function(sums) {
  paste(c("", paste("sha256", sums, names(sums))), collapse = "\n")
}

kitsui_checksums_parse <- function(text) {
  if (length(text) == 0 || is.na(text)) return(stats::setNames(character(), character()))
  lines <- setdiff(trimws(strsplit(text, "\n")[[1]]), "")
  lines <- lines[grepl("^sha256 [0-9a-f]{64} .+$", lines)]
  stats::setNames(sub("^sha256 (\\S+) .*$", "\\1", lines), sub("^sha256 \\S+ ", "", lines))
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

# VERSION 的欄位；Sources 一行一個檔案：「波次 md5 檔名」。Scales 平常用 ", " 接起來；
# 有量尺名稱含 ", "（領域名稱有逗號）時改成一行一個，讀回來才拆得對
kitsui_dcf <- function(fixed) {
  v <- fixed$version
  src <- with(fixed$sources, paste(round, md5, file))
  waves <- purrr::map_chr(unique(fixed$waves$scale), \(s) {
    paste0(s, ": ", kitsui_wave_range(fixed, s))
  })
  cbind(
    Kits = v$kits, KitsUI = v$kitsUI, Created = v$created,
    Rounds = paste(v$rounds, collapse = ", "), Reference = v$reference,
    # 波次的單位（months、years、waves；第一回合在 ① 選）。沒有設定時不寫，讀回來當成月
    WaveUnit = v$wave_unit,
    Scales = if (any(grepl(", ", v$scales, fixed = TRUE))) {
      paste(c("", v$scales), collapse = "\n")
    } else {
      paste(v$scales, collapse = ", ")
    },
    Waves = paste(waves, collapse = "; "),
    Sources = paste(c("", src), collapse = "\n")
  )
}

# Scales 一行一個時，和 Sources 一樣保留換行（平常的 ", " 讓 write.dcf() 自己折行）
kitsui_dcf_white <- function(scales) {
  c(KITSUI_DCF_WHITE, if (startsWith(scales, "\n")) "Scales")
}

kitsui_from_dcf <- function(dcf) {
  split <- \(x) if (is.na(x) || x == "") character() else strsplit(x, ", ")[[1]]
  # 一行一個時，write.dcf() 在每一行前面加了一個空白
  lines <- \(x) if (grepl("\n", x, fixed = TRUE)) {
    sub("^ ", "", setdiff(strsplit(x, "\n")[[1]], ""))
  } else {
    split(x)
  }
  src <- setdiff(trimws(strsplit(dcf[["Sources"]], "\n")[[1]]), "")
  pick <- \(i) sub("^(\\S+) (\\S+) (.*)$", i, src)
  version <- list(
    kits = dcf[["Kits"]], kitsUI = dcf[["KitsUI"]], created = dcf[["Created"]],
    rounds = split(dcf[["Rounds"]]), reference = dcf[["Reference"]],
    scales = lines(dcf[["Scales"]])
  )
  if ("WaveUnit" %in% names(dcf) && !is.na(dcf[["WaveUnit"]])) {
    version$wave_unit <- dcf[["WaveUnit"]]
  }
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
