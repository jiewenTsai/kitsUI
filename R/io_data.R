# 讀檔、預覽與匯出

#' 讀取 CSV 或 SAV
#'
#' CSV 自動判斷編碼（UTF-8 → CP950/BIG5；兩種都不是時停下來）；SAV 保留變項與數值標籤。
#' 壓縮過的 CSV（.csv.gz）由 readr 直接讀。
#' 讀不了的檔案會以白話中文錯誤訊息 kit_abort()，由呼叫端顯示給使用者。
#'
#' @param path 檔案路徑
#' @param name 原始檔名（用來判斷副檔名）
#' @param col_types NULL 或具名字元向量（readr 的縮寫，例如 c(visit_id = "c")）：
#'   檔案裡有這些欄時照這個型別讀，其他欄自動判斷（只用於 CSV）
#' @return list(data, encoding, n_problems, problems)；encoding 為 "UTF-8"、"BIG5" 或
#'   NA（SAV）；problems 是 readr::problems()
#' @noRd
read_data_file <- function(path, name, col_types = NULL) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "gz" && grepl("[.]csv[.]gz$", tolower(name))) {
    return(read_csv_auto(gz_path(path), col_types))
  }
  if (ext == "csv") {
    return(read_csv_auto(path, col_types))
  }
  if (ext %in% c("sav", "zsav")) {
    data <- tryCatch(
      haven::read_sav(path),
      error = function(e) {
        kit_abort(
          "無法讀取 ", name, "：檔案可能已損壞，或不是 SPSS 檔。",
          "請用 SPSS 開啟確認後重新存檔再上傳。"
        )
      }
    )
    return(list(
      data = fix_utf8_df(data),
      encoding = NA_character_,
      n_problems = 0L,
      problems = NULL
    ))
  }
  kit_abort(
    "無法讀取 ", name, "：目前只支援 CSV、壓縮過的 CSV（.csv.gz）與 SPSS（.sav）檔。",
    if (ext %in% c("xlsx", "xls")) {
      "Excel 檔請先在 Excel 用「另存新檔 → CSV UTF-8（逗號分隔）」存成 CSV 再上傳。"
    }
  )
}

#' 預覽用：轉成方便閱讀的文字
#'
#' 整數不顯示小數點、標籤欄顯示「值 標籤」、日期顯示 yyyy-mm-dd。
#' @noRd
df_for_display <- function(df, n = NULL) {
  if (!is.null(n)) df <- utils::head(df, n)
  out <- lapply(df, function(x) {
    if (haven::is.labelled(x)) {
      return(as.character(haven::as_factor(x, levels = "both")))
    }
    if (inherits(x, c("Date", "POSIXt", "hms"))) return(format(x))
    if (is.numeric(x)) return(as.character(round(x, 2)))
    as.character(x)
  })
  tibble::as_tibble(out)
}

#' 預覽用：欄位過多時只取前 max_cols 欄（優先保留指定欄）
#' @noRd
preview_df_cols <- function(
    df,
    max_cols = KIT_PREVIEW_COLS,
    priority_cols = NULL) {
  cols <- names(df)
  if (length(cols) <= max_cols) return(df)
  priority_cols <- intersect(priority_cols, cols)
  keep <- unique(c(priority_cols, setdiff(cols, priority_cols)))
  df[, keep[seq_len(max_cols)], drop = FALSE]
}

#' 匯出用：將 key 欄位移到最前
#' @noRd
reorder_key_first <- function(df, key_cols) {
  key_cols <- intersect(key_cols, names(df))
  if (length(key_cols) == 0) return(df)
  df[, c(key_cols, setdiff(names(df), key_cols)), drop = FALSE]
}

#' 匯出 CSV：遺漏值一律寫成空白儲存格（SPSS／Excel 才不會把數值欄當成文字）
#' @noRd
write_kit_csv <- function(df, file) {
  readr::write_excel_csv(fix_utf8_df(df), file, na = "")
}

#' 匯出 SAV：不合法的 SPSS 變項名稱自動改名
#' @noRd
write_kit_sav <- function(df, file) {
  df <- fix_utf8_df(df)
  names(df) <- sav_safe_names(names(df))
  haven::write_sav(df, file)
}

#' 匯出 SAV 時會被改名的欄位（給 UI 事先提醒）
#' @return 具名字元向量：名稱為原欄名、值為新欄名；沒有要改的時候長度 0
#' @noRd
sav_renamed_cols <- function(cols) {
  new <- sav_safe_names(cols)
  changed <- new != cols
  stats::setNames(new[changed], cols[changed])
}

# SPSS 保留字，不能當變項名稱（不分大小寫）
SPSS_RESERVED <- c(
  "ALL", "AND", "BY", "EQ", "GE", "GT", "LE", "LT", "NE", "NOT", "OR", "TO",
  "WITH"
)

#' 轉成合法的 SPSS 變項名稱
#'
#' 規則（以 haven::write_sav() 實測）：只能有字母、數字與 _ . @ # $；
#' 開頭須為字母或 @；不能以 . 結尾；不能是保留字；最長 64 bytes；
#' 不分大小寫不可重複。
#' @noRd
sav_safe_names <- function(cols) {
  out <- gsub("[^[:alnum:]_.@#$]", "_", cols)
  out <- sub("\\.+$", "", out)
  needs_prefix <- !grepl("^[[:alpha:]@]", out)
  out[needs_prefix] <- paste0("v_", out[needs_prefix])
  reserved <- toupper(out) %in% SPSS_RESERVED
  out[reserved] <- paste0(out[reserved], "_")
  out <- vapply(
    out,
    truncate_bytes,
    character(1),
    max_bytes = 64L,
    USE.NAMES = FALSE
  )

  # 不分大小寫的重複：第二個起加上 _2、_3…
  for (i in seq_along(out)) {
    taken <- toupper(out[seq_len(i - 1)])
    if (!toupper(out[i]) %in% taken) next
    k <- 2L
    repeat {
      tail <- paste0("_", k)
      candidate <- paste0(
        truncate_bytes(out[i], 64L - nchar(tail, "bytes")),
        tail
      )
      if (!toupper(candidate) %in% c(taken, toupper(out[-seq_len(i)]))) break
      k <- k + 1L
    }
    out[i] <- candidate
  }
  out
}

#' 依 UTF-8 位元組數截斷（不切斷中文字）
#' @noRd
truncate_bytes <- function(x, max_bytes) {
  while (nchar(x, "bytes") > max_bytes) x <- substr(x, 1, nchar(x) - 1)
  x
}


# ── 內部工具 ─────────────────────────────────────────────────────────────────

#' 依序嘗試 UTF-8、CP950（BIG5）；兩種都不是時停下來，說怎麼另存
#'
#' readr 2.1.5 用 CP950 讀不合法的多位元組（例如 Latin-1、GBK、Shift-JIS 的檔案）又指定
#' col_types 時，整個 R 會當掉（segfault）。所以不是合法的 UTF-8 時，先用 iconv() 檢查每一列
#' 都能從 CP950 轉成 UTF-8，才交給 readr；不能時停下來（不再退回 Latin1 猜）。
#' @noRd
read_csv_auto <- function(path, col_types = NULL) {
  try_enc <- function(enc) {
    locale <- readr::locale(encoding = enc)
    # guess_max = Inf：用全部列判斷欄位型別，避免後段的文字值被默默變成 NA
    readr::read_csv(
      path,
      col_types = csv_col_spec(path, col_types, locale),
      show_col_types = FALSE,
      guess_max = Inf,
      locale = locale,
      progress = FALSE
    )
  }
  # readr 以 UTF-8 讀非 UTF-8 檔不會報錯（也不轉碼），需檢查結果是否為合法 UTF-8
  df <- tryCatch(try_enc("UTF-8"), error = function(e) NULL)
  if (!is.null(df) && is_valid_utf8_df(df)) return(csv_result(df, "UTF-8"))
  # CP950 為 BIG5 超集（Windows 繁中 Excel 另存的 CSV）：先確認每個位元組都轉得過去
  enc <- csv_bytes_encoding(path)
  if (identical(enc, "CP950")) {
    df <- tryCatch(try_enc("CP950"), error = function(e) NULL)
    if (!is.null(df) && is_valid_utf8_df(df)) return(csv_result(df, "BIG5"))
  }
  kit_abort(
    "無法讀取這個 CSV 檔：它的編碼不是 UTF-8，也不是繁體中文的 Big5（CP950），",
    "可能是 Latin-1、簡體中文（GBK）或日文（Shift-JIS）的檔案，或不是文字格式的 CSV。",
    "請用 Excel 開啟，另存為「CSV UTF-8（逗號分隔）」再上傳。"
  )
}

#' 檔案的位元組是合法的 UTF-8、CP950，還是都不是（不經過 readr，不會當掉）
#'
#' readLines() 不轉碼，只讀位元組；.csv.gz 用 gzfile() 一樣讀得到（沒壓縮的檔也可以）。
#' @return "UTF-8"、"CP950" 或 NA
#' @noRd
csv_bytes_encoding <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  lines <- tryCatch(readLines(con, warn = FALSE, skipNul = TRUE), error = function(e) NULL)
  if (is.null(lines)) return(NA_character_)
  if (all(validUTF8(lines))) return("UTF-8")
  if (!anyNA(iconv(lines, from = "CP950", to = "UTF-8"))) return("CP950")
  NA_character_
}

# readr 讀進來的文字已經是合法的 UTF-8（上面檢查過），不用再 fix_utf8_df()（大檔要好幾秒）
csv_result <- function(df, encoding) {
  problems <- readr::problems(df)
  list(
    data = df,
    encoding = encoding,
    n_problems = nrow(problems),
    problems = problems
  )
}

#' 指定型別的欄（檔案裡有的才給 readr，其他欄自動判斷）；col_types 是 NULL 時全部自動判斷
#' @noRd
csv_col_spec <- function(path, col_types, locale) {
  if (is.null(col_types)) return(NULL)
  head <- names(readr::read_csv(path, n_max = 0, locale = locale, show_col_types = FALSE,
                                progress = FALSE))
  keep <- col_types[names(col_types) %in% head]
  do.call(readr::cols, c(as.list(keep), list(.default = readr::col_guess())))
}

#' readr 看副檔名決定要不要解壓縮：路徑不是 .gz 結尾時，複製成 .gz 結尾的暫存檔
#' @noRd
gz_path <- function(path) {
  if (grepl("[.]gz$", path)) return(path)
  out <- tempfile(fileext = ".csv.gz")
  file.copy(path, out)
  out
}

is_valid_utf8_df <- function(df) {
  chr_ok <- vapply(df, function(x) {
    !is.character(x) || all(validUTF8(x[!is.na(x)]))
  }, logical(1))
  all(validUTF8(names(df))) && all(chr_ok)
}

fix_utf8_chr <- function(x) {
  if (!is.character(x)) return(x)
  iconv(x, from = "", to = "UTF-8", sub = "?")
}

#' 文字欄轉成 UTF-8；保留 SAV 的變項標籤與數值標籤
#' @noRd
fix_utf8_df <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), function(x) {
    if (is.factor(x)) {
      label <- attr(x, "label", exact = TRUE)
      x <- fix_utf8_chr(as.character(x))
      attr(x, "label") <- label
      return(x)
    }
    if (!is.character(x)) return(x)
    # iconv() 會清掉屬性（含 haven 的 class 與標籤），轉完再放回去
    attrs <- attributes(x)
    x <- fix_utf8_chr(as.vector(unclass(x)))
    attributes(x) <- attrs
    x
  }))
}
