# 讀檔、預覽與匯出

#' 讀取 CSV 或 SAV
#'
#' CSV 自動判斷編碼（UTF-8 → CP950/BIG5；兩種都不是時停下來）；SAV 保留變項與數值標籤。
#' 壓縮過的 CSV（.csv.gz）由 readr 直接讀。
#' 讀不了的檔案會以白話中文錯誤訊息 kitDesign::kit_abort()，由呼叫端顯示給使用者。
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
    return(read_csv_auto(kitDesign::kit_gz_path(path), col_types))
  }
  if (ext == "csv") {
    return(read_csv_auto(path, col_types))
  }
  if (ext %in% c("sav", "zsav")) {
    data <- tryCatch(
      haven::read_sav(path),
      error = function(e) {
        kitDesign::kit_abort(
          # 原文：無法讀取  | ：檔案可能已損壞，或不是 SPSS 檔。
          "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u6a94\u6848\u53ef\u80fd\u5df2\u640d\u58de\uff0c\u6216\u4e0d\u662f SPSS \u6a94\u3002",
          # 原文：請用 SPSS 開啟確認後重新存檔再上傳。
          "\u8acb\u7528 SPSS \u958b\u555f\u78ba\u8a8d\u5f8c\u91cd\u65b0\u5b58\u6a94\u518d\u4e0a\u50b3\u3002"
        )
      }
    )
    return(list(
      data = kitDesign::kit_fix_utf8_df(data),
      encoding = NA_character_,
      n_problems = 0L,
      problems = NULL
    ))
  }
  kitDesign::kit_abort(
    # 原文：無法讀取  | ：目前只支援 CSV、壓縮過的 CSV（.csv.gz）與 SPSS（.sav）檔。
    "\u7121\u6cd5\u8b80\u53d6 ", name, "\uff1a\u76ee\u524d\u53ea\u652f\u63f4 CSV\u3001\u58d3\u7e2e\u904e\u7684 CSV\uff08.csv.gz\uff09\u8207 SPSS\uff08.sav\uff09\u6a94\u3002",
    if (ext %in% c("xlsx", "xls")) {
      # 原文：Excel 檔請先在 Excel 用「另存新檔 → CSV UTF-8（逗號分隔）」存成 CSV 再上傳。
      "Excel \u6a94\u8acb\u5148\u5728 Excel \u7528\u300c\u53e6\u5b58\u65b0\u6a94 \u2192 CSV UTF-8\uff08\u9017\u865f\u5206\u9694\uff09\u300d\u5b58\u6210 CSV \u518d\u4e0a\u50b3\u3002"
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

#' 匯出 SAV：不合法的 SPSS 變項名稱自動改名
#' @noRd
write_kit_sav <- function(df, file) {
  df <- kitDesign::kit_fix_utf8_df(df)
  names(df) <- kitDesign::kit_sav_safe_names(names(df))
  haven::write_sav(df, file)
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
  if (!is.null(df) && kitDesign::kit_is_valid_utf8_df(df)) return(csv_result(df, "UTF-8"))
  # CP950 為 BIG5 超集（Windows 繁中 Excel 另存的 CSV）：先確認每個位元組都轉得過去
  enc <- csv_bytes_encoding(path)
  if (identical(enc, "CP950")) {
    df <- tryCatch(try_enc("CP950"), error = function(e) NULL)
    if (!is.null(df) && kitDesign::kit_is_valid_utf8_df(df)) return(csv_result(df, "BIG5"))
  }
  kitDesign::kit_abort(
    # 原文：無法讀取這個 CSV 檔：它的編碼不是 UTF-8，也不是繁體中文的 Big5（CP950），
    "\u7121\u6cd5\u8b80\u53d6\u9019\u500b CSV \u6a94\uff1a\u5b83\u7684\u7de8\u78bc\u4e0d\u662f UTF-8\uff0c\u4e5f\u4e0d\u662f\u7e41\u9ad4\u4e2d\u6587\u7684 Big5\uff08CP950\uff09\uff0c",
    # 原文：可能是 Latin-1、簡體中文（GBK）或日文（Shift-JIS）的檔案，或不是文字格式的 CSV。
    "\u53ef\u80fd\u662f Latin-1\u3001\u7c21\u9ad4\u4e2d\u6587\uff08GBK\uff09\u6216\u65e5\u6587\uff08Shift-JIS\uff09\u7684\u6a94\u6848\uff0c\u6216\u4e0d\u662f\u6587\u5b57\u683c\u5f0f\u7684 CSV\u3002",
    # 原文：請用 Excel 開啟，另存為「CSV UTF-8（逗號分隔）」再上傳。
    "\u8acb\u7528 Excel \u958b\u555f\uff0c\u53e6\u5b58\u70ba\u300cCSV UTF-8\uff08\u9017\u865f\u5206\u9694\uff09\u300d\u518d\u4e0a\u50b3\u3002"
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

# readr 讀進來的文字已經是合法的 UTF-8（上面檢查過），不用再 kit_fix_utf8_df()（大檔要好幾秒）
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

