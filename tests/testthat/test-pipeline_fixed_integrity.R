# 固定檔的核對碼（B6）：VERSION 的 Checksums（每個檔案的 SHA-256）；上傳時核對；
# 不用 junkpaths，子資料夾裡同名的檔案不會蓋掉真的檔案

# 解開固定檔，改一下，再壓回去（edit 是 function(dir)）
rezip <- function(zip_path, edit, prefix = NULL) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  inner <- if (is.null(prefix)) dir else file.path(dir, prefix)
  dir.create(inner, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = inner)
  edit(inner)
  out <- file.path(dir, "out.zip")
  old <- setwd(dir)
  on.exit(setwd(old))
  zip::zip(out, setdiff(list.files(".", recursive = TRUE, all.files = TRUE), "out.zip"))
  out
}

fixed_zip <- function(env = parent.frame()) {
  zip_path <- withr::local_tempfile(fileext = ".zip", .local_envir = env)
  kitsui_write_fixed(linked()$fixed, zip_path)
  zip_path
}

test_that("寫出的固定檔：VERSION 有每個檔案的 SHA-256；讀回來核對過、內容不變", {
  zip_path <- fixed_zip()
  dir <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = dir)
  v <- read.dcf(file.path(dir, "VERSION"), keep.white = KITSUI_DCF_WHITE)
  sums <- kitsui_checksums_parse(v[, "Checksums"])
  expect_setequal(names(sums), kitsui_fixed_files())
  expect_equal(unname(sums["bank.csv"]), kitsui_sha256(file.path(dir, "bank.csv")))
  # zip 裡的檔案沒有變多（Checksums 在 VERSION 裡）
  expect_setequal(utils::unzip(zip_path, list = TRUE)$Name,
                  c("README.txt", kitsui_fixed_files()))
  ok <- kitsui_fixed_integrity(zip_path)
  expect_equal(ok$status, "ok")
  expect_match(ok$text, "核對過：8 個檔案")
  expect_equal(kitsui_read_fixed(zip_path)$bank, kitsui_read_fixed(zip_path, TRUE)$bank)
})

test_that("改過 bank.csv：擋下來（kit_fixed_modified）；仍然使用時讀進改過的值", {
  zip_path <- fixed_zip()
  bad <- rezip(zip_path, \(dir) {
    b <- readr::read_csv(file.path(dir, "bank.csv"), col_types = readr::cols(.default = "c"))
    b$beta[1] <- as.character(as.numeric(b$beta[1]) + 0.5)
    readr::write_csv(b, file.path(dir, "bank.csv"), na = "")
  })
  expect_error(kitsui_read_fixed(bad), "被改過：bank.csv", class = "kit_fixed_modified")
  integ <- kitsui_fixed_integrity(bad)
  expect_equal(integ$status, "modified")
  expect_equal(integ$files, "bank.csv")
  back <- kitsui_read_fixed(bad, allow_modified = TRUE)
  expect_equal(back$bank$beta[1], linked()$fixed$bank$beta[1] + 0.5, tolerance = 1e-12)
})

test_that("改過 links.csv 或 VERSION 的參照世代也抓得到", {
  zip_path <- fixed_zip()
  links <- rezip(zip_path, \(dir) {
    l <- readLines(file.path(dir, "links.csv"))
    writeLines(sub(",", ",", l), file.path(dir, "links.csv"))
    cat("\n", file = file.path(dir, "links.csv"), append = TRUE)
  })
  expect_equal(kitsui_fixed_integrity(links)$files, "links.csv")
  version <- rezip(zip_path, \(dir) {
    v <- readLines(file.path(dir, "VERSION"))
    writeLines(sub("^Reference: M03$", "Reference: M36", v), file.path(dir, "VERSION"))
  })
  expect_equal(kitsui_fixed_integrity(version)$files, "VERSION")
  expect_error(kitsui_read_fixed(version), "VERSION", class = "kit_fixed_modified")
})

test_that("舊的固定檔（沒有 Checksums）：照樣讀，說明不能核對", {
  zip_path <- fixed_zip()
  old <- rezip(zip_path, old_version_file)
  integ <- kitsui_fixed_integrity(old)
  expect_equal(integ$status, "none")
  expect_match(integ$text, "不能確認有沒有被改過")
  back <- kitsui_read_fixed(old)
  expect_equal(back$bank, kitsui_read_fixed(zip_path)$bank)
  expect_equal(back$version, kitsui_read_fixed(zip_path)$version)
})

test_that("子資料夾裡同名的檔案（old/bank.csv）不會蓋掉 bank.csv：擋下來", {
  zip_path <- fixed_zip()
  bad <- rezip(zip_path, \(dir) {
    dir.create(file.path(dir, "old"))
    writeLines(c("scale,item_id,item_score,beta,beta_se,round", "BAD,x,1,0,0,1"),
               file.path(dir, "old", "bank.csv"))
  })
  expect_error(kitsui_read_fixed(bad), "同名的檔案：old/bank.csv", class = "kit_user_error")
})

test_that("解壓縮後整個資料夾再壓縮（一層資料夾、macOS 的 __MACOSX）：照樣讀，核對過", {
  zip_path <- fixed_zip()
  wrapped <- rezip(zip_path, \(dir) {
    dir.create(file.path(dirname(dir), "__MACOSX", "fixed_set"), recursive = TRUE)
    writeLines("junk", file.path(dirname(dir), "__MACOSX", "fixed_set", "._bank.csv"))
  }, prefix = "fixed_set")
  expect_equal(kitsui_fixed_integrity(wrapped)$status, "ok")
  expect_equal(kitsui_read_fixed(wrapped)$links, kitsui_read_fixed(zip_path)$links)
})

test_that("根目錄多出來的檔案：不讀，列出來", {
  zip_path <- fixed_zip()
  extra <- rezip(zip_path, \(dir) writeLines("hello", file.path(dir, "notes.txt")))
  integ <- kitsui_fixed_integrity(extra)
  expect_equal(integ$status, "ok")
  expect_equal(integ$ignored, "notes.txt")
  expect_equal(kitsui_read_fixed(extra)$bank, kitsui_read_fixed(zip_path)$bank)
})

test_that("① 上傳改過的固定檔：紅字與「仍然使用」；按了以後固定檔資訊一直提醒", {
  zip_path <- fixed_zip()
  bad <- rezip(zip_path, \(dir) {
    b <- readLines(file.path(dir, "bank.csv"))
    writeLines(c(b, b[2]), file.path(dir, "bank.csv"))
  })
  up <- \(p, n) data.frame(name = n, size = file.size(p), type = "", datapath = p)
  shiny::testServer(mod_data_server, args = list(goto = \(tab) NULL), {
    session$setInputs(source = "demo", domains = "lan", waves = "M03 3", round_label = "")
    session$setInputs(fixed_file = up(zip_path, "good.zip"))
    expect_match(output$fixed_info$html, "核對過")
    session$setInputs(fixed_file = up(bad, "bad.zip"))
    expect_match(output$fixed_error$html, "這個固定檔被改過：bank.csv")
    expect_match(output$fixed_error$html, "fixed_force")
    expect_match(output$fixed_error$html, "仍然用之前的固定檔：good.zip")
    expect_match(output$fixed_info$html, "good.zip")
    session$setInputs(fixed_force = 1)
    expect_null(output$fixed_error)
    expect_match(output$fixed_info$html, "bad.zip")
    expect_match(output$fixed_info$html, "kit-warn-text")
    expect_match(output$fixed_info$html, "這個固定檔被改過")
    expect_equal(nrow(session$getReturned()()$fixed$bank), nrow(linked()$fixed$bank) + 1)
  })
})
