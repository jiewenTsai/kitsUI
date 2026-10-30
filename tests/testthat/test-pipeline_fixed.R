test_that("空的固定檔寫出再讀回相同", {
  zip_path <- withr::local_tempfile(fileext = ".zip")
  fixed <- kitsui_fixed_empty()
  kitsui_write_fixed(fixed, zip_path)
  expect_identical(kitsui_read_fixed(zip_path), fixed)
})

test_that("有題庫、版本、連結、檢查的固定檔寫出再讀回相同，連結重建後係數相同", {
  lk <- linked()
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(lk$fixed, zip_path)
  back <- kitsui_read_fixed(zip_path)
  expect_identical(back, lk$fixed)
  expect_setequal(utils::unzip(zip_path, list = TRUE)$Name,
                  c("README.txt", "VERSION", "bank.csv", "versions.csv", "links.csv",
                    "checks.csv", "waves.csv"))
  links <- kitsui_links(back)
  expect_identical(coef(links[["lan M36"]]), coef(lk$internal))
  expect_identical(coef(links$D), coef(lk$D))
  expect_identical(links$D$intercept_error, lk$D$intercept_error)
  expect_identical(links[["lan M36"]]$slope_error, lk$internal$slope_error)
})

test_that("zip 缺檔時以中文說明", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "bank.csv"))
  zip_path <- file.path(dir, "bad.zip")
  zip::zip(zip_path, file.path(dir, "bank.csv"), mode = "cherry-pick")
  expect_error(kitsui_read_fixed(zip_path), "不是完整的固定檔：缺少 VERSION",
               class = "kit_user_error")
})

test_that("舊的固定檔：沒有 waves.csv、versions.csv 沒有 cohort 欄，也讀得進來", {
  chain <- lan_chain()
  fixed <- chain[["48"]]$fixed
  expect_equal(unique(fixed$versions$cohort), "M03")
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(fixed, zip_path)
  dir <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = dir)
  file.remove(file.path(dir, "waves.csv"))
  v <- readr::read_csv(file.path(dir, "versions.csv"), show_col_types = FALSE)
  readr::write_csv(v[c("scale", "item_id", "current_id")], file.path(dir, "versions.csv"))
  old_zip <- file.path(dir, "old.zip")
  zip::zip(old_zip, list.files(dir, full.names = TRUE), mode = "cherry-pick")
  old <- kitsui_read_fixed(old_zip)
  expect_identical(old$versions, fixed$versions)
  expect_identical(old$bank, fixed$bank)
  expect_equal(old$waves$scale, "lan M03")
  expect_true(is.na(old$waves$wave_m))
  # 新的固定檔記下各題庫校準過的波次；VERSION 也寫出範圍
  expect_equal(fixed$waves$wave_m, c(3, 6, 12, 18, 24, 36, 48))
  expect_match(read.dcf(file.path(dir, "VERSION"))[, "Waves"], "lan M03: 3-48")
})

test_that("不是 zip 的檔案：說「這不是固定檔（zip）」；zip 少了檔案時說少了什麼", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b\n1,2", csv)
  expect_error(kitsui_read_fixed(csv), "這不是固定檔（zip）", class = "kit_user_error")
  expect_error(kitsui_read_fixed(tempfile()), "這不是固定檔（zip）", class = "kit_user_error")
})

test_that("固定檔的 VERSION：kitsUI 的版本讀 DESCRIPTION（不是 dev）；第一回合的 Sources 寫進去", {
  desc <- kit_app_description()
  expect_false(is.na(desc$version))
  expect_equal(kitsui_version(), desc$version)
  expect_equal(kitsui_fixed_empty()$version$kitsUI, desc$version)
  r1 <- lan_chain()[["3"]]
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(r1$fixed, zip_path)
  dir <- withr::local_tempdir()
  utils::unzip(zip_path, exdir = dir)
  v <- read.dcf(file.path(dir, "VERSION"), keep.white = "Sources")
  expect_equal(unname(v[, "Rounds"]), "3")
  expect_match(v[, "Sources"], "^\\s*3 [0-9a-f]{32} data$")
  expect_equal(kitsui_read_fixed(zip_path)$sources, r1$fixed$sources)
})

test_that("固定檔裡有中文 README.txt；讀回來不受影響", {
  fixed <- linked()$fixed
  zip_path <- tempfile(fileext = ".zip")
  kitsui_write_fixed(fixed, zip_path)
  expect_true("README.txt" %in% basename(zip::zip_list(zip_path)$filename))
  back <- kitsui_read_fixed(zip_path)
  expect_equal(back$bank, fixed$bank)
})
