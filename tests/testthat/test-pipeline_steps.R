# 通過步驟（得分至少 step 算通過）：題目表的門檻與 age50、固定檔的 pass_steps、版本、檢查
# 示範資料的題都是 0–3 分；把 lanc02 設成 2 分以上才算通過

test_that("通過步驟改了門檻與 age50：門檻是那一步的門檻，age50 比 1 分以上晚", {
  tw <- two_cohorts_linked()
  item <- tw$round$items[["lan M03"]]
  one <- kitsui_item_table(tw$round, tw$df, tw$fixed, "lan M03")
  two <- kitsui_item_table(tw$round, tw$df, tw$fixed, "lan M03", steps = c(lanc02 = 2L))
  expect_false("step" %in% names(one))
  expect_equal(two$step[two$item_id == "lanc02"], 2)
  expect_equal(unique(two$step[two$item_id != "lanc02"]), 1)
  co <- coef(item)
  thr <- \(s) co$thr[co$item_id == "lanc02" & co$item_score == s]
  expect_equal(one$D[one$item_id == "lanc02"], thr(1))
  expect_equal(two$D[two$item_id == "lanc02"], thr(2))
  expect_gt(two$age50[two$item_id == "lanc02"], one$age50[one$item_id == "lanc02"])
  # 其他題不變
  others <- one$item_id != "lanc02"
  expect_equal(two$D[match(one$item_id[others], two$item_id)], one$D[others])
  expect_equal(two$age50[match(one$item_id[others], two$item_id)], one$age50[others])
  # 換到 D 的門檻也是第 2 步
  links <- kitsui_links(tw$fixed)
  full <- coef(kits::kits_convert(item, links[["D"]]))
  d <- attr(two, "thresholds")
  expect_equal(d$d_threshold[d$item_id == "lanc02"],
               full$thr[full$item_id == "lanc02" & full$item_score == 2])
  # 畫面上的表：step 在題號旁邊
  shown <- kitsui_item_table_shown(two)
  expect_equal(names(shown)[1:2], c("item_id", "step"))
  expect_false("step" %in% names(kitsui_item_table_shown(one)))
})

test_that("新版本的題沿用原來題號的設定（lana02 -> lana02_r12）", {
  ch <- lan_chain()
  r <- ch[["18"]]
  item <- r$items[["lan M03"]]
  vers <- r$fixed$versions
  expect_true("lana02_r12" %in% coef(item)$item_id)
  expect_equal(kitsui_base_ids(c("lana02_r12", "lana01"), vers), c("lana02", "lana01"))
  expect_equal(kitsui_item_step_arg(item, c(lana02 = 2L), vers), c(lana02_r12 = 2L))
  # 都是 1、沒有設定、校準裡沒有那個分數：用 1
  expect_equal(kitsui_item_step_arg(item, c(lana02 = 1L), vers), 1)
  expect_equal(kitsui_item_step_arg(item, kitsui_steps_vec(NULL), vers), 1)
  expect_equal(kitsui_item_step_arg(item, c(lana02 = 9L), vers), 1)
  tab <- kitsui_item_table(r, kitsui_demo("M03", 18, "lan"), r$fixed, "lan M03",
                           steps = c(lana02 = 2L))
  expect_equal(tab$step[tab$item_id == "lana02_r12"], 2)
  # 這一回合的題：目前的題號與原來的題號
  known <- kitsui_round_item_steps(r$items, vers)
  expect_equal(known$item_id[known$current_id == "lana02_r12"], "lana02")
  co <- coef(item)
  expect_equal(known$top[known$current_id == "lana02_r12"],
               max(co$item_score[co$item_id == "lana02_r12"]))
  # 上傳時用新版本的題號也可以：換回原來的題號
  chk <- kitsui_steps_check(tibble::tibble(item_id = "lana02_r12", step = "2"), known)
  expect_equal(chk$steps, c(lana02 = 2L))
})

test_that("固定檔：寫出再讀回相同；下一回合用它當預設；沒有改時固定檔原樣", {
  r8 <- round8()
  lk <- linked()
  expect_identical(kitsui_fixed_pass_steps(lk$fixed, kitsui_steps_vec(NULL), "r8"), lk$fixed)
  f1 <- kitsui_fixed_pass_steps(lk$fixed, c(lanb08 = 2L, cogD01 = 3L), "r8")
  expect_equal(f1$pass_steps$item_id, c("lanb08", "cogD01"))
  expect_equal(f1$pass_steps$step, c(2L, 3L))
  expect_equal(unique(f1$pass_steps$round), "r8")
  zip_path <- withr::local_tempfile(fileext = ".zip")
  kitsui_write_fixed(f1, zip_path)
  back <- kitsui_read_fixed(zip_path)
  expect_equal(back$pass_steps, f1$pass_steps)
  # 讀回來的固定檔：題目表預設用它的設定
  tab <- kitsui_item_table(r8$round, r8$df, back, "lan M36")
  expect_equal(tab$step[tab$item_id == "lanb08"], 2)
  # 下一回合（kitsui_round）帶著設定
  nxt <- suppressMessages(kitsui_round(kitsui_demo("M03", 3, "motor"), back, "r9"))
  expect_equal(nxt$fixed$pass_steps, f1$pass_steps)
  # 一題一列，最新的贏：沒改的留原來的回合，改了的記這一回合；改回 1 的留一列 step = 1
  f2 <- kitsui_fixed_pass_steps(back, c(lanb08 = 3L, cogD01 = 3L), "r9")
  expect_equal(f2$pass_steps$step, c(3L, 3L))
  expect_equal(f2$pass_steps$round, c("r9", "r8"))
  f3 <- kitsui_fixed_pass_steps(f2, c(cogD01 = 3L), "r10")
  expect_equal(f3$pass_steps$step[f3$pass_steps$item_id == "lanb08"], 1L)
  expect_equal(f3$pass_steps$round[f3$pass_steps$item_id == "lanb08"], "r10")
  expect_identical(kitsui_fixed_pass_steps(f3, kitsui_steps_vec(f3$pass_steps), "r11"), f3)
  expect_equal(kitsui_steps_vec(f3$pass_steps), c(lanb08 = 1L, cogD01 = 3L))
})

test_that("上傳的設定檔：擋下來的錯誤與只提醒的題，都是中文", {
  r8 <- round8()
  known <- kitsui_round_item_steps(r8$round$items, r8$round$fixed$versions)
  chk <- \(id, step) kitsui_steps_check(tibble::tibble(item_id = id, step = step), known)
  ok <- chk(c("lanb08", "lanb09", " "), c("2", "3", ""))
  expect_equal(ok$steps, c(lanb08 = 2L, lanb09 = 3L))
  expect_length(ok$errors, 0)
  expect_length(ok$warnings, 0)
  # 缺欄
  miss <- kitsui_steps_check(tibble::tibble(item_id = "lanb08"), known)
  expect_null(miss$steps)
  expect_match(miss$errors, "缺少：step")
  expect_match(kitsui_steps_check(tibble::tibble(x = 1), known)$errors, "item_id、step")
  # 不是 1 以上的整數
  bad <- chk(c("lanb08", "lanb09", "lanc01", "lanc02"), c("0", "1.5", "", "two"))
  expect_null(bad$steps)
  expect_match(bad$errors, "step 要是 1 以上的整數")
  expect_match(bad$errors, "lanb08（0）、lanb09（1.5）、lanc01（空白）、lanc02（two）")
  # 同一題兩次、step 不一樣（一樣時沒關係）
  expect_match(chk(c("lanb08", "lanb08"), c("2", "3"))$errors, "同一題列了兩次以上.*lanb08")
  expect_equal(chk(c("lanb08", "lanb08"), c("2", "2"))$steps, c(lanb08 = 2L))
  # 校準裡沒有這麼高的分數
  high <- chk("lanb08", "4")
  expect_match(high$errors, "lanb08（設定 4，最高 3 分）")
  # 不認得的題：提醒，不擋
  away <- chk(c("lanb08", "cogD01"), c("2", "3"))
  expect_equal(away$steps, c(lanb08 = 2L, cogD01 = 3L))
  expect_length(away$errors, 0)
  expect_match(away$warnings[1], "校準裡沒有這幾題：cogD01")
  expect_match(paste(away$warnings, collapse = ""), "存進固定檔")
  expect_match(chk(character(), character())$errors, "沒有任何一題")
  # 讀不進來的檔案
  expect_error(kitsui_read_steps(tempfile()), "讀不進這個檔案")
  # 範本：每一題一列、目前的 step；套用只改列出的題
  tpl <- kitsui_steps_template(known, c(lanb08 = 2L))
  expect_equal(names(tpl), c("item_id", "step", "scale", "max_score"))
  expect_setequal(tpl$item_id, known$item_id)
  expect_equal(tpl$step[tpl$item_id == "lanb08"], 2L)
  expect_equal(kitsui_steps_merge(c(lanb08 = 2L, cogD01 = 3L), c(lanb08 = 1L, lanb09 = 3L)),
               c(cogD01 = 3L, lanb09 = 3L))
  shown <- kitsui_steps_shown(known, c(lanb08 = 2L, cogD01 = 3L))
  expect_equal(shown$scale, c("lan M36", NA))
})

test_that("① 作答設計圖：通過步驟不是 1 的題，通過比例是得分達到通過步驟以上", {
  df <- kitsui_demo("M03", c(3, 6), "lan")
  r <- kitsui_resp(df, "lan M03")
  passed <- kitsui_resp_pass(r, c(lana01 = 2L, cogD01 = 3L))
  expect_equal(passed$lana01, as.integer(r$lana01 >= 2))
  expect_equal(passed$lana02, r$lana02)
  expect_identical(kitsui_resp_pass(r, kitsui_steps_vec(NULL)), r)
  z <- \(p) {
    b <- plotly::plotly_build(p)$x$data[[1]]
    b$z[, which(b$x == "lana01")]
  }
  want <- tapply(r$lana01 >= 2, r$wave_m, mean, na.rm = TRUE)
  expect_equal(unname(z(plot(passed))), unname(as.numeric(want)))
  expect_true(all(z(plot(passed)) < z(plot(r))))
})
