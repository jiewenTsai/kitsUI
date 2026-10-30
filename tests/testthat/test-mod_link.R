# ③ 連結：在 round8()（lan M36、motor M03 都是自由校準；lan M03 在題庫裡）上點按鈕

# 模組的輸入：data 與 items 都直接由 pipeline 做（不經過 ①、② 的模組）
link_args <- function(round, df, previous, fixed_file = NULL) {
  list(
    data = shiny::reactive(list(df = df, fixed = previous, round_label = "r8", files = NULL,
                                fixed_file = fixed_file)),
    items = shiny::reactive(round),
    goto = function(tab) NULL
  )
}

test_that("內部連結與外部連結各做一次，回傳加上連結的固定檔", {
  r8 <- round8()
  args <- link_args(r8$round, r8$df, lan_chain()[["48"]]$fixed)
  shiny::testServer(mod_link_server, args = args, {
    session$setInputs(internal_scale = "lan M36")
    expect_equal(internal()$state, "ready")
    expect_match(output$internal_status$html, "建立內部連結")
    # 按鈕旁一句：做了就凍結，不能復原
    expect_match(output$internal_status$html, "不能復原")
    expect_null(session$returned()$links[["lan M36"]])
    # 還沒有任何連結：不說「連結做好了」
    expect_match(output$next_step$html, "還沒有連結")
    expect_false(grepl("做好了", output$next_step$html))
    expect_match(output$next_step$html, "還可以建立：lan M36、外部連結")

    session$setInputs(make_internal = 1)
    out <- session$returned()
    expect_equal(out$links[["lan M36"]]$to_scale, "lan M03")
    expect_equal(coef(out$links[["lan M36"]]), coef(linked()$internal))
    expect_match(output$internal_status$html, "這一回合剛做好")
    expect_false(grepl("不能復原", output$internal_status$html))
    # 這一回合沒有 lan M03 的作答：獨立檢核做不了，說明原因
    html <- output$internal_result$html
    expect_match(html, "這次不能做")
    # 公式與係數直接寫在畫面上；圖還沒畫好時有一句「正在畫」
    expect_match(html, "lan M03 = ")
    expect_match(html, "linking_error")
    expect_match(html, "正在畫連結圖")
    expect_match(html, "（HTML）")
    expect_match(html, "（CSV）")
    expect_match(output$internal_checks, "全部通過")
    # 只建了這一條連結
    expect_named(link_objs(), "lan M36")

    # 再按一次也不會重做（沿用固定檔裡的連結）
    session$setInputs(make_internal = 2)
    expect_identical(session$returned()$fixed, out$fixed)

    # D：lan M03 2 個、motor M03 5 個錨題
    expect_equal(sum(anchors()$n_anchors), 7)
    expect_match(output$d_status$html, "建立外部連結")
    session$setInputs(make_d = 1)
    out <- session$returned()
    expect_setequal(names(out$links), c("lan M36", "D"))
    expect_equal(coef(out$links$D), coef(linked()$D))
    expect_equal(nrow(out$fixed$links), nrow(linked()$fixed$links))
    # 外部連結的檢查：kits 的英文訊息之外，一句中文說明
    expect_match(output$d_checks, "domain balance")
    expect_match(output$d_checks, "三分之二")
    expect_match(output$d_checks, "link_D")
    expect_false(grepl("object 1", output$d_checks))
    expect_match(output$d_result$html, "外部連結的檢查")
    expect_match(output$code, "kits_link_screen")
    # R 程式讀真的固定檔檔名（這一回合做的連結在 ④ 下載的新固定檔）
    expect_match(output$code, "kitsui_fixed_r8.zip")
    expect_false(grepl("fixed_set.zip", output$code))
    expect_match(output$links_table, "這一回合")
    expect_match(output$links_table, "> round </th")
    expect_match(output$links_table, "外部連結")
    expect_match(output$links_table, "內部連結")
    expect_match(output$links_table, "r8")
    expect_match(output$coverage, "建立外部連結時（第 r8 回合）")
    expect_match(output$coverage, "external_link")
    expect_match(output$next_step$html, "go_score")
    expect_match(output$next_step$html, "固定檔裡有 2 條連結")
    # 錨題數之後可能變多，但凍結的連結不重新估計
    expect_match(output$d_status$html, "錨題數可能變多")
    expect_match(output$d_status$html, "不會用新的錨題重新估計")
    # 這一回合每個領域只有一個世代：垂直連結不畫，一句話說明
    expect_match(output$vertical_ui$html, "只有一個世代")
  })
})

test_that("固定檔裡已有的連結：凍結、沿用，不給建立的按鈕", {
  r8 <- round8()
  lk <- linked()
  round <- r8$round
  round$fixed <- lk$fixed
  args <- link_args(round, r8$df, lk$fixed, fixed_file = "kitsui_fixed_r7.zip")
  shiny::testServer(mod_link_server, args = args, {
    session$setInputs(internal_scale = "lan M36")
    expect_equal(internal()$state, "frozen")
    expect_match(output$internal_status$html, "之前的回合")
    expect_match(output$internal_status$html, "這一回合開始時的固定檔")
    expect_false(grepl("上傳", output$internal_status$html))
    # 之前的回合做的連結：R 程式讀上傳的固定檔
    expect_match(output$code, 'unzip\\("kitsui_fixed_r7.zip"')
    expect_false(grepl("make_internal", output$internal_status$html))
    expect_equal(d_state()$state, "frozen")
    expect_identical(session$returned()$fixed, lk$fixed)
    expect_match(output$links_table, "之前的回合")
  })
})

test_that("錨題不到 3 個時不能做外部連結，顯示錨題數與 kits 的訊息", {
  r48 <- lan_chain()[["48"]]
  args <- link_args(r48, kitsui_demo("M03", 48, "lan"), lan_chain()[["36"]]$fixed)
  shiny::testServer(mod_link_server, args = args, {
    session$flushReact()
    expect_equal(anchors()$n_anchors, 2)
    expect_equal(d_state()$state, "missing")
    expect_match(output$d_status$html, "至少要 3 個")
    expect_match(output$d_status$html, "at least 3")
    expect_match(output$internal_status$html, "還沒有 M36")
  })
})

test_that("兩個世代都在這一回合時可以做獨立檢核；下載連結圖", {
  skip_on_cran()
  withr::local_envvar(RSTUDIO_PANDOC = "/opt/quarto/bin/tools/x86_64")
  both <- both_cohorts()
  args <- link_args(both$round, both$df, kitsui_fixed_empty())
  shiny::testServer(mod_link_server, args = args, {
    # 還沒有內部連結：垂直連結說明原因
    session$setInputs(internal_scale = "lan M36", vertical_domain = "lan")
    expect_match(output$vertical_ui$html, "vertical_domain")
    # 三個分頁：未連結畫得出來；內部、外部連結後說明原因
    body <- output$vertical_body$html
    expect_match(body, "vertical_tab")
    expect_match(body, "vertical_none_plot")
    expect_match(body, "未連結：兩個世代各在自己校準的量尺上")
    expect_match(body, "還沒有內部連結")
    expect_true(file.exists(output$vertical_none_dl))
    session$setInputs(make_internal = 1)
    expect_s3_class(vertical()$internal$vertical, "kits_vertical")
    expect_equal(vertical()$internal$scale, "lan M03")
    body <- output$vertical_body$html
    expect_match(body, "vertical_internal_plot")
    expect_match(body, "內部連結後：lan M36 換到 lan M03")
    expect_match(body, "還沒有外部連結")
    expect_true(file.exists(output$vertical_internal_dl))
    expect_match(output$code, "v_none <- kits_vertical\\(raw\\)")
    expect_match(output$code, "v_internal <- kits_vertical")
    expect_false(grepl("v_external", output$code))
    expect_match(output$internal_result$html, "做獨立檢核")
    # 不是每次都要做：收在預設收起來的 details 裡
    expect_match(output$internal_result$html, "<details class=\"kit-fold\">", fixed = TRUE)
    session$setInputs(run_check = 1)
    expect_s3_class(check_now(), "kits_link_check")
    # 小表與中文判斷；kits 的完整輸出收在 details 裡
    expect_match(output$check_table, "judgement")
    expect_match(output$check_table, "同年齡組")
    expect_match(output$check_table, "一致")
    html <- output$check_result$html
    expect_match(html, "判斷規則")
    expect_match(html, "<details>")
    expect_match(output$check_print, "same-age groups")
    expect_true(file.exists(output$internal_plot_dl))
    expect_gt(nrow(readr::read_csv(output$internal_points_dl, show_col_types = FALSE)), 0)
    expect_match(output$checks, "concurrent CML")
    expect_match(output$checks, "兩種方法不一致")
  })
})

test_that("做連結時分數不重算：垂直連結的分數只跟這一回合與領域有關", {
  both <- both_cohorts()
  args <- link_args(both$round, both$df, kitsui_fixed_empty())
  shiny::testServer(mod_link_server, args = args, {
    session$setInputs(internal_scale = "lan M36", vertical_domain = "lan")
    raw <- v_raw()
    expect_named(raw, c("lan M03", "lan M36"))
    session$setInputs(make_internal = 1)
    expect_identical(v_raw(), raw)
    expect_equal(vertical()$internal$scale, "lan M03")
  })
})
