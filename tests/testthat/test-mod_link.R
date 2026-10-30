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
    # 下載鈕的位置；鈕在 server 端畫，字是（HTML）（CSV）
    expect_match(html, session$ns("internal_plot_dl_ui"))
    expect_match(html, session$ns("internal_points_dl_ui"))
    expect_match(output$internal_points_dl_ui$html, "下載內部連結的點（CSV）")
    expect_equal(kit_test_download_name(session, "internal_points_dl"), "link_lan_M36_points.csv")
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
    # lan 這一回合只有 M36：之前回合 lan M03 的點（固定檔的摘要）照現在的連結一起畫
    expect_match(output$vertical_ui$html, "vertical_domain")
    session$setInputs(vertical_domain = "lan")
    body <- output$vertical_body$html
    expect_match(body, "vertical_external_plot")
    expect_match(body, "空心、較淡的點是之前的回合（3、6、12、18、24、36、48）")
    expect_equal(vertical()$external$scale, "D")
    expect_setequal(vertical()$external$table$source, c("this", "earlier"))
    # 檢查只看這一回合兩個世代的比較：只有一個世代時不放進檢查
    expect_null(vertical_now())
    # R 程式：讀新固定檔的 wave_summary.csv，照連結換到 D
    expect_match(output$code, 'unzip\\("kitsui_fixed_r8.zip"')
    expect_match(output$code, "wave_summary.csv")
    expect_match(output$code, "`lan M36` = list\\(link_lan_M36, link_D\\)")
  })
})

test_that("舊的固定檔沒有各波次摘要：一個世代的回合說明摘要從哪一版開始、怎麼補", {
  b <- borrow_chain()
  r2 <- b$r2
  r2$fixed$wave_summary <- r2$fixed$wave_summary[r2$fixed$wave_summary$round == "2", ]
  args <- link_args(r2, b$df2, b$fixed)
  shiny::testServer(mod_link_server, args = args, {
    session$flushReact()
    html <- output$vertical_ui$html
    expect_match(html, "只有一個世代")
    expect_match(html, "wave_summary.csv")
    expect_match(html, "重跑之前的回合")
  })
})

test_that("借用另一世代題庫的回合（只有 M03 60 個月）：垂直連結畫得出之前回合兩個世代的點", {
  skip_on_cran()
  # 垂直連結圖的下載（HTML）要 pandoc
  withr::local_envvar(RSTUDIO_PANDOC = "/opt/quarto/bin/tools/x86_64")
  b <- borrow_chain()
  args <- link_args(b$r2, b$df2, b$fixed)
  shiny::testServer(mod_link_server, args = args, {
    session$setInputs(vertical_domain = "motor")
    expect_match(output$vertical_ui$html, "vertical_domain")
    v <- vertical()
    expect_equal(v$internal$scale, "motor M03")
    expect_equal(v$external$scale, "D")
    tab <- v$external$table
    expect_equal(tab$wave_m[tab$source == "this"], 60)
    expect_setequal(tab$cohort[tab$source == "earlier"], c("M03", "M36"))
    expect_match(output$vertical_body$html, "vertical_internal_plot")
    html <- output$vertical_internal_dl_ui$html
    expect_match(html, "下載這張垂直連結圖（HTML）")
    expect_match(kit_link_attr(html, session$ns("vertical_internal_dl"), "href"),
                 "/vertical_motor_internal\\.html\\?")
    res <- kit_test_download(session, "vertical_internal_dl")
    expect_equal(res$status, 200)
    expect_equal(res$content_type, "text/html; charset=UTF-8")
    expect_true(file.exists(res$content$file))
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
    expect_match(output$internal_status$html, "還沒有參照世代（\\s*M03\\s*）以外的世代的量尺")
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
    expect_match(body, "未連結：各世代在自己校準的量尺上")
    # 第一個回合：沒有之前回合的點，不說空心的點
    expect_false(grepl("空心", body))
    expect_match(body, "還沒有內部連結")
    expect_true(file.exists(kit_test_dl_file(output, session, "vertical_none_dl")))
    expect_equal(kit_test_download_name(session, "vertical_none_dl"), "vertical_lan_none.html")
    session$setInputs(make_internal = 1)
    expect_s3_class(vertical()$internal$vertical, "kits_vertical")
    expect_equal(vertical()$internal$scale, "lan M03")
    body <- output$vertical_body$html
    expect_match(body, "vertical_internal_plot")
    expect_match(body, "內部連結後：lan M36 換到 lan M03")
    expect_match(body, "還沒有外部連結")
    expect_true(file.exists(kit_test_dl_file(output, session, "vertical_internal_dl")))
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
    expect_true(file.exists(kit_test_dl_file(output, session, "internal_plot_dl")))
    expect_equal(kit_test_download_name(session, "internal_plot_dl"), "link_lan_M36.html")
    expect_match(output$internal_plot_dl_ui$html, "下載內部連結圖（HTML）")
    points <- kit_test_dl_file(output, session, "internal_points_dl")
    expect_gt(nrow(readr::read_csv(points, show_col_types = FALSE)), 0)
    expect_equal(kit_test_download_name(session, "internal_points_dl"), "link_lan_M36_points.csv")
    old <- withr::local_tempfile(fileext = ".csv")
    readr::write_excel_csv(as.data.frame(internal_link()$points), old)
    expect_equal(file_bytes(points), file_bytes(old))
    # 獨立檢核的結果
    chk <- readr::read_csv(kit_test_dl_file(output, session, "check_dl"), show_col_types = FALSE)
    expect_equal(nrow(chk), nrow(as.data.frame(check_now()$at)))
    expect_equal(kit_test_download_name(session, "check_dl"), "link_lan_M36_check.csv")
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
