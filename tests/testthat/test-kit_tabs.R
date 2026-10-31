# 步驟分頁的進度：做完的打 ✓、還不能做的灰色（原因）、結果過期時 ✓ 拿掉

test_that("每個分頁的狀態：還不能做、可以做、做完", {
  st <- \(...) {
    s <- kitsui_tab_states(...)
    stats::setNames(s$state, s$tab)
  }
  expect_equal(st(FALSE), c(data = "ready", items = "locked", link = "locked", score = "locked"))
  expect_equal(kitsui_tab_states(FALSE)$why,
               c("", "先在「① 資料」選好這一回合的資料", "先做「② 題目校準」", "先做「② 題目校準」"))
  expect_equal(st(TRUE), c(data = "done", items = "ready", link = "locked", score = "locked"))
  ok <- list(ok = TRUE, items = list(a = 1))
  expect_equal(st(TRUE, ok), c(data = "done", items = "done", link = "ready", score = "ready"))
  # 有量尺停下來：② 沒有做完，③、④ 仍然可以先看
  part <- list(ok = FALSE, items = list(a = 1))
  expect_equal(st(TRUE, part),
               c(data = "done", items = "ready", link = "ready", score = "ready"))
  # ② 沒有 ✓ 時 ③ 也不打 ✓（「沒有連結可以做」不算做完）
  expect_equal(st(TRUE, part, TRUE),
               c(data = "done", items = "ready", link = "ready", score = "ready"))
  # 全部停下來：④ 沒有東西可以評分
  none <- list(ok = FALSE, items = list())
  expect_equal(st(TRUE, none, TRUE),
               c(data = "done", items = "ready", link = "ready", score = "locked"))
  expect_match(kitsui_tab_states(TRUE, none)$why[4], "沒有一個量尺校準好")
  expect_equal(st(TRUE, ok, TRUE, list(scores = 1)),
               c(data = "done", items = "done", link = "done", score = "done"))
  # 資料改了（② 的結果清掉）：② 以後的 ✓ 都拿掉
  expect_equal(st(TRUE, NULL, TRUE, list(scores = 1)),
               c(data = "done", items = "ready", link = "locked", score = "locked"))
  expect_true(all(kitsui_tab_states(TRUE, ok)$why == ""))
  # 送到瀏覽器（kitDesign::kit_tab_states()）：做完 ✓，還不能做的有原因
  msg <- kitsui_tab_send(kitsui_tab_states(TRUE))
  expect_length(msg, 4)
  expect_equal(msg[[1]][c("tabset", "value", "state")],
               list(tabset = "tabs", value = "data", state = "done"))
  expect_equal(msg[[2]]$state, "ready")
  expect_null(msg[[2]]$tip)
  expect_equal(msg[[3]]$state, "locked")
  expect_equal(msg[[3]]$tip, "先做「② 題目校準」")
})

test_that("整個 app：分頁的狀態跟著資料、校準、評分變，結果過期時 ✓ 拿掉", {
  shiny::testServer(kit_app(), {
    st <- \() stats::setNames(tab_states()$state, tab_states()$tab)
    session$setInputs(`data-source` = "demo", `data-domains` = "lan", `data-round_label` = "")
    expect_equal(unname(st()[c("data", "items", "link", "score")]),
                 c("ready", "locked", "locked", "locked"))
    session$setInputs(`data-waves` = "M03 3")
    expect_equal(unname(st()[c("data", "items", "link", "score")]),
                 c("done", "ready", "locked", "locked"))
    session$setInputs(`items-run` = 1)
    expect_equal(st()[["items"]], "done")
    expect_true(st()[["link"]] %in% c("ready", "done"))
    expect_equal(st()[["score"]], "ready")
    session$setInputs(`score-run` = 1)
    expect_equal(st()[["score"]], "done")
    # 重新校準：④ 的分數過期，✓ 拿掉
    session$setInputs(`items-run` = 2)
    expect_equal(st()[["items"]], "done")
    expect_equal(st()[["score"]], "ready")
    # 資料改了：② 的結果清掉，③、④ 不能做
    session$setInputs(`data-waves` = c("M03 3", "M03 6"))
    expect_equal(unname(st()[c("data", "items", "link", "score")]),
                 c("done", "ready", "locked", "locked"))
    expect_match(output$`items-status`$html, "【要重新執行】")
  })
})

test_that("步驟分頁：kitDesign 的流程分頁，使用手冊放最右邊", {
  html <- as.character(kit_ui(NULL))
  expect_match(html, 'class="kit-flow-tabs" data-kit-aside="manual"', fixed = TRUE)
  expect_match(html, 'id="tabs"', fixed = TRUE)
})
