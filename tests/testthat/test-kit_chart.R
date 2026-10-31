# 圖：kits 的 plot() → kit_chart_categorical()（KIT 的類別色）→ kitDesign::kit_plotly_style()

test_that("類別色：同一組同一個顏色，依序取 Okabe–Ito；跳過的組不換；偏移把關圖的顏色", {
  p <- plotly::plot_ly() |>
    plotly::add_trace(x = 1:2, y = 1:2, type = "scatter", mode = "lines", name = "model",
                      line = list(color = "#7F7F7F")) |>
    plotly::add_trace(x = 1:2, y = 2:3, type = "scatter", mode = "lines+markers", name = "M03") |>
    plotly::add_trace(x = 1:2, y = 3:4, type = "scatter", mode = "markers", name = "M36")
  d <- kit_chart_categorical(p, skip = "model")$x$data
  expect_equal(d[[1]]$line$color, "#7F7F7F")
  expect_equal(d[[2]]$line$color, "#0072B2")
  expect_equal(d[[2]]$marker$color, "#0072B2")
  expect_equal(d[[3]]$marker$color, "#E69F00")
  expect_equal(KITSUI_DRIFT_LOOKS$moved$col, kit_chart_role("moved"))
  expect_equal(kit_chart_role("moved"), "#D55E00")
  expect_equal(kit_chart_role("unjudged"), kitDesign::kit_chart_neutral())
  expect_true(KITSUI_DRIFT_LOOKS$fixed$col %in% kitDesign::kit_chart_colors())
})

test_that("資料裡的中文名字（題號、領域、世代）在套好樣子的圖上照原樣；圖的字照 kits 的英文", {
  df <- kitsui_demo("M03", c(3, 6), "lan")
  df$domain <- "語言"
  df$cohort <- "三月"
  df$item_id <- paste0("語", df$item_id)
  r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty("三月"), "1"))
  expect_named(r$items, "語言 三月")
  ids <- utils::head(unique(df$item_id), 2)
  p <- kitDesign::kit_plotly_style(kit_chart_categorical(plot(r$items[[1]], type = "category",
                                                             items = ids)))
  panels <- unlist(lapply(p$x$layout$annotations, `[[`, "text"))
  expect_setequal(panels, ids)
  expect_equal(p$x$layout$title$text, "<b>Category probability curves of each item</b>")
  expect_true("Score 0" %in% purrr::map_chr(p$x$data, \(tr) as.character(tr$name)[1]))
  # 自己畫的 plotly 圖：標題、軸、圖例、x 的值都是使用者的中文，只多了粗體的標題
  q <- plotly::plot_ly(x = c("語lana01", "語lana02"), y = 1:2, type = "scatter", mode = "markers",
                       name = "三月") |>
    plotly::add_trace(x = c("語lana01", "語lana02"), y = 2:3, name = "三十六月") |>
    plotly::layout(title = "theta (語言 三月)", xaxis = list(title = "語言"),
                   yaxis = list(title = "theta (語言 三月)"))
  s <- kitDesign::kit_plotly_style(kit_chart_categorical(q))
  expect_equal(s$x$layout$title$text, "<b>theta (語言 三月)</b>")
  expect_equal(s$x$layout$xaxis$title$text, "語言")
  expect_equal(s$x$layout$yaxis$title$text, "theta (語言 三月)")
  expect_equal(purrr::map_chr(s$x$data, \(tr) tr$name), c("三月", "三十六月"))
  expect_equal(as.character(s$x$data[[1]]$x), c("語lana01", "語lana02"))
})

test_that("下載的圖（HTML）是套好樣子的圖：字型、工具列", {
  skip_if_not(kit_pandoc_ok())
  p <- kitDesign::kit_plotly_style(plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter",
                                                   mode = "markers"))
  file <- withr::local_tempfile(fileext = ".html")
  plot_save(p, file)
  html <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(html, "Chiron GoRound TC", fixed = TRUE)
  expect_match(html, "displaylogo", fixed = TRUE)
})
