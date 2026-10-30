# ② 固定了哪些題：每一題的狀態、核對固定的步驟、偏移把關圖、程式碼

# lan 鏈的第 w 回合（lan M03）：狀態與核對；題庫是上一回合的固定檔
chain_status <- function(w) {
  ch <- lan_chain()
  i <- match(w, names(ch))
  r <- ch[[i]]
  bank <- if (i == 1) kitsui_fixed_empty()$bank else ch[[i - 1]]$fixed$bank
  bank <- dplyr::filter(bank, scale == "lan M03")
  item <- r$items[["lan M03"]]
  tab <- kitsui_item_status(item, r$drift[["lan M03"]],
                            dplyr::filter(r$log, scale == "lan M03"), bank)
  check <- if (nrow(bank) > 0) kitsui_fixed_check(item, tab, bank)
  list(round = r, item = item, bank = bank, tab = tab, check = check)
}

# KITSUI_STATUS 的順序
sort_by_status <- function(kind) kind[order(match(kind, names(KITSUI_STATUS)))]

test_that("偏移的題換成新版本；沒有判斷的題固定，原因來自紀錄與偏移表", {
  x <- chain_status("24")
  tab <- x$tab
  # 每一題一列：這一回合的題（新版本算原來的題）
  co_ids <- unique(coef(x$item)$item_id)
  expect_equal(nrow(tab), length(co_ids))
  expect_false(anyDuplicated(tab$key) > 0)
  moved <- tab[tab$kind == "moved", ]
  expect_equal(moved$key, c("lanc03", "lanc08"))
  expect_equal(moved$item_id, c("lanc03 → lanc03_r24", "lanc08 → lanc08_r24"))
  expect_equal(unique(moved$status), "偏移 → 新版本")
  expect_match(moved$detail, "離判斷用的線 [0-9.]+ logit，超過界線")
  status_of <- \(id) tab$status[tab$key == id]
  # 分數類別有問題（紀錄）、步驟上下的人不夠、分數比題庫高（偏移表的 reason）
  expect_equal(status_of("lana01"), "固定（不判斷偏移，沿用題庫）")
  expect_match(tab$detail[tab$key == "lana01"], "沒有人得 0 分")
  expect_match(tab$detail[tab$key == "lana03"], "步驟上下的人不夠")
  expect_match(tab$detail[tab$key == "lanc09"], "這一回合估的步驟：")
  expect_equal(status_of("lana02_r12"), "固定（不判斷偏移，沿用題庫）")
  # 判斷過、沒有偏移
  judged <- x$round$drift[["lan M03"]]$items
  ok <- judged$item_id[judged$drift %in% FALSE]
  expect_true(all(tab$status[match(ok, tab$key)] == "固定（沿用題庫）"))
  expect_match(tab$detail[match(ok, tab$key)], "已判斷，沒有偏移")
  # 問題在前：偏移、拿掉、新題，再是固定的
  expect_equal(tab$kind, sort_by_status(tab$kind))
  expect_equal(tab$steps[tab$key == "lanc03"], 3L)
})

test_that("新題：不在題庫的題這一回合估計", {
  tab <- chain_status("36")$tab
  new <- tab[tab$kind == "new", ]
  expect_true(all(startsWith(new$key, "lanb")))
  expect_equal(unique(new$status), "新題（這一回合估計）")
  expect_equal(tab$kind[seq_len(nrow(new))], rep("new", nrow(new)))
})

test_that("每一回合固定的步驟都和題庫完全相同（最大差 0）", {
  for (w in c("6", "12", "18", "24", "36", "48")) {
    x <- chain_status(w)
    expect_equal(x$check$max_diff, 0, label = paste("round", w))
    expect_equal(nrow(x$check$bad), 0)
    expect_equal(x$check$n_items, sum(x$tab$kind %in% c("kept", "fixed")))
    expect_gt(x$check$n_steps, x$check$n_items - 1)
    txt <- kitsui_status_text(x$tab, x$check)
    expect_true(txt$ok)
    expect_match(txt$verify, "參數和題庫完全相同（最大差 0）。", fixed = TRUE)
    expect_match(txt$verify, sprintf("固定的 %d 題、%d 個步驟", x$check$n_items,
                                     x$check$n_steps))
  }
  txt <- kitsui_status_text(chain_status("24")$tab, chain_status("24")$check)
  expect_equal(txt$main,
               "這一回合固定 13 題（沿用題庫的參數），換成新版本 2 題，新題 0 題，拿掉 0 題。")
})

test_that("題庫的值被改過：核對抓得到，紅框列出步驟", {
  x <- chain_status("24")
  bank <- x$bank
  hit <- which(bank$item_id == "lana01" & bank$item_score == 2)
  bank$beta[hit] <- bank$beta[hit] + 0.5
  check <- kitsui_fixed_check(x$item, x$tab, bank)
  expect_equal(nrow(check$bad), 1)
  expect_equal(check$bad$item_id, "lana01")
  expect_equal(check$max_diff, 0.5, tolerance = 1e-12)
  txt <- kitsui_status_text(x$tab, check)
  expect_false(txt$ok)
  expect_match(txt$verify, "有 1 個步驟和題庫不同")
  html <- as.character(items_fixed_ui(x$tab, check))
  expect_match(html, "alert-danger")
  expect_match(html, "lana01 步驟 2")
  expect_match(html, "差 5.0e-01")
  # 差很小（1e-8 以內）不算不同
  bank$beta[hit] <- x$bank$beta[hit] + 1e-10
  expect_equal(nrow(kitsui_fixed_check(x$item, x$tab, bank)$bad), 0)
})

test_that("自由校準：沒有固定的題；拿掉的題也列出來", {
  r <- round8()$round
  for (s in c("lan M36", "motor M03")) {
    log <- dplyr::filter(r$log, scale == s)
    bank <- dplyr::filter(lan_chain()[["48"]]$fixed$bank, scale == s)
    expect_equal(nrow(bank), 0)
    tab <- kitsui_item_status(r$items[[s]], r$drift[[s]], log, bank)
    gone <- log$item_id[log$check == "category problem, left out"]
    expect_setequal(tab$key[tab$kind == "out"], gone)
    expect_equal(sum(tab$kind == "free"), length(unique(coef(r$items[[s]])$item_id)))
    expect_true(all(tab$kind %in% c("free", "out")))
    txt <- kitsui_status_text(tab, NULL)
    expect_match(txt$main, "沒有題庫，這個量尺是自由校準，沒有固定的題")
    expect_null(txt$verify)
    html <- as.character(items_fixed_ui(tab, NULL))
    expect_match(html, "看全部自由校準的題")
  }
  # 停下來的量尺：沒有 item
  expect_null(kitsui_item_status(NULL, NULL, r$log[0, ], kitsui_fixed_empty()$bank))
  expect_match(as.character(items_fixed_ui(NULL, NULL)), "停下來")
})

test_that("最大差的寫法", {
  expect_equal(kitsui_diff_text(0), "0")
  expect_equal(kitsui_diff_text(3.6e-15), "3.6e-15")
})

test_that("偏移把關圖：點照狀態上色，帶子外的步驟就是 kits 拿掉的步驟", {
  x <- chain_status("24")
  drift <- x$round$drift[["lan M03"]]
  p <- kitsui_drift_plot(drift, x$tab)
  expect_s3_class(p, "plotly")
  names <- purrr::map_chr(p$x$data, \(tr) as.character(tr$name)[1])
  expect_match(names[1], "Grey band")
  # 帶子的中線（判斷用的虛線），然後是 kits 拿掉偏移步驟之後的線
  expect_match(names[2], "Judging line")
  expect_identical(p$x$data[[2]]$line$dash, "dash")
  expect_match(names[3], "Final line")
  expect_true(all(c("Fixed (bank value)", "Drifted -> new version",
                    "Step not judged (too few people above/below)") %in%
                    names))
  # 沒有判斷的步驟：hover 寫題的狀態（例如 lana03 步驟上下的人不夠，固定在題庫的值）
  open <- p$x$data[[match("Step not judged (too few people above/below)", names)]]
  expect_true(any(grepl("^lana03 step 1<br>Fixed \\(not judged, bank value\\)", open$hovertext)))
  expect_true(all(grepl("Step not judged", open$hovertext)))
  moved <- p$x$data[[match("Drifted -> new version", names)]]
  expect_true(all(grepl("^lanc0[38] step", moved$hovertext)))
  expect_match(moved$hovertext[1], "Bank threshold \\(fixed value\\)")
  expect_match(moved$hovertext[1], "Free trial threshold")
  # 判斷的步驟寫離判斷用的線多遠與界線；沒有判斷的步驟不寫
  expect_true(any(grepl("Distance from judging line: [0-9.]+ \\(limit [0-9.]+\\)", moved$hovertext)))
  expect_false(any(grepl("Distance from judging line", open$hovertext)))
  # 軸與標題說清楚：點是固定之前的試算
  lay <- plotly::plotly_build(p)$x$layout
  expect_match(lay$title$text, "Drift check before fixing")
  expect_match(lay$xaxis$title$text, "free trial this round \\(before fixing")
  expect_match(lay$yaxis$title$text, "Bank threshold \\(fixed value\\)")
  # 圖上的字都是英文（題號等使用者的資料除外）
  txt <- c(unlist(lay[c("title", "xaxis", "yaxis")]), names,
           unlist(purrr::map(p$x$data, "hovertext")))
  expect_false(any(grepl("[\u3000-\u9fff]", txt)))
  # 帶子：判斷的步驟在帶子外 <=> 被拿掉
  pts <- as.data.frame(drift$points)
  band <- kitsui_drift_band(drift, pts)
  use <- pts[pts$judged, ]
  slope <- diff(band$y[1:2]) / diff(band$x[1:2])
  mid <- band$y[1] - slope * band$x[1]
  half <- (band$y[1] - band$y[4]) / 2
  centre <- mid - half
  outside <- abs(use$thr_ref - (centre + slope * use$thr_new)) > half
  expect_equal(outside, use$dropped)
  expect_true(any(use$dropped))
})

test_that("程式碼：有 kits_drift() 時加上 plot(drift)，自由校準時不變", {
  fixed <- lan_chain()[["3"]]$fixed
  code <- kitsui_code_round("lan M03", "6", fixed = fixed)
  out <- kitsui_code_drift_plot(code)
  lines <- strsplit(out, "\n")[[1]]
  at <- grep("^drift <- kits_drift", lines)
  expect_match(lines[at + 1], "^plot\\(drift\\)")
  free <- kitsui_code_round("lan M03", "3")
  expect_identical(kitsui_code_drift_plot(free), free)
})
