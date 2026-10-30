# 模組測試不等波次的 1 秒（debounce）
options(kitsui.waves_delay = 0)

# 測試共用的資料：算一次存起來（各測試檔共用）
pipeline_cache <- new.env()

# kitm03 lan 3-48 個月，一波一回合（同 chain_v.R）
lan_chain <- function() {
  if (is.null(pipeline_cache$chain)) {
    fixed <- kitsui_fixed_empty()
    rounds <- list()
    for (w in c(3, 6, 12, 18, 24, 36, 48)) {
      r <- suppressMessages(kitsui_round(kitsui_demo("M03", w, "lan"), fixed, as.character(w)))
      rounds[[as.character(w)]] <- r
      fixed <- r$fixed
    }
    pipeline_cache$chain <- rounds
  }
  pipeline_cache$chain
}

# 第 8 回合：lan M36（36、48 個月）與 motor M03（3 個月），兩個量尺都是自由校準
round8 <- function() {
  if (is.null(pipeline_cache$round8)) {
    df <- dplyr::bind_rows(kitsui_demo("M36", c(36, 48), "lan"), kitsui_demo("M03", 3, "motor"))
    r <- suppressMessages(kitsui_round(df, lan_chain()[["48"]]$fixed, "r8"))
    pipeline_cache$round8 <- list(df = df, round = r)
  }
  pipeline_cache$round8
}

# 第 8 回合之後做好三條連結（lan 內部、D）
linked <- function() {
  if (is.null(pipeline_cache$linked)) {
    fixed <- round8()$round$fixed
    internal <- kitsui_link_internal(fixed, "lan")
    d <- kitsui_link_D(internal$fixed)
    pipeline_cache$linked <- list(fixed = d$fixed, internal = internal$link, D = d$link)
  }
  pipeline_cache$linked
}

# 兩個世代同一回合（M03、M36 的 3、36、48 個月）：內部連結可以做獨立檢核
both_cohorts <- function() {
  if (is.null(pipeline_cache$both)) {
    df <- kitsui_demo(waves = c(3, 36, 48))
    r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "1"))
    pipeline_cache$both <- list(df = df, round = r)
  }
  pipeline_cache$both
}

# 兩個世代、兩個領域同一回合（3、6、12、36、48 個月），做好內部連結與 D（截圖的情境）
two_cohorts_linked <- function() {
  if (is.null(pipeline_cache$two)) {
    df <- kitsui_demo(waves = c(3, 6, 12, 36, 48))
    r <- suppressMessages(kitsui_round(df, kitsui_fixed_empty(), "1"))
    fixed <- kitsui_link_internal(r$fixed, "lan")$fixed
    fixed <- kitsui_link_internal(fixed, "motor")$fixed
    fixed <- kitsui_link_D(fixed)$fixed
    pipeline_cache$two <- list(df = df, round = r, fixed = fixed)
  }
  pipeline_cache$two
}

# 模擬資料（矩陣）轉成長格式
sim_long <- function(X, wave_m = 1) {
  tibble::tibble(
    visit_id = rep(rownames(X), ncol(X)), item_id = rep(colnames(X), each = nrow(X)),
    item_score = as.integer(X), domain = "sim", cohort = "M03", wave_m = wave_m
  )
}

# 借題庫：第 1 回合 motor M03 3-48 個月、motor M36 36、48、84、96 個月（都是自由校準），
# 做好 motor 的內部連結與外部連結；第 2 回合 M03 的 60 個月（自己的題庫只涵蓋 3-48）
borrow_chain <- function() {
  if (is.null(pipeline_cache$borrow)) {
    df1 <- dplyr::bind_rows(kitsui_demo("M03", c(3, 6, 12, 18, 24, 36, 48), "motor"),
                            kitsui_demo("M36", c(36, 48, 84, 96), "motor"))
    r1 <- suppressMessages(kitsui_round(df1, kitsui_fixed_empty(), "1"))
    fixed <- kitsui_link_internal(r1$fixed, "motor")$fixed
    fixed <- kitsui_link_D(fixed)$fixed
    df2 <- kitsui_demo("M03", 60, "motor")
    r2 <- suppressMessages(kitsui_round(df2, fixed, "2"))
    pipeline_cache$borrow <- list(r1 = r1, fixed = fixed, df2 = df2, r2 = r2)
  }
  pipeline_cache$borrow
}

# 模擬資料：12 題 a1-a12 的題庫（第 1 回合自由校準）
sim_bank <- function() {
  if (is.null(pipeline_cache$sim_bank)) {
    betas <- purrr::set_names(as.list(seq(-2, 2, length.out = 12)), paste0("a", 1:12))
    X1 <- kits::kits_sim(seq(-3, 3, length.out = 1000), betas, seed = 1)
    r1 <- suppressMessages(kitsui_round(sim_long(X1), kitsui_fixed_empty(), "1"))
    pipeline_cache$sim_bank <- list(betas = betas, r1 = r1)
  }
  pipeline_cache$sim_bank
}
