# 分數類別的測試共用的模擬資料（test-pipeline_categories.R、test-mod_items.R）
# pipeline_cache 在 helper-pipeline.R

# 模擬資料：10 題 0/1 分（a1-a10）與 2 題 0-3 分（p1、p2）的題庫（第 1 輪自由校準）
poly_bank <- function() {
  if (is.null(pipeline_cache$poly_bank)) {
    betas <- c(purrr::set_names(as.list(seq(-2, 2, length.out = 10)), paste0("a", 1:10)),
               list(p1 = c(-1, 0, 1), p2 = c(-0.5, 0.3, 1.2)))
    X1 <- kits::kits_sim(seq(-3, 3, length.out = 1000), betas, seed = 11)
    r1 <- suppressMessages(kitsui_round(sim_long(X1), kitsui_fixed_empty(), "1"))
    pipeline_cache$poly_bank <- list(betas = betas, r1 = r1)
  }
  pipeline_cache$poly_bank
}

# 第 2 輪的資料：同樣的題，加上兩題新的題 n1、n2（題庫沒有）
poly_round2 <- function(seed = 12) {
  pb <- poly_bank()
  betas <- c(pb$betas, list(n1 = c(-1, 0, 1), n2 = c(-0.5, 0.5, 1)))
  kits::kits_sim(seq(-3, 3, length.out = 1000), betas, prefix = "r", seed = seed)
}
