# 測試時暫時換掉 app 裡的一個函式（原始碼 source() 或安裝的套件都可以）；測試結束換回來
mock_app_fn <- function(name, fn, env = environment(kit_app), frame = parent.frame()) {
  old <- get(name, envir = env)
  locked <- bindingIsLocked(name, env)
  if (locked) unlockBinding(name, env)
  assign(name, fn, envir = env)
  withr::defer({
    assign(name, old, envir = env)
    if (locked) lockBinding(name, env)
  }, envir = frame)
  invisible(old)
}

# 「用 R 怎麼做」畫出來的程式碼（kitDesign::kit_render_code_panel() 的 HTML）換回純文字；
# 沒有程式碼時是那一行說明
code_of <- function(x) {
  html <- if (is.list(x)) x$html else x
  html <- as.character(html)
  if (!grepl("<pre><code>", html, fixed = TRUE)) return(gsub("<[^>]+>", "", html))
  code <- sub("(?s).*<pre><code>(.*?)</code></pre>.*", "\\1", html, perl = TRUE)
  ents <- c(lt = "<", gt = ">", quot = "\"", "#39" = "'", amp = "&")
  for (k in names(ents)) code <- gsub(paste0("&", k, ";"), ents[[k]], code, fixed = TRUE)
  code
}
