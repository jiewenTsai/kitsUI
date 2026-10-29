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
