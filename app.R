# 入口：Posit Connect Cloud（雲端版）與本機 run.bat 共用
#
# 雲端：Connect Cloud 以本檔為 primary file，回傳 Shiny app 物件（不可呼叫 runApp）。
# 本機：run.bat → scripts/run_app.R → shiny::runApp(本資料夾)。
# 套件：kitsUI::run_kitsui() 不經過本檔，但同樣呼叫 kit_app()，畫面一致。

# Shiny 不自動 source R/：靠 R/_disable_autoload.R（shiny 在執行本檔之前就讀這個開關，
# 在這裡設 options(shiny.autoload.r = FALSE) 來不及，R/ 會被載入兩次）

# Connect Cloud 以非互動模式執行本檔：未設定 KIT_DEPLOY 時視為雲端版
# （run.bat 會先設定 KIT_DEPLOY = "local"）
if (!nzchar(Sys.getenv("KIT_DEPLOY")) && !interactive()) {
  Sys.setenv(KIT_DEPLOY = "cloud")
}

# 載入 R/ 的所有函式到獨立環境（不污染使用者的 global environment）
kit_env <- new.env(parent = globalenv())
# 和套件一樣只讀字母或數字開頭的檔（不讀 R/_disable_autoload.R）
for (f in sort(list.files("R", pattern = "^[A-Za-z0-9].*[.]R$", full.names = TRUE))) {
  source(f, local = kit_env, encoding = "UTF-8")
}

options(shiny.launch.browser = kit_env$kit_open_browser)
kit_env$kit_app()
