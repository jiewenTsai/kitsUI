# shiny 的開關：R/ 裡有這個檔時，shiny::runApp() 不自動 source R/（見 ?shiny::loadSupport）。
# 根目錄 app.R 自己把 R/ 載入 kit_env；沒有這個檔時 R/ 會先被 shiny 載入一次（啟動慢一倍），
# 還跳出「這個資料夾看起來是 R 套件」的警告。shiny 在執行 app.R 之前就讀這個開關，
# 所以只在 app.R 裡設 options(shiny.autoload.r = FALSE) 來不及。
# 檔名以底線開頭：套件安裝與 devtools::load_all() 都不讀它；套件建置時也排除（.Rbuildignore）。
