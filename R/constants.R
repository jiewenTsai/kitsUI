# 全 app 共用的常數

# 預覽表最多顯示幾欄、幾列
KIT_PREVIEW_COLS <- 20L
KIT_PREVIEW_ROWS <- 10L

# 單檔上傳上限（MB）
KIT_UPLOAD_MB <- 200

# kitJoin 雲端版：寬格式或欄位不合時，到它的「kits 作答檔」轉好再上傳（本機版不連，見
# ui_kitjoin_button()）
KITJOIN_URL <- "https://jiewentsai-kitjoin.share.connect.posit.cloud/"
# kitJoin 的分頁名稱（kitJoin 的分頁沒有編號）：kitsUI 提到它們時用一樣的名字
KITJOIN_KITS_TAB <- "kits 作答檔"
KITJOIN_MISSING_TAB <- "遺漏值處理"

# 通知訊息顯示秒數
KIT_NOTIFY_SECONDS <- 6

# 找不到 pandoc 時（圖檔要用 pandoc 存成單一 html）
KIT_PANDOC_MISSING <- "找不到 pandoc：圖檔（HTML）不能下載。請安裝 RStudio 或 Quarto。"

# 這一回合名稱停手多久才檢查（毫秒）；勾世代與波次停手多久（毫秒）
KIT_LABEL_DELAY_MS <- 500
KIT_PICK_DELAY_MS <- 1000
