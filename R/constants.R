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
# 原文：kits 作答檔
KITJOIN_KITS_TAB <- "kits \u4f5c\u7b54\u6a94"
# 原文：遺漏值處理
KITJOIN_MISSING_TAB <- "\u907a\u6f0f\u503c\u8655\u7406"

# 通知訊息顯示秒數
KIT_NOTIFY_SECONDS <- 6

# 找不到 pandoc 時（圖檔要用 pandoc 存成單一 html）
KIT_PANDOC_MISSING <- paste0(
  # 原文：找不到 pandoc：圖檔（HTML）不能下載。請在 R 執行 kitverse::kitverse_update()（會安裝 pandoc），
  "\u627e\u4e0d\u5230 pandoc\uff1a\u5716\u6a94\uff08HTML\uff09\u4e0d\u80fd\u4e0b\u8f09\u3002\u8acb\u5728 R \u57f7\u884c kitverse::kitverse_update()\uff08\u6703\u5b89\u88dd pandoc\uff09\uff0c",
  # 原文：或安裝 RStudio、Quarto，再重新開啟 R。
  "\u6216\u5b89\u88dd RStudio\u3001Quarto\uff0c\u518d\u91cd\u65b0\u958b\u555f R\u3002"
)

# 這一回合名稱停手多久才檢查（毫秒）；勾世代與波次停手多久（毫秒）
KIT_LABEL_DELAY_MS <- 500
KIT_PICK_DELAY_MS <- 1000
