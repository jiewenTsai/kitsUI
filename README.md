# kitsUI：KIT 量尺建構工具

kitsUI 是 [kits](https://github.com/jiewenTsai/kits) 的 Shiny 介面。
計算全部交給 kits，kitsUI 只負責畫面、檢查和下載。

四個頁面依序做：

1. ① 資料：上傳長格式資料（可以一次好幾個檔，也收 .csv.gz），勾這一回合的世代與波次。
2. ② 題目校準：沒有題庫時自由校準；有固定檔時，舊題固定在題庫的值，只估新題，並檢查偏移。
3. ③ 連結：內部連結（M36 → M03）與外部連結（M03 → D）；每條連結只做一次，做好就凍結。
4. ④ 受試者評分：這一回合的分數、分數對照表、題目表，並下載新的固定檔（zip）。

下一回合上傳新的波次和上一回合的固定檔，從 ① 再做一次。
最後一頁「使用手冊」有完整的操作步驟與截圖（原稿在 `tools/manual/`）。
舊的參數不動，早先的分數也不重算。

## 安裝與執行

```r
install.packages("remotes")
remotes::install_github("jiewenTsai/kits")
remotes::install_github("jiewenTsai/kitsUI")
kitsUI::run_kitsui()
```

Windows 也可以雙擊 `run.bat`（第一次會自動安裝需要的套件）。
下載圖檔（HTML）需要 pandoc；裝了 RStudio 或 Quarto 就有。

## 資料格式

長格式，一列是一個列次（一個孩子的一個波次）的一題作答。

手上的資料是寬格式（一題一欄）、欄名不一樣，或分數不是從 0 開始時，
先用 [kitJoin](https://jiewentsai-kitjoin.share.connect.posit.cloud/) 的「⑥ 轉成 kits 作答檔」轉好再上傳。
① 資料頁的上傳說明和紅字裡也有「開啟 kitJoin」按鈕。

| 欄位 | 必填 | 意思 |
| --- | --- | --- |
| visit_id | 是 | 列次編號，每個孩子的每個波次不同 |
| subj_id | 否 | 孩子編號 |
| cohort | 是 | 世代，例如 M03、M36 |
| sex | 否 | 性別 |
| wave_m | 是 | 波次（施測月齡） |
| age_m | 否 | 實際月齡 |
| item_id | 是 | 題號 |
| item_score | 是 | 得分，從 0 開始的整數 |
| domain | 是 | 領域，例如 lan、motor、cog |

① 頁可以下載欄位範例檔。
外部連結用 `kits::kits_anchors()` 的錨題，領域名稱要和它一致（lan、motor、cog）。

## 雲端部署

`app.R` 是 Posit Connect Cloud 的主檔，`manifest.json` 記錄套件版本。
新增或移除 `R/` 的檔案、或更新套件後，請重新產生 `manifest.json`：

```r
rsconnect::writeManifest(appPrimaryDoc = "app.R")
```

雲端版只能上傳已去識別化的資料。

## 授權

GPL-3。
