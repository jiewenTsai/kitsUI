# 使用手冊的原稿

app 最後一頁「使用手冊」顯示的是 `inst/manual/kitsui_manual.html`。
這個資料夾是它的原稿，不會放進套件，也不會部署到雲端。

- `kitsui_manual_src.html`：手冊的文字與版面；`{{名稱}}` 是截圖的位置。
- `img/`：截圖（WebP）。全部用 app 內建的示範資料（`kits::kitm03`、`kits::kitm36`），不放真實資料。
- `build_manual.R`：把截圖內嵌進原稿，寫出 `inst/manual/kitsui_manual.html`。

更新手冊：

1. 改 `kitsui_manual_src.html`，或換掉 `img/` 裡的截圖（檔名不變）。
2. 在 kitsUI 資料夾執行 `Rscript tools/manual/build_manual.R`。
3. 重新產生 `manifest.json`（雲端版）。
