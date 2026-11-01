# kitsUI 0.8.3

* Uses kitDesign 0.7.0 (the same version as kitJoin, kitverse and kitItemBank). Nothing changes on screen.

# kitsUI 0.8.2

## Fixes

* ④ 「下一步：用這一回合的固定檔開始下一回合」: ① could stay as it was (still showing 「沒有（第一回合）」,
  and checking the new wave did nothing). The ① 作答設計 plot was drawn while its section was closed, at
  76 px wide; plotly failed with "axis scaling" and that stopped the other ① outputs from updating. The plot
  could also open squashed. Closed sections are now really hidden, so plots inside are drawn when opened
  (kitDesign 0.6.1).
* The message when pandoc is missing now says to run `kitverse::kitverse_update()` (it installs pandoc),
  or install RStudio or Quarto.
* The local hint for kitJoin mentions `run_kitjoin()` after `library(kitverse)` and kitJoin's title,
  KIT 資料處理工具.
* ② no longer logs a `map2_lgl` warning when the next round has fewer scales than the last.

# kitsUI 0.8.1

## Changes you can see

* The 使用手冊 (manual) is up to date with this version: every screenshot retaken with the demo data,
  and the text covers what changed since 0.3.11 — how a page reads (status boxes, one main button,
  details), the reference cohort and wave unit in round 1, the round name under 進階設定, pass steps
  in ④, the full raw-score table, the plot toolbar (PNG 300 dpi, SVG), and the local version through
  kitverse (`library(kitverse)`, `run_kitsui()`, `kitverse_check()`). New FAQ entries.

# kitsUI 0.8.0

## Changes you can see

* Plots for papers: the toolbar above every plot saves a PNG that is always 2100 pixels wide with
  300 dpi written in the file (7 inches, the width of a two-column paper; Word inserts it at that
  size), whatever the window size. A new button saves an SVG (vector) for journals that want 600 dpi
  or more, EPS or PDF; its text uses the fonts on your computer. Both also work in a downloaded
  plot HTML file. (kitDesign 0.6.0)

# kitsUI 0.7.2

## Fixes

* Firefox with some add-ons (for example Google PDF Viewer and other "document viewer" add-ons): clicking
  a download button left the page and the download was "Not Found". The add-on takes over download
  links on the page. Download buttons now come from kitDesign 0.5.2, where they are buttons, not links,
  so there is nothing for an add-on to take over. The buttons look the same.

# kitsUI 0.7.1

## Fixes

* ② 偏移把關圖: for a scale with no bank (round 1, free calibration) the plot area explains why there is
  no drift plot, but the download button was still there; clicking it failed with two red
  notifications saying the plot had no content. Plot download buttons now appear only when their plot
  can be drawn, and a failed plot download shows one notification with the real reason.
* The cloud version and run.bat sourced `R/` twice at start-up (shiny loaded it before `app.R` could
  turn that off) and warned that the folder looks like an R package. `R/_disable_autoload.R` now turns
  shiny's autoload off; start-up is a little faster and the warning is gone.

# kitsUI 0.7.0

## Changes you can see

* ④ 分數對照表 now shows every possible raw score by default, with a note that it is the table of all
  possible raw scores (a raw score nobody in this round's data got has n = 0). The option 「只顯示本次
  資料中實際出現的原始分數」 ("only the raw scores that occur in this round's data", off by default)
  replaces 「顯示沒有人的原始分數」. The download is the full table either way, as before.

# kitsUI 0.6.0

## Changes you can see

* The version line in the header starts with the package name: "kitsUI v0.6.0 · 2026-11-01 ·
  kits 0.25.2" (kitDesign 0.5.0).
* ③ 各世代的垂直連結: when a domain has only one cohort and no earlier-round points (for
  example round 1 with only M03), the unlinked chart (each wave's median with P25–P75) is now
  drawn, under a note saying that the linked charts need another cohort or earlier-round
  points. Before, the section showed only the note. The 內部連結後 and 外部連結後 tabs are
  left out in this case because they cannot be drawn in this round. The same applies to an old
  fixed file without wave summaries and to a fixed file with no earlier points for the domain.
  The domain menu now lists every calibrated domain.
* After calibration and scoring, a short notice appears and the page scrolls to the status box,
  following the design language's status flow (like kitJoin).

## Under the hood

* The download button and the cloud footer come from kitDesign 0.4.0
  (`kit_download_button()`, `kit_cloud_footer()`); `R/download.R` is gone. `.onLoad()` sets
  `options(kitDesign.app = "kitsUI")`, so file names, headers and log lines are unchanged and
  every download is byte-identical.
* The file helpers come from kitDesign; five helpers that were never called are gone. All 36
  flow outputs are identical.
* The version line and the fixed file's VERSION use `kit_app_description("kitsUI")`; the
  manual's header uses `kit_header(logo = "inline")`.
* Uses kitDesign 0.5.0 (`Imports: kitDesign (>= 0.5.0)`, `Remotes: jiewenTsai/kitDesign@v0.5.0`);
  `manifest.json` regenerated.
* NEWS.md replaces the README's 「0.5.0 的新功能」 section; the history is filled in from 0.3.0.

# kitsUI 0.5.0

## Changes you can see

* The interface comes from **kitDesign**: the Bootswatch Brite theme with the KIT colors, the
  full KIT logo in the header and the bean as the tab icon, step tabs, status boxes, the page
  guide, the "用 R 怎麼做" code panel (code lines wrap at 70 columns), callouts and buttons.
* Charts use kits' English labels and `kit_plotly_style()`; categorical colors are the
  Okabe–Ito palette.
* Every page puts the result first: ① data, ② items, ③ link and ④ scores each have one status
  box, the main button, and details collapsed below. ② explains why a scale stopped; ③ keeps
  all built links visible and marks the tab ✓ only after ②; ④ shows the summary first, and
  0 rows is 【停下來】.
* ① A typed round label is no longer reset to R1. ② Changing settings after a run marks the
  result stale. A failed download shows an error notification.
* The local version (`run.bat`) installs or upgrades packages that are too old.

## Under the hood

* `Imports: kitDesign (>= 0.2.0)`; the app's own copies of the theme, CSS, header, logo and
  UI helpers are gone. NAMESPACE exports only `kit_app()` and `run_kitsui()`.

# kitsUI 0.4.0

## Changes you can see

* The reference cohort is chosen in round 1 (no longer always M03). Without anchor items the
  external link is marked as not applicable.
* The wave unit (months, years or wave number) is chosen in round 1.
* The drift table shows the judging line, the judged steps and the dropped steps.
* A scale that stops no longer blocks the round; the flag threshold depends on the scale size.
* Common kits errors are shown in plain Chinese, with the original text in a details block.
* ① The data page catches problems that used to stop every scale.

## Crashes and broken files

* A CSV that is neither UTF-8 nor CP950 (for example Latin-1, GBK, Shift-JIS) is refused with
  a clear message instead of crashing R.
* Cohort names with spaces and domain names with commas work end to end.
* The fixed file has checksums; only the zip root is read.

# kitsUI 0.3.12

* Drift plot: title, axes, legend and hover text in English.
* Vertical plot: the x axis says which age it uses (median actual age of each wave, or nominal
  age); a plot never mixes the two. Waves calibrated on another cohort's bank are their own
  series, for example "M03 (motor M36 bank)".

# kitsUI 0.3.11

* ③ Vertical plots put every point, this round's and earlier rounds', at the actual median age.
  `wave_summary.csv` gains `age_med`; fixed files made before 0.3.11 fall back to the nominal
  wave and the caption names those rounds.

# kitsUI 0.3.10

* ③ Vertical plot: x-axis ticks at every plotted wave (kits' default stopped at 96 months).
* ③ 未連結: waves calibrated on another cohort's bank are their own series, 「M03（motor M36 題庫）」.

# kitsUI 0.3.9

* ④ 通過步驟 is a collapsible block: pick items, enter a step and press 「套用到選的題」;
  「改回 1」 per item and 「全部改回 1」. The CSV upload moved into 「用檔案設定（題目很多時）」.
* The item table note, manual and README say when age50 is empty.

# kitsUI 0.3.8

* ③ Vertical plot also for rounds with one cohort: each round stores per-wave summaries
  (`wave_summary.csv`) in the fixed file, and later rounds draw them next to this round's points.
* ④ Pass steps per item (`pass_steps.csv`), kept in the fixed file and reused next round.
* ② 「固定了哪些題」: status per item and a check that fixed steps equal the bank.

# kitsUI 0.3.7

* ③ 「內部連結的獨立檢核」 is collapsed by default.

# kitsUI 0.3.6

* ① When an uploaded file fails the checks, the red box points to kitJoin's
  「⑥ 轉成 kits 作答檔」 with an 「開啟 kitJoin」 button.

# kitsUI 0.3.5

* The 「使用手冊」 tab shows the manual in the app's own style, with a sticky table of contents.

# kitsUI 0.3.4

* New last tab 「使用手冊」 with the operation manual.

# kitsUI 0.3.3

* The item table shows the first-step threshold on three scales (`ability_threshold`,
  `ability_linked_threshold`, `d_threshold`).

# kitsUI 0.3.2

* Clearer score columns: `ability`/`ability_se` and `d_score`/`d_score_se`; the raw-score table
  adds `ability_int`.

# kitsUI 0.3.0

* KIT 量尺建構工具: a Shiny app over kits with four pages (data, item calibration, links,
  scores), wave-by-wave updates with a fixed file, and borrowing another cohort's bank by wave age.
