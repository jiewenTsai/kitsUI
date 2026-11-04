# Report snapshot of kitsUI: one demo scenario (two cohorts, lan + motor, 3/6/12/36/48 months,
# one round, both internal links and the D link, then scores). Every table that goes into the
# fixed set and the score table is written and its MD5 compared with results/snapshot_md5.txt.
# Run: source(system.file("validation", "snapshot.R", package = "kitsUI"))
out <- Sys.getenv("KIT_VALIDATION_OUT", file.path(tempdir(), "kit_validation"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ns <- asNamespace("kitsUI")
df <- ns$kitsui_demo(waves = c(3, 6, 12, 36, 48))
r <- suppressMessages(ns$kitsui_round(df, ns$kitsui_fixed_empty(), "1"))
fixed <- ns$kitsui_link_internal(r$fixed, "lan")$fixed
fixed <- ns$kitsui_link_internal(fixed, "motor")$fixed
fixed <- ns$kitsui_link_D(fixed)$fixed
sc <- suppressMessages(ns$kitsui_score(r, df, fixed))
zip <- file.path(out, "fixed.zip")
ns$kitsui_write_fixed(fixed, zip)
utils::unzip(zip, exdir = file.path(out, "fixed"))
utils::write.csv(sc$scores, file.path(out, "scores.csv"), row.names = FALSE)
files <- c(file.path(out, "fixed", c("bank.csv", "links.csv", "versions.csv", "checks.csv", "waves.csv",
                                     "wave_summary.csv", "pass_steps.csv")), file.path(out, "scores.csv"))
now <- data.frame(file = basename(files), md5 = unname(tools::md5sum(files)))
ref <- utils::read.table(system.file("validation", "results", "snapshot_md5.txt", package = "kitsUI"),
                         header = TRUE, stringsAsFactors = FALSE)
cmp <- merge(ref, now, by = "file", suffixes = c("_ref", "_now"))
cmp$same <- cmp$md5_ref == cmp$md5_now
print(cmp[c("file", "same")], row.names = FALSE)
cat(if (all(cmp$same)) "Snapshot unchanged.\n" else "Snapshot CHANGED: say why in NEWS.\n")
