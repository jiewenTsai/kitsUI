# kitsUI validation report

Standard: [KIT validation standard](https://github.com/jiewenTsai/kitDesign/blob/main/inst/validation/criteria.md)
(`kitDesign::kit_validation("kitDesign", "criteria")`). Version validated: kitsUI 0.8.7, with the
regression tests and the estimate warnings of kitsUI 0.8.8. Date: 2026-11-04. The numbers kitsUI writes come from kits; see
the [kits report](https://github.com/jiewenTsai/kits/blob/main/inst/validation/validation.md) for
the engine comparisons.

## Summary

The report kitsUI writes for a fixed demo scenario is byte-identical across two runs, and 13 of 13
metamorphic relations hold, including the propagation of linking error into `se_total`. The
existing tests caught 6 of 9 planted faults; the 3 survivors are all in the drift flag rule, exactly
at its thresholds, and are now covered by regression tests. This validation was run after release:
no acceptance criteria were written before generation, and 8 faults were found after 2026-11-01,
all fixed. The last one was found by this validation: a round reported ok although kits flagged its
estimates as not identified.

## Acceptance criteria (written before generation)

None were written before the code; this report is retrospective. The criteria that the tests now
imply:

1. The fixed set, the score table and the downloaded R code give the same numbers as the app.
2. A round with the same data in another row order, or with renamed visit IDs, writes the same fixed set.
3. An item with a score category problem is left out (not in the bank) or fixed at the bank values
   (in the bank), in the app and in the downloaded code alike.
4. The drift flag is raised only above its thresholds, never at them.
5. A round whose estimates kits flags in its diagnostics is not reported as ok.

## Scope

| Function | Output that reaches a report | Error types asked |
|---|---|---|
| `kitsui_round`, `kitsui_estimate_warnings` | `bank.csv`, `versions.csv`, `checks.csv`, round log | Formula, data management, variable mapping, boundary |
| `kitsui_cap`, `kitsui_category_problems` | Merged scores, category checks | Boundary, data management |
| `kitsui_flag_rule` | The flag of a round | Boundary |
| `kitsui_link_internal`, `kitsui_link_D` | `links.csv` | Formula, interval |
| `kitsui_score` | Score table (`d_score`, `se_total`) | Formula, interval, variable mapping |
| `kitsui_code_*` | Downloaded R code | Variable mapping |

## Engine equivalence

kitsUI has no estimator of its own: it calls `kits_item`, `kits_drift`, `kits_linking` and
`kits_subj`, which are compared with dexter, psychotools, TAM and mirt in the kits report. The
downloaded R code is compared with the app instead: `tests/testthat/test-pipeline_code.R` runs the
code and checks that its numbers equal the app's (free and fixed rounds, links, scores, item table).

## Metamorphic relations

Script: `kitverse/inst/validation/metamorphic.R`. 4 functions × 3 relations, plus a fourth for
`kitsui_score` (interval); 13 of 13 hold.

| Function | Relations |
|---|---|
| `kitsui_round` | Row order, one-to-one renaming of visit and subject IDs, and an all-NA item leave the bank and the checks unchanged |
| `kitsui_cap` | Capping twice equals capping once; column order; NA, items outside the bank and `no_new` items behave as stated |
| `kitsui_category_problems` | Row order; score + 1 gives "no 0 score"; an emptied middle category gives "score gap" |
| `kitsui_score` | Row order; internal-link target + c gives theta + c; an all-zero visit gets a finite score below everyone with the same booklet; (R4, interval) on D, se_total² = d_score_se² + se_link² for every visit, se_link² is a quadratic in ability within each scale (the variance of a random affine map, largest residual 7.7e-14), and a new bootstrap seed for the D link changes se_link (by up to 0.07) but not d_score or d_score_se |

## Report snapshot

Script: `inst/validation/snapshot.R`. Scenario: `kitm03` and `kitm36`, lan and motor, 3, 6, 12, 36
and 48 months, one round, internal links for both domains, the D link, then scores. The files are
compared with the MD5 in `inst/validation/results/snapshot_md5.txt`.

| File | Lines | Two runs identical |
|---|---|---|
| `bank.csv` | 487 | yes |
| `links.csv` | 220 | yes |
| `waves.csv`, `wave_summary.csv` | 15 each | yes |
| `versions.csv`, `checks.csv`, `pass_steps.csv` | header only | yes |
| `scores.csv` | 5,477 | yes |

A release that changes any of these files says why in NEWS. The estimate warnings of 0.8.8 leave
the snapshot unchanged: kits flags none of its estimates.

## Mutation testing

9 mutants in copies of the source; 6 killed (67%). Scripts and results:
`kitverse/inst/validation/mutation/`.

| Error type | Mutants | Killed | Surviving (and the missing test) |
|---|---|---|---|
| Boundary | 4 | 2 | U03 (drift share `>` became `>=`), U05 (fixable `<` became `<=`): tests added in 0.8.8 |
| Variable mapping | 2 | 1 | U04 (`judged` replaced by `n_fixed` in the share): test added in 0.8.8 |
| Formula | 1 | 1 | |
| Interval | 1 | 1 | |
| Data management | 1 | 1 | |

## Faults and regression tests

From NEWS and git (`kitverse/inst/validation/results/bugs.csv`): 35 faults recorded. The faults
after 2026-11-01:

| Version | Fault | Error type | Caught by | Regression test |
|---|---|---|---|---|
| 0.8.8 | A round with estimates that kits flags (diagnostics "extreme estimates": beta −19, SE 8665) was reported ok and its values went into the bank. With only M03 3, 36 and 48 months picked, the two groups of booklets share only lanc01, whose scores do not overlap between them | Boundary | This validation (demo data) | `test-pipeline_round.R` |
| 0.8.7 | Downloaded round code did not leave out or fix items with a score category problem, so `kits_item()` stopped where the app went on | Boundary | App-versus-code test, after the demo data changed | yes |
| 0.8.6 | Tests named items of the old demo data | Other (test) | Unit tests | not needed |
| 0.8.5 | 2 WARNINGs on r-universe | Other | r-universe | not needed |
| 0.8.2 | A later round with fewer scales logged a `map2_lgl` warning | Boundary | Not stated | no |
| 0.7.1 | Plot download button shown when the plot cannot be drawn | Interface | Not stated | yes |
| 0.7.1 | R/ sourced twice at start-up | Install | Not stated | unknown |
| 0.6.0 | Round 1 with one cohort showed no chart | Boundary | Not stated | yes |

A lesson from 0.8.6 and 0.8.7: the app-versus-code test first failed, and 0.8.6 moved it to waves
where the problem did not occur. The code was fixed 5 hours later. Under rule 5 of the standard, a
failing test is fixed in the code, not by changing the test.

## Metrics

| Metric | Value |
|---|---|
| Acceptance criteria written beforehand | 0 |
| First-generation pass rate | 12 of 19 versions with a behaviour change (63%) |
| Faults caught per layer | Snapshot 0, metamorphic 0, mutation 3 missing tests, validation 1 (U35, by inspecting the demo rounds); history: unit test 2, r-universe 1, not stated 4 (after release) |
| Faults found after release | 8 (strict: after 2026-11-01), 30 (broad: in any earlier tag) |

## Reproduce

```r
source(system.file("validation", "snapshot.R", package = "kitsUI"))
source(system.file("validation", "metamorphic.R", package = "kitverse"))
```
