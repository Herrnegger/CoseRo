# Fix: ungauged subbasins break DDS/SCE optimization

## Problem

When a target subbasin has no runoff observations (Qobs coded as `-999` /
negative), every optimizer evaluation fails with "metric failed" and COSERO's
`statistics.txt` reports a spurious KGE = 1.0 (NSE = NaN) for that subbasin.
Both `optimize_cosero_dds()` and `optimize_cosero_sce()` are affected — they
share `create_objective_function()` and `calculate_single_metric()` in
`R/cosero_optimize.R`.

Desired behavior: ungauged subbasins may stay in `target_subbasins` (their
zone parameters are still calibrated, constrained via gauged/downstream
basins) but must be excluded from the objective function.

## Fix 1 — sanitize statistics at the source

File: `R/cosero_readers.R`, function `read_cosero_statistics()` (~lines
342–398). After the numeric conversion loop (~lines 382–386), for every row
where `NSE` is NA/NaN, set all other metric columns (KGE, BETA, ...) to NA.
NSE = NaN is COSERO's indicator for "no valid observations"; the spurious
KGE = 1.0 must never survive the read. (Verify the actual NA coding in a real
`statistics.txt` first; if a sentinel like `-9999` is used instead of NaN,
cover that too.)

This fixes single runs (`extract_run_metrics()` in `R/cosero_metrics.R`),
sensitivity analysis, optimization baselines, and reports in one place.
No change needed in `calculate_single_metric()`: with KGE now NA it falls
through to the time-series path, finds <10 valid obs pairs, returns NA —
same as NSE today.

## Fix 2 — weight-aware NA handling in the objective

File: `R/cosero_optimize.R`, inside `create_objective_function()` — replace
the blanket check at ~lines 548–551:

```r
if (any(is.na(metric_matrix))) { ... return(1e6) }
```

with a per-subbasin validity mask (row valid = all metrics non-NA), then:

- `aggregation = "weighted"`: return 1e6 only if a subbasin with
  `subbasin_weights > 0` is invalid; sum over valid rows (no renormalization
  — users give ungauged basins weight 0).
- `"mean"` / `"min"` / `"product"`: aggregate over valid rows only
  (~lines 561–566); return 1e6 only if no valid row remains.

Genuine model failures are still caught upstream by the `result$success`
check, so this is safe.

Apply the identical change to `dev/cosero_optimize_dv.R` (~line 397) if that
file is still maintained.

## Fix 3 — one-time warning

After the baseline run in `optimize_cosero_dds()` / `optimize_cosero_sce()`
(both call `run_initial_baseline()`), if any target subbasin has all-NA
baseline metrics, message once:

```
Subbasins 001, 002: no runoff observations - excluded from objective; their zones are still calibrated.
```

## Fix 4 — documentation

Roxygen of `optimize_cosero_dds()` AND `optimize_cosero_sce()` in
`R/cosero_optimize.R` (the `@details` block must be added to both; the param
text reaches SCE via `@inheritParams`).

Addition to `@param target_subbasins`:

```
#'   Subbasins without runoff observations may also be included --
#'   see section "Ungauged subbasins" in Details.
```

New block in `@details`:

```
#' \strong{Ungauged subbasins:}
#' Subbasins without runoff observations can be included in
#' \code{target_subbasins}. Missing observations are coded as negative
#' values in the observed runoff file (typically \code{-999}; COSERO
#' treats any value < 0 as missing). Including ungauged subbasins is
#' often desirable: their zone parameters are still modified during
#' calibration and are thereby constrained indirectly through the
#' observations at gauged (e.g. downstream) subbasins. Since no
#' performance metrics can be computed for ungauged subbasins, they are
#' automatically excluded from the objective function. With
#' \code{aggregation = "weighted"}, assign them a weight of 0; with the
#' other aggregation methods they are simply skipped.
```

Then run `devtools::document()` to regenerate `man/optimize_cosero_dds.Rd`
and `man/optimize_cosero_sce.Rd`.

## Verification

- `devtools::document()` runs clean; `R CMD check` (or `devtools::check()`)
  passes.
- Unit test idea: feed `read_cosero_statistics()` a statistics.txt fixture
  with an NSE = NaN row and assert KGE becomes NA.
- Reproduce the original scenario (4 target subbasins, first two ungauged,
  `aggregation = "weighted"`, weights `c(0, 0, 0.5, 0.5)`,
  `metric = c("NSE", "KGE")`): optimization must progress instead of
  returning "metric failed" on every run, and baseline output must not show
  KGE = 1.0 for the ungauged subbasins.
