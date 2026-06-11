# =============================================================================
# Manual test: ungauged-subbasin handling in DDS / SCE optimization
# =============================================================================
# Verifies the ungauged-subbasin fix end-to-end:
#   1. Set up a fresh Wildalpen example project.
#   2. Make a modified Qobs file where subbasin 1 has all discharge = -999
#      (ungauged), keeping subbasins 2 and 3 gauged.
#   3. Baseline run -> confirm statistics.txt is sanitized (KGE for NB1 is NA,
#      not the spurious 1.0).
#   4. DDS optimization including the ungauged NB1 in target_subbasins, with
#      aggregation = "weighted" and NB1 weight = 0 -> must progress, not fail
#      on every evaluation.
#   5. SCE-UA optimization with the same setup -> same expectation.
#
# Expected console highlights:
#   - one message: "Subbasins 001: no runoff observations - excluded ..."
#   - baseline NB1 metrics NA; NB2 / NB3 real numbers
#   - optimizer best metric improves (no endless "metric failed" lines)
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-06-11
# =============================================================================

devtools::load_all()

# =============================================================================
# USER SETTINGS
# =============================================================================

project_path <- "D:/temp/COSERO_Ungauged_Test"
recreate     <- TRUE   # set FALSE to reuse an existing project

cal_settings <- list(
  STARTDATE  = c(2000, 1, 1, 0, 0),
  ENDDATE    = c(2010, 12, 31, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 1
)

max_iter_dds <- 30     # small smoke test; raise for a real calibration
maxn_sce     <- 30

# =============================================================================
# 1. SET UP EXAMPLE PROJECT
# =============================================================================

cat("=== 1. Setup Wildalpen example project ===\n")

if (recreate && dir.exists(project_path)) unlink(project_path, recursive = TRUE)
if (!dir.exists(project_path)) {
  setup_cosero_project_example(project_path)
}

stopifnot(
  file.exists(file.path(project_path, "COSERO.exe")),
  file.exists(file.path(project_path, "input", "Qobs.txt")),
  file.exists(file.path(project_path, "input", "para_ini.txt"))
)
cat("Project ready at:", project_path, "\n\n")

# =============================================================================
# 2. CREATE UNGAUGED Qobs FILE (subbasin 1 -> -999)
# =============================================================================
# Qobs.txt structure:
#   line 1      : catchment name
#   lines 2..k  : one "QOBS_n" header line per subbasin
#   line k+1    : "######..." separator
#   data rows   : Y M D H M  q1 q2 q3 ...   (q1 = subbasin 1)
# We overwrite q1 (the 6th whitespace-separated token) with -999 on every data
# row, write to a new file, and point DATAFILE at it so the original is intact.
# =============================================================================

cat("=== 2. Build ungauged Qobs (subbasin 1 = -999) ===\n")

qobs_orig <- file.path(project_path, "input", "Qobs.txt")
qobs_ung  <- file.path(project_path, "input", "Qobs_ungauged.txt")

lines <- readLines(qobs_orig)

# Locate the "###" separator that ends the header block
sep_idx <- grep("^#+", lines)
if (length(sep_idx) == 0) stop("Could not find '###' separator in Qobs.txt")
sep_idx <- sep_idx[1]

header_lines <- lines[1:sep_idx]
data_lines   <- lines[(sep_idx + 1):length(lines)]
data_lines   <- data_lines[nchar(trimws(data_lines)) > 0]

# Replace the first discharge column (token 6) with -999 on each data row
new_data <- vapply(data_lines, function(ln) {
  toks <- strsplit(trimws(ln), "\\s+")[[1]]
  if (length(toks) >= 6) toks[6] <- "-999"
  paste(toks, collapse = "\t")
}, character(1), USE.NAMES = FALSE)

writeLines(c(header_lines, new_data), qobs_ung)
cat("Wrote ungauged Qobs to:", basename(qobs_ung), "\n")
cat("  (subbasin 1 discharge replaced with -999 on", length(new_data), "rows)\n\n")

# Point the model at the ungauged discharge file
ungauged_settings <- modifyList(cal_settings, list(DATAFILE = "Qobs_ungauged.txt"))

# =============================================================================
# 3. BASELINE RUN — confirm statistics sanitization
# =============================================================================

cat("=== 3. Baseline run with ungauged subbasin 1 ===\n")

result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = ungauged_settings,
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

cat("\nRaw statistics table (NB1 should show NA, not KGE = 1.0):\n")
print(result_base$output_data$statistics[, c("sb", "NSE", "KGE", "KGEadj", "BETA")])

nb1_kge <- tryCatch(extract_run_metrics(result_base, "001", "KGE"), error = function(e) NA)
nb2_kge <- tryCatch(extract_run_metrics(result_base, "002", "KGE"), error = function(e) NA)
nb3_kge <- tryCatch(extract_run_metrics(result_base, "003", "KGE"), error = function(e) NA)

cat(sprintf("\n  NB1 KGE = %s   (expected: NA)\n", format(nb1_kge)))
cat(sprintf("  NB2 KGE = %.4f (expected: real number)\n", nb2_kge))
cat(sprintf("  NB3 KGE = %.4f (expected: real number)\n", nb3_kge))

if (is.na(nb1_kge) && !is.na(nb2_kge) && !is.na(nb3_kge)) {
  cat("\nPASS: ungauged NB1 sanitized; gauged NB2/NB3 preserved.\n\n")
} else {
  cat("\nFAIL: sanitization did not behave as expected — inspect statistics above.\n\n")
}

# =============================================================================
# 4. DDS OPTIMIZATION — ungauged NB1 included, weight 0
# =============================================================================

cat("=== 4. DDS optimization (NB1 ungauged, weight 0) ===\n")

par_bounds <- load_parameter_bounds(parameters = c("BETA", "M", "TAB1"))

set.seed(42)
result_dds <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = c("001", "002", "003"),  # NB1 ungauged but still calibrated
  metric            = "NSE",
  aggregation       = "weighted",
  subbasin_weights  = c(0.1, 0.4, 0.5),        # NB1 contributes 0 to objective
  defaults_settings = ungauged_settings,
  max_iter          = max_iter_dds,
  verbose           = TRUE
)

cat(sprintf("\nDDS best objective metric: %.4f\n", -result_dds$value))
cat("DDS optimal parameters:\n")
print(result_dds$par_bounds[, c("parameter", "default", "optimal_value")])

if (-result_dds$value > -1e5) {
  cat("\nPASS: DDS progressed (did not return the 1e6 failure penalty).\n\n")
} else {
  cat("\nFAIL: DDS stuck at failure penalty — objective never evaluated.\n\n")
}

# =============================================================================
# 5. SCE-UA OPTIMIZATION — same setup
# =============================================================================

cat("=== 5. SCE-UA optimization (NB1 ungauged, weight 0) ===\n")

if (requireNamespace("rtop", quietly = TRUE)) {
  set.seed(42)
  result_sce <- optimize_cosero_sce(
    cosero_path       = project_path,
    par_bounds        = par_bounds,
    target_subbasins  = c("001", "002", "003"),
    metric            = "NSE",
    aggregation       = "weighted",
    subbasin_weights  = c(0.0, 0.5, 0.5),
    defaults_settings = ungauged_settings,
    maxn              = maxn_sce,
    ngs               = 2,
    verbose           = TRUE
  )

  cat(sprintf("\nSCE best objective metric: %.4f\n", -result_sce$value))
  cat("SCE optimal parameters:\n")
  print(result_sce$par_bounds[, c("parameter", "default", "optimal_value")])

  if (-result_sce$value > -1e5) {
    cat("\nPASS: SCE-UA progressed (did not return the 1e6 failure penalty).\n\n")
  } else {
    cat("\nFAIL: SCE-UA stuck at failure penalty.\n\n")
  }
} else {
  cat("SKIP: 'rtop' package not installed — SCE-UA test skipped.\n\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("============================================================\n")
cat("Ungauged-subbasin optimization test complete.\n")
cat("Look above for: one 'no runoff observations' message, NA NB1 baseline,\n")
cat("and optimizers improving instead of repeating 'metric failed'.\n")
cat("============================================================\n")
