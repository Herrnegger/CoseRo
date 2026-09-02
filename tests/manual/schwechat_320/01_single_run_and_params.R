# =============================================================================
# Schwechat 320 (lumped, NZ=1) -- single run + parameter read-in check
#
# Project: D:/COSERO/COSERO_NDC/COSERO_320_Schwechat
# Exe    : COSERO_Lhotse_v0.9.6_release.exe (small NDC-era release build)
#
# Goals:
#   1. Confirm the package can drive this exe for a single cold-start run.
#   2. Confirm the parameter file (para_opt.txt, NDC-era columns incl.
#      HYPSO*/SOILVAR_B/HYDROVAR_B/CTVAR_B/NDC_/FHL_) reads in correctly via
#      read_cosero_parameters() and matches what is on disk.
#
# Notes:
#   - input/statevar.dmp exists but a warm-start attempt just before this
#     session errored ("Error opening input - statevar.dmp"). Use cold start
#     (statevar_source = 1) here; do not touch statevar_source = 2 until that
#     is understood.
#   - defaults.txt has SPINUP = 11137, STARTDATE 2013, ENDDATE 2024 already
#     set -- this is a real project, not a fresh setup_cosero_project() tree.
#     We override to a short period + OUTPUTTYPE = 1 for a fast smoke test
#     rather than reproducing the full 12-year OUTPUTTYPE=3 run that already
#     exists in output_0.9.6_release/.
#   - create_backup stays TRUE (default): this is real project data, back up
#     defaults.txt before run_cosero() rewrites it.
# =============================================================================

devtools::load_all()

project_path <- "D:/COSERO/COSERO_NDC/COSERO_320_Schwechat"
exe_name     <- "COSERO_Lhotse_v0.9.6_release.exe"
param_file   <- "para_opt.txt"   # matches PARAFILE in Defaults.txt

stopifnot(dir.exists(project_path))
stopifnot(file.exists(file.path(project_path, exe_name)))

# =============================================================================
# 1. PARAMETER READ-IN CHECK (no COSERO run needed)
# =============================================================================

cat("\n=== Parameter file read-in ===\n")

params <- read_cosero_parameters(file.path(project_path, "input", param_file))

cat("rows (zones):", nrow(params), "  cols:", ncol(params), "\n")
cat("NB_ / NZ_ present:", all(c("NB_", "NZ_") %in% names(params)), "\n")

# NDC-era columns this project's para_opt.txt carries -- confirm they made it
# through the reader unmodified.
ndc_cols <- c("LAPSE_T_", "LAPSE_P_", "SOILVAR_B", "HYDROVAR_B", "CTVAR_B",
             "NDC_", "FHL_", paste0("HYPSO", seq(0, 100, 5), "_"))
missing <- setdiff(ndc_cols, names(params))
if (length(missing)) {
  cat("MISSING NDC columns:", paste(missing, collapse = ", "), "\n")
} else {
  cat("All", length(ndc_cols), "NDC-era columns present.\n")
}

cat("\nKey values (zone 1):\n")
print(params[1, intersect(c("NB_", "IZ_", "NZ_", "M_", "KBF_", "BETA_",
                            "LAPSE_T_", "LAPSE_P_", "SOILVAR_B",
                            "HYDROVAR_B", "CTVAR_B", "NDC_", "FHL_"),
                          names(params))])

# Cross-check a couple of values directly against the raw file (independent
# of the reader) so a parsing bug would show up as a mismatch here.
raw_line  <- readLines(file.path(project_path, "input", param_file))[3]
raw_toks  <- as.numeric(strsplit(trimws(raw_line), "[[:space:]]+")[[1]])
raw_hdr   <- strsplit(trimws(readLines(file.path(project_path, "input", param_file))[2]), "[[:space:]]+")[[1]]
names(raw_toks) <- raw_hdr

for (col in c("M_", "NDC_", "FHL_", "HYPSO50_")) {
  ok <- isTRUE(all.equal(unname(params[[col]][1]), unname(raw_toks[[col]])))
  cat(sprintf("  %-10s reader=%-12s raw=%-12s  %s\n",
             col, params[[col]][1], raw_toks[[col]], if (ok) "OK" else "MISMATCH"))
}

# =============================================================================
# 2. SINGLE COLD-START RUN (short period, fast smoke test)
# =============================================================================

cat("\n=== Single run ===\n")

result <- run_cosero(
  project_path = project_path,
  exe_name = exe_name,
  defaults_settings = list(
   # STARTDATE  = "2018 1 1 0 0",
   # ENDDATE    = "2019 12 31 0 0",
    SPINUP     = 365,
    OUTPUTTYPE = 1,
    PARAFILE   = param_file
  ),
  statevar_source = 1,   # cold start -- see note above re: statevar.dmp error
  tmmon_option = 1,
  quiet = FALSE
)

cat("\nsuccess:", result$success, "  has_error:", result$has_error, "\n")
if (result$has_error) cat("error_message:", result$error_message, "\n")
cat("runtime (s):", round(result$runtime_seconds, 1), "\n")

if (isTRUE(result$success)) {
  nse <- extract_run_metrics(result, "001", "NSE")
  kge <- extract_run_metrics(result, "001", "KGE")
  cat(sprintf("NSE = %.4f   KGE = %.4f\n", nse, kge))
}

# =============================================================================
# 3. LENIENT ID-COLUMN HEADER CHECK (NB/IZ/NZ with/without "_", any case)
# =============================================================================
# read_cosero_parameters() normalizes the structural id columns (NB_, IZ_,
# NZ_, WATERBODY_, SOILTYPE_) via normalize_id_column_names() right after
# parsing the header, so a file whose header spells them "nb", "Nb_", "NB",
# etc. is read identically to the canonical "NB_" form. This does NOT touch
# calibration parameter columns (BETA_, TAB1, ...) -- those were already
# matched leniently via find_parameter_column(), unchanged here.
#
# This section copies para_opt.txt, rewrites ONLY its header line with
# mangled id-column names, and checks that read_cosero_parameters() on the
# copy produces the same NB_/IZ_/NZ_ columns and values as the original.
# The COSERO exe is not involved in this section (it reads whatever header
# is literally on disk; this only tests the R-side reader).

cat("\n=== Lenient ID-column header check ===\n")

orig_file <- file.path(project_path, "input", param_file)
mangled_file <- file.path(tempdir(), "para_opt_mangled_header.txt")

orig_lines <- readLines(orig_file)
header_idx <- 2  # line 1 = project info, line 2 = column header (skip_lines = 1 default)

mangle_header <- function(header_line) {
  tokens <- strsplit(trimws(header_line), "[[:space:]]+")[[1]]
  # Strip trailing "_" and lowercase ONLY the structural id columns; leave
  # every calibration parameter column exactly as it is in the real file.
  id_cols <- c("NB_", "IZ_", "NZ_", "WATERBODY_", "SOILTYPE_")
  is_id <- tokens %in% id_cols
  tokens[is_id] <- tolower(sub("_$", "", tokens[is_id]))  # "NB_" -> "nb"
  paste(tokens, collapse = "\t")
}

mangled_lines <- orig_lines
mangled_lines[header_idx] <- mangle_header(orig_lines[header_idx])
writeLines(mangled_lines, mangled_file)

cat("original header id columns :",
   paste(intersect(c("NB_", "IZ_", "NZ_"),
                   strsplit(trimws(orig_lines[header_idx]), "[[:space:]]+")[[1]]),
        collapse = " "), "\n")
cat("mangled header id columns  :",
   paste(intersect(c("nb", "iz", "nz"),
                   strsplit(trimws(mangled_lines[header_idx]), "[[:space:]]+")[[1]]),
        collapse = " "), "\n")

params_mangled <- read_cosero_parameters(mangled_file, quiet = TRUE)

cat("NB_/IZ_/NZ_ present after read (should be TRUE):",
   all(c("NB_", "IZ_", "NZ_") %in% names(params_mangled)), "\n")
cat("NB_ class (should be integer):", class(params_mangled$NB_), "\n")

same_values <- isTRUE(all.equal(params_mangled$NB_, params$NB_)) &&
  isTRUE(all.equal(params_mangled$IZ_, params$IZ_)) &&
  isTRUE(all.equal(params_mangled$NZ_, params$NZ_)) &&
  isTRUE(all.equal(params_mangled$BETA_, params$BETA_))
cat("NB_/IZ_/NZ_/BETA_ values match original file:", same_values, "\n")

file.remove(mangled_file)
