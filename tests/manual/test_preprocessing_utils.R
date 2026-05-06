# Manual tests: convert_txt_to_binary() and write_ehyd_qobs() ungauged basins
# Adjust paths to your local setup before running.

devtools::load_all()

# =============================================================================
# SETTINGS
# =============================================================================

ehyd_dir    <- "D:/temp/Qobs_eHYD/raw"           # folder with Q-Tagesmittel-*.csv
qobs_out    <- "D:/temp/Qobs_eHYD/test_preprocessing/Qobs.txt"
txt_file    <- "D:/temp/test_preprocessing/P_NZ_1991_2024.txt"  # existing ASCII met file

# =============================================================================
# 1. write_ehyd_qobs() — gauged + ungauged basins
# =============================================================================

cat("\n=== write_ehyd_qobs: gauged + ungauged ===\n")

gauge_mapping <- c(
  "210864" = 1,   # gauged
  "210880" = 2,   # gauged
  "NA"     = 3,   # ungauged — sentinel "NA"
  "-999"   = 4,   # ungauged — sentinel "-999"
  "-1"     = 5    # ungauged — negative number sentinel
)

qobs_df <- write_ehyd_qobs(
  input_dir      = ehyd_dir,
  output_file    = qobs_out,
  gauge_to_nb    = gauge_mapping,
  catchment_name = "Test-Catchment",
  start_date     = "2010-01-01",
  end_date       = "2050-12-31"
)

# Verify ungauged columns are entirely -999
stopifnot(all(qobs_df$QOBS_3 == -999))
stopifnot(all(qobs_df$QOBS_4 == -999))
stopifnot(all(qobs_df$QOBS_5 == -999))
cat("OK: ungauged columns contain only -999\n")

# Verify gauged columns are not entirely -999
stopifnot(any(qobs_df$QOBS_1 != -999))
cat("OK: gauged column QOBS_1 has real values\n")

# =============================================================================
# 2. convert_txt_to_binary() — basic conversion
# =============================================================================

cat("\n=== convert_txt_to_binary ===\n")

bin_file <- convert_txt_to_binary(txt_file)
cat("Output: ", bin_file, "\n")

# Check file exists and is non-empty
stopifnot(file.exists(bin_file))
stopifnot(file.info(bin_file)$size > 0)
cat("OK: binary file created\n")

# Verify record count: each record = (5 + NZ) * 4 bytes
# Read header of txt to get NZ
first_line <- readLines(txt_file, n = 1L)
n_cols     <- length(strsplit(trimws(first_line), "\\s+")[[1]])
n_zones    <- n_cols - 5L
n_rows_txt <- as.integer(system(paste0('find /c /v "" "', txt_file, '"'), intern = TRUE))
record_bytes <- (5L + n_zones) * 4L
expected_bytes <- file.info(txt_file[1])  # just for reference
actual_bytes   <- file.info(bin_file)$size
cat(sprintf("  Zones: %d | Record size: %d bytes | Binary size: %.1f MB\n",
            n_zones, record_bytes, actual_bytes / 1024^2))

# =============================================================================
# 3. convert_txt_to_binary() — overwrite = FALSE guard
# =============================================================================

cat("\n=== convert_txt_to_binary: overwrite guard ===\n")

result <- tryCatch(
  convert_txt_to_binary(txt_file, overwrite = FALSE),
  error = function(e) e
)
stopifnot(inherits(result, "error"))
cat("OK: error raised when bin_file exists and overwrite = FALSE\n")
