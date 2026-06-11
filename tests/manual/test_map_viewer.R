# Manual tests for launch_cosero_map() (R/cosero_map.R)
#
# Covers the verification list in dev/feature_map_timeseries_viewer.md:
#   1. Cache build + numerical equivalence with the ASCII readers.
#      NOTE: COSERO opens the *B files with access='direct' and never
#      truncates them, so they can carry a STALE TAIL from an earlier,
#      longer run (true for D:/temp/COSERO_Optim_DDS: 10958 records on
#      disk, 7671 valid). The reader recovers the valid region from the
#      ASCII first/last rows -- expect "(binary, 7671 rows, ...)" for all
#      four files.
#   2. Synthetic binary round-trip: validates the binary reader bit-exactly,
#      independent of which files happen to be on disk, plus stale-tail
#      recovery and full-mismatch rejection.
#   3. Cache invalidation on source mtime change.
#   4. Per-click latency + ungauged metric handling.
#   5. Synthetic 10,000-station map (interactive).
#   6. Real shapefile launch (interactive) -- see also
#      tests/manual/test_map_viewer_austria.R for the Austria-wide project.

devtools::load_all()

project_path <- "D:/temp/COSERO_Optim_DDS"
output_dir <- file.path(project_path, "output")

# Part 1: cache build + equivalence with ASCII readers #########################

cache_dir <- file.path(output_dir, ".cache")
unlink(cache_dir, recursive = TRUE)  # force fresh build
cache <- CoseRo:::build_output_cache(output_dir)

ascii_ref <- list(
  runoff = read_runoff(output_dir, quiet = TRUE),
  prec   = read_precipitation(output_dir, quiet = TRUE),
  plus   = read_plus(output_dir, quiet = TRUE),
  plus1  = read_plus1(output_dir, quiet = TRUE)
)

# ASCII prints 2-3 decimals; a binary-built cache is full float32 -> allow
# half ULP of the printed precision. An ASCII-built cache matches exactly.
tolerances <- c(runoff = 0.006, prec = 0.0006, plus = 0.0006, plus1 = 0.002)

for (part in names(cache)) {
  fst_df <- fst::read_fst(cache[[part]]$path)
  ref <- ascii_ref[[part]]
  ref <- ref[!is.na(ref$DateTime), , drop = FALSE]
  common <- setdiff(intersect(colnames(fst_df), colnames(ref)), "DateTime")

  stopifnot(nrow(fst_df) == nrow(ref))
  stopifnot(all(fst_df$DateTime == ref$DateTime))

  max_diff <- 0
  for (col in common) {
    a <- fst_df[[col]]; b <- ref[[col]]
    stopifnot(identical(is.na(a), is.na(b)))
    ok <- !is.na(a)
    if (any(ok)) max_diff <- max(max_diff, max(abs(a[ok] - b[ok])))
  }
  status <- if (max_diff <= tolerances[part]) "OK" else "FAIL"
  cat(sprintf("%-7s %3d common columns, max |cache - ascii| = %.6f  [%s]\n",
              part, length(common), max_diff, status))
  stopifnot(max_diff <= tolerances[part])
}

# Part 2: synthetic binary round-trip ##########################################
# Writes a small ASCII/binary pair in the documented COSERO.runoffB format
# and checks (a) exact recovery, (b) rejection when the ASCII content
# changes (stale-binary guard).

tmp <- file.path(tempdir(), "cosero_map_bin_test")
dir.create(tmp, showWarnings = FALSE)
n <- 50; nb <- 2
dts <- seq(as.POSIXct("2001-01-01"), by = "day", length.out = n)
set.seed(1)
vals <- matrix(round(runif(n * nb * 2) * 100, 3), nrow = n)
colnames(vals) <- c("QOBS_0001", "QSIM_0001", "QOBS_0002", "QSIM_0002")

ascii_file <- file.path(tmp, "COSERO.runoff")
writeLines(c(
  "synthetic test file",
  paste(" yyyy mm dd hh mm ", paste(colnames(vals), collapse = " ")),
  paste(format(dts, " %Y %m %d %H %M"),
        apply(vals, 1, function(r) paste(sprintf("%9.3f", r), collapse = " ")))
), ascii_file)

bin_file <- paste0(ascii_file, "B")
con <- file(bin_file, "wb")
writeBin(as.integer(nb), con, size = 4, endian = "little")
lt <- as.POSIXlt(dts)
for (i in seq_len(n)) {
  writeBin(as.integer(c(lt$year[i] + 1900, lt$mon[i] + 1, lt$mday[i], 0, 0)),
           con, size = 4, endian = "little")
  writeBin(vals[i, ], con, size = 4, endian = "little")
}
close(con)

nms <- CoseRo:::read_output_header_names(ascii_file)
edge <- CoseRo:::read_ascii_edge_rows(ascii_file)
df <- CoseRo:::read_cosero_binary_file(bin_file, nms, has_dates = TRUE,
                                       header_ints = 1L,
                                       edge_rows = edge)
stopifnot(!is.null(df), nrow(df) == n,
          max(abs(as.matrix(df[, colnames(vals)]) - vals)) < 1e-3,
          all(as.Date(df$DateTime) == as.Date(dts)))
cat("Synthetic binary round-trip: OK\n")

# Stale TAIL recovery: append leftover records from a longer "earlier run"
# (COSERO never truncates the *B files) -> reader must still return n rows
con <- file(bin_file, open = "ab")
for (i in 1:10) {
  writeBin(as.integer(c(2002, 1, i, 0, 0)), con, size = 4, endian = "little")
  writeBin(runif(nb * 2) * 100, con, size = 4, endian = "little")
}
close(con)
df_tail <- CoseRo:::read_cosero_binary_file(bin_file, nms, has_dates = TRUE,
                                            header_ints = 1L,
                                            edge_rows = edge)
stopifnot(!is.null(df_tail), nrow(df_tail) == n,
          identical(df_tail$DateTime, df$DateTime))
cat("Stale-tail recovery (non-truncated binary): OK\n")

# Full mismatch: perturbed ASCII edge values must reject the binary
edge_bad <- edge
edge_bad$last$values <- edge_bad$last$values + 1
stopifnot(is.null(CoseRo:::read_cosero_binary_file(bin_file, nms,
                                                   has_dates = TRUE,
                                                   header_ints = 1L,
                                                   edge_rows = edge_bad)))
cat("Stale-binary rejection: OK\n")
unlink(tmp, recursive = TRUE)

# Part 3: cache invalidation on mtime change ###################################

fst_mtime_before <- file.mtime(cache$runoff$path)
Sys.sleep(1)
Sys.setFileTime(file.path(output_dir, "COSERO.runoff"), Sys.time())
cache2 <- CoseRo:::build_output_cache(output_dir)
stopifnot(file.mtime(cache2$runoff$path) > fst_mtime_before)
cat("Cache invalidation on mtime change: OK\n")

# Part 4: per-click latency + ungauged handling ################################

env <- new.env()
t1 <- system.time(d1 <- CoseRo:::load_station_data(cache, 1, env))["elapsed"]
t2 <- system.time(d2 <- CoseRo:::load_station_data(cache, 1, env))["elapsed"]
cat(sprintf("First click: %.0f ms, repeated click (memoized): %.1f ms\n",
            t1 * 1000, t2 * 1000))
stopifnot(t1 < 0.5)  # spec: < ~100 ms; generous margin for slow disks
str(d1$metrics)

# Ungauged behaviour: all-NA / all -999 obs -> NULL metrics ("no observations")
stopifnot(is.null(CoseRo:::compute_station_metrics(rep(NA_real_, 100),
                                                   runif(100))))
stopifnot(is.null(CoseRo:::compute_station_metrics(rep(-999, 100),
                                                   runif(100))))
cat("Ungauged metric handling: OK\n")

# Part 5: synthetic 10,000-station map (interactive) ###########################
# Check: map renders, panning stays smooth, clicking any station with
# NB 1-3 opens the viewer; other NBs show the "No data" dialog.

if (interactive()) {
  set.seed(42)
  n <- 10000
  pts <- sf::st_as_sf(
    data.frame(NB = c(1:3, sample(1:3000, n - 3, replace = TRUE)),
               lon = stats::runif(n, 9.5, 17.2),
               lat = stats::runif(n, 46.4, 49.0)),
    coords = c("lon", "lat"), crs = 4326
  )
  launch_cosero_map(pts, cosero_path = project_path)
}

# Part 6: real shapefile launch (interactive) ##################################
# See tests/manual/test_map_viewer_austria.R for the Austria-wide project
# (gauges_basins_cosero.shp via field ID_, basins_info_CE.shp catchments).
