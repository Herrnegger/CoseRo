###############################################################################
# subset_cosero_model.R
#
# Builds a reduced, self-contained COSERO model directory from the full
# 13605-basin setup, keeping only a user-defined set of basins (NB ids) and a
# user-defined simulation period.
#
# The generated folder mirrors the source layout:
#
#   <dest>/COSERO.exe                 (copied)
#   <dest>/lib*.dll                   (copied)
#   <dest>/run_cosero_temp.bat        (copied)
#   <dest>/input/                     (subset)
#   <dest>/output/                    (created empty)
#
# ---------------------------------------------------------------------------
# USAGE
#
#   Edit the CONFIG block below and run:      Rscript subset_cosero_model.R
#
#   Or override from the command line:
#     Rscript subset_cosero_model.R --nb=10,12 --start=2000-01-01 --end=2010-12-31
#     Rscript subset_cosero_model.R --nb=100-250 --start=1990-01-01 --end=2000-12-31 \
#             --dest="D:/KLIRES/working files/Model_sub"
#
#   --nb accepts single ids, comma lists and ranges: "10,12", "100-250", "1,5,90-99"
#   --nb-file=<path> reads ids from a text file (one per line, or comma separated)
#
#   --met-format=bin | ascii | keep   format of the met files that get written.
#     Build the same subset both ways to compare COSERO read speed and size:
#       Rscript subset_cosero_model.R --nb=8067 --met-format=bin   --dest=".../sub_bin"
#       Rscript subset_cosero_model.R --nb=8067 --met-format=ascii --dest=".../sub_txt"
#     MetDefaults.txt (ASCIIorBIN, PRECFILE, TEMPFILE) and the file extensions
#     are rewritten to match, so each folder runs as-is.
#   --ascii-digits=<n>  decimals when writing ASCII met values (default 2)
#
# ---------------------------------------------------------------------------
# WHAT GETS SUBSET
#
#   Qobs_*.txt      header block (QOBS_<i>) and the value columns are reduced to
#                   the selected basins; rows are clipped to the date window.
#   para_*.txt      one row per selected zone; NB_/NZ_/TONZ_/Div_TONZ_ are
#                   renumbered to the new compact 1..n index space.
#   pr/tas met      clipped in time and in the zone dimension, and written in
#                   whichever format `met_format` asks for - the source may be
#                   binary or ASCII, all four conversion paths are supported.
#                   Binary layout: 5 int32 date + nzone float32 per record.
#   Defaults.txt    DATAFILE / PARAFILE / STARTDATE / ENDDATE / SPINUP rewritten.
#   MetDefaults.txt ASCIIorBIN / PRECFILE / TEMPFILE rewritten to match the
#                   format and the file names actually written.
#   radmat.par      copied unchanged (12 months x 24 hours, zone independent).
#   raster_write.txt copied unchanged.
#
# ---------------------------------------------------------------------------
# ROUTING NOTE
#
#   TONZ_ is the downstream zone; TONZ_ = 0 marks a catchment outlet. When a
#   selected zone drains to a zone that is NOT in the selection, that link
#   cannot be kept. Such zones become outlets (TONZ_ = 0, Div_TONZ_ = 0,
#   QDIV_* = 0) so the subset stays internally consistent. The script reports
#   how many links were cut. Set `expand_upstream = TRUE` to automatically pull
#   in the full upstream contributing area of the requested basins, which is
#   normally what you want for a hydrologically meaningful run.
###############################################################################

suppressWarnings(rm(list = ls()))
options(scipen = 999, stringsAsFactors = FALSE)

## ===========================================================================
## CONFIG  --  edit here (command line flags override these)
## ===========================================================================

cfg <- list(
  ## source model directory (the full 13605 basin setup)
  src  = "D:/KLIRES/working files/Model",

  ## destination directory for the reduced model
  dest = "D:/KLIRES/working files/Model_subset",

  ## basins to keep. Vector of NB ids, e.g. c(10, 12) or 100:250
  nb = c(4539),

  ## simulation window (inclusive), "YYYY-MM-DD"
  start_date = "2000-01-01",
  end_date   = "2020-12-31",

  ## spin-up length in time steps written to Defaults.txt.
  ## NULL keeps the value already in the source Defaults.txt.
  spinup = 365,

  ## pull in every upstream zone draining into the requested basins.
  ## STRONGLY recommended, otherwise the selected basins receive no inflow.
  expand_upstream = TRUE,

  ##### met file format:  "bin"  or  "ascii"   <<< CHOOSE HERE >>>
  met_format = "bin",

  ## number of decimal places when writing ASCII met values
  ascii_digits = 2,

  ## overwrite dest if it already exists
  overwrite = TRUE
)

## ===========================================================================
## command line overrides
## ===========================================================================

parse_nb_spec <- function(x) {
  x <- gsub("[[:space:]]", "", x)
  parts <- unlist(strsplit(x, ",", fixed = TRUE))
  parts <- parts[nzchar(parts)]
  out <- integer(0)
  for (p in parts) {
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      ab <- as.integer(unlist(strsplit(p, "-", fixed = TRUE)))
      out <- c(out, seq.int(ab[1], ab[2]))
    } else if (grepl("^[0-9]+$", p)) {
      out <- c(out, as.integer(p))
    } else {
      stop("cannot parse basin specification: '", p, "'")
    }
  }
  sort(unique(out))
}

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit) == 0) return(NULL)
  sub(paste0("^--", name, "="), "", hit[length(hit)])
}

if (!is.null(v <- get_flag("src")))     cfg$src        <- v
if (!is.null(v <- get_flag("dest")))    cfg$dest       <- v
if (!is.null(v <- get_flag("nb")))      cfg$nb         <- parse_nb_spec(v)
if (!is.null(v <- get_flag("nb-file"))) cfg$nb         <- parse_nb_spec(paste(readLines(v, warn = FALSE), collapse = ","))
if (!is.null(v <- get_flag("start")))   cfg$start_date <- v
if (!is.null(v <- get_flag("end")))     cfg$end_date   <- v
if (!is.null(v <- get_flag("spinup")))  cfg$spinup     <- as.integer(v)
if (!is.null(v <- get_flag("expand-upstream"))) cfg$expand_upstream <- toupper(v) %in% c("1", "TRUE", "YES")
if (!is.null(v <- get_flag("met-format")))      cfg$met_format      <- tolower(trimws(v))
if (!is.null(v <- get_flag("ascii-digits")))    cfg$ascii_digits    <- as.integer(v)

cfg$met_format <- tolower(trimws(cfg$met_format))
if (identical(cfg$met_format, "binary")) cfg$met_format <- "bin"
if (identical(cfg$met_format, "txt"))    cfg$met_format <- "ascii"
if (!cfg$met_format %in% c("bin", "ascii", "keep"))
  stop("met_format must be one of \"bin\", \"ascii\" or \"keep\" (got: '",
       cfg$met_format, "')")

## ===========================================================================
## helpers
## ===========================================================================

msg <- function(...) cat(sprintf(...), "\n", sep = "")
hdr <- function(t) { cat("\n"); cat(strrep("-", 74), "\n"); cat(t, "\n"); cat(strrep("-", 74), "\n") }

## read the "KEY (comment)" / "value" style COSERO settings files.
## Returns the raw lines; values are edited in place by key.
read_settings <- function(path) readLines(path, warn = FALSE)

## Replace the value line(s) that follow the line whose first token is `key`.
## n_val = how many lines of value follow the key line.
set_setting <- function(lines, key, value, n_val = 1L) {
  idx <- grep(paste0("^", key, "\\b"), lines)
  if (length(idx) == 0) {
    warning("key '", key, "' not found - left unchanged")
    return(lines)
  }
  idx <- idx[1]
  # the value lines start at the first non-empty line after the key line
  j <- idx + 1
  while (j <= length(lines) && !nzchar(trimws(lines[j]))) j <- j + 1
  if (n_val == 1L) {
    lines[j] <- value
  } else {
    # replace n_val consecutive lines with the supplied vector
    lines <- append(lines[-seq(j, j + n_val - 1)], value, after = j - 1)
  }
  lines
}

## which value lines follow a key (used to count multi-line PARAFILE blocks)
count_value_lines <- function(lines, key) {
  idx <- grep(paste0("^", key, "\\b"), lines)
  if (length(idx) == 0) return(0L)
  j <- idx[1] + 1
  while (j <= length(lines) && !nzchar(trimws(lines[j]))) j <- j + 1
  n <- 0L
  while (j + n <= length(lines) && nzchar(trimws(lines[j + n]))) n <- n + 1L
  n
}

fmt_date_cosero <- function(d) format(as.Date(d), "%Y %m %d 0 0")

## ===========================================================================
## 0) validate inputs
## ===========================================================================

hdr("COSERO model subsetting")

src  <- normalizePath(cfg$src, winslash = "/", mustWork = TRUE)
dest <- cfg$dest
src_in <- file.path(src, "input")
if (!dir.exists(src_in)) stop("input folder not found under: ", src)

start_date <- as.Date(cfg$start_date)
end_date   <- as.Date(cfg$end_date)
if (is.na(start_date) || is.na(end_date)) stop("start/end date could not be parsed (use YYYY-MM-DD)")
if (end_date < start_date) stop("end date is before start date")

nb_req <- sort(unique(as.integer(cfg$nb)))
if (length(nb_req) == 0) stop("no basins requested")

msg("source      : %s", src)
msg("destination : %s", dest)
msg("period      : %s .. %s", start_date, end_date)
msg("basins req. : %d  (%s%s)", length(nb_req),
    paste(head(nb_req, 12), collapse = ","),
    if (length(nb_req) > 12) ", ..." else "")

## ===========================================================================
## 1) read the source Defaults / MetDefaults to discover file names
## ===========================================================================

hdr("1) reading settings")

def_lines <- read_settings(file.path(src_in, "Defaults.txt"))
met_lines <- read_settings(file.path(src_in, "MetDefaults.txt"))

get_value <- function(lines, key, which = 1L) {
  idx <- grep(paste0("^", key, "\\b"), lines)
  if (length(idx) == 0) return(NA_character_)
  j <- idx[1] + 1
  while (j <= length(lines) && !nzchar(trimws(lines[j]))) j <- j + 1
  trimws(lines[j + which - 1L])
}

datafile   <- get_value(def_lines, "DATAFILE")
n_para     <- count_value_lines(def_lines, "PARAFILE")
parafiles  <- vapply(seq_len(n_para), function(i) get_value(def_lines, "PARAFILE", i), character(1))

## The source may list several PARAFILE entries; the subset gets exactly one -
## the first, which is the file COSERO actually reads. Defaults.txt is rewritten
## to a single PARAFILE line to match (see section 8).
parafile_keep <- parafiles[1]
precfile   <- get_value(met_lines, "PRECFILE")
tempfile_  <- get_value(met_lines, "TEMPFILE")
ascii_flag <- as.integer(get_value(met_lines, "ASCIIorBIN"))

msg("DATAFILE    : %s", datafile)
msg("PARAFILE    : %s%s", parafile_keep,
    if (n_para > 1)
      sprintf("   (%d listed, keeping the first)", n_para) else "")
msg("PRECFILE    : %s", precfile)
msg("TEMPFILE    : %s", tempfile_)
msg("ASCIIorBIN  : %d", ascii_flag)

## ===========================================================================
## 2) read the master parameter file -> topology
## ===========================================================================

hdr("2) reading parameter file / topology")

para_master <- file.path(src_in, parafiles[1])
if (!file.exists(para_master)) stop("parameter file not found: ", para_master)

para_title  <- readLines(para_master, n = 1, warn = FALSE)
para_header <- scan(para_master, what = "", sep = "\t", skip = 1, nlines = 1,
                    quiet = TRUE, quote = "")
para_header <- para_header[nzchar(para_header)]

## rows are ragged (trailing tabs vary), so read as raw lines and split
para_raw <- readLines(para_master, warn = FALSE)
para_body <- para_raw[-c(1, 2)]
para_body <- para_body[nzchar(trimws(para_body))]

split_tab <- strsplit(para_body, "\t", fixed = TRUE)
ncol_max  <- max(lengths(split_tab))
msg("parameter rows : %d", length(split_tab))
msg("header columns : %d   (max data columns: %d)", length(para_header), ncol_max)

col_of <- function(nm) {
  i <- match(nm, para_header)
  if (is.na(i)) stop("column '", nm, "' not found in parameter header")
  i
}
i_NB   <- col_of("NB_");   i_IZ  <- col_of("IZ_")
i_NZ   <- col_of("NZ_");   i_TO  <- col_of("TONZ_")
i_DIVT <- col_of("Div_TONZ_")
i_QLT  <- col_of("QDIV_LT_"); i_QUT <- col_of("QDIV_UT_"); i_QRAT <- col_of("QDIV_RATIO_")

fld <- function(v, i) { x <- v[i]; if (is.na(x) || !nzchar(x)) "0" else x }

NB   <- as.integer(vapply(split_tab, fld, character(1), i_NB))
NZ   <- as.integer(vapply(split_tab, fld, character(1), i_NZ))
TONZ <- as.integer(vapply(split_tab, fld, character(1), i_TO))
DIVT <- as.integer(vapply(split_tab, fld, character(1), i_DIVT))

n_zone_src <- length(NZ)
msg("zones in source: %d   (NB range %d..%d)", n_zone_src, min(NB), max(NB))

bad <- setdiff(nb_req, NB)
if (length(bad) > 0) {
  stop("requested basin id(s) not present in the parameter file: ",
       paste(head(bad, 20), collapse = ", "),
       if (length(bad) > 20) ", ..." else "")
}

## ===========================================================================
## 3) optional: expand selection upstream
## ===========================================================================

hdr("3) building zone selection")

## map zone id -> row index (here NZ == row, but do not rely on it)
row_of_zone <- integer(max(c(NZ, TONZ), na.rm = TRUE) + 1L)
row_of_zone[] <- NA_integer_
row_of_zone[NZ + 1L] <- seq_along(NZ)

sel_zone <- nb_req   # NB == NZ in this setup; keep the distinction explicit

if (isTRUE(cfg$expand_upstream)) {
  ## invert the TONZ links: for each target, list its direct upstream zones
  valid <- !is.na(TONZ) & TONZ > 0
  ups <- split(NZ[valid], TONZ[valid])

  frontier <- as.character(sel_zone)
  seen <- new.env(hash = TRUE, parent = emptyenv())
  for (z in sel_zone) assign(as.character(z), TRUE, envir = seen)

  while (length(frontier) > 0) {
    add <- unlist(ups[frontier], use.names = FALSE)
    add <- add[!is.na(add)]
    if (length(add) == 0) break
    add <- unique(add)
    new <- add[!vapply(as.character(add), exists, logical(1),
                       envir = seen, inherits = FALSE)]
    if (length(new) == 0) break
    for (z in new) assign(as.character(z), TRUE, envir = seen)
    frontier <- as.character(new)
  }
  sel_zone <- sort(as.integer(ls(seen)))
  msg("upstream expansion: %d requested -> %d zones kept",
      length(nb_req), length(sel_zone))
} else {
  sel_zone <- sort(unique(sel_zone))
  msg("upstream expansion disabled: %d zones kept", length(sel_zone))
}

keep_rows <- row_of_zone[sel_zone + 1L]
if (anyNA(keep_rows)) stop("internal error: some selected zones have no parameter row")

n_keep <- length(keep_rows)
## old zone id -> new compact id (1..n_keep), 0 stays 0 (outlet marker)
remap <- integer(max(c(NZ, TONZ), na.rm = TRUE) + 1L)
remap[] <- 0L
remap[sel_zone + 1L] <- seq_len(n_keep)

## how many downstream links leave the selection?
old_to <- TONZ[keep_rows]
cut_links <- sum(old_to > 0 & remap[old_to + 1L] == 0L)
msg("zones kept      : %d", n_keep)
msg("outlets created : %d link(s) leaving the selection were cut to TONZ_=0",
    cut_links)

## ===========================================================================
## 4) create destination tree
## ===========================================================================

hdr("4) creating destination tree")

if (dir.exists(dest) && !isTRUE(cfg$overwrite))
  stop("destination already exists and overwrite = FALSE: ", dest)

dest_in  <- file.path(dest, "input")
dest_out <- file.path(dest, "output")
for (d in c(dest, dest_in, dest_out))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
dest <- normalizePath(dest, winslash = "/", mustWork = TRUE)
msg("created: %s", dest)

## executable, dlls, batch, command file
root_files <- c("COSERO.exe", "libifcoremd.dll", "libiomp5md.dll", "libmmd.dll",
                "run_cosero_temp.bat")
for (f in root_files) {
  s <- file.path(src, f)
  if (file.exists(s)) {
    file.copy(s, file.path(dest, f), overwrite = TRUE)
    msg("copied  : %s", f)
  } else {
    msg("MISSING : %s (skipped)", f)
  }
}

## zone independent input files
for (f in c("radmat.par", "raster_write.txt")) {
  s <- file.path(src_in, f)
  if (file.exists(s)) {
    file.copy(s, file.path(dest_in, f), overwrite = TRUE)
    msg("copied  : input/%s", f)
  }
}

## ===========================================================================
## 5) subset the parameter file(s)
## ===========================================================================

hdr("5) writing parameter file(s)")

subset_para <- function(in_path, out_path) {
  raw  <- readLines(in_path, warn = FALSE)
  ttl  <- raw[1]
  hd   <- raw[2]
  body <- raw[-c(1, 2)]
  body <- body[nzchar(trimws(body))]
  if (length(body) != n_zone_src)
    stop("parameter file '", basename(in_path), "' has ", length(body),
         " rows, expected ", n_zone_src)

  st <- strsplit(body[keep_rows], "\t", fixed = TRUE)

  out <- vapply(seq_along(st), function(k) {
    v <- st[[k]]
    ## pad so the index positions always exist
    if (length(v) < i_QRAT) v <- c(v, rep("", i_QRAT - length(v)))

    v[i_NB] <- as.character(k)
    v[i_NZ] <- as.character(k)
    v[i_IZ] <- "1"

    to_old <- suppressWarnings(as.integer(fld(v, i_TO)))
    dv_old <- suppressWarnings(as.integer(fld(v, i_DIVT)))
    if (is.na(to_old)) to_old <- 0L
    if (is.na(dv_old)) dv_old <- 0L

    to_new <- if (to_old > 0L) remap[to_old + 1L] else 0L
    dv_new <- if (dv_old > 0L) remap[dv_old + 1L] else 0L

    if (to_new == 0L) {
      ## link left the selection (or was already an outlet) -> make it an outlet
      v[i_TO]   <- "0"
      v[i_DIVT] <- "0"
      v[i_QLT]  <- "0"
      v[i_QUT]  <- "0"
      v[i_QRAT] <- "0"
    } else {
      v[i_TO] <- as.character(to_new)
      if (dv_new == 0L) {
        ## the diversion target is gone: send everything down the main link
        v[i_DIVT] <- as.character(to_new)
        v[i_QLT]  <- "0"
        v[i_QUT]  <- "9999"
        v[i_QRAT] <- "1"
      } else {
        v[i_DIVT] <- as.character(dv_new)
      }
    }
    paste(v, collapse = "\t")
  }, character(1))

  writeLines(c(ttl, hd, out), out_path, sep = "\n")
  msg("wrote   : input/%s   (%d zones)", basename(out_path), length(out))
}

## only the file named by parafile_keep is written into the subset
local({
  s <- file.path(src_in, parafile_keep)
  if (file.exists(s)) subset_para(s, file.path(dest_in, parafile_keep))
  else stop("parameter file not found: ", s)
})
if (n_para > 1)
  msg("skipped : %s (only the first PARAFILE entry is kept)",
      paste(parafiles[-1], collapse = ", "))

## ===========================================================================
## 6) subset the discharge (Qobs) file
## ===========================================================================

hdr("6) writing discharge file")

subset_qobs <- function(in_path, out_path) {
  con <- file(in_path, "r")
  on.exit(close(con), add = TRUE)

  ttl <- readLines(con, n = 1, warn = FALSE)

  ## header block: one QOBS_<i> line per basin, terminated by a separator line
  qnames <- character(0)
  repeat {
    ln <- readLines(con, n = 1, warn = FALSE)
    if (length(ln) == 0) stop("unexpected end of file in Qobs header")
    if (grepl("^\\s*#", ln)) { sep_line <- ln; break }
    qnames <- c(qnames, trimws(ln))
  }
  n_q <- length(qnames)
  msg("Qobs header basins: %d", n_q)
  if (n_q < max(sel_zone))
    stop("Qobs file holds ", n_q, " basins but zone ", max(sel_zone), " was selected")

  ## new header: keep the selected names but renumber them 1..n
  new_names <- paste0("QOBS_", seq_len(n_keep))

  ocon <- file(out_path, "w")
  on.exit(close(ocon), add = TRUE)
  writeLines(c(ttl, new_names, sep_line), ocon)

  ## data: 5 date columns + n_q values
  keep_cols <- 5L + sel_zone
  n_written <- 0L; n_read <- 0L
  repeat {
    chunk <- readLines(con, n = 20000L, warn = FALSE)
    if (length(chunk) == 0) break
    chunk <- chunk[nzchar(trimws(chunk))]
    if (length(chunk) == 0) next
    n_read <- n_read + length(chunk)

    parts <- strsplit(chunk, "[[:space:]]+")
    parts <- lapply(parts, function(p) if (nzchar(p[1])) p else p[-1])

    y <- as.integer(vapply(parts, `[`, character(1), 1L))
    m <- as.integer(vapply(parts, `[`, character(1), 2L))
    d <- as.integer(vapply(parts, `[`, character(1), 3L))
    dt <- as.Date(sprintf("%04d-%02d-%02d", y, m, d))
    ok <- !is.na(dt) & dt >= start_date & dt <= end_date
    if (!any(ok)) {
      if (all(!is.na(dt)) && min(dt) > end_date) break
      next
    }

    sel <- which(ok)
    outl <- vapply(sel, function(i) {
      p <- parts[[i]]
      paste(c(p[1:5], p[keep_cols]), collapse = " ")
    }, character(1))
    writeLines(outl, ocon)
    n_written <- n_written + length(outl)
  }
  msg("wrote   : input/%s   (%d of %d time steps, %d basins)",
      basename(out_path), n_written, n_read, n_keep)
  if (n_written == 0)
    warning("no discharge time steps fell inside the requested period")
  n_written
}

q_src <- file.path(src_in, datafile)
if (!file.exists(q_src)) stop("discharge file not found: ", q_src)
invisible(subset_qobs(q_src, file.path(dest_in, datafile)))

## ===========================================================================
## 7) subset the meteorological files
## ===========================================================================

hdr("7) writing meteorological files")

## Binary layout (verified against the source files):
##   one record per time step, no Fortran record markers
##   [ 5 x int32 : YYYY MM DD hh mm ][ nzone x float32 : values ]
##
## Four conversion paths are supported, so the source format and the requested
## output format are independent:
##      bin   -> bin      met_bin2bin
##      bin   -> ascii    met_bin2ascii
##      ascii -> ascii    met_ascii2ascii
##      ascii -> bin      met_ascii2bin
## All of them stream in chunks, so the 1.2 GB inputs never land in memory.

met_chunk_rows <- 20000L          # ASCII rows read per pass
met_bin_bytes  <- 64e6            # ~ bytes read per binary pass

## --- shared helpers --------------------------------------------------------

## date vector -> logical mask for the requested window
in_window <- function(y, m, d) {
  dt <- as.Date(sprintf("%04d-%02d-%02d", y, m, d))
  list(ok = !is.na(dt) & dt >= start_date & dt <= end_date, dt = dt)
}

## split whitespace separated rows into a list of character field vectors
split_rows <- function(chunk) {
  parts <- strsplit(chunk, "[[:space:]]+")
  lapply(parts, function(p) if (nzchar(p[1])) p else p[-1])
}

## geometry of a binary met file with n_zone values per record
bin_geom <- function(path, n_zone) {
  rec_words <- 5L + n_zone
  rec_bytes <- rec_words * 4L
  fsz <- file.info(path)$size
  if (fsz %% rec_bytes != 0)
    stop("binary '", basename(path), "' size ", fsz,
         " is not a multiple of the record size ", rec_bytes,
         " - zone count or layout mismatch")
  list(n_rec = fsz %/% rec_bytes, rec_bytes = rec_bytes, rec_words = rec_words)
}

## --- bin -> bin ------------------------------------------------------------

met_bin2bin <- function(in_path, out_path, n_zone) {
  g <- bin_geom(in_path, n_zone)
  icon <- file(in_path, "rb");  on.exit(close(icon), add = TRUE)
  ocon <- file(out_path, "wb"); on.exit(close(ocon), add = TRUE)

  chunk_rec <- max(1L, floor(met_bin_bytes / g$rec_bytes))
  done <- 0L; kept <- 0L

  while (done < g$n_rec) {
    nr   <- min(chunk_rec, g$n_rec - done)
    rawv <- readBin(icon, "raw", n = nr * g$rec_bytes)
    if (length(rawv) < nr * g$rec_bytes) break

    ints <- readBin(rawv, "integer", n = nr * g$rec_words, size = 4L)
    dim(ints) <- c(g$rec_words, nr)
    w <- in_window(ints[1, ], ints[2, ], ints[3, ])

    if (any(w$ok)) {
      flts <- readBin(rawv, "numeric", n = nr * g$rec_words, size = 4L)
      dim(flts) <- c(g$rec_words, nr)
      for (i in which(w$ok)) {
        writeBin(as.integer(ints[1:5, i]), ocon, size = 4L)
        writeBin(as.double(flts[5L + sel_zone, i]), ocon, size = 4L)
      }
      kept <- kept + sum(w$ok)
    }
    done <- done + nr
    if (!anyNA(w$dt) && length(w$dt) && min(w$dt) > end_date) break
  }
  list(kept = kept, total = g$n_rec)
}

## --- bin -> ascii ----------------------------------------------------------

met_bin2ascii <- function(in_path, out_path, n_zone) {
  g <- bin_geom(in_path, n_zone)
  icon <- file(in_path, "rb"); on.exit(close(icon), add = TRUE)
  ocon <- file(out_path, "w"); on.exit(close(ocon), add = TRUE)

  chunk_rec <- max(1L, floor(met_bin_bytes / g$rec_bytes))
  done <- 0L; kept <- 0L
  nd <- as.integer(cfg$ascii_digits)

  while (done < g$n_rec) {
    nr   <- min(chunk_rec, g$n_rec - done)
    rawv <- readBin(icon, "raw", n = nr * g$rec_bytes)
    if (length(rawv) < nr * g$rec_bytes) break

    ints <- readBin(rawv, "integer", n = nr * g$rec_words, size = 4L)
    dim(ints) <- c(g$rec_words, nr)
    w <- in_window(ints[1, ], ints[2, ], ints[3, ])

    if (any(w$ok)) {
      flts <- readBin(rawv, "numeric", n = nr * g$rec_words, size = 4L)
      dim(flts) <- c(g$rec_words, nr)
      idx <- which(w$ok)

      ## build the whole chunk at once: "Y M D h m" + formatted values
      dhead <- apply(ints[1:5, idx, drop = FALSE], 2L,
                     function(z) paste(z, collapse = " "))
      vals <- matrix(formatC(flts[5L + sel_zone, idx, drop = FALSE],
                             format = "f", digits = nd),
                     nrow = length(sel_zone))
      vrow <- apply(vals, 2L, paste, collapse = " ")

      writeLines(paste(dhead, vrow), ocon)
      kept <- kept + length(idx)
    }
    done <- done + nr
    if (!anyNA(w$dt) && length(w$dt) && min(w$dt) > end_date) break
  }
  list(kept = kept, total = g$n_rec)
}

## --- ascii -> ascii --------------------------------------------------------

met_ascii2ascii <- function(in_path, out_path, n_zone) {
  icon <- file(in_path, "r");  on.exit(close(icon), add = TRUE)
  ocon <- file(out_path, "w"); on.exit(close(ocon), add = TRUE)
  keep_cols <- 5L + sel_zone
  kept <- 0L; total <- 0L

  repeat {
    chunk <- readLines(icon, n = met_chunk_rows, warn = FALSE)
    if (length(chunk) == 0) break
    chunk <- chunk[nzchar(trimws(chunk))]
    if (length(chunk) == 0) next
    total <- total + length(chunk)

    parts <- split_rows(chunk)
    y <- as.integer(vapply(parts, "[", character(1), 1L))
    m <- as.integer(vapply(parts, "[", character(1), 2L))
    d <- as.integer(vapply(parts, "[", character(1), 3L))
    w <- in_window(y, m, d)

    if (any(w$ok)) {
      outl <- vapply(which(w$ok), function(i) {
        p <- parts[[i]]; paste(c(p[1:5], p[keep_cols]), collapse = " ")
      }, character(1))
      writeLines(outl, ocon)
      kept <- kept + length(outl)
    }
    if (!anyNA(w$dt) && length(w$dt) && min(w$dt) > end_date) break
  }
  list(kept = kept, total = total)
}

## --- ascii -> bin ----------------------------------------------------------

met_ascii2bin <- function(in_path, out_path, n_zone) {
  icon <- file(in_path, "r");   on.exit(close(icon), add = TRUE)
  ocon <- file(out_path, "wb"); on.exit(close(ocon), add = TRUE)
  keep_cols <- 5L + sel_zone
  kept <- 0L; total <- 0L

  repeat {
    chunk <- readLines(icon, n = met_chunk_rows, warn = FALSE)
    if (length(chunk) == 0) break
    chunk <- chunk[nzchar(trimws(chunk))]
    if (length(chunk) == 0) next
    total <- total + length(chunk)

    parts <- split_rows(chunk)
    y <- as.integer(vapply(parts, "[", character(1), 1L))
    m <- as.integer(vapply(parts, "[", character(1), 2L))
    d <- as.integer(vapply(parts, "[", character(1), 3L))
    w <- in_window(y, m, d)

    if (any(w$ok)) {
      for (i in which(w$ok)) {
        p <- parts[[i]]
        writeBin(as.integer(p[1:5]), ocon, size = 4L)
        writeBin(as.double(as.numeric(p[keep_cols])), ocon, size = 4L)
      }
      kept <- kept + sum(w$ok)
    }
    if (!anyNA(w$dt) && length(w$dt) && min(w$dt) > end_date) break
  }
  list(kept = kept, total = total)
}

## --- driver ----------------------------------------------------------------

met_files <- c(precfile, tempfile_)
met_files <- met_files[!is.na(met_files) & nzchar(met_files) &
                       !grepl("^not_used", met_files, ignore.case = TRUE)]

## what the source declares, and what was asked for
src_is_bin <- identical(ascii_flag, 1L)
out_fmt <- if (identical(cfg$met_format, "keep"))
             (if (src_is_bin) "bin" else "ascii") else cfg$met_format
out_is_bin <- identical(out_fmt, "bin")

msg("source format : %s   (ASCIIorBIN = %d)",
    if (src_is_bin) "binary" else "ascii", ascii_flag)
msg("output format : %s%s", out_fmt,
    if (identical(cfg$met_format, "keep")) "  (kept from source)" else "")

## target name: extension follows the chosen format
met_out_name <- function(mf) {
  base <- sub("\\.(bin|txt)$", "", mf, ignore.case = TRUE)
  paste0(base, if (out_is_bin) ".bin" else ".txt")
}

## source file actually present on disk: the declared name first, then its twin
met_src_path <- function(mf) {
  cand <- unique(c(mf,
                   sub("\\.(bin|txt)$", ".bin", mf, ignore.case = TRUE),
                   sub("\\.(bin|txt)$", ".txt", mf, ignore.case = TRUE)))
  for (c1 in cand) {
    p <- file.path(src_in, c1)
    if (file.exists(p)) return(p)
  }
  NA_character_
}

n_met_steps  <- integer(0)
met_name_map <- character(0)     # declared name -> written name

for (mf in met_files) {
  s <- met_src_path(mf)
  if (is.na(s)) { msg("MISSING : input/%s (skipped)", mf); next }

  in_is_bin <- grepl("\\.bin$", s, ignore.case = TRUE)
  onm <- met_out_name(mf)
  out <- file.path(dest_in, onm)
  met_name_map[mf] <- onm

  t0 <- proc.time()[["elapsed"]]
  r <- if (in_is_bin && out_is_bin)       met_bin2bin(s, out, n_zone_src)
       else if (in_is_bin)                met_bin2ascii(s, out, n_zone_src)
       else if (out_is_bin)               met_ascii2bin(s, out, n_zone_src)
       else                               met_ascii2ascii(s, out, n_zone_src)
  el <- proc.time()[["elapsed"]] - t0

  msg("wrote   : input/%s", onm)
  msg("          %s -> %s, %d of %d steps, %d zones, %.2f MB, %.1f s",
      if (in_is_bin) "bin" else "ascii", if (out_is_bin) "bin" else "ascii",
      r$kept, r$total, length(sel_zone),
      file.info(out)$size / 1024^2, el)

  if (r$kept == 0)
    warning("no met time steps fell inside the requested period for ",
            basename(s))
  n_met_steps <- c(n_met_steps, r$kept)
}


## ===========================================================================
## 8) rewrite Defaults.txt / MetDefaults.txt
## ===========================================================================

hdr("8) writing settings files")

new_def <- def_lines
## collapse the PARAFILE block to the single file that was actually written
new_def <- set_setting(new_def, "PARAFILE", parafile_keep, n_val = n_para)
new_def <- set_setting(new_def, "STARTDATE", fmt_date_cosero(start_date))
new_def <- set_setting(new_def, "ENDDATE",   fmt_date_cosero(end_date))
if (!is.null(cfg$spinup))
  new_def <- set_setting(new_def, "SPINUP", as.character(as.integer(cfg$spinup)))

## warn if the spin-up no longer fits inside the shortened period
spin <- suppressWarnings(as.integer(get_value(new_def, "SPINUP")))
n_steps <- as.integer(end_date - start_date) + 1L
if (!is.na(spin) && spin >= n_steps)
  warning("SPINUP (", spin, ") is >= the number of time steps in the period (",
          n_steps, ") - nothing will be evaluated. Lower it with --spinup=")

writeLines(new_def, file.path(dest_in, "Defaults.txt"))
msg("wrote   : input/Defaults.txt   (STARTDATE %s / ENDDATE %s / SPINUP %s)",
    fmt_date_cosero(start_date), fmt_date_cosero(end_date), spin)
msg("          PARAFILE -> %s", parafile_keep)

## MetDefaults: the format flag and the file names must match what section 7
## actually wrote, otherwise COSERO reads the wrong file or the wrong layout.
new_met <- met_lines
new_met <- set_setting(new_met, "ASCIIorBIN", if (out_is_bin) "1" else "0")
if (!is.na(precfile)  && precfile  %in% names(met_name_map))
  new_met <- set_setting(new_met, "PRECFILE", met_name_map[[precfile]])
if (!is.na(tempfile_) && tempfile_ %in% names(met_name_map))
  new_met <- set_setting(new_met, "TEMPFILE", met_name_map[[tempfile_]])

writeLines(new_met, file.path(dest_in, "MetDefaults.txt"))
msg("wrote   : input/MetDefaults.txt   (ASCIIorBIN %s)", if (out_is_bin) "1" else "0")
for (k in names(met_name_map))
  if (!identical(k, met_name_map[[k]]))
    msg("          %s  ->  %s", k, met_name_map[[k]])

## ===========================================================================
## 9) mapping table + summary
## ===========================================================================

hdr("9) summary")

map <- data.frame(new_NB = seq_len(n_keep),
                  old_NB = NB[keep_rows],
                  old_NZ = NZ[keep_rows],
                  requested = as.integer(NZ[keep_rows] %in% nb_req))
write.csv(map, file.path(dest, "basin_id_mapping.csv"), row.names = FALSE)
msg("wrote   : basin_id_mapping.csv  (old NB <-> new NB)")

if (length(n_met_steps) && length(unique(n_met_steps)) > 1)
  warning("the meteorological files ended up with different numbers of time ",
          "steps (", paste(n_met_steps, collapse = ", "),
          ") - the source files do not cover the same period")

msg("")
msg("basins requested : %d", length(nb_req))
msg("zones written    : %d", n_keep)
msg("period           : %s .. %s  (%d daily steps)", start_date, end_date, n_steps)
msg("met format       : %s", out_fmt)
msg("input size       : %.2f MB", sum(file.info(list.files(dest_in,
        full.names = TRUE))$size, na.rm = TRUE) / 1024^2)
msg("destination      : %s", dest)
msg("")
msg("Run it with:   cd \"%s\"  &&  run_cosero_temp.bat", dest)
cat(strrep("-", 74), "\n")
