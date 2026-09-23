# =============================================================================
# Snow Water Equivalent Penalty (PenSWE)
# =============================================================================

#' Read swwgeb.txt (Subbasin Snow Water Equivalent)
#'
#' Reads the subbasin-mean snow water equivalent written by COSERO builds that
#' support it (including OUTPUTTYPE 0). The file carries one row per timestep
#' and one \code{SWWGEB_<id>} column per simulated subbasin, in mm.
#'
#' Only the requested subbasin columns are parsed, which keeps the per-run cost
#' low when the file is read once per optimizer evaluation.
#'
#' @param output_dir Path to COSERO output directory
#' @param subbasins Subbasin IDs (character or numeric) to read, or NULL for
#'   all subbasins in the file. IDs are matched regardless of zero-padding
#'   (\code{"17"}, \code{"017"} and \code{"00017"} all find \code{SWWGEB_00017}).
#' @param quiet Suppress messages
#'
#' @return Data frame with columns \code{yyyy, mm, dd, hh, min} followed by the
#'   found \code{SWWGEB_<id>} columns, or NULL if the file does not exist.
#'   Requested subbasins without a column are listed in the \code{"missing"}
#'   attribute.
#' @export
#' @examples
#' \dontrun{
#' swe <- read_swwgeb("D:/COSERO_project/output", subbasins = c("17", "254"))
#' }
read_swwgeb <- function(output_dir, subbasins = NULL, quiet = TRUE) {
  file <- file.path(output_dir, "swwgeb.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: swwgeb.txt\n")

  header <- strsplit(trimws(readLines(file, n = 1L, warn = FALSE)), "\\s+")[[1]]
  sww_idx <- grep("^SWWGEB_", header)

  missing <- character(0)
  if (!is.null(subbasins)) {
    cols <- vapply(subbasins, function(sb) {
      find_subbasin_column(header, "SWWGEB_", sb)
    }, character(1))
    missing <- as.character(subbasins[is.na(cols)])
    sww_idx <- match(unique(cols[!is.na(cols)]), header)
  }

  # header = FALSE with explicit names: the header repeats "mm" (month and
  # minute), which fread would otherwise have to de-duplicate
  df <- fread(file, skip = 1, header = FALSE, data.table = FALSE,
              select = c(1:5, sww_idx),
              col.names = c("yyyy", "mm", "dd", "hh", "min", header[sww_idx]))

  attr(df, "missing") <- missing
  df
}

#' Find a per-subbasin output column regardless of id padding
#'
#' COSERO pads subbasin ids to 3, 4 or 5 digits depending on the project size.
#'
#' @param cols Available column names
#' @param prefix Column prefix, e.g. \code{"SWWGEB_"}
#' @param subbasin Subbasin ID (character or numeric)
#' @return The matching column name, or NA_character_
#' @keywords internal
find_subbasin_column <- function(cols, prefix, subbasin) {
  sb_num <- as.numeric(subbasin)
  candidates <- paste0(prefix, c(sprintf("%05d", sb_num), sprintf("%04d", sb_num),
                                 sprintf("%03d", sb_num), sprintf("%d", sb_num)))
  hit <- candidates[candidates %in% cols]
  if (length(hit) == 0) NA_character_ else hit[1]
}

#' SWE Accumulation Penalty (PenSWE)
#'
#' Penalises snow that survives the melt season. For every evaluated year the
#' subbasin SWE on the last timestep of \code{date} (default 31 August, the end
#' of COSERO's hydrological year) is compared against \code{threshold}; only the
#' amount above it counts:
#' \deqn{PenSWE = \mathrm{aggregate}_{sb}\left(\overline{\max(0, SWE_{sb,y} - threshold)}^{\,y}\right)}
#'
#' Subbasins below the threshold in every year contribute 0, so a parameter set
#' without snow accumulation is not penalised at all. The result is in mm.
#' In the optimizers it is multiplied by a weight and added to the objective
#' (see the \code{penalty} argument of \code{\link{optimize_cosero_dds}}).
#'
#' Years whose \code{date} falls inside the spin-up period are skipped. Unlike
#' discharge metrics, PenSWE needs no observations, so it applies to ungauged
#' subbasins as well.
#'
#' @param swe Data frame from \code{\link{read_swwgeb}}
#' @param subbasins Subbasin IDs to evaluate, or NULL for every
#'   \code{SWWGEB_} column in \code{swe}
#' @param threshold SWE (mm) allowed on \code{date} without penalty
#' @param spinup Number of spin-up timesteps to skip
#' @param date Day of year to evaluate, as \code{"MM-DD"}
#' @param weights Optional subbasin weights (same order as \code{subbasins}).
#'   Renormalised over the subbasins that could be evaluated. NULL gives the
#'   unweighted mean.
#'
#' @return List with
#'   \item{value}{Aggregated PenSWE in mm (NA if nothing could be evaluated)}
#'   \item{per_subbasin}{Data frame: subbasin, n_years, mean_excess (mm),
#'     max_swe (mm on \code{date}), years_over (count above threshold)}
#'   \item{swe_on_date}{Matrix of SWE on \code{date}, years x subbasins}
#'   \item{threshold, date}{The settings used}
#' @export
#' @examples
#' \dontrun{
#' swe <- read_swwgeb("D:/COSERO_project/output")
#' pen <- penswe(swe, threshold = 5, spinup = 365)
#' pen$value
#' head(pen$per_subbasin[order(-pen$per_subbasin$mean_excess), ])
#' }
penswe <- function(swe, subbasins = NULL, threshold = 5, spinup = 0,
                   date = "08-31", weights = NULL) {
  md <- as.integer(strsplit(date, "-", fixed = TRUE)[[1]])
  if (length(md) != 2 || anyNA(md)) {
    stop("date must be given as \"MM-DD\", e.g. \"08-31\"", call. = FALSE)
  }

  sww_cols <- grep("^SWWGEB_", colnames(swe), value = TRUE)
  if (is.null(subbasins)) {
    subbasins <- sub("^SWWGEB_", "", sww_cols)
    cols <- sww_cols
  } else {
    cols <- vapply(subbasins, function(sb) {
      find_subbasin_column(sww_cols, "SWWGEB_", sb)
    }, character(1))
  }
  if (!is.null(weights) && length(weights) != length(subbasins)) {
    stop("weights must match subbasins length", call. = FALSE)
  }

  # Last timestep of the evaluation day in each year after spin-up (hourly
  # runs have 24 rows per day; the end-of-day state is the one that counts)
  rows <- which(swe$mm == md[1] & swe$dd == md[2])
  rows <- rows[rows > spinup]
  rows <- rows[!duplicated(swe$yyyy[rows], fromLast = TRUE)]

  swe_on_date <- matrix(NA_real_, nrow = length(rows), ncol = length(subbasins),
                        dimnames = list(swe$yyyy[rows], as.character(subbasins)))
  found <- !is.na(cols)
  if (any(found) && length(rows) > 0) {
    swe_on_date[, found] <- as.matrix(swe[rows, cols[found], drop = FALSE])
  }
  swe_on_date[swe_on_date <= -999] <- NA

  excess <- pmax(swe_on_date - threshold, 0)
  n_years <- colSums(!is.na(swe_on_date))
  mean_excess <- ifelse(n_years > 0, colMeans(excess, na.rm = TRUE), NA_real_)

  per_subbasin <- data.frame(
    subbasin    = as.character(subbasins),
    n_years     = n_years,
    mean_excess = mean_excess,
    max_swe     = ifelse(n_years > 0,
                         suppressWarnings(apply(swe_on_date, 2, max, na.rm = TRUE)),
                         NA_real_),
    years_over  = colSums(excess > 0, na.rm = TRUE),
    row.names   = NULL,
    stringsAsFactors = FALSE
  )

  valid <- !is.na(mean_excess)
  value <- if (!any(valid)) {
    NA_real_
  } else if (!is.null(weights) && sum(weights[valid]) > 0) {
    sum(mean_excess[valid] * weights[valid]) / sum(weights[valid])
  } else {
    mean(mean_excess[valid])
  }

  list(value = value, per_subbasin = per_subbasin, swe_on_date = swe_on_date,
       threshold = threshold, date = date)
}

# Optimizer integration #####

#' Resolve the optimizer penalty specification
#'
#' Accepts \code{NULL} (no penalty), \code{"PenSWE"} (defaults) or
#' \code{list(PenSWE = list(...))} with any of \code{weight}, \code{threshold},
#' \code{date} and \code{subbasins}. Missing entries take their defaults;
#' \code{subbasins} defaults to all \code{target_subbasins}.
#'
#' @param penalty Penalty specification as passed to the optimizers
#' @param target_subbasins Resolved target subbasin IDs
#' @return NULL or \code{list(PenSWE = list(weight, threshold, date, subbasins))}
#' @keywords internal
resolve_penalty <- function(penalty, target_subbasins) {
  if (is.null(penalty)) return(NULL)
  if (is.character(penalty)) {
    penalty <- setNames(rep(list(list()), length(penalty)), penalty)
  }
  if (!is.list(penalty) || is.null(names(penalty)) || any(names(penalty) == "")) {
    stop("penalty must be NULL, \"PenSWE\" or list(PenSWE = list(...))",
         call. = FALSE)
  }
  unknown <- setdiff(names(penalty), "PenSWE")
  if (length(unknown) > 0) {
    stop("Unknown penalty: ", paste(unknown, collapse = ", "),
         ". Available: PenSWE", call. = FALSE)
  }

  cfg <- utils::modifyList(
    list(weight = 0.001, threshold = 5, date = "08-31",
         subbasins = target_subbasins),
    penalty$PenSWE
  )
  if (!is.numeric(cfg$weight) || length(cfg$weight) != 1 || cfg$weight < 0) {
    stop("PenSWE weight must be a single non-negative number", call. = FALSE)
  }
  if (!is.numeric(cfg$threshold) || length(cfg$threshold) != 1) {
    stop("PenSWE threshold must be a single number (mm)", call. = FALSE)
  }
  cfg$subbasins <- as.character(cfg$subbasins)

  list(PenSWE = cfg)
}

#' Subbasin weights for PenSWE
#'
#' With \code{aggregation = "weighted"}, PenSWE uses the subbasin weights as
#' the caller gave them -- before ungauged subbasins lose their weight in
#' \code{resolve_ungauged_subbasins()}, since snow accumulation needs no
#' observations. Every other aggregation (and a custom PenSWE subbasin list)
#' uses the unweighted mean; "min" and "product" have no meaning for a penalty.
#'
#' @param penalty Resolved penalty settings, or NULL
#' @param aggregation Aggregation method of the metric
#' @param subbasin_weights Caller's subbasin weights, or NULL
#' @param target_subbasins Resolved target subbasin IDs
#' @return Numeric weights aligned with \code{penalty$PenSWE$subbasins}, or NULL
#' @keywords internal
penalty_subbasin_weights <- function(penalty, aggregation, subbasin_weights,
                                     target_subbasins) {
  if (is.null(penalty$PenSWE) || aggregation != "weighted" ||
      is.null(subbasin_weights)) {
    return(NULL)
  }
  if (!identical(penalty$PenSWE$subbasins, as.character(target_subbasins))) {
    return(NULL)
  }
  subbasin_weights
}

#' Evaluate PenSWE on the current COSERO output
#'
#' @param output_dir COSERO output directory
#' @param cfg Resolved PenSWE settings (see \code{resolve_penalty})
#' @param spinup_value Spin-up timesteps
#' @param weights Subbasin weights aligned with \code{cfg$subbasins}, or NULL
#' @return Result of \code{\link{penswe}}, or NULL if swwgeb.txt is missing
#' @keywords internal
run_penswe <- function(output_dir, cfg, spinup_value, weights = NULL) {
  swe <- read_swwgeb(output_dir, subbasins = cfg$subbasins)
  if (is.null(swe)) return(NULL)
  penswe(swe, subbasins = cfg$subbasins, threshold = cfg$threshold,
         spinup = spinup_value, date = cfg$date, weights = weights)
}

#' Delete swwgeb.txt before a run
#'
#' run_cosero() does not clear the output folder, so an executable that does
#' not write swwgeb.txt would leave the previous run's file in place and
#' PenSWE would silently score stale snow.
#'
#' @param cosero_path COSERO project path
#' @keywords internal
clear_swwgeb <- function(cosero_path) {
  f <- file.path(cosero_path, "output", "swwgeb.txt")
  if (file.exists(f)) unlink(f)
  invisible(NULL)
}
