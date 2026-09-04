# =============================================================================
# Peak Difference (PDIFF) Metric
# =============================================================================

#' Peak Difference (PDIFF) - Nash-Sutcliffe Efficiency of Top N Peaks
#'
#' Evaluates model performance on the highest independent flood peaks within
#' an event search and masking window, following the COSERO Fortran
#' formulation in \code{optimCOS_objectiveFunctions.f}.
#'
#' @details
#' The algorithm iteratively extracts the \code{n_maxima} highest peaks from the
#' observed series. For each observed peak at time index \eqn{i}, it searches for
#' the corresponding simulated peak within an event window \eqn{[i - W, i + W]},
#' which allows for timing (lead/lag) errors. Once recorded, the entire window is
#' masked out from both series so that secondary peaks of the same storm event
#' cannot be selected again (hydrological declustering).
#'
#' The Nash-Sutcliffe Efficiency (NSE) is then calculated over the paired peak values:
#' \deqn{PDIFF = 1 - \frac{\sum_{k=1}^N (Q_{\mathrm{obs}, k} - Q_{\mathrm{sim}, k})^2}{\sum_{k=1}^N (Q_{\mathrm{obs}, k} - \bar{Q}_{\mathrm{obs}})^2}}
#'
#' \strong{Missing values and sentinels.}
#' Observations that are \code{NA}, negative, or at or below \code{na_value}
#' (default \code{-999}, COSERO's no-data sentinel) are excluded from peak
#' selection. This mirrors the Fortran \code{eval} mask, which is applied to
#' \code{qobs} only. Invalid \emph{simulated} values are carried as \code{NA}
#' and dropped pairwise at the aggregation step -- they never enter the sums as
#' numbers. Crucially, an invalid simulated value does \strong{not} remove the
#' corresponding observed peak: if it did, the set of evaluated events would
#' depend on the simulation, and an optimiser would score different candidate
#' parameter sets against different peaks.
#'
#' \strong{Time-step aware default window (COSERO Fortran standard).}
#' In COSERO Fortran the event window radius is 48 \emph{timesteps}, which on the
#' hourly data it was designed for equals 48 hours (2 days before and after the
#' peak). This function expresses the window physically in hours
#' (\code{window_hours = 48}) and converts it using the time step inferred from
#' \code{time}:
#' \itemize{
#'   \item \strong{Hourly} (\eqn{\Delta t = 1\text{ h}}): \eqn{48 / 1 = 48} steps (Fortran default)
#'   \item \strong{Daily} (\eqn{\Delta t = 24\text{ h}}): \eqn{48 / 24 = 2} steps
#'   \item \strong{15-minute} (\eqn{\Delta t = 0.25\text{ h}}): \eqn{48 / 0.25 = 192} steps
#' }
#' Specifying \code{window} explicitly overrides the automatic scaling and uses
#' that exact number of timesteps. If \code{window} is \code{NULL} and no usable
#' \code{time} vector is supplied, the function \strong{errors} rather than
#' guessing a time step -- assuming hourly data would silently apply a
#' \eqn{\pm 48}-day window to daily input.
#'
#' \strong{Agreement with COSERO Fortran.}
#' Verified against \code{statistics.txt} on real hourly output. The one case
#' where the two differ is a series too short to contain \code{n_maxima}
#' independent peaks -- roughly \eqn{n_{maxima} \times 2W} timesteps are needed,
#' about 1440 for the hourly defaults. The Fortran then still divides by
#' \code{pdiff_number_maxima} and counts unfilled peak slots (which keep their
#' \code{-99.0} initialiser) in both sums; this function computes NSE over the
#' peaks actually found and warns instead.
#'
#' @param sim Numeric vector of simulated discharge.
#' @param obs Numeric vector of observed discharge.
#' @param n_maxima Integer, number of independent peaks to evaluate (default: 15,
#'   the COSERO Fortran default).
#' @param window Integer, search and masking radius in timesteps. If \code{NULL}
#'   (default), derived from \code{time} and \code{window_hours}.
#' @param time Optional vector of \code{Date} or \code{POSIXct} timestamps
#'   corresponding to \code{sim} and \code{obs}. Required when \code{window} is
#'   \code{NULL}, and used to label peaks when \code{details = TRUE}.
#' @param window_hours Numeric, physical event window radius in hours
#'   (default: 48, i.e. \eqn{\pm 2} days around the peak). Ignored when
#'   \code{window} is given.
#' @param na_value Numeric no-data sentinel, or \code{NULL} to disable. Values at
#'   or below it are treated as missing in both series (default: \code{-999},
#'   matching COSERO output).
#' @param details Logical, if \code{TRUE}, returns a list containing the scalar
#'   metric and a data frame of the detected peak pairs.
#'
#' @return Numeric scalar (PDIFF value) or, if \code{details = TRUE}, a list with:
#'   \describe{
#'     \item{val}{The PDIFF efficiency score.}
#'     \item{peaks}{A data frame with columns \code{rank}, \code{obs_idx},
#'       \code{obs_peak}, \code{sim_idx}, \code{sim_peak}, \code{lag_steps}
#'       (\code{sim_idx - obs_idx}), and \code{obs_time}/\code{sim_time} if
#'       \code{time} was supplied.}
#'     \item{n_peaks}{Number of independent peaks actually identified.}
#'     \item{n_maxima}{Number of peaks requested.}
#'     \item{window}{Effective window radius in timesteps used.}
#'   }
#'
#' @seealso \code{\link{calculate_run_metrics}} to compute PDIFF directly from a
#'   COSERO run, \code{\link{extract_run_metrics}} to read COSERO's own
#'   pre-calculated PDIFF from \code{statistics.txt}.
#'
#' @export
#' @examples
#' \dontrun{
#' # Hourly simulation, window auto-scaled to 48 / 1 = 48 steps:
#' pdiff(sim, obs, time = datetime_vec)
#'
#' # Daily simulation, window auto-scaled to 48 / 24 = 2 steps:
#' pdiff(sim, obs, time = date_vec)
#'
#' # 72-hour event window on 15-min data (72 / 0.25 = 288 steps):
#' pdiff(sim, obs, time = datetime_vec, window_hours = 72)
#'
#' # Explicit step count, reproducing the raw COSERO Fortran window:
#' pdiff(sim, obs, window = 48)
#'
#' # Inspect the extracted peaks, values, and timing lags:
#' res <- pdiff(sim, obs, time = datetime_vec, details = TRUE)
#' head(res$peaks)
#' }
pdiff <- function(sim,
                  obs,
                  n_maxima = 15,
                  window = NULL,
                  time = NULL,
                  window_hours = 48,
                  na_value = -999,
                  details = FALSE) {
  stopifnot(length(sim) == length(obs))
  n <- length(obs)
  if (n == 0) return(NA_real_)

  # 1. Determine window in timesteps
  if (is.null(window)) {
    dt_hours <- NA_real_
    if (!is.null(time) && length(time) > 1) {
      diffs_sec <- as.numeric(diff(time), units = "secs")
      diffs_sec <- diffs_sec[!is.na(diffs_sec) & diffs_sec > 0]
      if (length(diffs_sec) > 0) {
        dt_hours <- stats::median(diffs_sec) / 3600
      }
    }
    if (is.na(dt_hours)) {
      # No usable timestamps: the physical window cannot be converted to
      # timesteps. Assuming hourly data would silently produce a +/-48-day
      # window on daily input, so require an explicit choice instead.
      stop("Cannot convert 'window_hours' to timesteps: no usable 'time' ",
           "vector supplied. Pass 'time' (Date/POSIXct timestamps) for ",
           "automatic scaling, or set 'window' explicitly in timesteps ",
           "(COSERO Fortran uses window = 48).",
           call. = FALSE)
    }
    window <- as.integer(round(window_hours / dt_hours))
  } else {
    window <- as.integer(window)
  }

  window <- max(1L, window)

  # 2. Local working copies
  obs_work <- as.numeric(obs)
  sim_work <- as.numeric(sim)

  # Invalidate observations only (mirrors the Fortran `eval` mask, which is
  # applied to qobs alone). Coupling this to sim would make the evaluated
  # event set depend on the simulation, so the objective function would score
  # different candidate parameter sets on different peaks.
  obs_invalid <- is.na(obs_work) | obs_work < 0
  if (!is.null(na_value)) obs_invalid <- obs_invalid | obs_work <= na_value
  obs_work[obs_invalid] <- -Inf

  # Sentinel/missing simulated values must never enter the sums as numbers.
  # They are carried as NA and dropped pairwise at the aggregation step.
  sim_invalid <- is.na(sim_work)
  if (!is.null(na_value)) sim_invalid <- sim_invalid | sim_work <= na_value
  sim_work[sim_invalid] <- NA_real_

  obs_peaks <- rep(NA_real_, n_maxima)
  sim_peaks <- rep(NA_real_, n_maxima)
  obs_indices <- rep(NA_integer_, n_maxima)
  sim_indices <- rep(NA_integer_, n_maxima)

  # 3. Iterative peak extraction with declustering window masking
  k <- 0L
  for (i in seq_len(n_maxima)) {
    idx_max <- which.max(obs_work)
    # which.max() drops -Inf only when everything is -Inf (empty result);
    # an explicit check is still needed because a single remaining -Inf
    # would otherwise be recorded as a peak.
    if (length(idx_max) == 0 || !is.finite(obs_work[idx_max])) break

    obs_max <- obs_work[idx_max]
    lb <- max(1L, idx_max - window)
    ub <- min(n, idx_max + window)

    k <- k + 1L
    obs_peaks[k] <- obs_max
    obs_indices[k] <- idx_max

    sim_window <- sim_work[lb:ub]
    if (any(!is.na(sim_window))) {
      rel_sim_max <- which.max(sim_window)
      sim_idx <- lb + rel_sim_max - 1L
      sim_peaks[k] <- sim_work[sim_idx]
      sim_indices[k] <- sim_idx
    } else {
      sim_peaks[k] <- NA_real_
      sim_indices[k] <- NA_integer_
    }

    # Decluster: remove the whole event window from both series so secondary
    # peaks of the same storm cannot be selected again.
    obs_work[lb:ub] <- -Inf
    sim_work[lb:ub] <- NA_real_
  }

  n_short <- n_maxima - k

  # 4. NSE on extracted peaks
  if (k < 2L) {
    val <- NA_real_
  } else {
    o <- obs_peaks[seq_len(k)]
    s <- sim_peaks[seq_len(k)]
    valid <- !is.na(o) & !is.na(s)
    if (sum(valid) < 2L) {
      val <- NA_real_
    } else {
      o_val <- o[valid]
      s_val <- s[valid]
      denom <- sum((o_val - mean(o_val))^2)
      if (denom == 0) {
        val <- NA_real_
      } else {
        val <- 1.0 - sum((o_val - s_val)^2) / denom
      }
    }
    if (n_short > 0) {
      warning("pdiff: only ", k, " of ", n_maxima, " independent peaks found ",
              "(window = ", window, " timesteps). PDIFF is computed over ",
              k, " peaks. Reduce 'n_maxima' or 'window' for a longer ",
              "evaluation period.", call. = FALSE)
    }
  }

  if (!details) {
    return(val)
  }

  peaks_df <- data.frame(
    rank = seq_len(k),
    obs_idx = obs_indices[seq_len(k)],
    obs_peak = obs_peaks[seq_len(k)],
    sim_idx = sim_indices[seq_len(k)],
    sim_peak = sim_peaks[seq_len(k)],
    lag_steps = sim_indices[seq_len(k)] - obs_indices[seq_len(k)]
  )
  if (!is.null(time) && length(time) == n) {
    peaks_df$obs_time <- time[peaks_df$obs_idx]
    peaks_df$sim_time <- time[peaks_df$sim_idx]
  }

  list(val = val, peaks = peaks_df, n_peaks = k, n_maxima = n_maxima,
       window = window)
}
