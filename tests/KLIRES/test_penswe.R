# Manual test: PenSWE (SWE left on 31.08) -- no COSERO run needed
#
# Part 1 checks penswe() on a small synthetic series whose answer is worked
# out by hand. Part 2 reads the swwgeb.txt of an existing glacier run; on the
# a-priori output it gives 113.84 mm over 2001-2020 (SPINUP 365), but the
# file is rewritten by every optimization run (and absent while one is
# running), so only its structure is checked. Part 3 renders the PenSWE
# section of the optimization report and the CSV export from those numbers,
# without running an optimization.

devtools::load_all()


# --- 1  Synthetic series ------------------------------------------------------
# Daily 2000-01-01 .. 2002-12-31. Subbasin 1 always carries 20 mm (excess 15),
# subbasin 2 stays at 3 mm (below threshold), subbasin 3 grows by 100 mm/yr.
dates <- seq(as.Date("2000-01-01"), as.Date("2002-12-31"), by = "day")
yr    <- as.integer(format(dates, "%Y"))
swe <- data.frame(
  yyyy = yr,
  mm   = as.integer(format(dates, "%m")),
  dd   = as.integer(format(dates, "%d")),
  hh = 0L, min = 0L,
  SWWGEB_00001 = 20,
  SWWGEB_00002 = 3,
  SWWGEB_00003 = 100 * (yr - 1999)       # 100, 200, 300 on each 31.08
)

p <- penswe(swe, threshold = 5)
# excess per subbasin: 15 | 0 | mean(95, 195, 295) = 195  -> mean = 70
stopifnot(nrow(p$swe_on_date) == 3,
          isTRUE(all.equal(p$per_subbasin$mean_excess, c(15, 0, 195))),
          isTRUE(all.equal(p$value, 70)),
          identical(p$per_subbasin$years_over, c(3, 0, 3)))

# Spin-up covering the first 31.08 (2000-08-31 is timestep 244) drops that year
p_sp <- penswe(swe, threshold = 5, spinup = 365)
stopifnot(nrow(p_sp$swe_on_date) == 2,
          isTRUE(all.equal(p_sp$per_subbasin$mean_excess[3], 245)))

# Weights: renormalised over evaluable subbasins; unpadded ids are matched
p_w <- penswe(swe, subbasins = c("1", "3", "99"), threshold = 5,
              weights = c(3, 1, 10))
stopifnot(isTRUE(all.equal(p_w$value, (3 * 15 + 1 * 195) / 4)),
          is.na(p_w$per_subbasin$mean_excess[3]))

# Hourly data: only the last timestep of 31.08 counts
hourly <- swe[rep(seq_len(nrow(swe)), each = 24), ]
hourly$hh <- rep(0:23, times = nrow(swe))
hourly$SWWGEB_00001[hourly$mm == 8 & hourly$dd == 31 & hourly$hh < 23] <- 1e4
stopifnot(isTRUE(all.equal(penswe(hourly, threshold = 5)$value, 70)))

# Penalty specification
cfg <- resolve_penalty("PenSWE", c("001", "002"))$PenSWE
stopifnot(cfg$weight == 0.001, cfg$threshold == 5, cfg$date == "08-31",
          identical(cfg$subbasins, c("001", "002")))
cfg <- resolve_penalty(list(PenSWE = list(weight = 0.002)), "001")$PenSWE
stopifnot(cfg$weight == 0.002, cfg$threshold == 5)
stopifnot(inherits(try(resolve_penalty("PenXYZ", "001"), silent = TRUE),
                   "try-error"))
cat("Part 1 (synthetic): all checks passed\n")


# --- 2  Real glacier run ------------------------------------------------------
output_dir <- "D:/KLIRES/working files/Model/Model_glaciers_01/output"
if (!file.exists(file.path(output_dir, "swwgeb.txt"))) {
  stop("No swwgeb.txt in ", output_dir, " -- run the model once with the ",
       "new COSERO build (and not while an optimization is running).")
}

target <- sprintf("%03d", 1:403)        # as optimize_cosero_dds("all") gives
swe_real <- read_swwgeb(output_dir, subbasins = target)
stopifnot(length(attr(swe_real, "missing")) == 0)

base <- penswe(swe_real, subbasins = target, threshold = 5, spinup = 365)
cat(sprintf("Baseline PenSWE: %.2f mm over %d years (%s-%s)\n",
            base$value, nrow(base$swe_on_date),
            rownames(base$swe_on_date)[1], tail(rownames(base$swe_on_date), 1)))
stopifnot(is.finite(base$value), nrow(base$swe_on_date) == 20,
          nrow(base$per_subbasin) == 403)

ps <- base$per_subbasin
cat("\nWorst subbasins on 31.08:\n")
print(head(ps[order(-ps$mean_excess), ], 10), row.names = FALSE)
cat(sprintf("\n%d of %d subbasins above 5 mm in at least one year\n",
            sum(ps$years_over > 0), nrow(ps)))


# --- 3  Report section and export --------------------------------------------
# A fake "optimized" result: every subbasin's excess halved
final <- base
final$per_subbasin$mean_excess <- final$per_subbasin$mean_excess / 2
final$value <- base$value / 2
penalty <- resolve_penalty("PenSWE", target)

print_optimization_report(
  algorithm = "DDS", par_filename = "para.txt", target_subbasins = target[1:2],
  zones_to_modify = NULL, metric = "NSE", metric_weights = NULL,
  subbasin_weights = NULL, aggregation = "mean", runtime = 0, n_iter = 0,
  initial_metrics = NULL, final_metrics = NULL,
  initial_param_summary = NULL, final_param_summary = NULL,
  opt_filename = "para_opt.txt", penalty = penalty,
  initial_penswe = base, final_penswe = final
)

export_dir <- file.path(tempdir(), "penswe_export")
export_cosero_optimization(
  list(par_bounds = data.frame(parameter = "CTMAX"), value = -0.5,
       runtime_seconds = 0, par = 1, metric = "NSE", target_subbasins = target,
       aggregation = "mean", algorithm = "DDS", penalty = penalty,
       initial_penswe = base, final_penswe = final,
       eval_log = data.frame(eval = 1, objective = -0.5 + 0.1138,
                             metric = 0.5, penswe = 113.8)),
  export_dir
)
exp <- read.csv(file.path(export_dir, "penswe_per_subbasin.csv"))
stopifnot(nrow(exp) == 403,
          isTRUE(all.equal(exp$mean_excess_optimized, exp$mean_excess_initial / 2)))
cat("Part 2-3 (real file, report, export): all checks passed\n")
