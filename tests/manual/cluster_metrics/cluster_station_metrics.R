# ---------------------------------------------------------------------------
# Cluster COSERO subbasins by model-performance signature
# ---------------------------------------------------------------------------
# Reads the per-subbasin metrics CSV written by launch_cosero_map()
#   (<project>/output/.cache/station_metrics.csv)
# and groups subbasins into clusters with similar performance / deficit
# patterns. Useful for spotting regions where the model behaves alike
# (e.g. systematic over-/under-estimation, poor timing, low NSE).
#
# Clustering is done with BOTH hierarchical (Ward.D2) and k-means so the two
# labellings can be compared. lon/lat are folded into the feature matrix (with
# a tunable weight) so clusters are spatially coherent.
#
# Standalone base-R script. Optional 'cluster' package adds a silhouette
# diagnostic; without it the script falls back to the elbow plot only.
#
# Folders (relative to this script):
#   input/   drop a station_metrics.csv here to use it instead of csv_path
#   output/  all generated files (labelled CSV, summary, plots) land here
#
# Run:  "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" tests/manual/cluster_metrics/cluster_station_metrics.R
# ---------------------------------------------------------------------------

# ---- Configuration --------------------------------------------------------

# Input file name, read from this script's input/ folder (see resolution below).
# A copy of the Austria calibrated project's metrics already lives there.
input_csv_name <- "station_metrics.csv"

# Number of clusters (revise after looking at the elbow / silhouette diagnostic)
k <- 3

# Metrics that make up the performance signature
metric_cols <- "beta"# c("NSE", "KGE", "beta")#"r", "alpha", "beta")

# alpha/beta diverge around 1 (1 = ideal). "signed" keeps the raw value so
# over- vs under-estimation can separate; "magnitude" uses abs(x - 1).
ab_treatment <- "signed"          # "signed" or "magnitude"

# Fold lon/lat into the clustering. coord_weight scales the (already
# standardized) coordinate columns relative to the metric columns:
#   1  = coords weigh as much as one metric each
#  <1  = geography nudges, metrics dominate
#  >1  = geography dominates
include_coords <- TRUE
coord_weight   <- 0.5 #1.0

# k-means reproducibility
set.seed(42)

# ---- Input / output folders ------------------------------------------------

# Fixed absolute paths so the script works the same under Rscript, source(),
# or the IDE console (self-location is unreliable across all three).
in_dir  <- "D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R/tests/manual/cluster_metrics/input"
out_dir <- "D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R/tests/manual/cluster_metrics/output"
dir.create(in_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Input CSV comes from the input/ folder next to this script.
csv_path <- file.path(in_dir, input_csv_name)
cat("Input CSV:  ", csv_path,  "\n", sep = "")
cat("Output dir: ", out_dir, "\n", sep = "")

# ---- Read & clean ---------------------------------------------------------

if (!file.exists(csv_path)) {
  stop("Input CSV not found: ", csv_path,
       "\nDrop '", input_csv_name, "' into ", in_dir,
       " (or change input_csv_name at the top), then rerun.", call. = FALSE)
}

dat <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

needed <- c("subbasin", "lon", "lat", metric_cols)
missing_cols <- setdiff(needed, names(dat))
if (length(missing_cols) > 0) {
  stop("CSV is missing expected column(s): ",
       paste(missing_cols, collapse = ", "), call. = FALSE)
}

n_total <- nrow(dat)

# Drop rows with NA in ANY metric (ungauged subbasins, <10 valid pairs, etc.).
# If coords are used for clustering, also require non-NA lon/lat.
cluster_input_cols <- if (include_coords) c(metric_cols, "lon", "lat") else metric_cols
complete <- stats::complete.cases(dat[, cluster_input_cols, drop = FALSE])

dropped <- dat[!complete, , drop = FALSE]
dat <- dat[complete, , drop = FALSE]
n_dropped <- nrow(dropped)

cat(sprintf("Read %d subbasins; dropped %d with NA in %s; %d remain.\n",
            n_total, n_dropped,
            paste(cluster_input_cols, collapse = "/"), nrow(dat)))
if (n_dropped > 0) {
  cat("  Dropped subbasins: ",
      paste(dropped$subbasin, collapse = ", "), "\n", sep = "")
}
if (nrow(dat) < k + 1) {
  stop("Too few usable subbasins (", nrow(dat),
       ") for k = ", k, " clusters.", call. = FALSE)
}

# ---- Build feature matrix -------------------------------------------------

feat <- dat[, metric_cols, drop = FALSE]

if (ab_treatment == "magnitude") {
  feat$alpha <- abs(feat$alpha - 1)
  feat$beta  <- abs(feat$beta  - 1)
  cat("alpha/beta transformed to abs(x - 1) (deficit magnitude).\n")
} else if (ab_treatment == "signed") {
  cat("alpha/beta kept raw/signed.\n")
} else {
  stop("ab_treatment must be 'signed' or 'magnitude'.", call. = FALSE)
}

# Standardize each feature (mean 0, sd 1) so no metric dominates by scale.
X <- scale(as.matrix(feat))

if (include_coords) {
  coords <- scale(as.matrix(dat[, c("lon", "lat")]))
  coords <- coords * coord_weight
  colnames(coords) <- c("lon_z", "lat_z")
  X <- cbind(X, coords)
  cat(sprintf("Coordinates included in clustering (coord_weight = %.2f).\n",
              coord_weight))
} else {
  cat("Coordinates used for display only (not in clustering).\n")
}

# ---- Diagnostics to inform k ----------------------------------------------

diag_path <- file.path(out_dir, "cluster_diagnostics.png")
have_cluster_pkg <- requireNamespace("cluster", quietly = TRUE)

k_range <- 2:min(10, nrow(X) - 1)

# Elbow: within-cluster sum of squares vs k (k-means)
wss <- vapply(k_range, function(kk) {
  stats::kmeans(X, centers = kk, nstart = 25, iter.max = 100)$tot.withinss
}, numeric(1))

# Silhouette (optional): mean silhouette width vs k, for the hierarchical cut
sil <- rep(NA_real_, length(k_range))
if (have_cluster_pkg) {
  d <- stats::dist(X)
  hc_tmp <- stats::hclust(d, method = "ward.D2")
  sil <- vapply(seq_along(k_range), function(i) {
    cl <- stats::cutree(hc_tmp, k = k_range[i])
    if (length(unique(cl)) < 2) return(NA_real_)
    s <- cluster::silhouette(cl, d)
    mean(s[, "sil_width"])
  }, numeric(1))
}

grDevices::png(diag_path, width = 1100, height = 500, res = 110)
op <- graphics::par(mfrow = c(1, if (have_cluster_pkg) 2 else 1),
                    mar = c(4.2, 4.2, 2.5, 1))
plot(k_range, wss, type = "b", pch = 19,
     xlab = "k", ylab = "Total within-cluster SS",
     main = "Elbow (k-means)")
graphics::abline(v = k, lty = 2, col = "red")
if (have_cluster_pkg) {
  plot(k_range, sil, type = "b", pch = 19,
       xlab = "k", ylab = "Mean silhouette width",
       main = "Silhouette (hierarchical Ward.D2)")
  graphics::abline(v = k, lty = 2, col = "red")
}
graphics::par(op)
grDevices::dev.off()
cat("Wrote diagnostic: ", diag_path, "\n", sep = "")
if (!have_cluster_pkg) {
  cat("  ('cluster' package not installed -> silhouette panel skipped; ",
      "elbow only.)\n", sep = "")
}

# ---- Cluster: hierarchical (Ward.D2) + k-means ----------------------------

d  <- stats::dist(X)
hc <- stats::hclust(d, method = "ward.D2")
cl_hier <- stats::cutree(hc, k = k)

km <- stats::kmeans(X, centers = k, nstart = 25, iter.max = 100)
cl_kmeans <- km$cluster

# Relabel k-means clusters to best-match the hierarchical labels (so the same
# colour ~ same group across both maps where possible). Greedy by overlap.
relabel_to_match <- function(reference, target) {
  tab <- table(target, reference)
  mapping <- integer(0)
  used_ref <- integer(0)
  # assign each target label to the reference label it overlaps most
  for (t in rownames(tab)) {
    ord <- order(tab[t, ], decreasing = TRUE)
    ref_choice <- NA_integer_
    for (j in ord) {
      cand <- as.integer(colnames(tab)[j])
      if (!(cand %in% used_ref)) { ref_choice <- cand; break }
    }
    if (is.na(ref_choice)) ref_choice <- as.integer(colnames(tab)[ord[1]])
    mapping[t] <- ref_choice
    used_ref <- c(used_ref, ref_choice)
  }
  mapping[as.character(target)]
}
cl_kmeans <- relabel_to_match(cl_hier, cl_kmeans)

dat$cluster_hier   <- cl_hier
dat$cluster_kmeans <- cl_kmeans

agree <- mean(cl_hier == cl_kmeans) * 100
cat(sprintf("\nHierarchical vs k-means label agreement: %.0f%%\n", agree))

# ---- Per-cluster diagnostic (mean metric per cluster) ---------------------

# Report on the ORIGINAL (untransformed) metric values, for the hierarchical
# labelling (the primary one). Includes mean lon/lat as cluster "location".
summarise_clusters <- function(labels, label_name) {
  groups <- sort(unique(labels))
  summ <- data.frame(cluster = groups)
  summ$n <- vapply(groups, function(g) sum(labels == g), integer(1))
  for (m in metric_cols) {
    summ[[m]] <- vapply(groups, function(g)
      mean(dat[[m]][labels == g]), numeric(1))
  }
  summ$lon <- vapply(groups, function(g) mean(dat$lon[labels == g]), numeric(1))
  summ$lat <- vapply(groups, function(g) mean(dat$lat[labels == g]), numeric(1))
  cat("\n--- Cluster means (", label_name, ", original metric values) ---\n",
      sep = "")
  print(format(summ, digits = 3), row.names = FALSE)
  summ
}

summ_hier <- summarise_clusters(cl_hier,   "hierarchical")
summ_km   <- summarise_clusters(cl_kmeans, "k-means")

# ---- What drives the clusters? --------------------------------------------
# Uses the hierarchical labels (the primary labelling) on the SAME standardized
# feature matrix X that the clustering saw, so the answer reflects the actual
# basis of separation (metrics + weighted coords).

drive_labels <- cl_hier
feat_names <- colnames(X)

# (a) Per-cluster mean z-score for each feature. A feature far from 0 is what
#     makes that cluster distinctive: + = above sample average, - = below.
z_by_cluster <- t(vapply(sort(unique(drive_labels)), function(g)
  colMeans(X[drive_labels == g, , drop = FALSE]), numeric(ncol(X))))
rownames(z_by_cluster) <- paste0("C", sort(unique(drive_labels)))
colnames(z_by_cluster) <- feat_names

cat("\n--- Per-cluster mean z-scores (what makes each cluster distinctive) ---\n")
cat("    (+ = above sample mean, - = below; |z| > ~0.8 is a strong driver)\n")
print(round(z_by_cluster, 2))

# (b) Discriminative power per feature: one-way F = between-group var /
#     within-group var. Higher F => that feature separates clusters more.
f_stat <- vapply(feat_names, function(f) {
  fit <- stats::aov(X[, f] ~ factor(drive_labels))
  s <- summary(fit)[[1]]
  s[["F value"]][1]
}, numeric(1))
f_rank <- sort(f_stat, decreasing = TRUE)

cat("\n--- Feature discriminative power (one-way F across clusters) ---\n")
cat("    (larger F => feature drives the clustering more)\n")
print(round(f_rank, 1))

# (c) How much is geography vs performance? Sum the F-statistics by group.
coord_feats <- intersect(c("lon_z", "lat_z"), feat_names)
metric_feats <- setdiff(feat_names, coord_feats)
if (length(coord_feats) > 0) {
  share_geo <- sum(f_stat[coord_feats]) / sum(f_stat) * 100
  cat(sprintf("\nGeography accounts for ~%.0f%% of total cluster separation; ",
              share_geo))
  cat(sprintf("performance metrics ~%.0f%% (coord_weight = %.2f).\n",
              100 - share_geo, coord_weight))
}

# Save the driver tables
drivers_path <- file.path(out_dir, "cluster_drivers.csv")
drivers_out <- data.frame(cluster = rownames(z_by_cluster),
                          round(z_by_cluster, 4), check.names = FALSE)
utils::write.csv(drivers_out, drivers_path, row.names = FALSE)
cat("Wrote cluster drivers (z-scores): ", drivers_path, "\n", sep = "")

# Visual: grouped barplot of per-cluster z-scores, one panel per cluster
drivers_png <- file.path(out_dir, "cluster_drivers.png")
ng <- nrow(z_by_cluster)
grDevices::png(drivers_png, width = 1100, height = 280 * ceiling(ng / 2),
               res = 110)
op <- graphics::par(mfrow = c(ceiling(ng / 2), 2), mar = c(5, 4, 2.5, 1))
bar_cols <- ifelse(feat_names %in% coord_feats, "grey60", "steelblue")
grp_ids <- sort(unique(drive_labels))
for (i in seq_len(ng)) {
  graphics::barplot(z_by_cluster[i, ], names.arg = feat_names,
                    col = bar_cols, ylim = range(z_by_cluster) * 1.1,
                    main = paste0(rownames(z_by_cluster)[i],
                                  "  (n = ", sum(drive_labels == grp_ids[i]), ")"),
                    ylab = "mean z-score", las = 2, cex.names = 0.8)
  graphics::abline(h = 0, col = "black")
}
graphics::par(op)
grDevices::dev.off()
cat("Wrote cluster driver plot: ", drivers_png, "\n", sep = "")

# Visual: overall feature importance (F-statistic) + a z-score heatmap so the
# whole picture is in one figure.
importance_png <- file.path(out_dir, "cluster_feature_importance.png")
grDevices::png(importance_png, width = 1150, height = 520, res = 110)
op <- graphics::par(mfrow = c(1, 2), mar = c(7, 4, 3, 1))

# Left: F-statistic ranking (how strongly each feature separates clusters)
imp_cols <- ifelse(names(f_rank) %in% coord_feats, "grey60", "steelblue")
graphics::barplot(f_rank, col = imp_cols, las = 2,
                  ylab = "F (between/within var)",
                  main = "Feature discriminative power")
graphics::legend("topright", fill = c("steelblue", "grey60"),
                 legend = c("performance", "geography"), bty = "n", cex = 0.85)

# Right: z-score heatmap (clusters x features)
graphics::par(mar = c(7, 4, 3, 3))
zlim <- max(abs(z_by_cluster))
heat_cols <- grDevices::hcl.colors(21, "Blue-Red 3")
graphics::image(x = seq_len(ncol(z_by_cluster)),
                y = seq_len(nrow(z_by_cluster)),
                z = t(z_by_cluster), col = heat_cols,
                zlim = c(-zlim, zlim), axes = FALSE,
                xlab = "", ylab = "", main = "Cluster z-score signature")
graphics::axis(1, at = seq_len(ncol(z_by_cluster)),
               labels = colnames(z_by_cluster), las = 2, cex.axis = 0.8)
graphics::axis(2, at = seq_len(nrow(z_by_cluster)),
               labels = rownames(z_by_cluster), las = 1)
# annotate each cell with its value
for (rr in seq_len(nrow(z_by_cluster)))
  for (cc in seq_len(ncol(z_by_cluster)))
    graphics::text(cc, rr, sprintf("%.1f", z_by_cluster[rr, cc]), cex = 0.7)
graphics::box()
graphics::par(op)
grDevices::dev.off()
cat("Wrote feature-importance plot: ", importance_png, "\n", sep = "")

# ---- Write labelled CSV ---------------------------------------------------

labelled_path <- file.path(out_dir, "station_metrics_clustered.csv")
utils::write.csv(dat, labelled_path, row.names = FALSE)
cat("\nWrote labelled metrics: ", labelled_path, "\n", sep = "")

summ_path <- file.path(out_dir, "cluster_summary.csv")
summ_hier_out <- cbind(method = "hierarchical", summ_hier)
summ_km_out   <- cbind(method = "kmeans",       summ_km)
utils::write.csv(rbind(summ_hier_out, summ_km_out), summ_path, row.names = FALSE)
cat("Wrote cluster summary: ", summ_path, "\n", sep = "")

# ---- Map-style scatter (lon/lat coloured by cluster) ----------------------

palette_k <- grDevices::hcl.colors(k, "Dark 3")

map_path <- file.path(out_dir, "cluster_map.png")
grDevices::png(map_path, width = 1200, height = 620, res = 110)
op <- graphics::par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.5, 1))
for (lab in list(list(cl = cl_hier,   t = "Hierarchical (Ward.D2)"),
                 list(cl = cl_kmeans, t = "k-means"))) {
  plot(dat$lon, dat$lat, col = palette_k[lab$cl], pch = 19, cex = 1.1,
       xlab = "Longitude", ylab = "Latitude",
       main = lab$t, asp = 1)
  graphics::legend("topright", legend = paste("Cluster", sort(unique(lab$cl))),
                   col = palette_k[sort(unique(lab$cl))], pch = 19,
                   bty = "n", cex = 0.9)
}
graphics::par(op)
grDevices::dev.off()
cat("Wrote cluster map: ", map_path, "\n", sep = "")

cat("\nDone. Outputs in: ", out_dir, "\n", sep = "")
cat("Review the diagnostic plot, adjust k / coord_weight / ab_treatment ",
    "at the top, and rerun as needed.\n", sep = "")
