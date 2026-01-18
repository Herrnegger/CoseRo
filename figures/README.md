# Figures Directory

This directory should contain the 10 PNG figures referenced in `sensitivity_report.qmd`.

## Required Figures

Place your sensitivity analysis figures here with the following names:

### Sobol Sensitivity Analysis Figures
1. **01_sobol_indices_nse.png** - Sobol sensitivity indices for NSE with first-order and total-order effects
2. **02_sobol_indices_kge.png** - Sobol sensitivity indices for KGE
3. **09_sobol_nse_kge_comparison.png** - Side-by-side comparison of NSE and KGE sensitivity indices

### Dotty Plots (Parameter vs Performance)
4. **03_dotty_plots_nse.png** - 9-panel dotty plots showing all parameters vs NSE
5. **04_dotty_plots_kge.png** - 9-panel dotty plots showing all parameters vs KGE

### Best Simulation Highlights
6. **05_best_nse_dotty.png** - Best NSE simulation highlighted in red on dotty plots
7. **06_best_kge_dotty.png** - Best KGE simulation highlighted in green on dotty plots

### Performance Comparisons
8. **07_nse_vs_kge_comparison.png** - Scatter plot of NSE vs KGE across all simulations
9. **08_parameter_correlations_nse.png** - Correlation heatmap or bar chart of parameters with NSE

### Behavioral Parameter Analysis
10. **10_behavioral_parameter_ranges.png** - Visualization of behavioral parameter ranges (NSE > 0.5)

## Current Status

**TODO:** Copy your existing PNG files from their current location to this directory.

If your figures are located elsewhere, you can either:

### Option 1: Copy Files Here
```bash
# Example: Copy from another directory
cp /path/to/your/figures/*.png "d:\CORSERO PRIVATE\COSERO-R\figures/"
```

### Option 2: Update Report to Point to Your Location
Edit `sensitivity_report.qmd` and change the figure paths from:
```markdown
knitr::include_graphics("figures/01_sobol_indices_nse.png")
```
to:
```markdown
knitr::include_graphics("path/to/your/figures/01_sobol_indices_nse.png")
```

## Figure Specifications

For best quality in the report:
- **Format:** PNG (preferred) or PDF
- **Resolution:** 300 DPI minimum for publication
- **Width:** 8-12 inches recommended
- **Height:** 6-10 inches recommended (depending on layout)

## Generating Figures

If you need to regenerate these figures, use the R scripts from your sensitivity analysis workflow:
- `workflow_2_generate_results.R`
- Or the plotting functions from the COSERO-R package

---
Last updated: 2026-01-10
