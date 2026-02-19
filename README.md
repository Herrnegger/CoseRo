# CoseRo <img src="man/figures/logo.svg" align="right" height="139" alt="CoseRo logo" />

**R Interface and Shiny App for COSERO Hydrological Model**

<!-- badges: start -->

[![R build status](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/) [![License: GPL-3](https://img.shields.io/badge/License-GPL%203-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

<!-- badges: end -->

> **⚠️ Work in Progress** — This package is under active development. Features are functional, but not fully tested. Use with caution and validate results for your specific setup.

## Overview

**CoseRo** provides a complete R interface for the COSERO hydrological model, including:

-   ✅ **Automated Model Execution**: Run COSERO simulations programmatically from R scripts
-   ✅ **Fast Output Reading**: Efficiently read and parse 13+ different COSERO output file formats
-   ✅ **Interactive Visualization**: Modern Shiny web application with 5 analysis tabs
-   ✅ **Parameter Optimization**: DDS and SCE-UA algorithms for automated parameter calibration
-   ✅ **Sensitivity Analysis**: Sobol-based global sensitivity analysis framework with parallel computing
-   ✅ **Input Data Preprocessing**: Convert SPARTACUS gridded climate data to COSERO input format
-   ✅ **Flexible Usage**: Use as scripting library OR interactive GUI

## Installation

### Option 1: For Package Developers (Recommended)

If you're developing or modifying the package, load it directly from source:

``` r
# Install devtools if you don't have it
install.packages("devtools")

# Load all functions from your package for development
devtools::load_all()

# Now you can use all CoseRo functions without installing
launch_cosero_app()
```

### Option 2: Install as Regular Package

``` r
devtools::install()
library(CoseRo)
launch_cosero_app()
```

### Option 3: Install from GitHub

``` r
install.packages("remotes")
remotes::install_github("Herrnegger/CoseRo")
```

### System Requirements

-   **R**: Version 4.0.0 or higher
-   **COSERO.exe**: Windows executable for the COSERO hydrological model
-   **Operating System**: Windows (required for COSERO.exe execution)
-   **Required R Packages**: See DESCRIPTION file for full list (automatically installed)
    -   Core: shiny, bslib, plotly, dplyr, tidyr, lubridate, data.table
    -   Analysis: sensobol, hydroGOF, parallel, pbapply
    -   Visualization: ggplot2, DT
    -   Spatial: terra, sf, exactextractr (for SPARTACUS preprocessing)
    -   Parallel: future, furrr (for parallel data processing)

## Quick Start

``` r
library(CoseRo)

# 1. Create a working example project (Wildalpen catchment, includes data and binaries)
setup_cosero_project_example("D:/COSERO_example")

# 2. Launch app with auto-loaded data
launch_cosero_app("D:/COSERO_example")
```

## Usage

### 1. Interactive Shiny App (Recommended for Beginners)

``` r
library(CoseRo)

# Launch with a specific project (auto-loads data)
launch_cosero_app("path/to/project")

# Or launch without pre-loading
launch_cosero_app()
```

The Shiny app provides five interactive analysis tabs:

**1. COSERO Run Tab** — Browse project directory, configure simulation settings, choose warm/cold start, execute runs, view log.

**2. Time Series Tab** — Interactive plotly graphs for discharge, precipitation, runoff components, water balance. Zoom, pan, hover for detail.

**3. Seasonality Tab** — Monthly aggregated analysis, observed vs simulated patterns, seasonal water balance.

**4. Statistics Tab** — NSE, KGE, BIAS, RMSE, correlation — per-subbasin performance metrics table.

**5. Export Tab** — Download plots as PNG, time series as CSV, statistics tables.

### 2. Scripting with R Functions

#### Run COSERO Model

``` r
library(CoseRo)

result <- run_cosero(
  project_path = "path/to/project",
  defaults_settings = list(
    STARTDATE = "2015 1 1 0 0",
    ENDDATE   = "2015 12 31 23 59",
    SPINUP    = 365,
    OUTPUTTYPE = 3  # 1=basic, 2=+glacier/met, 3=+monitor/longterm
  ),
  statevar_source = 1,  # 1 = cold start, 2 = warm start (statevar.dmp)
  read_outputs    = TRUE
)

print(result$success)
discharge  <- result$output_data$runoff
statistics <- result$output_data$statistics

# Warm start example
result2 <- run_cosero(
  project_path    = "path/to/project",
  defaults_settings = list(STARTDATE = "2016 1 1 0 0", ENDDATE = "2016 12 31 23 59"),
  statevar_source = 2  # Use saved state from previous run
)
```

#### Read COSERO Outputs

``` r
# Read all available output files (auto-detects OUTPUTTYPE)
output <- read_cosero_output(
  output_dir    = "path/to/project/output",
  defaults_file = "path/to/project/input/defaults.txt"
)

discharge        <- output$runoff              # COSERO.runoff
precipitation    <- output$precipitation       # COSERO.prec
runoff_comps     <- output$runoff_components   # COSERO.plus
water_balance    <- output$water_balance       # COSERO.plus1
statistics       <- output$statistics          # statistics.txt
topology         <- output$topology            # topology.txt
# OUTPUTTYPE 2+
glacier          <- output$glacier             # var_glac.txt
meteorology      <- output$meteorology         # var_MET.txt
# OUTPUTTYPE 3
monitor          <- output$monitor             # monitor.txt
longterm_annual  <- output$longterm_annual     # long-term_annual_means.txt

# Extract metrics from a single run
metrics <- extract_run_metrics(result, subbasin_id = "001", metric = "NSE")
metrics <- calculate_run_metrics(result, subbasin_id = "001", metric = "KGE")

# Subbasin helpers
subbasin_data <- get_subbasin_data(output$runoff, subbasin_id = "0001")
list_subbasins(output$runoff)
```

#### Parameter Optimization

``` r
library(CoseRo)

# Define parameter bounds
par_bounds <- create_optimization_bounds(
  parameters        = c("BETA", "CTMAX", "M", "TAB1"),
  lower             = c(1, 2, 20, 1),
  upper             = c(6, 8, 600, 50),
  modification_type = rep("relchg", 4)
)
# Or load from bundled CSV (30 pre-defined parameters)
par_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "M", "TAB1"))

# DDS optimization (fast, recommended for 3-10 parameters)
result_dds <- optimize_cosero_dds(
  cosero_path       = "D:/COSERO_project",
  par_bounds        = par_bounds,
  target_subbasins  = "001",          # Only zones in this subbasin are modified
  metric            = "NSE",
  defaults_settings = list(SPINUP = 365, OUTPUTTYPE = 1),
  max_iter          = 2000
)
# Optimized file auto-saved: input/para_optimized_NB1_NSE_<timestamp>.txt

# SCE-UA optimization (more robust, slower)
result_sce <- optimize_cosero_sce(
  cosero_path      = "D:/COSERO_project",
  par_bounds       = par_bounds,
  target_subbasins = "001",
  metric           = "KGE",
  maxn             = 5000,
  ngs              = 3
)

# Multi-objective (60% NSE + 40% KGE)
result_multi <- optimize_cosero_dds(
  cosero_path      = "D:/COSERO_project",
  par_bounds       = par_bounds,
  target_subbasins = "001",
  metric           = c("NSE", "KGE"),
  metric_weights   = c(0.6, 0.4),
  max_iter         = 2000
)

# Visualize and export
plot_cosero_optimization(result_dds)
export_cosero_optimization(result_dds, "D:/optimization_results")
```

**DDS vs SCE-UA:** DDS is fast and efficient for most calibrations (3-10 parameters). SCE-UA is more robust for complex problems but requires more evaluations. The original parameter file is never modified — it is backed up and restored automatically.

#### Sensitivity Analysis

``` r
# 1. Load parameter bounds
bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "FK", "M"))

# 2. Generate Sobol samples
sobol_bounds <- create_sobol_bounds(bounds)
samples      <- generate_sobol_samples(sobol_bounds, n = 100)

# 3. Run ensemble (parallel recommended)
ensemble_result <- run_cosero_ensemble_parallel(
  project_path    = "path/to/project",
  parameter_sets  = samples$parameter_sets,
  par_bounds      = bounds,
  base_settings   = list(STARTDATE = "2015 1 1 0 0", ENDDATE = "2015 12 31 23 59"),
  n_cores         = 4
)

# 4. Extract metrics and calculate indices
Y             <- extract_ensemble_metrics(ensemble_result, subbasin_id = "001", metric = "KGE")
sobol_indices <- calculate_sobol_indices(Y, samples, boot = TRUE, R = 100)

# 5. Visualize and export
plot_sobol(sobol_indices)
plot_dotty(samples$parameter_sets, Y, y_label = "KGE")
export_sensitivity_results("sensitivity_results", sobol_indices, samples$parameter_sets, Y)
```

#### SPARTACUS Data Preprocessing

``` r
library(CoseRo)
library(sf)

zones <- st_read("path/to/model_zones.shp")

# Precipitation: SPARTACUS2-DAILY_RR_YYYY.nc → P_NZ_<years>.txt
write_spartacus_precip(
  nc_dir     = "data/SPARTACUS_Daily/RR",
  output_dir = "output/cosero_input",
  model_zones = zones,
  nz_col     = "NZ",
  n_cores    = 4
)

# Temperature: TN + TX → Tmean → T_NZ_<years>.txt
write_spartacus_temp(
  tmin_dir    = "data/SPARTACUS_Daily/TN",
  tmax_dir    = "data/SPARTACUS_Daily/TX",
  output_dir  = "output/cosero_input",
  model_zones = zones,
  nz_col      = "NZ",
  tmean_method = "dall_amico",  # Recommended for Alpine catchments
  n_cores     = 4
)
```

**Tmean methods:** `"simple"` (weighted average), `"dall_amico"` (day-length adjusted, recommended for Alps), `"parton_logan"` (full diurnal simulation). The simple arithmetic mean overestimates Tmean by 0.5–2°C.

**SPARTACUS Dataset (GeoSphere Austria):** 1 km daily gridded data for Austria (1961–present), DOI: <https://doi.org/10.60669/m6w8-s545>.

### 3. Understanding COSERO Output Types

**OUTPUTTYPE 1** — `COSERO.runoff`, `COSERO.prec`, `COSERO.plus`, `COSERO.plus1`, `statistics.txt`, `topology.txt`

**OUTPUTTYPE 2** — All OUTPUTTYPE 1 files + `var_glac.txt`, `var_MET.txt`

**OUTPUTTYPE 3** — All OUTPUTTYPE 2 files + `monitor.txt`, `monitor_sb*.txt`, `rundepth.txt`, `long-term_annual_means.txt`, `long-term_seasonal_means.txt`

## Package Structure

```
CoseRo/
├── R/
│   ├── setup_project.R           # Project setup
│   ├── cosero_run.R              # Model execution
│   ├── cosero_readers.R          # Output file readers
│   ├── cosero_optimize.R         # DDS and SCE-UA optimization
│   ├── sensitivity_analysis.R    # Sobol sensitivity analysis
│   ├── spartacus_preprocessing.R # SPARTACUS NetCDF → COSERO input
│   ├── app_helpers.R             # Plotting & data processing
│   └── launch_app.R              # App launcher
├── inst/
│   ├── shiny-app/app.R           # Interactive Shiny application
│   └── extdata/
│       ├── COSERO_Wildalpen.zip  # Example project
│       └── parameter_bounds.csv  # Default parameter bounds (30 parameters)
└── man/                          # Auto-generated documentation
```

## Key Functions

### Project Setup

| Function | Description |
|---|---|
| `setup_cosero_project_example()` | Create ready-to-run example project (Wildalpen catchment) |
| `setup_cosero_project()` | Create empty project structure with binaries |
| `show_required_files()` | Display checklist of required COSERO input files |

### Model Execution

| Function | Description |
|---|---|
| `run_cosero()` | Execute COSERO with custom settings and warm/cold start |
| `launch_cosero_app()` | Launch interactive Shiny app |
| `extract_run_metrics()` | Extract performance metrics from a single run result |
| `calculate_run_metrics()` | Calculate metrics by comparing QSIM vs QOBS for a single run |

### Output Reading

| Function | Description |
|---|---|
| `read_cosero_output()` | Read all COSERO output files (auto-detects OUTPUTTYPE 1–3) |
| `get_subbasin_data()` | Extract discharge data for a specific subbasin |
| `list_subbasins()` | List available subbasins in output |
| `read_cosero_parameters()` | Read parameter file (para.txt) |
| `write_cosero_parameters()` | Write parameter data frame to file |
| `detect_outputtype()` | Detect OUTPUTTYPE by checking which files exist |

### Configuration Management

| Function | Description |
|---|---|
| `read_defaults()` | Read defaults.txt configuration file |
| `show_cosero_defaults()` | Display available configuration parameters |
| `validate_cosero_defaults()` | Validate configuration parameters |

### Parameter Optimization

| Function | Description |
|---|---|
| `optimize_cosero_dds()` | Optimize parameters with DDS (fast, greedy search) |
| `optimize_cosero_sce()` | Optimize parameters with SCE-UA (robust, population-based) |
| `create_optimization_bounds()` | Define parameter bounds for optimization |
| `plot_cosero_optimization()` | Plot optimization convergence history |
| `export_cosero_optimization()` | Export results to CSV (parameters, history, summary) |

### Sensitivity Analysis

| Function | Description |
|---|---|
| `load_parameter_bounds()` | Load parameter bounds from CSV or create custom bounds |
| `create_custom_bounds()` | Create parameter bounds manually |
| `generate_sobol_samples()` | Generate Sobol quasi-random parameter sets |
| `run_cosero_ensemble()` | Run ensemble simulations (sequential) |
| `run_cosero_ensemble_parallel()` | Run ensemble simulations (parallel) |
| `extract_ensemble_metrics()` | Extract metrics from ensemble statistics output |
| `calculate_ensemble_metrics()` | Calculate metrics from ensemble QSIM/QOBS |
| `calculate_sobol_indices()` | Calculate Sobol sensitivity indices with bootstrap |
| `plot_sobol()` | Visualize Sobol sensitivity indices |
| `plot_dotty()` | Parameter scatter plots |
| `export_sensitivity_results()` | Export sensitivity results to files |

### Input Data Preprocessing

| Function | Description |
|---|---|
| `write_spartacus_precip()` | Convert SPARTACUS precipitation NetCDF to COSERO format |
| `write_spartacus_temp()` | Convert SPARTACUS Tmin/Tmax NetCDF to COSERO Tmean format |

## Common Use Cases

### Calibration Period Run

``` r
result <- run_cosero(
  project_path      = "D:/COSERO/MyBasin",
  defaults_settings = list(STARTDATE = "2010 1 1 0 0", ENDDATE = "2015 12 31 23 59",
                           SPINUP = 365, OUTPUTTYPE = 3),
  statevar_source   = 1
)
print(result$output_data$statistics[, c("sb", "NSE", "KGE")])
```

### Warm Start Validation

``` r
# Run calibration to generate statevar.dmp, then:
val_result <- run_cosero(
  project_path      = "D:/COSERO/MyBasin",
  defaults_settings = list(STARTDATE = "2016 1 1 0 0", ENDDATE = "2020 12 31 23 59",
                           SPINUP = 1),
  statevar_source   = 2
)
```

### Parameter Calibration

``` r
par_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "M"))
result     <- optimize_cosero_dds(
  cosero_path      = "D:/COSERO/MyBasin",
  par_bounds       = par_bounds,
  target_subbasins = "001",
  metric           = "NSE",
  max_iter         = 1000
)
print(result$par_bounds[, c("parameter", "default", "optimal_value")])
```

## Building Documentation

``` r
devtools::document()
devtools::check()
```

## Citation

If you use CoseRo in your research, please cite:

```
Herrnegger, M., Fiaz, A., and the COSERO Development Team (2025).
CoseRo: R Interface and Shiny Application for the COSERO Hydrological Model.
R package version 0.1.0. https://github.com/Herrnegger/CoseRo
```

Or in BibTeX format:

```bibtex
@Manual{cosero2025,
  title  = {CoseRo: R Interface and Shiny Application for the COSERO Hydrological Model},
  author = {Herrnegger, Mathew and Fiaz, Ahmed and {COSERO Development Team}},
  year   = {2025},
  note   = {R package version 0.1.0},
  url    = {https://github.com/Herrnegger/CoseRo}
}
```

You can also get the current citation by running `citation("CoseRo")` in R.


## Issues & Support

**Bug Reports & Feature Requests:** <https://github.com/Herrnegger/CoseRo/issues>

## Acknowledgments

-   COSERO hydrological model developers
-   sensobol package authors for the sensitivity analysis framework
-   hydroGOF package authors for performance metrics
-   Shiny and plotly teams for visualization tools

------------------------------------------------------------------------

**Package Maintainer**: Mathew Herrnegger | **Repository**: <https://github.com/Herrnegger/CoseRo> | **License**: GPL-3
