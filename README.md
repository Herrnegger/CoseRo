# COSERO: R Interface and Shiny App for COSERO Hydrological Model

<!-- badges: start -->
[![R build status](https://img.shields.io/badge/R-package-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

**COSERO** provides a complete R interface for the COSERO hydrological model, including:

- ✅ **Automated Model Execution**: Run COSERO simulations programmatically from R scripts
- ✅ **Fast Output Reading**: Efficiently read and parse 13+ different COSERO output file formats
- ✅ **Interactive Visualization**: Modern Shiny web application with 5 analysis tabs
- ✅ **Sensitivity Analysis**: Sobol-based global sensitivity analysis framework with parallel computing
- ✅ **Flexible Usage**: Use as scripting library OR interactive GUI

## Important Notice

**⚠️ SENSITIVITY ANALYSIS FUNCTIONALITY**

The sensitivity analysis features (Sobol-based global sensitivity analysis) are currently under development and have not been fully tested. While the core implementation is functional, it requires comprehensive validation before use in production environments.

**Status:**
- Parameter bounds and Sobol sampling: Implemented
- Ensemble execution (sequential and parallel): Implemented
- Parameter modification for tabular and simple formats: Implemented
- Sensitivity indices calculation and visualization: Implemented
- Comprehensive testing: **PENDING**

**Recommendation:** If you plan to use the sensitivity analysis functionality, please conduct thorough testing with your specific COSERO setup and validate results carefully. Contributions and feedback are welcome!

## Installation

### Option 1: For Package Developers (Recommended)

If you're developing or modifying the package, load it directly from source:

```r
# Install devtools if you don't have it
install.packages("devtools")

# Load all functions from your package for development
devtools::load_all()

# Now you can use all COSERO functions without installing
launch_cosero_app()
```

This loads your package directly from the source code without installing it, making development faster.

### Option 2: Install as Regular Package

For regular use, install the package locally:

```r
# Install from source directory
devtools::install()

# Now you can use library
library(COSERO)
launch_cosero_app()
```

### Option 3: Install from GitHub

Once published, install directly from GitHub:

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install COSERO package
remotes::install_github("yourusername/COSERO-R")
```

### System Requirements

- **R**: Version 4.0.0 or higher
- **COSERO.exe**: Windows executable for the COSERO hydrological model
- **Operating System**: Windows (required for COSERO.exe execution)
- **Required R Packages**: See DESCRIPTION file for full list (automatically installed)
  - Core: shiny, bslib, plotly, dplyr, tidyr, lubridate, data.table
  - Analysis: sensobol, hydroGOF, parallel, pbapply
  - Visualization: ggplot2, DT

## Usage

### 1. Interactive Shiny App (Recommended for Beginners)

Launch the interactive web application:

```r
library(COSERO)

# Launch the app in your default browser
launch_cosero_app()
```

The Shiny app provides five interactive analysis tabs:

**1. COSERO Run Tab**
- Browse and select COSERO project directory
- Configure simulation settings (dates, spin-up, output type)
- Choose state variable source (cold start vs warm start from statevar.dmp)
- Select monthly temperature option (from file or calculated)
- Execute model runs with one click
- View execution log and runtime statistics

**2. Time Series Tab**
- Interactive plotly graphs for discharge (Q_obs vs Q_sim)
- Precipitation components (rain vs snow)
- Runoff components (surface, subsurface, baseflow, glacier melt)
- Water balance variables (soil moisture, groundwater, snow water equivalent)
- Zoom, pan, and hover for detailed values
- Date range filtering

**3. Seasonality Tab**
- Monthly aggregated analysis
- Compare observed vs simulated patterns
- Precipitation and evapotranspiration monthly totals
- Visualize seasonal water balance
- Identify model performance by season

**4. Statistics Tab**
- Comprehensive performance metrics table
- NSE (Nash-Sutcliffe Efficiency)
- KGE (Kling-Gupta Efficiency)
- BIAS (Bias percentage)
- RMSE (Root Mean Square Error)
- Correlation coefficient and other metrics
- Per-subbasin statistics

**5. Export Tab**
- Download interactive plots as PNG
- Export time series data as CSV
- Export statistics tables
- Batch export functionality

### 2. Scripting with R Functions (For Advanced Users)

#### Run COSERO Model

```r
library(COSERO)

# Basic run with defaults (reads existing defaults.txt)
result <- run_cosero(
  project_path = "path/to/cosero/project"
)

# Run with custom settings
result <- run_cosero(
  project_path = "path/to/project",
  defaults_settings = list(
    STARTDATE = "2015 1 1 0 0",
    ENDDATE = "2015 12 31 23 59",
    SPINUP = 365,
    OUTPUTTYPE = 3  # 1=basic, 2=+glacier/met, 3=+monitor/longterm
  ),
  statevar_source = 1,  # 1 = cold start (from para.txt), 2 = warm start (from statevar.dmp)
  tmmon_option = 1,     # 1 = use TMMon from para.txt, 2 = calculate from input data
  read_outputs = TRUE,  # Automatically read all outputs after successful run
  quiet = FALSE         # Show progress messages
)

# Check results
print(result$success)          # TRUE if simulation succeeded
print(result$runtime_seconds)  # Execution time
print(result$exit_code)        # 0 = success

# Access output data directly
discharge <- result$output_data$runoff
statistics <- result$output_data$statistics

# Warm start example (use state from previous run)
# COSERO.exe saves statevar.dmp automatically, enabling warm start
result2 <- run_cosero(
  project_path = "path/to/project",
  defaults_settings = list(
    STARTDATE = "2016 1 1 0 0",
    ENDDATE = "2016 12 31 23 59"
  ),
  statevar_source = 2  # Use saved state from previous simulation
)
```

#### Read COSERO Outputs

The package can read 13+ different COSERO output file formats automatically based on OUTPUTTYPE:

```r
# Read all available output files (auto-detects OUTPUTTYPE)
output <- read_cosero_output(
  output_dir = "path/to/project/output",
  defaults_file = "path/to/project/input/defaults.txt"
)

# Access different data types
discharge <- output$runoff              # COSERO.runoff (QOBS, QSIM, Qloc)
precipitation <- output$precipitation   # COSERO.prec (PRAIN, PSNOW by subbasin)
runoff_components <- output$runoff_components  # COSERO.plus (QAB123, QAB23, QAB3, glacmelt)
water_balance <- output$water_balance   # COSERO.plus1 (BW0, BW3, SWW, cumulative P/ET/Q)
statistics <- output$statistics         # statistics.txt (NSE, KGE, BIAS, RMSE, etc.)
topology <- output$topology             # topology.txt (subbasin structure)

# OUTPUTTYPE 2 files (if available)
glacier <- output$glacier               # var_glac.txt
meteorology <- output$meteorology       # var_MET.txt

# OUTPUTTYPE 3 files (if available)
monitor <- output$monitor               # monitor.txt (catchment-wide variables)
monitor_sb <- output$monitor_subbasins  # monitor_sb*.txt (per-subbasin detailed outputs)
rundepth <- output$rundepth             # rundepth.txt (runoff depth)
longterm_annual <- output$longterm_annual     # long-term_annual_means.txt
longterm_seasonal <- output$longterm_seasonal # long-term_seasonal_means.txt

# Access metadata
output$metadata$outputtype              # Detected OUTPUTTYPE (1, 2, or 3)
output$metadata$subbasins               # List of available subbasins

# Get specific subbasin data
subbasin_data <- get_subbasin_data(output$runoff, subbasin_id = "0001")
# Returns data frame with DateTime, Date, Q_obs, Q_sim columns

# List all available subbasins
list_subbasins(output$runoff)

# Get parameters for specific subbasin (if parameter file was read)
if (!is.null(output$parameters)) {
  sb_params <- get_subbasin_parameters(output$parameters, subbasin_id = 1)
}
```

#### Sensitivity Analysis

**Note:** This functionality is still under testing. See Important Notice above.

The package provides a complete framework for Sobol-based global sensitivity analysis:

```r
# 1. Define parameter bounds
# Option A: Load from package CSV (30 pre-defined parameters)
bounds <- load_parameter_bounds()

# Option B: Load specific parameters only
bounds <- load_parameter_bounds(
  parameters = c("BETA", "CTMAX", "FK", "M")
)

# Option C: Create custom bounds manually
# Note: All parameter modifications preserve spatial patterns
# - relchg: multiply original values (preserves relative differences)
# - abschg: add to original values (preserves absolute differences)
custom_bounds <- create_custom_bounds(
  parameter = c("BETA", "FK", "TCOR"),
  min = c(0.1, 0.5, -2),        # Physical bounds
  max = c(10, 2.0, 2),          # Physical bounds
  default = c(1.0, 1.0, 0),     # Default values
  modification_type = c("relchg", "relchg", "abschg"),  # How to modify
  sample_min = c(0.5, 0.8, -1), # Sampling range for Sobol (optional)
  sample_max = c(2.0, 1.5, 1)   # Sampling range for Sobol (optional)
)

# 2. Generate Sobol samples
# Creates N * (2 + n_params) parameter sets
sobol_bounds <- create_sobol_bounds(bounds)
samples <- generate_sobol_samples(sobol_bounds, n = 100)  # 100 * (2 + n_params) runs

# 3. Run ensemble simulations
# Option A: Parallel execution (recommended, much faster)
ensemble_result <- run_cosero_ensemble_parallel(
  project_path = "path/to/project",
  parameter_sets = samples$parameter_sets,
  par_bounds = bounds,
  base_settings = list(STARTDATE = "2015 1 1 0 0",
                       ENDDATE = "2015 12 31 23 59"),
  n_cores = 4  # Use NULL to auto-detect
)

# Option B: Sequential execution (for debugging)
ensemble_result <- run_cosero_ensemble(
  project_path = "path/to/project",
  parameter_sets = samples$parameter_sets,
  par_bounds = bounds
)

# 4. Extract performance metrics
# Method 1: Use COSERO's pre-calculated metrics (RECOMMENDED)
# This is faster and uses spin-up correctly
Y <- extract_cosero_metrics(
  ensemble_result,
  subbasin_id = "001",
  metric = "KGE"  # NSE, KGE, BIAS, RMSE, etc.
)

# Method 2: Calculate custom metrics (for metrics not in COSERO output)
# Y_custom <- calculate_ensemble_metrics(
#   ensemble_result,
#   subbasin_id = "001",
#   metric = "PBIAS",
#   spinup = NULL  # Auto-read from defaults_settings
# )

# 5. Calculate Sobol sensitivity indices
sobol_indices <- calculate_sobol_indices(Y, samples, boot = TRUE, R = 100)

# 6. Visualize results
# Main sensitivity plot
plot_sobol(sobol_indices)

# Parameter scatter plots (dotty plots)
plot_dotty(samples$parameter_sets, Y, y_label = "KGE")

# 7. Export results
export_sensitivity_results(
  output_dir = "sensitivity_results",
  sobol_indices = sobol_indices,
  parameter_sets = samples$parameter_sets,
  metrics = Y,
  prefix = "cosero_sa"
)
```

**Key Features:**
- Supports both simple and tabular parameter file formats
- Handles monthly parameters (e.g., TCor1-TCor12) automatically
- Preserves spatial patterns in distributed parameters
- Parallel execution with progress tracking
- Works with all COSERO performance metrics (NSE, KGE, BIAS, RMSE, etc.)

### 3. Understanding COSERO Output Types

COSERO generates different sets of output files based on the OUTPUTTYPE setting:

**OUTPUTTYPE 1 (Basic Output)**
Essential files for model evaluation:
- `COSERO.runoff` - Discharge time series (QOBS, QSIM, Qloc per subbasin)
- `COSERO.prec` - Precipitation by subbasin (PRAIN, PSNOW)
- `COSERO.plus` - Runoff components (QAB123, QAB23, QAB3, glacmelt)
- `COSERO.plus1` - Water balance (BW0, BW3, SWW, cumulative P/ET/Q)
- `statistics.txt` - Performance metrics (NSE, KGE, BIAS, RMSE, etc.)
- `topology.txt` - Subbasin structure and connectivity

**OUTPUTTYPE 2 (+ Glacier and Meteorology)**
All OUTPUTTYPE 1 files plus:
- `var_glac.txt` - Glacier variables (if glaciers present)
- `var_MET.txt` - Meteorological variables by zone

**OUTPUTTYPE 3 (+ Monitoring and Long-term Analysis)**
All OUTPUTTYPE 2 files plus:
- `monitor.txt` - Catchment-wide state variables
- `monitor_sb*.txt` - Detailed per-subbasin state variables
- `rundepth.txt` - Runoff depth time series
- `long-term_annual_means.txt` - Annual averages
- `long-term_seasonal_means.txt` - Seasonal averages

**Recommendation:** Use OUTPUTTYPE 3 for comprehensive analysis, OUTPUTTYPE 1 for quick model evaluation.

## Package Structure

```
COSERO/
├── R/                          # Core R functions
│   ├── cosero_run.R           # run_cosero() and execution functions
│   ├── cosero_readers.R       # read_cosero_output() and readers
│   ├── sensitivity_analysis.R # Sensitivity analysis framework
│   ├── app_helpers.R          # Plotting and data processing
│   └── launch_app.R           # launch_cosero_app()
│
├── inst/
│   ├── shiny-app/             # Shiny application
│   │   └── app.R              # Main Shiny app
│   └── extdata/               # Data files
│       └── parameter_bounds.csv
│
├── tests/                      # Test suite
│   └── testthat/              # testthat tests
│
└── man/                        # Documentation (auto-generated)
```

## Key Functions

### Model Execution
| Function | Description |
|----------|-------------|
| `run_cosero()` | Execute COSERO model with custom settings, warm/cold start options |
| `launch_cosero_app()` | Launch interactive Shiny application for model setup and visualization |

### Output Reading
| Function | Description |
|----------|-------------|
| `read_cosero_output()` | Read and parse all COSERO output files (auto-detects OUTPUTTYPE 1-3) |
| `get_subbasin_data()` | Extract discharge data for specific subbasin |
| `list_subbasins()` | List all available subbasins in output |
| `get_subbasin_parameters()` | Extract parameters for specific subbasin |
| `read_cosero_parameters()` | Read parameter file (para.txt) with zone/subbasin structure |

### Configuration Management
| Function | Description |
|----------|-------------|
| `read_defaults()` | Read defaults.txt configuration file |
| `modify_defaults()` | Modify defaults.txt with new settings |
| `validate_cosero_defaults()` | Validate configuration parameters |
| `show_cosero_defaults()` | Display available configuration parameters |

### Sensitivity Analysis (Under Testing)
| Function | Description |
|----------|-------------|
| `load_parameter_bounds()` | Load parameter bounds from CSV or create custom bounds |
| `create_custom_bounds()` | Create custom parameter bounds manually |
| `generate_sobol_samples()` | Generate Sobol parameter sets for sensitivity analysis |
| `run_cosero_ensemble()` | Run ensemble simulations (sequential execution) |
| `run_cosero_ensemble_parallel()` | Run ensemble simulations (parallel execution with progress) |
| `extract_cosero_metrics()` | Extract pre-calculated metrics from COSERO statistics |
| `calculate_ensemble_metrics()` | Calculate custom metrics from ensemble results |
| `calculate_sobol_indices()` | Calculate Sobol sensitivity indices with bootstrap |
| `plot_sobol()` | Visualize Sobol sensitivity indices |
| `plot_dotty()` | Create parameter scatter plots (dotty plots) |
| `export_sensitivity_results()` | Export sensitivity analysis results to files |

## Documentation

### Built-in Help

Access detailed function documentation from R:

```r
# Main functions
?run_cosero                    # Model execution
?read_cosero_output           # Output reading
?launch_cosero_app            # Shiny app

# Configuration
?read_defaults                # Read configuration
?modify_defaults              # Modify configuration
?validate_cosero_defaults     # Validate settings

# Data extraction
?get_subbasin_data           # Extract subbasin discharge
?list_subbasins              # List available subbasins
?read_cosero_parameters      # Read parameter file

# Sensitivity analysis
?load_parameter_bounds       # Load parameter bounds
?generate_sobol_samples      # Generate Sobol samples
?run_cosero_ensemble_parallel # Run parallel ensemble
?extract_cosero_metrics      # Extract metrics
?calculate_sobol_indices     # Calculate indices
```

### Quick Start Guide

1. **First Time Setup**
```r
# Install package
devtools::install()
library(COSERO)

# Verify installation
packageVersion("COSERO")
```

2. **Run Single Simulation**
```r
# Launch GUI (easiest)
launch_cosero_app()

# Or use script
result <- run_cosero("path/to/project")
```

3. **Analyze Results**
```r
# Read outputs
output <- read_cosero_output("path/to/project/output",
                             "path/to/project/input/defaults.txt")

# Extract discharge for subbasin 1
sb_data <- get_subbasin_data(output$runoff, subbasin_id = "0001")

# View statistics
print(output$statistics)
```

## Common Use Cases

### Use Case 1: Model Calibration Period Run

```r
# Set up calibration period
result <- run_cosero(
  project_path = "D:/COSERO/MyBasin",
  defaults_settings = list(
    STARTDATE = "2010 1 1 0 0",
    ENDDATE = "2015 12 31 23 59",
    SPINUP = 365,  # 1 year warm-up
    OUTPUTTYPE = 3
  ),
  statevar_source = 1  # Cold start
)

# Check calibration metrics
stats <- result$output_data$statistics
print(stats[, c("sb", "NSE", "KGE")])
```

### Use Case 2: Validation Period Run (Warm Start)

```r
# Run calibration first to generate statevar.dmp
cal_result <- run_cosero(
  project_path = "D:/COSERO/MyBasin",
  defaults_settings = list(
    STARTDATE = "2010 1 1 0 0",
    ENDDATE = "2015 12 31 23 59"
  )
)

# Use warm start for validation
val_result <- run_cosero(
  project_path = "D:/COSERO/MyBasin",
  defaults_settings = list(
    STARTDATE = "2016 1 1 0 0",
    ENDDATE = "2020 12 31 23 59",
    SPINUP = 0  # No spin-up needed with warm start
  ),
  statevar_source = 2  # Use state from calibration
)
```

### Use Case 3: Batch Processing Multiple Scenarios

```r
scenarios <- list(
  baseline = list(STARTDATE = "2010 1 1 0 0", ENDDATE = "2020 12 31 23 59"),
  period1  = list(STARTDATE = "2010 1 1 0 0", ENDDATE = "2015 12 31 23 59"),
  period2  = list(STARTDATE = "2015 1 1 0 0", ENDDATE = "2020 12 31 23 59")
)

results <- list()
for (scenario_name in names(scenarios)) {
  cat("Running scenario:", scenario_name, "\n")
  results[[scenario_name]] <- run_cosero(
    project_path = "D:/COSERO/MyBasin",
    defaults_settings = scenarios[[scenario_name]],
    quiet = TRUE
  )
}

# Compare results
sapply(results, function(r) r$output_data$statistics$NSE[1])
```

## Development

### Running Tests

```r
# Install development dependencies
devtools::install_dev_deps()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-cosero_run.R")

# Check test coverage
covr::package_coverage()
```

### Building Documentation

```r
# Generate documentation from roxygen2 comments
devtools::document()

# Build and check package
devtools::check()
```

## Citation

If you use COSERO-R in your research, please cite:

```
[Add citation information here when published]
```

## Troubleshooting

### Common Issues

**Issue: "COSERO executable not found"**
- Ensure COSERO.exe is in your project directory
- Check that you're using the correct project_path
- Verify the exe_name parameter if using a different filename

**Issue: "Parameter file not found"**
- Check that para.txt exists in the input/ folder
- Verify the PARAFILE setting in defaults.txt
- Ensure file permissions allow reading

**Issue: "Outputs not reading correctly"**
- Verify simulation completed successfully (check result$success)
- Ensure OUTPUTTYPE matches your expectations (1, 2, or 3)
- Check that output/ folder contains the expected files

**Issue: Parallel ensemble fails**
- Verify all workers can access the COSERO executable
- Check available disk space for temporary folders
- Reduce n_cores if memory is limited
- Try sequential execution first for debugging

### Performance Tips

**For Large Ensembles:**
- Use `run_cosero_ensemble_parallel()` instead of sequential
- Set OUTPUTTYPE to 1 if detailed outputs aren't needed
- Consider shorter simulation periods for sensitivity analysis
- Monitor disk space (each run creates temporary files)

**For Long Simulations:**
- Increase SPINUP to ensure model equilibration
- Use warm start (statevar_source = 2) for continued runs
- Set quiet = TRUE to reduce console output overhead

## Issues & Support

**Bug Reports & Feature Requests:**
https://github.com/Herrnegger/COSERO-R/issues

**Questions & Discussions:**
Use GitHub Issues with the "question" label

**Before Reporting:**
1. Check this README for common issues
2. Verify COSERO.exe works standalone
3. Try with a minimal example
4. Include R version and package version

## Contributing

Contributions are welcome! Areas where help is especially appreciated:

- Testing sensitivity analysis functionality
- Additional output file format readers
- Performance optimization for ensemble runs
- Documentation improvements
- Example workflows and use cases

**To Contribute:**
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes with clear commit messages
4. Add tests if applicable
5. Update documentation
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## Acknowledgments

- COSERO hydrological model developers
- R community and package contributors
- sensobol package authors for sensitivity analysis framework
- hydroGOF package authors for performance metrics
- Shiny and plotly teams for visualization tools

---

**Package Maintainer**: COSERO Development Team
**Repository**: https://github.com/Herrnegger/COSERO-R
**License**: MIT
**Last Updated**: 2025-11-13
