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

## Installation

You can install the development version of COSERO from GitHub:

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

## Usage

### 1. Interactive Shiny App (Recommended for Beginners)

Launch the interactive web application:

```r
library(COSERO)

# Launch the app in your default browser
launch_cosero_app()
```

The Shiny app provides:
- **COSERO Run Tab**: Configure and execute model runs with GUI
- **Time Series Tab**: Interactive discharge, precipitation, and water balance plots
- **Seasonality Tab**: Monthly aggregated analysis
- **Statistics Tab**: Performance metrics (NSE, KGE, RMSE, etc.)
- **Export Tab**: Download plots (PNG) and data (CSV)

### 2. Scripting with R Functions (For Advanced Users)

#### Run COSERO Model

```r
library(COSERO)

# Basic run with defaults
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
    OUTPUTTYPE = 3
  ),
  statevar_source = 1,  # 1 = cold start, 2 = warm start
  read_outputs = TRUE
)

# Check results
print(result$success)
print(result$runtime_seconds)
```

#### Read COSERO Outputs

```r
# Read output files
output <- read_cosero_output(
  output_dir = "path/to/project/output",
  defaults_file = "path/to/project/input/defaults.txt"
)

# Access different data types
discharge <- output$runoff
precipitation <- output$precipitation
water_balance <- output$water_balance
statistics <- output$statistics

# Get specific subbasin data
subbasin_data <- get_subbasin_data(output$runoff, subbasin_id = "0001")
```

#### Sensitivity Analysis

```r
# Load parameter bounds
bounds <- load_parameter_bounds()

# Or load specific parameters
bounds <- load_parameter_bounds(
  parameters = c("BETA", "CTMAX", "FK", "M")
)

# Generate Sobol samples
sobol_bounds <- create_sobol_bounds(bounds)
samples <- generate_sobol_samples(sobol_bounds, n = 100)

# Run ensemble (parallel execution)
ensemble_result <- run_cosero_ensemble_parallel(
  project_path = "path/to/project",
  parameter_sets = samples$parameter_sets,
  par_bounds = bounds,
  n_cores = 4
)

# Calculate sensitivity indices
Y <- calculate_ensemble_metrics(ensemble_result, observed_data, metric = "KGE")
sobol_indices <- calculate_sobol_indices(Y, samples)

# Visualize results
plot_sobol(sobol_indices)
plot_dotty(samples$parameter_sets, Y, y_label = "KGE")
```

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

| Function | Description |
|----------|-------------|
| `launch_cosero_app()` | Launch interactive Shiny application |
| `run_cosero()` | Execute COSERO model with custom settings |
| `read_cosero_output()` | Read and parse COSERO output files |
| `get_subbasin_data()` | Extract data for specific subbasin |
| `load_parameter_bounds()` | Load parameter bounds for sensitivity analysis |
| `generate_sobol_samples()` | Generate Sobol parameter sets |
| `run_cosero_ensemble()` | Run ensemble simulations (sequential) |
| `run_cosero_ensemble_parallel()` | Run ensemble simulations (parallel) |
| `calculate_sobol_indices()` | Calculate Sobol sensitivity indices |

## Documentation

Access function documentation:

```r
?run_cosero
?read_cosero_output
?launch_cosero_app
```

## Examples

See the example files included in the package:
- `03_cosero_execution_examples.R` - Basic execution examples
- `06_cosero_sensitivity_example.R` - Sensitivity analysis workflows

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

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Citation

If you use COSERO-R in your research, please cite:

```
[Add citation information here]
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Issues & Support

Report bugs or request features at: https://github.com/yourusername/COSERO-R/issues

## Acknowledgments

- COSERO hydrological model developers
- R community and package contributors

---

**Maintained by**: [Your Name/Organization]
**Last Updated**: 2025-11-04
