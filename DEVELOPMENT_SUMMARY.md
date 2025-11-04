# COSERO R Interface - Development Summary

## Project Overview

Complete R interface for the COSERO hydrological model, including automated execution, output reading, and interactive visualization.

## Development Timeline

### Phase 1: Core Execution Interface
**File:** `01_cosero_core_run.R`

**Purpose:** Automate COSERO model execution from R with configuration management

**Key Functions:**
- `run_cosero()` - Main execution function
- `read_defaults()` - Parse defaults.txt configuration
- `modify_defaults()` - Update configuration parameters
- `execute_cosero()` - Run COSERO.exe with automated responses

**Key Achievements:**
1. Flexible parameter handling (string or vector format for dates)
2. Automatic backup system for configuration files
3. Parameter validation with cross-checks (e.g., ENDDATE > STARTDATE)
4. State variable management (cold start vs warm start)
5. Monthly temperature handling (from file vs calculated)
6. Error detection in COSERO output
7. Runtime tracking

**Input Parameters:**
- `project_path` - Path to COSERO project directory
- `defaults_settings` - List of configuration parameters
- `statevar_source` - 1 (cold start) or 2 (warm start from statevar.dmp)
- `tmmon_option` - 1 (from file) or 2 (calculate from data)
- `read_outputs` - TRUE/FALSE to load results after execution
- `quiet` - TRUE/FALSE for console output control

---

### Phase 2: Output Readers
**File:** `02_cosero_readers.R`

**Purpose:** Fast, automatic reading of COSERO output files

**Key Functions:**
- `read_cosero_output()` - Main reader with auto-detection
- `detect_outputtype()` - Identifies OUTPUTTYPE 1, 2, or 3
- Individual readers for each file type (runoff, precipitation, statistics, etc.)
- `get_subbasin_data()` - Extract single subbasin data

**Key Achievements:**
1. **Auto-detection** of OUTPUTTYPE based on available files
2. **Fast reading** using `data.table::fread()`
3. **Multi-line header parsing** for complex formats (rundepth.txt, longterm files)
4. **Automatic DateTime creation** from yyyy/mm/dd/hh/mm columns
5. **Missing value handling** (-999 → NA)
6. **Cumulative to timestep conversion** for COSERO.plus1 (ETAGEB_SUM, QABGEB_SUM)
   - Preserves day 1 values
   - Handles September 1 resets
7. **Column name cleaning** (removes GEB suffixes)

**Critical Bug Fixes:**
- **Issue:** `rundepth.txt` had multi-line headers causing duplicate column names
  - **Solution:** Parse all header lines, concatenate properly without duplicates
- **Issue:** `long-term_annual_means.txt` and `long-term_seasonal_means.txt` had "SC_FLAG:" prefix
  - **Solution:** Skip "SC_FLAG:" and "1" tokens, extract actual column names starting from "subbasin"
- **Issue:** Project name column (COSERO_R) in longterm files caused NA coercion warnings
  - **Solution:** Remove project column entirely before numeric conversion

**Data Structure Returned:**
```r
result <- list(
  # OUTPUTTYPE 1 (Basic)
  runoff = data.frame(...),              # QOBS, QSIM, Qloc (m³/s)
  precipitation = data.frame(...),       # PRAIN, PSNOW (mm)
  runoff_components = data.frame(...),   # QAB123, QAB23, QAB3 (mm)
  water_balance = data.frame(...),       # BW0, BW3, SWW, ETAGEB, QABGEB (mm)
  statistics = data.frame(...),          # NSE, KGE, RMSE, etc.
  topology = data.frame(...),            # Network structure

  # OUTPUTTYPE 2 (adds)
  glacier = data.frame(...),             # glacmelt, glacacc (mm)
  meteorology = data.frame(...),         # ETAT, ETP0, P, T (mm, °C)

  # OUTPUTTYPE 3 (adds)
  rundepth = data.frame(...),            # Q in mm
  monitor = data.frame(...),             # Wide format, 8 vars
  monitor_subbasins = list(...),         # 15 dataframes, 39 vars each
  longterm_annual = data.frame(...),     # Annual means (mm/year)
  longterm_seasonal = data.frame(...),   # Seasonal means (mm/season)

  # Metadata
  metadata = list(
    outputtype = integer,
    output_dir = character,
    subbasins = character vector
  )
)
```

---

### Phase 3: Interactive Visualization App
**Files:** `04_cosero_visualization_app.R`, `app_helpers.R`, `run_app.R`

**Purpose:** Interactive Shiny app for exploring COSERO outputs

#### NEW: Integrated COSERO Run GUI (2025-11-03)

**Added "COSERO Run" tab** - Run COSERO directly from the app

**Key Features:**
1. **Project Selection**
   - Browse button (shinyFiles) - interactive directory picker
   - Text input with path normalization (handles quotes, / and \)
   - Real-time validation (checks for COSERO.exe)

2. **Configuration Loading**
   - "Load Defaults" button reads values from existing defaults.txt
   - Auto-populates all input fields
   - Handles both string and vector date formats

3. **Essential Parameters**
   - Date/time inputs (separate date picker + hour/minute dropdowns)
   - Spinup period (auto-adjusts: 0 for warm start, 365 for cold start)
   - Run mode: Cold Start / Warm Start (with statevar.dmp validation)
   - Output level: Basic / Standard / Comprehensive

4. **Advanced Parameters** (collapsible button toggle)
   - SC_FLAG (local vs upstream area)
   - Zonal outputs toggle
   - Temperature calculation method
   - Snow classes (IKL), Land use classes (NCLASS)
   - Project name

5. **Execution & Results**
   - Progress indicator with status messages
   - Runtime tracking
   - Performance metrics display (NSE, KGE by subbasin)
   - Auto-load results into visualization tabs
   - Auto-switch to Time Series tab on success

**Critical Fixes:**
- Fixed parameter name: `settings` → `defaults_settings`
- Enhanced date parsing for both string and vector formats
- Fixed subbasin detection: `extract_subbasin_ids()` → `detect_subbasins()`
- Added topology to data structure
- Integer type conversion for all numeric parameters

**Helper Functions Added** (app_helpers.R):
- `normalize_path_input()` - Handles Windows paths (quotes, slashes)
- `parse_cosero_date()` - Flexible date parsing
- `format_cosero_date()` - Create "YYYY MM DD HH MM" format
- `read_cosero_defaults_safe()` - Read defaults.txt with fallbacks

**UI/UX Improvements:**
- Interactive folder browser (no copy/paste needed)
- Toggle button for advanced parameters (chevron icons)
- Smart spinup adjustment based on run mode
- Warm start file status indicator
- Comprehensive validation with user-friendly error messages
- Logo header (BOKU) with auto-sizing (75px height)
- Working directory validation and error handling

**App Launcher** (run_app.R):
- Ensures correct working directory for Shiny resource paths
- Validates www/ directory and logo.svg existence
- Provides clear error messages if files missing
- Alternative to running app directly from RStudio

**Key Components:**

#### A. `app_helpers.R` - Helper Functions

**Data Processing:**
- `prepare_subbasin_data()` - Extract and filter data for selected subbasin
  - Filters precipitation by subbasin (PRAINGEB_0001 → PRAIN)
  - Filters runoff components by subbasin (QAB123GEB_0001 → QAB123)
  - Filters water balance by subbasin and removes suffixes
  - Applies date range filtering
- `calculate_cumulative()` - Recalculate cumulative sums for display (water year reset on Oct 1)
- `prepare_seasonality_data()` - Monthly aggregation for seasonality tab
  - Auto-detects timestep (hourly/daily/monthly)
  - Aggregates hourly to daily before monthly calculation
  - Fluxes: sum (mm/month), States: mean (mm)
  - Excludes spin-up period
- `detect_timestep()` - Analyzes date differences to determine data frequency

**Plotting Functions (Time Series):**
- `plot_discharge()` - Q_Obs vs Q_Sim (m³/s)
- `plot_precipitation()` - PRAIN/PSNOW stacked bars (mm)
- `plot_runoff_components()` - QAB123, QAB23, QAB3, glacmelt (mm)
- `plot_water_balance()` - Storage + cumulative fluxes (mm)

**Plotting Functions (Seasonality):**
- `plot_seasonality_discharge()` - Mean monthly with markers
- `plot_seasonality_precipitation()` - Monthly precipitation + ET
- `plot_seasonality_runoff()` - Monthly runoff components
- `plot_seasonality_water_balance()` - Monthly state variables

**Plot Features:**
- Centralized color configuration in app_helpers.R
- Custom hover templates with formatted values
- Responsive sizing (12-column grid)
- Unified x-axis for synchronized zooming
- Lines + markers for seasonality plots

**Export Functions:**
- `export_plot_png()` - Save plots as PNG (requires webshot package)

#### B. `04_cosero_visualization_app.R` - Main Shiny App

**UI Structure:**

**Header:**
- BOKU logo (served from www/ directory, 75px height)
- "COSERO Workbench" title (25px font)
- Responsive flex layout

**Collapsible Control Panels** (per tab):
- Show/Hide toggle buttons with chevron icons
- Compact layout to maximize plot space
- Synchronized controls across tabs

**Main Panel - 5 Tabs:**

**Tab 0: COSERO Run** (NEW)
- Integrated GUI for running COSERO model
- Left column: Project setup, dates, run config
- Right column: Advanced options, execution, results

**Tab 1: Time Series** (4 vertically stacked plots)
- Discharge plot (300px height)
- Precipitation plot (250px height)
- Runoff components plot (250px height)
- Water balance plot (300px height)
- All synchronized for zooming

**Tab 2: Seasonality** (4 monthly analysis plots)
- Mean monthly discharge (m³/s)
- Mean monthly precipitation + ET (mm/month)
- Mean monthly runoff components (mm/month)
- Mean monthly water balance states (mm)
- Automatic timestep detection (hourly/daily/monthly)
- Spin-up period exclusion from aggregation
- Water year aggregation (Oct 1 - Sep 30)

**Tab 3: Statistics**
- Interactive DataTable with performance metrics
- Two view modes: All subbasins (by OF) or Single subbasin (all metrics)
- Bar chart visualization for selected objective function
- Dynamic column detection (handles any numeric columns)
- Summary text with key statistics

**Tab 4: Export & Download**
- PNG export buttons (individual plots)
- CSV export buttons (filtered data)
- Timestamped filenames with subbasin ID

**Server Logic:**

**Reactive Values:**
```r
rv <- reactiveValues(
  cosero_data = NULL,           # Full COSERO output
  subbasin_data = NULL,         # Filtered for selected subbasin
  date_range_full = NULL,       # Full date range from data
  status_message = character    # User feedback
)
```

**Key Observers:**
1. `observeEvent(input$load_data)` - Loads COSERO output, updates date range
2. `observe()` for subbasin data - Filters data when subbasin/date changes
3. Plot renderers - Generate Plotly objects reactively
4. Download handlers - Export PNG and CSV files

**Critical Bug Fixes:**
- **Issue:** Duplicate column names (`mm` for month and minute) caused dplyr::filter() to fail
  - **Solution:** Replaced `dplyr::filter()` with base R bracket subsetting `[...]`
- **Issue:** PRAIN/PSNOW not found - columns have subbasin suffixes (PRAINGEB_0001)
  - **Solution:** Use `grep()` to find subbasin-specific columns and rename to standard names
- **Issue:** Runoff components showed all subbasins instead of selected one
  - **Solution:** Filter QAB columns by subbasin ID pattern
- **Issue:** Water balance plot empty
  - **Solution:** Extract columns matching subbasin pattern, remove GEB and subbasin suffixes
- **Issue:** Statistics table error on column names
  - **Solution:** Dynamic column detection instead of hardcoded NSE/KGE/RMSE
- **Issue:** Logo not displaying (404 error)
  - **Solution:** Created run_app.R launcher to ensure correct working directory for Shiny's www/ resource path

**Performance Optimizations:**
- Data caching with cache invalidation (checks file modification times)
- Debounced date range slider (500ms delay to reduce plot updates)
- Cached seasonality calculations with `bindCache()`
- Selective plot re-rendering with `bindEvent()` for water balance variables

**Modern Theming with bslib (2025-11-04):**
- Integrated `bslib` package for professional, modern appearance
- Bootstrap 5 with "Flatly" bootswatch theme
- Custom BOKU-inspired color scheme (green primary colors)
- Google Fonts: Open Sans (body), Montserrat (headings), Fira Code (code)
- Converted main panels to `card()` components with headers
- Enhanced wellPanel styling with rounded corners and subtle shadows
- Improved download buttons with full-width layout and color coding
- Easy theme customization - see `docs/THEME_CUSTOMIZATION.md`
- Can switch themes by changing one line (bootswatch parameter)
- Interactive theme builder available with `bs_themer()` for live customization

---

## Complete Input Documentation

### 1. COSERO Execution Inputs (`run_cosero`)

#### Project Configuration
```r
defaults_settings = list(
  # Date and time (character string "yyyy mm dd hh mm" or numeric vector)
  STARTDATE = "2020 1 1 0 0",        # Simulation start
  ENDDATE = "2022 12 31 0 0",        # Simulation end
  SPINUP = 365,                      # Warm-up timesteps

  # Output control
  OUTPUTTYPE = 3,                    # 1=basic, 2=+glacier/met, 3=+longterm
  SC_FLAG = 1,                       # 0=local area (EZFL_B), 1=upstream (EZFL_T)
  OUTCONTROL = 0,                    # 0=off, 1=zonal outputs

  # File paths
  PROJECTINFO = "COSERO_R",          # Project name
  DATAFILE = "data.txt",             # Input data file
  PARAFILE = "para.txt",             # Parameter file
  RUNOFFFILE = "COSERO.runoff",      # Output runoff file
  STATSFILE = "statistics.txt",      # Output statistics file

  # Model setup
  IKL = 9,                           # Number of snow classes
  NCLASS = 10,                       # Number of land use classes
  ADDFLUXCONT = 0,                   # Additional inflow: 0=off, 1=on
  ADDFLUXFILE = "addflux.txt"        # Additional inflow file
)
```

#### Run Options
```r
statevar_source = 1                  # 1=cold start (from parafile)
                                     # 2=warm start (from statevar.dmp)

tmmon_option = 1                     # 1=use TMMon from parafile
                                     # 2=calculate TMMon from data

read_outputs = TRUE                  # Load outputs after execution
quiet = FALSE                        # Show console output
```

### 2. Output Reader Inputs (`read_cosero_output`)

```r
output_dir = "path/to/output"        # COSERO output directory
defaults_file = NULL                 # Optional: path to defaults.txt for parameters
quiet = FALSE                        # Suppress messages
```

### 3. Visualization App Inputs

#### Data Selection
- **Output Directory**: Path to COSERO output folder
- **Load Data Button**: Triggers data reading

#### Display Options
- **Subbasin**: Dropdown with IDs (0001-0015)
- **Date Range**: Slider with start/end dates
- **Reset Zoom**: Button to restore full date range

#### Water Balance Variables
- **Storage Variables**: Checkboxes
  - BW0 (Soil Moisture Top, mm)
  - BW3 (Soil Moisture Deep, mm)
  - SWW (Snow Water Equivalent, mm)
- **Cumulative Fluxes**: Checkboxes
  - P_cum (Cumulative Precipitation, mm)
  - ETAGEB_cum (Cumulative ET, mm)
  - QABGEB_cum (Cumulative Runoff, mm)

#### Precipitation Display
- **Combined**: Stacked bars (PRAIN + PSNOW)
- **Separate**: Side-by-side bars

---

## Units Reference

| Variable | Description | Unit | File |
|----------|-------------|------|------|
| **Discharge** |
| QOBS, QSIM | Observed/simulated discharge | m³/s | COSERO.runoff |
| QOBS, QSIM | Runoff depth | mm | rundepth.txt |
| **Precipitation** |
| PRAIN, PSNOW | Rain/snow precipitation | mm | COSERO.prec |
| **Runoff Components** |
| QAB123 | Total runoff | mm | COSERO.plus |
| QAB23 | Subsurface runoff | mm | COSERO.plus |
| QAB3 | Baseflow | mm | COSERO.plus |
| **Water Balance** |
| BW0 | Soil moisture (top layer) | mm | COSERO.plus1 |
| BW3 | Soil moisture (deep layer) | mm | COSERO.plus1 |
| SWW | Snow water equivalent | mm | COSERO.plus1 |
| ETAGEB | Actual evapotranspiration | mm | COSERO.plus1 |
| QABGEB | Runoff generation | mm | COSERO.plus1 |
| P | Total precipitation | mm | COSERO.plus1 |
| **Meteorology** |
| T | Temperature | °C | var_MET.txt |
| ETAT | Actual ET | mm | var_MET.txt |
| ETP0 | Potential ET | mm | var_MET.txt |
| **Glacier** |
| MELT | Snowmelt | mm | var_glac.txt |
| glacmelt | Glacier melt | mm | var_glac.txt |
| glacacc | Glacier accumulation | mm | var_glac.txt |
| **Statistics** |
| NSE | Nash-Sutcliffe Efficiency | - | statistics.txt |
| KGE | Kling-Gupta Efficiency | - | statistics.txt |
| RMSE | Root Mean Square Error | m³/s | statistics.txt |
| CORR | Correlation coefficient | - | statistics.txt |
| BIAS | Bias | m³/s | statistics.txt |

---

## Key Technical Decisions

1. **Date Format Flexibility**: Accept both string "2020 1 1 0 0" and vector c(2020, 1, 1, 0, 0)
2. **Fast Reading**: Use `data.table::fread()` for speed
3. **Cumulative Conversion**: Convert COSERO.plus1 cumulative sums to timestep values automatically
4. **Subbasin Filtering**: Extract subbasin-specific columns and rename to standard names
5. **Interactive Plots**: Use Plotly for zoom, pan, hover capabilities
6. **Error Handling**: Base R subsetting instead of dplyr for duplicate column names
7. **Dynamic Statistics**: Auto-detect numeric columns instead of hardcoding

---

## File Dependencies

```
01_cosero_core_run.R
  ├── Executes: COSERO.exe
  ├── Reads: defaults.txt
  ├── Writes: defaults.txt (with backups)
  └── Calls: 02_cosero_readers.R (if read_outputs=TRUE)

02_cosero_readers.R
  ├── Reads: All COSERO output files
  │   ├── COSERO.runoff
  │   ├── COSERO.prec
  │   ├── COSERO.plus
  │   ├── COSERO.plus1
  │   ├── statistics.txt
  │   ├── topology.txt
  │   ├── var_glac.txt (OUTPUTTYPE 2+)
  │   ├── var_MET.txt (OUTPUTTYPE 2+)
  │   ├── monitor.txt (OUTPUTTYPE 3)
  │   ├── monitor_sb*.txt (OUTPUTTYPE 3)
  │   ├── rundepth.txt (OUTPUTTYPE 3)
  │   ├── long-term_annual_means.txt (OUTPUTTYPE 3)
  │   └── long-term_seasonal_means.txt (OUTPUTTYPE 3)
  └── Returns: Structured list with all datasets

04_cosero_visualization_app.R
  ├── Sources: 01_cosero_core_run.R
  ├── Sources: 02_cosero_readers.R
  ├── Sources: app_helpers.R
  ├── Reads: www/logo.svg
  └── Creates: Interactive Shiny web app

app_helpers.R
  ├── Data processing functions
  ├── Seasonality aggregation functions
  ├── Plotly plotting functions (time series + seasonality)
  ├── Export utilities
  └── COSERO configuration helpers

run_app.R
  ├── Sets working directory
  ├── Validates file structure
  └── Launches: 04_cosero_visualization_app.R
```

---

## Required R Packages

```r
# Core execution
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)

# Data reading
library(data.table)

# Visualization app
library(shiny)
library(bslib)       # Modern theming system
library(plotly)
library(DT)
library(shinyFiles)  # Directory browser
library(fs)          # File system operations
library(kaleido)     # Optional: for PNG export (replaces webshot)
```

---

## Quick Start Commands

```r
# Option 1: Use the GUI launcher (recommended)
source("run_app.R")
# Ensures correct working directory, then opens app
# Go to "COSERO Run" tab → Browse for project → Load Defaults → Run

# Option 2: Run from RStudio
# Open 04_cosero_visualization_app.R and click "Run App" button

# Option 3: Run COSERO from R script
source("01_cosero_core_run.R")
result <- run_cosero(
  "path/to/project",
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2022 12 31 0 0",
    OUTPUTTYPE = 3
  )
)

# Option 3: Read existing outputs
source("02_cosero_readers.R")
data <- read_cosero_output("path/to/output")
```

---

### Phase 4: Sensitivity Analysis Framework
**Files:** `05_cosero_sensitivity_analysis.R`, `06_cosero_sensitivity_example.R`, `cosero_parameter_bounds.csv`

**Purpose:** Global sensitivity analysis using Sobol method for COSERO parameter evaluation

**Date:** 2025-11-04

#### Key Components

**A. Parameter Database (`cosero_parameter_bounds.csv`)**
- 30 frequently-used COSERO parameters with bounds
- Three modification types:
  - **absval**: Direct replacement (e.g., BETA = 2.5)
  - **relchg**: Multiply by factor (e.g., TAB1 = original × 1.3)
  - **abschg**: Add to original (e.g., TCOR = original + 1.5)
- Organized by category: runoff, snow, evapotranspiration, soil, groundwater, routing

**B. Core Functions (`05_cosero_sensitivity_analysis.R`)**

**Parameter Setup:**
- `load_parameter_bounds()` - Load parameter bounds from CSV
- `create_sobol_bounds()` - Format bounds for Sobol sampling

**Sobol Sampling:**
- `generate_sobol_samples()` - Generate Sobol quasi-random parameter sets
  - Uses `sensobol` package for variance-based analysis
  - Returns scaled parameter sets within bounds
  - Sample size formula: Total runs = n × (2 + n_params)

**Ensemble Execution:**
- `run_cosero_ensemble()` - Sequential execution of COSERO with parameter sets
  - Reads parameter file name from defaults.txt
  - Backs up original parameter file
  - Applies modification types correctly (absval/relchg/abschg)
  - Restores original file after completion

- `run_cosero_ensemble_parallel()` - **Parallel execution** for 4-8× speedup
  - Auto-detects CPU cores (uses n-1 by default)
  - Creates isolated temporary project copies per worker
  - Distributes runs across cores evenly
  - Automatic cleanup of temp folders
  - Returns same format as sequential version

**Helper Functions:**
- `read_parameter_file()` - Extract current parameter values
- `modify_parameter_file()` - Apply parameter modifications with type-aware logic

**Result Processing:**
- `extract_ensemble_output()` - Extract simulation outputs (runoff, ET)
- `calculate_ensemble_metrics()` - Calculate NSE/KGE/RMSE for ensemble
- `aggregate_to_monthly()` / `aggregate_to_annual()` - Temporal aggregation

**Sensitivity Calculation:**
- `calculate_sobol_indices()` - Compute first-order (Si) and total-effect (STi) indices
  - Uses Jansen estimator with bootstrap (R=100)
  - Si = direct parameter effect
  - STi = total effect including interactions

**Visualization:**
- `plot_sobol()` - Bar charts of sensitivity indices with confidence intervals
- `plot_dotty()` - Parameter-output scatter plots across parameter space
- `plot_ensemble_uncertainty()` - Ensemble uncertainty bands with observations

**Export:**
- `export_sensitivity_results()` - Save Sobol indices, parameters, metrics to CSV/RDS

**C. Example Workflows (`06_cosero_sensitivity_example.R`)**

**Example 1: Snow and Runoff Parameters**
- 9 parameters: BETA, CTMAX, CTMIN, SNOWTRT, RAINTRT, TAB1, TAB2, H1, H2
- Analyzes sensitivity of mean monthly runoff
- Creates Sobol indices and dotty plots
- Optional KGE performance analysis

**Example 2: ET and Soil Parameters**
- 10 parameters: ETSLPCOR, ETSYSCOR, ETVEGCOR, FKFAK, M, FK, PWP, PCOR, TVS1, TVS2
- Analyzes sensitivity of mean annual ET
- Water balance check (ET + Runoff vs Precipitation)
- Dotty plots with reference lines

**Example 3: Full Parameter Set**
- All 30 parameters for comprehensive analysis
- Note: n=50 with 30 params = 1,600 runs

**Helper Functions:**
- `compare_sensitivity()` - Compare Sobol results between examples
- `create_summary_table()` - Format results for reporting

#### Methodology: Sobol Sensitivity Analysis

Based on SWAT model methodology (SWATrunR + sensobol workflow):

**Sobol Indices Interpretation:**
- **Si (First-Order)**: Direct effect of parameter on output variance
  - Si > 0.3: Dominant parameter requiring careful calibration
  - Si > 0.1: Substantial influence worth investigating
  - Si ≈ 0: Minimal individual effect

- **STi (Total-Effect)**: Total contribution including all interactions
  - STi ≥ Si always (total includes direct + interactions)
  - Large (STi - Si): Strong parameter interactions
  - STi ≈ Si: Effects are primarily direct

**Sample Size Trade-offs:**
| n | Total Runs (9 params) | Run Time* | Use Case |
|---|---|---|---|
| 25 | 275 | 2-5 min | Initial screening |
| 50 | 550 | 5-15 min | **Training/testing** |
| 100 | 1,100 | 15-30 min | Publication quality |
| 200 | 2,200 | 30-60 min | Research applications |

*Time depends on COSERO runtime and system specs

#### Parallel Processing Benefits

**Performance Gains:**
- 4 cores → ~3.5× faster
- 8 cores → ~7× faster
- Example: 550 runs at 10 sec each = 90 min → 13 min (8 cores)

**Implementation:**
- Uses R's `parallel` package with `makeCluster()`
- Each worker operates on isolated project copy
- No file conflicts or race conditions
- Automatic temp folder cleanup

#### Parameter Modification Types

**absval (Absolute Value):**
```r
# Direct replacement
BETA: sample from [0.1, 10] → BETA = 2.5
```

**relchg (Relative Change - Multiplier):**
```r
# Multiply by sampled factor
TAB1: sample from [0.1, 5.0]
If original = 50, sampled = 1.3 → TAB1 = 50 × 1.3 = 65
```

**abschg (Absolute Change - Additive):**
```r
# Add to original value
TCOR: sample from [0, 3]
If original = 0, sampled = 1.5 → TCOR = 0 + 1.5 = 1.5
```

#### Typical Workflow

```r
# 1. Load functions
source("05_cosero_sensitivity_analysis.R")

# 2. Select parameters and load bounds
params <- c("BETA", "CTMAX", "TAB1", "TAB2", "M")
par_bounds <- load_parameter_bounds(parameters = params)

# 3. Generate Sobol samples
sobol_bounds <- create_sobol_bounds(par_bounds)
sobol_samples <- generate_sobol_samples(sobol_bounds, n = 50)

# 4. Run ensemble (parallel for speed)
results <- run_cosero_ensemble_parallel(
  project_path = "path/to/project",
  parameter_sets = sobol_samples$parameter_sets,
  par_bounds = par_bounds,
  n_cores = 4
)

# 5. Calculate sensitivity for runoff
runoff_means <- sapply(results$results, function(x)
  mean(x$output_data$runoff$value))
sobol_runoff <- calculate_sobol_indices(runoff_means, sobol_samples)

# 6. Visualize
plot_sobol(sobol_runoff, title = "Runoff Sensitivity")
plot_dotty(sobol_samples$parameter_sets, runoff_means,
           y_label = "Mean Runoff")

# 7. Export
export_sensitivity_results("output/", sobol_runoff,
                           sobol_samples$parameter_sets)
```

#### Applications

**1. Calibration Preparation**
- Identify most influential parameters
- Reduce parameter space for optimization
- Set realistic calibration bounds

**2. Uncertainty Quantification**
- Assess parameter-induced uncertainty
- Identify behavioral parameter ranges
- Support GLUE or Bayesian calibration

**3. Model Understanding**
- Reveal parameter interactions
- Understand process dominance
- Guide model structure decisions

**4. Scenario Analysis**
- Test parameter uncertainty impacts
- Support climate change studies
- Inform data collection priorities

#### Required Packages

```r
library(sensobol)    # Sobol sensitivity analysis
library(hydroGOF)    # Hydrological performance metrics (NSE, KGE, RMSE)
library(parallel)    # Parallel processing
library(pbapply)     # Progress bars for parallel operations
library(dplyr)       # Data manipulation
library(tidyr)       # Data tidying
library(purrr)       # Functional programming
library(ggplot2)     # Visualization
```

---

## Summary

This COSERO R Interface provides:
1. **Integrated GUI** for running COSERO with interactive directory browser and parameter management
2. **Automated execution** with configuration management and warm start support
3. **Fast output reading** with auto-detection and data cleaning
4. **Interactive visualization** with zoom, filter, and export capabilities
5. **Sensitivity analysis framework** with Sobol method and parallel execution for efficient parameter evaluation

Complete workflow: Browse → Load Defaults → Adjust → Run → Visualize → Sensitivity Analysis

All components are production-ready with comprehensive error handling, user feedback, and documentation.
