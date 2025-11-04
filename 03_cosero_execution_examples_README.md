# COSERO Execution Examples

Practical examples for running COSERO with different configurations.

## Setup

```r
source("01_cosero_core_run.R")
project_path <- "path/to/your/COSERO_project"
```

## Example 1: Complete configuration with all common options

```r
result <- run_cosero(
  project_path,
  defaults_settings = list(
    # Date and time
    STARTDATE = "2020 1 1 0 0",        # year month day hour minute
    ENDDATE = "2022 12 31 0 0",
    SPINUP = 365,                      # warm-up period (timesteps)

    # Output control
    OUTPUTTYPE = 3,                    # 1=basic, 2=+glacier/met, 3=+longterm
    SC_FLAG = 1,                       # 0=local (EZFL_B), 1=upstream (EZFL_T)
    OUTCONTROL = 0,                    # 0=off, 1=zonal outputs

    # Run options
    tmmon_option = 1,                  # 1=from file, 2=calculate
    statevar_source = 1,               # 1=from file, 2=warm start

    # Project info
    PROJECTINFO = "MyProject",

    # Model setup
    IKL = 9,                           # snow classes
    NCLASS = 10,                       # land use classes
    ADDFLUXCONT = 0                    # additional inflow: 0=off, 1=on
  ),
  quiet = FALSE,
  read_outputs = TRUE
)

# Check results
if (result$success) {
  cat("Runtime:", round(result$runtime_seconds, 2), "seconds\n")
  head(result$output_data$runoff)      # Discharge (m³/s)
  head(result$output_data$statistics)  # NSE, KGE, RMSE
} else {
  cat("Error:", result$error_message, "\n")
}
```

## Example 2: Warm start continuation run

```r
# First run (2015-2020) - creates statevar.dmp
result1 <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2015 1 1 0 0",
    ENDDATE = "2020 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 3
  ),
  statevar_source = 1,                 # Cold start from parameter file
  quiet = FALSE
)

# Second run (2021-2025) - continues from previous state
result2 <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2021 1 1 0 0",
    ENDDATE = "2025 12 31 0 0",
    SPINUP = 0,                        # No spinup needed (warm start)
    OUTPUTTYPE = 3
  ),
  statevar_source = 2,                 # Warm start from statevar.dmp
  quiet = FALSE
)

cat("Run 1:", result1$runtime_seconds, "sec\n")
cat("Run 2:", result2$runtime_seconds, "sec\n")
```

## Example 3: Fast batch run for optimization

```r
# Run without reading outputs (much faster)
result <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2022 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 1                     # Basic output only
  ),
  read_outputs = FALSE,                # Skip reading outputs
  quiet = TRUE                         # Minimal console output
)

# Read only statistics file manually
stats <- read_cosero_statistics(
  file.path(project_path, "output", "statistics.txt")
)
print(stats[, c("sb", "NSE", "KGE")])  # Show performance metrics
```

## Configuration Parameter Quick Reference

### Essential Parameters

| Parameter | Options | Description |
|-----------|---------|-------------|
| `STARTDATE` | `"yyyy mm dd hh mm"` | Simulation start date |
| `ENDDATE` | `"yyyy mm dd hh mm"` | Simulation end date |
| `SPINUP` | integer | Warm-up timesteps (typically 365) |
| `OUTPUTTYPE` | 1, 2, 3 | Output detail level |
| `statevar_source` | 1, 2 | 1=cold start, 2=warm start |
| `tmmon_option` | 1, 2 | 1=from file, 2=calculate |

### Output Control

| Parameter | Options | Description |
|-----------|---------|-------------|
| `OUTPUTTYPE` | 1 | Basic: runoff (m³/s), precip (mm), stats |
| | 2 | + Glacier/meteorology variables |
| | 3 | + Long-term means, rundepth (mm) |
| `SC_FLAG` | 0 | Local subbasin area (EZFL_B) |
| | 1 | Total upstream area (EZFL_T) |
| `OUTCONTROL` | 0, 1 | Zonal outputs (needs OUTPUTTYPE=3) |

### Model Setup

| Parameter | Type | Description |
|-----------|------|-------------|
| `IKL` | integer | Number of snow classes |
| `NCLASS` | integer | Number of land use classes |
| `ADDFLUXCONT` | 0, 1 | Additional inflow control |

## Common Workflows

### Workflow 1: Single period simulation
1. Set date range with `STARTDATE`, `ENDDATE`
2. Set `SPINUP = 365` for warm-up
3. Use `statevar_source = 1` (cold start)
4. Set desired `OUTPUTTYPE`

### Workflow 2: Multi-period continuous simulation
1. **First period**: Use `statevar_source = 1`, creates statevar.dmp
2. **Subsequent periods**: Use `statevar_source = 2`, `SPINUP = 0`
3. Each run continues from previous state

### Workflow 3: Calibration/optimization
1. Set `read_outputs = FALSE` for speed
2. Set `quiet = TRUE` for clean output
3. Use `OUTPUTTYPE = 1` (minimal output)
4. Read only statistics file manually
5. Extract NSE/KGE for objective function

## Tips

- **Dates**: Use format `"year month day hour minute"` (e.g., `"2020 1 1 0 0"`)
- **Spinup**: Typically 365 days for cold starts, 0 for warm starts
- **Speed**: Disable `read_outputs` and use `quiet = TRUE` for batch runs
- **State files**: statevar.dmp is saved to `output/` directory after each run
- **Backups**: Configuration backups saved to `input/parameterfile_backup/`
