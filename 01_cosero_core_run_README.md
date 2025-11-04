# COSERO Core Run Interface

Main interface for executing the COSERO hydrological model from R with automated configuration management.

## Main Function

```r
run_cosero(project_path, defaults_settings = NULL, statevar_source = 1, tmmon_option = 1,
           read_outputs = TRUE, quiet = FALSE)
```

**Parameters:**

- `project_path`: Path to COSERO project directory (must contain COSERO.exe)
- `defaults_settings`: List of configuration parameters to modify
- `statevar_source`: State variable initialization
  - `1` = from parameter file (cold start, default)
  - `2` = from statevar.dmp file (warm start from previous run)
- `tmmon_option`: Monthly temperature handling
  - `1` = use TMMon from parameter file (default)
  - `2` = calculate TMMon from input data
- `read_outputs`: Whether to read output files after execution (TRUE/FALSE)
- `quiet`: Suppress console output except runtime and errors (TRUE/FALSE)

**Returns:** List with:

- `success`: TRUE/FALSE
- `exit_code`: 0 = success
- `runtime_seconds`: Execution time
- `output_data`: Parsed outputs (if read_outputs = TRUE)
- `error_message`: Error details (if failed)

## Configuration Parameters

### Date/Time Settings
- `STARTDATE`: Simulation start `"year month day hour minute"` (e.g., `"2015 1 1 0 0"`)
- `ENDDATE`: Simulation end `"year month day hour minute"` (e.g., `"2022 12 31 23 59"`)
- `SPINUP`: Warm-up period (timesteps, typically 365)

### Output Control
- `OUTPUTTYPE`: Output level
  - `1` = Basic (runoff, precipitation, statistics)
  - `2` = + glacier/meteorology variables
  - `3` = + long-term annual/seasonal means, rundepth
- `SC_FLAG`: Flux calculation mode
  - `0` = Local subbasin area (EZFL_B)
  - `1` = Total upstream catchment area (EZFL_T)
- `OUTCONTROL`: Zonal output control (0 = off, 1 = on, requires OUTPUTTYPE=3)

### Files
- `PROJECTINFO`: Project name string
- `DATAFILE`: Input data filename
- `PARAFILE`: Parameter filename
- `RUNOFFFILE`: Output runoff filename
- `STATSFILE`: Output statistics filename

### Model Setup
- `IKL`: Number of snow classes (integer)
- `NCLASS`: Number of land use classes (integer)
- `ADDFLUXCONT`: Additional inflow control (0 = off, 1 = on)
- `ADDFLUXFILE`: Additional inflow filename

## Usage Examples

### Example 1: Basic run with date range
```r
result <- run_cosero(
  "path/to/project",
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2022 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 3
  )
)
```

### Example 2: Warm start from previous run
```r
# First run - creates statevar.dmp
result1 <- run_cosero("path/to/project",
                      defaults_settings = list(
                        STARTDATE = "2015 1 1 0 0",
                        ENDDATE = "2020 12 31 0 0"
                      ))

# Second run - continues from previous state
result2 <- run_cosero("path/to/project",
                      defaults_settings = list(
                        STARTDATE = "2021 1 1 0 0",
                        ENDDATE = "2025 12 31 0 0"
                      ),
                      statevar_source = 2)  # Use saved state variables
```

### Example 3: Fast batch run without reading outputs
```r
result <- run_cosero(
  "path/to/project",
  defaults_settings = list(STARTDATE = "2020 1 1 0 0", ENDDATE = "2022 12 31 0 0"),
  read_outputs = FALSE,  # Skip reading for speed
  quiet = TRUE           # Minimal console output
)

if (result$success) {
  cat("Runtime:", result$runtime_seconds, "seconds\n")
}
```

## Key Features

- **Automatic backup**: Creates timestamped backups of configuration files before modifications
- **Validation**: Checks parameter types and cross-validates date ranges
- **Error detection**: Scans COSERO output for error messages
- **Flexible dates**: Accepts dates as strings `"year month day hour minute"` or vectors `c(year, month, day, hour, minute)`
- **State persistence**: Supports warm starts using saved state variables (statevar.dmp)
- **Integration**: Automatically loads output reader if `read_outputs = TRUE`
