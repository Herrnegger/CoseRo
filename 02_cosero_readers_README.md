# COSERO Output Readers

Fast, automatic readers for COSERO hydrological model output files using `data.table::fread()`.

## Main Function

```r
read_cosero_output(output_dir, defaults_file = NULL, quiet = FALSE)
```

**Parameters:**

- `output_dir`: Path to COSERO output directory
- `defaults_file`: Optional path to defaults.txt (for parameter reading)
- `quiet`: Suppress console messages (TRUE/FALSE)

**Returns:** List containing datasets organized by theme, plus metadata

## Output Structure

Auto-detects OUTPUTTYPE (1, 2, or 3) based on files present in output directory.

### OUTPUTTYPE 1 (Basic)

| Dataset | Description | Key Variables | Units |
|---------|-------------|---------------|-------|
| `runoff` | Discharge timeseries | QOBS, QSIM, Qloc | m³/s |
| `precipitation` | Precipitation | PRAIN, PSNOW | mm |
| `runoff_components` | Runoff components | QAB123, QAB23, QAB3, T | mm |
| `water_balance` | Water balance | BW0, BW3, SWW, storage, ETAGEB, QABGEB | mm |
| `statistics` | Performance metrics | NSE, KGE, RMSE, Bias, R² | - |
| `topology` | Network structure | NZ, NB, IZ, upstream links | - |

**Note**: Cumulative variables (ETAGEB_SUM, QABGEB_SUM) are automatically converted to timestep values, preserving day 1 values.

### OUTPUTTYPE 2 (adds to OUTPUTTYPE 1)

| Dataset | Description | Key Variables | Units |
|---------|-------------|---------------|-------|
| `glacier` | Glacier mass balance | glacmelt, glacacc | mm |
| `meteorology` | Full meteorological variables | ETAT, ETP0, P, T | mm, °C |

### OUTPUTTYPE 3 (adds to OUTPUTTYPE 2)

| Dataset | Description | Key Variables | Units |
|---------|-------------|---------------|-------|
| `rundepth` | Runoff depth | QOBS, QSIM per subbasin | mm |
| `monitor` | Multi-subbasin wide format | 8 key variables across basins | various |
| `monitor_subbasins` | List of 15 dataframes per subbasin | 39 variables each (most comprehensive) | various |
| `longterm_annual` | Annual means | All water balance components | mm, mm/year |
| `longterm_seasonal` | Seasonal means (4 seasons) | All water balance components | mm, mm/season |

**Key water balance variables** (in mm): ETAG, ETAT, P, PRAIN, PSNOW, QAB, QAB1, QAB2, QAB3, BW0, BW3, SWW, MELT, etc.

### Metadata

```r
result$metadata
  $outputtype      # Detected OUTPUTTYPE (1, 2, or 3)
  $output_dir      # Path to output directory
  $subbasins       # Character vector of subbasin IDs
```

## Usage Examples

### Example 1: Read all outputs
```r
source("02_cosero_readers.R")

result <- read_cosero_output("path/to/project/output")

# Access datasets
head(result$runoff)              # Discharge (m³/s)
head(result$precipitation)       # Precipitation (mm)
head(result$statistics)          # NSE, KGE, etc.
head(result$water_balance)       # Water balance (mm)
head(result$rundepth)            # Runoff depth (mm)
head(result$longterm_annual)     # Annual means (mm/year)

# Check metadata
result$metadata$outputtype       # 1, 2, or 3
result$metadata$subbasins        # c("0001", "0002", ...)
```

### Example 2: Extract single subbasin data
```r
# Get discharge for subbasin 1
sb_data <- get_subbasin_data(result$runoff, subbasin_id = 1)
# Returns: DateTime, Date, Q_obs, Q_sim, Q_local (m³/s)

# List all available subbasins
list_subbasins(result$runoff)    # Prints: "0001", "0002", ...
```

### Example 3: Access monitor data (OUTPUTTYPE 3)
```r
# Wide format - all subbasins in one dataframe
head(result$monitor)

# Per-subbasin detailed data (39 variables)
sb1 <- result$monitor_subbasins[["0001"]]
colnames(sb1)  # Shows all 39 variables

# Access specific subbasin
sb5 <- result$monitor_subbasins[["0005"]]
```

## Key Features

- **Auto-detection**: Identifies OUTPUTTYPE from available files
- **Fast reading**: Uses `data.table::fread()` for speed
- **DateTime handling**: Automatically creates DateTime and Date columns from yyyy/mm/dd/hh/mm
- **Missing values**: Converts -999 to NA in discharge columns
- **Cumulative conversion**: COSERO.plus1 cumulative sums (ETAGEB_SUM, QABGEB_SUM) converted to timestep values
- **Clean headers**: Removes GEB suffixes and subbasin suffixes where appropriate
- **Multi-line headers**: Correctly parses complex headers (rundepth.txt, longterm files)

## Common Variables and Units

| Variable | Description | Unit | File |
|----------|-------------|------|------|
| QOBS, QSIM | Observed/simulated discharge | m³/s | runoff |
| QOBS, QSIM | Runoff depth | mm | rundepth |
| PRAIN, PSNOW | Rain/snow precipitation | mm | precipitation |
| QAB, QAB1, QAB2, QAB3 | Total/fast/medium/slow runoff | mm | runoff_components |
| ETAGEB, ETAG | Actual evapotranspiration (timestep) | mm | water_balance |
| QABGEB | Runoff generation (timestep) | mm | water_balance |
| BW0, BW3 | Soil moisture storage (top/deep) | mm | water_balance |
| SWW | Snow water equivalent | mm | water_balance |
| NSE, KGE, RMSE | Model performance metrics | - | statistics |
| T | Temperature | °C | meteorology |
| MELT | Snowmelt | mm | glacier/longterm |

## Data Processing Notes

1. **Cumulative to timestep**: Variables ending in `_SUM_` in COSERO.plus1 are converted:
   - First value preserved (day 1 actual value)
   - Subsequent values = diff(cumulative)
   - Handles year resets (Sept 1) automatically

2. **Missing values**: -999 converted to NA in discharge columns (QOBS, QSIM, Qloc)

3. **Column name cleaning**:
   - Removes "_GEB" suffixes
   - Removes subbasin ID suffixes in monitor_subbasins

4. **Multi-line headers**: Handles complex formats in rundepth.txt and longterm files
