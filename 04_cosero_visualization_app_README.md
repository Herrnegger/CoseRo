# COSERO Interactive Visualization App

Interactive Shiny app for visualizing COSERO hydrological model outputs with zoom, pan, and export capabilities.

## Quick Start

```r
# Launch the app
shiny::runApp("04_cosero_visualization_app.R")
```

Or from RStudio: Open `04_cosero_visualization_app.R` and click "Run App"

## Features

### Data Loading
- **Directory selection**: Enter path or use default `COSERO_MonteCarlo_Optimierung_SH/output`
- **Auto-detection**: Automatically detects OUTPUTTYPE (1, 2, or 3)
- **Load button**: Click to read all available output files

### Interactive Controls

**Subbasin Selection:**
- Dropdown menu with all available subbasins (e.g., 0001-0015)

**Date Range Zoom:**
- Interactive slider to zoom into specific time periods
- Reset button to return to full date range

**Precipitation Display:**
- Radio buttons: Combined (stacked) or Separate (PRAIN/PSNOW)

### Visualization Tabs

#### Tab 1: Time Series (4 vertically stacked plots)

**Plot 1 - Discharge (m³/s):**
- Blue line: Observed discharge (Q_Obs)
- Red line: Simulated discharge (Q_Sim)
- Hover shows date and values

**Plot 2 - Precipitation (mm):**
- Blue bars: Rain (PRAIN)
- Black bars: Snow (PSNOW)
- Combined (stacked) or separate display

**Plot 3 - Runoff Components (mm):**
- Cyan: Total runoff (QAB123)
- Orange: Subsurface runoff (QAB23)
- Light blue: Baseflow (QAB3)

**Plot 4 - Water Balance (mm):**
- Red: Soil moisture top (BW0)
- Blue: Soil moisture deep (BW3)
- Cyan: Snow water equivalent (SWW)
- Green: Cumulative precipitation (P_cum)
- Orange: Cumulative ET (ETAGEB_cum)
- Purple: Cumulative runoff (QABGEB_cum)

**Interactive Features:**
- Zoom: Click and drag on any plot
- Pan: Shift + drag to pan
- Reset: Double-click to reset zoom
- Legend: Click legend items to hide/show traces
- Synchronized x-axes: All plots zoom together

#### Tab 2: Statistics
- Performance metrics table: NSE, KGE, RMSE, Bias, R²
- Summary text with key statistics
- Filtered by selected subbasin

#### Tab 3: Export & Download

**Export Plots (PNG):**
- Individual plot downloads (requires `webshot` package)
- Filenames include subbasin ID and date
- Resolution: 1200×400 pixels

**Export Data (CSV):**
- Download filtered data for selected subbasin and date range
- Separate files for each dataset
- All variables included

## Requirements

### R Packages

```r
install.packages(c(
  "shiny",      # App framework
  "plotly",     # Interactive plots
  "dplyr",      # Data manipulation
  "lubridate",  # Date handling
  "DT",         # Interactive tables
  "webshot"     # Optional: for PNG export
))

# For PNG export (optional):
webshot::install_phantomjs()
```

### Data Files
Requires COSERO output directory with at least:
- `COSERO.runoff` (discharge, m³/s)
- `COSERO.prec` (precipitation, mm)
- `COSERO.plus` (runoff components, mm)
- `COSERO.plus1` (water balance, mm)
- `statistics.txt` (performance metrics)

## Usage Examples

### Example 1: Load default directory
1. Click "Load Data" (uses default path)
2. Select subbasin from dropdown
3. Explore interactive plots

### Example 2: Load custom directory
1. Enter path: `path/to/your/COSERO/output`
2. Click "Load Data"
3. Select subbasin and adjust date range

### Example 3: Zoom into specific period
1. Load data and select subbasin
2. Use date range slider to select period (e.g., summer 2021)
3. All plots zoom simultaneously
4. Click "Reset Zoom" to return to full range

### Example 4: Customize water balance plot
1. Uncheck unwanted storage variables
2. Select/deselect cumulative fluxes
3. Plot updates automatically

### Example 5: Export results
1. Navigate to "Export & Download" tab
2. Click download buttons for PNG plots or CSV data
3. Files saved with subbasin ID and date

## Tips

- **Performance**: Date range filtering speeds up rendering for long time series
- **Variables**: Hover over plot legend to see full variable names
- **Cumulative**: Cumulative fluxes are calculated from timestep values automatically
- **Missing data**: NA values are handled gracefully in all plots
- **Multiple runs**: Change output directory to compare different COSERO runs

## Troubleshooting

**Error: "Directory not found"**
- Check that output path exists and contains COSERO output files
- Use forward slashes `/` or double backslashes `\\` in Windows paths

**Error: "No subbasins found"**
- Verify that `COSERO.runoff` exists and contains QOBS/QSIM columns
- Check that file format matches expected structure

**PNG export not working:**
- Install webshot: `install.packages("webshot")`
- Install PhantomJS: `webshot::install_phantomjs()`
- Alternative: Use screenshot tool to capture plots

**Slow performance:**
- Reduce date range using slider
- Disable unused water balance variables
- Use OUTPUTTYPE 1 for faster loading (basic outputs only)

## File Dependencies

The app requires:
- `02_cosero_readers.R` - Data reading functions
- `app_helpers.R` - Plotting and processing functions
- COSERO output directory with text files

## Notes

- All plots use Plotly for interactivity
- Cumulative variables recalculated from timestep data (matches COSERO.plus1 conversion)
- Date filtering applied reactively to all datasets
- Subbasin-specific data extracted automatically
- Statistics tab shows only metrics for selected subbasin
