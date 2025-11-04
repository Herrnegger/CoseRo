# Parameter File Handling in Sensitivity Analysis

This document explains how the COSERO parameter file is read, modified, and written during sensitivity analysis, and **what happens to parameters that are NOT included in the sensitivity analysis**.

## Quick Answer

**Parameters NOT in the sensitivity analysis retain their original values from the parameter file.**

This happens because:
1. The original file is backed up before any modifications
2. Before each run, the backup is restored (resetting ALL parameters)
3. Then only the sensitivity analysis parameters are modified
4. All other parameters remain unchanged from the original file

---

## Detailed Process Flow

### 1. Initialization (Start of Ensemble)

**File:** `05_cosero_sensitivity_analysis.R`, function `run_cosero_ensemble()`

```r
# Line 284-285: Create backup of original parameter file
backup_file <- paste0(par_file, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
file.copy(par_file, backup_file)

# Line 288: Read original values ONLY for parameters being modified
original_values <- read_parameter_file(par_file, names(parameter_sets))
# Example: If analyzing BETA, CTMAX, TAB1
# original_values = list(BETA=4.5, CTMAX=5, TAB1=1)
```

**Location:** `run_cosero_ensemble()` at line 250-329

**What happens:**
- Original `para.txt` is copied to `para.txt.backup_YYYYMMDD_HHMMSS`
- Only the parameters in the sensitivity analysis are read from the file
- Their values are stored in `original_values` for use with `relchg` and `abschg` modification types

---

### 2. For Each Simulation Run

**File:** `05_cosero_sensitivity_analysis.R`

```r
# Line 297-298: RESTORE original file before each run
file.copy(backup_file, par_file, overwrite = TRUE)
# This resets ALL parameters to their original values

# Line 301: Modify ONLY the sensitivity analysis parameters
modify_parameter_file(par_file, parameter_sets[i, ], par_bounds, original_values)
# This changes only BETA, CTMAX, TAB1 (in our example)
# All other parameters (FK, PWP, M, etc.) remain unchanged

# Line 305-310: Run COSERO with modified file
result <- run_cosero(...)
```

**What happens to different parameter groups:**

| Parameter Group | Example | Status in Run #1 | Status in Run #2 |
|----------------|---------|------------------|------------------|
| **Modified by SA** | BETA=2.5 (sampled) | Modified to 2.5 | Modified to 7.1 |
| **Modified by SA** | CTMAX=8.2 (sampled) | Modified to 8.2 | Modified to 4.3 |
| **NOT in SA** | FK=1.0 (from file) | Unchanged (1.0) | Unchanged (1.0) |
| **NOT in SA** | PWP=0.5 (from file) | Unchanged (0.5) | Unchanged (0.5) |
| **NOT in SA** | M=0.7 (from file) | Unchanged (0.7) | Unchanged (0.7) |

---

### 3. Parameter File Modification Details

**Function:** `modify_parameter_file()` at line 360-408

```r
modify_parameter_file <- function(par_file, params, par_bounds, original_values) {
  # Line 361: Read ALL lines from the file
  lines <- readLines(par_file)

  # Line 363: Loop through ONLY the modified parameters
  for (param_name in names(params)) {
    # ... calculate final_value based on modification type ...

    # Line 397-403: Find and update ONLY this parameter
    param_idx <- grep(paste0("^", param_name, "\\b"), lines)
    if (length(param_idx) > 0) {
      value_idx <- param_idx[1] + 1
      lines[value_idx] <- as.character(final_value)
    }
  }

  # Line 407: Write ALL lines back (modified + unmodified)
  writeLines(lines, par_file)
}
```

**Critical insight:**
- The function reads ALL lines from the file
- It modifies ONLY the parameters in `params` (the sensitivity analysis parameters)
- It writes ALL lines back, preserving unmodified parameters

---

### 4. After All Runs

**File:** `05_cosero_sensitivity_analysis.R`, line 318-319

```r
# Restore original parameter file
file.copy(backup_file, par_file, overwrite = TRUE)
```

**What happens:**
- The original parameter file is restored
- The backup file remains for reference

---

## Example Scenario

### Original Parameter File (`para.txt`)

```
BETA
4.5
CTMAX
5
CTMIN
2
FK
1.0
PWP
0.5
M
0.7
TAB1
1
```

### Sensitivity Analysis Setup

```r
# Only analyzing 3 parameters
par_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "TAB1"))
sobol_samples <- generate_sobol_samples(sobol_bounds, n = 50)
```

### Run #1: Parameter Set = {BETA: 2.5, CTMAX: 8.2, TAB1: 1.5}

**Modified `para.txt` during Run #1:**

```
BETA
2.5          ← MODIFIED (absval: replaced with 2.5)
CTMAX
8.2          ← MODIFIED (absval: replaced with 8.2)
CTMIN
2            ← UNCHANGED (not in sensitivity analysis)
FK
1.0          ← UNCHANGED (not in sensitivity analysis)
PWP
0.5          ← UNCHANGED (not in sensitivity analysis)
M
0.7          ← UNCHANGED (not in sensitivity analysis)
TAB1
1.5          ← MODIFIED (relchg: 1.0 * 1.5 = 1.5)
```

### Run #2: Parameter Set = {BETA: 7.1, CTMAX: 4.3, TAB1: 0.8}

**Process:**
1. Restore from backup (resets all parameters)
2. Modify only BETA, CTMAX, TAB1
3. FK, PWP, M, CTMIN remain at original values

**Modified `para.txt` during Run #2:**

```
BETA
7.1          ← MODIFIED (absval: replaced with 7.1)
CTMAX
4.3          ← MODIFIED (absval: replaced with 4.3)
CTMIN
2            ← UNCHANGED (restored from backup, then not modified)
FK
1.0          ← UNCHANGED (restored from backup, then not modified)
PWP
0.5          ← UNCHANGED (restored from backup, then not modified)
M
0.7          ← UNCHANGED (restored from backup, then not modified)
TAB1
0.8          ← MODIFIED (relchg: 1.0 * 0.8 = 0.8)
```

---

## Modification Type Behavior

### 1. `absval` (Absolute Value)

**Example:** BETA with bounds [0.5, 8]

```r
# Original value in file: 4.5
# Sampled value: 2.5
# Final value: 2.5 (original is ignored)
```

### 2. `relchg` (Relative Change - Multiplier)

**Example:** TAB1 with bounds [0.5, 3]

```r
# Original value in file: 1.0
# Sampled value: 1.5 (multiplier)
# Final value: 1.0 * 1.5 = 1.5 (multiplies original)
```

**This is why original_values is needed!**

### 3. `abschg` (Absolute Change - Additive)

**Example:** TCOR with bounds [0, 3]

```r
# Original value in file: 0
# Sampled value: 1.5 (additive)
# Final value: 0 + 1.5 = 1.5 (adds to original)
```

**This is also why original_values is needed!**

---

## Code Locations

| Function | File | Lines | Purpose |
|----------|------|-------|---------|
| `run_cosero_ensemble()` | 05_cosero_sensitivity_analysis.R | 250-329 | Main ensemble orchestration |
| `read_parameter_file()` | 05_cosero_sensitivity_analysis.R | 336-352 | Read specific parameters from file |
| `modify_parameter_file()` | 05_cosero_sensitivity_analysis.R | 360-408 | Modify specific parameters in file |
| Backup creation | 05_cosero_sensitivity_analysis.R | 284-285 | Create backup before modifications |
| Restore before run | 05_cosero_sensitivity_analysis.R | 297-298 | Restore backup before each run |
| Restore after ensemble | 05_cosero_sensitivity_analysis.R | 318-319 | Restore backup after all runs |

---

## Parallel Execution

The same logic applies to `run_cosero_ensemble_parallel()`:

```r
# Line 372-374: Each worker creates backup
backup_file <- paste0(worker_par_file, ".backup")
file.copy(worker_par_file, backup_file, overwrite = TRUE)

# Line 380-381: Restore before each run (in worker loop)
file.copy(backup_file, worker_par_file, overwrite = TRUE)

# Line 384-385: Modify only SA parameters
modify_parameter_file(worker_par_file, parameter_sets[run_i, ],
                     par_bounds, original_values)
```

**Key difference:**
- Each parallel worker gets its own project copy in a temp directory
- Each worker has its own backup and restore cycle
- Unmodified parameters still retain original values

---

## Summary

### What values are used for parameters NOT modified in sensitivity analysis?

**Answer: Their original values from the parameter file.**

### How does this work?

1. **Backup Strategy:** Original file is backed up once at the start
2. **Restore Per Run:** Before each simulation, backup is restored (all parameters reset)
3. **Selective Modification:** Only sensitivity analysis parameters are modified
4. **Preservation:** All other parameters keep their values from the original file
5. **Final Restoration:** Original file is restored after all runs complete

### Why is this design good?

✅ **Safe:** Original file is preserved
✅ **Predictable:** Unmodified parameters always use file values
✅ **Flexible:** Can modify any subset of parameters
✅ **Traceable:** Backup files remain for reference
✅ **Correct:** `relchg` and `abschg` work correctly with original values

### Potential Considerations

⚠️ **File Format Assumption:** Assumes COSERO parameter format is:
```
PARAMETER_NAME
value
```

⚠️ **Regex Matching:** Uses `grep(paste0("^", param_name, "\\b"), lines)` to find parameter names
  - Should work for most cases
  - May fail if parameter names are prefixes of each other (e.g., TAB vs TABL)

⚠️ **Line-by-line Writing:** Preserves file structure exactly as-is
  - Good: Preserves comments, formatting
  - Good: Works with any COSERO parameter file format
