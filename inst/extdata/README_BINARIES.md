# COSERO Binaries Setup

This directory should contain `cosero_binaries.zip` for distribution with the package.

## Current build (dev/spatial-disaggregation)

`cosero_binaries.zip` and `COSERO_Wildalpen_agreggated.zip` bundle a
**Lhotse 0.9.6** build (`COSERO_Lhotse.exe` from
`COSEROgit/x64/Release_X64/`, renamed to `COSERO.exe` inside the zips, md5
`f8ffcadc79ec72fb05cf1e921a8b680a`) with `libiomp5md.dll`. Updated
2026-09-04, replacing the previous 0.9.6 build (md5 `387c2814...`, see
gotcha #61 in CLAUDE.md). This build adds:

- **OUTPUTTYPE = 0** — calibration mode (writes only `COSERO.runoff` +
  `statistics.txt`); used by the optimizers by default.
- **NDC hypsometric disaggregation** and the `FHL_` hydraulic-lift parameter.

**Note:** the new build produces measurably different results than the
previous one on the same project/parameters (verified on the aggregated
Wildalpen example: subbasin 0001 NSE 0.7668→0.7649, KGE 0.8224→0.8675,
BETA 0.9861→0.9392 — same magnitude of change on subbasins 0002/0003). This
was confirmed as an expected/intentional change in the underlying model
code, not a packaging error, so `COSERO_Wildalpen_agreggated.zip`'s
`output/` was regenerated with the new exe to keep the shipped example
internally consistent (input + exe + output all match). If you're
comparing results against older calibration runs, re-run the baseline
with the current exe rather than assuming the old numbers still apply.

The plain `COSERO_Wildalpen.zip` still ships the previous exe. The `main` branch
exe does **not** support OUTPUTTYPE = 0, NDC, or `FHL_`.

### Updating the bundled exe

Both zips were rebuilt with R's `utils::zip()` (not an external `zip`/`7z`
tool) to avoid depending on tools that may not be on PATH:

```r
# cosero_binaries.zip: exe + dll only, flat structure
setwd("path/to/staging/dir")  # containing COSERO.exe + libiomp5md.dll
utils::zip("inst/extdata/cosero_binaries.zip", files = c("COSERO.exe", "libiomp5md.dll"))

# COSERO_Wildalpen_agreggated.zip: preserves input/ and output/ subfolders.
# Extract the existing zip, replace COSERO.exe + libiomp5md.dll at the root,
# regenerate output/ by running the project with the new exe, then re-zip
# the whole staging directory (relative paths, so input/output stay as
# subfolders rather than being flattened):
setwd("path/to/extracted/wildalpen_agreggated")
utils::zip("inst/extdata/COSERO_Wildalpen_agreggated.zip",
           files = list.files(".", recursive = TRUE))
```

Verify afterward with `list_package_binaries()` and by running
`setup_cosero_project_example_aggregated()` + `run_cosero()` end-to-end —
`system.file()` picks up the local `inst/` copy under `devtools::load_all()`,
so this can be tested before the package is reinstalled.

## Creating cosero_binaries.zip

### Required Files

The zip file must contain the COSERO executable and all required DLL files at the **root level** (not in subdirectories):

```
cosero_binaries.zip
├── COSERO.exe
├── [required_dll_1].dll
├── [required_dll_2].dll
└── [any other required dlls]
```

### Steps to Create the Zip

1. **Gather all required files**:
   - COSERO.exe
   - Any DLL files that COSERO.exe depends on (check with Dependency Walker or similar if unsure)
   - Common DLLs might include: runtime libraries, math libraries, etc.

2. **Create the zip file**:

   **Windows (PowerShell):**
   ```powershell
   # Navigate to folder containing COSERO.exe and DLLs
   cd C:\path\to\cosero\binaries

   # Create zip (PowerShell 5.0+)
   Compress-Archive -Path *.exe,*.dll -DestinationPath cosero_binaries.zip
   ```

   **Windows (7-Zip):**
   ```
   7z a cosero_binaries.zip COSERO.exe *.dll
   ```

   **R:**
   ```r
   # Set working directory to where binaries are located
   setwd("C:/path/to/cosero/binaries")

   # Get all exe and dll files
   files <- list.files(pattern = "\\.(exe|dll)$", ignore.case = TRUE)

   # Create zip
   zip("cosero_binaries.zip", files = files)
   ```

3. **Place the zip file**:
   ```
   COSERO-R/inst/extdata/cosero_binaries.zip
   ```

4. **Verify contents**:
   ```r
   library(COSEROR)
   list_package_binaries()
   ```

### File Size Considerations

If the zip file is large (>10 MB), consider:

- **Git LFS (Large File Storage)**: Track the zip with Git LFS
  ```bash
  git lfs track "inst/extdata/cosero_binaries.zip"
  git add .gitattributes
  git add inst/extdata/cosero_binaries.zip
  git commit -m "Add COSERO binaries with LFS"
  ```

- **.gitignore alternative**: If you prefer not to commit binaries to git, add to `.gitignore`:
  ```
  inst/extdata/cosero_binaries.zip
  ```
  Then distribute the zip separately (e.g., via file sharing, separate download).

### Testing the Setup

After creating the zip, test the setup function:

```r
# Load package functions
devtools::load_all()

# Test project creation
setup_cosero_project(
  project_path = "C:/temp/test_cosero_project",
  cosero_bin_source = "package"
)

# Verify COSERO.exe was extracted
file.exists("C:/temp/test_cosero_project/COSERO.exe")
```

## Alternative: Manual Binary Distribution

If you prefer not to include binaries in the package, users can:

1. Download COSERO binaries separately
2. Use `cosero_bin_source` parameter:
   ```r
   setup_cosero_project(
     "C:/my_project",
     cosero_bin_source = "C:/COSERO/installation/folder"
   )
   ```

## Security Note

Ensure you have permission to distribute COSERO.exe and its DLLs. Check:
- Software license terms
- Institutional agreements
- Copyright restrictions

For teaching/research within your institution, this is typically acceptable.
