# COSERO Binaries Setup

This directory should contain `cosero_binaries.zip` for distribution with the package.

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
