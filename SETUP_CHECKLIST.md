# Setup Checklist for Sensitivity Report

Complete this checklist before rendering your `sensitivity_report.qmd` to ensure everything is ready.

---

## Prerequisites (One-Time Setup)

### Software Installation

- [ ] **Quarto installed** (version 1.4 or higher)
  - Download from: https://quarto.org/docs/get-started/
  - Verify: `quarto --version`

- [ ] **R installed** (version 4.3.0 or higher recommended)
  - Download from: https://cran.r-project.org/

- [ ] **RStudio or Positron installed** (optional but recommended)
  - RStudio: https://posit.co/download/rstudio-desktop/
  - Positron: https://github.com/posit-dev/positron

- [ ] **TinyTeX installed** (for PDF rendering)
  - Run in terminal: `quarto install tinytex`
  - Or in R: `install.packages("tinytex"); tinytex::install_tinytex()`

### R Packages

- [ ] **Required R packages installed**
  ```r
  install.packages(c(
    "knitr",
    "kableExtra",
    "ggplot2",
    "dplyr",
    "tidyr",
    "data.table"
  ))
  ```

---

## File Checklist

### Core Files (Already Present)

- [x] **sensitivity_report.qmd** - Main report file
- [x] **QUICKSTART.md** - Quick start guide
- [x] **references.bib** - Bibliography (9 references)
- [x] **apa.csl** - Citation style file ✅ (downloaded)
- [x] **figures/README.md** - Figure directory guide ✅ (created)
- [x] **CATCHMENT_TEMPLATE.md** - Template for catchment info ✅ (created)

### Files You Need to Provide

- [ ] **10 PNG figures** in the `figures/` directory:
  - [ ] 01_sobol_indices_nse.png
  - [ ] 02_sobol_indices_kge.png
  - [ ] 03_dotty_plots_nse.png
  - [ ] 04_dotty_plots_kge.png
  - [ ] 05_best_nse_dotty.png
  - [ ] 06_best_kge_dotty.png
  - [ ] 07_nse_vs_kge_comparison.png
  - [ ] 08_parameter_correlations_nse.png
  - [ ] 09_sobol_nse_kge_comparison.png
  - [ ] 10_behavioral_parameter_ranges.png

**Action needed:** Copy your PNG files from their current location to `d:\CORSERO PRIVATE\COSERO-R\figures\`

**Alternative:** Update all figure paths in `sensitivity_report.qmd` to point to your actual figure location.

---

## Content to Complete in sensitivity_report.qmd

### Section 1: Study Area (Lines 90-106)

- [ ] **Line 92:** Location (geographic location and coordinates)
- [ ] **Line 93:** Catchment area (in km²)
- [ ] **Line 94:** Elevation range (min-max in m a.s.l.)
- [ ] **Line 95:** Mean annual precipitation (in mm/year)
- [ ] **Line 96:** Mean annual temperature (in °C)
- [ ] **Line 97:** Land use description (dominant land cover types)
- [ ] **Lines 102-105:** Data sources and periods

**Tip:** Use `CATCHMENT_TEMPLATE.md` to organize this information first.

### Section 2: Model Configuration (Line 128)

- [ ] **Line 128:** Simulation period (e.g., 2015-2022)

### Section 3: Computational Details (Line 212)

- [ ] **Line 212:** Total computational time (replace `[X]` with actual hours)
- [ ] **Line 212:** Hardware description (optional but useful)

Example: "Approximately 18 hours of computational time on Intel i7-10700K CPU @ 3.80GHz with 32GB RAM"

### Section 4: Best KGE Simulation (Line 1050)

- [ ] **Line 1050:** Best KGE parameter values

Replace `[Parameter values from best KGE file]` with actual values:
```markdown
- M = [value] mm/°C/day
- TAB1 = [value]°C, TAB2 = [value]°C, TAB3 = [value]°C
- TVS1 = [value]°C, TVS2 = [value]°C
- KBF = [value] days
- H1 = [value] mm, H2 = [value] mm
```

### Section 5: GitHub Repository (Line 1425)

- [ ] **Line 1425:** Add your GitHub repository URL (if applicable)
- If no GitHub repo, remove this line or write "Available upon request"

---

## Validation Checklist

### Before First Render

- [ ] All placeholder text `[Insert ...]` has been replaced
- [ ] All 10 figures are accessible at the specified paths
- [ ] `references.bib` is in the same directory as `.qmd` file
- [ ] `apa.csl` is in the same directory as `.qmd` file
- [ ] Author name updated (Line 4) if needed

### Test Rendering

- [ ] **Test HTML render first** (fastest to check for errors)
  ```bash
  quarto render sensitivity_report.qmd --to html
  ```

- [ ] Check for errors in R code chunks (if any)
- [ ] Verify all figures display correctly
- [ ] Check that citations render properly (e.g., "Kling et al., 2015")
- [ ] Verify cross-references work (e.g., @fig-sobol-comparison)

- [ ] **Test PDF render** (takes longer but essential for final output)
  ```bash
  quarto render sensitivity_report.qmd --to pdf
  ```

- [ ] Check PDF formatting (margins, fonts, page breaks)
- [ ] Verify all figures are high quality
- [ ] Check table formatting

- [ ] **Test DOCX render** (optional)
  ```bash
  quarto render sensitivity_report.qmd --to docx
  ```

---

## Common Issues and Solutions

### Issue 1: "Error: File 'figures/XX.png' not found"
**Solution:**
- Copy your PNG files to `figures/` directory, OR
- Update figure paths in the `.qmd` file to match your actual location

### Issue 2: "TinyTeX not found" or PDF won't render
**Solution:**
```bash
quarto install tinytex
```

### Issue 3: Citations not appearing
**Solution:**
- Ensure `references.bib` is in same directory as `.qmd`
- Check that citation keys match (e.g., `@kling2015` matches the key in .bib)

### Issue 4: R code chunk errors
**Solution:**
- Most code chunks are set to `eval: false` (won't execute)
- If you get errors, check that required R packages are installed
- You can set `error: true` in YAML to continue rendering despite errors

### Issue 5: Figure numbering is wrong
**Solution:**
- Ensure each figure has unique `#| label: fig-something`
- Labels must start with `fig-` prefix for automatic numbering

---

## Final Quality Check

Before sharing your report:

- [ ] All sections complete (no placeholder text remaining)
- [ ] All 10 figures present and display correctly
- [ ] Table of contents generates correctly
- [ ] Cross-references work (click @fig-X, @tbl-X in HTML/PDF)
- [ ] Citations appear in text and reference list
- [ ] Page numbers correct (PDF)
- [ ] No obvious formatting issues
- [ ] Spell-check completed
- [ ] Co-author/advisor reviewed (if applicable)

---

## Rendering Commands Reference

### Single Format
```bash
# HTML (fastest, good for checking)
quarto render sensitivity_report.qmd --to html

# PDF (publication quality)
quarto render sensitivity_report.qmd --to pdf

# Word (editable)
quarto render sensitivity_report.qmd --to docx
```

### All Formats
```bash
quarto render sensitivity_report.qmd
```

### Preview with Live Reload
```bash
quarto preview sensitivity_report.qmd
```
(Opens in browser, auto-refreshes on save)

---

## After Successful Render

Your output files will be in the project directory:
- `sensitivity_report.html`
- `sensitivity_report.pdf`
- `sensitivity_report.docx`

Optional: Create an `_output/` directory to keep things organized:
```bash
mkdir _output
mv sensitivity_report.* _output/
```

---

## Need Help?

1. **Check the QUICKSTART.md** for basic usage
2. **Check Quarto documentation**: https://quarto.org/docs/guide/
3. **Check this project's README files**:
   - `figures/README.md` - Figure requirements
   - `CATCHMENT_TEMPLATE.md` - Catchment info template
4. **GitHub Issues**: If this is on GitHub, report issues there

---

**Checklist created:** 2026-01-10
**Last updated:** 2026-01-10
**For use with:** sensitivity_report.qmd v1.0
