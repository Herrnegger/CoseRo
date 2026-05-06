# COSERO Disaggregation Parameters
## Reference for Hypsometric Band Disaggregation Framework

---

## 1. Overview

Each COSERO zone receives a single zone-mean temperature and precipitation. For zones spanning large elevation ranges this is physically inadequate: snow lines, melt rates, and orographic precipitation all vary strongly with elevation. The disaggregation framework subdivides each zone into `NDC` elevation bands, each running the full hydrological process chain independently, and collapses the results back to the zone mean before routing.

`NDC` (Number of Disaggregation Classes) is the only runtime switch. It is read from `defaults.txt`. When `NDC ≤ 1` the entire framework is a no-op and results are bit-identical to the baseline.

**Five parameters control the disaggregation behaviour:**

| # | Name (para_specs) | Fortran variable | Controls |
|---|---|---|---|
| 60 | `LAPSE_T` | `lapse_t_b(NZ)` | Temperature lapse rate per band |
| 61 | `LAPSE_P` | `lapse_p_b(NZ)` | Precipitation lapse rate per band |
| 62 | `SOILVAR` | `soilvar_b(NZ)` | Within-zone spread of soil storage params |
| 63 | `HYDROVAR` | `hydrovar_b(NZ)` | Within-zone spread of hydraulic routing params |
| 64 | `CTVAR` | `ctvar_b(NZ)` | Within-zone spread of snow melt factor params |

All five are 2D zone-level parameters (`PARA_DIMS = 2`, i.e. `(NB, IZ)`), calibratable via the standard optimisation interface.

---

## 2. Hypsometric Data and Band Elevation Derivation

### 2.1 Input: Hypsometric Curve (`HYPSO_B`)

For each real zone the parameter file provides 21 elevation quantile values at 5 % cumulative area intervals:

| Parameter file columns | Values | Array |
|---|---|---|
| `HYPSO0_`, `HYPSO5_`, ..., `HYPSO100_` | elevation [m] at 0 %, 5 %, ..., 100 % | `HYPSO_B(NB, IZ, 1:21)` |

This defines the hypsometric curve: the elevation that encloses a given fraction of the zone area. Index 1 = minimum elevation (0 % quantile), index 21 = maximum elevation (100 % quantile).

**Compile-time constants** (`module_basic_props.f`):

```fortran
HYPSO_STEP = 5      ! % interval between stored quantiles
NHYPSO     = 21     ! number of stored quantile points (0%,5%,...,100%)
MAXNDC     = 10     ! compile-time upper bound for NDC
```

### 2.2 Deriving Band Mean Elevations

`initCOS_derive_hypsometry()` (called once from `menueCOS` after `zonelinks()`) partitions each zone into `NDC` equal-area bands and interpolates the band midpoint elevation from `HYPSO_B`:

**Band midpoint cumulative area fraction:**

$$p_{\text{mid}}(IDC) = \frac{IDC - 0.5}{NDC}$$

**Array position in HYPSO_B** (0-based fractional index):

$$\text{frac} = p_{\text{mid}} \times \frac{100}{\text{HYPSO\_STEP}}$$

**Linear interpolation** between adjacent stored quantiles $i\_lo$ and $i\_hi = i\_lo + 1$:

$$\text{elev\_band}(IDC) = \text{HYPSO\_B}[i\_lo] \times (1 - w) + \text{HYPSO\_B}[i\_hi] \times w$$

where $w = \text{frac} - (i\_lo - 1)$.

The result is written directly into the existing flat array `elev_b(NZ_band)`. No separate elevation array is added.

**Area fraction per band** (equal-area partition):

$$\text{AFRAC\_DC\_B}(NB, IZ\_band) = \frac{1}{NDC}$$

The zone area carried by each band slot is therefore `dfzon_b(NZ_real) / NDC`.

### 2.3 Band Slot Index Formulas

Real zones occupy `IZ = 1..izone_b(NB)` and flat `NZ = 1..max_nz`. Band slots are appended after them:

$$IZ\_band = izone\_b(NB) + (IZ - 1) \times NDC + IDC$$

$$NZ\_band = max\_nz + (NZ\_real - 1) \times NDC + IDC$$

**Inverse — parent real zone from band slot** (Fortran integer division discards IDC):

$$IZ\_real = \frac{IZ\_band - izone\_b(NB) - 1}{NDC} + 1$$

> **Important:** `TOIZ_B` stores the routing destination (downstream zone) and must **not** be used for parent-zone lookup. The parent is always derived arithmetically from the formula above.

---

## 3. Forcing Disaggregation — `LAPSE_T` and `LAPSE_P`

Applied every timestep in `run_p_t_n_B.f` for each band slot (`IZ > izone_b(NB)`).

### 3.1 Elevation Difference

$$\Delta z_{hm} = \frac{\text{elev\_b}(NZ\_band) - \text{elev\_b}(NZ\_real)}{100} \quad [\text{100 m units}]$$

A negative $\Delta z_{hm}$ means the band is below the zone mean (lowest bands); positive means above.

### 3.2 Temperature Lapse Correction

$$T_{band} = (T_{raw} + \text{TCOR}_{month}) + \text{LAPSE\_T} \times \Delta z_{hm}$$

where $T_{raw}$ is the station input and $\text{TCOR}_{month}$ is the monthly additive bias correction. The lapse rate `LAPSE_T` is typically negative (~−0.65 °C/100 m) so lower bands are warmer and upper bands are colder.

### 3.3 Precipitation Lapse Correction

$$P_{band} = \max\!\left(0,\ (P_{raw} \times \text{PCOR}_{month}) \times (1 + \text{LAPSE\_P} \times \Delta z_{hm})\right)$$

`LAPSE_P` is a fractional scaling per 100 m. A value of 0.05 increases precipitation by ~5 % per 100 m of elevation gain (orographic enhancement).

### 3.4 Monthly Mean Temperature for Thornthwaite ET (`TMMON_B`)

`TMMON_B` is a static parameter used for Thornthwaite potential ET. It is lapse-corrected once at initialisation in `disagg_params_B()`:

$$\text{TMMON\_B}_{band}(NM) = \text{TMMON\_B}_{real}(NM) + \text{LAPSE\_T} \times \Delta z_{hm}$$

This ensures Thornthwaite PET is physically consistent with the band elevation. It is re-applied at every `run()` call so calibration of `LAPSE_T` is reflected immediately.

### 3.5 Parameter Table — Forcing Lapse Rates

| Parameter | Variable | Unit | Lower | Upper | Typical Alpine | Notes |
|---|---|---|---|---|---|---|
| `LAPSE_T` | `lapse_t_b` | °C / 100 m | −1.2 | 1.0 | −0.65 | Negative = T decreases with elevation |
| `LAPSE_P` | `lapse_p_b` | factor / 100 m | −0.5 | 0.5 | 0.05 | 0.05 = +5 % per 100 m gain |

---

## 4. Parameter Disaggregation — `SOILVAR`, `HYDROVAR`, `CTVAR`

Applied once per `run()` call in `disagg_params_B()` before the initialisation loop. Three coefficient-of-variation (CV) parameters spread 14 process parameters across band slots using a lognormal distribution that **preserves the zone mean exactly**.

### 4.1 Lognormal Scaling Formula

For a zone-mean parameter value $\bar{p}$ and CV $= cv$:

**Step 1 — lognormal sigma from CV:**

$$\sigma = \sqrt{\ln(1 + cv^2)}$$

**Step 2 — Hazen plotting position for band IDC:**

$$p_{IDC} = \frac{IDC}{NDC + 1}$$

**Step 3 — standard normal quantile:**

$$z_{IDC} = \Phi^{-1}(p_{IDC})$$

(Acklam rational approximation, error < 1.15 × 10⁻⁹, implemented in `run_disagg_params_B.f`)

**Step 4 — scaled parameter for band IDC:**

$$p_{band}(IDC) = \bar{p} \times \exp\!\left(\sigma \, z_{IDC} - \frac{\sigma^2}{2}\right)$$

The $-\sigma^2/2$ term is the lognormal mean correction; it ensures $\text{mean}(p_{band}) = \bar{p}$.

**Degenerate case:** when $cv = 0$ → $\sigma = 0$ → $p_{band}(IDC) = \bar{p}$ for all bands. Existing runs with no CV columns in the parameter file default to 0 and are therefore backward-compatible.

**Zero-value guard:** when $\bar{p} \leq 0$ the formula is skipped and the value is copied identically (prevents scale errors on zero-initialised or unused parameters).

### 4.2 Sign Convention for `beta_b`

`beta_b` (nonlinearity exponent of the soil drainage curve) is negated: lower elevation bands receive a **higher** beta. This reflects the physical expectation that low-elevation bands tend to have shallower soils with more nonlinear drainage, while higher bands have deeper, more porous soils. The call in code:

```fortran
beta_b(NZ_band) = lognormal_scale(beta_b(NZ_real), cv_hydro, -z_IDC)
```

All other `HYDROVAR` group parameters use the standard (positive) $z_{IDC}$.

### 4.3 Parameter Table — CV Parameters

| Parameter | Variable | Unit | Lower | Upper | Applies to |
|---|---|---|---|---|---|
| `SOILVAR` | `soilvar_b` | dimensionless (CV) | 0.1 | 2.5 | `m_b`, `h1_b`, `pex2_b`, `pex3_b`, `h2_b` |
| `HYDROVAR` | `hydrovar_b` | dimensionless (CV) | 0.1 | 2.5 | `beta_b`*, `kbf_b`, `tab1_b`, `tvs1_b`, `tab2_b`, `tvs2_b`, `tab3_b` |
| `CTVAR` | `ctvar_b` | dimensionless (CV) | 0.1 | 2.5 | `ctmin_b`, `ctmax_b` |

\* `beta_b` uses negated $z_{IDC}$ (lower bands get higher beta).

### 4.4 Affected Parameters and Physical Groupings

**`SOILVAR` group — soil storage geometry**

| Parameter | Description | para_specs range |
|---|---|---|
| `m_b` | Maximum soil water storage [mm] | 1 – 2000 |
| `h1_b` | Threshold for fast subsurface flow [mm] | 0 – 20 |
| `pex2_b` | Percolation exponent to lower zone | 0 – 99999 |
| `pex3_b` | Percolation exponent to groundwater | 0 – 99999 |
| `h2_b` | Threshold for percolation to groundwater [mm] | 0 – 50 |

**`HYDROVAR` group — hydraulic dynamics**

| Parameter | Description | para_specs range |
|---|---|---|
| `beta_b` | Soil drainage nonlinearity exponent [−] | 0 – 10 |
| `kbf_b` | Baseflow recession constant [d] | 1000 – 10000 |
| `tab1_b` | Fast subsurface flow routing time [d] | 0 – 500 |
| `tvs1_b` | Fast subsurface volume threshold [mm] | 0 – 500 |
| `tab2_b` | Slow subsurface flow routing time [d] | 0 – 5000 |
| `tvs2_b` | Slow subsurface volume threshold [mm] | 0 – 10000 |
| `tab3_b` | Very slow subsurface routing time [d] | 0 – 50000 |

**`CTVAR` group — snow melt factors**

| Parameter | Description | para_specs range |
|---|---|---|
| `ctmin_b` | Minimum degree-day melt factor [mm/°C/d] | 0 – 10 |
| `ctmax_b` | Maximum degree-day melt factor [mm/°C/d] | 0 – 15 |

---

## 5. Area-Weighted Aggregation

After all band slots are processed (`run_disagg_aggregate_B.f`), fluxes and states are collapsed back to the real zone using equal-area weights $w = 1/NDC$:

$$X_{real} = \frac{1}{NDC} \sum_{IDC=1}^{NDC} X_{band}(IDC)$$

Applied to: `QABZON_B`, `QAB1–3ZON_B`, `PZON_B`, `PRAINZON_B`, `PSNOWZON_B`, `ETATZON_B`, `MELTZON_B`, `SWWZON_B`, `SHZON_B`, `BW0–3ZON_B`, `BWIZON_B`, `TOTALSZON_B`, `DELTASZON_B`, and others.

After aggregation, `DELTASZON_B` and `TOTALSZON_B` are recomputed from the aggregated components to ensure water balance closure.

`QZU4ZON_T` and `QZU4ZONend_T` for real zones are zeroed inside `disagg_aggregate_B` (not inherited from band slots) so that the hillslope routing step starts clean.

---

## 6. Interaction with Existing Snow Class Disaggregation (`tvar_b`)

The existing `tvar_b` parameter applies 7 Gaussian temperature quantiles within each zone for snow class heterogeneity. With hypsometric bands active, the physical elevation-driven T gradient is explicitly represented per band. The two mechanisms are orthogonal: each band still runs the full `IKL` snow class loop. `tvar_b` is copied identically to all band slots and remains active; it may become partially redundant for the elevation component but is retained for now.

---

## 7. Parameter File Columns

All five disaggregation parameters and the 21 hypsometric curve columns must be present in `para_*.txt`. Column names with `_B` suffix match the Fortran substring checks:

| Column name | Fortran check | Notes |
|---|---|---|
| `LAPSE_T_` | `(:8) .eq. 'LAPSE_T_'` | One value per zone |
| `LAPSE_P_` | `(:8) .eq. 'LAPSE_P_'` | One value per zone |
| `SOILVAR_B` | `(:8) .eq. 'SOILVAR_'` | `_B` suffix in file, 8-char check |
| `HYDROVAR_B` | `(:9) .eq. 'HYDROVAR_'` | `_B` suffix in file, 9-char check |
| `CTVAR_B` | `(:6) .eq. 'CTVAR_'` | `_B` suffix in file, 6-char check |
| `HYPSO0_` .. `HYPSO100_` | `(:7) .eq. 'HYPSO0_'` etc. | 21 columns, elevations in [m] |

Total parameter file columns: **195** (169 original + 23 Phase 1 + 3 Phase 2).

---

## 8. Initialisation Call Order

```
menueCOS()
  READ_PARAFILE()                reads lapse rates, CV params, HYPSO_B
  zonelinks()                    builds NB_NZ / IZ_NZ for real zones and band slots
  initCOS_derive_hypsometry()    → elev_b(NZ_band), AFRAC_DC_B

run()                            called each optimisation iteration / simulation run
  disagg_params_B()              → copies + lognormal-scales params to band slots
                                    lapse-corrects TMMON_B, scales dfzon_b by 1/NDC
  init loop (IZ=1..izone_b_dis)  initialises band slot states from INI params
  timestep loop
    run_p_t_n_B()                → lapse-corrected T, P per band slot
    run_snow_B() / run_soil_B() / run_evapo_B() / ...  (band slots only, NDC>1)
    disagg_aggregate_B()         → area-weighted collapse to real zones
    run_sbroute_T() / run_route_q*()   (real zones only)
```

---

## 9. Debugging and Verification

**`output/debug_lapse.txt`** — written by `run_p_t_n_B.f` for the first N timesteps.
Columns: `ND, NB, IZ, IZ_real, NZ_rf, elev_band, elev_real, dElev_hm, T_raw, T_cor, lapse_t, TZON, P_raw, P_cor, lapse_p, PZON`

**`output/debug_disagg_params.txt`** — written by `disagg_params_B()` at each call.
Columns: `NB, IZ, is_band, PCOR_1..12, TMMON_1..12`
PCOR should be identical across all bands; TMMON should decrease with band elevation (negative lapse rate).

**Quick R check for precipitation lapse:**
```r
d <- read.csv("output/debug_lapse.txt")
d_rain <- subset(d, P_raw > 0)
d_rain$PZON_check <- d_rain$P_raw * d_rain$P_cor *
                     (1 + d_rain$lapse_p * d_rain$dElev_hm)
# PZON and PZON_check should match
```
