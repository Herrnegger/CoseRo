# COSERO spec — optional parameter columns default to "off" when absent

## About CoseRo (context for this spec)

**CoseRo** is an R package that wraps the COSERO hydrological model. It does not
reimplement the model — it **drives `COSERO.exe`** and reads its output:

- **Execution:** CoseRo writes the configuration (`input/defaults.txt`) and the
  parameter table (`input/para*.txt`), then launches `COSERO.exe` as a
  subprocess, feeding it the interactive answers via a redirected stdin
  (run mode, warm/cold start, etc.). COSERO writes its result files to
  `output/`; CoseRo never touches the model internals.
- **Parameter files:** the tabular parameter file (`para*.txt`) is the contract
  between CoseRo and COSERO. CoseRo reads it (`fread`), can modify individual
  parameter columns for calibration/sensitivity, writes it back, and COSERO.exe
  consumes it on the next run. **This is why column presence matters** — CoseRo
  may produce parameter files with or without the optional columns below.
- **Output:** CoseRo reads `COSERO.runoff`, `statistics.txt`, `COSERO.plus*`,
  `monitor*` etc. to compute metrics (NSE/KGE), run Sobol sensitivity analysis,
  and DDS/SCE-UA calibration — all by repeatedly modifying the parameter table
  and re-running `COSERO.exe`.

So COSERO.exe is the compute engine; CoseRo is the orchestration, I/O, and
analysis layer around it. The change specified here lives entirely inside
`COSERO.exe` (its parameter-file reader); the R side already copes (see the
R-side note at the end).

---

**Goal:** one COSERO.exe runs both old and new parameter files. If the
disaggregation (NDC) and hydraulic-lift (FHL) columns are **not present** in the
parameter file, the corresponding feature is switched off internally and the run
is bit-identical to the classic model. This removes the need for separate
main-branch and dev-branch executables.

## Requirement

Read the parameter-file table **by header name**, not by fixed column position.
Build a `name -> column index` map from the header row, then for each optional
parameter below: **if its column is absent, set the in-memory value to the
"off" default** (do not error, do not read a neighbouring column).

## Optional columns and their "off" defaults

| Column        | Absent → value | Effect when off |
|---------------|----------------|-----------------|
| `NDC_`        | `1`            | disaggregation disabled (NDC ≤ 1 already = off) |
| `LAPSE_T_`    | `0.0`          | no temperature lapse within zone |
| `LAPSE_P_`    | `0.0`          | no precipitation lapse within zone |
| `SOILVAR_`    | `0.0`          | CV = 0 → no within-zone soil-parameter spread |
| `HYDROVAR_`   | `0.0`          | CV = 0 → no within-zone routing-parameter spread |
| `CTVAR_`      | `0.0`          | CV = 0 → no within-zone melt-factor spread |
| `FHL_`        | `0.0`          | hydraulic lift off → original ET (reproduces baseline) |
| `HYPSO0_` … `HYPSO100_` | (only read when `NDC_ > 1`) | zone geometry; not needed when disaggregation is off |

**Important:** the "off" CV default is **0.0**, NOT the `0.5` calibration default
used elsewhere — 0.0 is what reproduces the non-disaggregated model.

> **As built (see IMPLEMENTATION below):** these are the only **optional**
> columns. `NDC_` absent ⇒ NDC=1 (off). For the *other* disaggregation columns
> the final behaviour is **stricter than this table** — if `NDC_ > 1` is
> requested but `HYPSO0_/HYPSO100_`, `LAPSE_T_`, `LAPSE_P_`, `SOILVAR_`,
> `HYDROVAR_` or `CTVAR_` is missing, NDC is **forced off for all basins** with
> a warning, rather than running with those defaulted to 0. `FHL_` absent ⇒
> lift off only (independent of NDC).

## Notes

- These defaults already correspond to existing no-op behaviour: `NDC ≤ 1`
  disables disaggregation and `FHL = 0` reproduces the original ET. So "absent ⇒
  off" introduces no new physics — only defensive, name-based parsing.
- Apply the existing clamp on read: `FHL = min(1., max(0., FHL))`.
- If the current reader uses fixed column order, switching to header-name lookup
  is the real change here. If it already reads by header name, this is small.

## Acceptance test

1. Run an **old** parameter file (no NDC/FHL/LAPSE/CV/HYPSO columns) → results
   bit-identical to the previous main-branch exe.
2. Run a **new** parameter file (all columns present, NDC > 1, FHL > 0) →
   disaggregation + hydraulic lift active, as today.
3. Run a new file with **NDC_ = 1** → identical to case 1 (feature off via value).

## R-side note (no change needed)

CoseRo already tolerates both file shapes: `read_cosero_parameters()` (fread)
reads whatever columns exist, and the parameter-modification path
(`find_parameter_column()`) warns-and-skips any parameter whose column is absent.
So the unified exe is the only piece required to collapse the exe divergence.

---

# IMPLEMENTATION (as built, 06/2026)

Implemented in `COSERO.exe`. The reader (`initCOS_read_paraf.f`) already mapped
columns **by header name** and consumed every parameter under `if (I_xxx > 0)`,
so "absent ⇒ skip" was half-present. The gaps that were fixed:

### 1. Column count is now discovered, not hardcoded
- `NPARA` (in `module_basic_props.f`) is now the compile-time **maximum/buffer**,
  not a required exact count.
- New global `NPARA_FILE` (`module_var_params.f`): the **actual** column count.
  `READ_PARAFILE` reads the header **line as a string and tokenizes it** (split
  on space/tab) → `NPARA_FILE`. All loop bounds — header map, first-pass read,
  data read — use `NPARA_FILE`. `>NPARA` columns ⇒ clean stop (errorcode 24).
- `datalog_save_parat.f` writes `NPARA_FILE` columns back (local `NPW`, falls
  back to `NPARA`), so a file round-trips with its own shape.

### 2. "Off" defaults actually initialised
`alloc_varparams.f` now zeroes `lapse_t_b, lapse_p_b, soilvar_b, hydrovar_b,
ctvar_b, fhl_b` (previously only `fhl_b`). `NDC_B` already defaults to 1.
Absent optional column ⇒ its `I_xxx` stays 0 ⇒ assignment skipped ⇒ off-default
stays. Off defaults exactly as in the table above (CV = **0.0**, not 0.5).

### 3. Required vs optional validation (STEP 1b)
Because every column is consumed under `if (I_xxx>0)`, a missing **required**
column would silently run on uninitialised memory. After the header map,
`READ_PARAFILE` checks **each required column index > 0** via helper `req_col`
(prints `MISSING required column: X` per miss). Any required column absent ⇒
stop (errorcode 25). The **only** exempt (optional) columns are:
`NDC_, LAPSE_T_, LAPSE_P_, SOILVAR_, HYDROVAR_, CTVAR_, FHL_, HYPSO0_..HYPSO100_`.
Absent optional columns are reported informationally via helper `opt_col`
(`note: optional column X absent -> <effect>`). Monthly 12-blocks (PCor, TCor,
TMMon, INTMAX, ETVEGCOR, DAYSDRY, DAYSWET, ETSEAS) are validated by first+last
member as block sentinels.

### 4. Disaggregation safety: NDC forced off if its support columns are missing
This is **stricter than the original spec**, by request. NDC>1 needs supporting
columns to run meaningfully. If any basin requests `NDC_ > 1` **but any of**
the following is absent, the reader forces `NDC_B = 1` for **ALL** basins and
prints a loud WARNING box (it does NOT stop):

`HYPSO0_`, `HYPSO100_`, `LAPSE_T_`, `LAPSE_P_`, `SOILVAR_`, `HYDROVAR_`, `CTVAR_`

(`HYPSO0_/HYPSO100_` are the band-geometry sentinels; without geometry the bands
cannot be built, so this is the hard dependency. The lapse/CV columns are
included so a half-configured disaggregation never runs silently.) When NDC is
forced off, `initCOS_derive_hypsometry` self-guards (`if NDC_B(NB)<=1 cycle`)
and no-ops. `FHL_` is independent — its absence only disables hydraulic lift,
never NDC.

### Files changed
| File | Change |
|---|---|
| `module_basic_props.f` | `NPARA` reframed as max/buffer (tidy + comments) |
| `module_var_params.f` | new `NPARA_FILE` global |
| `initCOS_read_paraf.f` | header tokenizer → `NPARA_FILE`; bounds via `NPARA_FILE`; STEP 1b required-check (`req_col`) + optional report (`opt_col`); NDC force-off block; helpers at file foot |
| `alloc_varparams.f` | zero the 5 missing optional off-defaults |
| `datalog_save_parat.f` | write `NPARA_FILE` columns (round-trip) |
| `datalog_write_errorfile.f` | errorcode 24 (too many cols) + 25 (required missing) |

### Behaviour summary
- Old classic file (no NDC/FHL/LAPSE/CV/HYPSO) → runs, optional notes printed,
  classic model (bit-identical intent).
- New full file, NDC>1, FHL>0 → disaggregation + lift active.
- NDC>1 but a support column missing → **WARNING, NDC forced to 1**, runs.
- A **required** column missing → stop, names the column(s).
- `>NPARA` columns → stop (raise `NPARA`).

### Acceptance (build & run)
Covers the original 3 tests plus: remove `HYPSO100_` (or `LAPSE_T_`, `SOILVAR_`,
`HYDROVAR_`, `CTVAR_`) from a NDC>1 file → WARNING box + runs as NDC=1 (no crash,
no false required-stop); remove a required column (e.g. `FK_`) → stop naming it.
