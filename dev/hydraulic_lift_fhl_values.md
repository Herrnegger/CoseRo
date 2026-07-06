# Hydraulic Lift — A Priori `fhl` Values by Land Cover

**Companion to:** `hydraulic_lift_hydlift_B.md`
**Scope:** A priori parameterization of the hydraulic-lift efficiency parameter `fhl_b`
as a function of COSERO land-cover class. Elevation dependence is **out of scope here**
(deferred — see section 6).

---

## 1. What `fhl` is — and the rule for assigning a priori values

`fhl` is a single dimensionless coefficient (0–1) **per land-cover class**: the maximum
fraction of the unmet, stress-weighted transpiration demand that deep roots can supply
from the deep stores (`BW2`, then `BW3`).

The governing demand term in `hydlift_B` is:

```
DEF = fhl · ETPRZON_B · (1 - BFALFZON_B)        (applied only where TSOIL > 0)
```

**The key principle for parameterization — capacity vs. opportunity:**

> `fhl` encodes deep-root **capacity** (how much a vegetation type *can* lift).
> The runtime gates decide the **opportunity** (whether conditions call for lift at all).

Two runtime gates, both already computed each step, supply all the seasonality and
state-dependence:

1. **`(1 - BFALF)`** — high only when the top soil is actually dry/stressed. When the
   soil is wet, `BFALF ≈ 1`, so `(1 - BFALF) ≈ 0` and lift is off automatically.
2. **`TSOIL > 0`** — lift is suppressed on frozen ground.

Consequences for assigning values:
- `fhl` is a **single annual value per class**, *not* monthly. The seasonality is owned by
  the gates and by `ETVEGCOR` (section 3) — a monthly `fhl` would double-count it.
- Values are assigned **purely on deep-root capability** (rooting depth, perennial vs.
  seasonal roots, drought-buffering physiology), *not* on how wet or seasonal the class
  typically is. The model suppresses lift when inappropriate at runtime.
- This is why **wetlands get a high value despite often being wet**: when wet, the
  `(1 - BFALF)` gate keeps lift off regardless; the high `fhl` only acts during dry-surface
  spells, when phreatophyte deep-water access is exactly what should be represented.

---

## 2. A priori table

COSERO land-cover classes (interception/`ETVEGCOR` scheme):

| Code | German name | Land cover | `fhl` (a priori) | Calibration ceiling |
|---|---|---|---|---|
| 1 | Bebaute Siedlungsflächen | built-up areas | 0.00 | 0.00 |
| 2 | Ackerland | farmland | 0.05 | ~0.15 |
| 3 | Grünland | meadows and pastures | 0.15 | ~0.25 |
| 4 | Laubwälder | deciduous forests | 0.32 | ~0.40 |
| 5 | Nadelwälder | coniferous forests | 0.42 | ~0.50 |
| 6 | Mischwälder | mixed forests | 0.37 | ~0.45 |
| 7 | Vegetationsarme Flächen | sparsely vegetated areas | 0.02 | ~0.05 |
| 8 | Gletscher | glaciers | 0.00 | 0.00 |
| 9 | Wasserflächen | water bodies | 0.00 | 0.00 |
| 10 | Feuchtgebiete | wetlands | 0.42 | ~0.55 |

Hard parameter bound: **0–1** (for the optimiser). A priori values and plausible ceilings
sit well below 1 — `fhl = 1` would mean deep roots meet 100% of unmet demand, decoupling
transpiration from top-soil moisture entirely, which is physically extreme.

---

## 3. Interaction with `ETVEGCOR` — why these magnitudes are justified

At the point `hydlift_B` reads it, `ETPRZON_B` has **already** been multiplied by
`ETVEGCOR(NC, month)` — a vegetation- and season-corrected demand, *not* raw PET.

Crucially, for the deep-rooted classes the growing-season `ETVEGCOR` is **≥ 1.0**
(deciduous/mixed peak 1.1–1.2, conifers ~1.05, wetlands up to 1.2): COSERO *amplifies*
PET in summer for these canopies, reflecting that dense, deep-rooted vegetation transpires
more than reference PET when water is available.

This matters for two reasons:

1. **It removes the double-counting concern.** A worry with high `fhl` would be that it
   re-supplies demand a canopy-limit correction already removed. But `ETVEGCOR` *raises*
   demand for exactly the forest/wetland classes — so applying `fhl ≈ 0.4` to that
   already-inflated demand is supplying part of a correctly scaled-up demand, not
   double-counting. The higher magnitudes map cleanly to observed fluxes.

2. **It reinforces the annual-not-monthly decision.** The monthly vegetation/phenology
   signal is *already* carried by `ETVEGCOR`. Stacking a monthly `fhl` on top would be a
   third seasonal layer (alongside `ETVEGCOR` and the `(1 - BFALF)` gate). Keeping `fhl`
   a pure annual capacity term is therefore the correct structure.

**Calibration note:** `ETVEGCOR` and `fhl` both scale forest dry-season ET and are
therefore *not independent*. Because `ETVEGCOR` is a fixed a priori lookup table, `fhl`
calibrates against a fixed `ETVEGCOR` and the entanglement is harmless. **Do not calibrate
`ETVEGCOR` and `fhl` together** — they would be strongly correlated.

---

## 4. Empirical basis (literature)

The forest/wetland magnitudes are anchored in field evidence for deep-water subsidies
during surface droughts. Because `fhl` is defined as the *fraction of unmet demand met from
depth*, the documented buffering fractions map onto `fhl`. Full citations with DOIs in
section 8.

> **Note on the source figures.** The values below were checked against the primary
> literature; several attributions in earlier drafts were corrected (year, journal, and the
> precise quantity reported). Read the qualifications carefully — the headline percentages are
> often more specific (shrubland, tropical) than "temperate forest," so they bound the
> *plausible range* rather than pin a temperate-forest value.

**Hydraulic redistribution — magnitude of the transpiration subsidy:**
- **Ryel et al. (2002)** simulated hydraulic redistribution in a stand of *Artemisia
  tridentata* (big sagebrush, semi-arid shrub) and found it could increase whole-canopy
  transpiration by **up to ~19%** over a 100-day drying cycle. This is the firmest single
  number, but it is for a semi-arid shrub, not temperate forest — so it anchors the *lower*
  end of the forest range, with forest taproot systems plausibly higher.
- **Dawson (1993)** — the foundational hydraulic-lift study (sugar maple, *Acer saccharum*):
  isotopic evidence that nocturnally lifted water is a substantial source for the tree and its
  neighbours. *(Earlier drafts cited "Dawson 1996"; the HR-magnitude paper is 1993.)*

**Deep-root uptake fraction under experimental drought:**
- **Markewitz et al. (2010)**, throughfall-exclusion experiment in a seasonal Amazon forest
  (Tapajós): simulated deep-root uptake supplied **~20% of water demand from 2.5–5.5 m and
  ~10% from 5.5–11.5 m** (so on the order of ~30% combined across that deep profile) when
  surface soils were depleted. *(Earlier drafts compressed this to "~30% from 2.5–11.5 m";
  the depth-resolved split is the accurate statement.)* Tropical setting — an analogue, not a
  temperate prior.

**Deep-water reliance under prolonged dry seasons (East-Africa context):**
- **Miguez-Macho & Fan (2021)** — globally **~18% of annual transpiration** originates from
  deeper unsaturated soil/rock moisture and groundwater; in the southern Amazon in August
  this rises to **~60–90%**. These are deep-weathered tropical-regolith extremes; they justify
  letting *calibration* explore high in East-African dry-season catchments, **not** the
  temperate priors.
- Isotope/sap-flow field studies report deep-water contributions to transpiration of **~21–90%
  during drought** in tropical trees, but note the *absolute* quantity is often small because
  trees cut sap flow under drought. Relevant caution: a high `fhl` should not be read as a
  large absolute lift volume — the runtime gates and the demand level govern that.

**Actual-vs-potential ceiling (plausibility check, not a citable figure):**
- Closed-canopy forest transpiration is commonly observed to cap well below reference PET even
  when well-watered. Combined with `ETVEGCOR ≥ 1` in summer for forest/wetland classes, a
  dry-spell deep-root subsidy of ~0.30–0.45 of *demand* is plausible rather than excessive.
  *(Stated as a general observation; no specific publication is cited here, so no DOI is
  given — do not attribute this to a particular institute without verifying the source.)*

**Physiological note:** deep roots are typically a small fraction of total root biomass
(global root profiles put ~90–95% of root biomass in the top ~0.7–1.1 m; Schenk & Jackson
2002) yet can sustain a disproportionate share of transpiration during drought, because deep
roots access water the shallow profile has lost.

> Caveat: the above are field-measured *buffering fractions* for specific species/biomes,
> used to rank and scale a model-specific lumped coefficient. They are **not** direct
> measurements of `fhl`, which has no published values. Treat the **relative ordering** across
> classes as better constrained
> than any absolute magnitude.

---

## 5. Class-by-class rationale

| Code | Class | `fhl` | Rationale |
|---|---|---|---|
| 1 | built-up areas | 0.00 | No transpiring vegetation; sealed/impervious surfaces. No lift pathway exists. |
| 2 | farmland | 0.05 | Shallow, seasonal root systems (annual crops, ploughed horizon). Roots rarely reach the deep stores; the class is quickly stressed by topsoil drying and out of season has near-zero deep-root activity. Kept just above zero. |
| 3 | meadows and pastures | 0.15 | Perennial grass/herb swards with somewhat deeper, denser root networks than row crops, giving mild but real buffering. Documented hydraulic redistribution in grass/shrub systems (e.g. *Artemisia* ~19%, Ryel et al. 2002) anchors the low-moderate value. |
| 4 | deciduous forests | 0.32 | Deep root systems capable of significant lift, but many temperate broadleaves adopt a relatively water-saving strategy (earlier stomatal closure) under drought, so set below conifers. Consistent with HR transpiration subsidies in the ~0.2–0.4 range. |
| 5 | coniferous forests | 0.42 | Deep perennial taproots; evergreen canopy maintains transpiration well into dry spells, maximising reliance on deep water when the surface dries. Highest forest value. Supported by deep-uptake evidence sustaining dry-season transpiration in coniferous and tropical stands (Markewitz et al. 2010 and related). |
| 6 | mixed forests | 0.37 | Intermediate deep-root buffering between deciduous and coniferous; placement mirrors how COSERO already positions mixed forest between classes 4 and 5 in the `INTMAX`/`ETVEGCOR` tables. |
| 7 | sparsely vegetated areas | 0.02 | Minimal root density and shallow rooting at the macro scale; deep-root lift capacity is negligible. Near-zero. |
| 8 | glaciers | 0.00 | No vegetation; the frozen-ground gate (`TSOIL > 0`) would suppress lift regardless. |
| 9 | water bodies | 0.00 | Handled by open-water evaporation; `hydlift_B` skips the water-body branch entirely. |
| 10 | wetlands | 0.42 | Phreatophytic vegetation specifically adapted to deep groundwater access. Assigned a high value on *capacity* grounds. When the wetland is wet, the `(1 - BFALF)` gate keeps lift off automatically; the high `fhl` only acts during dry-surface spells — exactly the regime where phreatophyte deep-water uptake should be represented. (See section 1 on why a high value is safe despite frequent saturation.) |

Note the magnitudes track the empirical ordering in section 4: the firm lower anchor (~19% for a semi-arid shrub) sits near the grassland value, and the forest/wetland values reach into the ~0.30–0.45 band that deep-uptake and HR studies support during surface drought.

---

## 6. Deferred: elevation dependence

An elevation modifier (`fhl_eff = fhl(NC) · f_elev(z)`) is **deliberately not included** in
this version. Key considerations recorded for the future discussion:

- Part of the "less lift at altitude" signal is **already emergent** from the runtime gates
  (more frequent freezing → `TSOIL > 0` fails; elevation-resolved water balance → `BFALF`).
  An explicit `f_elev` must represent only the *residual* decline in deep-root capacity
  (rooting depth, soil thinning toward treeline) not already captured by the gates.
- It may also be partly encoded already via **elevation-varying deep-store capacity**
  (`PEX2`/`PEX3`) and soil depth: if those shrink with altitude, the `(1-eps)·BW`
  availability cap already throttles lift at elevation. Adding `f_elev` on `fhl` would then
  double-count.
- If pursued: a multiplicative, separable `f_elev(z)` (flat below `z_low`, linear decline to
  0 at treeline `z_high`) evaluated per hypsometric band is the clean first form — kept
  separable so it cannot entangle with `fhl` or `ETVEGCOR`.

Decision blocked on: how `PEX2`/`PEX3` and soil depth already vary with elevation in the
target catchments.

---

## 7. Implementation pointer

These values populate the `FHL_` column of the parafile, read into `fhl_b(nz_b(NB,IZ))` per
the parameter-reader plan (see `hydraulic_lift_hydlift_B.md`, Phase 2). They are per-zone
via the land-cover class assigned to each zone. Defensive clamp `fhl_b = min(1., max(0., fhl_b))`
applies. `fhl = 0` everywhere reproduces the original model (regression baseline).

---

## 8. References

All entries below were checked against the primary sources; DOIs verified. Where an earlier
draft mis-stated a year, journal, or quantity, the corrected form is given here.

- **Dawson, T. E. (1993).** Hydraulic lift and water use by plants: implications for water
  balance, performance and plant–plant interactions. *Oecologia* 95(4), 565–574.
  doi:10.1007/BF00317442
  *(This is the foundational hydraulic-lift magnitude study — earlier drafts cited it as
  "Dawson 1996".)*

- **Ryel, R. J., Caldwell, M. M., Yoder, C. K., Or, D., & Leffler, A. J. (2002).** Hydraulic
  redistribution in a stand of *Artemisia tridentata*: evaluation of benefits to transpiration
  assessed with a simulation model. *Oecologia* 130(2), 173–184. doi:10.1007/s004420100794
  *(Reported up to ~19% increase in transpiration over a 100-day drying cycle, for a semi-arid
  shrub — not the "19–40% temperate forest" of earlier drafts.)*

- **Markewitz, D., Devine, S., Davidson, E. A., Brando, P., & Nepstad, D. C. (2010).** Soil
  moisture depletion under simulated drought in the Amazon: impacts on deep root uptake.
  *New Phytologist* 187(3), 592–607. doi:10.1111/j.1469-8137.2010.03391.x
  *(Deep-root uptake ~20% of demand from 2.5–5.5 m and ~10% from 5.5–11.5 m under throughfall
  exclusion. Tropical setting — an analogue, not a temperate prior.)*

- **Miguez-Macho, G., & Fan, Y. (2021).** Spatiotemporal origin of soil water taken up by
  vegetation. *Nature* 598, 624–627. doi:10.1038/s41586-021-03958-6
  *(~18% of global annual transpiration from deeper soil/rock moisture and groundwater;
  ~60–90% in the southern Amazon in August. Context for East-African dry-season catchments,
  not the temperate priors.)*

- **Schenk, H. J., & Jackson, R. B. (2002).** The global biogeography of roots. *Ecological
  Monographs* 72(3), 311–328. doi:10.1890/0012-9615(2002)072[0311:TGBOR]2.0.CO;2
  *(Global root-profile synthesis; ~90–95% of root biomass in the top ~0.7–1.1 m — basis for
  the "deep roots are a small biomass fraction" note.)*

**Not cited with a DOI (deliberately):** the "closed-canopy forest transpiration caps near
~75% of PET" plausibility argument traces to an unverified institutional note, not a specific
peer-reviewed paper. It is retained in section 4 only as a general qualitative observation; do
not attach a citation to it without independently confirming the source.

> **Caveat on all of the above:** these are field/modelling estimates of deep-water *buffering
> fractions* for specific species and biomes. They are used here to **rank and scale** a
> model-specific lumped coefficient (`fhl`), which itself has no published values. The relative
> ordering across land-cover classes is better supported than any single absolute `fhl`
> magnitude, which is why all values remain calibration targets, not fixed constants.
