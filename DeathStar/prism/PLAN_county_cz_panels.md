# Implementation Plan: Texas County & Commuting Zone Weather Panels (2010–2026)

---

## ✅ COMPLETE as of 2026-08-12

Both panels are built and passed `06_validate_panels.py` with zero failures:
`clean_data/tx_county_daily_weather.{csv,parquet}` (1,503,680 rows, 254
counties × 5,920 days) and `clean_data/tx_cz_daily_weather.{csv,parquet}`
(290,080 rows, 49 CZs × 5,920 days). The tdmean/humidity gap described below
was fully closed (not left as NA) — all variables are 100% non-NA in both
panels. See `README.md`'s "PRISM Variables Downloaded" / Step 00 sections for
the resolution and a gotcha found along the way (stale corrupt raster stubs
that a folder-existence check didn't detect). The STATUS section below is
kept for historical/audit context; it no longer reflects current state.

---

## STATUS (historical) — resume point as of 2026-08-08

**Read this section first.** Implementation is in progress; both background jobs
were manually stopped (not crashed) so they'd stop cleanly overnight. Nothing is
corrupted — just pick up at the point below.

### Done
- §3.0 Backed up old CZ panel to `clean_data/_backup_20260807/`; extended
  `config.yaml` with `counties:` block + `geo_vintage`/`cz_vintage` keys.
- §5 Fixed the `OUT_DIR`/`OUT_CSV` bug in `04_build_panel.py`.
- §3.2 `02b_get_counties.py` ran successfully — `intermediate_data/county_shapefiles/
  tx_counties_2024.gpkg` and `clean_data/county_meta.csv` exist (254 counties, 49
  distinct CZs, tripwire assertions passed). **No need to re-run.**
- Scripts written and untested-but-ready: `04b_build_county_panel.py`,
  `04c_build_cz_panel.py`, `06_validate_panels.py`.
- §7 docs done: `README.md`, `run_pipeline.sh`, root `CLAUDE.md` all updated.

### §3.1 tdmean download — STOPPED, do not blindly retry
`00_download_tdmean_gap.R` hit PRISM's server-side daily rate limit. `prism_raw/`
contains 482 stale `.txt` error stubs (e.g. `prism_tdmean_us_25m_20241114.txt`,
dated Mar 30 — from an *earlier* attempt, not this run) instead of real `.bil`
folders for the whole 2024-12-05→2026-03-17 gap. The script's `already_downloaded()`
correctly doesn't count `.txt` stubs as done, so it immediately re-requested all
482 dates and tripped the "downloaded more than twice in one day" limiter again.
Process was killed (was PID 1108) before it could retry further and risk an IP
block. **Confirmed with user:** proceed via this plan's own §3.1 fallback — build
both panels now with `tdmean`/`rh_mean`/`vpd_mean` present through 2024-12-04 and
NA thereafter, flagged loudly in the coverage report and README. Retry the
download on a fresh day (PRISM's limit is per-day) by re-running
`00_download_tdmean_gap.R` — do **not** delete the `.txt` stubs first, they're
harmless (not treated as "done"), just re-run once the daily limit has reset.

### §3.3 county extraction — STOPPED cleanly, safe to resume
`03b_extract_county_weather.R` was killed (was PID 1141) at the user's request to
pause for the day — not an error. Per-variable-year output is resumable via the
`file.exists()` skip in `extract_year()`. Exact state of
`intermediate_data/extracted_county/` when stopped:

| Variable | Years done | Resume behavior |
|----------|-----------|------------------|
| `ppt`    | 17/17 (2010–2026) | skipped entirely on re-run |
| `tmax`   | 17/17 (2010–2026) | skipped entirely on re-run |
| `tmin`   | 1/17 (2010 only)  | re-run continues from 2011 |
| `tmean`  | 0/17              | starts fresh |
| `tdmean` | 0/17              | starts fresh — extracts whatever `tdmean` rasters exist today (through 2024-12-04; see §3.1 above) |

**To resume:** just re-run `Rscript 03b_extract_county_weather.R` in the
background — no flags or edits needed, the skip-if-exists logic handles it.
Remaining work: finish `tmin` 2011–2026, then all of `tmean` and `tdmean`
(3.x variable-years remaining out of 5×17=85 total) — expect well under the
original 2–6h estimate for the rest.

### Next steps in order
1. Re-run `03b_extract_county_weather.R` in the background (§3.3, resumes as above).
2. Once it finishes: run `04b_build_county_panel.py` → `04c_build_cz_panel.py` →
   `06_validate_panels.py` in sequence (§3.4, §3.5, §6 below — scripts already
   exist and don't need changes).
3. Add a prominent note to `README.md`'s "PRISM Variables Downloaded" section
   (or a new section) documenting the tdmean rate-limit gap: NA from 2024-12-05
   onward in this build, with instructions to retry §3.1 and rebuild once fixed.
4. Once §3.1 eventually succeeds on a retry, re-run `03b` for `tdmean` only
   (already-resumable) and rebuild `04b`/`04c`/`06`.

---

**Audience:** Claude Sonnet implementing this plan.
**Goal:** Two daily panel datasets of Texas weather — a **county panel** (254 counties)
built directly from PRISM rasters, and a **commuting zone panel** (49 CZs) built
**by aggregating the county panel** — each covering 2010-01-01 through 2026-03-17
with temperature, rainfall, and humidity variables.

---

## 0. Context: what already exists

The repo already contains a working CZ pipeline in [DeathStar/prism/](.):

| File | Status |
|------|--------|
| `01_download_prism.R` | Works. Downloads PRISM daily BIL rasters into `prism_raw/`. |
| `02_get_commuting_zones.py` | Works. Dissolves TX counties → 49 CZ polygons. |
| `03_extract_cz_weather.R` | Works. Stacked zonal extraction, PRISM → CZ polygons. |
| `04_build_panel.py` | **Has a bug** (see §5). Assembles the final panel. |
| `05_diagnostics.R` | Exists, not yet reviewed. |

**Key architectural change this plan makes:** the current pipeline extracts PRISM
*directly to CZ polygons*, so there is no county panel and the CZ values cannot be
reconciled with county values. This plan **re-points extraction at county polygons**
and derives CZ values from counties, exactly as requested.

### Verified facts about the current data

Confirmed by inspecting the repo — do not re-verify, but do re-check if you change config:

- `prism_raw/` holds **5,920 raster-day folders each** for `ppt`, `tmax`, `tmin`, `tmean`,
  spanning **2010-01-01 → 2026-03-17**. That is complete for the target window.
- `tdmean` has only **5,438 folders, ending 2024-12-04** — an incomplete download,
  missing ~470 days. PRISM still publishes `tdmean`; this is a local gap, not a
  source limitation. **This is the only humidity variable currently downloaded.**
- `vpdmin` / `vpdmax` are listed in the README but **zero files are downloaded** and
  they are **not in `config.yaml`'s variable list**.
- The county shapefile on disk is a single vintage: `cb_2024_us_county_500k`
  (**254 Texas counties**, EPSG:4269).
- The CZ crosswalk on disk is a single vintage: `commuting-zones-2020.csv`
  (**254 TX counties → 49 TX CZs**, USDA CZ2020).

---

## 1. ⚠️ Boundary vintages: the answer to "are these snapshots?"

**They are snapshots, and this plan keeps them as snapshots — deliberately.**
Read this section before writing code; it determines the schema.

### County boundaries

The Census cartographic boundary files are published as **annual vintages**, and each
vintage is a snapshot of boundaries as of that year's Boundary and Annexation Survey.
The repo has exactly one vintage on disk (`cb_2024`), so **the current pipeline uses a
single 2024 snapshot applied to all years 2010–2026**.

**However — for Texas specifically, this is harmless.** Per the Census Bureau's
"Substantial Changes to Counties and County Equivalent Entities" documentation, the
complete list of county-level changes since 2010 is:

| Decade | Change | State |
|--------|--------|-------|
| 2010s | Petersburg Borough created (2013) | Alaska |
| 2010s | Valdez-Cordova split → Chugach + Copper River (2019) | Alaska |
| 2010s | Wade Hampton → Kusilvak Census Area (2015) | Alaska |
| 2010s | Hoonah-Angoon / Prince of Wales-Hyder boundary adjustments | Alaska |
| 2010s | Shannon County → Oglala Lakota County (2015) | South Dakota |
| 2010s | Bedford independent city → town, absorbed into Bedford County (2013) | Virginia |
| 2010s | LaSalle Parish name correction | Louisiana |
| 2020s | 8 counties → 9 planning regions / COGs (2022, effective 2024) | Connecticut |

**Texas appears nowhere on that list.** Texas has had the same **254 counties with the
same FIPS codes** for the entire 2010–2026 window. The only changes to TX county
polygons across vintages are sub-kilometer cartographic refinements (coastline
re-digitizing, annexation-driven edge corrections) — immaterial relative to PRISM's
4 km grid cells.

**Conclusion:** a time-invariant county geography is *correct* for Texas. You do **not**
need to download 17 annual shapefile vintages.

### Commuting zone boundaries

CZs are **not** annual-vintage geographies. They are delineated once per decennial
census from commuting flows: CZ1990, CZ2000, CZ2010, CZ2020. The repo uses **CZ2020**.
A CZ vintage is a *fixed definition*, not a snapshot that drifts — so applying CZ2020
across 2010–2026 is a definitional choice, not a data error.

The trade-off worth knowing: applying CZ2020 to 2010-era data means early years use
commuting patterns that had not yet formed. If a later analysis needs
definition-consistent early years, swap in the CZ2010 crosswalk — the pipeline below
makes this a one-line config change.

### What this means for the schema

Because geography is time-invariant, **do not** add `year` to the geography join key.
Instead, make the vintage explicit and auditable:

1. Record the vintage as **dataset-level metadata** (§7) — not as a per-row column,
   which would waste ~5 M rows of storage on a constant.
2. Add a **`geo_vintage` column** to the two *crosswalk/metadata* files only
   (`county_meta.csv`, `cz_meta.csv`), valued `"cb_2024"` and `"czone_2020"`.
3. Write the §6.4 assertion that fails loudly if the county count ever ≠ 254 — this is
   the tripwire that catches a future vintage swap silently changing the panel.

**State this explicitly in the README you update (§7): the panels use fixed 2024 county
boundaries and fixed CZ2020 definitions, which is valid for Texas because no Texas
county changed between 2010 and 2026.**

---

## 2. Target outputs

Write to `clean_data/`:

| File | Rows | Key |
|------|------|-----|
| `tx_county_daily_weather.parquet` / `.csv` | 254 × 5,920 ≈ **1,503,680** | `fips` × `date` |
| `tx_cz_daily_weather.parquet` / `.csv` | 49 × 5,920 ≈ **290,080** | `cz_id` × `date` |
| `county_meta.csv` | 254 | `fips` |
| `cz_meta.csv` | 49 | `cz_id` |

The existing `tx_cz_daily_weather.*` (290,081 lines ✓ — matches 49 × 5,920 + header)
will be **overwritten**. Back it up first (§3.0).

### Schema — county panel

```
fips            chr    5-digit county FIPS, zero-padded ("48001"). NEVER read as int.
county_name     chr
date            date
year month day doy week quarter   int
tmax tmin tmean chr→dbl  °C, area-weighted county mean
ppt             dbl    mm/day
tdmean          dbl    °C, mean dew point  ← humidity
rh_mean         dbl    %, derived (§4.5)   ← humidity, headline
vpd_mean        dbl    kPa, derived (§4.5) ← humidity
prism_stability chr    stable / provisional / early
```

### Schema — CZ panel

Same weather columns, but keyed `cz_id` + `cz_name`, plus:

```
n_counties      int    counties aggregated into this CZ-day
area_km2        dbl    total CZ land area
```

---

## 3. Step-by-step implementation

### 3.0 — Preliminaries

```
Back up clean_data/tx_cz_daily_weather.{csv,parquet} to clean_data/_backup_YYYYMMDD/
```

Then extend `config.yaml` with a `counties:` block mirroring `commuting_zones:`,
adding paths for the new county shapefile output, county extraction dir, and the two
new panel outputs. Keep all existing keys — other scripts read them.

**Add `geo_vintage: "cb_2024"` and `cz_vintage: "czone_2020"` keys to config** so the
metadata files in §1 can be populated from a single source of truth.

---

### 3.1 — Close the `tdmean` gap (do this FIRST; it runs long)

Humidity is a required variable and is currently missing 2024-12-05 → 2026-03-17.

Create **`00_download_tdmean_gap.R`** modelled on `01_download_prism.R`:

- `.libPaths("Z:/ek559/RPackages")`; `library(prism)`; `prism_set_dl_dir(prism_dir)`.
- Scan `prism_raw/` for existing `*_tdmean_*` folders, parse the trailing `YYYYMMDD`,
  and request **only the missing dates** — never re-download the 5,438 you have.
- Download in annual chunks with the same tryCatch/retry structure as `01_`.
- **Run this in the background**; it is a multi-hour network job. Start it, then
  continue with §3.2 while it runs.

**Also strongly consider downloading `vpdmax` + `vpdmin` for the full window** — VPD is
the more defensible humidity/heat-stress measure in the health-outcomes literature this
project is heading toward (see `DeathStar/figures/heatsummary.R`). It is a large
download; if you skip it, the derived `vpd_mean` in §4.5 stands in.

**Fallback if the download cannot complete:** build the panels with `tdmean` present
through 2024-12-04 and `NA` thereafter. Do **not** silently drop 2025–2026 rows, and do
**not** interpolate across a 15-month gap. Flag it loudly in the coverage report and
README instead.

---

### 3.2 — `02b_get_counties.py` — build the county polygon file

New script, modelled closely on `02_get_commuting_zones.py`.

1. Read `cb_2024_us_county_500k.shp`; filter `STATEFP == "48"`.
2. Rename `GEOID` → `fips`, `NAME` → `county_name`. `fips` is **str, zfill(5)**.
3. Project to **EPSG:3083** (Texas Albers) to compute `area_km2` and centroids; convert
   centroids back to EPSG:4326 for `centroid_lon` / `centroid_lat`.
   *(The existing `02_` script does this correctly — copy that pattern verbatim.)*
4. Attach `cz_id` + `CZName` from `commuting-zones-2020.csv` (join on `fips`).
5. **Assert `len(tx_counties) == 254`** and that **every county has a non-null `cz_id`**.
   Hard-fail on violation — this is the §1 tripwire.
6. Save `intermediate_data/county_shapefiles/tx_counties_2024.gpkg` in EPSG:4326.
7. Also write `clean_data/county_meta.csv` with
   `fips, county_name, cz_id, cz_name, area_km2, centroid_lon, centroid_lat, geo_vintage`.

---

### 3.3 — `03b_extract_county_weather.R` — zonal extraction to counties

Copy `03_extract_cz_weather.R` and change the polygon layer from CZ to county.
**Preserve the stacked-raster strategy** — it is the reason the pipeline runs in hours
rather than days. Specifically keep:

- Stack a full variable-year of BIL files into one `SpatRaster`.
- Name layers with date strings so `exact_extract` column names carry the date.
- `terra::crop` once to the TX bbox before extracting.
- `exactextractr::exact_extract(fun = "mean")` — **area-weighted**, correct for partial
  cell overlap at boundaries.
- Pre-transform polygons to the raster CRS **once**, outside the loop.
- Per-variable-year CSV output with a `file.exists()` skip so the job is resumable.

Changes required:

- Load `tx_counties_2024.gpkg`; group ID is `fips` (not `cz_id`).
- Output to `intermediate_data/extracted_county/{var}_{year}.csv` — a **new directory**,
  so the existing CZ extracts are not clobbered.
- **Also extract `fun = "sum"` of cell-coverage-weighted area per county**, or simply
  carry `area_km2` from the metadata file — needed for §4.2 weighting.

Runtime: 254 polygons vs 49, but the cost is dominated by raster I/O, not polygon
count. Expect roughly the same 2–6 h as the CZ run. Run it in the background.

---

### 3.4 — `04b_build_county_panel.py` — assemble the county panel

Copy `04_build_panel.py`, then **fix the bug** in it (§5) before adapting.

Keep these behaviors from the original — they are all correct:

- Per-variable load + outer-merge on the key.
- **Skeleton join** (`MultiIndex.from_product`) so every county × date row exists even
  when a raster is missing. This is what makes gaps visible rather than silent.
- Temporal features (`year, month, day, doy, week, quarter`).
- `prism_stability` tier flag.
- Outlier range check → set implausible values to `NaN` **with a printed count**.
- Save both CSV and Parquet.

Adapt: key on `fips`; merge `county_meta.csv`; add the §4.5 derived humidity columns.

---

### 3.5 — `04c_build_cz_panel.py` — aggregate county → CZ

**This is the core deliverable — the CZ panel must be derived from the county panel,
not extracted independently.**

1. Read `tx_county_daily_weather.parquet`.
2. Join `cz_id` from `county_meta.csv` on `fips`.
3. Group by `cz_id, date` and aggregate per §4.2.
4. Attach `cz_meta.csv`; add `n_counties`.
5. Assert **49 CZs** and **290,080 rows**.
6. Save CSV + Parquet.

---

## 4. Methodological decisions (follow these exactly)

### 4.1 County-level extraction — area-weighted mean
Use `exact_extract(fun = "mean")`. It weights each PRISM cell by its fractional overlap
with the county polygon, which matters for small counties relative to a 4 km grid.

### 4.2 County → CZ aggregation — **area-weighted, not simple mean**

A simple `mean()` over counties would weight 700 km² Rockwall equally with 16,000 km²
Brewster. Use land-area weights:

```python
w = df["area_km2"]
def wmean(x):
    m = x.notna()
    return np.nan if not m.any() else np.average(x[m], weights=w[m])
```

Apply `wmean` to **`tmax`, `tmin`, `tmean`, `tdmean`, `rh_mean`, `vpd_mean`**.

**`ppt` also uses the area-weighted mean** — it is a per-unit-area depth (mm), so the
area-weighted mean is the correct CZ-level average depth. Do **not** sum it.

**NA handling:** weights must be renormalized over non-missing counties only (the
`wmean` above does this). If *every* county in a CZ-day is NA, the result is NA — do not
emit 0.

> **Design note worth flagging to the user:** area weighting is right for describing
> *ambient* conditions. If the panel is later joined to health claims, **population
> weighting** answers "what did the average person experience" and is usually the better
> exposure measure. Build area-weighted now, but keep the county panel as the source of
> truth so a population-weighted CZ variant is a cheap re-run. Optionally emit
> `tmean_popw` alongside `tmean` if county population is readily available.

### 4.3 Missing days
PRISM has no missing days in the target window, but the skeleton join guarantees a
complete calendar regardless. Leave gaps as `NA`; do not interpolate.

### 4.4 Units
Keep PRISM native units: temperature °C, precipitation mm. Do not convert to °F/inches
in the panel — do that at the figure layer.

### 4.5 Humidity — derived variables

`tdmean` (dew point, °C) is the raw humidity input. Compute two standard derived
measures at the **county level** (before aggregation — deriving them post-aggregation
from averaged inputs introduces Jensen's-inequality bias):

**Saturation vapor pressure** (Magnus / August-Roche-Magnus, kPa):

```
es(T) = 0.6108 * exp(17.27 * T / (T + 237.3))
```

**Actual vapor pressure:** `ea = es(tdmean)`

**Relative humidity (%):** `rh_mean = 100 * ea / es(tmean)`, then **clip to [0, 100]**
(the approximation can overshoot slightly near saturation).

**Vapor pressure deficit (kPa):** `vpd_mean = es(tmean) - ea`, **clip at ≥ 0**.

Both are `NA` wherever `tdmean` or `tmean` is `NA` — which, until §3.1 completes, means
all of 2025–2026.

---

## 5. 🐛 Known bug to fix

[04_build_panel.py:39-41](04_build_panel.py#L39-L41) — `OUT_DIR` is referenced on the
line **before** `OUT_CSV` is defined:

```python
OUT_DIR       = OUT_CSV.parent   # NameError: OUT_CSV not defined
OUT_CSV       = Path(cfg["output"]["panel_csv"])
```

Fix by reordering:

```python
OUT_CSV       = Path(cfg["output"]["panel_csv"])
OUT_PARQUET   = Path(cfg["output"]["panel_parquet"])
OUT_DIR       = OUT_CSV.parent
```

Apply this fix in `04_build_panel.py` **and** in every copy derived from it.

---

## 6. Validation (implement as `06_validate_panels.py`)

The script must **exit non-zero** on any failure — not just print.

**6.1 Shape**
- County: 254 unique `fips`, 5,920 unique dates, 1,503,680 rows, no duplicate `(fips, date)`.
- CZ: 49 unique `cz_id`, 5,920 unique dates, 290,080 rows, no duplicate `(cz_id, date)`.

**6.2 Internal consistency — the key check**
For 20 randomly sampled CZ-days, recompute the area-weighted mean from the county panel
by hand and assert it matches the CZ panel to within `1e-6`. This is what proves the CZ
panel is genuinely derived from the county panel.

**6.3 Physical plausibility**
- `tmin <= tmean <= tmax` on every non-NA row (report violation count; a handful from
  independent PRISM interpolation is tolerable — a systematic pattern is not).
- `ppt >= 0`; `tdmean <= tmax`; `0 <= rh_mean <= 100`; `vpd_mean >= 0`.
- Ranges from `REASONABLE_RANGES` in `04_build_panel.py`.

**6.4 Geography tripwire (per §1)**
- Assert exactly 254 TX counties and 49 TX CZs.
- Assert every `fips` in the county panel maps to exactly one `cz_id`.
- Assert the CZ→county partition is complete: `sum(n_counties) / 5920 == 254`.

**6.5 Coverage report**
Print % non-NA per variable per year for both panels. **The `tdmean` / `rh_mean` /
`vpd_mean` gap must show up clearly here** if §3.1 did not finish.

**6.6 Sanity spot-check**
Print July mean `tmax` for a few known CZs. Houston/Beaumont (`cz_id` 219, the Gulf
Coast zone in the current data) should be ~34 °C with high humidity; West Texas zones
hotter and drier. Anything wildly off signals a CRS or join error.

---

## 7. Documentation to update on completion

1. **`DeathStar/prism/README.md`** — add the county panel, renumber the step list, and
   **add a "Geographic vintages" section stating the §1 finding**: fixed `cb_2024`
   county boundaries + fixed CZ2020 definitions, valid for Texas because no Texas county
   changed 2010–2026; cite the Census "Substantial Changes to Counties" documentation.
2. **`run_pipeline.sh`** — add `00`, `02b`, `03b`, `04b`, `04c`, `06` in order.
3. **Root `CLAUDE.md`** — it is referenced in the git log but **does not exist** at the
   repo root. Create it, documenting the folder layout (`raw_data/` → `prism_raw/` →
   `intermediate_data/` → `clean_data/` → `output/`) and the two panel datasets.

---

## 8. Execution order

```
§3.0  Back up existing CZ panel; extend config.yaml            [5 min]
§3.1  00_download_tdmean_gap.R          ── background, hours ──┐
§3.2  02b_get_counties.py                                      │  [2 min]
§3.3  03b_extract_county_weather.R      ── background, 2–6 h ──┤
§5    Fix the OUT_DIR bug                                      │  [1 min]
      ── wait for both background jobs ─────────────────────────┘
§3.4  04b_build_county_panel.py                                   [10 min]
§3.5  04c_build_cz_panel.py                                       [2 min]
§6    06_validate_panels.py                                       [5 min]
§7    Update README, run_pipeline.sh, CLAUDE.md                   [15 min]
```

§3.1 and §3.3 are independent and should run concurrently. §3.3 does not depend on the
`tdmean` download — it extracts whatever rasters are present, so if the download lands
late, **re-run `03b` for `tdmean` only** (the `file.exists()` skip makes this cheap) and
rebuild the panels.

---

## 9. Gotchas

- **FIPS as string, always.** `"48001"` read as int becomes `48001`; a leading-zero
  state would silently lose a digit. Pass `dtype={"fips": str}` on every read.
- **CRS discipline.** PRISM is NAD83 (EPSG:4269); `cb_2024` is also EPSG:4269. Areas and
  centroids must be computed in **EPSG:3083**, never in a geographic CRS.
- **`cz_id` is a string** in the existing pipeline (`.astype(str)`). Stay consistent or
  the county→CZ join silently produces zero matches.
- **Don't clobber `intermediate_data/extracted/`** — that is the CZ-direct extraction.
  County extracts go to `extracted_county/`.
- **Memory.** ~120 MB per variable-year stack. Do not load all variables × all years at
  once; the per-variable-year loop exists for this reason.
- **`prism_raw/` is huge** (5,920 folders × 5 variables). Directory listings there are
  slow — filter by pattern, and never `ls` it unfiltered in a loop.
- **`.gitignore` excludes `*.parquet` and all data dirs.** The new panels will not be
  committed; that is intended. Commit scripts only.
