# Texas County & Commuting Zone Daily Weather Datasets
## Using PRISM Climate Data, 2010–2026

### Overview
This workflow constructs two panel datasets of daily weather variables for
Texas, 2010-01-01 through 2026-03-17 (provisional through the latest PRISM
date): a **county panel** (254 counties) built directly from PRISM rasters,
and a **commuting zone (CZ) panel** (49 CZs) **derived from the county panel**
by area-weighted aggregation — not extracted independently. This means county
and CZ values are always reconcilable (see `06_validate_panels.py` §6.2).

```
County panel: fips  | date | tmax | tmin | tmean | ppt | tdmean | rh_mean | vpd_mean
CZ panel:     cz_id | date | tmax | tmin | tmean | ppt | tdmean | rh_mean | vpd_mean | n_counties
```

`rh_mean` (relative humidity, %) and `vpd_mean` (vapor pressure deficit, kPa)
are derived from `tdmean` (dew point) at the **county** level, before
aggregation, to avoid Jensen's-inequality bias.

---

### Directory Structure
```
prism/
├── README.md
├── PLAN_county_cz_panels.md         # Implementation plan (this workflow's spec)
├── config.yaml                      # Shared configuration
├── 00_download_tdmean_gap.R         # Close the tdmean download gap (R)
├── 01_download_prism.R              # Download daily PRISM rasters (R)
├── 02_get_commuting_zones.py        # Build Texas CZ shapefile from local files (Python)
├── 02b_get_counties.py              # Build Texas county shapefile from local files (Python)
├── 03_extract_cz_weather.R          # Zonal extraction: PRISM → CZ polygons (R)
├── 03b_extract_county_weather.R     # Zonal extraction: PRISM → county polygons (R)
├── 04_build_panel.py                # (legacy) CZ-direct panel assembly (Python)
├── 04b_build_county_panel.py        # Assemble county panel (Python)
├── 04c_build_cz_panel.py            # Aggregate county panel → CZ panel (Python)
├── 05_diagnostics.R                 # Diagnostic plots (R)
├── 06_validate_panels.py            # Validate both panels; exits non-zero on failure
├── run_pipeline.sh                  # Runs the full pipeline in order
├── requirements.txt                 # Python dependencies
└── packages.R                       # R dependencies
```

---

### Data Sources
| Source | Description | URL |
|--------|-------------|-----|
| PRISM Climate Group | 4km daily gridded climate (BIL rasters) | https://prism.oregonstate.edu |
| US Census Bureau | Cartographic boundary files, county (`cb_2024_us_county_500k`) | https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html |
| USDA ERS | 2020 Commuting Zone crosswalk (CZ2020) | https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/ |

---

### PRISM Variables Downloaded
| Variable | Description | Units |
|----------|--------------|-------|
| `ppt`    | Precipitation | mm |
| `tmax`   | Maximum temperature | °C |
| `tmin`   | Minimum temperature | °C |
| `tmean`  | Mean temperature | °C |
| `tdmean` | Mean dew point temperature | °C — raw humidity input (gap closed 2026-08-12; see Step 00 below) |

Derived (not downloaded): `rh_mean` (relative humidity, %) and `vpd_mean`
(vapor pressure deficit, kPa), computed from `tdmean` and `tmean` via the
Magnus/August-Roche-Magnus saturation vapor pressure formula.

`vpdmin`/`vpdmax` are PRISM-published variables but are **not currently
downloaded** and are not in `config.yaml`'s variable list. They are a more
defensible heat-stress measure for health-outcomes analysis and are worth
downloading in a future pass — see `PLAN_county_cz_panels.md` §3.1.

> **Note:** PRISM data has three stability tiers:
> - **stable**: finalized (available ~6 months after date)
> - **provisional**: recent months, subject to revision
> - **early**: most recent ~2 weeks, least reliable

---

## Geographic vintages

Both panels use **fixed, time-invariant geography** rather than annual
boundary vintages:

- **County boundaries**: a single 2024 Census cartographic boundary vintage
  (`cb_2024_us_county_500k`), applied to all years 2010–2026.
- **Commuting zones**: a single CZ2020 definition (USDA ERS), applied to all
  years 2010–2026.

**This is valid for Texas.** Per the Census Bureau's "Substantial Changes to
Counties and County Equivalent Entities" documentation
(https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.html),
every county-level boundary change in the US since 2010 occurred in Alaska,
South Dakota, Virginia, Louisiana, or Connecticut. **Texas has had the same
254 counties with the same FIPS codes for the entire 2010–2026 window.** The
only cross-vintage differences in TX county polygons are sub-kilometer
cartographic refinements (coastline re-digitizing, annexation-driven edge
corrections), immaterial relative to PRISM's 4 km grid.

CZs are delineated once per decennial census (CZ1990/2000/2010/2020) from
commuting flow data, not as an annual snapshot — applying CZ2020 uniformly
across 2010–2026 is a definitional choice, not a data error. The trade-off:
CZ2020 encodes commuting patterns that had not yet formed in the early years
of the panel. If a later analysis needs definition-consistent early years,
swap in the CZ2010 crosswalk (`commuting_zones.crosswalk` in `config.yaml`).

Because geography is time-invariant, it is **not** part of the join key.
Instead:
- The vintage is recorded as dataset-level metadata (this section).
- `county_meta.csv` and `cz_meta.csv` each carry a `geo_vintage` column
  (`"cb_2024"` and `"czone_2020"` respectively).
- `06_validate_panels.py` §6.4 fails loudly if the county count ever ≠ 254 —
  the tripwire that catches a future vintage swap silently changing the panel.

---

### Setup & Execution Order

**Step 0 — Install dependencies**
```bash
Rscript packages.R
pip install -r requirements.txt
```

**Step 00 — Close the tdmean download gap** (background, multi-hour)
```bash
Rscript 00_download_tdmean_gap.R
```
Scans `prism_raw/` for existing `tdmean` folders and downloads only the
missing dates.

**Status: gap closed (2026-08-12).** `tdmean` originally had a ~470-day local
download gap (2024-12-05 → 2026-03-17) from a PRISM per-day rate limit hit on
2026-08-07; retrying on a later day closed it. All 5,920 days are now present
and both panels carry 100% non-NA `tdmean`/`rh_mean`/`vpd_mean` coverage — see
`06_validate_panels.py` §6.5 output.

**Gotcha found during this run:** `already_downloaded()` (used by both `00_`
and `03b_`) only checks that a dated folder exists — it does not validate the
`.bil` inside. Six dates in Nov–Dec 2024 (`20241116/17/22`, `20241201/02/03/04`)
had corrupt stub files (0–172 KB, vs. ~3.49 MB for a real raster) left over
from an earlier, unrelated failed download attempt, so they read as "already
present" and were silently skipped on every re-run. `03b_extract_county_weather.R`
caught each one via a `terra::rast()` GDAL error when it tried to stack that
year, one date at a time across several passes. Fixed by deleting the bad
folder(s) and re-running `00_download_tdmean_gap.R`, which then correctly
detected them as missing and re-fetched real data. If PRISM downloads ever
stop cleanly mid-run again, check file sizes in `prism_raw/`, not just folder
existence, before assuming a resume is complete.

**Step 1 — Download PRISM rasters** (only needed if `prism_raw/` is incomplete
for `ppt`/`tmax`/`tmin`/`tmean`; these were already complete for 2010–2026 as
of the last check)
```bash
Rscript 01_download_prism.R
```

**Step 2 — Build Texas commuting zone shapefile**
```bash
python 02_get_commuting_zones.py
```

**Step 2b — Build Texas county shapefile**
```bash
python 02b_get_counties.py
```
Writes `intermediate_data/county_shapefiles/tx_counties_2024.gpkg` and
`clean_data/county_meta.csv`. Hard-fails if county count ≠ 254 or any county
is missing a `cz_id`.

**Step 3b — Extract zonal statistics per county** (background, 2–6 hours)
```bash
Rscript 03b_extract_county_weather.R
```
Area-weighted (`exact_extract(fun="mean")`) extraction to `intermediate_data/
extracted_county/{var}_{year}.csv`. Resumable — skips year-variable files
that already exist. Does not depend on the tdmean download finishing; extracts
whatever rasters are present. If the tdmean download lands after this step
completes, re-run this script for `tdmean` only (cheap, thanks to the
`file.exists()` skip) and rebuild the panels.

**Step 4b — Build the county panel**
```bash
python 04b_build_county_panel.py
```
Merges all variables, fills the complete 254 x n_days skeleton, adds temporal
features and the PRISM stability flag, derives `rh_mean`/`vpd_mean` at the
county level, flags implausible values, and writes
`clean_data/tx_county_daily_weather.{csv,parquet}`.

**Step 4c — Aggregate to the CZ panel**
```bash
python 04c_build_cz_panel.py
```
Reads the county panel, joins `cz_id`, aggregates with land-area weights
(§ below), and writes `clean_data/tx_cz_daily_weather.{csv,parquet}` and
`clean_data/cz_meta.csv`.

**Step 6 — Validate**
```bash
python 06_validate_panels.py
```
Exits non-zero on any failed check (shape, internal consistency, physical
plausibility, geography tripwire); prints a coverage report and sanity
spot-check.

**Step 5 — Diagnostics (optional)**
```bash
Rscript 05_diagnostics.R
```

Legacy CZ-direct extraction/assembly (`03_extract_cz_weather.R`,
`04_build_panel.py`) is retained but superseded — the current CZ panel is
derived from the county panel, not extracted directly, per the design above.

---

### Zonal Aggregation Method

**County-level extraction**: each PRISM raster cell is assigned to a county
using **area-weighted mean** via `exactextractr`, which handles partial
overlaps at polygon boundaries correctly — this matters for small counties
relative to a 4 km grid.

**County → CZ aggregation**: also **area-weighted**, using land area
(`area_km2`) as weights, renormalized over non-missing counties per CZ-day.
A simple unweighted mean would weight a 700 km² county equally with a
16,000 km² county. `ppt` is also area-weighted (not summed) since it is a
per-unit-area depth. If every county in a CZ-day is missing, the CZ value is
`NA`, never 0.

> Area weighting describes *ambient* conditions. For health-outcomes work
> (e.g. joining to workers' comp claims), **population weighting** — "what did
> the average person experience" — is usually the better exposure measure.
> The county panel is the source of truth, so a population-weighted CZ
> variant (`tmean_popw`, etc.) is a cheap re-run once county population is
> available.

---

### Runtime Estimates
| Step | Estimated Time |
|------|---------------|
| 00 — tdmean gap download | multi-hour (network-bound, ~470 days missing) |
| 01 — Full PRISM download (5 vars × 16 yrs) | 4–12 hours (already complete as of last check) |
| 02 / 02b — Shapefile builds | < 2 min each |
| 03b — County zonal extraction | 2–6 hours |
| 04b / 04c — Panel assembly | < 15 min combined |
| 06 — Validation | < 5 min |

> Tip: run Step 00 and Step 3b concurrently in the background; they are
> independent of each other. Steps 4b/4c must wait for both to finish (4c
> also needs 4b's output).
