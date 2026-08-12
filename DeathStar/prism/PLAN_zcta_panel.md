# PLAN — Texas ZCTA × Day Weather Panel (PRISM 800m, 2010–2026)

**Status:** Ready to implement. Written for execution by a Sonnet model.
**Author:** EK  **Date:** 2026-08-12
**Scope:** Build `clean_data/tx_zcta_daily_weather.{csv,parquet}` — a
ZIP-code-tabulation-area × day weather panel for Texas, 2010-01-01 →
2026-03-17, from PRISM **800m** daily grids.

> Read this whole document before writing code. §1 records facts that were
> verified live against the PRISM and Census servers on 2026-08-12; several of
> them contradict the older 4km pipeline's assumptions and the reference
> `.Rmd`. Do not "fix" the code back toward those older patterns.

---

## 0. TL;DR of the design

| Decision | Choice | Why |
|---|---|---|
| Resolution | PRISM 800m (`30s`) | Free to the public since Mar 2025 |
| Spatial subsetting at source | **Not available** | Web service has no bbox param; one full CONUS grid per request |
| Storage strategy | **Stream: download → clip to TX → delete CONUS** | Z: has only 319 GB free; the ~957 GB of CONUS never lands |
| Variables | `tmax`, `tmin`, `tmean`, `tdmean` | Heat exposure + derived `rh_mean`/`vpd_mean` |
| ZCTA geography | **Two-era**: ZCTA510 (2010–2019), ZCTA520 (2020–2026) | ZCTAs only change at decennial censuses |
| Extraction | **Month-stacked** multi-band `exact_extract` | 16× faster than per-day (§1.6) |
| Panel key | `zcta5 × date` + `zcta_vintage` column | Unbalanced by construction; see §6.3 |

Final panel ≈ **12.0M rows** (see §6.4 for the exact arithmetic).

---

## 1. Verified facts — do not re-litigate these

Each was tested live on 2026-08-12. Re-verify only if a step fails.

### 1.1 800m daily data is free and the endpoint works
```
https://services.nacse.org/prism/data/get/us/800m/<element>/<YYYYMMDD>
```
Returns HTTP 200, a ~40 MB `.zip`. Tested with
`.../us/800m/tmax/20150819` → 40,438,762 bytes in 2.8 s.

Authoritative spec: `https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf`
(last updated 26 Mar 2025). Valid values:
- `<element>`: `ppt, tmin, tmax, tmean, tdmean, vpdmin, vpdmax`
- `<region>`: `us` (only CONUS is implemented)
- `<res>`: `800m`, `4km` (`400m` not yet implemented)
- `<date>`: `YYYYMMDD` daily, valid 19810101→present

### 1.2 There is NO bounding-box / spatial subset parameter
The user asked to subset at the source if possible. **It is not possible.**
The web service delivers one whole-CONUS grid per request; the only options
are format (`?format=[nc|asc|bil]`). Therefore the download-then-clip path in
§3 is mandatory, not a fallback.

### 1.3 BIL is retired — data is now Cloud Optimized GeoTIFF
As of 2025-10-01 PRISM moved all time series to COG. Filenames dropped the
stability tier (`stable`/`provisional`/`early`) and gained a resolution code:
`15s`=400m, `30s`=800m, `25m`=4km.

New naming: `prism_<var>_us_<res>_<YYYYMMDD>.<ext>`
e.g. `prism_tmax_us_30s_20150819.tif`

**Consequence:** the existing `already_downloaded()` regex in
`01_download_prism.R`/`03b_...R` and the `prism` R package's archive parser
target the *old* names. Do not reuse them. Also: the old `prism_stability`
column in the county panel came from the filename, which no longer carries
it — §5.4 derives it from the date instead.

The 800m COG grid is: `EPSG:4269`, 3105 × 7025, float32, LZW, nodata `-9999`,
0.008333° (30 arc-sec) resolution.

### 1.4 Download limits are real
From the spec, verbatim:
> if a file is downloaded twice in a 24-hour period, no more downloads of that
> file will be allowed during that period. Repeated excessive download
> activity may result in IP address blocking, at our discretion.

This is the single biggest operational risk. §3.3 covers it. The project has
already been bitten by a PRISM rate limit once (README, Step 00, 2026-08-07),
so treat this as a known-hostile constraint, not a theoretical one.

### 1.5 Texas clips to 5.8% of CONUS
Measured: CONUS `.tif` = 41.08 MB → TX-clipped (deflate, predictor=3,
tiled) = **2.39 MB**. This is what makes the project fit on disk.

### 1.6 Stacked extraction is 16× faster — this is the key perf decision
Benchmarked with `exactextract` 0.3.0 against 1,989 TX ZCTA polygons on a
TX-clipped 800m grid:

| Approach | Per var-day | 23,680 var-days |
|---|---|---|
| One `exact_extract` call per day | 11.61 s | **76.4 h** |
| 30-day multi-band stack, one call | 0.74 s | **4.9 h** |

The cost is dominated by rasterizing 1,989 polygons, which is paid *once per
call* regardless of band count. **Always extract a whole month as one
multi-band call.** This mirrors the year-stacking already used in
`03b_extract_county_weather.R`; here we use months because 800m grids are
~17× larger than 4km and a full year would not fit comfortably in RAM.

### 1.7 ZCTAs are NOT meaningfully time-varying
The user asked for "time varying zipcode shapefiles." Verified availability:

| Vintage | Cartographic (`GENZ`) | TIGER line |
|---|---|---|
| ZCTA510 | 2015, 2017, 2019 = 200 | 2012, 2020 = 200 |
| ZCTA520 | 2020 = 200 | 2020–2025 = 200 |
| 2010, 2013, 2022, 2023, 2024 `GENZ` | 404 | — |

The Census delineates ZCTAs only at the decennial census. Annual TIGER
releases republish the same delineation. So "time-varying" reduces to a
**two-era** design, which is what §2 builds. Anything finer would be fake
precision.

Measured Texas overlap between the two eras:
- 2,018 ZCTAs present in both
- 13 present only pre-2020
- 75 present only 2020+

### 1.8 Python environment
Use the project virtualenv (per `.claude/CLAUDE.md`); there is no system
Python on PATH:
```
"Z:/ek559/nys_algal_bloom/NYS algal bloom/code2/Scripts/python.exe"
```
Confirmed present: `rasterio` 1.4.3, `geopandas` 1.0.1, `exactextract` 0.3.0,
`shapely` 2.1.0, `pyogrio` 0.10.0, `requests` 2.34.2, `pyarrow` 23.0.1,
`pandas` 2.2.3, `numpy` 2.2.4, `yaml`, `fiona`.
Missing: `rioxarray` (not needed — use `rasterio` windowed reads).

**This pipeline is Python-only.** The reference `.Rmd` is R, but every step
here has a better Python equivalent given the packages above, and the
`exactextract` Python binding is what was benchmarked in §1.6.

### 1.9 Disk budget
`Z:` has **319 GB free of 2.5 TB (88% used)**. The plan's steady-state
footprint is ~57 GB of clipped rasters + ~3 GB of outputs. Peak transient
disk is < 1 GB because CONUS files are deleted immediately (§3.2).

---

## 2. Relevance of `20260812_PRISMA data.Rmd`

The user asked whether this file is relevant. **Partially — borrow two ideas,
reject the rest.**

**Borrow:**
1. **`st_point_on_surface` for state assignment.** The `.Rmd` correctly uses a
   representative point rather than a centroid to assign each ZCTA to one
   state. This matters: 2,031/2,093 ZCTAs *intersect* Texas, but many are
   Louisiana/Oklahoma/New Mexico ZCTAs merely touching the border (e.g.
   `71119`, `71419`, `73568`). Centroid/representative-point assignment gives
   **1,989** true TX ZCTAs. Use `representative_point().within(tx_union)`.
2. **Clamping RH to [0, 100]** before any heat-index computation, and the
   reasoning about `heat.index()` silently returning NA above 100.

**Reject:**
1. **It uses GridMET, not PRISM** (`northwestknowledge.net`, 4km, Kelvin,
   `tmmx`/`rmax`). Different product, different units, different resolution.
   Nothing about its download code transfers.
2. **It downloads whole-CONUS annual NetCDFs and never clips** — the opposite
   of what is needed here.
3. **`tigris::zctas()`** pulls all 33k US ZCTAs at runtime. Prefer explicitly
   versioned Census URLs (§2.1) so the vintage is pinned and auditable.
4. **Its `day_1…day_365` → date reconstruction is fragile** (assumes band
   order == calendar order, breaks on leap years / missing days). §4 keys
   dates off filenames instead.

**Optional carry-over:** if the analysis wants a NWS heat index alongside
`rh_mean`/`vpd_mean`, port `weathermetrics::heat.index()` — but note the
`.Rmd` computes a *daily max* HI from `tmmx`+`rmax`, whereas this panel's
`tdmean` is a daily *mean* dew point. Do not mix them without relabeling.

### 2.1 Pinned ZCTA sources
| Era | Years | URL |
|---|---|---|
| ZCTA510 | 2010–2019 | `https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_zcta510_500k.zip` |
| ZCTA520 | 2020–2026 | `https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_zcta520_500k.zip` |

Both verified HTTP 200 (61.6 MB / 66.7 MB). ID columns are `ZCTA5CE10` and
`ZCTA5CE20` respectively; normalize both to `zcta5`.

---

## 3. Step 10 — Download + clip (`10_download_clip_prism800.py`)

The long pole. Written to be **interruptible and resumable**; assume it will
be stopped and restarted several times.

### 3.1 Config additions (`config.yaml`)
Add a new top-level block. **Do not modify existing keys** — the 4km
county/CZ pipeline still depends on them.
```yaml
prism800:
  base_url:   "https://services.nacse.org/prism/data/get/us/800m"
  clip_dir:   "Z:/ek559/DeathStar/prism_raw_800m_tx"
  variables:  [tmax, tmin, tmean, tdmean]
  start_date: "2010-01-01"
  end_date:   "2026-03-17"
  tx_bbox:    [-106.75, 25.75, -93.4, 36.6]   # xmin, ymin, xmax, ymax (EPSG:4269)
  request_sleep_sec: 2

zcta:
  shp_dir: "Z:/ek559/DeathStar/intermediate_data/zcta_shapefiles"
  eras:
    - {name: zcta510, start: "2010-01-01", end: "2019-12-31",
       url: "https://www2.census.gov/geo/tiger/GENZ2019/shp/cb_2019_us_zcta510_500k.zip",
       id_col: "ZCTA5CE10"}
    - {name: zcta520, start: "2020-01-01", end: "2026-03-17",
       url: "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_zcta520_500k.zip",
       id_col: "ZCTA5CE20"}

output:
  extracted_zcta_dir:  "Z:/ek559/DeathStar/intermediate_data/extracted_zcta"
  zcta_panel_csv:      "Z:/ek559/DeathStar/clean_data/tx_zcta_daily_weather.csv"
  zcta_panel_parquet:  "Z:/ek559/DeathStar/clean_data/tx_zcta_daily_weather.parquet"
  zcta_meta_csv:       "Z:/ek559/DeathStar/clean_data/zcta_meta.csv"
```
The `tx_bbox` is padded ~0.25° beyond the TX hull so every ZCTA assigned to
Texas is fully covered even where it straddles the border.

### 3.2 Per-grid-day procedure
Output path: `{clip_dir}/{var}/{year}/prism_{var}_tx_30s_{YYYYMMDD}.tif`

```
if output exists AND is a valid readable raster of the expected shape:
    skip                                    # resume support
download {base_url}/{var}/{YYYYMMDD}  -> scratch/tmp.zip
verify: HTTP 200, Content-Length > 30 MB, valid zip
extract the single *.tif to scratch/
open with rasterio; window = from_bounds(*tx_bbox, src.transform)
read that window ONLY (windowed read — never src.read() the full CONUS array)
write clipped GeoTIFF:
    compress=deflate, predictor=3, tiled=True, blockxsize=256, blockysize=256
    preserve crs, nodata=-9999, dtype float32
delete scratch zip + extracted tif IMMEDIATELY   # non-negotiable, see §1.9
sleep(request_sleep_sec)
```

**Validate before skipping.** The README documents exactly this failure: six
dates in Nov–Dec 2024 had 0–172 KB stub files that a folder-existence check
read as "present," silently poisoning every re-run. Check that the file opens
and has the expected width/height, not merely that it exists.

### 3.3 Rate-limit handling (§1.4)
- Never request the same date+var twice in a run. The resume check (§3.2)
  already guarantees this; do not add a "retry the whole year" loop.
- Sleep ≥ 2 s between requests, matching PRISM's own sample script.
- On HTTP 429 / 403 / non-200: exponential backoff (30 s, 60 s, 120 s, 300 s),
  max 4 retries, then **log the date to a failures CSV and move on**. Do not
  abort the run — a single bad date must not cost hours of progress.
- Write `logs/download_failures_800m.csv` (`var,date,http_code,message`) and
  re-run the script later to pick up only those dates.
- Keep concurrency to **at most 2** parallel requests. The bottleneck is
  PRISM's tolerance, not local I/O. A 26-hour serial download is acceptable;
  an IP ban is not.

### 3.4 Expected cost — report before running
Per `.claude/CLAUDE.md`, this must be surfaced to the user before execution:

- **Grid-days:** 5,920 days × 4 vars = **23,680 requests**
- **Transient download:** ~957 GB (never stored)
- **Final on disk:** 23,680 × 2.39 MB ≈ **56.6 GB**
- **Wall time:** ~26 h serial at 2 s download + 2 s sleep; ~13–15 h at
  concurrency 2
- **Disk after:** 319 GB free → ~262 GB free

Run in a background/detached session. It will span more than one day.

---

## 4. Step 11 — Build ZCTA shapefiles (`11_get_zctas.py`)

For each era in `zcta.eras`:
1. Download the pinned URL to `{zcta.shp_dir}/` if absent (cache it).
2. Read with geopandas; reproject to **EPSG:4269** to match the PRISM COG
   exactly, avoiding a per-extraction CRS transform.
3. Build the Texas mask: read `cb_2024_us_county_500k.shp`, filter
   `STATEFP == "48"`, `union_all()`.
4. Select TX ZCTAs by **`representative_point().within(tx_union)`** (§2).
   Expect **~1,989** for ZCTA520. Assert the count is in 1,900–2,100 and fail
   loudly otherwise (tripwire mirroring the 254-county check).
5. Normalize the id column (`ZCTA5CE10`/`ZCTA5CE20`) → `zcta5` (string,
   zero-padded to 5).
6. `make_valid()` on geometries; keep only POLYGON/MULTIPOLYGON (the `.Rmd`
   does this and it is worth keeping — invalid rings make `exact_extract`
   throw).
7. Compute `area_km2`, `centroid_lon`, `centroid_lat` via EPSG:3083 (Texas
   Albers), matching `02b_get_counties.py`.
8. Attach `county_fips` and `cz_id` by largest-area overlap with the TX county
   layer, so the ZCTA panel can be reconciled against the existing
   county/CZ panels and joined to the ordinance indicators.
9. Write `{zcta.shp_dir}/tx_{era}.gpkg` and append to
   `clean_data/zcta_meta.csv` with columns:
   `zcta5, zcta_vintage, area_km2, centroid_lon, centroid_lat, county_fips, county_name, cz_id, cz_name`

---

## 5. Step 12 — Zonal extraction (`12_extract_zcta_weather.py`)

### 5.1 Loop structure
```
for era in eras:
    polys = read tx_{era}.gpkg            # already EPSG:4269
    for var in variables:
        for (year, month) in months within era ∩ [start_date, end_date]:
            out = {extracted_zcta_dir}/{var}_{era}_{YYYYMM}.csv
            if out exists: continue        # resume support
            files = sorted clipped tifs for that var/month
            if not files: log gap; continue
            build an in-memory multi-band stack from those files
            exact_extract(stack, polys, ["mean"]) -> one column per band
            reshape long: zcta5 | date | <var>
            write out
```

### 5.2 Getting dates right
Derive each band's date from its **filename**, not band order (§2, rejected
item 4). Carry an explicit `bands=[(date, path), …]` list and map column *i*
of the result back to `bands[i][0]`. Assert
`len(result_columns) == len(bands)`.

### 5.3 Memory
A month of 800m TX clips is 1,302 × 1,602 × 31 × 4 bytes ≈ **259 MB** as
float32 — fine. Do **not** stack a full year (~3 GB). Build the stack either
with `rasterio` into a preallocated `numpy` array wrapped in a `MemoryFile`,
or via a small VRT. Verified working shape from the benchmark: (1302, 1602).

### 5.4 `prism_stability`
The tier is no longer in the filename (§1.3). Derive from the date, keeping
the existing county panel's thresholds so the columns stay comparable:
`stable` if > 180 days old, `provisional` if > 14, else `early`.
Optionally cross-check a sample against the Release Date service
(`.../releaseDate/us/800m/<var>/<YYYYMMDD>?json=true`, verified working).

### 5.5 Cost
~4.9 h single-core (§1.6). Parallelize across variables with 4 processes
(each holds its own ~259 MB stack — ~1 GB total) → **~1.5–2 h**. This step is
CPU-bound and local, so parallelism here is free of the PRISM risk in §3.3.

---

## 6. Step 13 — Assemble the panel (`13_build_zcta_panel.py`)

Model on `04b_build_county_panel.py`; the column conventions should match so
the three panels are interoperable.

### 6.1 Merge and skeleton
Merge the four variables on `(zcta5, date)`. Build the skeleton
**per era**, then concatenate:
- era 510: its ZCTA list × dates 2010-01-01 → 2019-12-31
- era 520: its ZCTA list × dates 2020-01-01 → 2026-03-17

Do **not** cross the full ZCTA union with the full date range — that would
fabricate rows for ZCTAs in periods where they did not exist.

### 6.2 Derived variables
Compute at ZCTA level, before any aggregation (Jensen's inequality — same
reasoning as the county panel):
```
es(T)    = 0.6108 * exp(17.27 * T / (T + 237.3))     # kPa
rh_mean  = clip(100 * es(tdmean) / es(tmean), 0, 100)
vpd_mean = clip(es(tmean) - es(tdmean), 0, None)
```

### 6.3 Columns
```
zcta5 | date | year | month | day | doy | week | quarter
      | tmax | tmin | tmean | tdmean | rh_mean | vpd_mean
      | zcta_vintage | county_fips | county_name | cz_id | cz_name
      | area_km2 | centroid_lon | centroid_lat | prism_stability
```
`zcta_vintage` ∈ {`zcta510`, `zcta520`} is **mandatory** — it is the flag that
tells any downstream user the geography changed at 2020-01-01.

### 6.4 Expected size
- era 510: 1,989-ish ZCTAs × 3,652 days ≈ 7.27M rows
- era 520: 1,989-ish ZCTAs × 2,268 days ≈ 4.51M rows
- **total ≈ 11.8M rows** (exact counts depend on §4 step 4)

Estimate ~2.5 GB CSV / ~0.4 GB parquet. Write both, as the other panels do.
Per `.claude/CLAUDE.md`, flag the >1 GB CSV to the user.

### 6.5 Plausibility flags
Reuse the county panel's `REASONABLE_RANGES` for `tmax/tmin/tmean/tdmean/
rh_mean/vpd_mean`, setting out-of-range values to NaN with a printed count.

---

## 7. Step 14 — Validate (`14_validate_zcta_panel.py`)

Mirror `06_validate_panels.py`; exit non-zero on failure.

1. **Shape**: no duplicate `(zcta5, date)`; every date in range present in the
   correct era; per-era ZCTA counts match `zcta_meta.csv`.
2. **Vintage tripwire**: no `zcta510` row after 2019-12-31 and no `zcta520`
   row before 2020-01-01.
3. **Physical**: `tmin ≤ tmean ≤ tmax` (allow a small tolerance for
   independently interpolated PRISM surfaces); `tdmean ≤ tmean`;
   `0 ≤ rh_mean ≤ 100`; `vpd_mean ≥ 0`.
4. **Cross-panel consistency** — the highest-value check: population-agnostic
   area-weighted mean of ZCTA `tmax` within a county-day, compared against
   `tx_county_daily_weather`. These are different geographies and different
   resolutions (800m vs 4km), so expect small differences; flag |Δ| > 1.5 °C
   on more than 1% of county-days for investigation rather than asserting
   equality.
5. **Coverage**: % non-NA by variable and year. Any year below 99% means the
   download has gaps — cross-check `logs/download_failures_800m.csv`.
6. **Spot check**: print Harris County (Houston) ZCTA `77002` for
   2011-08-01…2011-08-07 (the 2011 heat wave) and eyeball the values.

---

## 8. Execution order

```bash
cd Z:/ek559/DeathStar/DeathStar/prism
PY="Z:/ek559/nys_algal_bloom/NYS algal bloom/code2/Scripts/python.exe"

"$PY" 11_get_zctas.py                  # ~5 min
"$PY" 10_download_clip_prism800.py     # ~26 h — run detached, resumable
"$PY" 12_extract_zcta_weather.py       # ~2 h parallel
"$PY" 13_build_zcta_panel.py           # ~20 min
"$PY" 14_validate_zcta_panel.py        # ~10 min
```
Step 11 must precede 12 but is independent of 10, so run it first and confirm
the ~1,989 count before committing to the 26-hour download.

Add a `run_zcta_pipeline.sh` following `run_pipeline.sh`'s structure
(numbered steps, `logs/` timestamped log, resume-from-step argument).

---

## 9. Rules for the implementing model

1. **Do not touch `raw_data/`.** Everything new goes to
   `prism_raw_800m_tx/`, `intermediate_data/`, or `clean_data/`.
2. **Do not modify the existing 4km pipeline** (`01`–`06`, `config.yaml`'s
   existing keys). The county/CZ panels are built and validated; this is an
   additive pipeline. Only *append* the new `prism800`/`zcta` config blocks.
3. **Do not overwrite an existing file in `clean_data/` without asking** —
   `.claude/CLAUDE.md` requires confirmation.
4. **Report the §3.4 cost estimate and get approval before launching step 10.**
   It is >10 minutes and >500 MB, which the project instructions gate on user
   approval.
5. **Every script gets the standard header block** (Script / Purpose / Inputs /
   Outputs / Author / Date) and uses `snake_case`.
6. **Resume support is mandatory** in steps 10 and 12 — check for valid
   existing output and skip. Validate content, not just existence (§3.2).
7. **`git status` is dirty right now.** Flag this and ask whether to commit
   before starting a multi-file session; suggest a branch such as
   `zcta-800m-panel`.
8. If a verified fact in §1 turns out to be wrong at runtime, **stop and report
   it** rather than working around it silently — several of these facts
   (rate limits, COG migration) have hard operational consequences.
