# CLAUDE.md — Repository Root

This is the git root for the Municipal Heat Ordinances & Worker Health project.
Project-specific instructions (data safeguards, coding conventions, variable
naming) live in [DeathStar/.claude/CLAUDE.md](DeathStar/.claude/CLAUDE.md) —
treat that file as read-only and authoritative for how to work in this repo.
This file documents the folder layout and, in particular, the PRISM weather
pipeline outputs, since they are referenced across build/analysis scripts.

---

## Folder layout

```
DeathStar/ (git root)
├── raw_data/               # READ ONLY — source data (Census shapefiles, CZ crosswalk,
│                            #   TDI claims, BLS QCEW, ICD crosswalks). ~11 GB, gitignored.
├── prism_raw/               # READ ONLY once downloaded — raw PRISM daily BIL rasters,
│                            #   one folder per variable-day. Gitignored, huge; never `ls`
│                            #   it unfiltered.
├── intermediate_data/       # Derived, reproducible from scripts. Gitignored.
│   ├── cz_shapefiles/        # Dissolved CZ2020 polygons
│   ├── county_shapefiles/    # TX county polygons (cb_2024) with cz_id attached
│   ├── extracted/            # Per-variable-year zonal extraction CSVs, CZ-direct
│   └── extracted_county/     # Per-variable-year zonal extraction CSVs, county-direct
├── clean_data/               # Analysis-ready outputs. Gitignored (incl. all *.parquet).
│   ├── tx_county_daily_weather.{csv,parquet}   # County panel (254 counties x days)
│   ├── tx_cz_daily_weather.{csv,parquet}       # CZ panel, DERIVED from county panel
│   ├── county_meta.csv                         # County crosswalk/metadata (254 rows)
│   ├── cz_meta.csv                             # CZ crosswalk/metadata (49 rows)
│   └── claimsclean.parquet                     # Cleaned TDI workers' comp claims
├── DeathStar/                # Code
│   ├── .claude/CLAUDE.md      # Project instructions (data safeguards, conventions)
│   ├── build/                 # Data cleaning and construction scripts
│   ├── prism/                 # PRISM weather pipeline (see below)
│   ├── analysis/               # Regression and estimation scripts
│   └── figures/                 # Plot and visualization scripts
├── output/
│   ├── tables/                # LaTeX/CSV regression tables
│   └── figures/                # Publication-quality figures
└── writing/                   # Paper drafts and notes
```

---

## The PRISM weather pipeline

Lives in [DeathStar/prism/](DeathStar/prism/); see
[DeathStar/prism/README.md](DeathStar/prism/README.md) for full step-by-step
docs. Produces two panel datasets, written to `clean_data/`:

| Dataset | Grain | Rows | Key |
|---|---|---|---|
| `tx_county_daily_weather` | Texas county x day | 254 x 5,920 | `fips` x `date` |
| `tx_cz_daily_weather` | Texas commuting zone x day | 49 x 5,920 | `cz_id` x `date` |

The CZ panel is **derived from the county panel** by area-weighted aggregation
(not extracted independently from PRISM rasters), so county and CZ values are
always reconcilable — see `04c_build_cz_panel.py` and the validation in
`06_validate_panels.py` (§6.2 internal-consistency check).

Both panels use **fixed geography**: 2024 Census county boundaries (`cb_2024`)
and CZ2020 definitions, applied uniformly across 2010–2026. This is a
deliberate choice, valid for Texas because no Texas county boundary or FIPS
code changed in that window — see `DeathStar/prism/README.md` for the full
rationale and the Census Bureau source it's based on.

Weather variables: `tmax`, `tmin`, `tmean`, `ppt` (native PRISM units — °C,
mm) plus humidity derived from `tdmean`: `rh_mean` (%) and `vpd_mean` (kPa).
