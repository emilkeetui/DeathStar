"""
04c_build_cz_panel.py
===========================================================
Step 4c: Aggregate the county panel up to commuting zones.

This is the core deliverable of the plan: the CZ panel is
DERIVED from the county panel (not extracted independently),
so county and CZ values are always reconcilable.

    1. Read tx_county_daily_weather.parquet
    2. Join cz_id from county_meta.csv
    3. Group by (cz_id, date) and aggregate with area weights
    4. Attach cz_meta.csv; add n_counties
    5. Assert 49 CZs, 290,080 rows
    6. Save CSV + Parquet

Aggregation (plan §4.2): area-weighted mean for tmax, tmin, tmean,
tdmean, rh_mean, vpd_mean, AND ppt (ppt is a per-unit-area depth,
so area-weighted mean — not sum — is the correct CZ-level average).
Weights are renormalized over non-missing counties per CZ-day; if
every county in a CZ-day is NA, the result is NA (never 0).

Output: clean_data/tx_cz_daily_weather.csv (.parquet)
        clean_data/cz_meta.csv
===========================================================
"""

import numpy as np
import pandas as pd
import geopandas as gpd
from pathlib import Path
import yaml

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

COUNTY_PANEL = Path(cfg["output"]["county_panel_parquet"])
COUNTY_META  = Path(cfg["output"]["county_meta_csv"])
OUT_CSV      = Path(cfg["output"]["panel_csv"])
OUT_PARQUET  = Path(cfg["output"]["panel_parquet"])
CZ_META_CSV  = Path(cfg["output"]["cz_meta_csv"])
CZ_SHP       = Path(cfg["commuting_zones"]["shp_dir"]) / "tx_commuting_zones_2020.gpkg"
CZ_VINTAGE   = cfg["cz_vintage"]
VARIABLES    = cfg["prism"]["variables"]
START_DATE   = pd.Timestamp(cfg["prism"]["start_date"])
END_DATE     = pd.Timestamp(cfg["prism"]["end_date"])

OUT_CSV.parent.mkdir(parents=True, exist_ok=True)

print("=" * 55)
print("  Step 4c: Aggregate County Panel -> CZ Panel")
print("=" * 55)

WEIGHTED_VARS = ["tmax", "tmin", "tmean", "ppt", "tdmean", "rh_mean", "vpd_mean"]

# ── 1. Read county panel ───────────────────────────────────
print(f"\n  Loading county panel: {COUNTY_PANEL}")
county = pd.read_parquet(COUNTY_PANEL)
county["fips"] = county["fips"].astype(str)
print(f"  Loaded {len(county):,} county-day rows")

# ── 2. Join cz_id (already present via county_meta merge in 04b,
#      but re-join explicitly from county_meta.csv as the source
#      of truth per plan §3.5 step 2) ───────────────────────────
meta = pd.read_csv(COUNTY_META, dtype={"fips": str, "cz_id": str})
county = county.drop(columns=[c for c in ["cz_id", "cz_name", "area_km2"] if c in county.columns])
county = county.merge(
    meta[["fips", "cz_id", "cz_name", "area_km2"]],
    on="fips", how="left"
)

assert county["cz_id"].notna().all(), "Some county rows missing cz_id after join"

# ── 3. Area-weighted aggregation ───────────────────────────
def wmean(group_df: pd.DataFrame, var: str, weight_col: str = "area_km2"):
    """Area-weighted mean, renormalized over non-missing rows only."""
    x = group_df[var]
    w = group_df[weight_col]
    m = x.notna()
    if not m.any():
        return np.nan
    return np.average(x[m], weights=w[m])


print("\n  Aggregating counties -> CZ-days (area-weighted)...")
grouped = county.groupby(["cz_id", "date"], sort=False)

agg_records = []
for (cz_id, date), g in grouped:
    rec = {"cz_id": cz_id, "date": date, "n_counties": len(g)}
    for var in WEIGHTED_VARS:
        if var in g.columns:
            rec[var] = wmean(g, var)
    agg_records.append(rec)

cz_panel = pd.DataFrame.from_records(agg_records)
print(f"  Produced {len(cz_panel):,} CZ-day rows")

# ── 4. Temporal features + stability flag (recompute, same logic
#      as county panel, since date is preserved through aggregation) ──
cz_panel["date"]    = pd.to_datetime(cz_panel["date"])
cz_panel["year"]    = cz_panel["date"].dt.year
cz_panel["month"]   = cz_panel["date"].dt.month
cz_panel["day"]     = cz_panel["date"].dt.day
cz_panel["doy"]     = cz_panel["date"].dt.day_of_year
cz_panel["week"]    = cz_panel["date"].dt.isocalendar().week.astype(int)
cz_panel["quarter"] = cz_panel["date"].dt.quarter

today = pd.Timestamp.today()
def classify(dt):
    delta = (today - dt).days
    if delta > 180:
        return "stable"
    elif delta > 14:
        return "provisional"
    else:
        return "early"

cz_panel["prism_stability"] = cz_panel["date"].map(classify)

# ── 5. Attach CZ metadata; write cz_meta.csv ───────────────
print(f"\n  Loading CZ shapefile: {CZ_SHP}")
cz_gdf = gpd.read_file(CZ_SHP)
cz_gdf["cz_id"] = cz_gdf["cz_id"].astype(str)

cz_meta = pd.DataFrame(cz_gdf.drop(columns="geometry"))
cz_meta = cz_meta.rename(columns={"CZName": "cz_name"})
cz_meta["geo_vintage"] = CZ_VINTAGE
cz_meta_cols = ["cz_id", "cz_name", "area_km2", "centroid_lon", "centroid_lat", "geo_vintage"]
cz_meta = cz_meta[[c for c in cz_meta_cols if c in cz_meta.columns]]

assert len(cz_meta) == 49, f"Expected 49 Texas CZs, found {len(cz_meta)}"

cz_meta.to_csv(CZ_META_CSV, index=False)
print(f"  Saved: {CZ_META_CSV}")

cz_panel = cz_panel.merge(cz_meta, on="cz_id", how="left")

# ── 6. Assertions (plan §3.5 step 5) ───────────────────────
n_cz    = cz_panel["cz_id"].nunique()
n_dates = cz_panel["date"].nunique()
n_rows  = len(cz_panel)

assert n_cz == 49, f"Expected 49 CZs, found {n_cz}"
assert n_rows == 49 * 5920, f"Expected {49*5920:,} rows, found {n_rows:,}"
assert not cz_panel.duplicated(subset=["cz_id", "date"]).any(), "Duplicate (cz_id, date) rows found"

print(f"\n  Validation OK: {n_cz} CZs x {n_dates} dates = {n_rows:,} rows")

# ── 7. Column order ─────────────────────────────────────────
id_cols   = ["cz_id", "cz_name", "date", "year", "month", "day", "doy", "week", "quarter"]
var_cols  = [v for v in VARIABLES if v in cz_panel.columns]
humidity_cols = [c for c in ["rh_mean", "vpd_mean"] if c in cz_panel.columns]
meta_cols = ["n_counties", "area_km2", "centroid_lon", "centroid_lat", "geo_vintage"]
flag_cols = ["prism_stability"]
other_cols = [c for c in cz_panel.columns
              if c not in id_cols + var_cols + humidity_cols + meta_cols + flag_cols]

final_cols = id_cols + var_cols + humidity_cols + meta_cols + flag_cols + other_cols
cz_panel = cz_panel[[c for c in final_cols if c in cz_panel.columns]]

cz_panel = cz_panel.sort_values(["cz_id", "date"]).reset_index(drop=True)

# ── 8. Coverage report ──────────────────────────────────────
print("\n" + "=" * 55)
print("  Coverage Report")
print("=" * 55)
print(f"  Total CZ-day rows : {len(cz_panel):,}")
print(f"  Unique CZs        : {cz_panel['cz_id'].nunique()}")
print(f"  Date range        : {cz_panel['date'].min().date()} -> {cz_panel['date'].max().date()}")

for var in var_cols + humidity_cols:
    n_valid   = cz_panel[var].notna().sum()
    pct_valid = 100 * n_valid / len(cz_panel)
    mean_val  = cz_panel[var].mean()
    print(f"  {var:10s}: {pct_valid:5.1f}% valid  (mean={mean_val:.2f})")

# ── 9. Save ──────────────────────────────────────────────
print(f"\n  Saving CSV     -> {OUT_CSV}")
cz_panel.to_csv(OUT_CSV, index=False)

print(f"  Saving Parquet -> {OUT_PARQUET}")
cz_panel.to_parquet(OUT_PARQUET, index=False, compression="snappy")

mb_csv     = OUT_CSV.stat().st_size / 1e6
mb_parquet = OUT_PARQUET.stat().st_size / 1e6
print(f"\n  File sizes:")
print(f"    CSV     : {mb_csv:.1f} MB")
print(f"    Parquet : {mb_parquet:.1f} MB  ({100*(1-mb_parquet/mb_csv):.0f}% smaller)")

print("\nCZ panel dataset complete (derived from county panel).")
print(f"  Final shape: {cz_panel.shape[0]:,} rows × {cz_panel.shape[1]} columns")
