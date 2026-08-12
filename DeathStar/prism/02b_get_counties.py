"""
02b_get_counties.py
===========================================================
Step 2b: Build Texas county polygon file from local files.

Uses:
  - Local Census county shapefile (cb_2024_us_county_500k)
  - Local USDA 2020 CZ crosswalk (commuting-zones-2020.csv)

Filters to Texas (FIPS prefix "48"), attaches CZ assignment to
each county (for later county -> CZ aggregation), and writes
both the county polygon file and clean_data/county_meta.csv.

Output: intermediate_data/county_shapefiles/tx_counties_2024.gpkg
        clean_data/county_meta.csv
===========================================================
"""

import yaml
import pandas as pd
import geopandas as gpd
from pathlib import Path

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

SHP_DIR      = Path(cfg["counties"]["shp_dir"])
STATE_FIPS   = cfg["counties"]["state_fips"]   # "48"
CRS          = cfg["output"]["crs"]
CROSSWALK    = Path(cfg["counties"]["crosswalk"])
COUNTY_SHP   = Path(cfg["counties"]["county_shp"])
GEO_VINTAGE  = cfg["geo_vintage"]
COUNTY_META  = Path(cfg["output"]["county_meta_csv"])

SHP_DIR.mkdir(parents=True, exist_ok=True)
COUNTY_META.parent.mkdir(parents=True, exist_ok=True)
OUT_FILE = SHP_DIR / "tx_counties_2024.gpkg"

print("=" * 55)
print("  Step 2b: Texas County Polygon File (local files)")
print("=" * 55)

if OUT_FILE.exists() and COUNTY_META.exists():
    print(f"\n  Output already exists: {OUT_FILE}")
    print(f"  Output already exists: {COUNTY_META}")
    print("  Loading to verify...")
    counties_gdf = gpd.read_file(OUT_FILE)
    print(f"  {len(counties_gdf)} Texas counties, CRS: {counties_gdf.crs}")
    assert len(counties_gdf) == 254, (
        f"Expected 254 Texas counties, found {len(counties_gdf)}"
    )
    assert counties_gdf["cz_id"].notna().all(), "Some counties missing cz_id"
    print("\nDone. Proceed to 03b_extract_county_weather.R")
    raise SystemExit(0)

# ── 1. Load crosswalk ──────────────────────────────────────
# Columns: FIPStxt, CountyName, StateName, CZ2020, CZName, ...
print(f"\n  Loading crosswalk: {CROSSWALK}")
xw = pd.read_csv(CROSSWALK, dtype={"FIPStxt": str})
xw["fips"]  = xw["FIPStxt"].str.zfill(5)
xw["cz_id"] = xw["CZ2020"].astype(str)

xw_tx = xw[xw["fips"].str.startswith(STATE_FIPS)][["fips", "cz_id", "CZName"]].copy()
print(f"  Texas counties in crosswalk: {len(xw_tx)}")

# ── 2. Load county shapefile ───────────────────────────────
print(f"\n  Loading county shapefile: {COUNTY_SHP}")
counties = gpd.read_file(COUNTY_SHP)

# Standardize FIPS column — Census shapefiles use GEOID or FIPS
if "GEOID" in counties.columns:
    counties = counties.rename(columns={"GEOID": "fips"})
elif "FIPS" in counties.columns:
    counties = counties.rename(columns={"FIPS": "fips"})

counties["fips"] = counties["fips"].astype(str).str.zfill(5)

if "NAME" in counties.columns:
    counties = counties.rename(columns={"NAME": "county_name"})

# Filter to Texas
tx_counties = counties[counties["fips"].str.startswith(STATE_FIPS)].copy()
tx_counties = tx_counties[["fips", "county_name", "geometry"]].copy()
print(f"  Texas counties in shapefile: {len(tx_counties)}")

# ── 3. Attach CZ assignment ────────────────────────────────
print("\n  Merging crosswalk onto county geometries...")
merged = tx_counties.merge(xw_tx, on="fips", how="left")
merged = merged.rename(columns={"CZName": "cz_name"})

n_missing = merged["cz_id"].isna().sum()
if n_missing > 0:
    missing_fips = merged.loc[merged["cz_id"].isna(), "fips"].tolist()
    raise ValueError(f"{n_missing} counties missing CZ assignment: {missing_fips}")

# ── 4. Add area and centroid metadata ──────────────────────
# Project to Texas-centric equal-area (Texas Albers) for accurate area calc
counties_proj = merged.to_crs("EPSG:3083")
merged["area_km2"] = counties_proj.geometry.area / 1e6
centroids_wgs84 = counties_proj.geometry.centroid.to_crs("EPSG:4326")
merged["centroid_lon"] = centroids_wgs84.x
merged["centroid_lat"] = centroids_wgs84.y

merged["geo_vintage"] = GEO_VINTAGE

merged = merged.to_crs(CRS)

# ── 5. Validate (§1 tripwire) ──────────────────────────────
assert len(merged) == 254, f"Expected 254 Texas counties, found {len(merged)}"
assert merged["cz_id"].notna().all(), "Some counties missing cz_id after merge"

print(f"\n  County shapefile summary:")
print(f"    N counties       : {len(merged)}")
print(f"    CRS               : {merged.crs}")
print(f"    Geometry type     : {merged.geom_type.unique()}")
print(f"    Bounding box      : {merged.total_bounds.round(2)}")
print(f"    N distinct CZs    : {merged['cz_id'].nunique()}")

# ── 6. Save polygon file ───────────────────────────────────
county_cols = ["fips", "county_name", "cz_id", "cz_name", "area_km2",
               "centroid_lon", "centroid_lat", "geo_vintage", "geometry"]
counties_gdf = merged[county_cols].copy()
counties_gdf.to_file(OUT_FILE, driver="GPKG")
print(f"\n  Saved: {OUT_FILE}")

# ── 7. Save county metadata CSV ────────────────────────────
meta_cols = ["fips", "county_name", "cz_id", "cz_name", "area_km2",
             "centroid_lon", "centroid_lat", "geo_vintage"]
county_meta = pd.DataFrame(counties_gdf.drop(columns="geometry"))[meta_cols]
county_meta.to_csv(COUNTY_META, index=False)
print(f"  Saved: {COUNTY_META}")

print("\nDone. Proceed to 03b_extract_county_weather.R")
