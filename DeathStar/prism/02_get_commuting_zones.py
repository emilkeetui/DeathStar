"""
02_get_commuting_zones.py
===========================================================
Step 2: Build Texas commuting zone polygons from local files.

Uses:
  - Local Census county shapefile (cb_2024_us_county_500k)
  - Local USDA 2020 CZ crosswalk (commuting-zones-2020.csv)

Filters to Texas (FIPS prefix "48"), merges county geometries
with CZ assignments, dissolves to CZ polygons, and saves.

Output: data/cz_shapefiles/tx_commuting_zones_2020.gpkg
===========================================================
"""

import yaml
import pandas as pd
import geopandas as gpd
from pathlib import Path

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

SHP_DIR     = Path(cfg["commuting_zones"]["shp_dir"])
STATE_FIPS  = cfg["commuting_zones"]["state_fips"]   # "48"
CRS         = cfg["output"]["crs"]
CROSSWALK   = Path(cfg["commuting_zones"]["crosswalk"])
COUNTY_SHP  = Path(cfg["commuting_zones"]["county_shp"])

SHP_DIR.mkdir(parents=True, exist_ok=True)
OUT_FILE = SHP_DIR / "tx_commuting_zones_2020.gpkg"

print("=" * 55)
print("  Step 2: Texas Commuting Zone Shapefile (local files)")
print("=" * 55)

if OUT_FILE.exists():
    print(f"\n  Output already exists: {OUT_FILE}")
    print("  Loading to verify...")
    cz_gdf = gpd.read_file(OUT_FILE)
    print(f"  {len(cz_gdf)} Texas commuting zones, CRS: {cz_gdf.crs}")
    print(f"  CZ IDs: {sorted(cz_gdf['cz_id'].tolist())}")
    print("\nDone. Proceed to 03_extract_cz_weather.R")
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

# Filter to Texas
tx_counties = counties[counties["fips"].str.startswith(STATE_FIPS)].copy()
print(f"  Texas counties in shapefile: {len(tx_counties)}")

# ── 3. Merge and dissolve to CZ polygons ──────────────────
print("\n  Merging crosswalk onto county geometries...")
merged = tx_counties.merge(xw_tx, on="fips", how="left")

n_missing = merged["cz_id"].isna().sum()
if n_missing > 0:
    missing_fips = merged.loc[merged["cz_id"].isna(), "fips"].tolist()
    print(f"  Warning: {n_missing} counties missing CZ assignment: {missing_fips}")

merged = merged.dropna(subset=["cz_id"])
print(f"  Dissolving {len(merged)} counties -> CZ polygons...")

cz_gdf = merged.dissolve(by="cz_id", aggfunc="first").reset_index()
cz_gdf = cz_gdf[["cz_id", "CZName", "geometry"]].copy()

# ── 4. Add centroid and area metadata ─────────────────────
# Project to Texas-centric equal-area for accurate area calculation
cz_proj = cz_gdf.to_crs("EPSG:3083")
cz_gdf["area_km2"]      = cz_proj.geometry.area / 1e6
# Compute centroids in projected CRS, then map back to WGS84
centroids_wgs84 = cz_proj.geometry.centroid.to_crs("EPSG:4326")
cz_gdf["centroid_lon"]  = centroids_wgs84.x
cz_gdf["centroid_lat"]  = centroids_wgs84.y

cz_gdf = cz_gdf.to_crs(CRS)

# ── 5. Validate ───────────────────────────────────────────
print(f"\n  CZ shapefile summary:")
print(f"    N commuting zones : {len(cz_gdf)}")
print(f"    CRS               : {cz_gdf.crs}")
print(f"    Geometry type     : {cz_gdf.geom_type.unique()}")
print(f"    Bounding box      : {cz_gdf.total_bounds.round(2)}")
print(f"\n  CZ IDs: {sorted(cz_gdf['cz_id'].tolist())}")

# ── 6. Save ───────────────────────────────────────────────
cz_gdf.to_file(OUT_FILE, driver="GPKG")
print(f"\n  Saved: {OUT_FILE}")

print("\nDone. Proceed to 03_extract_cz_weather.R")
