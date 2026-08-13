"""
11_get_zctas.py
===========================================================
Step 11: Build Texas ZCTA polygon files, two eras.

Downloads the pinned Census cartographic-boundary ZCTA files
(plan §2.1), selects the Texas subset by representative-point
containment (plan §2, borrowed from 20260812_PRISMA data.Rmd),
attaches county_fips/cz_id by largest-area overlap with the TX
county layer, and writes one GeoPackage + zcta_meta.csv row set
per era (ZCTA510: 2010-2019, ZCTA520: 2020-2026).

ZCTAs are only redelineated at the decennial census (plan §1.7),
so "time-varying ZCTA shapefiles" reduces to this two-era design.

Inputs:  config.yaml (zcta.eras), raw_data cb_2024 county shapefile
Outputs: intermediate_data/zcta_shapefiles/tx_{era}.gpkg
         clean_data/zcta_meta.csv
Author: EK  Date: 2026-08-12
===========================================================
"""

import io
import zipfile
import requests
import numpy as np
import pandas as pd
import geopandas as gpd
from pathlib import Path
import yaml

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

ERAS         = cfg["zcta"]["eras"]
SHP_DIR      = Path(cfg["zcta"]["shp_dir"])
COUNTY_SHP   = Path(cfg["counties"]["county_shp"])
STATE_FIPS   = cfg["counties"]["state_fips"]
CRS          = cfg["output"]["crs"]
ZCTA_META    = Path(cfg["output_zcta"]["zcta_meta_csv"])

SHP_DIR.mkdir(parents=True, exist_ok=True)
ZCTA_META.parent.mkdir(parents=True, exist_ok=True)

EXPECTED_MIN, EXPECTED_MAX = 1900, 2100  # plan §4 step 4 tripwire

print("=" * 55)
print("  Step 11: Texas ZCTA Polygon Files (two eras)")
print("=" * 55)

# ── 1. Load Texas county layer (mask + county_fips/cz_id join) ──
print(f"\n  Loading county shapefile: {COUNTY_SHP}")
counties = gpd.read_file(COUNTY_SHP)
if "GEOID" in counties.columns:
    counties = counties.rename(columns={"GEOID": "county_fips"})
elif "FIPS" in counties.columns:
    counties = counties.rename(columns={"FIPS": "county_fips"})
counties["county_fips"] = counties["county_fips"].astype(str).str.zfill(5)
if "NAME" in counties.columns:
    counties = counties.rename(columns={"NAME": "county_name"})

tx_counties = counties[counties["county_fips"].str.startswith(STATE_FIPS)].copy()
tx_counties = tx_counties[["county_fips", "county_name", "geometry"]].copy()
print(f"  Texas counties: {len(tx_counties)}")

# County -> cz_id crosswalk, same source as 02b_get_counties.py
county_meta_path = Path(cfg["output"]["county_meta_csv"])
if county_meta_path.exists():
    cz_xw = pd.read_csv(county_meta_path, dtype={"fips": str, "cz_id": str})
    cz_xw = cz_xw.rename(columns={"fips": "county_fips"})[["county_fips", "cz_id", "cz_name"]]
    tx_counties = tx_counties.merge(cz_xw, on="county_fips", how="left")
else:
    print(f"  Warning: {county_meta_path} not found; cz_id will be NA. Run 02b_get_counties.py first.")
    tx_counties["cz_id"] = np.nan
    tx_counties["cz_name"] = np.nan

tx_counties = tx_counties.to_crs("EPSG:4269")
tx_union = tx_counties.geometry.union_all()

# Texas Albers, for area-based county/cz assignment and area_km2/centroid
tx_counties_proj = tx_counties.to_crs("EPSG:3083")


def download_zip_shapefile(url: str, cache_dir: Path) -> Path:
    """Download+unzip a Census shapefile bundle if not already cached; return the .shp path."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    marker = cache_dir / ".complete"
    if marker.exists():
        shp_files = list(cache_dir.glob("*.shp"))
        if shp_files:
            return shp_files[0]

    print(f"    Downloading {url} ...")
    resp = requests.get(url, timeout=300)
    resp.raise_for_status()
    with zipfile.ZipFile(io.BytesIO(resp.content)) as zf:
        zf.extractall(cache_dir)
    marker.touch()

    shp_files = list(cache_dir.glob("*.shp"))
    if not shp_files:
        raise FileNotFoundError(f"No .shp found after extracting {url} to {cache_dir}")
    return shp_files[0]


def assign_county_cz(zcta_gdf: gpd.GeoDataFrame) -> gpd.GeoDataFrame:
    """Attach county_fips/county_name/cz_id/cz_name by largest-area overlap."""
    zcta_proj = zcta_gdf.to_crs("EPSG:3083")
    overlay = gpd.overlay(
        zcta_proj[["zcta5", "geometry"]], tx_counties_proj,
        how="intersection", keep_geom_type=True,
    )
    overlay["overlap_area"] = overlay.geometry.area
    best = (
        overlay.sort_values("overlap_area", ascending=False)
        .drop_duplicates(subset="zcta5", keep="first")
        [["zcta5", "county_fips", "county_name", "cz_id", "cz_name"]]
    )
    return zcta_gdf.merge(best, on="zcta5", how="left")


# ── 2. Process each era ────────────────────────────────────
all_meta = []

for era in ERAS:
    name    = era["name"]
    url     = era["url"]
    id_col  = era["id_col"]
    out_file = SHP_DIR / f"tx_{name}.gpkg"

    print(f"\n>> Era: {name} ({era['start']} -> {era['end']})")

    if out_file.exists() and ZCTA_META.exists():
        existing_meta = pd.read_csv(ZCTA_META, dtype={"zcta5": str})
        if (existing_meta["zcta_vintage"] == name).any():
            print(f"    Output already exists: {out_file}")
            n_existing = (existing_meta["zcta_vintage"] == name).sum()
            print(f"    zcta_meta.csv already has {n_existing} rows for {name}. Skipping.")
            era_meta = existing_meta[existing_meta["zcta_vintage"] == name]
            all_meta.append(era_meta)
            continue

    cache_dir = SHP_DIR / f"_raw_{name}"
    shp_path = download_zip_shapefile(url, cache_dir)

    print(f"    Reading {shp_path} ...")
    zcta = gpd.read_file(shp_path)
    zcta = zcta.rename(columns={id_col: "zcta5"})
    zcta["zcta5"] = zcta["zcta5"].astype(str).str.zfill(5)
    zcta = zcta.to_crs("EPSG:4269")

    # ── Select TX ZCTAs by representative-point containment (§2, §4 step 4) ──
    rep_pts = zcta.geometry.representative_point()
    in_tx = rep_pts.within(tx_union)
    tx_zcta = zcta.loc[in_tx, ["zcta5", "geometry"]].copy()
    print(f"    ZCTAs intersecting TX bbox: {len(zcta)}; true TX ZCTAs (rep-point test): {len(tx_zcta)}")

    assert EXPECTED_MIN <= len(tx_zcta) <= EXPECTED_MAX, (
        f"[{name}] Expected {EXPECTED_MIN}-{EXPECTED_MAX} TX ZCTAs, found {len(tx_zcta)}. "
        "Tripwire per plan §4 step 4 — stop and investigate."
    )

    # ── Validity + geometry type filter (§4 step 6) ──
    tx_zcta["geometry"] = tx_zcta.geometry.make_valid()
    tx_zcta = tx_zcta[tx_zcta.geom_type.isin(["Polygon", "MultiPolygon"])].copy()

    # ── Area / centroid (Texas Albers, §4 step 7) ──
    proj = tx_zcta.to_crs("EPSG:3083")
    tx_zcta["area_km2"] = proj.geometry.area / 1e6
    centroids_wgs84 = proj.geometry.centroid.to_crs("EPSG:4326")
    tx_zcta["centroid_lon"] = centroids_wgs84.x
    tx_zcta["centroid_lat"] = centroids_wgs84.y
    tx_zcta["zcta_vintage"] = name

    # ── county_fips / cz_id by largest-area overlap (§4 step 8) ──
    tx_zcta = assign_county_cz(tx_zcta)
    n_missing_cz = tx_zcta["cz_id"].isna().sum()
    if n_missing_cz > 0:
        print(f"    Warning: {n_missing_cz} ZCTAs have no county/cz overlap (likely border slivers)")

    # ── Save polygon file ──
    poly_cols = ["zcta5", "zcta_vintage", "area_km2", "centroid_lon", "centroid_lat",
                 "county_fips", "county_name", "cz_id", "cz_name", "geometry"]
    tx_zcta_out = tx_zcta[poly_cols].copy()
    tx_zcta_out.to_file(out_file, driver="GPKG")
    print(f"    Saved: {out_file}")

    era_meta = pd.DataFrame(tx_zcta_out.drop(columns="geometry"))
    all_meta.append(era_meta)

# ── 3. Write combined zcta_meta.csv ────────────────────────
meta_cols = ["zcta5", "zcta_vintage", "area_km2", "centroid_lon", "centroid_lat",
             "county_fips", "county_name", "cz_id", "cz_name"]
zcta_meta = pd.concat(all_meta, ignore_index=True)[meta_cols]
zcta_meta.to_csv(ZCTA_META, index=False)
print(f"\n  Saved: {ZCTA_META}")

# ── 4. Summary (overlap check vs plan §1.7) ────────────────
print("\n" + "=" * 55)
print("  Summary")
print("=" * 55)
for name in zcta_meta["zcta_vintage"].unique():
    n = (zcta_meta["zcta_vintage"] == name).sum()
    print(f"  {name}: {n} TX ZCTAs")

if set(zcta_meta["zcta_vintage"].unique()) == {"zcta510", "zcta520"}:
    z510 = set(zcta_meta.loc[zcta_meta["zcta_vintage"] == "zcta510", "zcta5"])
    z520 = set(zcta_meta.loc[zcta_meta["zcta_vintage"] == "zcta520", "zcta5"])
    print(f"  In both eras       : {len(z510 & z520)} (plan §1.7 expects ~2,018)")
    print(f"  Only pre-2020      : {len(z510 - z520)} (plan §1.7 expects ~13)")
    print(f"  Only 2020+         : {len(z520 - z510)} (plan §1.7 expects ~75)")

print("\nDone. Proceed to 10_download_clip_prism800.py")
