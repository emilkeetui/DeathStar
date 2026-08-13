"""
12_extract_zcta_weather.py
===========================================================
Step 12: Zonal extraction of PRISM 800m clipped rasters onto
Texas ZCTA polygons, month-stacked (plan §1.6 — 16x faster than
per-day extraction, since exact_extract's per-call cost is
dominated by rasterizing the 1,989 polygons, paid once per call
regardless of band count).

For each era x variable x (year, month) within that era's date
range: stack that month's clipped TX GeoTIFFs into one in-memory
multi-band raster, run exact_extract once, reshape long. Dates
are taken from each file's filename (plan §5.2), never band
order, and mapped back onto extraction columns by index with an
explicit length assertion.

Resume support: skip any (var, era, year, month) whose output CSV
already exists (plan §5.1).

Inputs:  prism_raw_800m_tx/{var}/{year}/prism_{var}_tx_30s_{YYYYMMDD}.tif
         intermediate_data/zcta_shapefiles/tx_{era}.gpkg
Outputs: intermediate_data/extracted_zcta/{var}_{era}_{YYYYMM}.csv
Author: EK  Date: 2026-08-12
===========================================================
"""

import re
import sys
import argparse
from pathlib import Path
from calendar import monthrange
from datetime import date
from concurrent.futures import ProcessPoolExecutor, as_completed

import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.io import MemoryFile
from exactextract import exact_extract
import yaml

with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

CLIP_DIR      = Path(cfg["prism800"]["clip_dir"])
VARIABLES     = cfg["prism800"]["variables"]
ERAS          = cfg["zcta"]["eras"]
ZCTA_SHP_DIR  = Path(cfg["zcta"]["shp_dir"])
EXTRACTED_DIR = Path(cfg["output_zcta"]["extracted_zcta_dir"])

EXTRACTED_DIR.mkdir(parents=True, exist_ok=True)

FNAME_RE = re.compile(r"prism_(\w+)_tx_30s_(\d{8})\.tif$")


def months_in_range(start: date, end: date):
    y, m = start.year, start.month
    while (y, m) <= (end.year, end.month):
        yield (y, m)
        m += 1
        if m > 12:
            m = 1
            y += 1


def find_month_files(var: str, year: int, month: int) -> list[tuple[date, Path]]:
    """Return sorted [(date, path), ...] for one var-month, keyed off filenames (plan §5.2)."""
    month_dir = CLIP_DIR / var / str(year)
    if not month_dir.exists():
        return []
    _, n_days = monthrange(year, month)
    prefix = f"prism_{var}_tx_30s_{year}{month:02d}"
    files = sorted(month_dir.glob(f"{prefix}*.tif"))

    bands = []
    for f in files:
        m = FNAME_RE.match(f.name)
        if not m:
            continue
        d = date(int(m.group(2)[:4]), int(m.group(2)[4:6]), int(m.group(2)[6:8]))
        bands.append((d, f))
    bands.sort(key=lambda x: x[0])
    return bands


def build_stack(bands: list[tuple[date, Path]]):
    """Stack a month of single-band clipped TIFFs into one in-memory multi-band raster (plan §5.3)."""
    with rasterio.open(bands[0][1]) as src0:
        profile = src0.profile.copy()
        shape = (src0.height, src0.width)

    n = len(bands)
    stack = np.empty((n, shape[0], shape[1]), dtype="float32")
    for i, (_, path) in enumerate(bands):
        with rasterio.open(path) as src:
            stack[i] = src.read(1)

    profile.update(count=n, dtype="float32")
    memfile = MemoryFile()
    with memfile.open(**profile) as dst:
        dst.write(stack)
    return memfile


def extract_one_month(var: str, era_name: str, year: int, month: int, polys_path: str) -> str:
    out_file = EXTRACTED_DIR / f"{var}_{era_name}_{year}{month:02d}.csv"
    if out_file.exists():
        return f"[{var}/{era_name}] {year}-{month:02d}: already done, skipped"

    bands = find_month_files(var, year, month)
    if not bands:
        return f"[{var}/{era_name}] {year}-{month:02d}: NO FILES (gap)"

    polys = gpd.read_file(polys_path)

    memfile = build_stack(bands)
    try:
        with memfile.open() as raster_ds:
            wide = exact_extract(raster_ds, polys, ["mean"], include_cols=["zcta5"],
                                  output="pandas")
    finally:
        memfile.close()

    # exact_extract(..., output="pandas") names columns band_1_mean..band_N_mean,
    # 1-indexed in band order == bands[] order (verified against installed
    # exactextract 0.3.0; plan §5.2 requires filename-derived dates, not band
    # order, so band order is only used as an index into bands[], never as
    # a date source itself).
    band_cols = [f"band_{i+1}_mean" for i in range(len(bands))]
    missing = [c for c in band_cols if c not in wide.columns]
    assert not missing, (
        f"[{var}/{era_name}] {year}-{month:02d}: expected columns {missing} not found "
        f"in exact_extract output {list(wide.columns)}"
    )

    long_df = wide.melt(id_vars=["zcta5"], value_vars=band_cols,
                         var_name="band_col", value_name=var)
    band_to_date = {col: d.isoformat() for (d, _path), col in zip(bands, band_cols)}
    long_df["date"] = long_df["band_col"].map(band_to_date)
    long_df = long_df.drop(columns="band_col").sort_values(["zcta5", "date"])

    df = long_df[["zcta5", "date", var]]
    df.to_csv(out_file, index=False)
    return f"[{var}/{era_name}] {year}-{month:02d}: {len(bands)} days x {polys.shape[0]} ZCTAs -> {out_file.name}"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--workers", type=int, default=4,
                         help="Parallel processes across variables (plan §5.5)")
    args = parser.parse_args()

    print("=" * 55)
    print("  Step 12: Zonal Extraction — PRISM 800m -> ZCTA")
    print("=" * 55)

    jobs = []
    for era in ERAS:
        era_name = era["name"]
        polys_path = ZCTA_SHP_DIR / f"tx_{era_name}.gpkg"
        if not polys_path.exists():
            print(f"  Skipping era {era_name}: {polys_path} not found. Run 11_get_zctas.py first.")
            continue
        start = date.fromisoformat(era["start"])
        end = date.fromisoformat(era["end"])
        for var in VARIABLES:
            for (y, m) in months_in_range(start, end):
                jobs.append((var, era_name, y, m, str(polys_path)))

    print(f"  Total var-era-months to check: {len(jobs)}\n")

    if args.workers <= 1:
        for job in jobs:
            print("  " + extract_one_month(*job))
    else:
        with ProcessPoolExecutor(max_workers=args.workers) as pool:
            futures = {pool.submit(extract_one_month, *job): job for job in jobs}
            for fut in as_completed(futures):
                job = futures[fut]
                try:
                    print("  " + fut.result())
                except Exception as e:
                    print(f"  [{job[0]}/{job[1]}] {job[2]}-{job[3]:02d}: ERROR — {e}")

    print("\nDone. Proceed to 13_build_zcta_panel.py")


if __name__ == "__main__":
    main()
