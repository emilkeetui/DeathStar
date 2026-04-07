"""
04_build_panel.py
===========================================================
Step 4: Assemble, clean, and validate the final panel dataset.

Reads per-variable per-year CSVs from data/extracted/ and
merges them into a single long-format panel:

    cz_id | date | ppt | tmax | tmin | tmean | tdmean | vpdmin | vpdmax

Also adds:
  - Year, month, day-of-year columns
  - Data quality flags (PRISM stability tier: stable/provisional/early)
  - Basic descriptive statistics and coverage report
  - Saves as both CSV and Parquet

Output: output/tx_cz_daily_weather.csv (.parquet)
===========================================================
"""

import os
import re
import yaml
import warnings
import numpy as np
import pandas as pd
import geopandas as gpd
from pathlib import Path
from datetime import date, timedelta

warnings.filterwarnings("ignore", category=pd.errors.PerformanceWarning)

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

EXTRACTED_DIR = Path(cfg["output"]["extracted_dir"])
OUT_DIR       = OUT_CSV.parent
OUT_CSV       = Path(cfg["output"]["panel_csv"])
OUT_PARQUET   = Path(cfg["output"]["panel_parquet"])
VARIABLES     = cfg["prism"]["variables"]
START_DATE    = pd.Timestamp(cfg["prism"]["start_date"])
END_DATE      = pd.Timestamp(cfg["prism"]["end_date"])
SHP_PATH      = Path(cfg["commuting_zones"]["shp_dir"]) / "tx_commuting_zones_2020.gpkg"

OUT_DIR.mkdir(parents=True, exist_ok=True)

print("=" * 55)
print("  Step 4: Build Final Panel Dataset")
print("=" * 55)
print(f"  Variables : {', '.join(VARIABLES)}")
print(f"  Date range: {START_DATE.date()} -> {END_DATE.date()}")
print(f"  Output    : {OUT_CSV}")
print()

# ── 1. Load all extracted CSVs ─────────────────────────────
def load_extracted_data(var: str) -> pd.DataFrame:
    """Load all yearly CSVs for one variable and concatenate."""
    pattern = EXTRACTED_DIR / f"{var}_*.csv"
    files = sorted(EXTRACTED_DIR.glob(f"{var}_*.csv"))

    if not files:
        print(f"  Warning: No extracted files for variable '{var}'")
        return pd.DataFrame()

    dfs = []
    for f in files:
        try:
            df = pd.read_csv(f, dtype={"cz_id": str})
            df["date"] = pd.to_datetime(df["date"])
            dfs.append(df)
        except Exception as e:
            print(f"  Warning: Could not read {f}: {e}")

    if not dfs:
        return pd.DataFrame()

    combined = pd.concat(dfs, ignore_index=True)
    combined = combined.sort_values(["cz_id", "date"]).reset_index(drop=True)
    print(f"  [{var}] Loaded {len(combined):,} rows from {len(files)} files")
    return combined


# ── 2. Merge all variables ─────────────────────────────────
def merge_variables(variables: list) -> pd.DataFrame:
    """
    Merge variable-specific DataFrames into one wide panel.
    Join key: (cz_id, date)
    """
    print("\n  Merging variables...")
    panel = None

    for var in variables:
        df = load_extracted_data(var)
        if df.empty:
            continue

        # Ensure expected columns
        if var not in df.columns:
            print(f"  Skipping {var}: column '{var}' not found")
            continue

        df = df[["cz_id", "date", var]].copy()

        if panel is None:
            panel = df
        else:
            panel = panel.merge(df, on=["cz_id", "date"], how="outer")

    if panel is None or panel.empty:
        raise ValueError("No data loaded. Run 03_extract_cz_weather.R first.")

    return panel


# ── 3. Create complete date-CZ skeleton ───────────────────
def create_skeleton(panel: pd.DataFrame) -> pd.DataFrame:
    """
    Ensure every CZ × date combination exists in the panel,
    even if all weather values are missing (flags data gaps).
    """
    all_dates = pd.date_range(START_DATE, END_DATE, freq="D")
    all_czs   = panel["cz_id"].unique()

    skeleton = pd.MultiIndex.from_product(
        [all_czs, all_dates],
        names=["cz_id", "date"]
    ).to_frame(index=False)

    complete = skeleton.merge(panel, on=["cz_id", "date"], how="left")
    n_orig = len(panel)
    n_full = len(complete)

    if n_full > n_orig:
        print(f"\n  Note: Skeleton added {n_full - n_orig:,} missing CZ-day rows")

    return complete


# ── 4. Add temporal features ───────────────────────────────
def add_temporal_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["year"]    = df["date"].dt.year
    df["month"]   = df["date"].dt.month
    df["day"]     = df["date"].dt.day
    df["doy"]     = df["date"].dt.day_of_year   # day of year 1–366
    df["week"]    = df["date"].dt.isocalendar().week.astype(int)
    df["quarter"] = df["date"].dt.quarter
    return df


# ── 5. Add PRISM stability tier flags ─────────────────────
def add_stability_flags(df: pd.DataFrame) -> pd.DataFrame:
    """
    PRISM data has three stability tiers:
      stable      : finalized (>= ~6 months ago)
      provisional : recent months, subject to minor revision
      early       : most recent ~2 weeks
    We flag based on approximate cutoffs.
    """
    today = pd.Timestamp.today()
    df = df.copy()

    def classify(dt):
        delta = (today - dt).days
        if delta > 180:
            return "stable"
        elif delta > 14:
            return "provisional"
        else:
            return "early"

    df["prism_stability"] = df["date"].map(classify)
    return df


# ── 6. Merge in CZ metadata ───────────────────────────────
def merge_cz_metadata(df: pd.DataFrame) -> pd.DataFrame:
    """Add CZ area and centroid from the shapefile."""
    if not SHP_PATH.exists():
        print("  Shapefile not found; skipping CZ metadata merge.")
        return df

    cz_meta = gpd.read_file(SHP_PATH)[["cz_id", "area_km2",
                                        "centroid_lon", "centroid_lat"]]
    cz_meta["cz_id"] = cz_meta["cz_id"].astype(str)
    df["cz_id"] = df["cz_id"].astype(str)

    return df.merge(cz_meta, on="cz_id", how="left")


# ── 7. Unit conversions / sanity checks ───────────────────
REASONABLE_RANGES = {
    "ppt"    : (-0.1, 800),    # mm/day (up to ~30 in/day for extreme TX events)
    "tmax"   : (-30,  55),     # °C
    "tmin"   : (-35,  40),     # °C
    "tmean"  : (-30,  50),     # °C
    "tdmean" : (-40,  35),     # °C
    "vpdmin" : (0,    10),     # hPa
    "vpdmax" : (0,    80),     # hPa
}

def flag_outliers(df: pd.DataFrame) -> pd.DataFrame:
    """Flag meteorologically implausible values as NaN with a warning."""
    df = df.copy()
    for var, (lo, hi) in REASONABLE_RANGES.items():
        if var not in df.columns:
            continue
        mask_out = (df[var] < lo) | (df[var] > hi)
        n_out = mask_out.sum()
        if n_out > 0:
            print(f"  Warning: {n_out} implausible values in {var} set to NaN")
            df.loc[mask_out, var] = np.nan
    return df


# ── 8. Coverage report ─────────────────────────────────────
def print_coverage_report(df: pd.DataFrame):
    print("\n" + "="*55)
    print("  Coverage Report")
    print("="*55)
    print(f"  Total CZ-day rows : {len(df):,}")
    print(f"  Unique CZs        : {df['cz_id'].nunique()}")
    print(f"  Date range        : {df['date'].min().date()} -> {df['date'].max().date()}")
    print(f"  Calendar days     : {(df['date'].max() - df['date'].min()).days + 1:,}")
    print()

    for var in VARIABLES:
        if var not in df.columns:
            continue
        n_valid   = df[var].notna().sum()
        n_total   = len(df)
        pct_valid = 100 * n_valid / n_total
        mean_val  = df[var].mean()
        print(f"  {var:8s}: {pct_valid:5.1f}% valid  (mean={mean_val:.2f})")

    stability_counts = df["prism_stability"].value_counts()
    print()
    print(f"  PRISM stability:")
    for tier, cnt in stability_counts.items():
        print(f"    {tier:12s}: {cnt:,} CZ-days")


# ── 9. Main ───────────────────────────────────────────────
if __name__ == "__main__":

    # Load and merge
    panel = merge_variables(VARIABLES)
    print(f"\n  Merged panel: {len(panel):,} rows, {panel.shape[1]} columns")

    # Complete skeleton
    panel = create_skeleton(panel)

    # Feature engineering
    panel = add_temporal_features(panel)
    panel = add_stability_flags(panel)
    panel = merge_cz_metadata(panel)

    # Quality checks
    panel = flag_outliers(panel)

    # Column order
    id_cols   = ["cz_id", "date", "year", "month", "day", "doy", "week", "quarter"]
    meta_cols = ["centroid_lon", "centroid_lat", "area_km2"]
    flag_cols = ["prism_stability"]
    var_cols  = [v for v in VARIABLES if v in panel.columns]
    other_cols = [c for c in panel.columns
                  if c not in id_cols + meta_cols + flag_cols + var_cols]

    final_cols = id_cols + var_cols + meta_cols + flag_cols + other_cols
    panel = panel[[c for c in final_cols if c in panel.columns]]

    # Sort
    panel = panel.sort_values(["cz_id", "date"]).reset_index(drop=True)

    # Report
    print_coverage_report(panel)

    # ── Save ─────────────────────────────────────────────
    print(f"\n  Saving CSV    -> {OUT_CSV}")
    panel.to_csv(OUT_CSV, index=False)

    print(f"  Saving Parquet -> {OUT_PARQUET}")
    panel.to_parquet(OUT_PARQUET, index=False, compression="snappy")

    mb_csv     = OUT_CSV.stat().st_size / 1e6
    mb_parquet = OUT_PARQUET.stat().st_size / 1e6
    print(f"\n  File sizes:")
    print(f"    CSV     : {mb_csv:.1f} MB")
    print(f"    Parquet : {mb_parquet:.1f} MB  ({100*(1-mb_parquet/mb_csv):.0f}% smaller)")

    print("\nPanel dataset complete.")
    print(f"  Final shape: {panel.shape[0]:,} rows × {panel.shape[1]} columns")
    print(f"\n  Sample (first 5 rows):")
    print(panel.head().to_string(index=False))
