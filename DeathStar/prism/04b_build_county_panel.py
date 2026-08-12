"""
04b_build_county_panel.py
===========================================================
Step 4b: Assemble, clean, and validate the county panel dataset.

Reads per-variable per-year CSVs from intermediate_data/extracted_county/
and merges them into a single long-format panel:

    fips | date | ppt | tmax | tmin | tmean | tdmean

Also adds:
  - County metadata (name, cz_id, area, centroid)
  - Year, month, day-of-year columns
  - Data quality flags (PRISM stability tier: stable/provisional/early)
  - Derived humidity variables (rh_mean, vpd_mean) — computed at the
    county level, before any aggregation (see plan §4.5)
  - Basic descriptive statistics and coverage report
  - Saves as both CSV and Parquet

Output: clean_data/tx_county_daily_weather.csv (.parquet)
===========================================================
"""

import warnings
import numpy as np
import pandas as pd
from pathlib import Path
import yaml

warnings.filterwarnings("ignore", category=pd.errors.PerformanceWarning)

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

EXTRACTED_DIR = Path(cfg["output"]["extracted_county_dir"])
OUT_CSV       = Path(cfg["output"]["county_panel_csv"])
OUT_PARQUET   = Path(cfg["output"]["county_panel_parquet"])
OUT_DIR       = OUT_CSV.parent
VARIABLES     = cfg["prism"]["variables"]
START_DATE    = pd.Timestamp(cfg["prism"]["start_date"])
END_DATE      = pd.Timestamp(cfg["prism"]["end_date"])
COUNTY_META   = Path(cfg["output"]["county_meta_csv"])

OUT_DIR.mkdir(parents=True, exist_ok=True)

print("=" * 55)
print("  Step 4b: Build County Panel Dataset")
print("=" * 55)
print(f"  Variables : {', '.join(VARIABLES)}")
print(f"  Date range: {START_DATE.date()} -> {END_DATE.date()}")
print(f"  Output    : {OUT_CSV}")
print()

# ── 1. Load all extracted CSVs ─────────────────────────────
def load_extracted_data(var: str) -> pd.DataFrame:
    """Load all yearly CSVs for one variable and concatenate."""
    files = sorted(EXTRACTED_DIR.glob(f"{var}_*.csv"))

    if not files:
        print(f"  Warning: No extracted files for variable '{var}'")
        return pd.DataFrame()

    dfs = []
    for f in files:
        try:
            df = pd.read_csv(f, dtype={"fips": str})
            df["date"] = pd.to_datetime(df["date"])
            dfs.append(df)
        except Exception as e:
            print(f"  Warning: Could not read {f}: {e}")

    if not dfs:
        return pd.DataFrame()

    combined = pd.concat(dfs, ignore_index=True)
    combined = combined.sort_values(["fips", "date"]).reset_index(drop=True)
    print(f"  [{var}] Loaded {len(combined):,} rows from {len(files)} files")
    return combined


# ── 2. Merge all variables ─────────────────────────────────
def merge_variables(variables: list) -> pd.DataFrame:
    """
    Merge variable-specific DataFrames into one wide panel.
    Join key: (fips, date)
    """
    print("\n  Merging variables...")
    panel = None

    for var in variables:
        df = load_extracted_data(var)
        if df.empty:
            continue

        if var not in df.columns:
            print(f"  Skipping {var}: column '{var}' not found")
            continue

        df = df[["fips", "date", var]].copy()

        if panel is None:
            panel = df
        else:
            panel = panel.merge(df, on=["fips", "date"], how="outer")

    if panel is None or panel.empty:
        raise ValueError("No data loaded. Run 03b_extract_county_weather.R first.")

    return panel


# ── 3. Create complete fips × date skeleton ────────────────
def create_skeleton(panel: pd.DataFrame, all_fips) -> pd.DataFrame:
    """
    Ensure every county × date combination exists in the panel,
    even if all weather values are missing (flags data gaps).
    Uses the full 254-county list from county_meta.csv, not just
    the counties present in the extracted data, so a raster that
    is missing for a whole county-year still surfaces as NA rows
    rather than silently vanishing.
    """
    all_dates = pd.date_range(START_DATE, END_DATE, freq="D")

    skeleton = pd.MultiIndex.from_product(
        [all_fips, all_dates],
        names=["fips", "date"]
    ).to_frame(index=False)

    complete = skeleton.merge(panel, on=["fips", "date"], how="left")
    n_orig = len(panel)
    n_full = len(complete)

    if n_full > n_orig:
        print(f"\n  Note: Skeleton added {n_full - n_orig:,} missing county-day rows")

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


# ── 6. Merge in county metadata ────────────────────────────
def merge_county_metadata(df: pd.DataFrame) -> pd.DataFrame:
    """Add county name, cz_id, area, and centroid from county_meta.csv."""
    if not COUNTY_META.exists():
        raise FileNotFoundError(
            f"{COUNTY_META} not found. Run 02b_get_counties.py first."
        )

    meta = pd.read_csv(COUNTY_META, dtype={"fips": str, "cz_id": str})
    df["fips"] = df["fips"].astype(str)

    return df.merge(meta, on="fips", how="left")


# ── 7. Derived humidity variables (§4.5) ───────────────────
# Computed at the county level, before aggregation to CZ, to avoid
# Jensen's-inequality bias from deriving nonlinear quantities post-average.
def es(temp_c):
    """Saturation vapor pressure (Magnus / August-Roche-Magnus), kPa."""
    return 0.6108 * np.exp(17.27 * temp_c / (temp_c + 237.3))


def add_humidity_variables(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    ea = es(df["tdmean"])          # actual vapor pressure
    es_tmean = es(df["tmean"])     # saturation vapor pressure at tmean

    rh = 100 * ea / es_tmean
    df["rh_mean"] = rh.clip(lower=0, upper=100)

    vpd = es_tmean - ea
    df["vpd_mean"] = vpd.clip(lower=0)

    return df


# ── 8. Unit conversions / sanity checks ───────────────────
REASONABLE_RANGES = {
    "ppt"      : (-0.1, 800),    # mm/day (up to ~30 in/day for extreme TX events)
    "tmax"     : (-30,  55),     # °C
    "tmin"     : (-35,  40),     # °C
    "tmean"    : (-30,  50),     # °C
    "tdmean"   : (-40,  35),     # °C
    "rh_mean"  : (0,    100),    # %
    "vpd_mean" : (0,    10),     # kPa
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


# ── 9. Coverage report ─────────────────────────────────────
def print_coverage_report(df: pd.DataFrame):
    print("\n" + "="*55)
    print("  Coverage Report")
    print("="*55)
    print(f"  Total county-day rows : {len(df):,}")
    print(f"  Unique counties       : {df['fips'].nunique()}")
    print(f"  Date range            : {df['date'].min().date()} -> {df['date'].max().date()}")
    print(f"  Calendar days         : {(df['date'].max() - df['date'].min()).days + 1:,}")
    print()

    report_vars = VARIABLES + ["rh_mean", "vpd_mean"]
    for var in report_vars:
        if var not in df.columns:
            continue
        n_valid   = df[var].notna().sum()
        n_total   = len(df)
        pct_valid = 100 * n_valid / n_total
        mean_val  = df[var].mean()
        print(f"  {var:10s}: {pct_valid:5.1f}% valid  (mean={mean_val:.2f})")

    stability_counts = df["prism_stability"].value_counts()
    print()
    print(f"  PRISM stability:")
    for tier, cnt in stability_counts.items():
        print(f"    {tier:12s}: {cnt:,} county-days")


# ── 10. Main ─────────────────────────────────────────────
if __name__ == "__main__":

    # County list comes from metadata (authoritative 254), not from
    # whatever fips values happen to appear in the extracted CSVs.
    county_meta_full = pd.read_csv(COUNTY_META, dtype={"fips": str})
    all_fips = sorted(county_meta_full["fips"].unique())
    assert len(all_fips) == 254, f"Expected 254 counties in county_meta.csv, found {len(all_fips)}"

    # Load and merge
    panel = merge_variables(VARIABLES)
    print(f"\n  Merged panel: {len(panel):,} rows, {panel.shape[1]} columns")

    # Complete skeleton
    panel = create_skeleton(panel, all_fips)

    # Feature engineering
    panel = add_temporal_features(panel)
    panel = add_stability_flags(panel)
    panel = merge_county_metadata(panel)
    panel = add_humidity_variables(panel)

    # Quality checks
    panel = flag_outliers(panel)

    # Column order
    id_cols   = ["fips", "county_name", "date", "year", "month", "day", "doy", "week", "quarter"]
    var_cols  = [v for v in VARIABLES if v in panel.columns]
    humidity_cols = ["rh_mean", "vpd_mean"]
    meta_cols = ["cz_id", "cz_name", "area_km2", "centroid_lon", "centroid_lat", "geo_vintage"]
    flag_cols = ["prism_stability"]
    other_cols = [c for c in panel.columns
                  if c not in id_cols + var_cols + humidity_cols + meta_cols + flag_cols]

    final_cols = id_cols + var_cols + humidity_cols + meta_cols + flag_cols + other_cols
    panel = panel[[c for c in final_cols if c in panel.columns]]

    # Sort
    panel = panel.sort_values(["fips", "date"]).reset_index(drop=True)

    # §6.4-style tripwire before writing
    n_fips  = panel["fips"].nunique()
    n_dates = panel["date"].nunique()
    assert n_fips == 254, f"Expected 254 counties in final panel, found {n_fips}"
    assert not panel.duplicated(subset=["fips", "date"]).any(), "Duplicate (fips, date) rows found"

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

    print("\nCounty panel dataset complete.")
    print(f"  Final shape: {panel.shape[0]:,} rows × {panel.shape[1]} columns")
    print(f"\n  Sample (first 5 rows):")
    print(panel.head().to_string(index=False))
