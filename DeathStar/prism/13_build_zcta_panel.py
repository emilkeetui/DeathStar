"""
13_build_zcta_panel.py
===========================================================
Step 13: Assemble, clean, and validate the ZCTA panel dataset.

Reads per-variable per-era per-month CSVs from
intermediate_data/extracted_zcta/, merges into a long-format
panel keyed on (zcta5, date), and builds the skeleton PER ERA
(plan §6.1) — never crossing the full ZCTA union with the full
date range, since that would fabricate rows for ZCTAs in periods
where they did not exist under that era's delineation.

Column conventions mirror 04b_build_county_panel.py so the three
panels (county/CZ/ZCTA) are interoperable.

Inputs:  intermediate_data/extracted_zcta/{var}_{era}_{YYYYMM}.csv
         clean_data/zcta_meta.csv
Outputs: clean_data/tx_zcta_daily_weather.csv (.parquet)
Author: EK  Date: 2026-08-12
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

EXTRACTED_DIR = Path(cfg["output_zcta"]["extracted_zcta_dir"])
OUT_CSV       = Path(cfg["output_zcta"]["zcta_panel_csv"])
OUT_PARQUET   = Path(cfg["output_zcta"]["zcta_panel_parquet"])
OUT_DIR       = OUT_CSV.parent
VARIABLES     = cfg["prism800"]["variables"]
ERAS          = cfg["zcta"]["eras"]
ZCTA_META     = Path(cfg["output_zcta"]["zcta_meta_csv"])

OUT_DIR.mkdir(parents=True, exist_ok=True)

print("=" * 55)
print("  Step 13: Build ZCTA Panel Dataset")
print("=" * 55)
print(f"  Variables : {', '.join(VARIABLES)}")
print(f"  Eras      : {', '.join(e['name'] for e in ERAS)}")
print(f"  Output    : {OUT_CSV}")
print()


# ── 1. Load all extracted CSVs for one variable × era ───────
def load_extracted_data(var: str, era_name: str) -> pd.DataFrame:
    files = sorted(EXTRACTED_DIR.glob(f"{var}_{era_name}_*.csv"))
    if not files:
        print(f"  Warning: No extracted files for {var}/{era_name}")
        return pd.DataFrame()

    dfs = []
    for f in files:
        try:
            df = pd.read_csv(f, dtype={"zcta5": str})
            df["date"] = pd.to_datetime(df["date"])
            dfs.append(df)
        except Exception as e:
            print(f"  Warning: Could not read {f}: {e}")

    if not dfs:
        return pd.DataFrame()

    combined = pd.concat(dfs, ignore_index=True)
    combined = combined.sort_values(["zcta5", "date"]).reset_index(drop=True)
    print(f"  [{var}/{era_name}] Loaded {len(combined):,} rows from {len(files)} files")
    return combined


# ── 2. Merge all variables, one era at a time ───────────────
def merge_variables_for_era(variables: list, era_name: str) -> pd.DataFrame:
    panel = None
    for var in variables:
        df = load_extracted_data(var, era_name)
        if df.empty:
            continue
        if var not in df.columns:
            print(f"  Skipping {var}: column '{var}' not found")
            continue
        df = df[["zcta5", "date", var]].copy()
        panel = df if panel is None else panel.merge(df, on=["zcta5", "date"], how="outer")

    if panel is None or panel.empty:
        raise ValueError(f"No data loaded for era {era_name}. Run 12_extract_zcta_weather.py first.")
    return panel


# ── 3. Per-era skeleton (plan §6.1 — do NOT cross full ZCTA union
#      with full date range) ────────────────────────────────
def create_era_skeleton(panel: pd.DataFrame, era_zctas, start_date, end_date) -> pd.DataFrame:
    all_dates = pd.date_range(start_date, end_date, freq="D")
    skeleton = pd.MultiIndex.from_product(
        [era_zctas, all_dates], names=["zcta5", "date"]
    ).to_frame(index=False)
    complete = skeleton.merge(panel, on=["zcta5", "date"], how="left")
    n_orig, n_full = len(panel), len(complete)
    if n_full > n_orig:
        print(f"    Skeleton added {n_full - n_orig:,} missing zcta-day rows")
    return complete


# ── 4. Temporal features ────────────────────────────────────
def add_temporal_features(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    df["year"]    = df["date"].dt.year
    df["month"]   = df["date"].dt.month
    df["day"]     = df["date"].dt.day
    df["doy"]     = df["date"].dt.day_of_year
    df["week"]    = df["date"].dt.isocalendar().week.astype(int)
    df["quarter"] = df["date"].dt.quarter
    return df


# ── 5. PRISM stability flag (plan §5.4 — no longer in filename) ─
def add_stability_flags(df: pd.DataFrame) -> pd.DataFrame:
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


# ── 6. Merge in ZCTA metadata ───────────────────────────────
def merge_zcta_metadata(df: pd.DataFrame, meta: pd.DataFrame, era_name: str) -> pd.DataFrame:
    era_meta = meta[meta["zcta_vintage"] == era_name].drop(columns="zcta_vintage")
    merged = df.merge(era_meta, on="zcta5", how="left")
    merged["zcta_vintage"] = era_name
    return merged


# ── 7. Derived humidity variables (plan §6.2, at ZCTA level,
#      before any aggregation — Jensen's inequality) ────────
def es(temp_c):
    """Saturation vapor pressure (Magnus / August-Roche-Magnus), kPa."""
    return 0.6108 * np.exp(17.27 * temp_c / (temp_c + 237.3))


def add_humidity_variables(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    ea = es(df["tdmean"])
    es_tmean = es(df["tmean"])
    df["rh_mean"] = (100 * ea / es_tmean).clip(lower=0, upper=100)
    df["vpd_mean"] = (es_tmean - ea).clip(lower=0)
    return df


# ── 8. Plausibility flags (plan §6.5, reuse county panel ranges) ─
REASONABLE_RANGES = {
    "tmax"     : (-30,  55),   # °C
    "tmin"     : (-35,  40),
    "tmean"    : (-30,  50),
    "tdmean"   : (-40,  35),
    "rh_mean"  : (0,    100),
    "vpd_mean" : (0,    10),
}


def flag_outliers(df: pd.DataFrame) -> pd.DataFrame:
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


# ── 9. Coverage report ──────────────────────────────────────
def print_coverage_report(df: pd.DataFrame):
    print("\n" + "=" * 55)
    print("  Coverage Report")
    print("=" * 55)
    print(f"  Total zcta-day rows : {len(df):,}")
    print(f"  Unique ZCTAs        : {df['zcta5'].nunique()}")
    print(f"  Date range          : {df['date'].min().date()} -> {df['date'].max().date()}")
    print()

    report_vars = VARIABLES + ["rh_mean", "vpd_mean"]
    for var in report_vars:
        if var not in df.columns:
            continue
        n_valid = df[var].notna().sum()
        pct_valid = 100 * n_valid / len(df)
        mean_val = df[var].mean()
        print(f"  {var:10s}: {pct_valid:5.1f}% valid  (mean={mean_val:.2f})")

    print()
    print("  Rows by era:")
    for era_name, cnt in df["zcta_vintage"].value_counts().items():
        print(f"    {era_name:10s}: {cnt:,}")


# ── 10. Main ─────────────────────────────────────────────
if __name__ == "__main__":
    if not ZCTA_META.exists():
        raise FileNotFoundError(f"{ZCTA_META} not found. Run 11_get_zctas.py first.")
    zcta_meta_full = pd.read_csv(ZCTA_META, dtype={"zcta5": str})

    era_panels = []
    for era in ERAS:
        era_name = era["name"]
        start_date = pd.Timestamp(era["start"])
        end_date = pd.Timestamp(era["end"])
        print(f"\n>> Era: {era_name} ({start_date.date()} -> {end_date.date()})")

        era_zctas = sorted(zcta_meta_full.loc[zcta_meta_full["zcta_vintage"] == era_name, "zcta5"].unique())
        if not era_zctas:
            raise ValueError(f"No ZCTAs found for era {era_name} in {ZCTA_META}")
        print(f"   {len(era_zctas)} ZCTAs for this era")

        panel = merge_variables_for_era(VARIABLES, era_name)
        print(f"   Merged: {len(panel):,} rows, {panel.shape[1]} columns")

        panel = create_era_skeleton(panel, era_zctas, start_date, end_date)
        panel = add_temporal_features(panel)
        panel = add_stability_flags(panel)
        panel = merge_zcta_metadata(panel, zcta_meta_full, era_name)
        panel = add_humidity_variables(panel)
        panel = flag_outliers(panel)

        era_panels.append(panel)

    panel = pd.concat(era_panels, ignore_index=True)

    # ── Vintage tripwire (plan §7.2, checked early since it's cheap) ──
    z510_bad = panel[(panel["zcta_vintage"] == "zcta510") & (panel["date"] > pd.Timestamp("2019-12-31"))]
    z520_bad = panel[(panel["zcta_vintage"] == "zcta520") & (panel["date"] < pd.Timestamp("2020-01-01"))]
    assert z510_bad.empty, f"{len(z510_bad)} zcta510 rows found after 2019-12-31"
    assert z520_bad.empty, f"{len(z520_bad)} zcta520 rows found before 2020-01-01"

    # ── Column order (plan §6.3) ────────────────────────────
    id_cols   = ["zcta5", "date", "year", "month", "day", "doy", "week", "quarter"]
    var_cols  = [v for v in VARIABLES if v in panel.columns]
    humidity_cols = ["rh_mean", "vpd_mean"]
    meta_cols = ["zcta_vintage", "county_fips", "county_name", "cz_id", "cz_name",
                 "area_km2", "centroid_lon", "centroid_lat"]
    flag_cols = ["prism_stability"]
    other_cols = [c for c in panel.columns
                  if c not in id_cols + var_cols + humidity_cols + meta_cols + flag_cols]

    final_cols = id_cols + var_cols + humidity_cols + meta_cols + flag_cols + other_cols
    panel = panel[[c for c in final_cols if c in panel.columns]]
    panel = panel.sort_values(["zcta5", "date"]).reset_index(drop=True)

    # ── Tripwires before writing ────────────────────────────
    assert not panel.duplicated(subset=["zcta5", "date"]).any(), "Duplicate (zcta5, date) rows found"

    print_coverage_report(panel)

    n_rows = len(panel)
    print(f"\n  Final row count: {n_rows:,} (plan §6.4 expects ~11.8M)")
    if n_rows > 15_000_000 or n_rows < 8_000_000:
        print("  WARNING: row count is well outside the plan's expected range — investigate before proceeding.")

    # ── Save ─────────────────────────────────────────────
    print(f"\n  Saving CSV     -> {OUT_CSV}")
    panel.to_csv(OUT_CSV, index=False)

    print(f"  Saving Parquet -> {OUT_PARQUET}")
    panel.to_parquet(OUT_PARQUET, index=False, compression="snappy")

    mb_csv     = OUT_CSV.stat().st_size / 1e6
    mb_parquet = OUT_PARQUET.stat().st_size / 1e6
    print(f"\n  File sizes:")
    print(f"    CSV     : {mb_csv:.1f} MB")
    print(f"    Parquet : {mb_parquet:.1f} MB  ({100*(1-mb_parquet/mb_csv):.0f}% smaller)")
    if mb_csv > 1024:
        print(f"  NOTE: CSV output is >1 GB ({mb_csv/1024:.2f} GB) — flagged per .claude/CLAUDE.md")

    print("\nZCTA panel dataset complete.")
    print(f"  Final shape: {panel.shape[0]:,} rows x {panel.shape[1]} columns")
    print(f"\n  Sample (first 5 rows):")
    print(panel.head().to_string(index=False))
