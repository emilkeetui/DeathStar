"""
06_validate_panels.py
===========================================================
Step 6: Validate the county and CZ panel datasets.

Exits non-zero on ANY failure (not just print) — this script is
meant to be usable as a CI-style gate, per plan §6.

Checks:
  6.1  Shape (counts, no duplicate keys)
  6.2  Internal consistency — CZ values reproduce from county panel
  6.3  Physical plausibility (temperature ordering, ranges)
  6.4  Geography tripwire (254 counties, 49 CZs, complete partition)
  6.5  Coverage report (% non-NA per variable per year)
  6.6  Sanity spot-check (known CZ climatology)
===========================================================
"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path
import yaml

with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

COUNTY_PANEL = Path(cfg["output"]["county_panel_parquet"])
CZ_PANEL     = Path(cfg["output"]["panel_parquet"])
COUNTY_META  = Path(cfg["output"]["county_meta_csv"])
CZ_META      = Path(cfg["output"]["cz_meta_csv"])
VARIABLES    = cfg["prism"]["variables"]
START_DATE   = pd.Timestamp(cfg["prism"]["start_date"])
END_DATE     = pd.Timestamp(cfg["prism"]["end_date"])
N_DAYS       = (END_DATE - START_DATE).days + 1

failures = []

def check(name: str, condition: bool, detail: str = ""):
    status = "PASS" if condition else "FAIL"
    print(f"  [{status}] {name}" + (f" — {detail}" if detail else ""))
    if not condition:
        failures.append(name)


print("=" * 55)
print("  Step 6: Panel Validation")
print("=" * 55)

county = pd.read_parquet(COUNTY_PANEL)
county["fips"] = county["fips"].astype(str)
cz = pd.read_parquet(CZ_PANEL)
cz["cz_id"] = cz["cz_id"].astype(str)
county_meta = pd.read_csv(COUNTY_META, dtype={"fips": str, "cz_id": str})

print(f"\n  County panel : {len(county):,} rows, {county.shape[1]} cols")
print(f"  CZ panel     : {len(cz):,} rows, {cz.shape[1]} cols")
print(f"  Expected days: {N_DAYS:,}")

# ── 6.1 Shape ────────────────────────────────────────────
print("\n--- 6.1 Shape ---")
check("County: 254 unique fips", county["fips"].nunique() == 254,
      f"found {county['fips'].nunique()}")
check("County: 5920 unique dates", county["date"].nunique() == N_DAYS,
      f"found {county['date'].nunique()}")
check("County: row count == 254 * n_days", len(county) == 254 * N_DAYS,
      f"found {len(county):,}, expected {254*N_DAYS:,}")
check("County: no duplicate (fips, date)", not county.duplicated(subset=["fips", "date"]).any())

check("CZ: 49 unique cz_id", cz["cz_id"].nunique() == 49,
      f"found {cz['cz_id'].nunique()}")
check("CZ: 5920 unique dates", cz["date"].nunique() == N_DAYS,
      f"found {cz['date'].nunique()}")
check("CZ: row count == 49 * n_days", len(cz) == 49 * N_DAYS,
      f"found {len(cz):,}, expected {49*N_DAYS:,}")
check("CZ: no duplicate (cz_id, date)", not cz.duplicated(subset=["cz_id", "date"]).any())

# ── 6.2 Internal consistency ────────────────────────────────
print("\n--- 6.2 Internal consistency (CZ derived from county) ---")
WEIGHTED_VARS = ["tmax", "tmin", "tmean", "ppt", "tdmean", "rh_mean", "vpd_mean"]
county_with_cz = county.merge(
    county_meta[["fips", "cz_id"]], on="fips", how="left", suffixes=("", "_meta")
)
if "cz_id" not in county_with_cz.columns:
    county_with_cz = county_with_cz.rename(columns={"cz_id_meta": "cz_id"})

rng = np.random.default_rng(42)
sample_pairs = cz[["cz_id", "date"]].sample(n=min(20, len(cz)), random_state=42)

n_checked = 0
n_mismatch = 0
for _, row in sample_pairs.iterrows():
    cz_id, date = row["cz_id"], row["date"]
    g = county_with_cz[(county_with_cz["cz_id"] == cz_id) & (county_with_cz["date"] == date)]
    if g.empty:
        continue
    cz_row = cz[(cz["cz_id"] == cz_id) & (cz["date"] == date)].iloc[0]

    for var in WEIGHTED_VARS:
        if var not in g.columns or var not in cz_row.index:
            continue
        x = g[var]
        w = g["area_km2"]
        m = x.notna()
        if not m.any():
            expected = np.nan
        else:
            expected = np.average(x[m], weights=w[m])
        actual = cz_row[var]
        n_checked += 1
        if pd.isna(expected) and pd.isna(actual):
            continue
        if pd.isna(expected) != pd.isna(actual) or abs(expected - actual) > 1e-6:
            n_mismatch += 1
            print(f"    MISMATCH cz_id={cz_id} date={date.date()} var={var}: "
                  f"expected={expected}, actual={actual}")

check("20 sampled CZ-days reproduce from county panel (tol 1e-6)",
      n_mismatch == 0, f"{n_checked} value-checks, {n_mismatch} mismatches")

# ── 6.3 Physical plausibility ───────────────────────────────
print("\n--- 6.3 Physical plausibility ---")
valid_temp = county[["tmin", "tmean", "tmax"]].dropna()
violations = ((valid_temp["tmin"] > valid_temp["tmean"]) |
              (valid_temp["tmean"] > valid_temp["tmax"])).sum()
pct_viol = 100 * violations / len(valid_temp) if len(valid_temp) else 0
check("County: tmin <= tmean <= tmax (tolerate a handful)",
      pct_viol < 1.0, f"{violations:,} violations ({pct_viol:.3f}%)")

check("County: ppt >= 0", (county["ppt"].dropna() >= 0).all())

if "tdmean" in county.columns:
    td_tmax = county[["tdmean", "tmax"]].dropna()
    check("County: tdmean <= tmax", (td_tmax["tdmean"] <= td_tmax["tmax"]).all())

if "rh_mean" in county.columns:
    rh = county["rh_mean"].dropna()
    check("County: 0 <= rh_mean <= 100", ((rh >= 0) & (rh <= 100)).all())

if "vpd_mean" in county.columns:
    vpd = county["vpd_mean"].dropna()
    check("County: vpd_mean >= 0", (vpd >= 0).all())

REASONABLE_RANGES = {
    "ppt"      : (-0.1, 800),
    "tmax"     : (-30,  55),
    "tmin"     : (-35,  40),
    "tmean"    : (-30,  50),
    "tdmean"   : (-40,  35),
    "rh_mean"  : (0,    100),
    "vpd_mean" : (0,    10),
}
for var, (lo, hi) in REASONABLE_RANGES.items():
    if var not in county.columns:
        continue
    vals = county[var].dropna()
    out_of_range = ((vals < lo) | (vals > hi)).sum()
    check(f"County: {var} within [{lo}, {hi}]", out_of_range == 0,
          f"{out_of_range} out-of-range values")

# ── 6.4 Geography tripwire ──────────────────────────────────
print("\n--- 6.4 Geography tripwire ---")
check("Exactly 254 TX counties", county_meta["fips"].nunique() == 254)
cz_meta = pd.read_csv(CZ_META, dtype={"cz_id": str})
check("Exactly 49 TX CZs", cz_meta["cz_id"].nunique() == 49)
check("Every fips maps to exactly one cz_id",
      county_meta.groupby("fips")["cz_id"].nunique().eq(1).all())

n_counties_sum = cz["n_counties"].sum()
check("CZ->county partition complete: sum(n_counties)/n_days == 254",
      abs(n_counties_sum / N_DAYS - 254) < 1e-9,
      f"sum(n_counties)={n_counties_sum:,}, /n_days={n_counties_sum/N_DAYS:.4f}")

# ── 6.5 Coverage report ─────────────────────────────────────
print("\n--- 6.5 Coverage report ---")
report_vars = VARIABLES + ["rh_mean", "vpd_mean"]
for label, df, key in [("County", county, "fips"), ("CZ", cz, "cz_id")]:
    print(f"\n  {label} panel — % non-NA by variable by year:")
    df2 = df.copy()
    df2["year"] = pd.to_datetime(df2["date"]).dt.year
    for var in report_vars:
        if var not in df2.columns:
            continue
        pct_by_year = df2.groupby("year")[var].apply(lambda s: 100 * s.notna().mean())
        low_years = pct_by_year[pct_by_year < 99.5]
        summary = ", ".join(f"{yr}:{pct:.0f}%" for yr, pct in low_years.items())
        print(f"    {var:10s}: overall {100*df2[var].notna().mean():5.1f}%"
              + (f"  | gaps -> {summary}" if summary else ""))

# ── 6.6 Sanity spot-check ───────────────────────────────────
print("\n--- 6.6 Sanity spot-check ---")
cz2 = cz.copy()
cz2["month"] = pd.to_datetime(cz2["date"]).dt.month
july = cz2[cz2["month"] == 7]

spot_check_ids = ["219"] + sorted(cz2["cz_id"].unique())[:3]
for cz_id in dict.fromkeys(spot_check_ids):
    sub = july[july["cz_id"] == cz_id]
    if sub.empty:
        continue
    mean_tmax = sub["tmax"].mean()
    mean_rh = sub["rh_mean"].mean() if "rh_mean" in sub.columns else np.nan
    label = " (Gulf Coast / Houston-Beaumont)" if cz_id == "219" else ""
    print(f"    cz_id={cz_id}{label}: July mean tmax={mean_tmax:.1f}°C, "
          f"mean rh={mean_rh:.1f}%")

if "219" in cz2["cz_id"].values:
    houston_tmax = july[july["cz_id"] == "219"]["tmax"].mean()
    check("Houston/Beaumont (cz_id 219) July tmax ~34°C (28-38 range)",
          28 <= houston_tmax <= 38, f"actual={houston_tmax:.1f}°C")

# ── Summary ──────────────────────────────────────────────
print("\n" + "=" * 55)
if failures:
    print(f"  VALIDATION FAILED — {len(failures)} check(s) failed:")
    for f in failures:
        print(f"    - {f}")
    print("=" * 55)
    sys.exit(1)
else:
    print("  VALIDATION PASSED — all checks OK")
    print("=" * 55)
    sys.exit(0)
