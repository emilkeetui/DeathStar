"""
14_validate_zcta_panel.py
===========================================================
Step 14: Validate the ZCTA panel dataset. Mirrors
06_validate_panels.py; exits non-zero on ANY failure so this
script is usable as a CI-style gate (plan §7).

Checks:
  1. Shape — no duplicate (zcta5, date); every date in range
     present in the correct era; per-era ZCTA counts match
     zcta_meta.csv.
  2. Vintage tripwire — no zcta510 row after 2019-12-31, no
     zcta520 row before 2020-01-01.
  3. Physical plausibility — tmin <= tmean <= tmax (tolerance),
     tdmean <= tmean, 0 <= rh_mean <= 100, vpd_mean >= 0.
  4. Cross-panel consistency — area-weighted ZCTA tmax rolled up
     to county-day vs tx_county_daily_weather; different
     geographies/resolutions (800m vs 4km), so flag rather than
     assert equality.
  5. Coverage — % non-NA by variable/year; low years flagged
     against logs/download_failures_800m.csv.
  6. Spot check — Harris County ZCTA 77002, 2011-08-01..08-07
     (2011 heat wave).
===========================================================
"""

import sys
import numpy as np
import pandas as pd
from pathlib import Path
import yaml

with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

ZCTA_PANEL   = Path(cfg["output_zcta"]["zcta_panel_parquet"])
ZCTA_META    = Path(cfg["output_zcta"]["zcta_meta_csv"])
COUNTY_PANEL = Path(cfg["output"]["county_panel_parquet"])
VARIABLES    = cfg["prism800"]["variables"]
ERAS         = cfg["zcta"]["eras"]
FAILURES_LOG = Path("logs/download_failures_800m.csv")

failures = []


def check(name: str, condition: bool, detail: str = ""):
    status = "PASS" if condition else "FAIL"
    print(f"  [{status}] {name}" + (f" — {detail}" if detail else ""))
    if not condition:
        failures.append(name)


print("=" * 55)
print("  Step 14: ZCTA Panel Validation")
print("=" * 55)

zcta = pd.read_parquet(ZCTA_PANEL)
zcta["zcta5"] = zcta["zcta5"].astype(str)
zcta["date"] = pd.to_datetime(zcta["date"])
zcta_meta = pd.read_csv(ZCTA_META, dtype={"zcta5": str})

print(f"\n  ZCTA panel: {len(zcta):,} rows, {zcta.shape[1]} cols")

# ── 1. Shape ─────────────────────────────────────────────
print("\n--- 1. Shape ---")
check("No duplicate (zcta5, date)", not zcta.duplicated(subset=["zcta5", "date"]).any())

for era in ERAS:
    era_name = era["name"]
    start_date = pd.Timestamp(era["start"])
    end_date = pd.Timestamp(era["end"])
    n_days = (end_date - start_date).days + 1
    era_df = zcta[zcta["zcta_vintage"] == era_name]

    check(f"{era_name}: date range covers {start_date.date()}..{end_date.date()}",
          era_df["date"].min() == start_date and era_df["date"].max() == end_date,
          f"found {era_df['date'].min().date()}..{era_df['date'].max().date()}")
    check(f"{era_name}: {n_days} unique dates", era_df["date"].nunique() == n_days,
          f"found {era_df['date'].nunique()}")

    expected_zctas = zcta_meta.loc[zcta_meta["zcta_vintage"] == era_name, "zcta5"].nunique()
    check(f"{era_name}: ZCTA count matches zcta_meta.csv ({expected_zctas})",
          era_df["zcta5"].nunique() == expected_zctas,
          f"found {era_df['zcta5'].nunique()}")
    check(f"{era_name}: row count == n_zctas * n_days",
          len(era_df) == expected_zctas * n_days,
          f"found {len(era_df):,}, expected {expected_zctas*n_days:,}")

# ── 2. Vintage tripwire ───────────────────────────────────
print("\n--- 2. Vintage tripwire ---")
z510_bad = zcta[(zcta["zcta_vintage"] == "zcta510") & (zcta["date"] > pd.Timestamp("2019-12-31"))]
z520_bad = zcta[(zcta["zcta_vintage"] == "zcta520") & (zcta["date"] < pd.Timestamp("2020-01-01"))]
check("No zcta510 rows after 2019-12-31", z510_bad.empty, f"{len(z510_bad)} found")
check("No zcta520 rows before 2020-01-01", z520_bad.empty, f"{len(z520_bad)} found")

# ── 3. Physical plausibility ──────────────────────────────
print("\n--- 3. Physical plausibility ---")
valid_temp = zcta[["tmin", "tmean", "tmax"]].dropna()
violations = ((valid_temp["tmin"] > valid_temp["tmean"]) |
              (valid_temp["tmean"] > valid_temp["tmax"])).sum()
pct_viol = 100 * violations / len(valid_temp) if len(valid_temp) else 0
check("tmin <= tmean <= tmax (tolerate a handful)", pct_viol < 1.0,
      f"{violations:,} violations ({pct_viol:.3f}%)")

if "tdmean" in zcta.columns:
    td_tmean = zcta[["tdmean", "tmean"]].dropna()
    n_bad = (td_tmean["tdmean"] > td_tmean["tmean"]).sum()
    pct_bad = 100 * n_bad / len(td_tmean) if len(td_tmean) else 0
    check("tdmean <= tmean (tolerate a handful)", pct_bad < 1.0,
          f"{n_bad:,} violations ({pct_bad:.3f}%)")

if "rh_mean" in zcta.columns:
    rh = zcta["rh_mean"].dropna()
    check("0 <= rh_mean <= 100", ((rh >= 0) & (rh <= 100)).all())

if "vpd_mean" in zcta.columns:
    vpd = zcta["vpd_mean"].dropna()
    check("vpd_mean >= 0", (vpd >= 0).all())

REASONABLE_RANGES = {
    "tmax"     : (-30,  55),
    "tmin"     : (-35,  40),
    "tmean"    : (-30,  50),
    "tdmean"   : (-40,  35),
    "rh_mean"  : (0,    100),
    "vpd_mean" : (0,    10),
}
for var, (lo, hi) in REASONABLE_RANGES.items():
    if var not in zcta.columns:
        continue
    vals = zcta[var].dropna()
    out_of_range = ((vals < lo) | (vals > hi)).sum()
    check(f"{var} within [{lo}, {hi}]", out_of_range == 0, f"{out_of_range} out-of-range values")

# ── 4. Cross-panel consistency ────────────────────────────
print("\n--- 4. Cross-panel consistency (ZCTA -> county rollup vs county panel) ---")
if COUNTY_PANEL.exists():
    county = pd.read_parquet(COUNTY_PANEL)
    county["fips"] = county["fips"].astype(str)
    county["date"] = pd.to_datetime(county["date"])

    zcta_valid = zcta.dropna(subset=["tmax", "area_km2", "county_fips"])
    rollup = (
        zcta_valid.groupby(["county_fips", "date"])
        .apply(lambda g: np.average(g["tmax"], weights=g["area_km2"]))
        .reset_index(name="tmax_from_zcta")
    )
    rollup = rollup.rename(columns={"county_fips": "fips"})

    merged = rollup.merge(county[["fips", "date", "tmax"]], on=["fips", "date"], how="inner")
    merged["abs_diff"] = (merged["tmax_from_zcta"] - merged["tmax"]).abs()

    n_compared = len(merged)
    n_large_diff = (merged["abs_diff"] > 1.5).sum()
    pct_large_diff = 100 * n_large_diff / n_compared if n_compared else 0

    check("ZCTA-rollup vs county tmax: <=1% of county-days differ by >1.5°C",
          pct_large_diff <= 1.0,
          f"{n_large_diff:,}/{n_compared:,} county-days ({pct_large_diff:.2f}%) — "
          "expected small differences (800m vs 4km, different geographies)")
else:
    print(f"  Skipped: {COUNTY_PANEL} not found")

# ── 5. Coverage report ────────────────────────────────────
print("\n--- 5. Coverage report ---")
report_vars = VARIABLES + ["rh_mean", "vpd_mean"]
zcta2 = zcta.copy()
zcta2["year"] = zcta2["date"].dt.year
low_coverage_years = set()
for var in report_vars:
    if var not in zcta2.columns:
        continue
    pct_by_year = zcta2.groupby("year")[var].apply(lambda s: 100 * s.notna().mean())
    low_years = pct_by_year[pct_by_year < 99.0]
    low_coverage_years |= set(low_years.index)
    summary = ", ".join(f"{yr}:{pct:.0f}%" for yr, pct in low_years.items())
    print(f"    {var:10s}: overall {100*zcta2[var].notna().mean():5.1f}%"
          + (f"  | gaps -> {summary}" if summary else ""))

if low_coverage_years:
    print(f"\n  Years below 99% coverage: {sorted(low_coverage_years)}")
    if FAILURES_LOG.exists():
        fail_log = pd.read_csv(FAILURES_LOG)
        fail_log["year"] = pd.to_datetime(fail_log["date"]).dt.year
        overlap = set(fail_log["year"]) & low_coverage_years
        print(f"  Cross-check {FAILURES_LOG}: {len(fail_log)} logged download failures, "
              f"years overlapping low-coverage: {sorted(overlap)}")
    else:
        print(f"  {FAILURES_LOG} not found — cannot cross-check download failures")

# ── 6. Spot check ──────────────────────────────────────────
print("\n--- 6. Spot check: Harris County ZCTA 77002, 2011-08-01..08-07 (heat wave) ---")
spot = zcta[(zcta["zcta5"] == "77002") &
            (zcta["date"] >= "2011-08-01") & (zcta["date"] <= "2011-08-07")]
if spot.empty:
    print("  ZCTA 77002 not found for this date range (check era coverage)")
else:
    print(spot[["zcta5", "date", "tmax", "tmin", "tmean", "rh_mean"]].to_string(index=False))
    mean_tmax = spot["tmax"].mean()
    check("77002 2011-08-01..07 mean tmax in plausible Houston August range (30-42°C)",
          30 <= mean_tmax <= 42, f"actual={mean_tmax:.1f}°C")

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
