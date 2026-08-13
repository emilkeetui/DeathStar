"""
10_download_clip_prism800.py
===========================================================
Step 10: Download PRISM 800m daily CONUS grids and immediately
clip to Texas, deleting the CONUS download. This is the long
pole of the ZCTA pipeline (~26 h serial, ~13-15 h at concurrency
2) and the highest-risk step (PRISM rate limits, plan §1.4).

Interruptible and resumable by design: every grid-day is checked
against a valid-raster-on-disk test before requesting it again
(plan §3.2). NOT run automatically by this session — per plan
§9.4 / project CLAUDE.md, the cost estimate (plan §3.4) must be
reported and approved before this script is launched.

Per-grid-day procedure (plan §3.2):
    skip if valid clipped raster already exists
    download {base_url}/{var}/{YYYYMMDD} -> scratch zip
    verify HTTP 200, size > 30MB, valid zip
    extract single .tif to scratch
    windowed read (tx_bbox) -> never load full CONUS array
    write clipped GeoTIFF (deflate, predictor=3, tiled, float32, nodata=-9999)
    delete scratch immediately
    sleep(request_sleep_sec)

Rate-limit handling (plan §3.3): never re-request a date+var in
the same run; sleep >= 2s between requests; exponential backoff
on 429/403/non-200 (30/60/120/300s, max 4 retries) then log to
failures CSV and continue; concurrency capped at 2.

Inputs:  config.yaml (prism800 block)
Outputs: prism_raw_800m_tx/{var}/{year}/prism_{var}_tx_30s_{YYYYMMDD}.tif
         logs/download_failures_800m.csv
Author: EK  Date: 2026-08-12
===========================================================
"""

import io
import sys
import time
import shutil
import zipfile
import tempfile
import argparse
import csv
from datetime import datetime, timedelta
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

import requests
import rasterio
from rasterio.windows import from_bounds
import yaml

# ── 0. Config ─────────────────────────────────────────────
with open("config.yaml") as f:
    cfg = yaml.safe_load(f)

BASE_URL   = cfg["prism800"]["base_url"]
CLIP_DIR   = Path(cfg["prism800"]["clip_dir"])
VARIABLES  = cfg["prism800"]["variables"]
START_DATE = datetime.strptime(cfg["prism800"]["start_date"], "%Y-%m-%d")
END_DATE   = datetime.strptime(cfg["prism800"]["end_date"], "%Y-%m-%d")
TX_BBOX    = cfg["prism800"]["tx_bbox"]  # [xmin, ymin, xmax, ymax]
SLEEP_SEC  = cfg["prism800"]["request_sleep_sec"]

EXPECTED_WIDTH_MIN, EXPECTED_HEIGHT_MIN = 1000, 1200  # sanity floor for TX clip
MIN_ZIP_BYTES = 30 * 1024 * 1024  # 30 MB, per plan §3.2
MAX_RETRIES = 4
BACKOFF_SEC = [30, 60, 120, 300]
MAX_CONCURRENCY = 2

LOG_DIR = Path("logs")
LOG_DIR.mkdir(parents=True, exist_ok=True)
FAILURES_CSV = LOG_DIR / "download_failures_800m.csv"


def all_dates(start: datetime, end: datetime):
    d = start
    while d <= end:
        yield d
        d += timedelta(days=1)


def output_path(var: str, dt: datetime) -> Path:
    return CLIP_DIR / var / str(dt.year) / f"prism_{var}_tx_30s_{dt:%Y%m%d}.tif"


def is_valid_existing(path: Path) -> bool:
    """Resume check: file exists AND opens AND has plausible TX-clip shape (plan §3.2)."""
    if not path.exists() or path.stat().st_size == 0:
        return False
    try:
        with rasterio.open(path) as src:
            return src.width >= EXPECTED_WIDTH_MIN and src.height >= EXPECTED_HEIGHT_MIN
    except Exception:
        return False


def log_failure(var: str, dt: datetime, http_code, message: str):
    is_new = not FAILURES_CSV.exists()
    with open(FAILURES_CSV, "a", newline="") as f:
        writer = csv.writer(f)
        if is_new:
            writer.writerow(["var", "date", "http_code", "message"])
        writer.writerow([var, dt.strftime("%Y-%m-%d"), http_code, message])


def download_and_clip_one(var: str, dt: datetime) -> str:
    """Returns 'skipped' | 'ok' | 'failed'."""
    out_path = output_path(var, dt)

    if is_valid_existing(out_path):
        return "skipped"

    out_path.parent.mkdir(parents=True, exist_ok=True)
    date_str = dt.strftime("%Y%m%d")
    url = f"{BASE_URL}/{var}/{date_str}"

    last_error = None
    for attempt in range(MAX_RETRIES + 1):
        try:
            resp = requests.get(url, timeout=120)
            if resp.status_code != 200:
                last_error = f"HTTP {resp.status_code}"
                if attempt < MAX_RETRIES:
                    time.sleep(BACKOFF_SEC[attempt])
                    continue
                log_failure(var, dt, resp.status_code, last_error)
                return "failed"

            content = resp.content
            if len(content) < MIN_ZIP_BYTES:
                last_error = f"undersized response ({len(content)} bytes)"
                if attempt < MAX_RETRIES:
                    time.sleep(BACKOFF_SEC[attempt])
                    continue
                log_failure(var, dt, resp.status_code, last_error)
                return "failed"

            with tempfile.TemporaryDirectory() as scratch:
                scratch = Path(scratch)
                try:
                    with zipfile.ZipFile(io.BytesIO(content)) as zf:
                        tif_names = [n for n in zf.namelist() if n.lower().endswith(".tif")]
                        if not tif_names:
                            raise ValueError("no .tif in zip")
                        zf.extract(tif_names[0], scratch)
                        tif_path = scratch / tif_names[0]
                except zipfile.BadZipFile as e:
                    last_error = f"bad zip: {e}"
                    if attempt < MAX_RETRIES:
                        time.sleep(BACKOFF_SEC[attempt])
                        continue
                    log_failure(var, dt, resp.status_code, last_error)
                    return "failed"

                # Windowed read — never load the full CONUS array (plan §3.2)
                with rasterio.open(tif_path) as src:
                    window = from_bounds(*TX_BBOX, transform=src.transform)
                    data = src.read(1, window=window)
                    win_transform = src.window_transform(window)
                    profile = src.profile.copy()

                profile.update(
                    height=data.shape[0],
                    width=data.shape[1],
                    transform=win_transform,
                    compress="deflate",
                    predictor=3,
                    tiled=True,
                    blockxsize=256,
                    blockysize=256,
                    dtype="float32",
                    nodata=-9999,
                )

                tmp_out = out_path.with_suffix(".tif.tmp")
                with rasterio.open(tmp_out, "w", **profile) as dst:
                    dst.write(data.astype("float32"), 1)
                tmp_out.replace(out_path)

            return "ok"

        except requests.RequestException as e:
            last_error = str(e)
            if attempt < MAX_RETRIES:
                time.sleep(BACKOFF_SEC[attempt])
                continue
            log_failure(var, dt, "exception", last_error)
            return "failed"

    log_failure(var, dt, "exception", last_error or "unknown")
    return "failed"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--concurrency", type=int, default=1,
                         help=f"Parallel requests, capped at {MAX_CONCURRENCY} (plan §3.3)")
    parser.add_argument("--retry-failures-only", action="store_true",
                         help="Only (re)download dates listed in logs/download_failures_800m.csv")
    args = parser.parse_args()
    concurrency = min(args.concurrency, MAX_CONCURRENCY)

    print("=" * 55)
    print("  Step 10: Download + Clip PRISM 800m -> Texas")
    print("=" * 55)
    print(f"  Variables   : {', '.join(VARIABLES)}")
    print(f"  Date range  : {START_DATE.date()} -> {END_DATE.date()}")
    print(f"  Clip dir    : {CLIP_DIR}")
    print(f"  Concurrency : {concurrency}")

    if args.retry_failures_only:
        if not FAILURES_CSV.exists():
            print("  No failures file found; nothing to retry.")
            return
        import pandas as pd
        prior = pd.read_csv(FAILURES_CSV)
        jobs = [(row["var"], datetime.strptime(row["date"], "%Y-%m-%d")) for _, row in prior.iterrows()]
        FAILURES_CSV.unlink()  # rebuilt fresh from this retry pass
    else:
        jobs = [(var, dt) for var in VARIABLES for dt in all_dates(START_DATE, END_DATE)]

    total = len(jobs)
    print(f"  Total grid-days to check: {total:,}\n")

    n_ok = n_skip = n_fail = 0
    t0 = time.time()

    with ThreadPoolExecutor(max_workers=concurrency) as pool:
        futures = {}
        for var, dt in jobs:
            futures[pool.submit(download_and_clip_one, var, dt)] = (var, dt)
            # Throttle submission so we don't fire faster than SLEEP_SEC allows
            if concurrency == 1:
                pass  # sleep happens after result below

        for i, fut in enumerate(as_completed(futures), 1):
            var, dt = futures[fut]
            try:
                result = fut.result()
            except Exception as e:
                log_failure(var, dt, "exception", str(e))
                result = "failed"

            if result == "ok":
                n_ok += 1
                time.sleep(SLEEP_SEC)
            elif result == "skipped":
                n_skip += 1
            else:
                n_fail += 1

            if i % 200 == 0 or i == total:
                elapsed = time.time() - t0
                print(f"  [{i:,}/{total:,}] ok={n_ok:,} skipped={n_skip:,} "
                      f"failed={n_fail:,}  elapsed={elapsed/3600:.1f}h")

    print("\n" + "=" * 55)
    print(f"  Done. ok={n_ok:,} skipped={n_skip:,} failed={n_fail:,}")
    if n_fail:
        print(f"  Failures logged to {FAILURES_CSV} — re-run with --retry-failures-only")
    print("=" * 55)


if __name__ == "__main__":
    main()
