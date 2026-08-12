#!/bin/bash
# run_pipeline.sh
# ============================================================
# Executes the full TX County/CZ Weather pipeline in sequence.
# Run from the prism/ directory:
#   bash run_pipeline.sh
#
# Optional: pass a starting step to resume mid-pipeline
#   bash run_pipeline.sh 3     # start at step 3
#
# Step numbering matches PLAN_county_cz_panels.md §8. Steps 00
# and 03b are long-running (background/hours) and independent of
# each other; steps 04b/04c/06 require both to have finished.
# ============================================================

set -euo pipefail

START_STEP=${1:-1}
LOGDIR="logs"
mkdir -p "$LOGDIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOGFILE="$LOGDIR/pipeline_${TIMESTAMP}.log"

log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOGFILE"
}

log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "  Texas County/CZ Weather Pipeline — $(date)"
log "  Starting from step $START_STEP"
log "  Log file: $LOGFILE"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# ── Step 0: Install dependencies ─────────────────────────
if [ "$START_STEP" -le 0 ]; then
  log "▶ Step 0: Installing dependencies..."
  Rscript packages.R 2>&1 | tee -a "$LOGFILE"
  pip install -q -r requirements.txt 2>&1 | tee -a "$LOGFILE"
  log "✓ Dependencies installed."
fi

# ── Step 00: Close the tdmean download gap ───────────────
if [ "$START_STEP" -le 00 ]; then
  log "▶ Step 00: Closing tdmean download gap..."
  log "  (This step may take multiple hours. Run in screen/tmux.)"
  Rscript 00_download_tdmean_gap.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 00 complete."
fi

# ── Step 1: Download PRISM rasters ───────────────────────
if [ "$START_STEP" -le 1 ]; then
  log "▶ Step 1: Downloading PRISM daily rasters..."
  log "  (This step may take 4–12 hours. Run in screen/tmux.)"
  Rscript 01_download_prism.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 1 complete."
fi

# ── Step 2: Get Texas CZ shapefile ───────────────────────
if [ "$START_STEP" -le 2 ]; then
  log "▶ Step 2: Downloading Texas commuting zones shapefile..."
  python 02_get_commuting_zones.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 2 complete."
fi

# ── Step 2b: Get Texas county shapefile ──────────────────
if [ "$START_STEP" -le 2 ]; then
  log "▶ Step 2b: Building Texas county shapefile..."
  python 02b_get_counties.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 2b complete."
fi

# ── Step 3: Extract zonal statistics (CZ, legacy) ────────
if [ "$START_STEP" -le 3 ]; then
  log "▶ Step 3: Extracting zonal weather statistics (CZ-direct, legacy)..."
  log "  (This step may take 2–6 hours.)"
  Rscript 03_extract_cz_weather.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 3 complete."
fi

# ── Step 3b: Extract zonal statistics (county) ───────────
if [ "$START_STEP" -le 3 ]; then
  log "▶ Step 3b: Extracting zonal weather statistics (county)..."
  log "  (This step may take 2–6 hours.)"
  Rscript 03b_extract_county_weather.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 3b complete."
fi

# ── Step 4b: Build county panel ──────────────────────────
if [ "$START_STEP" -le 4 ]; then
  log "▶ Step 4b: Building county panel dataset..."
  python 04b_build_county_panel.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 4b complete."
fi

# ── Step 4c: Aggregate to CZ panel ───────────────────────
if [ "$START_STEP" -le 4 ]; then
  log "▶ Step 4c: Aggregating county panel -> CZ panel..."
  python 04c_build_cz_panel.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 4c complete."
fi

# ── Step 5: Diagnostics (optional) ───────────────────────
if [ "$START_STEP" -le 5 ]; then
  log "▶ Step 5: Generating diagnostic plots..."
  Rscript 05_diagnostics.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 5 complete."
fi

# ── Step 6: Validate panels ──────────────────────────────
if [ "$START_STEP" -le 6 ]; then
  log "▶ Step 6: Validating panels..."
  python 06_validate_panels.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 6 complete."
fi

log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "  ✓ Pipeline complete!"
log "  Output: clean_data/tx_county_daily_weather.csv (.parquet)"
log "          clean_data/tx_cz_daily_weather.csv (.parquet)"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
