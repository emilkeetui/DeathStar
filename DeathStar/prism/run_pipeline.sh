#!/bin/bash
# run_pipeline.sh
# ============================================================
# Executes the full TX CZ Weather pipeline in sequence.
# Run from the tx_cz_weather/ directory:
#   bash run_pipeline.sh
#
# Optional: pass a starting step to resume mid-pipeline
#   bash run_pipeline.sh 3     # start at step 3
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
log "  Texas CZ Weather Pipeline — $(date)"
log "  Starting from step $START_STEP"
log "  Log file: $LOGFILE"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# ── Step 0: Install dependencies ─────────────────────────
if [ "$START_STEP" -le 0 ]; then
  log "▶ Step 0: Installing dependencies..."
  Rscript environment/packages.R 2>&1 | tee -a "$LOGFILE"
  pip install -q -r environment/requirements.txt 2>&1 | tee -a "$LOGFILE"
  log "✓ Dependencies installed."
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

# ── Step 3: Extract zonal statistics ─────────────────────
if [ "$START_STEP" -le 3 ]; then
  log "▶ Step 3: Extracting zonal weather statistics..."
  log "  (This step may take 2–6 hours.)"
  Rscript 03_extract_cz_weather.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 3 complete."
fi

# ── Step 4: Build final panel ─────────────────────────────
if [ "$START_STEP" -le 4 ]; then
  log "▶ Step 4: Building final panel dataset..."
  python 04_build_panel.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 4 complete."
fi

# ── Step 5: Diagnostics (optional) ───────────────────────
if [ "$START_STEP" -le 5 ]; then
  log "▶ Step 5: Generating diagnostic plots..."
  Rscript 05_diagnostics.R 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 5 complete."
fi

log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "  ✓ Pipeline complete!"
log "  Output: output/tx_cz_daily_weather.csv"
log "          output/tx_cz_daily_weather.parquet"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
