#!/bin/bash
# run_zcta_pipeline.sh
# ============================================================
# Executes the Texas ZCTA x day weather pipeline (PRISM 800m) in
# sequence. Run from the prism/ directory:
#   bash run_zcta_pipeline.sh
#
# Optional: pass a starting step to resume mid-pipeline
#   bash run_zcta_pipeline.sh 12     # start at step 12
#
# Step numbering matches PLAN_zcta_panel.md §8. Step 11 must run
# before step 12 but is independent of step 10 — run 11 first and
# confirm the ~1,989 ZCTA count before committing to the ~26-hour
# step 10 download (plan §9.4: report cost estimate and get
# approval before launching step 10).
# ============================================================

set -euo pipefail

START_STEP=${1:-11}
LOGDIR="logs"
mkdir -p "$LOGDIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOGFILE="$LOGDIR/zcta_pipeline_${TIMESTAMP}.log"

PY="Z:/ek559/nys_algal_bloom/NYS algal bloom/code2/Scripts/python.exe"

log() {
  echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" | tee -a "$LOGFILE"
}

log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "  Texas ZCTA Weather Pipeline (PRISM 800m) — $(date)"
log "  Starting from step $START_STEP"
log "  Log file: $LOGFILE"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# ── Step 11: Build ZCTA shapefiles ───────────────────────
if [ "$START_STEP" -le 11 ]; then
  log "▶ Step 11: Building Texas ZCTA shapefiles (two eras)..."
  "$PY" 11_get_zctas.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 11 complete."
fi

# ── Step 10: Download + clip PRISM 800m rasters ──────────
if [ "$START_STEP" -le 10 ] || [ "$START_STEP" -eq 10 ]; then
  log "▶ Step 10: Downloading + clipping PRISM 800m rasters to Texas..."
  log "  (~26h serial / ~13-15h at concurrency 2. Resumable — safe to interrupt."
  log "   Per plan §9.4, this must be cost-approved before running standalone.)"
  "$PY" 10_download_clip_prism800.py --concurrency 2 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 10 complete."
fi

# ── Step 12: Zonal extraction ────────────────────────────
if [ "$START_STEP" -le 12 ]; then
  log "▶ Step 12: Extracting zonal weather statistics (ZCTA, month-stacked)..."
  log "  (~2h at 4 parallel workers.)"
  "$PY" 12_extract_zcta_weather.py --workers 4 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 12 complete."
fi

# ── Step 13: Build ZCTA panel ─────────────────────────────
if [ "$START_STEP" -le 13 ]; then
  log "▶ Step 13: Assembling ZCTA panel dataset..."
  "$PY" 13_build_zcta_panel.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 13 complete."
fi

# ── Step 14: Validate ─────────────────────────────────────
if [ "$START_STEP" -le 14 ]; then
  log "▶ Step 14: Validating ZCTA panel..."
  "$PY" 14_validate_zcta_panel.py 2>&1 | tee -a "$LOGFILE"
  log "✓ Step 14 complete."
fi

log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
log "  ✓ ZCTA pipeline complete!"
log "  Output: clean_data/tx_zcta_daily_weather.csv (.parquet)"
log "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
