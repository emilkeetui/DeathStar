# 01_download_prism.R
# ============================================================
# Step 1: Download PRISM daily 4km rasters for all variables
# covering the configured date range.
#
# The `prism` package wraps the PRISM REST API and manages
# local file storage. Files are saved as BIL (band interleaved)
# rasters, one zip/folder per variable-day combination.
#
# Runtime: 4–12 hours for 7 variables × 16 years.
# Tip: Run in a screen/tmux session.
# ============================================================
.libPaths("Z:/ek559/RPackages")
install.packages('prism')
install.packages('lubridate')
install.packages('yaml')
install.packages('glue')
install.packages('fs')
install.packages('dplyr')

library(prism)
library(lubridate)
library(yaml)
library(glue)
library(fs)
library(dplyr)

# ── 0. Load configuration ─────────────────────────────────
cfg       <- yaml::read_yaml("Z:/ek559/DeathStar/DeathStar/prism/config.yaml")
prism_dir <- cfg$prism$data_dir
variables <- cfg$prism$variables
start_dt  <- as.Date(cfg$prism$start_date)
end_dt    <- as.Date(cfg$prism$end_date)

cat(glue("
╔══════════════════════════════════════════════╗
║  PRISM Daily Download — Texas CZ Weather     ║
╚══════════════════════════════════════════════╝
  Variables : {paste(variables, collapse=', ')}
  Date range: {start_dt} → {end_dt}
  Output dir: {prism_dir}
\n"))

# ── 1. Set PRISM local directory ───────────────────────────
fs::dir_create(prism_dir, recurse = TRUE)
prism_set_dl_dir(prism_dir)

# ── 2. Helper: download one variable for a date range ─────
# The prism package downloads in ~1-year chunks to avoid
# timeout issues with very large requests.
download_variable <- function(var, start, end) {

  # Split into annual chunks to be safe with API limits
  years <- seq(year(start), year(end))

  for (yr in years) {
    yr_start <- max(start, as.Date(glue("{yr}-01-01")))
    yr_end   <- min(end,   as.Date(glue("{yr}-12-31")))

    cat(glue("  [{var}] Downloading {yr_start} → {yr_end} ...\n"))

    tryCatch({
      get_prism_dailys(
        type     = var,
        minDate  = format(yr_start, "%Y-%m-%d"),
        maxDate  = format(yr_end,   "%Y-%m-%d"),
        keepZip  = FALSE   # delete zip after extraction to save space
      )
      cat(glue("  [{var}] Year {yr}: OK\n"))
    }, error = function(e) {
      cat(glue("  [{var}] Year {yr}: ERROR — {conditionMessage(e)}\n"))
      cat("  Retrying in 30 seconds...\n")
      Sys.sleep(30)
      tryCatch(
        get_prism_dailys(
          type    = var,
          minDate = format(yr_start, "%Y-%m-%d"),
          maxDate = format(yr_end,   "%Y-%m-%d"),
          keepZip = FALSE
        ),
        error = function(e2) {
          cat(glue("  [{var}] Year {yr}: FAILED after retry. Skipping.\n"))
        }
      )
    })

    Sys.sleep(2)  # be polite to the PRISM server
  }
}

# ── 3. Verify what's already downloaded (resume support) ──
# Scans prism_dir for both naming conventions:
#   - Downloaded by this script (prism pkg): PRISM_{var}_stable_4kmD2_{YYYYMMDD}_bil/
#   - Pre-existing data:                     prism_{var}_us_25m_{YYYYMMDD}/
already_downloaded <- function(var) {
  all_dirs <- fs::dir_ls(prism_dir, type = "directory")
  names    <- basename(all_dirs)

  # Match either naming convention — both contain an 8-digit date
  pattern  <- paste0("(?i)(PRISM|prism)_", var, "_.+_(\\d{8})")
  matched  <- names[grepl(pattern, names, perl = TRUE)]

  if (length(matched) == 0) return(as.Date(character()))

  date_str <- regmatches(matched, regexpr("\\d{8}", matched))
  as.Date(date_str, format = "%Y%m%d")
}

# ── 4. Download all variables ──────────────────────────────
all_dates <- seq(start_dt, end_dt, by = "day")

for (var in variables) {
  cat(glue("\n▶ Processing variable: {var}\n"))

  done_dates    <- already_downloaded(var)
  missing_dates <- setdiff(as.character(all_dates), as.character(done_dates))

  if (length(missing_dates) == 0) {
    cat(glue("  [{var}] Already complete ({length(done_dates)} days). Skipping.\n"))
    next
  }

  cat(glue("  [{var}] {length(done_dates)} already present; {length(missing_dates)} to download.\n"))

  # Find contiguous ranges of missing dates to minimize API calls
  missing_dt <- sort(as.Date(missing_dates))
  gaps <- c(0, diff(missing_dt) > 1)  # TRUE = new contiguous block
  blocks <- split(missing_dt, cumsum(gaps))

  for (blk in blocks) {
    download_variable(var, min(blk), max(blk))
  }
}

# ── 5. Final inventory ─────────────────────────────────────
cat("\n═══ Download Summary ═══\n")
pd_archive <- prism_archive_ls()

for (var in variables) {
  n <- sum(grepl(paste0("_", var, "_"), pd_archive))
  expected <- as.integer(end_dt - start_dt) + 1
  pct <- round(100 * n / expected, 1)
  cat(glue("  {var}: {n}/{expected} days ({pct}%)\n"))
}

cat("\nDone. Proceed to step 02_get_commuting_zones.py\n")
