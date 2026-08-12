# ============================================================
# Script: 00_download_tdmean_gap.R
# Purpose: Close the tdmean download gap (2024-12-05 -> 2026-03-17).
#          Scans prism_raw/ for existing tdmean folders and requests
#          only the missing dates -- never re-downloads the 5,438
#          days already present.
# Inputs:  prism_raw/ (existing tdmean folders), config.yaml
# Outputs: prism_raw/*tdmean*/  (new BIL folders, in place)
# Author: EK  Date: 2026-08-07
#
# Runtime: multi-hour network job. Run in the background.
# ============================================================
.libPaths("Z:/ek559/RPackages")
suppressPackageStartupMessages({
  library(prism)
  library(lubridate)
  library(yaml)
  library(glue)
  library(fs)
  library(dplyr)
})

# ── 0. Load configuration ─────────────────────────────────
cfg       <- yaml::read_yaml("Z:/ek559/DeathStar/DeathStar/prism/config.yaml")
prism_dir <- cfg$prism$data_dir
var       <- "tdmean"
start_dt  <- as.Date(cfg$prism$start_date)
end_dt    <- as.Date(cfg$prism$end_date)

cat(glue("
╔══════════════════════════════════════════════╗
║  Close tdmean gap — Texas County/CZ Weather  ║
╚══════════════════════════════════════════════╝
  Variable  : {var}
  Date range: {start_dt} → {end_dt}
  Output dir: {prism_dir}
\n"))

prism_set_dl_dir(prism_dir)

# ── 1. Scan for already-downloaded tdmean dates ────────────
# Same matching logic as 01_download_prism.R::already_downloaded()
already_downloaded <- function(var) {
  all_dirs <- fs::dir_ls(prism_dir, type = "directory")
  names    <- basename(all_dirs)

  pattern  <- paste0("(?i)(PRISM|prism)_", var, "_.+_(\\d{8})")
  matched  <- names[grepl(pattern, names, perl = TRUE)]

  if (length(matched) == 0) return(as.Date(character()))

  date_str <- regmatches(matched, regexpr("\\d{8}", matched))
  as.Date(date_str, format = "%Y%m%d")
}

# ── 2. Determine missing dates ──────────────────────────────
all_dates    <- seq(start_dt, end_dt, by = "day")
done_dates   <- already_downloaded(var)
missing_dates <- setdiff(as.character(all_dates), as.character(done_dates))

cat(glue("  [{var}] {length(done_dates)} already present; {length(missing_dates)} to download.\n\n"))

if (length(missing_dates) == 0) {
  cat(glue("  [{var}] Already complete. Nothing to do.\n"))
  quit(save = "no", status = 0)
}

missing_dt <- sort(as.Date(missing_dates))

# ── 3. Download missing dates in annual chunks ──────────────
# Same tryCatch/retry structure as 01_download_prism.R::download_variable()
download_variable <- function(var, start, end) {
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
        keepZip  = FALSE
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

# Find contiguous ranges of missing dates to minimize API calls
gaps   <- c(0, diff(missing_dt) > 1)  # TRUE = new contiguous block
blocks <- split(missing_dt, cumsum(gaps))

cat(glue("  [{var}] {length(blocks)} contiguous block(s) to download.\n\n"))

for (blk in blocks) {
  download_variable(var, min(blk), max(blk))
}

# ── 4. Final inventory ─────────────────────────────────────
cat("\n═══ Download Summary ═══\n")
done_dates_final <- already_downloaded(var)
expected <- as.integer(end_dt - start_dt) + 1
pct <- round(100 * length(done_dates_final) / expected, 1)
cat(glue("  {var}: {length(done_dates_final)}/{expected} days ({pct}%)\n"))

still_missing <- setdiff(as.character(all_dates), as.character(done_dates_final))
if (length(still_missing) > 0) {
  cat(glue("  WARNING: {length(still_missing)} dates still missing after this run.\n"))
  cat("  This is expected if PRISM has not yet published very recent days.\n")
  cat("  Panels will carry NA for tdmean/rh_mean/vpd_mean on these dates (see plan §3.1 fallback).\n")
} else {
  cat(glue("  [{var}] Gap closed. All {expected} days present.\n"))
}

cat("\nDone. Re-run 03b_extract_county_weather.R for tdmean if this completed after extraction.\n")
