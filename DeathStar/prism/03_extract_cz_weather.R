# ============================================================
# Script: 03_extract_cz_weather.R
# Purpose: Extract area-weighted mean weather values from PRISM
#          daily rasters for each Texas commuting zone polygon.
# Inputs:  prism_raw/ (BIL rasters), data/cz_shapefiles/tx_commuting_zones_2020.gpkg
# Outputs: data/extracted/{var}_{year}.csv
# Author: EK  Date: 2026-04-05
# ============================================================
#
# Strategy: stack a full year of rasters per variable into a
# SpatRaster, crop once to Texas, then call exact_extract once.
# This amortizes per-call overhead (CRS transform, spatial index
# build) across all 365 days, making it ~100x faster than
# extracting one raster at a time.
#
# Memory: ~120 MB per variable-year (TX-cropped float32 stack).
# ============================================================

.libPaths("Z:/ek559/RPackages")
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(lubridate)
  library(yaml)
  library(glue)
  library(purrr)
  library(fs)
  library(readr)
  library(tidyr)
})

# ── 0. Configuration ───────────────────────────────────────
cfg           <- yaml::read_yaml("Z:/ek559/DeathStar/DeathStar/prism/config.yaml")
PRISM_DIR     <- cfg$prism$data_dir
VARIABLES     <- cfg$prism$variables
START_DATE    <- as.Date(cfg$prism$start_date)
END_DATE      <- as.Date(cfg$prism$end_date)
SHP_PATH      <- file.path(cfg$commuting_zones$shp_dir,
                           "tx_commuting_zones_2020.gpkg")
EXTRACTED_DIR <- cfg$output$extracted_dir

fs::dir_create(EXTRACTED_DIR, recurse = TRUE)

cat(glue("
╔══════════════════════════════════════════════════╗
║  Step 3: Zonal Extraction — PRISM -> CZ Polygons ║
╚══════════════════════════════════════════════════╝
  Variables : {paste(VARIABLES, collapse=', ')}
  Date range: {START_DATE} -> {END_DATE}
  PRISM dir : {PRISM_DIR}
  Shapefile : {SHP_PATH}
  Output dir: {EXTRACTED_DIR}
\n"))

# ── 1. Load Texas CZ shapefile ─────────────────────────────
if (!file.exists(SHP_PATH)) {
  stop(glue("Shapefile not found: {SHP_PATH}\nRun 02_get_commuting_zones.py first."))
}

cz_sf <- sf::st_read(SHP_PATH, quiet = TRUE) |> sf::st_transform(4326)
cat(glue("  Loaded {nrow(cz_sf)} Texas commuting zones.\n\n"))

# Pre-compute TX bounding box for cropping (used every year)
tx_bbox <- sf::st_bbox(cz_sf)
tx_ext  <- terra::ext(tx_bbox[["xmin"]], tx_bbox[["xmax"]],
                      tx_bbox[["ymin"]], tx_bbox[["ymax"]])

# ── 2. Scan prism_raw for available raster files ───────────
# Handles two naming conventions:
#   Pre-downloaded: prism_{var}_us_25m_{YYYYMMDD}/
#   prism-package:  PRISM_{var}_stable_4kmD2_{YYYYMMDD}_bil/
get_prism_files <- function(var) {
  pattern  <- paste0("(?i)(PRISM|prism)_", var, "_.+_(\\d{8})")
  all_dirs <- fs::dir_ls(PRISM_DIR, type = "directory")
  matched  <- all_dirs[grepl(pattern, basename(all_dirs), perl = TRUE)]

  if (length(matched) == 0) {
    warning(glue("No folders found for variable '{var}' in {PRISM_DIR}"))
    return(tibble(date = as.Date(character()), bil_path = character()))
  }

  folder_names <- basename(matched)
  date_str     <- regmatches(folder_names, regexpr("\\d{8}", folder_names))
  dates        <- as.Date(date_str, format = "%Y%m%d")
  bil_paths    <- file.path(matched, paste0(folder_names, ".bil"))

  tibble(date = dates, bil_path = bil_paths) |>
    filter(date >= START_DATE, date <= END_DATE, file.exists(bil_path)) |>
    arrange(date)
}

# ── 3. Extract one variable × one year (stacked) ──────────
# Loads all daily BIL files for the year as a SpatRaster stack,
# crops once to TX, then calls exact_extract once for all days.
# Returns a tidy data frame: cz_id | date | <var>
extract_year <- function(var, yr, file_tbl, cz_nad83, tx_ext, out_dir) {
  out_file <- file.path(out_dir, glue("{var}_{yr}.csv"))

  if (file.exists(out_file)) {
    cat(glue("    [{var}] {yr}: already done. Skipping.\n"))
    return(invisible(NULL))
  }

  yr_files <- file_tbl |> filter(year(date) == yr)

  if (nrow(yr_files) == 0) {
    cat(glue("    [{var}] {yr}: no files. Skipping.\n"))
    return(invisible(NULL))
  }

  cat(glue("    [{var}] {yr}: stacking {nrow(yr_files)} rasters..."))

  tryCatch({
    # Stack all daily rasters for this year into a single SpatRaster
    # terra reads BIL files lazily — minimal memory until crop/extract
    r_stack <- terra::rast(yr_files$bil_path)

    # Name layers as date strings — exact_extract uses layer names as column
    # names, so this lets us recover the date directly after pivot_longer
    names(r_stack) <- as.character(yr_files$date)

    # Crop entire stack to Texas in one pass
    r_tx <- terra::crop(r_stack, tx_ext)

    # exact_extract returns a data frame: nrow = n_CZ, cols = mean.<date>
    vals <- exactextractr::exact_extract(
      x        = r_tx,
      y        = cz_nad83,
      fun      = "mean",
      progress = FALSE
    )

    # Reshape to long: cz_id | date | <var>
    result <- vals |>
      as.data.frame() |>
      mutate(cz_id = cz_nad83$cz_id) |>
      tidyr::pivot_longer(
        cols      = -cz_id,
        names_to  = "date",
        values_to = var
      ) |>
      mutate(date = as.Date(sub("^mean\\.", "", date))) |>
      select(cz_id, date, !!var) |>
      arrange(cz_id, date)

    readr::write_csv(result, out_file)
    cat(glue(" {nrow(result)} rows -> {basename(out_file)}\n"))
    invisible(result)

  }, error = function(e) {
    cat(glue(" ERROR: {conditionMessage(e)}\n"))
  })
}

# ── 4. Pre-transform CZ polygons to raster CRS (once) ─────
# All PRISM rasters use NAD83 geographic CRS. Transform CZ
# polygons once here rather than inside the extraction loop.
cat("  Pre-transforming CZ polygons to NAD83...\n")
# Read CRS from any raster file
sample_bil <- fs::dir_ls(PRISM_DIR, type = "directory")[1] |>
  (\(d) file.path(d, paste0(basename(d), ".bil")))()
r_sample   <- terra::rast(sample_bil)
raster_crs <- terra::crs(r_sample)
cz_nad83   <- sf::st_transform(cz_sf, crs = raster_crs)
rm(r_sample)

# ── 5. Main extraction loop ────────────────────────────────
years <- seq(year(START_DATE), year(END_DATE))

for (var in VARIABLES) {
  cat(glue("\n>> Variable: {var}\n"))

  file_tbl <- get_prism_files(var)
  cat(glue("   Found {nrow(file_tbl)} raster files\n"))

  if (nrow(file_tbl) == 0) next

  for (yr in years) {
    extract_year(var, yr, file_tbl, cz_nad83, tx_ext, EXTRACTED_DIR)
  }
}

# ── 6. Summary ─────────────────────────────────────────────
cat("\n=== Extraction Summary ===\n")
all_csv <- fs::dir_ls(EXTRACTED_DIR, glob = "*.csv")
for (var in VARIABLES) {
  var_csvs <- all_csv[grepl(glue("/{var}_[0-9]{{4}}\\.csv$"), all_csv)]
  cat(glue("  {var}: {length(var_csvs)}/{length(years)} years complete\n"))
}
cat("\nDone. Proceed to 04_build_panel.py\n")
