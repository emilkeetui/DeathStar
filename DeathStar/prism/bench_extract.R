.libPaths("Z:/ek559/RPackages")
suppressPackageStartupMessages({
  library(terra); library(sf); library(exactextractr)
  library(dplyr); library(lubridate); library(fs); library(glue); library(tidyr)
})

PRISM_DIR <- "Z:/ek559/DeathStar/prism_raw"
SHP_PATH  <- "Z:/ek559/DeathStar/data/cz_shapefiles/tx_commuting_zones_2020.gpkg"

cz_sf <- sf::st_read(SHP_PATH, quiet = TRUE) |> sf::st_transform(4326)

# TX bounding box
tx_bbox <- sf::st_bbox(cz_sf)
tx_ext  <- terra::ext(tx_bbox[["xmin"]], tx_bbox[["xmax"]],
                      tx_bbox[["ymin"]], tx_bbox[["ymax"]])

# Pre-transform CZ to NAD83 (raster CRS) once
sample_dir <- fs::dir_ls(PRISM_DIR, type = "directory")[1]
sample_bil <- file.path(sample_dir, paste0(basename(sample_dir), ".bil"))
cz_nad83   <- sf::st_transform(cz_sf, crs = terra::crs(terra::rast(sample_bil)))
cat(glue("CZs: {nrow(cz_nad83)}\n"))

# Get tmean files for 2 full years (2022-2023) to benchmark the stacked approach
all_dirs  <- fs::dir_ls(PRISM_DIR, type = "directory")
matched   <- all_dirs[grepl("(?i)(PRISM|prism)_tmean_.+_(\\d{8})", basename(all_dirs), perl = TRUE)]
dates     <- as.Date(regmatches(basename(matched), regexpr("\\d{8}", basename(matched))), "%Y%m%d")

for (test_yr in c(2023)) {
  yr_mask   <- year(dates) == test_yr
  yr_dirs   <- matched[yr_mask]
  yr_dates  <- dates[yr_mask]
  bil_paths <- file.path(yr_dirs, paste0(basename(yr_dirs), ".bil"))
  bil_paths <- bil_paths[file.exists(bil_paths)]
  yr_dates  <- yr_dates[file.exists(file.path(yr_dirs, paste0(basename(yr_dirs), ".bil")))]

  cat(glue("\nBenchmarking stacked extraction: tmean {test_yr} ({length(bil_paths)} days)\n"))

  t0 <- proc.time()
  r_stack <- terra::rast(bil_paths)
  r_tx    <- terra::crop(r_stack, tx_ext)

  vals <- exactextractr::exact_extract(
    x = r_tx, y = cz_nad83, fun = "mean", progress = TRUE
  )

  elapsed <- (proc.time() - t0)[["elapsed"]]
  cat(glue("  Elapsed for {test_yr}: {round(elapsed, 1)}s ({round(elapsed/length(bil_paths), 2)}s/day)\n"))
}

# Extrapolate to full job
# Count total rasters across all 5 variables
vars <- c("ppt", "tmax", "tmin", "tmean", "tdmean")
total_rasters <- 0L
for (v in vars) {
  m  <- all_dirs[grepl(paste0("(?i)(PRISM|prism)_", v, "_.+_(\\d{8})"), basename(all_dirs), perl = TRUE)]
  bp <- file.path(m, paste0(basename(m), ".bil"))
  total_rasters <- total_rasters + sum(file.exists(bp))
}

# Use benchmark result (use 2023 tmean as reference)
yr_matched <- matched[year(dates) == 2023]
yr_bp      <- file.path(yr_matched, paste0(basename(yr_matched), ".bil"))
n_days_ref <- sum(file.exists(yr_bp))

# Re-read elapsed from benchmark
cat(glue("
--- Extrapolation ---
Total rasters    : {total_rasters} ({round(total_rasters/5)} days x 5 vars)
(Run time extrapolation printed above per benchmark)
"))
