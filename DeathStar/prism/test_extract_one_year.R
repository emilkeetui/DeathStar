# Quick test: extract tmean for 2023 only and validate output
.libPaths("Z:/ek559/RPackages")
suppressPackageStartupMessages({
  library(terra); library(sf); library(exactextractr)
  library(dplyr); library(lubridate); library(yaml)
  library(glue); library(fs); library(readr); library(tidyr)
})

cfg           <- yaml::read_yaml("Z:/ek559/DeathStar/DeathStar/prism/config.yaml")
PRISM_DIR     <- cfg$prism$data_dir
SHP_PATH      <- file.path(cfg$commuting_zones$shp_dir, "tx_commuting_zones_2020.gpkg")
EXTRACTED_DIR <- cfg$output$extracted_dir
fs::dir_create(EXTRACTED_DIR, recurse = TRUE)

VAR <- "tmean"
YR  <- 2023

cz_sf   <- sf::st_read(SHP_PATH, quiet = TRUE) |> sf::st_transform(4326)
tx_bbox <- sf::st_bbox(cz_sf)
tx_ext  <- terra::ext(tx_bbox[["xmin"]], tx_bbox[["xmax"]],
                      tx_bbox[["ymin"]], tx_bbox[["ymax"]])

# Get raster files
all_dirs <- fs::dir_ls(PRISM_DIR, type = "directory")
matched  <- all_dirs[grepl(paste0("(?i)(PRISM|prism)_", VAR, "_.+_(\\d{8})"),
                           basename(all_dirs), perl = TRUE)]
dates    <- as.Date(regmatches(basename(matched), regexpr("\\d{8}", basename(matched))), "%Y%m%d")
mask     <- year(dates) == YR
matched  <- matched[mask]; dates <- dates[mask]
bil      <- file.path(matched, paste0(basename(matched), ".bil"))
ok       <- file.exists(bil)
bil      <- bil[ok]; dates <- dates[ok]

cat(glue("Files found for {VAR} {YR}: {length(bil)}\n"))

# Pre-transform CZ to raster CRS
cz_nad83 <- sf::st_transform(cz_sf, crs = terra::crs(terra::rast(bil[1])))

# Stack, name layers as dates, crop, extract
r_stack        <- terra::rast(bil)
names(r_stack) <- as.character(dates)
r_tx           <- terra::crop(r_stack, tx_ext)

cat("Running exact_extract...\n")
vals <- exactextractr::exact_extract(r_tx, cz_nad83, fun = "mean", progress = TRUE)

result <- vals |>
  as.data.frame() |>
  mutate(cz_id = cz_nad83$cz_id) |>
  tidyr::pivot_longer(cols = -cz_id, names_to = "date", values_to = VAR) |>
  mutate(date = as.Date(sub("^mean\\.", "", date))) |>
  select(cz_id, date, !!VAR) |>
  arrange(cz_id, date)

out_file <- file.path(EXTRACTED_DIR, glue("{VAR}_{YR}_test.csv"))
readr::write_csv(result, out_file)

# ── Validation ────────────────────────────────────────────
cat("\n=== Validation ===\n")
cat(glue("Rows          : {nrow(result)}\n"))
cat(glue("Expected rows : {length(bil) * nrow(cz_nad83)}  ({length(bil)} days x {nrow(cz_nad83)} CZs)\n"))
cat(glue("NA dates      : {sum(is.na(result$date))}\n"))
cat(glue("NA values     : {sum(is.na(result[[VAR]]))}\n"))
cat(glue("Date range    : {min(result$date)} -> {max(result$date)}\n"))
cat(glue("Unique CZs    : {length(unique(result$cz_id))}\n"))
cat(glue("Value range   : {round(min(result[[VAR]],na.rm=TRUE),1)} to {round(max(result[[VAR]],na.rm=TRUE),1)}\n"))
cat("\nFirst 5 rows:\n")
print(head(result, 5))
cat(glue("\nSaved to: {out_file}\n"))
