# 05_diagnostics.R
# ============================================================
# Optional Step 5: Diagnostics and visualization of the final
# panel dataset. Run after 04_build_panel.py.
#
# Produces:
#   - Missing data heatmap by CZ and year
#   - Seasonal precipitation distribution for Texas CZs
#   - Spatial map of mean annual temperature by CZ
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(lubridate)
  library(sf)
  library(yaml)
  library(glue)
  library(scales)
  library(arrow)   # Parquet support
  library(fs)
})

cfg        <- yaml::read_yaml("config.yaml")
PARQUET    <- cfg$output$panel_parquet
SHP_PATH   <- file.path(cfg$commuting_zones$shp_dir,
                        "tx_commuting_zones_2010.gpkg")
DIAG_DIR   <- "output/diagnostics"
fs::dir_create(DIAG_DIR)

cat("Loading panel dataset...\n")
panel <- arrow::read_parquet(PARQUET)
cat(glue("  Loaded {nrow(panel):,} rows × {ncol(panel)} columns\n\n"))

# ── 1. Missing data heatmap ────────────────────────────────
cat("Plot 1: Missing data by variable and year...\n")

missing_summary <- panel |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarise(
    across(
      c(ppt, tmax, tmin, tmean, tdmean, vpdmin, vpdmax),
      ~ mean(is.na(.)) * 100,
      .names = "{.col}_pct_missing"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = ends_with("_pct_missing"),
    names_to = "variable",
    values_to = "pct_missing"
  ) |>
  mutate(variable = gsub("_pct_missing", "", variable))

p1 <- ggplot(missing_summary, aes(x = year, y = variable, fill = pct_missing)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#2ecc71", high = "#e74c3c",
    name = "% Missing",
    limits = c(0, 100)
  ) +
  scale_x_continuous(breaks = seq(2010, 2026, 2)) +
  labs(
    title = "PRISM Data Completeness — Texas Commuting Zones",
    subtitle = "Percent of CZ-day observations missing by variable and year",
    x = "Year", y = "Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(DIAG_DIR, "01_missing_data_heatmap.png"),
       p1, width = 10, height = 5, dpi = 150)
cat("  Saved: 01_missing_data_heatmap.png\n")

# ── 2. Seasonal precipitation by CZ group ─────────────────
cat("Plot 2: Seasonal precipitation distribution...\n")

# Classify CZs into West / Central / East Texas by longitude
spatial_groups <- panel |>
  distinct(cz_id, centroid_lon) |>
  mutate(
    region = case_when(
      centroid_lon < -101 ~ "West Texas",
      centroid_lon < -97  ~ "Central Texas",
      TRUE                ~ "East Texas"
    )
  )

seasonal_ppt <- panel |>
  left_join(spatial_groups, by = "cz_id") |>
  filter(!is.na(ppt), !is.na(region)) |>
  group_by(region, month) |>
  summarise(
    mean_ppt  = mean(ppt, na.rm = TRUE),
    se_ppt    = sd(ppt, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p2 <- ggplot(seasonal_ppt,
             aes(x = month, y = mean_ppt, color = region, fill = region)) +
  geom_ribbon(
    aes(ymin = mean_ppt - 1.96 * se_ppt,
        ymax = mean_ppt + 1.96 * se_ppt),
    alpha = 0.15, color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_color_manual(values = c("#e67e22", "#2980b9", "#27ae60")) +
  scale_fill_manual(values = c("#e67e22", "#2980b9", "#27ae60")) +
  labs(
    title  = "Mean Daily Precipitation by Season and Region",
    subtitle = "Texas Commuting Zones, 2010–2025 (±95% CI)",
    x = "Month", y = "Mean Daily Precipitation (mm)",
    color = "Region", fill = "Region"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave(file.path(DIAG_DIR, "02_seasonal_precip.png"),
       p2, width = 10, height = 6, dpi = 150)
cat("  Saved: 02_seasonal_precip.png\n")

# ── 3. Annual mean temperature map ────────────────────────
cat("Plot 3: Spatial map of mean annual temperature...\n")

if (file.exists(SHP_PATH)) {
  cz_sf <- sf::st_read(SHP_PATH, quiet = TRUE)

  annual_tmean <- panel |>
    filter(year >= 2010, year <= 2024) |>   # full years only
    group_by(cz_id) |>
    summarise(
      mean_tmean = mean(tmean, na.rm = TRUE),
      mean_tmax  = mean(tmax,  na.rm = TRUE),
      mean_ppt   = mean(ppt,   na.rm = TRUE) * 365,  # annual total
      .groups = "drop"
    )

  cz_plot <- cz_sf |>
    left_join(annual_tmean, by = "cz_id")

  p3 <- ggplot(cz_plot) +
    geom_sf(aes(fill = mean_tmean), color = "white", linewidth = 0.3) +
    scale_fill_distiller(
      palette   = "RdYlBu",
      direction = -1,
      name      = "°C",
      na.value  = "grey80"
    ) +
    labs(
      title    = "Mean Daily Temperature by Commuting Zone",
      subtitle = "Texas, 2010–2024 (PRISM 4km, area-weighted)",
      caption  = "Source: PRISM Climate Group / USDA ERS"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "grey40"),
      legend.position = "right"
    )

  ggsave(file.path(DIAG_DIR, "03_mean_temp_map.png"),
         p3, width = 10, height = 7, dpi = 150)
  cat("  Saved: 03_mean_temp_map.png\n")
} else {
  cat("  Shapefile not found — skipping map.\n")
}

# ── 4. Quick summary table ─────────────────────────────────
cat("\n═══ Summary Statistics ═══\n")
panel |>
  summarise(
    across(
      c(ppt, tmax, tmin, tmean, tdmean, vpdmin, vpdmax),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd   = ~sd(., na.rm = TRUE),
        min  = ~min(., na.rm = TRUE),
        max  = ~max(., na.rm = TRUE),
        pct_valid = ~mean(!is.na(.)) * 100
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  tidyr::pivot_longer(everything()) |>
  tidyr::separate(name, into = c("variable", "stat"), sep = "_(?=[^_]+$)") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  mutate(across(where(is.numeric), ~round(., 3))) |>
  print(n = Inf)

cat(glue("\n✓ Diagnostics complete. Plots saved in {DIAG_DIR}/\n"))
