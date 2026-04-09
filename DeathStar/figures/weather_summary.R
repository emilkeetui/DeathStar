# ============================================================
# Script: weather_summary.R
# Purpose: Monthly average tmax and tmin across Texas commuting zones
# Inputs:  clean_data/tx_cz_daily_weather.parquet
# Outputs: output/figures/tx_monthly_temp.png
# Author: EK  Date: 2026-04-08
# ============================================================

library(arrow)
library(dplyr)
library(ggplot2)

# ── 1. Load and aggregate ─────────────────────────────────────────────────────

weather <- read_parquet("C:/Users/emilk/deathstar/clean_data/tx_cz_daily_weather.parquet")

# Step 1: monthly mean within each CZ (°C → °F)
cz_monthly <- weather %>%
  mutate(date = as.Date(date)) %>%
  group_by(cz_id, year, month) %>%
  summarise(
    tmax_f = mean(tmax, na.rm = TRUE) * 9/5 + 32,
    tmin_f = mean(tmin, na.rm = TRUE) * 9/5 + 32,
    .groups = "drop"
  )

# Step 2: statewide mean across CZs for each month-year
monthly_avg <- cz_monthly %>%
  group_by(year, month) %>%
  summarise(
    tmax_mean = mean(tmax_f, na.rm = TRUE),
    tmin_mean = mean(tmin_f, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

# ── 2. Plot ───────────────────────────────────────────────────────────────────

ggplot(monthly_avg, aes(x = date)) +
  geom_line(aes(y = tmax_mean, colour = "Maximum temperature"), linewidth = 0.7) +
  geom_line(aes(y = tmin_mean, colour = "Minimum temperature"), linewidth = 0.7) +
  scale_colour_manual(
    values = c("Maximum temperature" = "#d73027", "Minimum temperature" = "#4575b4")
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "\u00b0F")) +
  labs(
    title   = "Monthly Average Temperature in Texas, 2010\u20132026",
    x       = NULL,
    y       = "Temperature (\u00b0F)",
    colour  = NULL,
    caption = paste(
      "Notes: Monthly mean maximum and minimum daily temperatures averaged across Texas commuting zones, January 2010\u2013March 2026.",
      "Each month's value is the average across commuting zones of that zone's monthly mean temperature.",
      "Commuting zone temperature data are derived from PRISM Climate Group daily 4-km gridded estimates,",
      "aggregated to commuting zones using area-weighted averages.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position       = "top",
    legend.key.width      = unit(1.5, "cm"),
    panel.grid.minor      = element_blank(),
    plot.caption          = element_text(hjust = 0, size = 8, colour = "grey40"),
    plot.caption.position = "plot"
  )

ggsave("C:/Users/emilk/deathstar/output/figures/tx_monthly_temp.png",
       width = 8, height = 4.5, dpi = 300)

cat("Saved: output/figures/tx_monthly_temp.png\n")
