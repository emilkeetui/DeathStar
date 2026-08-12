# ============================================================
# Script: heatsummary.R
# Purpose: Summary statistics for heat-related workers' comp claims
# Inputs:  raw_data/icd9 and icd10/icd9dx2015.csv
#          raw_data/icd9 and icd10/FY24-CMS-1785-F-Code-Descriptions/icd10cm_codes_2024.txt
#          raw_data/commuting-zones-2020.csv
#          intermediate_data/claimsclean.parquet
# Outputs: output/tables/heat_claims_summary.tex
# Author: EK  Date: 2026-04-11
# ============================================================

# install.packages(c('arrow', 'dplyr', 'readr', 'stringr', 'tidyr', 'tinytable'))

library(arrow)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tinytable)

# ── 0. Paths ──────────────────────────────────────────────────────────────────

z_root     <- "z:/ek559/DeathStar"

icd9_path  <- file.path(z_root, "raw_data/icd9 and icd10/icd9dx2015.csv")
icd10_path <- file.path(z_root, "raw_data/icd9 and icd10/FY24-CMS-1785-F-Code-Descriptions/icd10cm_codes_2024.txt")
cz_path    <- file.path(z_root, "raw_data/commuting-zones-2020.csv")
claims_in  <- file.path(z_root, "intermediate_data/claimsclean.parquet")
table_out  <- file.path(z_root, "output/tables/heat_claims_summary.tex")

# ── 1. Build ICD dictionaries and identify heat-related codes ─────────────────

icd9 <- read_csv(icd9_path, show_col_types = FALSE) %>%
  select(code = dgns_cd, description = longdesc) %>%
  mutate(code = as.character(code))

icd10 <- read_fwf(
  icd10_path,
  fwf_cols(code = c(1, 7), description = c(9, NA)),
  col_types = "cc"
) %>%
  mutate(code = trimws(code))

icd_lookup <- bind_rows(icd9, icd10)

# Search terms: heat illness family + sunburn + dehydration.
# Simple substring matching is used throughout. Unwanted matches
# (tendon sheath codes, theater place-of-occurrence, appliance contact codes,
# anesthesia-related hyperthermia, newborn codes) are removed via exclusion.
heat_pattern <- paste(
  "heat",            # heat illness, prickly heat, effects of heat, heatstroke, etc.
  "sunstroke",
  "sun stroke",
  "hyperthermia",
  "sunburn",
  "sun burn",
  "dehydration",
  "volume depletion",
  sep = "|"
)

excl_pattern <- paste(
  "sheath",          # tendon sheath codes (contain "heat" as substring)
  "theater",         # Y92254 place-of-occurrence (contains "heat" as substring)
  "heating",         # X16 appliance contact codes ("hot heating appliances")
  "anesthesia",      # T883 malignant hyperthermia due to anesthesia
  "newborn",         # P810 environmental hyperthermia of newborn; P741 dehydration of newborn
  sep = "|"
)

heat_codes_df <- icd_lookup %>%
  filter( str_detect(str_to_lower(description), heat_pattern)) %>%
  filter(!str_detect(str_to_lower(description), excl_pattern))

# Print for review — confirm nothing unexpected is included/excluded
cat("\n── Heat-related ICD codes identified ─────────────────────────────────────\n")
print(heat_codes_df, n = Inf)
cat(sprintf("\n%d codes matched\n\n", nrow(heat_codes_df)))

heat_codes_norm <- str_remove_all(heat_codes_df$code, "\\.")

# ── 2. Identify Austin and Dallas CZs from the CZ crosswalk ──────────────────

# Filter Texas CZs whose CZName contains "Austin" or "Dallas".
# claimsclean.parquet already carries CZ2020 from the build step so no
# zip-to-county join is needed here.

cz_xw <- read.csv(cz_path, colClasses = c(FIPStxt = "character"))

target_czs <- cz_xw %>%
  filter(StateName == "Texas",
         grepl("Austin|Dallas", CZName, ignore.case = TRUE)) %>%
  distinct(CZ2020, CZName)

cat("── CZs encompassing Austin and Dallas ────────────────────────────────────\n")
print(target_czs)
cat("\n")

austin_dallas_cz <- unique(target_czs$CZ2020)

# ── 3. Load individual claims ─────────────────────────────────────────────────

claims <- read_parquet(claims_in)

# ICD columns (positions 1–5)
icd_cols <- c(
  "First.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
  "Second.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
  "Third.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
  "Fourth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
  "Fifth.ICD.9CM.or.ICD.10CM.Diagnosis.Code"
)

# ── 4. Flag heat-related claims ───────────────────────────────────────────────

# A claim is heat-related if any of the five ICD positions matches a heat code.
# Normalize both sides by stripping dots (consistent with claimssummary.R join).

claims <- claims %>%
  mutate(across(all_of(icd_cols),
                ~ str_remove_all(str_trim(.), "\\."),
                .names = "{.col}_norm")) %>%
  mutate(is_heat = if_any(ends_with("_norm"),
                          ~ . %in% heat_codes_norm & !is.na(.) & . != ""))

# ── 5. Derive date components needed for seasonal stats ───────────────────────

claims <- claims %>%
  mutate(
    year   = as.integer(format(Bill.Selection.Date, "%Y")),
    month  = as.integer(format(Bill.Selection.Date, "%m")),
    summer = month %in% 6:8   # June, July, August
  )

# ── 6. Compute summary statistics ────────────────────────────────────────────

# 6a. Total claims and heat claims — all claims, whole period
n_total_claims <- nrow(claims)
n_heat_claims  <- sum(claims$is_heat, na.rm = TRUE)

# 6b. Claims from CZs encompassing Austin and Dallas
n_austin_dallas <- claims %>%
  filter(CZ2020 %in% austin_dallas_cz) %>%
  nrow()

# Exclude 2020–2021 from rate/mean statistics to avoid COVID-period distortion
claims_excl_covid <- claims %>%
  filter(!year %in% c(2020, 2021), !is.na(Bill.Selection.Date))

# 6c. Mean claims per day (excl. 2020–2021)
mean_per_day <- claims_excl_covid %>%
  count(Bill.Selection.Date, name = "n_day") %>%
  summarise(mean = mean(n_day)) %>%
  pull(mean)

# 6d. Mean claims per calendar month (excl. 2020–2021)
mean_per_month <- claims_excl_covid %>%
  count(year, month, name = "n_month") %>%
  summarise(mean = mean(n_month)) %>%
  pull(mean)

# 6e. Mean daily claims: summer (Jun–Aug) vs rest of year (excl. 2020–2021)
seasonal_means <- claims_excl_covid %>%
  count(Bill.Selection.Date, summer, name = "n_day") %>%
  group_by(summer) %>%
  summarise(mean_daily = mean(n_day), .groups = "drop")

mean_summer     <- seasonal_means %>% filter(summer)  %>% pull(mean_daily)
mean_non_summer <- seasonal_means %>% filter(!summer) %>% pull(mean_daily)

# ── 7. Assemble and print results ─────────────────────────────────────────────

cat("── Summary statistics ────────────────────────────────────────────────────\n")
cat(sprintf("Total claims (all period):                %s\n",
            formatC(n_total_claims, format = "d", big.mark = ",")))
cat(sprintf("Heat-related claims (any ICD position):   %s  (%.2f%%)\n",
            formatC(n_heat_claims, format = "d", big.mark = ","),
            n_heat_claims / n_total_claims * 100))
cat(sprintf("Claims in Austin + Dallas CZs:            %s  (%.2f%%)\n",
            formatC(n_austin_dallas, format = "d", big.mark = ","),
            n_austin_dallas / n_total_claims * 100))
cat(sprintf("Mean claims per day:                      %.1f\n", mean_per_day))
cat(sprintf("Mean claims per month:                    %.1f\n", mean_per_month))
cat(sprintf("Mean daily claims — summer (Jun–Aug):     %.1f\n", mean_summer))
cat(sprintf("Mean daily claims — rest of year:         %.1f\n", mean_non_summer))
cat(sprintf("Summer / non-summer ratio:                %.2f\n",
            mean_summer / mean_non_summer))

# ── 8. Output as LaTeX table ──────────────────────────────────────────────────

summary_df <- tribble(
  ~Statistic,                                           ~Value,
  "Total claims (entire period)",                       formatC(n_total_claims,  format = "d", big.mark = ","),
  "Heat-related claims (any diagnosis position)",       sprintf("%s (%.2f\\%%)", formatC(n_heat_claims,    format = "d", big.mark = ","), n_heat_claims / n_total_claims * 100),
  "Claims in Austin and Dallas commuting zones",        sprintf("%s (%.2f\\%%)", formatC(n_austin_dallas,  format = "d", big.mark = ","), n_austin_dallas / n_total_claims * 100),
  "Mean claims per day",                                sprintf("%.1f", mean_per_day),
  "Mean claims per month",                              sprintf("%.1f", mean_per_month),
  "Mean daily claims --- summer (June--August)",        sprintf("%.1f", mean_summer),
  "Mean daily claims --- non-summer (Sep--May)",        sprintf("%.1f", mean_non_summer),
  "Summer to non-summer ratio",                         sprintf("%.2f", mean_summer / mean_non_summer)
)

summary_df %>%
  tt(caption = "Heat-related workers' compensation claims summary statistics. Total claim and heat claim counts cover the full sample period. Mean statistics exclude 2020--2021 to avoid COVID-period distortion.") %>%
  setNames(c("Statistic", "Value")) %>%
  theme_striped() %>%
  theme_latex(resize_width = 1, resize_direction = "down") %>%
  save_tt(table_out, overwrite = TRUE)

cat(sprintf("\nTable saved to: %s\n", table_out))
