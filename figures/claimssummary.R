# ============================================================
# Script: claimssummary.R
# Purpose: Summary tables and time-series figure for claims data
# Inputs:  processed_data/intermediate/claimsclean.parquet
#          processed_data/clean/claimsczpanel.parquet
#          raw_data/icd9 and icd10/ (ICD code descriptions)
# Outputs: output/tables/individual_claims_sum.tex
#          output/tables/top10_icd_codes.tex
#          output/figures/claims_per_100k_overtime.png
# Author: EK  Date: 2026-03-31
# ============================================================

# install.packages(c('arrow', 'dplyr', 'modelsummary', 'tinytable', 'knitr',
#                    'readr', 'ggplot2'))

library(arrow)
library(dplyr)
library(readr)
library(modelsummary)
library(tinytable)
library(knitr)
library(ggplot2)

# ── 1. Load cleaned individual claims ─────────────────────────────────────────

claims <- read_parquet("C:/Users/emilk/deathstar/processed_data/intermediate/claimsclean.parquet")

claims <- claims %>%
  rename(
    "Employee age"           = age,
    "Employee marital status" = Employee.Marital.Status.Code,
    "Employee gender"        = Employee.Gender.Code,
    "Total charge"           = Total.Charge.Per.Bill
  )

# ── 2. Summary statistics table ───────────────────────────────────────────────

datasummary(`Employee age` + `Total charge` + `Employee gender` ~ N + Mean + SD + Min + Max,
            data   = claims,
            title  = "Individual claim summary statistics",
            output = "tinytable") |>
  format_tt(escape = TRUE) |>
  theme_striped() |>
  theme_latex(resize_width = 1, resize_direction = "down") |>
  save_tt("C:/Users/emilk/deathstar/output/tables/individual_claims_sum.tex", overwrite = TRUE)

# ── 3. Top 10 ICD diagnosis codes ─────────────────────────────────────────────

icd9 <- read_csv("C:/Users/emilk/deathstar/raw_data/icd9 and icd10/icd9dx2015.csv") %>%
  select(code = dgns_cd, description = longdesc) %>%
  mutate(code = as.character(code))

icd10 <- read_fwf(
  "C:/Users/emilk/deathstar/raw_data/icd9 and icd10/FY24-CMS-1785-F-Code-Descriptions/icd10cm_codes_2024.txt",
  fwf_cols(code = c(1, 7), description = c(9, NA)),
  col_types = "cc"
) %>%
  mutate(code = trimws(code))

icd_lookup <- bind_rows(icd9, icd10)

top10 <- claims %>%
  count(First.ICD.9CM.or.ICD.10CM.Diagnosis.Code, sort = TRUE) %>%
  slice_head(n = 10) %>%
  rename(code = First.ICD.9CM.or.ICD.10CM.Diagnosis.Code) %>%
  filter(!is.na(code), code != "") %>%
  mutate(code_lookup = gsub("\\.", "", code)) %>%
  left_join(icd_lookup, by = c("code_lookup" = "code")) %>%
  select(code, description, n)

top10 %>%
  mutate(n = formatC(n, format = "d", big.mark = ",")) %>%
  tt(caption = "Top 10 Most Common Diagnosis Codes") %>%
  setNames(c("ICD Code", "Description", "N")) %>%
  theme_striped() %>%
  theme_latex(resize_width = 1, resize_direction = "down") %>%
  save_tt("C:/Users/emilk/deathstar/output/tables/top10_icd_codes.tex", overwrite = TRUE)

# ── 4. Claims per 100k over time ──────────────────────────────────────────────

panel <- read_parquet("C:/Users/emilk/deathstar/processed_data/clean/claimsczpanel.parquet")

daily_avg <- panel %>%
  filter(Bill.Selection.Date <= as.Date("2024-07-01")) %>%
  group_by(Bill.Selection.Date) %>%
  summarise(avg_claims_per_100k = mean(claims_per_100k, na.rm = TRUE), .groups = "drop")

ggplot(daily_avg, aes(x = Bill.Selection.Date, y = avg_claims_per_100k)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    x     = NULL,
    y     = "Claims per 100,000 Workers",
    title = "Workers' Compensation Claims per 100,000 Workers Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("C:/Users/emilk/deathstar/output/figures/claims_per_100k_overtime.png",
       width = 10, height = 6, dpi = 300)
