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
  mutate(
    Employee.Marital.Status.Code = case_when(
      Employee.Marital.Status.Code == "I" ~ "Single",
      Employee.Marital.Status.Code == "K" ~ "Unknown",
      Employee.Marital.Status.Code == "M" ~ "Married",
      Employee.Marital.Status.Code == "S" ~ "Separated",
      Employee.Marital.Status.Code == "U" ~ "Widowed",
      Employee.Marital.Status.Code == "" | is.na(Employee.Marital.Status.Code) ~ "Unknown",
      TRUE ~ Employee.Marital.Status.Code
    ),
    Employee.Gender.Code = case_when(
      Employee.Gender.Code == "F" ~ "Female",
      Employee.Gender.Code == "M" ~ "Male",
      TRUE                        ~ "Unknown"
    )
  ) %>%
  rename(
    "Employee age"            = age,
    "Employee marital status" = Employee.Marital.Status.Code,
    "Employee gender"         = Employee.Gender.Code,
    "Total claim cost"        = Total.Charge.Per.Bill
  )

# ── 2. Summary statistics table ───────────────────────────────────────────────

n_total <- nrow(claims)

cont_rows <- bind_rows(
  claims %>% summarise(
    Variable = "Employee age", Level = "",
    N    = formatC(sum(!is.na(`Employee age`)), format = "d", big.mark = ","),
    Mean = sprintf("%.2f", mean(`Employee age`, na.rm = TRUE)),
    SD   = sprintf("%.2f", sd(`Employee age`,   na.rm = TRUE)),
    Min  = sprintf("%.2f", min(`Employee age`,   na.rm = TRUE)),
    Max  = sprintf("%.2f", max(`Employee age`,   na.rm = TRUE)),
    Pct  = ""
  ),
  claims %>% summarise(
    Variable = "Total claim cost (\\$)", Level = "",
    N    = formatC(sum(!is.na(`Total claim cost`)), format = "d", big.mark = ","),
    Mean = sprintf("%.2f", mean(`Total claim cost`, na.rm = TRUE)),
    SD   = sprintf("%.2f", sd(`Total claim cost`,   na.rm = TRUE)),
    Min  = sprintf("%.2f", min(`Total claim cost`,   na.rm = TRUE)),
    Max  = sprintf("%.2f", max(`Total claim cost`,   na.rm = TRUE)),
    Pct  = ""
  )
)

gender_rows <- claims %>%
  count(`Employee gender`, name = "n") %>%
  arrange(`Employee gender`) %>%
  mutate(
    Variable = if_else(row_number() == 1, "Employee gender", ""),
    Level    = `Employee gender`,
    N        = formatC(n, format = "d", big.mark = ","),
    Mean = "", SD = "", Min = "", Max = "",
    Pct  = paste0(sprintf("%.1f", n / n_total * 100), "\\%")
  ) %>%
  select(Variable, Level, N, Mean, SD, Min, Max, Pct)

marital_rows <- claims %>%
  count(`Employee marital status`, name = "n") %>%
  arrange(`Employee marital status`) %>%
  mutate(
    Variable = if_else(row_number() == 1, "Employee marital status", ""),
    Level    = `Employee marital status`,
    N        = formatC(n, format = "d", big.mark = ","),
    Mean = "", SD = "", Min = "", Max = "",
    Pct  = paste0(sprintf("%.1f", n / n_total * 100), "\\%")
  ) %>%
  select(Variable, Level, N, Mean, SD, Min, Max, Pct)

bind_rows(cont_rows, gender_rows, marital_rows) %>%
  tt(caption = "Individual claim summary statistics") %>%
  setNames(c("", "", "N", "Mean", "SD", "Min", "Max", "\\%")) %>%
  theme_striped() %>%
  theme_latex(resize_width = 1, resize_direction = "down") %>%
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

total_claims <- sum(!is.na(claims$First.ICD.9CM.or.ICD.10CM.Diagnosis.Code) &
                      claims$First.ICD.9CM.or.ICD.10CM.Diagnosis.Code != "")

top10 <- claims %>%
  count(First.ICD.9CM.or.ICD.10CM.Diagnosis.Code, sort = TRUE) %>%
  slice_head(n = 10) %>%
  rename(code = First.ICD.9CM.or.ICD.10CM.Diagnosis.Code) %>%
  filter(!is.na(code), code != "") %>%
  mutate(
    code_lookup = gsub("\\.", "", code),
    pct = n / total_claims * 100
  ) %>%
  left_join(icd_lookup, by = c("code_lookup" = "code")) %>%
  mutate(description = case_when(
    code == "M54.5" & is.na(description) ~ "Low back pain",
    TRUE ~ description
  )) %>%
  select(code, description, n, pct)

top10 %>%
  mutate(
    n   = formatC(n, format = "d", big.mark = ","),
    pct = paste0(sprintf("%.1f", pct), "\\%")
  ) %>%
  tt(caption = "Top 10 Most Common Diagnosis Codes") %>%
  setNames(c("ICD Code", "Description", "N", "\\% of Claims")) %>%
  theme_striped() %>%
  theme_latex(resize_width = 1, resize_direction = "down") %>%
  save_tt("C:/Users/emilk/deathstar/output/tables/top10_icd_codes.tex", overwrite = TRUE)

# ── 4. Claims per 100k over time ──────────────────────────────────────────────

panel <- read_parquet("C:/Users/emilk/deathstar/processed_data/clean/claimsczpanel.parquet")

monthly_avg <- panel %>%
  filter(year >= 2010, Bill.Selection.Date < as.Date("2024-02-01"), !is.na(claims_per_100k)) %>%
  group_by(CZ2020, year, month) %>%
  summarise(monthly_claims_per_100k = sum(n_claims) / mean(emp_cz) * 100000,
            .groups = "drop") %>%
  group_by(year, month) %>%
  summarise(avg_claims_per_100k = mean(monthly_claims_per_100k, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

ggplot(monthly_avg, aes(x = date, y = avg_claims_per_100k)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x       = NULL,
    y       = "Claims per 100,000 Workers",
    title   = "Workers' Compensation Claims per 100,000 Workers Over Time",
    caption = "Notes: Monthly workers' compensation claims per 100,000 covered employees in Texas, January 2010 \u2013 January 2024.\nEach observation is the average across commuting zones of total claims in that month divided by quarterly QCEW covered employment.\nClaims data are from the Texas Department of Insurance medical billing records (SV1 and SV2).\nEmployment denominators are county-level covered employment from the Bureau of Labor Statistics\nQuarterly Census of Employment and Wages, aggregated to commuting zones."
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("C:/Users/emilk/deathstar/output/figures/claims_per_100k_overtime.png",
       width = 10, height = 6, dpi = 300)

# ── 5. Seasonal distribution of claims ───────────────────────────────────────

month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

seasonal <- panel %>%
  filter(
    Bill.Selection.Date < as.Date("2024-02-01"),
    !year %in% c(2020, 2021),
    !is.na(claims_per_100k)
  ) %>%
  group_by(CZ2020, year, month) %>%
  summarise(avg_daily = mean(claims_per_100k, na.rm = TRUE), .groups = "drop") %>%
  mutate(month = factor(month, levels = 1:12, labels = month_labels))

ggplot(seasonal, aes(x = month, y = avg_daily)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.size = 0.5,
               outlier.alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 2,
               color = "darkred") +
  labs(
    x       = NULL,
    y       = "Avg Daily Claims per 100,000 Workers",
    title   = "Seasonal Distribution of Workers' Compensation Claims",
    caption = "Note: 2020 and 2021 excluded. Red diamonds indicate monthly means.\nEach observation is a commuting zone \u00d7 year monthly average."
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("C:/Users/emilk/deathstar/output/figures/claims_seasonal.png",
       width = 10, height = 6, dpi = 300)
