# ============================================================
# Script: claimsdata.R
# Purpose: Build CZ x day claims panel matched to BLS employment
# Inputs:  raw_data/DeathStar health data/ (SV1, SV2 CSVs)
#          raw_data/bls_data/ (allhlcn xlsx files)
#          raw_data/admin boundaries/commuting-zones-2020.csv
#          raw_data/admin boundaries/ZIP_COUNTY_032020.xlsx
# Outputs: processed_data/intermediate/claimsclean.parquet
#          processed_data/clean/claimsczpanel.parquet
# Author: EK  Date: 2026-03-31
# ============================================================

# install.packages(c('readxl', 'dplyr', 'arrow', 'tidyr', 'purrr', 'stringr', 'readr'))

library(readr)
library(readxl)
library(dplyr)
library(arrow)
library(tidyr)
library(purrr)
library(stringr)

# ── 1. Geographic crosswalks ──────────────────────────────────────────────────

cnty2cz <- read.csv("C:/Users/emilk/deathstar/raw_data/admin boundaries/commuting-zones-2020.csv",
                    colClasses = c(FIPStxt = "character"))

zip2cnty <- read_excel("C:/Users/emilk/deathstar/raw_data/admin boundaries/ZIP_COUNTY_032020.xlsx") %>%
  rename(FIPStxt = COUNTY) %>%
  group_by(ZIP) %>%
  slice_max(RES_RATIO, n = 1, with_ties = FALSE) %>%
  ungroup()

geo <- left_join(zip2cnty, cnty2cz, by = "FIPStxt") %>%
  filter(StateName == "Texas") %>%
  select(FIPStxt, ZIP, CountyName, StateName, CZ2020, CZName)

# ── 2. Load and combine SV1 and SV2 claims ───────────────────────────────────

keep_cols <- c("Bill.Selection.Date", "Bill.ID",
               "Employee.Mailing.Postal.Code", "Employee.Date.of.Birth",
               "Employee.Gender.Code", "Employee.Marital.Status.Code",
               "Employee.Date.of.Injury", "Total.Charge.Per.Bill",
               "First.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
               "Second.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
               "Third.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
               "Fourth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
               "Fifth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
               "Facility.Postal.Code")

data_dir <- "C:/Users/emilk/deathstar/raw_data/DeathStar health data/DeathStar health data"

sv2 <- bind_rows(
  read_csv(file.path(data_dir, "Institutional_Medical_Billing_Services_(SV2)_Header_Information_-_Historical_20260317.csv"), show_col_types = FALSE, name_repair = ~ make.names(., unique = TRUE))[, keep_cols],
  read_csv(file.path(data_dir, "Institutional_Medical_Billing_Services_(SV2)_Header_Information_20260317.csv"), show_col_types = FALSE, name_repair = ~ make.names(., unique = TRUE))[, keep_cols]
)

sv1 <- bind_rows(
  read_csv(file.path(data_dir, "Professional_Medical_Billing_Services_(SV1)_Header_Information_-_Historical_20260317.csv"), show_col_types = FALSE, name_repair = ~ make.names(., unique = TRUE))[, keep_cols],
  read_csv(file.path(data_dir, "Professional_Medical_Billing_Services_(SV1)_Header_Information_20260317.csv"), show_col_types = FALSE, name_repair = ~ make.names(., unique = TRUE))[, keep_cols]
)

# ── 3. Merge claims to commuting zones ───────────────────────────────────────

claims <- rbind(sv1, sv2) %>%
  mutate(Employee.Mailing.Postal.Code = substr(Employee.Mailing.Postal.Code, 1, 5)) %>%
  rename(ZIP = Employee.Mailing.Postal.Code) %>%
  left_join(geo, by = "ZIP")

# ── 4. Clean dates, age, and charges ─────────────────────────────────────────

claims <- claims %>%
  mutate(
    Bill.Selection.Date    = as.Date(Bill.Selection.Date, format = "%m/%d/%Y"),
    Employee.Date.of.Birth = as.Date(paste0("01/", Employee.Date.of.Birth), format = "%d/%m/%Y"),
    Total.Charge.Per.Bill  = as.numeric(gsub("[$,]", "", Total.Charge.Per.Bill)),
    age = as.numeric(difftime(Bill.Selection.Date, Employee.Date.of.Birth, units = "days")) / 365.25
  ) %>%
  filter(!is.na(age), age >= 14, age <= 95) %>%
  mutate(Employee.Gender.Code = if_else(Employee.Gender.Code == "", "U", Employee.Gender.Code))

write_parquet(claims, "C:/Users/emilk/deathstar/processed_data/intermediate/claimsclean.parquet")

# ── 5. Aggregate to CZ2020 x date panel ──────────────────────────────────────

cz_daily <- claims %>%
  filter(!is.na(CZ2020), !is.na(Bill.Selection.Date)) %>%
  group_by(CZ2020, Bill.Selection.Date) %>%
  summarise(n_claims = n(), .groups = "drop")

all_dates <- seq(min(cz_daily$Bill.Selection.Date),
                 max(cz_daily$Bill.Selection.Date),
                 by = "day")
all_cz <- unique(cz_daily$CZ2020)

panel <- expand_grid(CZ2020 = all_cz, Bill.Selection.Date = all_dates) %>%
  left_join(cz_daily, by = c("CZ2020", "Bill.Selection.Date")) %>%
  replace_na(list(n_claims = 0)) %>%
  mutate(
    year  = as.integer(format(Bill.Selection.Date, "%Y")),
    month = as.integer(format(Bill.Selection.Date, "%m")),
    day   = as.integer(format(Bill.Selection.Date, "%d")),
    qtr   = case_when(
      month %in% 1:3   ~ 1L,
      month %in% 4:6   ~ 2L,
      month %in% 7:9   ~ 3L,
      month %in% 10:12 ~ 4L
    )
  )

# ── 6. Load BLS employment and aggregate to CZ x quarter ─────────────────────

bls_files <- list.files(
  path       = "C:/Users/emilk/deathstar/raw_data/bls_data",
  pattern    = "^allhlcn",
  recursive  = TRUE,
  full.names = TRUE
)

bls_raw <- map_dfr(bls_files, ~ read_excel(.x, col_types = "text"))

bls <- bls_raw %>%
  rename_with(~ str_replace_all(., "[\r\n ]", "_") %>%
                str_replace_all(., "_+", "_") %>%
                tolower()) %>%
  filter(
    st == "48",
    str_trim(str_to_lower(ownership)) == "total covered",
    str_detect(str_to_lower(industry), "total, all industries")
  ) %>%
  mutate(
    fips = str_pad(paste0(st, cnty), 5, pad = "0"),
    year = as.integer(year),
    qtr  = as.integer(qtr),
    across(c(january_employment, february_employment, march_employment,
             april_employment,   may_employment,       june_employment,
             july_employment,    august_employment,    september_employment,
             october_employment, november_employment,  december_employment),
           ~ as.numeric(gsub(",", "", .)))
  ) %>%
  mutate(avg_emp = case_when(
    qtr == 1 ~ (january_employment  + february_employment + march_employment)    / 3,
    qtr == 2 ~ (april_employment    + may_employment      + june_employment)      / 3,
    qtr == 3 ~ (july_employment     + august_employment   + september_employment) / 3,
    qtr == 4 ~ (october_employment  + november_employment + december_employment)  / 3
  )) %>%
  select(fips, year, qtr, avg_emp)

cw <- geo %>%
  rename(fips = FIPStxt) %>%
  select(fips, CZ2020) %>%
  distinct(fips, CZ2020)

bls_cz <- bls %>%
  left_join(cw, by = "fips") %>%
  filter(!is.na(CZ2020)) %>%
  group_by(CZ2020, year, qtr) %>%
  summarise(emp_cz = sum(avg_emp, na.rm = TRUE), .groups = "drop")

# ── 7. Merge employment into panel and compute claims_per_100k ────────────────

panel <- panel %>%
  left_join(bls_cz, by = c("CZ2020", "year", "qtr")) %>%
  mutate(claims_per_100k = if_else(emp_cz > 0, (n_claims / emp_cz) * 100000, NA_real_))

write_parquet(panel, "C:/Users/emilk/deathstar/processed_data/clean/claimsczpanel.parquet")
