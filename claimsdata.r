install.packages('readxl')
install.packages('dplyr')
install.packages('arrow')
install.packages('modelsummary')
install.packages('tinytable')
install.packages(c("rlang", "vctrs"))
install.packages('icd')

library(readr)
library(icd)
library(readxl)
library(dplyr)
library(arrow)
library(modelsummary)
library(tinytable)
library(knitr)
library(tidyr)
library(purrr)
library(stringr)

cnty2cz <- read.csv("C:/Users/emilk/deathstar/commuting-zones-2020.csv", colClasses = c(FIPStxt = "character"))
zip2cnty <- read_excel("C:/Users/emilk/deathstar/ZIP_COUNTY_032020.xlsx")

zip2cnty <- zip2cnty %>% rename(FIPStxt = COUNTY)

geo <- left_join(zip2cnty, cnty2cz, by = 'FIPStxt')

geo <- geo %>%
        filter(StateName=='Texas') %>%
        select(FIPStxt,ZIP,CountyName,StateName,CZ2020,CZName)

#health data

sv2hist <- read.csv("C:/Users/emilk/deathstar/DeathStar health data/DeathStar health data/Institutional_Medical_Billing_Services_(SV2)_Header_Information_-_Historical_20260317.csv")
sv2 <- read.csv("C:/Users/emilk/deathstar/DeathStar health data/DeathStar health data/Institutional_Medical_Billing_Services_(SV2)_Header_Information_20260317.csv")

sv2 <- rbind(sv2hist, sv2)

sv2<- sv2[, c("Bill.Selection.Date","Bill.ID","Employee.Date.of.Birth",
              "Employee.Gender.Code","Employee.Marital.Status.Code",
              "Employee.Date.of.Injury","Total.Charge.Per.Bill",
              "Employee.Mailing.Postal.Code",
              "First.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
              "Second.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
              "Third.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
              "Fourth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
              "Fifth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
              "Facility.Postal.Code")]

sv2$Employee.Mailing.Postal.Code <- substr(sv2$Employee.Mailing.Postal.Code, 1, 5)

sv1hist <- read.csv("C:/Users/emilk/deathstar/DeathStar health data/DeathStar health data/Professional_Medical_Billing_Services_(SV1)_Header_Information_-_Historical_20260317.csv")

sv1hist <- sv1hist[,c("Bill.Selection.Date","Bill.ID",
                             "Employee.Mailing.Postal.Code","Employee.Date.of.Birth",
                             "Employee.Gender.Code","Employee.Marital.Status.Code",
                             "Employee.Date.of.Injury","Total.Charge.Per.Bill",
                             "First.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                             "Third.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                             "Second.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                             "Fourth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                             "Fifth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                             "Facility.Postal.Code")]

sv1 <- read.csv("C:/Users/emilk/deathstar/DeathStar health data/DeathStar health data/Professional_Medical_Billing_Services_(SV1)_Header_Information_20260317.csv")

sv1 <- sv1[,c("Bill.Selection.Date","Bill.ID",
                 "Employee.Mailing.Postal.Code","Employee.Date.of.Birth",
                 "Employee.Gender.Code","Employee.Marital.Status.Code",
                 "Employee.Date.of.Injury","Total.Charge.Per.Bill",
                 "First.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                 "Third.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                 "Second.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                 "Fourth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                 "Fifth.ICD.9CM.or.ICD.10CM.Diagnosis.Code",
                 "Facility.Postal.Code")]

sv1 <- rbind(sv1hist, sv1)

sv1$Employee.Mailing.Postal.Code <- substr(sv1$Employee.Mailing.Postal.Code, 1, 5)

# merge claims to geo data

claims <- rbind(sv1, sv2)
claims <- claims %>% rename(ZIP = Employee.Mailing.Postal.Code)

claims <- left_join(claims, geo, by = 'ZIP')

write_parquet(claims, "C:/Users/emilk/deathstar/DeathStar health data/claimsgeo.parquet")

claims <- read_parquet("C:/Users/emilk/deathstar/DeathStar health data/claimsgeo.parquet")

# Summary data

claims <- claims %>%
  mutate(
    Bill.Selection.Date    = as.Date(Bill.Selection.Date, format = "%m/%d/%Y"),        # "10/30/2018" → Date
    Employee.Date.of.Birth   = as.Date(paste0("01/", Employee.Date.of.Birth), format = "%d/%m/%Y"),  # "2/2018" → Date (first of month)
    Total.Charge.Per.Bill  = as.numeric(gsub("[$,]", "", Total.Charge.Per.Bill))       # "$1,450.00" → 1450
  )

claims$age <- as.numeric(difftime(claims$Bill.Selection.Date, claims$Employee.Date.of.Birth, units = "days")) / 365.25

claims <- claims[!is.na(claims$age) & claims$age >= 14 & claims$age <= 95, ]
claims[['Employee.Gender.Code']][claims[['Employee.Gender.Code']] == ''] <- 'U'

claims <- claims %>%
  rename("Employee age" = age,
         "Employee marital status" = Employee.Marital.Status.Code,
         "Employee gender" = Employee.Gender.Code,
         "Total charge"= Total.Charge.Per.Bill)

# sum table 1 part 1

datasummary(`Employee age` + `Total charge` + `Employee gender` ~ N + Mean + SD + Min + Max,
            data = claims,
            title = "Individual claim summary statistics",
            output='tinytable') |>
format_tt(escape = TRUE) |>
theme_striped() |> 
theme_latex(resize_width= 1, resize_direction="down") |>
save_tt("C:/Users/emilk/deathstar/individual_claims_sum.tex", overwrite = TRUE)

# ICD9 and 10 codes

# Load ICD-9 from NBER CSV
icd9 <- read_csv("C:/Users/emilk/deathstar/icd9dx2015.csv") %>%
  select(code = dgns_cd, description = longdesc) %>%
  mutate(code = as.character(code))

# Load ICD-10 from CMS fixed-width txt
icd10 <- read_fwf("C:/Users/emilk/deathstar/FY24-CMS-1785-F-Code-Descriptions/icd10cm_codes_2024.txt",
                  fwf_cols(code = c(1, 7), description = c(9, NA)),
                  col_types = "cc") %>%
  mutate(code = trimws(code))

# Stack both
icd_lookup <- bind_rows(icd9, icd10)

# Get top 10 codes and join descriptions
top10 <- claims %>%
  count(First.ICD.9CM.or.ICD.10CM.Diagnosis.Code, sort = TRUE) %>%
  slice_head(n = 10) %>%
  rename(code = First.ICD.9CM.or.ICD.10CM.Diagnosis.Code) %>%
  filter(!is.na(code), code != "") %>%
  mutate(code_lookup = gsub("\\.", "", code)) %>%  # strip decimals from all codes
  left_join(icd_lookup, by = c("code_lookup" = "code")) %>%
  select(code, description, n)

# Print table
kable(top10, col.names = c("ICD Code", "Description", "N"),
      format.args = list(big.mark = ","))

top10 %>%
  mutate(n = formatC(n, format = "d", big.mark = ",")) %>%
  tt(
    caption = "Top 10 Most Common Diagnosis Codes"
  ) %>%
  setNames(c("ICD Code", "Description", "N")) %>%
  theme_striped() %>%
  theme_latex(resize_width = 1, resize_direction = "down") %>%
  save_tt("C:/Users/emilk/deathstar/top10_icd_codes.tex", overwrite = TRUE)

## CZ level data

# Step 1: aggregate claims to CZ2020-date level
cz_daily <- claims %>%
  filter(!is.na(CZ2020), !is.na(Bill.Selection.Date)) %>%
  group_by(CZ2020, Bill.Selection.Date) %>%
  summarise(n_claims = n(), .groups = "drop")

# Step 2: create a complete grid of every CZ2020 x every date in the data
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
    day   = as.integer(format(Bill.Selection.Date, "%d"))
  )

# Total claims should match original (minus any filtered NAs)
sum(panel$n_claims)

# Every CZ should have the same number of rows (fully balanced)
panel %>% count(CZ2020) %>% summarise(min(n), max(n))

# Check dimensions: should be n_cz * n_days
nrow(panel) == length(all_cz) * length(all_dates)

write_parquet(panel,"C:/Users/emilk/deathstar/DeathStar health data/claimsczpanel.parquet")

# Matching labor data to claims

library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(tidyr)

# Step 1: load all BLS xlsx files
bls_dir <- "C:/Users/emilk/deathstar/bls_data"

bls_files <- list.files(
  path       = bls_dir,
  pattern    = "^allhlcn",
  recursive  = TRUE,
  full.names = TRUE
)

bls_raw <- map_dfr(bls_files, function(f) {
  read_excel(f, col_types = "text")
})

# Step 2: clean names, filter to Texas total covered all industries
bls <- bls_raw %>%
  rename_with(~ str_replace_all(., "[\r\n ]", "_") %>%   # handle \r\n in first col
                str_replace_all(., "_+", "_") %>%          # collapse multiple underscores
                tolower()) %>%
   filter(
    st == "48",
    str_trim(str_to_lower(ownership)) == "total covered",
    str_detect(str_to_lower(industry), "total, all industries")  # handles both versions
  ) %>%
  mutate(
    fips  = str_pad(paste0(st, cnty), 5, pad = "0"),
    year  = as.integer(year),
    qtr   = as.integer(qtr),
    # convert all monthly employment columns
    across(c(january_employment, february_employment, march_employment,
             april_employment,   may_employment,       june_employment,
             july_employment,    august_employment,    september_employment,
             october_employment, november_employment,  december_employment),
           ~ as.numeric(gsub(",", "", .)))
  ) %>%
  # average the three months within each quarter
  mutate(avg_emp = case_when(
    qtr == 1 ~ (january_employment  + february_employment + march_employment)     / 3,
    qtr == 2 ~ (april_employment    + may_employment      + june_employment)       / 3,
    qtr == 3 ~ (july_employment     + august_employment   + september_employment)  / 3,
    qtr == 4 ~ (october_employment  + november_employment + december_employment)   / 3
  )) %>%
  select(fips, year, qtr, avg_emp)

# Step 3: load crosswalk, filter to Texas
cw <- geo %>%
  rename(fips=FIPStxt) %>%
  select(fips, CZ2020)
cw <- cw %>% distinct(fips, CZ2020, .keep_all = TRUE)

# Step 4: aggregate to CZ2020-year-quarter
bls_cz <- bls %>%
  left_join(cw, by = "fips") %>%
  filter(!is.na(CZ2020)) %>%
  group_by(CZ2020, year, qtr) %>%
  summarise(emp_cz = sum(avg_emp, na.rm = TRUE), .groups = "drop")

# Step 5: merge into panel
panel <- panel %>%
  mutate(qtr = case_when(
    month %in% 1:3   ~ 1L,
    month %in% 4:6   ~ 2L,
    month %in% 7:9   ~ 3L,
    month %in% 10:12 ~ 4L
  )) %>%
  left_join(bls_cz, by = c("CZ2020", "year", "qtr"))

# Sanity checks
n_distinct(bls_cz$CZ2020)
summary(panel$emp_cz)
sum(is.na(panel$emp_cz))

panel <- panel %>%
  mutate(claims_per_100k = if_else(emp_cz > 0, (n_claims / emp_cz) * 100000, NA_real_))

write_parquet(panel, "C:/Users/emilk/deathstar/DeathStar health data/claimsczpanel.parquet")

#plot 

library(ggplot2)

panel <- panel %>% filter(Bill.Selection.Date <= as.Date("2024-07-01"))

# Collapse to daily average across all CZs
daily_avg <- panel %>%
  group_by(Bill.Selection.Date) %>%
  summarise(avg_claims_per_100k = mean(claims_per_100k, na.rm = TRUE),
            .groups = "drop")

# Plot
ggplot(daily_avg, aes(x = Bill.Selection.Date, y = avg_claims_per_100k)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", linewidth = 1) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    x     = NULL,
    y     = "Claims per 100,000 Workers",
    title = "Workers' Compensation Claims per 100,000 Workers Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
ggsave("C:/Users/emilk/deathstar/claims_per_100k_overtime.png", 
       width = 10, height = 6, dpi = 300)
