# CLAUDE.md — Municipal Heat Ordinances & Worker Health Project

## Project Overview

This project estimates the causal effects of municipal worker heat ordinances
(Austin 2010, Dallas 2015) and their repeal by Texas HB 2127 (September 2023)
on workers' compensation claims and construction labor productivity.
A difference-in-differences / event study design is used to exploit the staggered 
adoption and sudden repeal of city-level ordinances as natural experiments.

---

## Directory Structure

```
deathstar/
├── raw_data/          # READ ONLY — never modify, overwrite, or delete
│   ├── DeathStar health data/     # Texas Dept of Insurance workers' comp claims
│   ├── bls_data/      # BLS all county Quarterly Census of Employment & Wages
│   ├── icd9 and icd10/ # ICD-9 and ICD-10 diagnosis code crosswalks
│   └── admin boundaries/    # Commuting zone and geographic boundary files
├── data/
│   ├── clean/         # Processed, analysis-ready datasets (can modify)
│   └── intermediate/  # Intermediate outputs during construction (can modify)
├── DeathStar/         # Code
│   ├── build/         # Data cleaning and construction scripts
│   ├── .claude/
│   │    └── CLAUDE.md      #(treat as read-only)
│   ├── .git/          # git folder
│   ├── README.md      # Git read me
│   ├── analysis/      # Regression and estimation scripts
│   └── figures/       # Plot and visualization scripts
├── output/
│   ├── tables/        # LaTeX and CSV regression tables
│   └── figures/       # Publication-quality figures
├── writing/           # Paper drafts and notes 
```

---

## DATA SAFEGUARDS — READ CAREFULLY

### raw_data/ is strictly read-only
- **Never write to, modify, overwrite, or delete any file in `raw_data/`.**
- Never run `rm`, `unlink()`, `file.remove()`, or any destructive operation
  targeting `raw_data/`.
- All cleaning and transformation must write outputs to `data/clean/` or
  `data/intermediate/`.

### Before any file operation
- When an intermediate or cleaned data file is needed in another file
  check that it exists already and do not create the file if it already exists.
- Only create an intermediate or cleaned data file that already exists if important
  changes to the build file have been made and subsequent analysis files rely
  on variables of the structure of the new build file to run.
- Never overwrite an existing file in `data/clean/` without first confirming
  the user wants to replace it.

### Git discipline
- Before any multi-file editing session, check `git status`. If there are
  uncommitted changes, flag this and ask whether to commit first.
- Suggest a `git branch` before major transformations. Merge branch
  to main after user confirms they are satisfied with the actions of the branch

---

## COST ESTIMATION — REQUIRED BEFORE LARGE OPERATIONS

Before running any operation that is computationally expensive or uses a large amount of 
tokens (i.e. a high /cost), **estimate and report the expected cost/time first**, then
wait for the user to approve before executing.

### What requires a cost estimate
- Any operation reading or joining across the full claims dataset (~52M rows)
- Building or updating the PRISM weather panel (daily × commuting zone)
- Exporting large files to disk

### How to estimate
- Install and load any R packages needed to run the scripts without my permission.
- For R: use a 1% sample (`slice_sample(prop = 0.01)`) to benchmark time,
  then extrapolate. Report: *"This will take approximately X minutes and
  produce a file of approximately Y GB."*
- For file sizes: check with `file.info()` or `du -sh` before loading.
- Flag if an operation will produce output >1 GB.
- If an API or external data call is involved, estimate the number of requests
  and any associated rate limits.

---

## Coding Conventions

### Language & packages
- **R** or **Python**

### Style
- Snake_case for all object and variable names
- Every script should have a header block:
  ```r
  # ============================================================
  # Script: [name].R
  # Purpose: [one-line description]
  # Inputs: [files read]
  # Outputs: [files written]
  # Author: [initials]  Date: [YYYY-MM-DD]
  # ============================================================
  ```
- Save cleaned datasets as `.rds` (faster) or `.parquet` (for large files).
  Never overwrite raw data with `write.csv()`.

---

## Project-Specific Variable Names & Concepts

| Concept | Variable name convention |
|---|---|
| Commuting zone identifier | `cz_id` |
| Date | `date` (class Date, format YYYY-MM-DD) |
| Daily max temperature (°F) | `tmax_f` |
| Daily mean temperature (°F) | `tmean_f` |
| Temperature bin indicators | `tbin_[low]_[high]` (e.g. `tbin_95_100`) |
| City ordinance indicator | `city_ord` (1 = ordinance in force, 0 = not) |
| Post-HB2127 repeal indicator | `post_repeal` (1 = after Sept 2023) |
| Claims per 100k workers | `claims_per_100k` |
| ICD code | `icd_code`, `icd_version` (9 or 10) |
| Worker home zip code | `zip_home` |

### Geography
- Unit of analysis: **commuting zone × day**
- Worker location inferred from home zip code → USDA commuting zone crosswalk
- PRISM weather aggregated as area-weighted mean over commuting zone boundary

### Empirical specification (Equation 1)
The main regression interacts temperature bins with the city ordinance
indicator and a post-repeal indicator. The key coefficients of interest (π_k)
capture the mitigating effect of ordinances on the temperature–medical
utilization relationship. Always include:
- Commuting zone fixed effects (`| cz_id`)
- Day fixed effects (`| date`)
- Standard errors clustered at the commuting zone level

---

## What to Ask Before Acting

If any of the following apply, **stop and ask** rather than proceeding:

1. The task requires writing to `raw_data/`
2. The operation will take >10 minutes or produce >500 MB of output
3. A file already exists at the output path

---
