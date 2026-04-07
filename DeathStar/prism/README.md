# Texas Commuting Zone Daily Weather Dataset
## Using PRISM Climate Data, 2010–2026

### Overview
This workflow constructs a panel dataset of daily weather variables for all Texas
commuting zones (CZs) from January 1, 2010 through the latest available PRISM date
(provisional through 2026). The final output is a long-format CSV/Parquet:

```
cz_id | date | tmax | tmin | tmean | ppt | tdmean | vpdmin | vpdmax
```

---

### Directory Structure
```
tx_cz_weather/
├── README.md
├── 01_download_prism.R          # Download daily PRISM rasters (R)
├── 02_get_commuting_zones.py    # Download & prep Texas CZ shapefile (Python)
├── 03_extract_cz_weather.R      # Zonal extraction: PRISM → CZ polygons (R)
├── 04_build_panel.py            # Assemble & clean final panel dataset (Python)
├── config.yaml                  # Shared configuration
└── environment/
    ├── requirements.txt         # Python dependencies
    └── packages.R               # R dependencies
```

---

### Data Sources
| Source | Description | URL |
|--------|-------------|-----|
| PRISM Climate Group | 4km daily gridded climate (BIL rasters) | https://prism.oregonstate.edu |
| USDA ERS | 2010 Commuting Zone shapefiles | https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/ |

---

### PRISM Variables Downloaded
| Variable | Description | Units |
|----------|-------------|-------|
| `ppt`    | Precipitation | mm |
| `tmax`   | Maximum temperature | °C |
| `tmin`   | Minimum temperature | °C |
| `tmean`  | Mean temperature | °C |
| `tdmean` | Mean dew point temperature | °C |
| `vpdmin` | Min vapor pressure deficit | hPa |
| `vpdmax` | Max vapor pressure deficit | hPa |

> **Note:** PRISM data has three stability tiers:
> - **stable**: finalized (available ~6 months after date)
> - **provisional**: recent months, subject to revision
> - **early**: most recent ~2 weeks, least reliable

---

### Setup & Execution Order

**Step 0 — Install dependencies**
```bash
# R
Rscript environment/packages.R

# Python
pip install -r environment/requirements.txt
```

**Step 1 — Download PRISM rasters** (~20–60 GB depending on variables)
```bash
Rscript 01_download_prism.R
```

**Step 2 — Download Texas commuting zone shapefile**
```bash
python 02_get_commuting_zones.py
```

**Step 3 — Extract zonal statistics per commuting zone**
```bash
Rscript 03_extract_cz_weather.R
```

**Step 4 — Build final panel dataset**
```bash
python 04_build_panel.py
```

---

### Zonal Aggregation Method
Each PRISM raster cell is assigned to a commuting zone using **area-weighted mean**
via `exactextractr`, which handles partial overlaps at polygon boundaries correctly.
This is important for CZs that span PRISM grid cell edges.

---

### Runtime Estimates
| Step | Estimated Time |
|------|---------------|
| Download PRISM (7 vars × 16 yrs) | 4–12 hours |
| CZ shapefile download | < 1 min |
| Zonal extraction | 2–6 hours |
| Panel assembly | < 5 min |

> Tip: Run Step 1 overnight. Steps 3–4 can be parallelized by year.
