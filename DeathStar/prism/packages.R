# environment/packages.R
# Install all required R packages for the TX CZ Weather workflow

cat("Installing R dependencies...\n")

# CRAN packages
cran_packages <- c(
  "prism",          # PRISM Climate Group API wrapper
  "terra",          # Modern raster handling (replaces raster pkg)
  "sf",             # Simple features / vector data
  "exactextractr",  # Fast, accurate zonal statistics
  "tidyverse",      # dplyr, tidyr, readr, purrr, ggplot2
  "lubridate",      # Date manipulation
  "yaml",           # Read config.yaml
  "glue",           # String interpolation
  "furrr",          # Parallel purrr (parallel extraction by year)
  "progressr",      # Progress bars
  "fs"              # File system operations
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    cat(sprintf("  Installed: %s\n", pkg))
  } else {
    cat(sprintf("  Already installed: %s\n", pkg))
  }
}

invisible(lapply(cran_packages, install_if_missing))
cat("\nAll R packages ready.\n")
