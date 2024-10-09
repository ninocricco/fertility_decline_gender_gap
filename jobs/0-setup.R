#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: SETUP FILE
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

# Initializing the R environment
renv::init()

# List of required packages
packages <- c(
  "tidyverse", "stargazer", "weights", "knitr", "gridExtra", "RColorBrewer",
  "fastDummies", "readstata13", "lubridate", "readxl", "janitor", "grid",
  "foreign", "scales", "reshape2", "haven", "foreach", "broom", "survey",
  "mice", "naniar", "tictoc", "readr", "kableExtra", "mitools", "ggrepel",
  "renv"
)

# Function to check and install packages
install_load_pkgs <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE, verbose = T)
  }
  library(pkg, character.only = TRUE)
}

# Install and update all packages
lapply(packages, install_load_pkgs)

# R environment snapshot
renv::snapshot()