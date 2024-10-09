#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: INSTALLS AND LOADS REQUIRED LIBRARIES
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

# Calls saved R environment with specified package versions
renv::restore()

# List of required packages
packages <- c(
  "tidyverse", "stargazer", "weights", "knitr", "gridExtra", "RColorBrewer",
  "fastDummies", "readstata13", "lubridate", "readxl", "janitor", "grid",
  "foreign", "scales", "reshape2", "haven", "foreach", "broom", "survey",
  "mice", "naniar", "tictoc", "readr", "kableExtra", "mitools", "ggrepel",
  "renv"
)

# Function to check and install packages
load_pkgs <- function(pkg) {
  library(pkg, character.only = TRUE)
}

# Install and update all packages
lapply(packages, load_pkgs)