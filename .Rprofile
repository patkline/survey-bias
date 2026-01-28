# ------------------------------------------------------------------------------
# Purpose: Auto-executing .Rprofile for survey bias project
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

# Activate renv, which manages package versions for reproducibility
source("renv/activate.R")

# Use Posit Package Manager for pre-compiled binaries (avoids compilation issues)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

# Ensure essential packages are installed
for (package in c("here", "jsonlite", "rlang")) {
  if (!requireNamespace(package, quietly = TRUE)) renv::install(package)
}

# Load here package for path management
library(here)

# Set working directory to the survey-bias folder
setwd(here::here())

# Confirm working directory root is the survey-bias folder
if (basename(getwd()) != "survey-bias") {
  stop("Error: Working directory is not set to the 'survey-bias' folder. Current directory: ", getwd())
}

# Source globals.R to set up project paths and load packages
source("code/globals.R")

# Message that this file was sourced 
message("🎃 .Rprofile was successfully sourced")