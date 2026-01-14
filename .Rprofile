# ------------------------------------------------------------------------------
# Purpose: Auto-executing .Rprofile for survey bias project
#
# Created: Nico Rotundo 2026-01-06
# ------------------------------------------------------------------------------

# Activate renv, which manages package versions for reproducibility
source("renv/activate.R")

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

# NR: could auto-load globals here and then do not need to put it at the top of every script, but then lose the ability to edit the globals without restarting R (could do both)