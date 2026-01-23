# ------------------------------------------------------------------------------
# Purpose: Metafile for data build
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# Run Python script to clean raw Qualtrics data
system2(python_venv_installation,
        args = file.path(build, "clean_raw_qualtrics_data.py"))

# Run sample preparation script
source(file.path(build, "sample_prep.R"))