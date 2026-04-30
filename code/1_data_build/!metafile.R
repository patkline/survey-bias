# ------------------------------------------------------------------------------
# Purpose: Metafile for data build
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# ------------------------------------------------------------------------------
# Define wrapper for running Python scripts that stops metafile
# execution if the given script fails
# ------------------------------------------------------------------------------
# Define a helper function that runs one Python script
run_python_fail_fast <- function(script_path) {

  # Run the script using the project's Python virtual environment
  status <- system2(python_venv_installation, args = script_path)

  # Check if the script failed
  if (status != 0L) {
    # Stop immediately and show the exact script that failed
    stop(sprintf("🪦 Python script failed with exit status %s: %s", status, shQuote(script_path)), call. = FALSE)
  }
  
  # Return silently when the script succeeds
  invisible(status)
}

# ------------------------------------------------------------------------------
# Data build
# ------------------------------------------------------------------------------

# Run Python script to clean raw Qualtrics data
run_python_fail_fast(file.path(build, "clean_raw_qualtrics_data.py"))

# Run script to output firm-industry sic code mapping from aer paper replication package  
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_aer_replication_package.py"))

# Run script to output firm-industry sic code mapping from RefUSA and aer paper replication package 
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_refusa.py"))

# Run script that harmonizes industry codes across data sources and creates final crosswalk of firms to industries for use in analysis
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_industry_map.py"))

# Create final working datasets
source(file.path(build, "sample_prep.R"))
