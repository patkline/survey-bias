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

# ------------------------------------------------------------------------------------------------
# Define wrapper for running Stata scripts that stops metafile execution if the 
#given script fails
#
# Note. Stata's batch mode (-b) always exits 0 regardless of whether the script 
# errored --- failure detection requires parsing the resulting .log file for the 
# `r(<errcode>);` pattern that Stata writes on any command failure
# ------------------------------------------------------------------------------------------------
# Define a helper function that runs one Stata script in batch mode
run_stata_fail_fast <- function(script_path) {

  # Define candidate Stata CLI binary names 
  stata_cli_candidates <- c("stata-mp", "stata-se", "stata-be", "stata")

  # Search for the first available Stata CLI on PATH
  stata_cli <- ""
  
  # Loop over candidates 
  for (candidate_bin in stata_cli_candidates) {
    
    # Assign the candidate path if found
    candidate_path <- Sys.which(candidate_bin)
    
    # If the candidate path is non-empty, we found a Stata CLI
    if (nzchar(candidate_path)) {
      # Use the first found Stata CLI and break the loop
      stata_cli <- unname(candidate_path)
      break
    }
  }

  # Fail-fast if no Stata CLI found on PATH
  if (!nzchar(stata_cli)) {
    stop(sprintf("🧌 No Stata CLI on PATH (tried: %s). On macOS, symlink the binary into /usr/local/bin, e.g.: ln -s /Applications/Stata/StataMP.app/Contents/MacOS/stata-mp /usr/local/bin/stata-mp",
                 paste(stata_cli_candidates, collapse = ", ")), call. = FALSE)
  }

  # Change R's cwd to code/logs so Stata's batch log lands there; restore on exit
  stata_logs_directory <- file.path(code, "logs")
  previous_working_directory <- setwd(stata_logs_directory)
  on.exit(setwd(previous_working_directory), add = TRUE)

  # Run the script in batch mode
  status <- system2(stata_cli, args = c("-b", "do", script_path))

  # Stata batch writes the log as <stata_cwd>/<script_basename>.log
  log_path <- file.path(stata_logs_directory, sub("\\.do$", ".log", basename(script_path)))

  # Fail-fast if the batch run did not produce a log
  if (!file.exists(log_path)) {
    stop(sprintf("🪦 Stata batch run did not produce log file: %s (exit status %s)", log_path, status), call. = FALSE)
  }

  # Scan the log for Stata's error signature: r(<errcode>); on a line by itself
  log_error_lines <- grep("^r\\([0-9]+\\);", readLines(log_path), value = TRUE)
  if (length(log_error_lines) > 0) {
    stop(sprintf("🪦 Stata script failed: %s\n  First error line(s):\n  %s\n  Full log: %s",
                 shQuote(script_path), paste(log_error_lines, collapse = "\n  "), log_path), call. = FALSE)
  }

  # Return silently when the script succeeds
  invisible(status)
}

# ------------------------------------------------------------------------------
# Data build
# ------------------------------------------------------------------------------

# Clean raw Qualtrics data
run_python_fail_fast(file.path(build, "clean_raw_qualtrics_data.py"))

# Output firm-industry sic code mapping from aer paper replication package  
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_aer_replication_package.py"))

# Output firm-industry sic code mapping from RefUSA and aer paper replication package 
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_refusa.py"))

# Harmonize industry codes across data sources and creates final crosswalk of firms to industries for use in analysis
run_python_fail_fast(file.path(build, "create_firm_industry_crosswalk_industry_map.py"))

# Build industry crosswalks (ind1990 → 2-digit SIC + 3-digit NAICS)
run_stata_fail_fast(file.path(build, "create_ind1990_crosswalks.do"))

# Clean EEO-1 2023 PUF and aggregate to naics3 x race x sex employment
run_stata_fail_fast(file.path(build, "build_industry_emp_by_demographic_eeo1.do"))

# Clean CPS ORG microdata and aggregate to industry x race x sex x age_bin employment and wage
run_stata_fail_fast(file.path(build, "build_industry_emp_wage_by_demographic_cps.do"))

# Create final working datasets
source(file.path(build, "sample_prep.R"))
