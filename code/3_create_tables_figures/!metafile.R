# ------------------------------------------------------------------------------
# Purpose: Metafile for creating tables and figures
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

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
# Summary statistics
# ------------------------------------------------------------------------------

# Summary statistics wrapper script
source(file.path(create_tables_figures, "summary_statistics_wrapper.R"))

# ------------------------------------------------------------------------------
# XX
# ------------------------------------------------------------------------------
# Source individual table/figure scripts
source(file.path(create_tables_figures, "summary_outcomes_config.R"))
source(file.path(create_tables_figures, "summary_variance_table.R"))
source(file.path(create_tables_figures, "summary_variance_within_between.R"))
source(file.path(create_tables_figures, "top_bottom_firm_ratings_dual_axis_figures.R"))
source(file.path(create_tables_figures, "heatmaps_combined.R"))
source(file.path(create_tables_figures, "eiv_table_panels.R"))
source(file.path(create_tables_figures, "eiv_table_discretion.R"))
source(file.path(create_tables_figures, "eiv_table_selectivity_discretion.R"))
if (!nzchar(Sys.getenv("CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS"))) {
  Sys.setenv(CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS = "499")
}
source(file.path(create_tables_figures, "cross_sample_signal_corr.R"))
source(file.path(create_tables_figures, "cross_sample_signal_corr_raw.R"))
source(file.path(create_tables_figures, "cross_sample_signal_corr_placebo.R"))
source(file.path(create_tables_figures, "cross_model_corr.R"))
source(file.path(create_tables_figures, "valence_correlation_bars.R"))
source(file.path(create_tables_figures, "opposite_valence_corr_table.R"))
source(file.path(create_tables_figures, "eiv_table_within_between.R"))
source(file.path(create_tables_figures, "eiv_table_within_between_selectivity.R"))
source(file.path(create_tables_figures, "industry_ratings_dual_axis_figures.R"))
# Skipping Revelio tables for this branch run because the Revelio data build and
# section-2 Revelio EIV outputs are intentionally not run.
# source(file.path(create_tables_figures, "eiv_revelio_composition_tables.R"))
# source(file.path(create_tables_figures, "eiv_revelio_outcome_tables.R"))
source(file.path(create_tables_figures, "eiv_eeo1_share_tables.R"))
source(file.path(create_tables_figures, "eiv_coefplot_by_subgroup.R"))

# Appendix table of firm-level belief estimates by aggregation method (Stata)
run_stata_fail_fast(file.path(create_tables_figures, "firm_belief_estimates_by_aggregation_method_table.do"))


message("🎃 Tables and figures complete")

#source(file.path(create_tables_figures, "heatmap_plots.R"))
