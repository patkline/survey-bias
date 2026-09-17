# ------------------------------------------------------------------------------
# Purpose: Metafile for creating tables and figures
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# Generated output is intentionally not stored in the repository. Create the
# destination directories so a clean checkout also works in GitHub mode.
dir.create(figures, recursive = TRUE, showWarnings = FALSE)
dir.create(tables, recursive = TRUE, showWarnings = FALSE)

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

  # Keep Stata's batch log in R's temporary directory; restore cwd on exit.
  stata_logs_directory <- file.path(tempdir(), "survey_bias_stata_logs")
  dir.create(stata_logs_directory, recursive = TRUE, showWarnings = FALSE)
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

# Tables 1 and 2
source(file.path(create_tables_figures, "summary_statistics_tables.R"))

# Figure 1
source(file.path(create_tables_figures, "summary_statistics_histograms.R"))

# Figures 2--5 and Appendix Figures A1--A2
source(file.path(create_tables_figures, "summary_statistics_bar_graphs.R"))

# Table 3
source(file.path(create_tables_figures, "belief_summary_ols_borda.R"))

# Table 4 and Appendix Table A1
source(file.path(create_tables_figures, "summary_variance_table.R"))

# Figure 6
source(file.path(create_tables_figures, "firm_ratings_signal_correlation_heatmaps.R"))

if (!nzchar(Sys.getenv("CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS"))) {
  Sys.setenv(CROSS_SAMPLE_SIGNAL_CORR_BOOTSTRAP_REPS = "499")
}

# Table 5
source(file.path(create_tables_figures, "cross_sample_signal_corr.R"))

# Figures 7--9
source(file.path(create_tables_figures, "top_bottom_firm_ratings_dual_axis_figures.R"))

# Figures 10--11
source(file.path(create_tables_figures, "industry_ratings_dual_axis_figures.R"))

# Table 6
source(file.path(create_tables_figures, "average_beliefs_vs_linkedin_workforce_shares_table.R"))

# Table 7
source(file.path(create_tables_figures, "eiv_pooled_belief_selectivity_controls_table.R"))

# Table 8
source(file.path(create_tables_figures, "eiv_table_selectivity_discretion.R"))

# Appendix Figure A3
source(file.path(create_tables_figures, "valence_correlation_bars.R"))

# Appendix Figure A4
source(file.path(create_tables_figures, "eiv_coefplot_by_subgroup.R"))

# Appendix Figure A5
source(file.path(create_tables_figures, "firm_likert_amad_scatterplots.R"))

# Appendix Table A2
source(file.path(create_tables_figures, "summary_variance_within_between.R"))

# Appendix Table A3
source(file.path(create_tables_figures, "subgroup_belief_mean_signal_variance_table.R"))

# Appendix Table A4
source(file.path(create_tables_figures, "cross_sample_signal_corr_raw.R"))

# Appendix Table A5
source(file.path(create_tables_figures, "eiv_contact_conduct_subsamples_appendix.R"))

# Appendix Table A6
source(file.path(create_tables_figures, "eiv_linkedin_share_controls_table.R"))

# Appendix Table A7
run_stata_fail_fast(file.path(create_tables_figures, "firm_belief_estimates_by_aggregation_method_table.do"))

message("🎃 Tables and figures complete")
