# ------------------------------------------------------------------------------
# Purpose: Compatibility wrapper for the old between-industry-only command
# ------------------------------------------------------------------------------

Sys.setenv(INDUSTRY_HEATMAP_INPUTS = "between")
source(file.path("code", "2_analysis", "run_industry_heatmap_inputs.R"))
