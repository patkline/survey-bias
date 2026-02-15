# ------------------------------------------------------------------------------
# Purpose: Metafile for creating tables and figures
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

# Summary statistics wrapper script
source(file.path(create_tables_figures, "summary_statistics_wrapper.R"))

# ------------------------------------------------------------------------------
# XX
# ------------------------------------------------------------------------------
# Source individual table/figure scripts
source(file.path(create_tables_figures, "summary_item_worths.R"))
source(file.path(create_tables_figures, "heatmaps_combined.R"))
source(file.path(create_tables_figures, "eiv_table_panels.R"))
source(file.path(create_tables_figures, "eiv_table_discretion.R"))
source(file.path(create_tables_figures, "eiv_table_bivariate.R"))
source(file.path(create_tables_figures, "cross_sample_signal_corr.R"))

message("🎃 Tables and figures complete")

#source(file.path(create_tables_figures, "heatmap_plots.R"))