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
source(file.path(create_tables_figures, "cross_sample_signal_corr_raw.R"))
source(file.path(create_tables_figures, "cross_sample_signal_corr_placebo.R"))
source(file.path(create_tables_figures, "cross_model_corr.R"))
source(file.path(create_tables_figures, "valence_correlation_bars.R"))
source(file.path(create_tables_figures, "opposite_valence_corr_table.R"))
source(file.path(create_tables_figures, "eiv_table_within_between.R"))
source(file.path(create_tables_figures, "industry_decomposition_line_charts.R"))

message("🎃 Tables and figures complete")

#source(file.path(create_tables_figures, "heatmap_plots.R"))
