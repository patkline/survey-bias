# ------------------------------------------------------------------------------
# Purpose: Metafile for creating tables and figures
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# Source individual table/figure scripts
source(file.path(create_tables_figures, "summary_item_worths.R"))
source(file.path(create_tables_figures, "heatmap_plots.R"))
source(file.path(create_tables_figures, "heatmaps_combined.R"))
source(file.path(create_tables_figures, "eiv_table_panels.R"))

message("🎃 Tables and figures complete")
