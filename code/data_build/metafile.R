# ------------------------------------------------------------------------------
# Purpose: Metafile for data build
#
# Created: Nico Rotundo 2026-01-11
# ------------------------------------------------------------------------------

# Run globals
source("code/globals.R")

# ------------------------------------------------------------------------------
# Data build
# ------------------------------------------------------------------------------

# Run Python script to clean raw Qualtrics data
system2(python_venv_installation,
        args = file.path(build, "clean_raw_qualtrics_data.py"))

# Run script to output firm-industry sic code mapping from aer paper replication package  
system2(python_venv_installation,
        args = file.path(build, "create_firm_industry_crosswalk_aer_replication_package.py"))

# Run script to output firm-industry sic code mapping from RefUSA and aer paper replication package 
system2(python_venv_installation,
        args = file.path(build, "create_firm_industry_crosswalk_refusa.py"))

# Run script that harmonizes industry codes across data sources and creates final crosswalk of firms to industries for use in analysis
system2(python_venv_installation,
        args = file.path(build, "create_firm_industry_crosswalk_industry_map.py"))

# Create final working datasets 
source(file.path(build, "sample_prep.R"))