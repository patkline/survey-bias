# ------------------------------------------------------------------------------
# Purpose: Create a firm-industry crosswalk from the csv of 
# firms, their sic codes, and their sic code groupings in the aer 
# paper replication package
#
# Created: Nico Rotundo 2026-03-03
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Import packages and path globals
# ------------------------------------------------------------------------------
# Import pandas for dataframe operations
import pandas as pd

# Import sys to edit module search path
import sys

# Import Path for filesystem path handling
from pathlib import Path

# Add code directory to Python path to import globals module
sys.path.insert(0, str(Path(__file__).parent.parent))

# Import path globals
from globals import code, external, dump 

# ------------------------------------------------------------------------------
# Import csv of estimation sample of firms, their sic codes, and 
# their sic code groupings from the aer paper replication package
# ------------------------------------------------------------------------------
# Assign estimation sample csv to dataframe 
theta_estimates_race = pd.read_csv(external / "theta_estimates_race.csv")

# Keep necessary variables and rename them
theta_estimates_race = theta_estimates_race[["firm_id", "sic_combined", "sic_grouped_code"]].rename(
    columns={
        "firm_id": "firm_id_aer_replication_package", 
        "sic_combined": "sic_code_two_digit_aer_replication_package", 
        "sic_grouped_code": "sic_code_aer_two_digit_aggregated_aer_replication_package"}
    )

# Export cleaned dataframe to csv in my scratch folder to view 
#theta_estimates_race.to_csv(code / "scratch_nico" / "firm_industry_crosswalk_aer_replication_package.csv", index = False)

# ------------------------------------------------------------------------------
# Merge names from aer replication package 
# ------------------------------------------------------------------------------
# Import csv of firm ids and names from aer replication package
formatted_firm_names_csv = pd.read_csv(external / "formatted_firm_names.csv")

# Rename the key column so it matches the renamed key in theta_estimates_race
formatted_firm_names_csv = formatted_firm_names_csv.rename(columns = {"firm_id": "firm_id_aer_replication_package"})

# Merge firm_name onto theta_estimates_race using the shared renamed key
theta_estimates_race = theta_estimates_race.merge(
    # Use the formatted firm names dataframe as the right-hand merge table
    formatted_firm_names_csv,
    
    # Merge using the renamed firm id key present in both dataframes
    on = "firm_id_aer_replication_package",
    
    # Keep all rows from both dataframes so unmatched firms are retained
    how = "outer",

    # Add merge source flag column to identify whether each row matched, was left-only, or right-only
    indicator = "merge_status",
    
    # Enforce one-to-one merge cardinality
    validate = "one_to_one"
)

# Assert only rows from formatted_firm_names_csv are unmatched
assert theta_estimates_race["merge_status"].isin(["both", "right_only"]).all()

# Drop the merge status column
theta_estimates_race = theta_estimates_race.drop(columns = "merge_status")

# Rename variables 
theta_estimates_race = theta_estimates_race.rename(columns = {"firm_code": "firm_name_aer_replication_package"})

# Reorder columns to put firm name, then firm id, then sic codes 
theta_estimates_race = theta_estimates_race[[
    "firm_name_aer_replication_package",
    "firm_id_aer_replication_package",
    "sic_code_two_digit_aer_replication_package",
    "sic_code_aer_two_digit_aggregated_aer_replication_package"
]]

# Export data into dump folder 
theta_estimates_race.to_csv(dump / "firm_industry_crosswalk_aer_replication_package.csv", index = False)