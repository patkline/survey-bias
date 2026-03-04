# ------------------------------------------------------------------------------
# Purpose: Merge aer replication package and RefUSA firm-industry
# crosswalks into one combined industry map.
#
# Created: Evan Rose
# Edited: Nico Rotundo 2026-03-03
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
from globals import dump, code, external, processed

# ------------------------------------------------------------------------------
# Import aer replication package and RefUSA firm-industry
# crosswalks and assign to dataframes
# ------------------------------------------------------------------------------
# Assign aer replication package firm-industry crosswalk csv to dataframe
firm_industry_crosswalk_aer_replication_package = pd.read_csv(dump / "firm_industry_crosswalk_aer_replication_package.csv")

# Assign RefUSA firm-industry crosswalk csv to dataframe
industry_map = pd.read_csv(dump / "firm_industry_crosswalk_refusa.csv")

# ------------------------------------------------------------------------------
# Build staged match keys for aer replication package and RefUSA names
# using one sequential loop over both datasets
# ------------------------------------------------------------------------------
# Define regex pattern that strips trailing legal/entity descriptor tokens
descriptor_suffix_pattern = r"(\s+(INC|INCORPORATED|CO|COMPANY|CORP|CORPORATION|LLC|LTD|PLC|HOLDINGS|HOLDING|GROUP|COS|THE|INTERNATIONAL|WORLDWIDE|GLOBAL|SERVICES|SERVICE|TECHNOLOGIES|TECHNOLOGY|SOLUTIONS|SYSTEMS|SYSTEM|COMMUNICATIONS|COMMUNICATION|NETWORKS|NETWORK|RETAIL|ENTERPRISES|ENTERPRISE|COM))+$"

# Initialize placeholder dataframe for staged aer replication package match keys
firm_name_match_keys_aer_replication_package = pd.DataFrame()

# Initialize placeholder dataframe for staged RefUSA match keys
firm_name_match_keys_refusa = pd.DataFrame()

# Loop over both crosswalk datasets and build staged match keys sequentially
for crosswalk_dataset in ["firm_industry_crosswalk_aer_replication_package", "industry_map"]:
    
    ## Set locals for current dataset in loop
    # Keep local dataframe variable for current dataset in loop
    if crosswalk_dataset == "firm_industry_crosswalk_aer_replication_package":
        firm_industry_crosswalk_dataset = firm_industry_crosswalk_aer_replication_package

    # Keep local dataframe variable for current dataset in loop
    if crosswalk_dataset == "industry_map":
        firm_industry_crosswalk_dataset = industry_map

    ## Set locals for firm_name_series for current dataset in loop
    # Set local firm_name_series to aer replication package firm-name variable
    if crosswalk_dataset == "firm_industry_crosswalk_aer_replication_package":
        firm_name_series = firm_industry_crosswalk_dataset["firm_name_aer_replication_package"]

    # Set local firm_name_series to RefUSA firm-name variable
    if crosswalk_dataset == "industry_map":
        firm_name_series = firm_industry_crosswalk_dataset["firm_clean"]

    ## Build staged match keys for current dataset in loop
    # Build uppercase firm-name key after trimming whitespace
    firm_name_merge_key_uppercase = firm_name_series.astype("string").str.strip().str.upper()

    # Build normalized firm-name key 
    firm_name_merge_key_normalized = (
        
        # Base variable 
        firm_name_merge_key_uppercase
        
        # Replace ampersands with " AND "
        .str.replace("&", " AND ", regex=False)
        
        # Replace all non-alphanumeric characters (except spaces) with a single space
        .str.replace(r"[^A-Z0-9 ]+", " ", regex=True)

        # Replace multiple consecutive spaces with a single space 
        .str.replace(r"\s+", " ", regex=True)

        # Trim leading and trailing whitespace again after replacements
        .str.strip()
    )

    # Build compact normalized key by removing all spaces
    firm_name_merge_key_normalized_compact = firm_name_merge_key_normalized.str.replace(" ", "", regex=False)

    # Build descriptor-stripped normalized key by removing trailing descriptor tokens
    firm_name_merge_key_normalized_descriptor = (
        
        # Base variable
        firm_name_merge_key_normalized
        
        # Remove trailing legal/entity descriptor tokens using regex pattern
        .str.replace(descriptor_suffix_pattern, "", regex=True)
        
        # Trim whitespace again after descriptor stripping
        .str.strip()
    )

    # Build compact descriptor-stripped key by removing all spaces
    firm_name_merge_key_normalized_descriptor_compact = (
        firm_name_merge_key_normalized_descriptor.str.replace(" ", "", regex=False)
    )

    # Build descriptor-stripped first-two-token key for deterministic token-prefix matching
    firm_name_merge_key_normalized_descriptor_first_two_tokens = (
        
        # Base variable
        firm_name_merge_key_normalized_descriptor
        
        # Keep only the first two tokens (if at least two tokens exist) for each firm name
        .str.split()
        
        # Join the first two tokens back into a string, or set to missing if fewer than two tokens exist
        .apply(lambda tokens: " ".join(tokens[:2]) if isinstance(tokens, list) and len(tokens) >= 2 else pd.NA)
    )

    # Build descriptor-stripped first-token key for deterministic token-prefix matching
    firm_name_merge_key_normalized_descriptor_first_one_token = (
        
        # Base variable
        firm_name_merge_key_normalized_descriptor
        
        # Keep only the first token (if at least one token exists) for each firm name
        .str.split()
        
        # Keep only the first token, or set to missing if no tokens exist
        .apply(lambda tokens: tokens[0] if isinstance(tokens, list) and len(tokens) >= 1 else pd.NA)
    )

    # Build staged firm-name match key dataset for current dataset in loop
    firm_name_match_keys_dataset = pd.DataFrame(
        {
            "firm_name_merge_key_uppercase": firm_name_merge_key_uppercase,
            "firm_name_merge_key_normalized": firm_name_merge_key_normalized,
            "firm_name_merge_key_normalized_compact": firm_name_merge_key_normalized_compact,
            "firm_name_merge_key_normalized_descriptor": firm_name_merge_key_normalized_descriptor,
            "firm_name_merge_key_normalized_descriptor_compact": firm_name_merge_key_normalized_descriptor_compact,
            "firm_name_merge_key_normalized_descriptor_first_two_tokens": firm_name_merge_key_normalized_descriptor_first_two_tokens,
            "firm_name_merge_key_normalized_descriptor_first_one_token": firm_name_merge_key_normalized_descriptor_first_one_token,
        }
    )

    # Store staged match keys for aer replication package dataset
    if crosswalk_dataset == "firm_industry_crosswalk_aer_replication_package":
        firm_name_match_keys_aer_replication_package = firm_name_match_keys_dataset

    # Store staged match keys for RefUSA dataset
    if crosswalk_dataset == "industry_map":
        firm_name_match_keys_refusa = firm_name_match_keys_dataset

# Assert aer replication package firm names are unique
assert not firm_industry_crosswalk_aer_replication_package["firm_name_aer_replication_package"].duplicated().any()

# Assert RefUSA firm names are unique
assert not industry_map["firm_clean"].duplicated().any()

# ------------------------------------------------------------------------------
# Match aer replication package rows onto RefUSA rows using staged
# deterministic matching keys with unique-key safeguards
# ------------------------------------------------------------------------------
# Store aer replication package columns to copy into industry map when matched
aer_replication_package_columns_to_copy = [
    "firm_name_aer_replication_package",
    "firm_id_aer_replication_package",
    "sic_code_two_digit_aer_replication_package",
    "sic_code_aer_two_digit_aggregated_aer_replication_package",
]

# Initialize aer replication package columns in industry map as missing
for aer_replication_package_column in aer_replication_package_columns_to_copy:
    # Add aer replication package column initialized to missing values
    industry_map[aer_replication_package_column] = pd.NA

# Initialize merge indicator column as right_only for all RefUSA rows
industry_map["merge_status"] = "right_only"

# Initialize merge-method column to record staged matching method used for matched rows
industry_map["merge_match_method"] = pd.NA

# Initialize merge-key-value column to record staged key value used for matched rows
industry_map["merge_match_key_value"] = pd.NA

# Define staged deterministic matching methods in order from strictest to loosest
merge_match_methods_and_key_columns = [
    ("uppercase_name", "firm_name_merge_key_uppercase"),
    ("normalized_name", "firm_name_merge_key_normalized"),
    ("normalized_compact_name", "firm_name_merge_key_normalized_compact"),
    ("normalized_descriptor_name", "firm_name_merge_key_normalized_descriptor"),
    ("normalized_descriptor_compact_name", "firm_name_merge_key_normalized_descriptor_compact"),
    ("normalized_descriptor_first_two_tokens_name", "firm_name_merge_key_normalized_descriptor_first_two_tokens"),
    ("normalized_descriptor_first_one_token_name", "firm_name_merge_key_normalized_descriptor_first_one_token"),
]

# Initialize list of RefUSA row indices still unmatched to aer replication package as all refusa rows to start 
refusa_row_indices_still_unmatched_to_aer_replication_package = list(
    industry_map.index
)

# Initialize set of aer replication package row indices already matched to RefUSA rows as empty to start
aer_replication_package_row_indices_already_matched_to_refusa = set()

# Loop over staged deterministic matching methods in order
for merge_match_method, merge_match_key_column in merge_match_methods_and_key_columns:
    # Keep aer replication package rows where current staged match key is non-missing and non-blank
    aer_replication_package_rows_with_non_missing_key = firm_name_match_keys_aer_replication_package[
        
        # Non-missing
        firm_name_match_keys_aer_replication_package[merge_match_key_column].notna()
        
        # Non-blank after stripping whitespace
        & (firm_name_match_keys_aer_replication_package[merge_match_key_column].astype("string").str.strip() != "")
    ]

    # Compute frequency of each staged match key in aer replication package rows
    aer_replication_package_key_frequency = aer_replication_package_rows_with_non_missing_key[
        merge_match_key_column
    ].value_counts(dropna=False)

    # Keep only aer replication package rows with unique staged match keys for deterministic one-to-one lookup
    aer_replication_package_rows_with_unique_key = aer_replication_package_rows_with_non_missing_key[
        aer_replication_package_rows_with_non_missing_key[merge_match_key_column]
        .map(aer_replication_package_key_frequency)
        .eq(1)
    ]

    # Build lookup from unique staged match key to aer replication package row index
    aer_replication_package_row_index_by_unique_key = pd.Series(
        aer_replication_package_rows_with_unique_key.index.to_numpy(),
        index=aer_replication_package_rows_with_unique_key[merge_match_key_column].to_numpy(),
    )

    # Initialize list collecting RefUSA row indices matched in this staged pass
    refusa_row_indices_matched_in_current_pass = []

    # Loop over RefUSA rows still unmatched to aer replication package
    for refusa_row_index in refusa_row_indices_still_unmatched_to_aer_replication_package:
        # Read current staged match key for this unmatched RefUSA row
        refusa_staged_match_key = firm_name_match_keys_refusa.at[refusa_row_index, merge_match_key_column]

        # Skip this RefUSA row when staged match key is missing or blank
        if pd.isna(refusa_staged_match_key) or str(refusa_staged_match_key).strip() == "":
            continue

        # Skip this RefUSA row when staged match key has no aer replication package lookup match
        if refusa_staged_match_key not in aer_replication_package_row_index_by_unique_key.index:
            continue

        # Read aer replication package row index matched by staged match key
        aer_replication_package_row_index = int(
            aer_replication_package_row_index_by_unique_key.loc[refusa_staged_match_key]
        )

        # Skip this staged match when aer replication package row is already matched to another RefUSA row
        if aer_replication_package_row_index in aer_replication_package_row_indices_already_matched_to_refusa:
            continue

        # Copy aer replication package columns into current matched RefUSA row
        for aer_replication_package_column in aer_replication_package_columns_to_copy:
            # Assign aer replication package value into matched industry-map row
            industry_map.at[
                refusa_row_index, aer_replication_package_column
            ] = firm_industry_crosswalk_aer_replication_package.at[
                aer_replication_package_row_index, aer_replication_package_column
            ]

        # Mark merge status as both for this newly matched row
        industry_map.at[refusa_row_index, "merge_status"] = "both"

        # Store staged merge method used for this newly matched row
        industry_map.at[refusa_row_index, "merge_match_method"] = (
            merge_match_method
        )

        # Store staged merge key value used for this newly matched row
        industry_map.at[refusa_row_index, "merge_match_key_value"] = (
            refusa_staged_match_key
        )

        # Track this RefUSA row as matched in current staged pass
        refusa_row_indices_matched_in_current_pass.append(refusa_row_index)

        # Track this aer replication package row as already matched
        aer_replication_package_row_indices_already_matched_to_refusa.add(
            aer_replication_package_row_index
        )

    # Update list of RefUSA rows still unmatched after current staged pass
    refusa_row_indices_still_unmatched_to_aer_replication_package = [
        refusa_row_index
        for refusa_row_index in refusa_row_indices_still_unmatched_to_aer_replication_package
        if refusa_row_index not in refusa_row_indices_matched_in_current_pass
    ]

# Assert merge status values are right_only or both i.e., that there are only unmatched rows from RefUSA and no unmatched rows from aer replication package
assert industry_map["merge_status"].isin(["right_only", "both"]).all()

# ------------------------------------------------------------------------------
# Keep and order output variables
# ------------------------------------------------------------------------------
# Drop aer replication package firm_id variable
industry_map = industry_map.drop(
    columns="firm_id_aer_replication_package"
)

# Order variables in output crosswalk
industry_map = industry_map[
    [
        # Firm names
        "firm_clean",
        "firm_name_aer_replication_package",

        # Merge variables re firm names
        "merge_match_method_refusa",
        "merge_match_key_value_refusa",
        "merge_match_method",
        "merge_match_key_value",

        # Two-digit SIC codes
        "sic_code_two_digit_aer_replication_package",
        "primary_sic_code_refusa_modal_two_digit",

        # Two-digit aggregated SIC codes
        "sic_code_aer_two_digit_aggregated_aer_replication_package",
        "primary_sic_code_refusa_modal_two_digit_aer_aggregation",

        # Merge variables
        "merge_status",

        # Other RefUSA variables
        "refusa_firm_match_key_type",
        "primary_sic_code_refusa_modal",
    ]
]

# ------------------------------------------------------------------------------
# Export merged firm-industry crosswalk diagnostics
# ------------------------------------------------------------------------------
# Output non-matching rows csv to scratch folder for inspection
industry_map[industry_map["merge_status"] != "both"].to_csv(code / "scratch_nico" / "temp_industry_map_non_matches.csv", index=False)

# Output rows that were matched but not with uppercase name to scratch folder for inspection
industry_map[
    (industry_map["merge_status"] == "both")
    & (industry_map["merge_match_method"] != "uppercase_name")
].to_csv(code / "scratch_nico" / "temp_industry_map_matches_non_uppercase.csv", index=False)

# Assert output still has one row per RefUSA firm_clean value
assert len(industry_map) == industry_map[
    "firm_clean"
].nunique()

# ------------------------------------------------------------------------------
# Export merged firm-industry crosswalk
# ------------------------------------------------------------------------------
# Write merged firm-industry crosswalk to dump folder
industry_map.to_csv(dump / "firm_industry_crosswalk_industry_map.csv", index=False)

# Report output write path and merge status counts
print(
    "\n 🎃 Wrote merged firm-industry crosswalk to "
    f"{dump / 'firm_industry_crosswalk_industry_map.csv'}" 
    "\n Merge counts"
    f"🧌 {industry_map['merge_status'].value_counts(dropna=False).to_dict()}."
)

# ------------------------------------------------------------------------------
# Export final industry map crosswalk as excel file to processed 
# folder with only necessary variables for use in analysis
# ------------------------------------------------------------------------------
# Generate harmonized two-digit SIC code variable prioritizing aer replication package two-digit SIC code when available
industry_map["sic_code_two_digit_harmonized"] = industry_map["sic_code_two_digit_aer_replication_package"].combine_first(
    industry_map["primary_sic_code_refusa_modal_two_digit"]
)

# Generate harmonized two-digit aggregated SIC code variable prioritizing aer replication package two-digit aggregated SIC code when available
industry_map["sic_code_aggregated_two_digit_harmonized"] = industry_map["sic_code_aer_two_digit_aggregated_aer_replication_package"].combine_first(
    industry_map["primary_sic_code_refusa_modal_two_digit_aer_aggregation"]
)

# Merge on new_numeric_sic from aer replication package as sic_code_aggregated_two_digit_harmonized_numeric_aer
# Keep harmonized aggregated SIC as string so keys match character values in sic_updates.csv
industry_map["sic_code_aggregated_two_digit_harmonized"] = pd.Series(
    industry_map["sic_code_aggregated_two_digit_harmonized"], dtype="string"
).str.strip().str.replace(r"\.0+$", "", regex=True)

# Build key-value lookup from aggregated SIC key to numeric SIC value
new_sic_numeric_by_new_sic = (
    pd.read_csv(external / "sic_updates.csv", usecols=["new_sic", "new_sic_numeric"])
    .assign(new_sic=lambda df: pd.Series(df["new_sic"], dtype="string").str.strip())
    .groupby("new_sic")["new_sic_numeric"]
    .first()
)

# Add numeric harmonized aggregated SIC by key lookup
industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"] = pd.to_numeric(
    industry_map["sic_code_aggregated_two_digit_harmonized"].map(new_sic_numeric_by_new_sic),
    errors="coerce",
)

## Merge on sic names from aer replication package as sic_code_aggregated_two_digit_harmonized_names_aer
# Build key-value lookup from numeric SIC key to SIC name
sic_name_by_sic_combined = (
    pd.read_csv(external / "formatted_sic_names.csv", usecols=["sic_combined", "sic_name"])
    .assign(sic_combined=lambda df: pd.to_numeric(df["sic_combined"], errors="coerce"))
    .groupby("sic_combined")["sic_name"]
    .first()
)

# Add harmonized aggregated SIC names by numeric SIC key lookup
industry_map["sic_code_aggregated_two_digit_harmonized_names_aer"] = (
    industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"].map(sic_name_by_sic_combined)
)

#  Subset industry map to necessary variables for analysis
industry_map = industry_map[
    [
        # Firm name 
        "firm_clean",

        # Harmonized SIC code variables
        "sic_code_two_digit_harmonized",
        "sic_code_aggregated_two_digit_harmonized",
        "sic_code_aggregated_two_digit_harmonized_numeric_aer",
        "sic_code_aggregated_two_digit_harmonized_names_aer"
    ]
]

# Write one version of industry map to the dump folder as a csv 
industry_map.to_csv(dump / "industry_map.csv", index=False)

# Write one version of industry map with just observations missing sic_code_aggregated_two_digit_harmonized_numeric_aer to my scratch folder as a csv for inspection
industry_map[industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"].isna()].to_csv(code / "scratch_nico" / "temp_industry_map_missing_harmonized_aggregated_sic_numeric.csv", index=False)

# Write one version of industry map to the processed folder as an excel file for use in analysis
industry_map.to_excel(processed / "industry_map.xlsx", index=False)
