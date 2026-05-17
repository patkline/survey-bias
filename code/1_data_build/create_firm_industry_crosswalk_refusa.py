# ------------------------------------------------------------------------------
# Purpose: Build a firm-by-SIC crosswalk from RefUSA and
# write industry_map.xlsx
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
from globals import processed, external, dump, code

# ------------------------------------------------------------------------------
# Import and prepare long survey data for crosswalk construction
# ------------------------------------------------------------------------------
# Import long_survey.csv
long_survey = pd.read_csv(processed / "long_survey.csv")

# Confirm ResponseId and option_number uniquely identifies observations i.e., unique values of response id x unique values of option number = total rows
assert len(long_survey) == long_survey[["ResponseId", "option_number"]].drop_duplicates().shape[0]

# Keep necessary variables i.e., firm_clean variable
long_survey = long_survey[["firm_clean"]].copy()

# Trim leading and trailing whitespace from firm_clean values
long_survey["firm_clean"] = long_survey["firm_clean"].astype("string").str.strip()

# Drop rows with missing or blank firm_clean values before uniqueness assertions
long_survey = long_survey[
    long_survey["firm_clean"].notna() & (long_survey["firm_clean"] != "")
].copy()

# Confirm firm_clean is never missing i.e., confirm that all rows have non-NA and non-blank firm_clean values
assert long_survey["firm_clean"].notna().all() and (long_survey["firm_clean"] != "").all()

# Keep one observation per unique firm_clean value to recover list of unique firms in our data, and keep only the firm_clean variable
long_survey = long_survey.drop_duplicates(subset=["firm_clean"]).reset_index(drop=True)

# Should be 164 unique firms uniquely identified off firm_clean (since we did not keep the missing value)
assert len(long_survey) == 164 and long_survey["firm_clean"].nunique() == 164

# ------------------------------------------------------------------------------
# Import RefUSA data for staged matching and SIC counting
# ------------------------------------------------------------------------------
# Read RefUSA columns needed for ABI uniqueness assertion and staged matching
refusa = pd.read_csv(
    # File 
    external / "2019_Business_Academic_QCQ.txt.gz",
    
    # Subset columns
    usecols=["Company", "Primary SIC Code", "ABI"],
    
    # Specify import data types for variables 
    dtype={"Company": "string", "Primary SIC Code": "string", "ABI": "string"},
    
    # Use latin1 encoding to avoid errors from invalid UTF-8 byte sequences in RefUSA data
    encoding="latin1",

    # Disable low-memory mode
    low_memory=False,
)

# Confirm ABI is unique across RefUSA rows
assert refusa["ABI"].astype("string").str.strip().nunique() == len(refusa)

# ------------------------------------------------------------------------------
# Build staged match keys for long_survey and RefUSA names
# using one sequential loop over both datasets
# ------------------------------------------------------------------------------
# Define regex pattern that strips trailing legal/entity descriptor tokens
descriptor_suffix_pattern = r"(\s+(INC|INCORPORATED|CO|COMPANY|CORP|CORPORATION|LLC|LTD|PLC|HOLDINGS|HOLDING|GROUP|COS|THE|INTERNATIONAL|WORLDWIDE|GLOBAL|SERVICES|SERVICE|TECHNOLOGIES|TECHNOLOGY|SOLUTIONS|SYSTEMS|SYSTEM|COMMUNICATIONS|COMMUNICATION|NETWORKS|NETWORK|RETAIL|ENTERPRISES|ENTERPRISE|COM))+$"

# Initialize placeholder dataframe for staged long_survey match keys
firm_name_match_keys_long_survey = pd.DataFrame()

# Initialize placeholder dataframe for staged RefUSA match keys
firm_name_match_keys_refusa = pd.DataFrame()

# Loop over long_survey and RefUSA datasets and build staged match keys sequentially
for crosswalk_dataset in ["long_survey", "refusa"]:
    
    ## Set locals for current dataset in loop
    # Keep local dataframe variable for current dataset in loop
    if crosswalk_dataset == "long_survey":
        firm_industry_crosswalk_dataset = long_survey

    # Keep local dataframe variable for current dataset in loop
    if crosswalk_dataset == "refusa":
        firm_industry_crosswalk_dataset = refusa

    ## Set locals for firm_name_series for current dataset in loop
    # Set local firm_name_series to long_survey firm-name variable
    if crosswalk_dataset == "long_survey":
        firm_name_series = firm_industry_crosswalk_dataset["firm_clean"]

    # Set local firm_name_series to RefUSA firm-name variable
    if crosswalk_dataset == "refusa":
        firm_name_series = firm_industry_crosswalk_dataset["Company"]

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

    # Store staged match keys for long_survey dataset
    if crosswalk_dataset == "long_survey":
        firm_name_match_keys_long_survey = firm_name_match_keys_dataset

    # Store staged match keys for RefUSA dataset
    if crosswalk_dataset == "refusa":
        firm_name_match_keys_refusa = firm_name_match_keys_dataset

# ------------------------------------------------------------------------------
# Match RefUSA rows onto long_survey rows using staged deterministic matching
# and accumulate firm-level match-method counts and firm-by-SIC counts
# ------------------------------------------------------------------------------
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

# Keep long_survey firm names as string series for staged lookup outputs and final row-order alignment
long_survey_firm_clean = long_survey["firm_clean"].astype("string")

# Initialize firm-by-method match count table with one row per firm_clean value
firm_clean_match_count_by_method = pd.DataFrame(
    0,
    index=long_survey_firm_clean.to_numpy(),
    columns=[merge_match_method for merge_match_method, _ in merge_match_methods_and_key_columns],
    dtype="int64",
)

# Initialize dictionary keyed by (firm_clean, raw SIC code) storing global counts
primary_sic_code_count_by_firm_and_sic = {}

# Initialize dictionary storing one unique-key lookup series per staged matching method
firm_clean_by_unique_key_lookup_by_match_method = {}

# Loop over staged methods and build unique-key lookup from staged key to firm_clean
for merge_match_method, merge_match_key_column in merge_match_methods_and_key_columns:
    # Keep long_survey rows with non-missing and non-blank staged key
    long_survey_rows_with_non_missing_key = firm_name_match_keys_long_survey[
        firm_name_match_keys_long_survey[merge_match_key_column].notna()
        & (firm_name_match_keys_long_survey[merge_match_key_column].astype("string").str.strip() != "")
    ]

    # Compute staged key frequency among long_survey rows with non-missing staged keys
    long_survey_key_frequency = long_survey_rows_with_non_missing_key[
        merge_match_key_column
    ].value_counts(dropna=False)

    # Keep only long_survey rows where staged key is unique for deterministic lookup
    long_survey_rows_with_unique_key = long_survey_rows_with_non_missing_key[
        long_survey_rows_with_non_missing_key[merge_match_key_column]
        .map(long_survey_key_frequency)
        .eq(1)
    ]

    # Build lookup from unique staged key value to firm_clean value
    firm_clean_by_unique_key_lookup_by_match_method[merge_match_method] = pd.Series(
        long_survey_firm_clean.loc[long_survey_rows_with_unique_key.index].to_numpy(),
        index=long_survey_rows_with_unique_key[merge_match_key_column].to_numpy(),
    )

# Keep raw RefUSA primary SIC code values and trim whitespace
primary_sic_code_refusa = refusa["Primary SIC Code"].astype("string").str.strip()

# Mark rows that have valid company key and valid classified Primary SIC Code values
row_is_valid_for_matching_and_sic_count = (
    firm_name_match_keys_refusa["firm_name_merge_key_uppercase"].notna()
    & (firm_name_match_keys_refusa["firm_name_merge_key_uppercase"] != "")
    & primary_sic_code_refusa.notna()
    & (primary_sic_code_refusa != "")
    & ~primary_sic_code_refusa.str.startswith("9999", na=False)
)

# Keep staged RefUSA company keys for valid rows only and reset row index for positional matching updates
refusa_staged_match_keys = firm_name_match_keys_refusa.loc[
    row_is_valid_for_matching_and_sic_count
].reset_index(drop=True)

# Keep raw SIC values for valid rows only and reset row index for positional alignment to match results
primary_sic_code_refusa = primary_sic_code_refusa.loc[
    row_is_valid_for_matching_and_sic_count
].reset_index(drop=True)

# Initialize matched firm_clean vector for valid RefUSA rows
matched_firm_clean = pd.Series(pd.NA, index=refusa_staged_match_keys.index, dtype="string")

# Initialize matched-method vector for valid RefUSA rows
matched_merge_match_method = pd.Series(
    pd.NA, index=refusa_staged_match_keys.index, dtype="string"
)

# Initialize matched-key-value vector for valid RefUSA rows
matched_merge_match_key_value = pd.Series(
    pd.NA, index=refusa_staged_match_keys.index, dtype="string"
)

# Loop over staged deterministic matching methods in order and fill unmatched rows
for merge_match_method, merge_match_key_column in merge_match_methods_and_key_columns:
    # Mark rows that are still unmatched before this staged pass
    row_is_still_unmatched = matched_firm_clean.isna()

    # Break staged loop when all valid RefUSA rows are already matched
    if not row_is_still_unmatched.any():
        break

    # Read staged keys for rows still unmatched before this staged pass
    refusa_staged_match_key = refusa_staged_match_keys.loc[
        row_is_still_unmatched, merge_match_key_column
    ]

    # Lookup firm_clean matches using staged unique-key lookup for current pass
    matched_firm_clean_from_lookup = refusa_staged_match_key.map(
        firm_clean_by_unique_key_lookup_by_match_method[merge_match_method]
    )

    # Mark rows successfully matched in this staged pass
    row_matched_in_current_pass = matched_firm_clean_from_lookup.notna()

    # Update matched vectors for rows successfully matched in this staged pass
    if row_matched_in_current_pass.any():
        # Keep indices of rows matched in current staged pass
        matched_row_indices = matched_firm_clean_from_lookup.index[
            row_matched_in_current_pass
        ]

        # Assign matched firm_clean values for rows matched in current staged pass
        matched_firm_clean.loc[matched_row_indices] = (
            matched_firm_clean_from_lookup.loc[
                row_matched_in_current_pass
            ].astype("string")
        )

        # Assign staged match method label for rows matched in current staged pass
        matched_merge_match_method.loc[matched_row_indices] = merge_match_method

        # Assign staged match key value for rows matched in current staged pass
        matched_merge_match_key_value.loc[matched_row_indices] = refusa_staged_match_key.loc[
            matched_row_indices
        ].astype("string")

# Mark rows successfully matched to firm_clean
row_successfully_matched_to_firm_clean = matched_firm_clean.notna()

# Keep matched rows in one dataframe for match-count and SIC-count updates
refusa_matched_rows = pd.DataFrame(
    {
        "firm_clean": matched_firm_clean.loc[
            row_successfully_matched_to_firm_clean
        ].astype("string"),
        "merge_match_method": matched_merge_match_method.loc[
            row_successfully_matched_to_firm_clean
        ].astype("string"),
        "merge_match_key_value": matched_merge_match_key_value.loc[
            row_successfully_matched_to_firm_clean
        ].astype("string"),
        "primary_sic_code_refusa": primary_sic_code_refusa.loc[
            row_successfully_matched_to_firm_clean
        ].astype("string"),
    }
)

# Build firm-by-method match count table
match_count_by_firm_and_method = (
    refusa_matched_rows.groupby(["firm_clean", "merge_match_method"], dropna=False)
    .size()
    .rename("match_count")
    .reset_index()
)

# Add firm-by-method counts into global firm-level match count table
for row in match_count_by_firm_and_method.itertuples(index=False):
    firm_clean_match_count_by_method.at[row.firm_clean, row.merge_match_method] += int(
        row.match_count
    )

# Build firm-by-SIC count table
firm_by_primary_sic_count_table = (
    refusa_matched_rows.groupby(["firm_clean", "primary_sic_code_refusa"], dropna=False)
    .size()
    .rename("primary_sic_code_count")
    .reset_index()
)

# Add firm-by-SIC counts into global firm-by-SIC count dictionary
for row in firm_by_primary_sic_count_table.itertuples(index=False):
    # Define dictionary key for this firm-by-SIC row
    firm_by_sic_key = (row.firm_clean, row.primary_sic_code_refusa)

    # Read prior global count for this firm-by-SIC key, defaulting to zero
    prior_count = primary_sic_code_count_by_firm_and_sic.get(firm_by_sic_key, 0)

    # Update global count for this firm-by-SIC key
    primary_sic_code_count_by_firm_and_sic[firm_by_sic_key] = (
        prior_count + int(row.primary_sic_code_count)
    )

# ------------------------------------------------------------------------------
# Build per-firm merge method and merge-key-value labels
# using staged-method precedence
# ------------------------------------------------------------------------------
# Initialize per-firm merge method labels as missing values
merge_match_method_by_firm_clean = pd.Series(
    pd.NA, index=long_survey_firm_clean.to_numpy(), dtype="string"
)

# Fill per-firm merge method labels using staged precedence order
for merge_match_method, _ in merge_match_methods_and_key_columns:
    # Keep firms that are unlabeled before this staged method
    firm_is_unlabeled = merge_match_method_by_firm_clean.isna()

    # Keep firms that have at least one RefUSA match in this staged method
    firm_has_match_in_current_method = (
        firm_clean_match_count_by_method[merge_match_method] > 0
    )

    # Assign current staged method label to firms unlabeled so far and matched in this method
    merge_match_method_by_firm_clean.loc[
        firm_is_unlabeled & firm_has_match_in_current_method
    ] = merge_match_method

# Build vector of firm-level merge method labels aligned to long_survey row order
merge_match_method = long_survey_firm_clean.map(
    merge_match_method_by_firm_clean
).astype("string")

# Build firm-by-method-by-key-value count table
match_count_by_firm_method_and_key_value = (
    refusa_matched_rows.groupby(
        ["firm_clean", "merge_match_method", "merge_match_key_value"], dropna=False
    )
    .size()
    .rename("match_count")
    .reset_index()
)

# Build one-row-per-firm table storing staged merge method selected by precedence
merge_method_selected_by_firm = pd.DataFrame(
    {
        "firm_clean": merge_match_method_by_firm_clean.index.astype("string"),
        "merge_match_method": merge_match_method_by_firm_clean.to_numpy(),
    }
)

# Keep key-value counts for each firm's selected merge method
match_count_by_selected_method = merge_method_selected_by_firm.merge(
    match_count_by_firm_method_and_key_value,
    on=["firm_clean", "merge_match_method"],
    how="left",
)

# Keep one deterministic merge_key_value per firm by taking highest-count key value,
# breaking ties by smallest lexical key value
merge_match_key_value_by_firm_clean = (
    match_count_by_selected_method.sort_values(
        ["firm_clean", "match_count", "merge_match_key_value"],
        ascending=[True, False, True],
        na_position="last",
    )
    .drop_duplicates(subset=["firm_clean"], keep="first")
    .set_index("firm_clean")["merge_match_key_value"]
    .astype("string")
)

# Build vector of firm-level merge key values aligned to long_survey row order
merge_match_key_value = long_survey_firm_clean.map(
    merge_match_key_value_by_firm_clean
).astype("string")

# Build per-firm merge key type labels as alias of merge_match_method for compatibility
refusa_firm_match_key_type = merge_match_method.copy()

# ------------------------------------------------------------------------------
# Convert global firm-by-SIC count dictionary into dataframe and collapse to
# one modal raw Primary SIC code per firm_clean value
# ------------------------------------------------------------------------------
# Convert global firm-by-SIC count dictionary to dataframe
if len(primary_sic_code_count_by_firm_and_sic) == 0:
    # Initialize empty dataframe when no firm-by-SIC matches were observed
    primary_sic_code_refusa_count_by_firm = pd.DataFrame(
        columns=["firm_clean", "primary_sic_code_refusa", "primary_sic_code_count"]
    )
else:
    # Convert firm-by-SIC dictionary entries to one row per firm_clean x raw SIC value
    primary_sic_code_refusa_count_by_firm = pd.DataFrame(
        [
            {
                "firm_clean": firm_clean_value,
                "primary_sic_code_refusa": primary_sic_code_value,
                "primary_sic_code_count": primary_sic_code_count_value,
            }
            for (firm_clean_value, primary_sic_code_value), primary_sic_code_count_value in primary_sic_code_count_by_firm_and_sic.items()
        ]
    )

# Assert RefUSA primary SIC values are numeric strings when any rows are present
if len(primary_sic_code_refusa_count_by_firm) > 0:
    assert primary_sic_code_refusa_count_by_firm["primary_sic_code_refusa"].str.fullmatch(
        r"[0-9]+"
    ).all()

# Output firm-by-SIC count table to scratch folder for inspection
primary_sic_code_refusa_count_by_firm.to_csv(code / "scratch_nico" / "temp_refusa_firm_by_sic_counts.csv", index=False)

# Count number of firms where ties in modal SIC code are being broken
if len(primary_sic_code_refusa_count_by_firm) == 0:
    # Keep tie count as zero when there are no matched firm-by-SIC rows
    primary_sic_code_refusa_modal_tie_count = 0

    # Initialize empty modal-by-firm dataframe
    primary_sic_code_refusa_modal_by_firm = pd.DataFrame(
        columns=["firm_clean", "primary_sic_code_refusa"]
    )
else:
    # Compute max SIC frequency per firm for modal candidate filtering
    max_sic_count_by_firm = primary_sic_code_refusa_count_by_firm.groupby("firm_clean")[
        "primary_sic_code_count"
    ].transform("max")

    # Keep SIC rows tied at max count per firm as modal candidates
    primary_sic_code_refusa_modal_candidates = primary_sic_code_refusa_count_by_firm.loc[
        primary_sic_code_refusa_count_by_firm["primary_sic_code_count"]
        == max_sic_count_by_firm
    ].copy()

    # Count firms with more than one modal SIC candidate
    primary_sic_code_refusa_modal_tie_count = int(
        (
            primary_sic_code_refusa_modal_candidates.groupby("firm_clean")
            .size()
            .gt(1)
        ).sum()
    )

    # Convert modal candidate SIC strings to numeric for deterministic tie-breaking on smallest SIC value
    primary_sic_code_refusa_modal_candidates["primary_sic_code_refusa_numeric"] = (
        pd.to_numeric(
            primary_sic_code_refusa_modal_candidates["primary_sic_code_refusa"],
            errors="coerce",
        )
    )

    # Collapse to one modal SIC row per firm by keeping smallest numeric SIC value among modal candidates
    primary_sic_code_refusa_modal_by_firm = (
        primary_sic_code_refusa_modal_candidates.sort_values(
            ["firm_clean", "primary_sic_code_refusa_numeric"]
        )
        .drop_duplicates(subset=["firm_clean"], keep="first")
        .loc[:, ["firm_clean", "primary_sic_code_refusa"]]
    )

# Report number of firm_clean values where tie-breaking is required for modal SIC code selection
print(
    f"Modal primary_sic_code_refusa tie-break applied for {primary_sic_code_refusa_modal_tie_count} firm_clean values."
)

# Merge modal raw SIC values onto long_survey firm list to keep long_survey row order and unmatched firms as missing
primary_sic_code_refusa_modal = (
    long_survey.loc[:, ["firm_clean"]]
    .merge(primary_sic_code_refusa_modal_by_firm, on="firm_clean", how="left")
    .loc[:, "primary_sic_code_refusa"]
    .astype("string")
)

# Parse two-digit SIC from modal raw RefUSA SIC values
primary_sic_code_refusa_modal_two_digit = pd.to_numeric(
    pd.Series(primary_sic_code_refusa_modal, dtype="string").str.slice(0, 2),
    errors="coerce",
).astype("Int64")

# Keep only valid two-digit SIC values in 1..99
primary_sic_code_refusa_modal_two_digit = primary_sic_code_refusa_modal_two_digit.where(
    primary_sic_code_refusa_modal_two_digit.between(1, 99),
    pd.NA,
)

# Aggregate SIC bins to match required AER-style bucket rules.
primary_sic_code_refusa_modal_two_digit_aer_aggregation = pd.Series(
    pd.NA,
    index=primary_sic_code_refusa_modal_two_digit.index,
    dtype="string",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.where(
    primary_sic_code_refusa_modal_two_digit.notna(),
    pd.NA,
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(24, 35).fillna(False),
    "24-35",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(42, 47).fillna(False),
    "42-47",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(50, 51).fillna(False),
    "50-51",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(61, 64).fillna(False),
    "61-64",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(65, 70).fillna(False),
    "65-70",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(72, 73).fillna(False),
    "72-73",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(75, 76).fillna(False),
    "75-76",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.between(80, 87).fillna(False),
    "80-87",
)
primary_sic_code_refusa_modal_two_digit_aer_aggregation = primary_sic_code_refusa_modal_two_digit_aer_aggregation.mask(
    primary_sic_code_refusa_modal_two_digit.notna()
    & primary_sic_code_refusa_modal_two_digit_aer_aggregation.isna(),
    primary_sic_code_refusa_modal_two_digit.astype("Int64").astype("string"),
)

# ------------------------------------------------------------------------------
# Keep and order output variables
# ------------------------------------------------------------------------------
# Build final firm_industry_crosswalk_refusa directly from long_survey, dropping extraneous variables and adding modal sic variables constructed above
firm_industry_crosswalk_refusa = long_survey.assign(
    merge_match_method_refusa=merge_match_method,
    merge_match_key_value_refusa=merge_match_key_value,
    refusa_firm_match_key_type=refusa_firm_match_key_type,
    primary_sic_code_refusa_modal=primary_sic_code_refusa_modal,
    primary_sic_code_refusa_modal_two_digit=primary_sic_code_refusa_modal_two_digit.astype(
        "Float64"
    ),
    primary_sic_code_refusa_modal_two_digit_aer_aggregation=primary_sic_code_refusa_modal_two_digit_aer_aggregation.astype(
        "string"
    ),
)[ 
    [
        # Firm names
        "firm_clean",

        # Merge variables re firm names
        "merge_match_method_refusa",
        "merge_match_key_value_refusa",

        # Two-digit SIC codes
        "primary_sic_code_refusa_modal_two_digit",

        # Two-digit aggregated SIC codes
        "primary_sic_code_refusa_modal_two_digit_aer_aggregation",

        # Other RefUSA variables
        "refusa_firm_match_key_type",
        "primary_sic_code_refusa_modal",
    ]
]

# Check that firm_industry_crosswalk_refusa is unique on firm_clean
assert firm_industry_crosswalk_refusa["firm_clean"].nunique() == len(firm_industry_crosswalk_refusa)

# ------------------------------------------------------------------------------
# Export firm-industry crosswalk diagnostics
# ------------------------------------------------------------------------------
# Output non-matching firms csv to scratch folder for inspection
firm_industry_crosswalk_refusa[firm_industry_crosswalk_refusa["primary_sic_code_refusa_modal_two_digit"].isna()].to_csv(
    code / "scratch_nico" / "temp_refusa_non_matches.csv", index=False
)

# Output matched firms that were not matched on uppercase_name to scratch folder for inspection
firm_industry_crosswalk_refusa[
    firm_industry_crosswalk_refusa["merge_match_method_refusa"].notna()
    & (firm_industry_crosswalk_refusa["merge_match_method_refusa"] != "uppercase_name")
].to_csv(code / "scratch_nico" / "temp_refusa_matches_non_uppercase.csv", index=False)

# ------------------------------------------------------------------------------
# Export firm-industry crosswalk
# /dump/firm_industry_crosswalk_refusa.csv
# ------------------------------------------------------------------------------
# Write updated crosswalk to dump/firm_industry_crosswalk_refusa.csv
firm_industry_crosswalk_refusa.to_csv(dump / "firm_industry_crosswalk_refusa.csv", index=False)

# Report output write path and match coverage
print(
    "🎃 Wrote updated industry map to data/dump/firm_industry_crosswalk_refusa.csv with "
    f"{firm_industry_crosswalk_refusa['primary_sic_code_refusa_modal_two_digit'].notna().sum()}/{len(firm_industry_crosswalk_refusa)} matched firms."
)
