# ------------------------------------------------------------------------------
# Purpose: Merge archived industry_map.xlsx aer_naics2 values onto
# current industry_map.csv by firm name and assert alignment with
# sic_code_aggregated_two_digit_harmonized_numeric_aer for all rows
# where aer_naics2 is non-missing
#
# Created: Nico Rotundo 2026-03-08
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
from globals import data, dump, code

# ------------------------------------------------------------------------------
# Read archive and current industry map files
# ------------------------------------------------------------------------------
# Read archived industry map with XML fallback support
archive_industry_map = pd.read_excel(data / "archive" / "industry_map.xlsx", engine="openpyxl" )

# Trim archived firm names and drop missing/blank firm values
archive_industry_map["firm"] = archive_industry_map["firm"].astype("string").str.strip()
archive_industry_map = archive_industry_map[
    archive_industry_map["firm"].notna() & (archive_industry_map["firm"] != "")
].copy()

# Coerce archived aer_naics2 to numeric so blank strings become missing
archive_industry_map["aer_naics2"] = pd.to_numeric(
    archive_industry_map["aer_naics2"], errors="coerce"
)

# Define current industry map csv path
industry_map_csv_path = dump / "industry_map.csv"

# Read current industry map csv
industry_map = pd.read_csv(industry_map_csv_path)


# ------------------------------------------------------------------------------
# Build uppercase firm-name keys and merge archived aer_naics2
# ------------------------------------------------------------------------------
# Build uppercase firm-name key in archived industry map
archive_industry_map["firm_name_uppercase"] = (
    archive_industry_map["firm"].astype("string").str.strip().str.upper()
)

# Build uppercase firm-name key in current industry map
industry_map["firm_name_uppercase"] = (
    industry_map["firm_clean"].astype("string").str.strip().str.upper()
)

# Assert uppercase key is unique in archived industry map
assert not archive_industry_map["firm_name_uppercase"].duplicated().any()

# Assert uppercase key is unique in current industry map
assert not industry_map["firm_name_uppercase"].duplicated().any()

# Merge archived aer_naics2 onto current industry map by uppercase firm-name key
industry_map = industry_map.merge(
    archive_industry_map[["firm_name_uppercase", "aer_naics2"]],
    on="firm_name_uppercase",
    how="left",
    validate="one_to_one",
)


# ------------------------------------------------------------------------------
# Assert alignment of aer_naics2 and harmonized aggregated SIC numeric
# ------------------------------------------------------------------------------
# Keep rows where archived aer_naics2 is non-missing
rows_with_non_missing_aer_naics2 = industry_map["aer_naics2"].notna()

# Assert merged non-missing aer_naics2 count equals archived non-missing count
assert rows_with_non_missing_aer_naics2.sum() == archive_industry_map[
    "aer_naics2"
].notna().sum()

# Assert non-missing aer_naics2 equals harmonized aggregated SIC numeric for all rows
assert pd.to_numeric(
    industry_map.loc[rows_with_non_missing_aer_naics2, "aer_naics2"],
    errors="coerce",
).astype("Int64").eq(
    pd.to_numeric(
        industry_map.loc[
            rows_with_non_missing_aer_naics2,
            "sic_code_aggregated_two_digit_harmonized_numeric_aer",
        ],
        errors="coerce",
    ).astype("Int64")
).all()

# Export merged industry map with archived aer_naics2 to csv in scratch folder for inspection
industry_map.to_csv(code / "scratch_nico" / "industry_map_with_archived_aer_naics2.csv", index=False)

# ------------------------------------------------------------------------------
# Report successful alignment check
# ------------------------------------------------------------------------------
# Print alignment success summary
print(
    "✅ Verified aer_naics2 alignment for "
    f"{int(rows_with_non_missing_aer_naics2.sum())} firms with non-missing aer_naics2."
)
