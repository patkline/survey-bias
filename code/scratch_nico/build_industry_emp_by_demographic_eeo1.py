# ==============================================================================
# DEPRECATED 2026-04-24: Ported to Stata as
# code/data_build/build_industry_emp_by_demographic_eeo1.do.
# Key differences from this Python version:
#   - Stata .do outputs at (naics3 x black_indicator x sex) grain; no 19-bucket
#     sic_category aggregation or naics3->sic_category manual resolutions.
#   - sic_code_aggregated_two_digit_harmonized_numeric_aer column + CPS-vs-EEO-1
#     diagnostic are deferred (tasks #13 + #14).
# This file kept for reference only --- not in the metafile pipeline.
# ==============================================================================

# ------------------------------------------------------------------------------
# Purpose: Clean EEO-1 2023 Public Use File and aggregate to the industry x race x sex
# level. Maps NAICS3 sectors to our 19 SIC categories using the ind1990 crosswalk
# (built by create_ind1990_crosswalks.py) plus explicit manual resolutions for the
# NAICS3 codes that are ambiguous or absent in that crosswalk.
#
# Reads from:
#   {external}/EEO1_2023_PUF.xlsx            — EEO-1 2023 Public Use File
#   {dump}/ind1990_crosswalks.csv            — ind1990 -> sic_category, naics3, naics2
#                                              (used to derive unambiguous naics3 -> sic_category mappings)
#
# Writes to:
#   {dump}/industry_emp_by_demographic_eeo1.csv  — long format, one row per
#                                                  (sic_category, black_indicator, sex) with EEO-1
#                                                  employment headcount summed across NAICS3 sectors.
#                                                  EEO-1 has no wage data, so "emp" (not "emp_wage") in
#                                                  the filename reflects the file's actual contents.
#
# Created: Nico Rotundo 2026-04-17
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
from globals import external, dump

# Import tabulate_variable function to tabulate variable distributions in diagnostics
from tools.define_tabulate_variable import tabulate_variable

# ------------------------------------------------------------------------------
# Load EEO-1 strictly-national NAICS3 rows
# ------------------------------------------------------------------------------
# The EEO-1 PUF encodes employment at {RACE}{SEX}{JOB_CAT} across many geographic aggregation levels.
# We load only the NAICS3 + race-sex-total columns ("10" suffix = total across job categories) for
# speed. Race-sex columns are read as strings because sub-national rows contain "*" suppression markers
# that break numeric parsing; we coerce to int after filtering to strictly-national rows (verified to
# contain only numeric values in the 2023 PUF).
eeo1_race_sex_total_columns = [
    # Male
    "WHM10", "BLKM10", "HISPM10", "ASIANM10", "AIANM10", "NHOPIM10", "TOMRM10",
    # Female
    "WHF10", "BLKF10", "HISPF10", "ASIANF10", "AIANF10", "NHOPIF10", "TOMRF10",
]
eeo1 = pd.read_excel(
    external / "EEO1_2023_PUF.xlsx",
    usecols=["Region", "Division", "State", "CBSA", "County", "NAICS3", "NAICS3_Name"] + eeo1_race_sex_total_columns,
    dtype={column: str for column in eeo1_race_sex_total_columns},
)

# Lowercase all column names for consistency with the rest of the project
eeo1.columns = eeo1.columns.str.lower()

# Keep strictly-national NAICS3 subtotals only:
#   - Region/Division/State/CBSA/County all null => rolled up to the national level (no sub-national duplication)
#   - NAICS3 non-null => NAICS3 detail rows (more granular than NAICS2 subtotals; avoids retail/mfg/info ambiguity)
eeo1 = eeo1[
    eeo1["region"].isna()
    & eeo1["division"].isna()
    & eeo1["state"].isna()
    & eeo1["cbsa"].isna()
    & eeo1["county"].isna()
    & eeo1["naics3"].notna()
].copy()

# Cast naics3 to int (float on load because of NaN in non-national rows)
eeo1["naics3"] = eeo1["naics3"].astype(int)

# Coerce race-sex columns from string to Int64 now that sub-national "*" suppression rows are filtered out;
# raise if any non-numeric values remain (would indicate the suppression pattern changed at the national level)
for race_sex_column in [column.lower() for column in eeo1_race_sex_total_columns]:
    numeric_values = pd.to_numeric(eeo1[race_sex_column], errors="coerce")
    assert numeric_values.notna().all(), \
        f"🪦 Non-numeric values in strictly-national EEO-1 column {race_sex_column} (unexpected suppression at national level)"
    eeo1[race_sex_column] = numeric_values.astype(int)

# Assert exactly one row per NAICS3 at this point (sanity: the strict-national filter should collapse to unique NAICS3)
assert not eeo1["naics3"].duplicated().any(), "Duplicated NAICS3 in strictly-national EEO-1 subset"

print(f"🎃 Loaded EEO-1 strictly-national NAICS3 rows: {len(eeo1):,} ({eeo1['naics3'].nunique()} unique NAICS3 codes)")

# ------------------------------------------------------------------------------
# Collapse race-sex totals into Black vs Not Black x male vs female and reshape to long
# ------------------------------------------------------------------------------
# Collapse the 14 race-sex-total columns into 4 group totals matching the CPS race/sex definitions.
# Not Black = every non-Black race column summed (WH, HISP, ASIAN, AIAN, NHOPI, TOMR).
not_black_male_columns = ["whm10", "hispm10", "asianm10", "aianm10", "nhopim10", "tomrm10"]
not_black_female_columns = ["whf10", "hispf10", "asianf10", "aianf10", "nhopif10", "tomrf10"]

eeo1["emp_black_male"]     = eeo1["blkm10"]
eeo1["emp_black_female"]   = eeo1["blkf10"]
eeo1["emp_notblack_male"]   = eeo1[not_black_male_columns].sum(axis=1)
eeo1["emp_notblack_female"] = eeo1[not_black_female_columns].sum(axis=1)

# Keep NAICS3 and the 4 demographic employment columns; drop raw race-sex columns
eeo1 = eeo1[[
    "naics3", "naics3_name",
    "emp_black_male", "emp_black_female",
    "emp_notblack_male", "emp_notblack_female",
]]

# Reshape to long: one row per (naics3, demographic) with employment. The demographic key is the
# column suffix (e.g., "black_male") which we split below.
eeo1 = eeo1.melt(
    id_vars=["naics3", "naics3_name"],
    value_vars=["emp_black_male", "emp_black_female", "emp_notblack_male", "emp_notblack_female"],
    var_name="_demographic_key",
    value_name="employment_eeo1",
)

# Parse race and sex from the column name via regex
demographic_parsed = eeo1["_demographic_key"].str.extract(
    r"emp_(?P<race>black|notblack)_(?P<sex>male|female)"
)
eeo1["black_indicator"] = demographic_parsed["race"].map({"black": "Black", "notblack": "Not Black"})
eeo1["sex"] = demographic_parsed["sex"]

# Drop the parse-helper column
eeo1 = eeo1.drop(columns=["_demographic_key"])

# Assert the reshape produced the expected cell count: 88 NAICS3 × 2 race × 2 sex = 352 rows
assert len(eeo1) == eeo1["naics3"].nunique() * 2 * 2, \
    f"Unexpected EEO-1 long-format row count: {len(eeo1)} (expected {eeo1['naics3'].nunique() * 2 * 2})"

print(f"🎃 Reshaped EEO-1 to long format: {len(eeo1):,} rows")

# ------------------------------------------------------------------------------
# Build NAICS3 -> SIC category lookup
# ------------------------------------------------------------------------------
# Start with unambiguous naics3 -> sic_category mappings derived from the ind1990 crosswalk
# (keep only naics3 codes where every ind1990 sharing that naics3 agrees on the sic_category).
naics3_to_sic_category_from_crosswalks = (
    pd.read_csv(dump / "ind1990_crosswalks.csv")
    .dropna(subset=["naics3", "sic_category"])
    .groupby("naics3")["sic_category"]
    .apply(lambda naics3_group: int(naics3_group.iloc[0]) if naics3_group.nunique() == 1 else None)
    .dropna()
    .astype(int)
    .to_dict()
)

# Manual resolutions for the two cases the crosswalk-derived lookup can't cover on its own:
#   (a) Ambiguous in the ind1990 crosswalk (multiple sic_category values share one naics3)
#   (b) Present in the EEO-1 2023 PUF but absent from the ind1990 crosswalk (primarily 2022 NAICS
#       reorganization — retail 44X/45X expansion and information 513/516 split)
# Values of `None` indicate the NAICS3 falls outside our 19 SIC categories and should drop from the output.
naics3_to_sic_category_manual_resolutions = {
    # ---- (a) Ambiguous in the ind1990 crosswalk (manual pick by industry semantics) ----
    311: 20,    # Food Manufacturing -> Food products
    445: 54,    # Food and Beverage Retailers -> Food stores
    448: 56,    # Clothing Retailers (2012 NAICS naming) -> Apparel stores
    451: 59,    # Sporting Goods/Hobby/Book/Music Retailers -> Other retail
    453: 59,    # Miscellaneous Store Retailers -> Other retail
    541: 72,    # Professional/Scientific/Technical Services -> Personal/business services
    811: 75,    # Repair and Maintenance -> Repair services
    # ---- (b) In EEO-1 but not in ind1990 crosswalk ----
    # Drops: industries whose underlying SIC codes are outside our 19 categories
    # (primary sector, construction, textile/apparel mfg, motion pictures, entertainment, private households).
    # These would-be `None` entries make the drop explicit and give the assertion block something to match on.
    111: None,  # Crop Production (SIC 01)
    112: None,  # Animal Production and Aquaculture (SIC 02)
    114: None,  # Fishing, Hunting and Trapping (SIC 09)
    115: None,  # Support Activities for Agriculture and Forestry (SIC 07)
    211: None,  # Oil and Gas Extraction (SIC 13)
    212: None,  # Mining, except Oil and Gas (SIC 10, 12, 14)
    213: None,  # Support Activities for Mining
    236: None,  # Construction of Buildings (SIC 15)
    237: None,  # Heavy and Civil Engineering Construction (SIC 16)
    238: None,  # Specialty Trade Contractors (SIC 17)
    313: None,  # Textile Mills (SIC 22)
    314: None,  # Textile Product Mills (SIC 22)
    315: None,  # Apparel Manufacturing (SIC 23)
    512: None,  # Motion Picture and Sound Recording Industries (SIC 78)
    711: None,  # Performing Arts, Spectator Sports (SIC 79)
    713: None,  # Amusement, Gambling, and Recreation Industries (SIC 79)
    814: None,  # Private Households (SIC 88)
    # Manufacturing (SIC 24-35 range)
    323: 24,    # Printing and Related Support Activities (SIC 27) -> Manufacturing
    # Wholesale trade
    425: 50,    # Wholesale Electronic Markets and Agents and Brokers -> Wholesale trade
    # Retail (NAICS 2022 44X/45X expansion)
    449: 57,    # Furniture, Home Furnishings, Electronics, Appliance Retailers -> Home furnishing
    455: 53,    # General Merchandise Retailers -> General merchandise
    456: 59,    # Health and Personal Care Retailers -> Other retail
    457: 55,    # Gasoline Stations and Fuel Dealers (SIC 554) -> Auto dealers/services/parts
    458: 56,    # Clothing, Clothing Accessories, Shoe, Jewelry Retailers (2022) -> Apparel stores
    459: 59,    # Sporting Goods, Hobby, Musical Instrument, Book, Miscellaneous Retailers (2022) -> Other retail
    # Transport / warehousing
    487: 42,    # Scenic and Sightseeing Transportation -> Freight/transport
    493: 42,    # Warehousing and Storage -> Freight/transport
    # Information (NAICS 2022 51X reorganization)
    513: 24,    # Publishing Industries (new 513 in 2022) -> Manufacturing (follows crosswalk 511 convention)
    516: 48,    # Broadcasting and Content Providers (new 516 in 2022) -> Communications
    # Finance
    521: 61,    # Monetary Authorities - Central Bank -> Banks/securities (via SIC 60 extension)
    525: 61,    # Funds, Trusts, and Other Financial Vehicles -> Banks/securities
    # Other services
    533: 72,    # Lessors of Nonfinancial Intangible Assets -> Personal/business services
    551: 72,    # Management of Companies and Enterprises -> Personal/business services
}

# Combine base crosswalk-derived lookup with manual resolutions (manual takes precedence on ambiguous codes)
naics3_to_sic_category_lookup = {
    **naics3_to_sic_category_from_crosswalks,
    **naics3_to_sic_category_manual_resolutions,
}

# Assert every NAICS3 in EEO-1 is classified (mapped to a sic_category or explicitly marked `None` to drop).
# If a future EEO-1 release introduces a new NAICS3 code, this fails with a clear message pointing here.
unclassified_naics3 = set(eeo1["naics3"].unique().tolist()) - set(naics3_to_sic_category_lookup.keys())
assert len(unclassified_naics3) == 0, (
    f"🪦 Unclassified NAICS3 codes in EEO-1: {sorted(unclassified_naics3)}. "
    "Add them to naics3_to_sic_category_manual_resolutions (sic_category value or None to drop)."
)

# ------------------------------------------------------------------------------
# Map NAICS3 -> sic_category and aggregate to industry x demographic
# ------------------------------------------------------------------------------
# Apply lookup; rows mapped to None (industries outside our 19 categories) become NaN and drop
eeo1["sic_category"] = eeo1["naics3"].map(naics3_to_sic_category_lookup)
eeo1 = eeo1.dropna(subset=["sic_category"])
eeo1["sic_category"] = eeo1["sic_category"].astype(int)

# Aggregate employment across NAICS3 codes within each (sic_category, black_indicator, sex) cell
eeo1 = (
    eeo1.groupby(["sic_category", "black_indicator", "sex"], observed=True)
    .agg(
        employment_eeo1=("employment_eeo1", "sum"),
        n_naics3_eeo1=("naics3", "nunique"),
    )
    .reset_index()
)

# Sort for reproducibility
eeo1 = eeo1.sort_values(["sic_category", "black_indicator", "sex"]).reset_index(drop=True)

# Assert expected cell count: 19 SIC categories x 2 race x 2 sex = 76 rows (fail-fast if any cell missing)
assert len(eeo1) == 19 * 2 * 2, (
    f"🪦 Expected 76 industry x demographic cells, got {len(eeo1)}. "
    "Some SIC categories have no EEO-1 data — inspect before proceeding."
)

# Diagnostic tabulation of sic_category coverage in the aggregate
tabulate_variable(eeo1, "sic_category")

# Write long-format industry x demographic EEO-1 aggregate to dump
eeo1.to_csv(dump / "industry_emp_by_demographic_eeo1.csv", index=False)

# Report output path and row count
print(
    f"🎃 Wrote industry x demographic EEO-1 aggregate to "
    f"{dump / 'industry_emp_by_demographic_eeo1.csv'}: {len(eeo1):,} rows"
)
