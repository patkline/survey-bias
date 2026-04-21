# ------------------------------------------------------------------------------
# Purpose: Build ind1990 -> (SIC category, NAICS3) crosswalks from external
# reference files (Autor/Dorn/Hanson + Census Bureau) and output a single
# combined crosswalk to disk for downstream use by clean_cps_data.py.
#
# Reads from:
#   {external}/cw_sic87_ind1990ddx.dta             — Autor/Dorn/Hanson 2019
#   {external}/cw_ind1990_ind1990ddx.dta           — Autor/Dorn/Hanson 2019
#   {external}/sic_updates.csv                     — 19-category SIC aggregation scheme
#   {external}/industry-crosswalk-90-00-02-07-12.xls — Census Bureau crosswalk
#   {dump}/industry_map.csv                        — firm-level industry map (for validating SIC extensions)
#
# Writes to:
#   {dump}/ind1990_crosswalks.csv                  — one row per ind1990 code with sic_category, naics3, naics2
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

# ------------------------------------------------------------------------------
# Collapse sic87 to 2-digit SIC and build sic to ind1990ddx crosswalk
# ------------------------------------------------------------------------------
# Load SIC87 -> ind1990ddx crosswalk (Autor/Dorn/Hanson 2019)
sic87_ind1990ddx_crosswalk = pd.read_stata(external / "cw_sic87_ind1990ddx.dta")

# Collapse SIC87 to 2-digit SIC
sic87_ind1990ddx_crosswalk["sic"] = sic87_ind1990ddx_crosswalk["sic87"] // 100

# Apply 4 manual duplicate resolutions from wage_regressions.do (lines 126-129)
sic87_ind1990ddx_crosswalk.loc[sic87_ind1990ddx_crosswalk["ind1990ddx"] == 60,  "sic"] = 15
sic87_ind1990ddx_crosswalk.loc[sic87_ind1990ddx_crosswalk["ind1990ddx"] == 710, "sic"] = 62
sic87_ind1990ddx_crosswalk.loc[sic87_ind1990ddx_crosswalk["ind1990ddx"] == 711, "sic"] = 63
sic87_ind1990ddx_crosswalk.loc[sic87_ind1990ddx_crosswalk["ind1990ddx"] == 800, "sic"] = 78

# Deduplicate on (sic, ind1990ddx) to collapse multiple SIC87 codes that fold into the same 2-digit SIC
sic87_ind1990ddx_crosswalk = sic87_ind1990ddx_crosswalk.drop_duplicates(subset=["sic", "ind1990ddx"])[["ind1990ddx", "sic"]]

# Assert each ind1990ddx maps to exactly one 2-digit SIC after dedup
assert not sic87_ind1990ddx_crosswalk["ind1990ddx"].duplicated().any(), "Duplicated ind1990ddx after SIC dedup"

# ------------------------------------------------------------------------------
# Build ind1990 to ind1990ddx crosswalk
# ------------------------------------------------------------------------------
# Load ind1990 -> ind1990ddx crosswalk (Autor/Dorn/Hanson 2019)
ind1990_ind1990ddx_crosswalk = pd.read_stata(external / "cw_ind1990_ind1990ddx.dta")

# Convert ind1990 from category (string codes like "0010") to int to match CPS dtype
ind1990_ind1990ddx_crosswalk["ind1990"] = ind1990_ind1990ddx_crosswalk["ind1990"].astype(int)

# Convert ind1990ddx from float32 to int to match merge key dtype
ind1990_ind1990ddx_crosswalk["ind1990ddx"] = ind1990_ind1990ddx_crosswalk["ind1990ddx"].astype(int)

# Assert ind1990 is unique in the linking crosswalk
assert not ind1990_ind1990ddx_crosswalk["ind1990"].duplicated().any(), "Duplicated ind1990 in linking crosswalk"

# ------------------------------------------------------------------------------
# Build ind1990 to 2-digit SIC crosswalk
# ------------------------------------------------------------------------------
# Merge 2-digit SIC onto ind1990 via ind1990ddx
ind1990_to_sic_crosswalk = ind1990_ind1990ddx_crosswalk.merge(
    sic87_ind1990ddx_crosswalk,
    on="ind1990ddx",
    how="left",
    validate="m:1",
)

# Drop ind1990 codes whose ind1990ddx has no SIC87 mapping
ind1990_to_sic_crosswalk = ind1990_to_sic_crosswalk.dropna(subset=["sic"])

# Cast SIC back to int after dropna
ind1990_to_sic_crosswalk["sic"] = ind1990_to_sic_crosswalk["sic"].astype(int)

# Keep only ind1990 -> sic 2-digit mapping
ind1990_to_sic_crosswalk = ind1990_to_sic_crosswalk[["ind1990", "sic"]]

# ------------------------------------------------------------------------------
# Map SIC 2-digit to our 19 aggregated SIC categories
# ------------------------------------------------------------------------------
# Load SIC aggregation scheme (19 unique categories; one row per firm in raw file)
sic_updates = (
    pd.read_csv(external / "sic_updates.csv", usecols=["new_sic", "new_sic_numeric"])
    .drop_duplicates(subset=["new_sic"])
)

# Assert the 19 expected categories are present
assert len(sic_updates) == 19, f"Expected 19 SIC categories in sic_updates.csv, found {len(sic_updates)}"

# Initialize lookup dict from 2-digit SIC integer to 19-category SIC numeric
sic_category_by_two_digit_sic = {}

# Loop over the 19 SIC categories in sic_updates
for _, sic_update_row in sic_updates.iterrows():

    # Parse range string, e.g., "24-35" -> (24, 35) and "20" -> (20, 20)
    sic_category_range_string = sic_update_row["new_sic"]
    if "-" in sic_category_range_string:
        sic_range_start_str, sic_range_end_str = sic_category_range_string.split("-")
        sic_range_start, sic_range_end = int(sic_range_start_str), int(sic_range_end_str)
    else:
        sic_range_start = sic_range_end = int(sic_category_range_string)

    # Expand range and populate lookup for each 2-digit SIC value
    for sic_value in range(sic_range_start, sic_range_end + 1):
        sic_category_by_two_digit_sic[sic_value] = int(sic_update_row["new_sic_numeric"])

# Snapshot the strict range-only lookup before adding extensions (used downstream to distinguish
# range-based firm mappings from extension/override mappings when validating against firm-level file)
sic_category_by_two_digit_sic_range_only = dict(sic_category_by_two_digit_sic)

# Add manual extensions to keep CPS aggregation consistent with the implicit firm-level imputations
# in `create_firm_industry_crosswalk_industry_map.py` (lines 487-509). Those imputations reveal that
# several 2-digit SIC codes fall outside sic_updates.csv ranges but are economically part of one of
# our 19 categories. Without these extensions, CPS workers in these industries would silently drop and
# the CPS benchmark for those categories would exclude them (e.g., aerospace/autos from Manufacturing,
# depository banks from Banking, rail/airlines from Freight). The validation block below asserts that
# every 2-digit SIC imputed at the firm level is covered by one of the extensions here, or is a
# firm-specific override that CPS should not apply (see sic_firm_specific_overrides_not_applied_to_cps).
sic_extensions_from_firm_level_imputations = {
    # Manufacturing (24): electronics (36), transport equipment (37), instruments (38), misc mfg (39)
    # Firms: GE, Whirlpool, Jabil, Amphenol (36); Boeing, GM, Ford, Lockheed, Northrop, Pratt&Whitney, Lear (37)
    36: 24, 37: 24, 38: 24, 39: 24,
    # Freight / transport (42): railroads (40), local transit (41), air transport (45). 47 already in 42-47 range.
    # Firms: BNSF (40), American Airlines, Delta (45)
    40: 42, 41: 42, 45: 42,
    # Banks / securities (61): depository banks (60). 61-64 already in range.
    # Firms: BB&T, Wells Fargo, Bank of America (60)
    60: 61,
}
sic_category_by_two_digit_sic.update(sic_extensions_from_firm_level_imputations)

# Catalog firm-specific overrides that the firm-level file applies to correct data-quality issues for
# specific firms but that CPS should NOT apply — the 2-digit SIC represents a genuinely different industry
# at the CPS worker level, so extending the category would wrongly pull real workers in that industry into
# the imputation target. Kept here (and validated against the firm-level file) so that if new firm-specific
# overrides appear, the assertion block flags them for explicit classification.
sic_firm_specific_overrides_not_applied_to_cps = {
    # SIC 17 (Construction - Special Trade Contractors) -> 24 (Manufacturing) in firm file:
    # applies to Carrier (HVAC), Otis (elevators), General Electric. RefUSA codes these as SIC 17
    # because of their installation/service arms; the firm-level imputation correctly re-classifies them
    # as manufacturers. CPS workers coded SIC 17, by contrast, are genuine plumbers/HVAC technicians/
    # electricians — they belong in construction, which is not one of our 19 categories, so they drop.
    17: 24,
}

# Validate extensions against the firm-level industry map so that a change in firm-level imputations
# surfaces a clear failure rather than a silent inconsistency. For each distinct (2-digit SIC, firm-level
# aggregated category) pair in industry_map.csv, classify as:
#   (a) range-based match      — sic_updates.csv range already gives the firm's category; no extension needed
#   (b) firm-specific override on a range SIC — range gives a valid but different category (e.g., Apple/Dell
#                                               SIC 35 -> 72 instead of the range-based 24); not a CPS-level
#                                               issue since we match on SIC, not firm
#   (c) extension required     — SIC outside all ranges; must be in sic_extensions_from_firm_level_imputations
#                                OR sic_firm_specific_overrides_not_applied_to_cps (explicitly classified)
firm_industry_map_for_validation = pd.read_csv(
    dump / "industry_map.csv",
    usecols=["firm_clean", "sic_code_two_digit_harmonized", "sic_code_aggregated_two_digit_harmonized_numeric_aer"],
)
firm_two_digit_sic_to_category_pairs = (
    firm_industry_map_for_validation
    .dropna(subset=["sic_code_two_digit_harmonized", "sic_code_aggregated_two_digit_harmonized_numeric_aer"])
    .astype({
        "sic_code_two_digit_harmonized": int,
        "sic_code_aggregated_two_digit_harmonized_numeric_aer": int,
    })[["sic_code_two_digit_harmonized", "sic_code_aggregated_two_digit_harmonized_numeric_aer"]]
    .drop_duplicates()
)

# Loop over each distinct (2-digit SIC, firm-level aggregated category) pair and classify
for firm_map_row in firm_two_digit_sic_to_category_pairs.itertuples(index=False):
    firm_two_digit_sic = firm_map_row.sic_code_two_digit_harmonized
    firm_level_category = firm_map_row.sic_code_aggregated_two_digit_harmonized_numeric_aer
    range_based_category = sic_category_by_two_digit_sic_range_only.get(firm_two_digit_sic)

    # Case (a): range-based mapping matches firm-level category — no extension needed
    if range_based_category == firm_level_category:
        continue

    # Case (b): range-based mapping exists but firm-level overrides it (firm-specific override on a range
    # SIC) — no CPS-level extension needed since we match on SIC not firm
    if range_based_category is not None:
        continue

    # Case (c): SIC not in any sic_updates.csv range — must be a cataloged extension OR a cataloged
    # firm-specific override
    is_economic_extension = firm_two_digit_sic in sic_extensions_from_firm_level_imputations
    is_firm_specific_override = firm_two_digit_sic in sic_firm_specific_overrides_not_applied_to_cps
    assert is_economic_extension or is_firm_specific_override, (
        f"🪦 Firm-level industry_map.csv imputes SIC 2-digit {firm_two_digit_sic} → category {firm_level_category}, "
        f"but this SIC is neither in sic_extensions_from_firm_level_imputations nor in "
        f"sic_firm_specific_overrides_not_applied_to_cps. Classify it (extension if SIC is economically in "
        f"the target category; firm-specific override if it's a firm data-quality fix) and update this script."
    )
    if is_economic_extension:
        assert sic_extensions_from_firm_level_imputations[firm_two_digit_sic] == firm_level_category, (
            f"🪦 CPS extension maps SIC {firm_two_digit_sic} → {sic_extensions_from_firm_level_imputations[firm_two_digit_sic]}, "
            f"but firm-level industry_map.csv imputes → {firm_level_category}. Reconcile sic_extensions_from_firm_level_imputations."
        )
    if is_firm_specific_override:
        assert sic_firm_specific_overrides_not_applied_to_cps[firm_two_digit_sic] == firm_level_category, (
            f"🪦 Cataloged firm-specific override maps SIC {firm_two_digit_sic} → "
            f"{sic_firm_specific_overrides_not_applied_to_cps[firm_two_digit_sic]}, but firm-level industry_map.csv "
            f"imputes → {firm_level_category}. Reconcile sic_firm_specific_overrides_not_applied_to_cps."
        )

# Map each ind1990's 2-digit SIC to aggregated SIC category
ind1990_to_sic_crosswalk["sic_category"] = ind1990_to_sic_crosswalk["sic"].map(sic_category_by_two_digit_sic)

# Drop ind1990 codes whose 2-digit SIC falls outside the 19 categories (e.g., agriculture, mining, construction services, government, military)
ind1990_to_sic_crosswalk = ind1990_to_sic_crosswalk.dropna(subset=["sic_category"])

# Cast sic_category to int after dropna
ind1990_to_sic_crosswalk["sic_category"] = ind1990_to_sic_crosswalk["sic_category"].astype(int)

# Assert each ind1990 maps to exactly one SIC category
assert not ind1990_to_sic_crosswalk["ind1990"].duplicated().any(), "Duplicated ind1990 in final SIC category crosswalk"

print(f"🎃 ind1990 -> SIC category crosswalk: {len(ind1990_to_sic_crosswalk):,} ind1990 codes mapped to {ind1990_to_sic_crosswalk['sic_category'].nunique()} categories")

# ------------------------------------------------------------------------------
# Build ind1990 -> NAICS3 crosswalk from Census Bureau industry crosswalk xls
# ------------------------------------------------------------------------------
# Logic ported from wage_regressions.do lines 17-96:
#   (1) Load rows A26:M307 (282 data rows) from industry-crosswalk-90-00-02-07-12.xls.
#   (2) Destring ind1990 (strip annotation chars *, p, +, -, +/-); drop null and "New" rows that have no 1990 equivalent.
#   (3) Extract NAICS3 from the naics2012 string: clean numeric -> first 3 chars; multi-code string -> only
#       assign naics3 if all sub-codes share the same first 3 digits (ambiguous otherwise).
#   (4) Recover NAICS3 for "Old" 1990 categories by parsing the 2000 code embedded in ind2000_title
#       ("(now part of XXX)") at chars 14-16 and reusing naics3 of rows that share that ind2000 code.
#   (5) Apply 24 manual resolutions (ambiguous multi-code naics2012 and duplicates) + set ind1990=992 to NaN.
#   (6) Deduplicate on (ind1990, naics3) and assert ind1990 uniqueness.

# Load Census Bureau industry crosswalk (A26:M307 in the xls = pandas skiprows=25, nrows=282)
census_industry_crosswalk = pd.read_excel(
    external / "industry-crosswalk-90-00-02-07-12.xls",
    skiprows=25,
    nrows=282,
    header=None,
    dtype=str,
)

# Keep only the 4 columns needed and rename:
#   column A (idx 0) -> ind1990       (1990 Census code, with annotation chars *, p, +, -, +/-)
#   column C (idx 2) -> ind2000       (Census 2000 code; "Old" string for categories dropped after 1990)
#   column D (idx 3) -> ind2000_title (Census 2000 title; "(now part of XXX)" for Old rows)
#   column K (idx 10) -> naics2012    (2012 NAICS code, may be clean number or multi-code string like "1131, 1132")
census_industry_crosswalk = census_industry_crosswalk[[0, 2, 3, 10]].copy()
census_industry_crosswalk.columns = ["ind1990", "ind2000", "ind2000_title", "naics2012"]

# Drop fully-null rows and "New" categories (no 1990 equivalent — unreachable from CPS IND1990)
census_industry_crosswalk = census_industry_crosswalk[
    census_industry_crosswalk["ind1990"].notna()
    & ~census_industry_crosswalk["ind1990"].astype(str).str.contains("New", na=False)
].copy()

# Destring ind1990 by removing all non-digit characters (covers *, p, +, -, / in +/- combos, trailing whitespace)
census_industry_crosswalk["ind1990"] = (
    census_industry_crosswalk["ind1990"].astype(str).str.replace(r"[^\d]", "", regex=True).astype(int)
)

# Initialize naics3 as missing; populated row-by-row below
census_industry_crosswalk["naics3"] = pd.NA

# Parse naics2012 into naics3 row-by-row (clean numeric -> first 3 chars; multi-code -> same-3-digit check)
for census_row_index, census_row in census_industry_crosswalk.iterrows():
    naics2012_value = census_row["naics2012"]

    # Skip missing (handled below via the "Old" recovery pass using ind2000_title)
    if pd.isna(naics2012_value):
        continue

    naics2012_string = str(naics2012_value).strip()

    # Clean numeric case: take first 3 chars
    if naics2012_string.isdigit():
        census_industry_crosswalk.at[census_row_index, "naics3"] = int(naics2012_string[:3])
        continue

    # Multi-code case: strip noise words and replace separators with whitespace
    for naics2012_noise_word in ["exc", "pt", "Pts", "Part of", "and"]:
        naics2012_string = naics2012_string.replace(naics2012_noise_word, "")
    for naics2012_separator in [".", ",", "-", "*"]:
        naics2012_string = naics2012_string.replace(naics2012_separator, " ")
    naics2012_subcodes = naics2012_string.split()

    # Skip if no sub-codes remain after cleaning
    if not naics2012_subcodes:
        continue

    # Candidate naics3 = first 3 chars of first sub-code; require exactly 3 digits
    first_subcode_three_digits = naics2012_subcodes[0][:3]
    if len(first_subcode_three_digits) != 3 or not first_subcode_three_digits.isdigit():
        continue

    # Assign naics3 only if every sub-code shares the first 3 digits (ambiguous multi-naics left missing)
    if all(naics2012_subcode[:3] == first_subcode_three_digits for naics2012_subcode in naics2012_subcodes):
        census_industry_crosswalk.at[census_row_index, "naics3"] = int(first_subcode_three_digits)

# Recover NAICS3 for "Old" rows (1990 categories dropped in 2000+): ind2000_title has format
# "(now part of XXX)" with XXX = 3-digit 2000 code at chars 14-16 (1-indexed; Python slice [13:16]).
# Destring ind2000 (replace "Old" with NaN, then to numeric) so we can look up rows with matching ind2000.
ind2000_destringed = census_industry_crosswalk["ind2000"].astype(str).str.strip().replace("Old", pd.NA)
census_industry_crosswalk["ind2000_int"] = pd.to_numeric(ind2000_destringed, errors="coerce").astype("Int64")

# Only recover for rows with missing naics2012 (i.e., rows where ind2000 is "Old")
naics2012_is_missing_mask = census_industry_crosswalk["naics2012"].isna()
ind2000_code_from_title = census_industry_crosswalk["ind2000_title"].astype(str).str.slice(13, 16)
census_industry_crosswalk["ind2000_temp"] = pd.to_numeric(
    ind2000_code_from_title.where(naics2012_is_missing_mask, pd.NA),
    errors="coerce",
).astype("Int64")

# For each distinct ind2000_temp value, find the unique naics3 among rows whose ind2000_int matches
# (per Stata's `r(N) == 1` condition); assign it to the Old rows that share that ind2000_temp
for ind2000_temp_value in census_industry_crosswalk["ind2000_temp"].dropna().unique():
    naics3_values_matching_ind2000 = census_industry_crosswalk.loc[
        (census_industry_crosswalk["ind2000_int"] == ind2000_temp_value)
        & census_industry_crosswalk["naics3"].notna(),
        "naics3",
    ].unique()
    if len(naics3_values_matching_ind2000) == 1:
        census_industry_crosswalk.loc[
            census_industry_crosswalk["ind2000_temp"] == ind2000_temp_value, "naics3"
        ] = naics3_values_matching_ind2000[0]

# Drop helper columns used only in the NAICS3 extraction (ind2000_temp, ind2000_int, ind2000, ind2000_title, naics2012)
census_industry_crosswalk = census_industry_crosswalk.drop(
    columns=["ind2000", "ind2000_int", "ind2000_title", "ind2000_temp", "naics2012"]
)

# Manual resolutions from wage_regressions.do lines 64-96. Two groups:
#   (i) Ambiguous multi-code naics2012 where parsing left naics3 missing (different 3-digit NAICS per sub-code)
#   (ii) Duplicates where the same ind1990 maps to multiple 3-digit NAICS after dedup; researcher picks one
ind1990_to_naics3_manual_resolutions = {
    # (i) Ambiguous multi-code — Stata lines 64-74
    132: 313,  # Knitting mills -> Textile Mills
    301: 331,  # Metal industries, n.s. -> Primary Metal Manufacturing
    392: 339,  # Manufacturing industries, n.s. -> Miscellaneous Manufacturing
    691: 453,  # Retail trade, n.s. -> Miscellaneous Store Retailers
    700: 522,  # Banking -> Credit Intermediation
    710: 523,  # Security/commodity brokerage -> Securities/Investments
    741: 561,  # Business services, n.e.c. -> Admin/Support Services
    910: 922,  # Justice/public order/safety -> Justice/Safety Activities
    930: 924,  # Admin of environmental quality -> Environmental Programs
    931: 926,  # Admin of economic programs -> Economic Programs
    # (ii) Duplicates at dedup — Stata lines 77-86
    810: 711,  # Misc entertainment/recreation -> Performing Arts
    732: 518,  # Computer/data processing -> Data Processing/Hosting
    682: 453,  # Misc retail stores -> Miscellaneous Store Retailers
    652: 451,  # Book/stationery stores -> Sporting Goods/Book/Music
    471: 562,  # Sanitary services -> Waste Management
    432: 488,  # Services incidental to transport -> Support Activities for Transport
    410: 484,  # Trucking service -> Truck Transportation
    401: 485,  # Bus service/urban transit -> Transit/Ground Passenger Transport
    342: 335,  # Electrical machinery, n.e.c. -> Electrical Equipment Mfg
    172: 511,  # Printing/publishing except newspapers -> Publishing
    # Codes originally mapped to 2-digit NAICS in Stata (padded to 3-digit assignments) — Stata lines 90-92
    472: 221,  # Utilities, n.s. -> Utilities
    60:  236,  # Construction -> Construction of Buildings
    571: 423,  # Wholesale trade, n.s. -> Merchant Wholesalers, Durable Goods
    # No pre-1990 courier/messengers category; Stata re-maps warehousing to couriers — Stata line 95
    411: 492,  # Warehousing/storage -> Courier/Messengers
}

# Apply each manual resolution (overwrites any existing naics3 for that ind1990)
for ind1990_manual, naics3_manual in ind1990_to_naics3_manual_resolutions.items():
    census_industry_crosswalk.loc[
        census_industry_crosswalk["ind1990"] == ind1990_manual, "naics3"
    ] = naics3_manual

# Explicit NaN for ind1990=992 ("Last worked 1984 or earlier") per Stata line 75 — unusable for current analysis
census_industry_crosswalk.loc[census_industry_crosswalk["ind1990"] == 992, "naics3"] = pd.NA

# Drop rows still missing naics3 after parsing + recovery + manual resolutions (only ind1990=992 in practice)
ind1990_to_naics3_crosswalk = census_industry_crosswalk.dropna(subset=["naics3"]).copy()

# Cast naics3 to int and deduplicate on (ind1990, naics3)
ind1990_to_naics3_crosswalk["naics3"] = ind1990_to_naics3_crosswalk["naics3"].astype(int)
ind1990_to_naics3_crosswalk = ind1990_to_naics3_crosswalk[["ind1990", "naics3"]].drop_duplicates()

# Assert each ind1990 maps to exactly one NAICS3 after manual resolutions
assert not ind1990_to_naics3_crosswalk["ind1990"].duplicated().any(), "Duplicated ind1990 in final NAICS3 crosswalk"

print(
    f"🎃 ind1990 -> NAICS3 crosswalk: {len(ind1990_to_naics3_crosswalk):,} ind1990 codes mapped to "
    f"{ind1990_to_naics3_crosswalk['naics3'].nunique()} NAICS3 codes"
)

# ------------------------------------------------------------------------------
# Combine into single ind1990-keyed crosswalk and save
# ------------------------------------------------------------------------------
# Outer merge on ind1990 so the output documents every ind1990 code that appears in either crosswalk.
# Rows outside the 19 SIC categories keep sic_category = NaN; rows without NAICS (e.g., ind1990=992) keep naics3 = NaN.
ind1990_crosswalks = ind1990_to_sic_crosswalk[["ind1990", "sic_category"]].merge(
    ind1990_to_naics3_crosswalk,
    on="ind1990",
    how="outer",
    validate="1:1",
)

# Derive NAICS 2-digit (floor(NAICS3 / 10)) for downstream EEO-1 NAICS -> SIC bridge
ind1990_crosswalks["naics2"] = (ind1990_crosswalks["naics3"] // 10).astype("Int64")

# Cast integer columns to nullable Int64 (handles NaN cleanly on save/load)
for integer_column in ["ind1990", "sic_category", "naics3", "naics2"]:
    ind1990_crosswalks[integer_column] = ind1990_crosswalks[integer_column].astype("Int64")

# Order columns: key first, then SIC mapping, then NAICS mapping
ind1990_crosswalks = ind1990_crosswalks[["ind1990", "sic_category", "naics3", "naics2"]].sort_values("ind1990").reset_index(drop=True)

# Assert ind1990 is unique in the output crosswalk (it is the primary key)
assert not ind1990_crosswalks["ind1990"].duplicated().any(), "Duplicated ind1990 in combined crosswalk output"

# Write combined crosswalk to dump
ind1990_crosswalks.to_csv(dump / "ind1990_crosswalks.csv", index=False)

# Report output write path and coverage
print(
    f"🎃 Wrote combined crosswalk to {dump / 'ind1990_crosswalks.csv'}: "
    f"{len(ind1990_crosswalks):,} ind1990 codes "
    f"({ind1990_crosswalks['sic_category'].notna().sum():,} with sic_category, "
    f"{ind1990_crosswalks['naics3'].notna().sum():,} with naics3)"
)
