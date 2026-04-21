# ------------------------------------------------------------------------------
# Purpose: Merge the industry x demographic CPS and EEO-1 aggregates onto the
# firm-level industry map to produce a single firm x race x sex x age_bin dataset
# with CPS employment + wage (age-varying) and EEO-1 employment (age-invariant,
# broadcast across age_bins).
#
# Reads from:
#   {dump}/industry_map.csv                           — firm-level industry map (164 firms,
#                                                       keyed on sic_code_aggregated_two_digit_harmonized_numeric_aer)
#   {dump}/industry_emp_wage_by_demographic_cps.csv   — CPS aggregate, one row per
#                                                       (sic_category, black_indicator, sex, age_bin)
#   {dump}/industry_emp_by_demographic_eeo1.csv       — EEO-1 aggregate, one row per
#                                                       (sic_category, black_indicator, sex)
#
# Writes to:
#   {dump}/industry_map_emp_wage_by_demographic.csv  — one row per firm x race x sex x age_bin
#                                                      (164 firms x 2 race x 2 sex x 4 age = 2,624 rows)
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
from globals import dump

# ------------------------------------------------------------------------------
# Merge industry_map with CPS (age-varying) and EEO-1 (age-invariant) aggregates
# ------------------------------------------------------------------------------
# Start from the firm-level industry map; the industry aggregates are small reference tables that we
# merge onto firm rows via sic_category. Work on a single `industry_map` dataframe throughout.
industry_map = pd.read_csv(dump / "industry_map.csv")

# Fail-fast invariants on the firm-level input: firm_clean must be unique and the SIC merge key must be
# present as a clean integer (the CPS/EEO-1 aggregates key on int sic_category; a float merge key is a
# latent dtype-drift hazard).
assert industry_map["firm_clean"].is_unique, \
    "🪦 industry_map.csv has duplicated firm_clean values — firm-level primary key violated"
assert industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"].notna().all(), \
    "🪦 industry_map.csv has missing sic_code_aggregated_two_digit_harmonized_numeric_aer (SIC merge key)"
industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"] = (
    industry_map["sic_code_aggregated_two_digit_harmonized_numeric_aer"].astype(int)
)

# Cross firm x (sic_category x race x sex x age_bin) CPS demographic cells: each firm's sic_category joins
# to its 16 demographic rows in the CPS aggregate. Result has 164 firms x 16 cells = 2,624 rows.
industry_map = industry_map.merge(
    pd.read_csv(dump / "industry_emp_wage_by_demographic_cps.csv"),
    left_on="sic_code_aggregated_two_digit_harmonized_numeric_aer",
    right_on="sic_category",
    how="left",
)

# Broadcast the age-invariant EEO-1 aggregate across age_bins by merging on (sic_category, black_indicator, sex).
# EEO-1 columns will be identical across the 4 age_bins within each (firm, race, sex) group.
industry_map = industry_map.merge(
    pd.read_csv(dump / "industry_emp_by_demographic_eeo1.csv"),
    on=["sic_category", "black_indicator", "sex"],
    how="left",
    validate="m:1",
)

# Drop the redundant sic_category column (it's the right-side join key from the CPS merge; the firm-side
# sic_code_aggregated_two_digit_harmonized_numeric_aer is the canonical column and is retained)
industry_map = industry_map.drop(columns=["sic_category"])

# Order columns: firm identifiers and SIC fields first, then demographic keys, then CPS metrics, then EEO-1 metrics
industry_map = industry_map[[
    # Firm identifiers and SIC fields (from industry_map.csv)
    "firm_clean",
    "sic_code_two_digit_harmonized",
    "sic_code_aggregated_two_digit_harmonized",
    "sic_code_aggregated_two_digit_harmonized_numeric_aer",
    "sic_code_aggregated_two_digit_harmonized_names_aer",
    "aer_naics2",
    "merge_status",
    "new_firm",
    # Demographic keys
    "black_indicator",
    "sex",
    "age_bin",
    # CPS restrictive wage-sample metrics (FT, age 20-60, not-allocated, valid-wage; vary by age_bin)
    "count_avg_hourly_wage_cps",
    "avg_hourly_wage_cps",
    "n_obs_cps",
    # CPS broader-sample employment for EEO-1 comparison (ORG universe only; constant across age_bin)
    "employment_cps_eeo1_comparable",
    "n_obs_cps_eeo1_comparable",
    "employment_cps_eeo1_comparable_2023",
    "n_obs_cps_eeo1_comparable_2023",
    # EEO-1 metrics (constant across age_bin within firm x race x sex)
    "employment_eeo1",
    "n_naics3_eeo1",
]]

# Sort for reproducibility: firm, then demographic cells in canonical order
industry_map = industry_map.sort_values(
    ["firm_clean", "black_indicator", "sex", "age_bin"]
).reset_index(drop=True)

# Assert expected row count: 164 firms x 2 race x 2 sex x 4 age_bin = 2,624 rows
assert len(industry_map) == 164 * 2 * 2 * 4, (
    f"🪦 Expected 2,624 firm x demographic rows, got {len(industry_map):,}. "
    "Check for firms missing from industry_map.csv or sic_categories missing from the CPS/EEO-1 aggregates."
)

# Assert primary-key uniqueness on the output: each firm appears in exactly 16 (race, sex, age_bin) cells
# with no duplicates. Catches duplicated firm rows in the input that would otherwise slip past the coarse
# row-count check above.
assert not industry_map.duplicated(subset=["firm_clean", "black_indicator", "sex", "age_bin"]).any(), \
    "🪦 Duplicated (firm_clean, black_indicator, sex, age_bin) in output — primary key violated"
rows_per_firm = industry_map.groupby("firm_clean").size()
assert (rows_per_firm == 16).all(), (
    f"🪦 Some firms do not have exactly 16 demographic rows: {rows_per_firm[rows_per_firm != 16].to_dict()}"
)

# Assert every row has CPS and EEO-1 metrics populated (every sic_category in industry_map should appear
# in both aggregates — assertion fails loudly if that invariant breaks)
for required_metric in [
    "count_avg_hourly_wage_cps", "avg_hourly_wage_cps",
    "employment_cps_eeo1_comparable", "employment_cps_eeo1_comparable_2023",
    "employment_eeo1",
]:
    assert industry_map[required_metric].notna().all(), (
        f"🪦 {required_metric} has missing values — one or more firms' sic_category is absent from the aggregate."
    )

# ------------------------------------------------------------------------------
# Sanity check: EEO-1 vs CPS industry-level employment (three CPS definitions)
# ------------------------------------------------------------------------------
# Compare three CPS employment measures against EEO-1 at the (sic_category, race, sex) level:
#   (1) count_avg_hourly_wage_cps (summed across age_bins) — restrictive wage sample (FT, age 20-60,
#                                                            not-allocated, valid wage) — the wage-analysis sample
#   (2) employment_cps_eeo1_comparable       — broader ORG-universe sample, 5-year pool (no FT/age/wage filters)
#   (3) employment_cps_eeo1_comparable_2023  — broader ORG-universe sample, 2023 only (exact year-match to EEO-1)
# EEO-1 is an annual firm-reported headcount restricted to establishments with >=100 employees (or federal
# contractors with >=50), so EEO-1 levels remain substantially smaller than CPS in all cases. The informative
# comparison is the demographic *share* within each sic_category.
industry_employment_comparison = (
    industry_map
    .drop_duplicates(subset=["sic_code_aggregated_two_digit_harmonized_numeric_aer", "black_indicator", "sex", "age_bin"])
    .groupby(
        ["sic_code_aggregated_two_digit_harmonized_numeric_aer", "black_indicator", "sex"],
        observed=True,
    )
    .agg(
        count_avg_hourly_wage_cps_all_ages=("count_avg_hourly_wage_cps", "sum"),
        employment_cps_eeo1_comparable=("employment_cps_eeo1_comparable", "first"),
        employment_cps_eeo1_comparable_2023=("employment_cps_eeo1_comparable_2023", "first"),
        employment_eeo1=("employment_eeo1", "first"),
    )
    .reset_index()
    .rename(columns={"sic_code_aggregated_two_digit_harmonized_numeric_aer": "sic_category"})
)

# Within each sic_category, compute demographic shares for all four employment measures
for source_column in [
    "count_avg_hourly_wage_cps_all_ages",
    "employment_cps_eeo1_comparable",
    "employment_cps_eeo1_comparable_2023",
    "employment_eeo1",
]:
    share_column = "share_" + source_column.replace("employment_", "").replace("count_", "")
    industry_employment_comparison[share_column] = (
        industry_employment_comparison[source_column]
        / industry_employment_comparison.groupby("sic_category")[source_column].transform("sum")
    )

# Share gaps (EEO-1 minus each CPS definition, in percentage points)
industry_employment_comparison["share_gap_eeo1_minus_cps_wage_sample"] = (
    industry_employment_comparison["share_eeo1"] - industry_employment_comparison["share_avg_hourly_wage_cps_all_ages"]
)
industry_employment_comparison["share_gap_eeo1_minus_cps_broad_pooled"] = (
    industry_employment_comparison["share_eeo1"] - industry_employment_comparison["share_cps_eeo1_comparable"]
)
industry_employment_comparison["share_gap_eeo1_minus_cps_broad_2023"] = (
    industry_employment_comparison["share_eeo1"] - industry_employment_comparison["share_cps_eeo1_comparable_2023"]
)

# Report the share comparison
print("🎃 EEO-1 vs CPS industry-level demographic-share comparison (3 CPS definitions):")
print(
    industry_employment_comparison[[
        "sic_category", "black_indicator", "sex",
        "share_avg_hourly_wage_cps_all_ages",
        "share_cps_eeo1_comparable",
        "share_cps_eeo1_comparable_2023",
        "share_eeo1",
        "share_gap_eeo1_minus_cps_wage_sample",
        "share_gap_eeo1_minus_cps_broad_pooled",
        "share_gap_eeo1_minus_cps_broad_2023",
    ]].round(3).to_string(index=False)
)

# Summarize: how many cells differ by >5 pp, and average absolute gap, under each CPS definition
for cps_definition_label, gap_column in [
    ("CPS wage sample (FT + age 20-60)",           "share_gap_eeo1_minus_cps_wage_sample"),
    ("CPS broad, 5-year pool (ORG universe)",      "share_gap_eeo1_minus_cps_broad_pooled"),
    ("CPS broad, 2023 only (ORG universe, 2023)",  "share_gap_eeo1_minus_cps_broad_2023"),
]:
    gap_series = industry_employment_comparison[gap_column]
    print(
        f"🎃 {cps_definition_label}: "
        f"{(gap_series.abs() > 0.05).sum()} of {len(gap_series)} cells differ >5pp from EEO-1; "
        f"mean |share gap| = {gap_series.abs().mean():.3f}"
    )

# External sanity check: total EEO-1 employment across all 19 SIC categories should be in the tens of
# millions (EEO-1 2023 covers ~60k establishments representing large-firm private-sector workforce).
total_eeo1_employment = industry_employment_comparison["employment_eeo1"].sum()
print(
    f"🎃 Total EEO-1 employment (sum across 19 SIC categories x 4 demographic cells): "
    f"{total_eeo1_employment:,.0f}"
)
assert 30_000_000 <= total_eeo1_employment <= 100_000_000, (
    f"🪦 Total EEO-1 employment {total_eeo1_employment:,.0f} is outside the plausible 30M-100M range "
    "for the 19 SIC categories. Inspect the NAICS3 -> SIC category mapping."
)

# Write merged firm x demographic dataset to dump
industry_map.to_csv(dump / "industry_map_emp_wage_by_demographic.csv", index=False)

# Report output path and row count
print(
    f"🎃 Wrote firm x demographic CPS+EEO-1 dataset to "
    f"{dump / 'industry_map_emp_wage_by_demographic.csv'} "
    f"({len(industry_map):,} rows, {industry_map['firm_clean'].nunique()} firms)"
)
