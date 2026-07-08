"""
Match pulled Revelio firm-level measures to survey firm IDs.

This version is compatible with the newer Revelio pull that outputs both:
    * entry-level / seniority-1 workforce measures, prefixed with entry_
    * all-employee workforce measures, prefixed with all_

It also creates backward-compatible unprefixed columns from the entry_ columns.
For example:
    entry_female_share -> female_share
    entry_white_share  -> white_share
    entry_avg_salary_all -> avg_salary_all

That means older downstream analysis code that expects female_share, white_share,
avg_salary_all, etc. will continue to use the entry-level seniority-1 measure.
The new all-employee measures remain available as all_female_share,
all_white_share, all_avg_salary_all, etc.

Inputs:
    data/external/revelio_company_race_gender_salary_2023_entry_and_all_with_parent_subsidiaries.csv
    data/processed/long_survey_final.csv

Outputs:
    data/processed/revelio_firm_measures.csv
    data/dump/revelio_firm_name_matches.csv
    data/dump/revelio_unmatched_survey_firms.csv
    data/dump/revelio_duplicate_match_keys.csv, only if duplicate normalized keys are found
"""

import re
import sys
import unicodedata
from pathlib import Path

# Add code directory to Python path to import globals module.
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from globals import dump, ensure_python_packages, external, processed

ensure_python_packages(["pandas"])

import pandas as pd


# New raw Revelio file produced by the latest WRDS/JupyterHub pull.
REVELIO_RAW_PATH = (
    external
    / "revelio_company_race_gender_salary_2023_entry_and_all_with_parent_subsidiaries.csv"
)

SURVEY_PATH = processed / "long_survey_final.csv"
REVELIO_FIRM_MEASURES_PATH = processed / "revelio_firm_measures.csv"
MATCH_DIAGNOSTICS_PATH = dump / "revelio_firm_name_matches.csv"
UNMATCHED_PATH = dump / "revelio_unmatched_survey_firms.csv"
DUPLICATE_KEYS_PATH = dump / "revelio_duplicate_match_keys.csv"


DROP_WORDS = {
    "inc",
    "incorporated",
    "corp",
    "corporation",
    "co",
    "company",
    "companies",
    "cos",
    "group",
    "holdings",
    "holding",
    "plc",
    "llc",
    "ltd",
    "limited",
    "the",
}

COMPANY_ALIASES = {
    "adp": "automatic data processing",
    "carrier": "carrier global",
    "costco": "costco wholesale",
    "disney": "walt disney",
    "dowdupont": "dow",
    "dr pepper snapple": "keurig dr pepper",
    "estee lauder": "estee lauder",
    "general electric": "ge aerospace",
    "ibm": "international business machines",
    "icahn enterprises": "icahn enterprises lp",
    "jpmorgan chase": "jpmorgan chase and",
    "laboratory of america": "labcorp",
    "merck": "merck and",
    "otis": "otis worldwide",
    "sears": "sears roebuck and",
    "synnex": "td synnex",
    "united continental": "united airlines",
    "ups": "united parcel service",
    "verizon": "verizon communications",
    "victoria s secret": "victoria s secret and",
    "wells fargo": "wells fargo and",
    "xpo logistics": "xpo",
}


# Columns from the old one-scope file that downstream code may still expect.
# In the new raw file, these live under entry_<column>. This script creates
# the old unprefixed column name as an alias to the entry-level measure.
LEGACY_ENTRY_ALIAS_COLUMNS = [
    "has_2023_worker_match",
    "n_position_rows_2023",
    "n_position_rcids_2023",
    "n_users_from_positions_2023",
    "n_position_rows_with_salary_2023",
    "n_unique_users_matched_to_user_file",
    "weighted_n_users",
    "n_missing_weight",
    "n_users_with_gender_probs",
    "n_users_with_ethnicity_probs",
    "weighted_n_gender",
    "weighted_n_ethnicity",
    "female_share",
    "male_share",
    "white_share",
    "black_share",
    "api_share",
    "hispanic_share",
    "native_share",
    "multiple_share",
    "n_users_with_salary",
    "weighted_n_salary",
    "unweighted_avg_salary_all",
    "avg_salary_all",
    "weighted_salary_denominator_female",
    "weighted_salary_denominator_male",
    "avg_salary_female",
    "avg_salary_male",
    "weighted_salary_denominator_white",
    "weighted_salary_denominator_black",
    "weighted_salary_denominator_api",
    "weighted_salary_denominator_hispanic",
    "weighted_salary_denominator_native",
    "weighted_salary_denominator_multiple",
    "avg_salary_white",
    "avg_salary_black",
    "avg_salary_api",
    "avg_salary_hispanic",
    "avg_salary_native",
    "avg_salary_multiple",
]


# Optional columns that appeared in older versions of the Revelio pull, but may
# not appear in the latest entry/all pull. Add placeholders so downstream code
# does not fail just because these variables are absent from the raw file.
OPTIONAL_REVELIO_PLACEHOLDER_COLUMNS = [
    # Aggregate sentiment_scores fields from the older JupyterHub pull.
    "sentiment_match_type",
    "sentiment_company",
    "sentiment_num_reviews",
    "management_sentiment",
    "innovative_technology_sentiment",
    "work_life_balance_sentiment",
    "mentorship_sentiment",
    "career_advancement_sentiment",
    "div_and_inclusion_sentiment",
    "coworkers_sentiment",
    "compensation_sentiment",
    "culture_sentiment",
    "co_and_division_size_sentiment",
    "perks_and_benefits_sentiment",
    "onboarding_sentiment",
    "remote_work_sentiment",
    # Individual-review D&I fields from the newer salary pull.
    "n_di_rating_reviews_current_ft_seniority1_2023",
    "avg_di_rating_current_ft_seniority1_2023",
]


def normalize_company_name(value):
    if pd.isna(value):
        return ""

    value = (
        unicodedata.normalize("NFKD", str(value))
        .encode("ascii", "ignore")
        .decode("ascii")
    )
    value = value.lower().replace("&", " and ")
    value = re.sub(r"[^a-z0-9]+", " ", value)
    key = " ".join(token for token in value.split() if token not in DROP_WORDS)
    return COMPANY_ALIASES.get(key.strip(), key.strip())


def require_file(path, label):
    if not path.exists():
        raise FileNotFoundError(f"{label} not found: {path}")


def add_downstream_compatibility_columns(revelio_raw):
    """
    Mutate and return revelio_raw with compatibility columns added.

    The new raw file contains entry_* and all_* variables. Older downstream
    analysis code expects unprefixed variables such as female_share,
    white_share, and avg_salary_all. We define those unprefixed variables to
    equal the entry-level seniority-1 variables.
    """
    revelio_raw = revelio_raw.copy()

    # Old unprefixed columns are treated as entry-level seniority-1 measures.
    for col in LEGACY_ENTRY_ALIAS_COLUMNS:
        entry_col = f"entry_{col}"
        if col not in revelio_raw.columns and entry_col in revelio_raw.columns:
            revelio_raw[col] = revelio_raw[entry_col]

    # Add placeholders for older optional sentiment / review columns that may
    # no longer be pulled. This keeps old downstream code from failing on a
    # missing column. Values are intentionally missing.
    for col in OPTIONAL_REVELIO_PLACEHOLDER_COLUMNS:
        if col not in revelio_raw.columns:
            revelio_raw[col] = pd.NA

    return revelio_raw


def build_revelio_lookup(revelio_raw):
    revelio_base = revelio_raw.rename(
        columns={
            "company": "revelio_company",
            "company_name": "revelio_company_name",
        }
    )

    required_cols = {
        "analysis_firm_key",
        "revelio_company",
        "revelio_company_name",
    }
    missing_cols = sorted(required_cols - set(revelio_base.columns))
    if missing_cols:
        raise ValueError(
            "Revelio raw file is missing required columns: "
            + ", ".join(missing_cols)
        )

    lookup_parts = []
    for priority, field in (
        (1, "revelio_company"),
        (2, "revelio_company_name"),
    ):
        lookup_piece = revelio_base.copy()
        lookup_piece["revelio_key"] = lookup_piece[field].map(normalize_company_name)
        lookup_piece["revelio_match_field"] = (
            "company" if field == "revelio_company" else "company_name"
        )
        lookup_piece["revelio_match_priority"] = priority
        lookup_parts.append(lookup_piece)

    lookup_all = pd.concat(lookup_parts, ignore_index=True)
    lookup_all = lookup_all.loc[
        lookup_all["revelio_key"].notna() & (lookup_all["revelio_key"] != "")
    ].drop_duplicates()

    duplicate_counts = (
        lookup_all[["revelio_key", "analysis_firm_key"]]
        .drop_duplicates()
        .groupby("revelio_key", dropna=False)
        .size()
    )
    duplicate_keys = duplicate_counts[duplicate_counts > 1].index.tolist()

    if duplicate_keys:
        dump.mkdir(parents=True, exist_ok=True)
        (
            lookup_all.loc[lookup_all["revelio_key"].isin(duplicate_keys)]
            .sort_values(["revelio_key", "analysis_firm_key"])
            .to_csv(DUPLICATE_KEYS_PATH, index=False)
        )
        raise ValueError(
            "Duplicate normalized Revelio match keys found. "
            f"Review: {DUPLICATE_KEYS_PATH}"
        )

    lookup = (
        lookup_all.sort_values(["revelio_key", "revelio_match_priority"])
        .drop_duplicates(subset=["revelio_key"], keep="first")
        .drop(columns=["revelio_match_priority"])
    )

    return lookup


def read_revelio_raw():
    revelio_raw = pd.read_csv(
        REVELIO_RAW_PATH,
        dtype={
            "analysis_firm_key": "string",
            "requested_rcid": "string",
            "workforce_source_rcid": "string",
        },
    )
    return add_downstream_compatibility_columns(revelio_raw)


def read_survey_firms():
    survey_firms = pd.read_csv(
        SURVEY_PATH,
        usecols=["firm_id", "firm", "aer_naics2"],
    )
    survey_firms = survey_firms.loc[
        survey_firms["firm"].notna() & (survey_firms["firm"] != "nan")
    ].drop_duplicates()
    survey_firms["firm_id"] = pd.to_numeric(
        survey_firms["firm_id"], errors="coerce"
    ).astype("Int64")
    survey_firms["aer_naics2"] = pd.to_numeric(
        survey_firms["aer_naics2"], errors="coerce"
    ).astype("Int64")
    survey_firms["survey_key"] = survey_firms["firm"].map(normalize_company_name)

    duplicate_survey_firms = (
        survey_firms.groupby("firm_id", dropna=False).size().loc[lambda x: x > 1]
    )
    if not duplicate_survey_firms.empty:
        duplicate_ids = ", ".join(str(x) for x in duplicate_survey_firms.index)
        raise ValueError(
            "Duplicate survey firm_id values in long_survey_final.csv: "
            f"{duplicate_ids}"
        )

    return survey_firms


def main():
    require_file(REVELIO_RAW_PATH, "Revelio raw file")
    require_file(SURVEY_PATH, "Survey analysis file")

    processed.mkdir(parents=True, exist_ok=True)
    dump.mkdir(parents=True, exist_ok=True)

    revelio_raw = read_revelio_raw()
    survey_firms = read_survey_firms()
    revelio_lookup = build_revelio_lookup(revelio_raw)

    revelio_firm_measures = survey_firms.merge(
        revelio_lookup,
        how="left",
        left_on="survey_key",
        right_on="revelio_key",
    )
    revelio_firm_measures["matched_to_revelio"] = revelio_firm_measures[
        "analysis_firm_key"
    ].notna()

    front_cols = [
        "firm_id",
        "firm",
        "survey_key",
        "matched_to_revelio",
        "revelio_match_field",
        "analysis_firm_key",
        "requested_rcid",
        "workforce_source_rcid",
        "workforce_source_company",
        "revelio_company",
        "revelio_company_name",
        "parent_company",
        "workforce_match_type",
        "aer_naics2",
    ]

    # If an older or otherwise modified raw file is missing a metadata column,
    # add it as missing rather than failing during column ordering.
    for col in front_cols:
        if col not in revelio_firm_measures.columns:
            revelio_firm_measures[col] = pd.NA

    remaining_cols = [
        col
        for col in revelio_firm_measures.columns
        if col not in front_cols and col != "revelio_key"
    ]
    revelio_firm_measures = revelio_firm_measures[front_cols + remaining_cols]
    revelio_firm_measures = revelio_firm_measures.sort_values("firm_id")

    unmatched_firms = revelio_firm_measures.loc[
        ~revelio_firm_measures["matched_to_revelio"],
        ["firm_id", "firm", "survey_key", "aer_naics2"],
    ].sort_values("firm")

    revelio_firm_measures.to_csv(REVELIO_FIRM_MEASURES_PATH, index=False)
    revelio_firm_measures.to_csv(MATCH_DIAGNOSTICS_PATH, index=False)
    unmatched_firms.to_csv(UNMATCHED_PATH, index=False)

    n_matched = int(revelio_firm_measures["matched_to_revelio"].sum())
    n_total = len(revelio_firm_measures)

    print(f"Matched {n_matched} of {n_total} survey firms to Revelio measures")
    print(f"Wrote Revelio firm measures: {REVELIO_FIRM_MEASURES_PATH}")
    print(f"Wrote Revelio match diagnostics: {MATCH_DIAGNOSTICS_PATH}")
    print(f"Wrote unmatched firm diagnostics: {UNMATCHED_PATH}")

    # Useful checks for the new entry/all structure and old downstream aliases.
    compatibility_checks = [
        "female_share",
        "white_share",
        "black_share",
        "api_share",
        "hispanic_share",
        "entry_female_share",
        "entry_white_share",
        "all_female_share",
        "all_white_share",
    ]
    present_checks = [
        col for col in compatibility_checks if col in revelio_firm_measures.columns
    ]
    if present_checks:
        print("\nCompatibility columns present:")
        for col in present_checks:
            print(f"  - {col}")

    if "female_share" in revelio_firm_measures.columns and "entry_female_share" in revelio_firm_measures.columns:
        alias_agrees = revelio_firm_measures["female_share"].equals(
            revelio_firm_measures["entry_female_share"]
        )
        print(
            "\nLegacy alias check: female_share equals entry_female_share = "
            f"{alias_agrees}"
        )


if __name__ == "__main__":
    main()