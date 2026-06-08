"""
Match pulled Revelio firm-level measures to survey firm IDs.

Inputs:
    data/external/revelio_company_race_gender_salary_2023_with_parent_subsidiaries.csv
    data/processed/long_survey_final.csv

Outputs:
    data/processed/revelio_firm_measures.csv
    data/dump/revelio_firm_name_matches.csv
    data/dump/revelio_unmatched_survey_firms.csv
"""

import re
import sys
import unicodedata
from pathlib import Path

# Add code directory to Python path to import globals module
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from globals import dump, ensure_python_packages, external, processed

ensure_python_packages(["pandas"])

import pandas as pd


REVELIO_RAW_PATH = (
    external / "revelio_company_race_gender_salary_2023_with_parent_subsidiaries.csv"
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


def build_revelio_lookup(revelio_raw):
    revelio_base = revelio_raw.rename(
        columns={
            "company": "revelio_company",
            "company_name": "revelio_company_name",
        }
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


def main():
    require_file(REVELIO_RAW_PATH, "Revelio raw file")
    require_file(SURVEY_PATH, "Survey analysis file")

    processed.mkdir(parents=True, exist_ok=True)
    dump.mkdir(parents=True, exist_ok=True)

    revelio_raw = pd.read_csv(
        REVELIO_RAW_PATH,
        dtype={
            "analysis_firm_key": "string",
            "requested_rcid": "string",
            "workforce_source_rcid": "string",
        },
    )
    if "div_and_inclusion_sentiment" not in revelio_raw.columns:
        revelio_raw["div_and_inclusion_sentiment"] = pd.NA

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

    print(
        "Matched "
        f"{int(revelio_firm_measures['matched_to_revelio'].sum())} of "
        f"{len(revelio_firm_measures)} survey firms to Revelio measures"
    )
    print(f"Wrote Revelio firm measures: {REVELIO_FIRM_MEASURES_PATH}")
    print(f"Wrote Revelio match diagnostics: {MATCH_DIAGNOSTICS_PATH}")
    print(f"Wrote unmatched firm diagnostics: {UNMATCHED_PATH}")


if __name__ == "__main__":
    main()
