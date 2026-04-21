# ------------------------------------------------------------------------------
# Purpose: Clean CPS microdata, compute hourly wages, attach SIC category and
# NAICS codes via the pre-built ind1990 crosswalks, and aggregate to the
# industry x race x sex x age_bin level for downstream merge onto the firm-level
# industry map (and eventual EEO-1 comparison).
#
# Reads from:
#   {external}/cps_extract_2022_2026.csv.gz  — IPUMS CPS 2022-2026 extract
#   {dump}/ind1990_crosswalks.csv            — ind1990 -> sic_category, naics3, naics2
#                                              (built by create_ind1990_crosswalks.py)
#
# Writes to:
#   {dump}/industry_emp_wage_by_demographic_cps.csv  — long format, one row per
#                                                      (sic_category, black_indicator, sex, age_bin)
#                                                      with weighted employment, weighted avg hourly
#                                                      wage, and unweighted observation count
#
# Created: Nico Rotundo 2026-04-10
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Import packages and path globals
# ------------------------------------------------------------------------------
# Import pandas for dataframe operations
import pandas as pd

# Import numpy for numerical operations
import numpy as np

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
# Load CPS extract
# ------------------------------------------------------------------------------
# Define columns to keep from CPS extract, grouped by role
cps_columns = [
    # Identifiers
    "CPSIDP", "YEAR", "MONTH",
    # Weight
    "EARNWT",
    # Demographics
    "AGE", "SEX", "RACE",
    # Labor force status
    "WKSTAT", "CLASSWKR",
    # Industry
    "IND1990",
    # Earnings and hours: UHRSWORKORG is only populated for hourly-paid workers (NIU for salaried);
    # UHRSWORK1 covers all employed civilians and is used as the hours denominator for salaried workers
    "EARNWEEK2", "UHRSWORKORG", "UHRSWORK1",
    # ORG eligibility and earnings allocation flags (QUHRSWORK1 is the allocation flag for UHRSWORK1)
    "ELIGORG", "QEARNWEE", "QUHRSWORKORG", "QUHRSWORK1",
]

# Load CPS extract, keeping only needed columns (integer dtypes specified; float columns default to float64)
cps = pd.read_csv(
    external / "cps_extract_2022_2026.csv.gz",
    usecols=cps_columns,
    dtype={
        # Identifiers
        "CPSIDP": int,
        "YEAR": int,
        "MONTH": int,
        # Demographics
        "AGE": int,
        "SEX": int,
        "RACE": int,
        # Labor force status
        "WKSTAT": int,
        "CLASSWKR": int,
        # Industry
        "IND1990": int,
        # Hours at ORG job (hourly-paid only, NIU=999) and at main job (all workers, 997=hours vary, 999=NIU)
        "UHRSWORKORG": int,
        "UHRSWORK1": int,
        # ORG eligibility and earnings allocation flags
        "ELIGORG": int,
        "QEARNWEE": int,
        "QUHRSWORKORG": int,
        "QUHRSWORK1": int,
    }
)

# Lowercase all column names
cps.columns = cps.columns.str.lower()

# Reorder columns to match the grouped structure above (identifiers first, then substantive vars)
cps = cps[[
    "cpsidp", "year", "month",
    "earnwt",
    "age", "sex", "race",
    "wkstat", "classwkr",
    "ind1990",
    "earnweek2", "uhrsworkorg", "uhrswork1",
    "eligorg", "qearnwee", "quhrsworkorg", "quhrswork1",
]]

# Assert non-empty load (fail-fast if extract is stale or path is wrong)
assert len(cps) > 0, "CPS extract loaded 0 rows"

# Assert person-month uniqueness (IPUMS CPS ORG natural key is cpsidp x year x month)
assert not cps.duplicated(subset=["cpsidp", "year", "month"]).any(), \
    "Duplicated (cpsidp, year, month) rows in CPS extract"

# Sort by identifiers for consistent ordering
cps = cps.sort_values(by=["cpsidp", "year", "month"]).reset_index(drop=True)

# Assert expected year coverage (guards against wrong extract being swapped in)
expected_years = {2022, 2023, 2024, 2025, 2026}
loaded_years = set(cps["year"].unique())
assert loaded_years == expected_years, \
    f"Expected CPS years {expected_years}, got {loaded_years}"

# Tabulate year for visual confirmation of row counts per year
tabulate_variable(cps, "year")

# Print progress message
print(f"🎃 Loaded CPS extract: {len(cps):,} rows, {len(cps.columns)} columns")

# ------------------------------------------------------------------------------
# Filter to analysis sample
# ------------------------------------------------------------------------------
# Keep respondents in the ORG earnings universe via ELIGORG (IPUMS: 1 = eligible for the ORG earnings battery,
# encodes rotation months 4/8 + wage/salary employment)
cps = cps[cps["eligorg"] == 1]
print(f"🎃 After ORG universe (eligorg==1) filter: {len(cps):,} rows")

# Validate RACE and SEX codes on the full ELIGORG universe before any demographic recoding happens so that
# unexpected codes fail loudly here rather than silently contaminating downstream labels. (This validation
# previously lived further down inside the race/age/sex construction section; running it earlier ensures
# the broader-sample aggregate below sees only validated codes too.)
#   RACE (IPUMS 2022+ scheme): 100 White only, 200 Black only, 300 AIAN only, 651 Asian only, 652 HPI only,
#   700 Other n.e.c., 801-819 specific multi-race, 820/830 unspecified multi-race. Verified 2022-2026
#   extract uses the harmonized scheme (no 2023+ detailed codes 201-234 appear).
#   SEX: 1 Male, 2 Female.
valid_race_codes = [100, 200, 300, 651, 652, 700, *range(801, 820), 820, 830]
assert cps["race"].isin(valid_race_codes).all(), \
    "Unexpected RACE codes outside the IPUMS 2022+ harmonized scheme"
assert cps["sex"].isin([1, 2]).all(), \
    "Unexpected SEX codes (IPUMS expects only 1=Male, 2=Female)"

# ------------------------------------------------------------------------------
# Build broader CPS employment aggregate for EEO-1 comparison
# ------------------------------------------------------------------------------
# The restrictive CPS sample below applies FT (wkstat), age 20-60, drop-allocated, and valid-wage filters
# that match the wage-analysis use case but exclude workers that EEO-1 counts (PT workers, teens, 60+,
# workers with allocated earnings, workers with missing hours). Build two parallel employment-only
# aggregates on the broader ORG universe (only requires ELIGORG==1 and a valid sic_category mapping) so
# the final dataset can carry apples-to-apples CPS employment columns alongside EEO-1 headcounts:
#   (a) 5-year pool 2022-2026 — larger sample for precision, but year-composition differs from EEO-1 2023 snapshot
#   (b) 2023 subset only      — exact year-match to EEO-1 2023, smaller sample
#
# Time scaling note: raw EARNWT sums are kept (no scaling). EARNWT is a monthly population-stock weight,
# so summing over the 5-year extract gives ~60 month-stocks (5 years x 12 months); the 2023-only sum
# gives ~12 month-stocks. Demographic shares within sic_category are scale-invariant; downstream users
# who want annual-stock levels can divide each measure by its appropriate month count.
cps_broad_employment_sample = cps.copy()

# Build demographic labels on the broader sample (race and sex only — age_bin is intentionally omitted)
cps_broad_employment_sample["black_indicator"] = np.where(
    cps_broad_employment_sample["race"] == 200, "Black", "Not Black"
)
cps_broad_employment_sample["sex"] = np.where(
    cps_broad_employment_sample["sex"] == 1, "male", "female"
)

# Merge ind1990 -> sic_category crosswalk; drop rows whose ind1990 is outside the 19 SIC categories
cps_broad_employment_sample = cps_broad_employment_sample.merge(
    pd.read_csv(dump / "ind1990_crosswalks.csv", usecols=["ind1990", "sic_category"]),
    on="ind1990",
    how="left",
    validate="m:1",
)
cps_broad_employment_sample = cps_broad_employment_sample[
    cps_broad_employment_sample["sic_category"].notna()
].copy()
cps_broad_employment_sample["sic_category"] = cps_broad_employment_sample["sic_category"].astype(int)

# Aggregate weighted employment (and unweighted obs count) at (sic_category x race x sex) for both
# the 5-year pool and the 2023-only subset, then merge the 2023 measure onto the pooled frame
broader_cps_industry_employment = (
    cps_broad_employment_sample
    .groupby(["sic_category", "black_indicator", "sex"], observed=True)
    .agg(
        employment_cps_eeo1_comparable=("earnwt", "sum"),
        n_obs_cps_eeo1_comparable=("earnwt", "size"),
    )
    .reset_index()
)
broader_cps_industry_employment_2023_only = (
    cps_broad_employment_sample[cps_broad_employment_sample["year"] == 2023]
    .groupby(["sic_category", "black_indicator", "sex"], observed=True)
    .agg(
        employment_cps_eeo1_comparable_2023=("earnwt", "sum"),
        n_obs_cps_eeo1_comparable_2023=("earnwt", "size"),
    )
    .reset_index()
)
broader_cps_industry_employment = broader_cps_industry_employment.merge(
    broader_cps_industry_employment_2023_only,
    on=["sic_category", "black_indicator", "sex"],
    how="left",
    validate="1:1",
)
print(
    f"🎃 Built broader CPS employment aggregate (EEO-1 comparable): "
    f"{len(broader_cps_industry_employment):,} cells; "
    f"pooled sample {len(cps_broad_employment_sample):,} rows, "
    f"2023-only subset {(cps_broad_employment_sample['year']==2023).sum():,} rows"
)
del cps_broad_employment_sample

# Keep "usually full-time" workers per IPUMS WKSTAT:
#   11 = Full-time hours (35+), usually full-time
#   12 = Part-time for non-economic reasons, usually full-time
#   13 = Not at work, usually full-time
#   21 = Part-time for economic reasons, usually full-time
# Broader than wage_regressions.do (wkstat == 11 only) — captures people whose usual status is full-time
# but who happened to work part-time / not-at-work in the reference week
cps = cps[cps["wkstat"].isin([11, 12, 13, 21])]
print(f"🎃 After full-time filter: {len(cps):,} rows")

# Assert wage/salary invariant: ELIGORG==1 is supposed to exclude self-employed (13, 14), armed forces (26),
# and unpaid family workers (29). We assert rather than filter so that a future IPUMS release admitting
# non-wage/salary codes into ELIGORG fails loudly instead of silently including them.
assert cps["classwkr"].isin([22, 23, 25, 27, 28]).all(), \
    "ELIGORG==1 unexpectedly contains non-wage/salary classwkr codes"

# Keep respondents aged 20-60
cps = cps[(cps["age"] >= 20) & (cps["age"] <= 60)]
print(f"🎃 After age 20-60 filter: {len(cps):,} rows")

# Classify worker type from the raw UHRSWORKORG universe so we can apply the allocation-flag filter
# branch-specifically (each branch uses a different hours variable in the wage denominator):
# 999 = NIU = salaried (ORG hours question skipped); everything else = hourly branch.
cps["worker_type"] = np.where(cps["uhrsworkorg"] == 999, "salaried", "hourly")

# Drop allocated/imputed observations, but only on variables that enter the wage formula for each branch:
#   qearnwee       — always required (EARNWEEK2 is the numerator for both branches)
#   quhrsworkorg   — required only for hourly workers (UHRSWORKORG is the hourly denominator)
#   quhrswork1     — required only for salaried workers (UHRSWORK1 is the salaried denominator)
# Requiring all three for everyone would over-drop (76 hourly rows blocked by quhrswork1, 44 salaried by quhrsworkorg).
# Rationale for dropping allocated: Hirsch & Schumacher (2004) show retained allocated observations cause match bias in wage-gap estimates.
mask_earnings_not_allocated = cps["qearnwee"] == 0
mask_hours_not_allocated = np.where(
    cps["worker_type"] == "hourly",
    cps["quhrsworkorg"] == 0,
    cps["quhrswork1"] == 0,
)
cps = cps[mask_earnings_not_allocated & mask_hours_not_allocated]
print(f"🎃 After branch-specific drop-allocated filter: {len(cps):,} rows")

# Drop filter columns no longer needed (worker_type retained for downstream wage formula and stratification)
cps = cps.drop(columns=["eligorg", "qearnwee", "quhrsworkorg", "quhrswork1", "wkstat", "classwkr"])

# ------------------------------------------------------------------------------
# Compute hourly wage
# ------------------------------------------------------------------------------
# Recode IPUMS missing/NIU sentinels to NaN (and anomalous zero hours/earnings that would yield div-by-zero or $0 wage):
#   earnweek2:    0 = data anomaly for employed wage/salary worker, 999999.99 = NIU
#   uhrsworkorg:  0 = anomalous, 998 = don't know, 999 = NIU (NIU for salaried — question only asked on hourly branch of ORG)
#   uhrswork1:    0 = anomalous, 997 = "hours vary", 999 = NIU
cps["earnweek2"] = cps["earnweek2"].replace([0, 999999.99], np.nan)
cps["uhrsworkorg"] = cps["uhrsworkorg"].replace([0, 998, 999], np.nan)
cps["uhrswork1"] = cps["uhrswork1"].replace([0, 997, 999], np.nan)

# Compute hourly wage with explicit conditionals by worker type:
#   Hourly workers:   EARNWEEK2 / UHRSWORKORG — usual hours at ORG job (specifically measured for the ORG job being priced by EARNWEEK2)
#   Salaried workers: EARNWEEK2 / UHRSWORK1  — usual hours at main job (ORG-job hours not available; UHRSWORK1 is the broader main-job hours variable)
cps["hourly_wage"] = np.where(
    cps["worker_type"] == "hourly",
    cps["earnweek2"] / cps["uhrsworkorg"],
    cps["earnweek2"] / cps["uhrswork1"],
)

# Count wage-missing rows by worker_type before dropping so the loss is visible (the salaried drop is usually
# dominated by UHRSWORK1 == 997 "hours vary"; the hourly drop is usually zero given branch-specific allocation filter above)
n_missing_by_type = cps[cps["hourly_wage"].isna()].groupby("worker_type").size().to_dict()
print(f"🎃 Rows dropped during wage construction by worker_type: {n_missing_by_type}")

# Drop rows with missing hourly wage (salaried "hours vary", anomalous zero earnings/hours, NIU sentinels)
cps = cps.dropna(subset=["hourly_wage"])

# Drop raw wage components (worker_type retained for downstream stratification/diagnostics)
cps = cps.drop(columns=["earnweek2", "uhrsworkorg", "uhrswork1"])

print(f"🎃 After wage construction: {len(cps):,} rows ({cps['worker_type'].value_counts().to_dict()})")

# ------------------------------------------------------------------------------
# Construct race and age bin variables
# ------------------------------------------------------------------------------
# Tabulate race variable (RACE and SEX validation already happened on the ELIGORG universe above)
tabulate_variable(cps, "race")

# Construct race: Black (RACE == 200) vs Not Black (everything else)
cps["black_indicator"] = np.where(cps["race"] == 200, "Black", "Not Black")

# Drop raw race column
cps = cps.drop(columns=["race"])

# Construct age bins: 20-29, 30-39, 40-49, 50-60
cps["age_bin"] = pd.cut(
    cps["age"],
    bins=[19, 29, 39, 49, 60],
    labels=["20_29", "30_39", "40_49", "50_60"]
)

# Assert no missing age bins
assert cps["age_bin"].notna().all(), "Missing age bin values after construction"

# Drop raw age column
cps = cps.drop(columns=["age"])

# Construct sex label
cps["sex"] = np.where(cps["sex"] == 1, "male", "female")

# ------------------------------------------------------------------------------
# Merge ind1990 -> (sic_category, naics3, naics2) crosswalks onto CPS
# ------------------------------------------------------------------------------
# Load combined ind1990 crosswalks produced by create_ind1990_crosswalks.py.
# sic_category drives sample retention (rows outside the 19 SIC categories are dropped); naics3 and
# naics2 are informational columns used downstream for the EEO-1 NAICS -> SIC bridge. Default dtype
# inference gives int64 for ind1990 and float64 for sic_category/naics3/naics2 (NaN-capable); we cast
# them to plain int after filtering out unmapped rows.
ind1990_crosswalks = pd.read_csv(dump / "ind1990_crosswalks.csv")

# Merge crosswalks onto CPS (left join with indicator so we can log left_only counts)
cps = cps.merge(
    ind1990_crosswalks,
    on="ind1990",
    how="left",
    validate="m:1",
    indicator="_merge_ind1990_crosswalks",
)
print(f"🎃 CPS x ind1990 crosswalks merge: {cps['_merge_ind1990_crosswalks'].value_counts().to_dict()}")

# Drop CPS rows whose ind1990 does not map to one of our 19 SIC categories. Every SIC-matched ind1990
# also has a NAICS3 in the combined crosswalk (verified in create_ind1990_crosswalks.py), so after this
# filter all remaining rows have sic_category, naics3, and naics2 populated.
cps = cps[cps["sic_category"].notna()].drop(columns=["_merge_ind1990_crosswalks", "ind1990"])

# Cast the crosswalk-derived integer columns back to plain int now that the NaN rows are gone
for integer_column in ["sic_category", "naics3", "naics2"]:
    cps[integer_column] = cps[integer_column].astype(int)

print(f"🎃 After ind1990 crosswalk merge: {len(cps):,} rows (sic_category, naics3, naics2 all populated)")

# ------------------------------------------------------------------------------
# Aggregate CPS to the industry x race x sex x age_bin level
# ------------------------------------------------------------------------------
# Build per-respondent weighted-wage numerator (hourly_wage * earnwt) so the group-level weighted mean
# can be computed as sum(numerator) / sum(earnwt) using pandas' named .agg(...) API — avoids defining a
# custom function while keeping the weighting explicit
cps["_wage_weighted_numerator"] = cps["hourly_wage"] * cps["earnwt"]

# Aggregate to one row per (sic_category, black_indicator, sex, age_bin):
#   count_avg_hourly_wage_cps = weighted count of respondents in the wage sample (= sum of EARNWT) —
#                               the denominator underlying avg_hourly_wage_cps. Named to reflect that
#                               it is the count that corresponds to the wage variable.
#   _wage_numerator_sum       = sum of (wage * weight), intermediate for the weighted mean below
#   n_obs_cps                 = unweighted respondent count (diagnostic — flag cells with <30 obs)
industry_emp_wage = (
    cps.groupby(
        ["sic_category", "black_indicator", "sex", "age_bin"],
        observed=True,
    )
    .agg(
        count_avg_hourly_wage_cps=("earnwt", "sum"),
        _wage_numerator_sum=("_wage_weighted_numerator", "sum"),
        n_obs_cps=("earnwt", "size"),
    )
    .reset_index()
)

# Compute weighted mean hourly wage per cell: sum(wage*weight) / sum(weight)
industry_emp_wage["avg_hourly_wage_cps"] = (
    industry_emp_wage["_wage_numerator_sum"] / industry_emp_wage["count_avg_hourly_wage_cps"]
)

# Drop the intermediate weighted-wage numerator column
industry_emp_wage = industry_emp_wage.drop(columns=["_wage_numerator_sum"])

# Merge the broader (EEO-1-comparable) employment aggregates onto the restrictive age-disaggregated
# aggregate, broadcasting the age-invariant broader employment across the 4 age_bins within each
# (sic, race, sex) cell. The restrictive sample retains count_avg_hourly_wage_cps / avg_hourly_wage_cps /
# n_obs_cps; the broader ORG-universe sample provides employment_cps_eeo1_comparable (5-year pool) and
# employment_cps_eeo1_comparable_2023 (exact year match to EEO-1 2023).
industry_emp_wage = industry_emp_wage.merge(
    broader_cps_industry_employment,
    on=["sic_category", "black_indicator", "sex"],
    how="left",
    validate="m:1",
)

# Order columns: keys first, then restrictive wage-sample metrics, then broader EEO-1-comparable metrics
industry_emp_wage = industry_emp_wage[[
    "sic_category", "black_indicator", "sex", "age_bin",
    "count_avg_hourly_wage_cps", "avg_hourly_wage_cps", "n_obs_cps",
    "employment_cps_eeo1_comparable", "n_obs_cps_eeo1_comparable",
    "employment_cps_eeo1_comparable_2023", "n_obs_cps_eeo1_comparable_2023",
]]

# Sort for reproducibility (sic_category ascending, then categorical age_bin order, then race, sex)
industry_emp_wage = industry_emp_wage.sort_values(
    ["sic_category", "black_indicator", "sex", "age_bin"],
).reset_index(drop=True)

# Assert expected cell count: 19 SIC categories x 2 race x 2 sex x 4 age_bin = 304 rows.
# Fail-fast if any cell is missing (would indicate a zero-obs cell; warrants a diagnostic check).
assert len(industry_emp_wage) == 19 * 2 * 2 * 4, (
    f"🪦 Expected 304 industry x demographic cells, got {len(industry_emp_wage)}. "
    "Some cells likely have zero CPS observations — inspect before proceeding."
)

# Diagnostic: flag cells with fewer than 30 unweighted observations (thin estimates)
thin_cells = industry_emp_wage[industry_emp_wage["n_obs_cps"] < 30]
if len(thin_cells) > 0:
    print(f"🧌 {len(thin_cells)} cells have fewer than 30 unweighted observations (noisy wage estimates):")
    print(thin_cells.to_string(index=False))
else:
    print("🎃 All 304 industry x demographic cells have >=30 unweighted observations")

# Write long-format industry x demographic aggregate to dump
industry_emp_wage.to_csv(dump / "industry_emp_wage_by_demographic_cps.csv", index=False)

# Report output path and row count
print(
    f"🎃 Wrote industry x demographic CPS aggregate to "
    f"{dump / 'industry_emp_wage_by_demographic_cps.csv'}: {len(industry_emp_wage):,} rows"
)
