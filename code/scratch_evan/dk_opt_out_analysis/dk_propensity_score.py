import pandas as pd
import numpy as np
import statsmodels.formula.api as smf

import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _config import MIRROR_ROOT as MIRROR
from _config import RESULTS_DIR as OUT_DIR

raw = pd.read_csv(f"{MIRROR}/data/processed/long_survey_final_summary_stats.csv", low_memory=False)

def build_dk_indicator(df, v1, v2):
    a, b = df[v1], df[v2]
    assert ((a.notna()) & (b.notna())).sum() == 0, f"{v1}/{v2} not mutually exclusive"
    administered = a.notna() | b.notna()
    raw_response = a.where(a.notna(), b)
    dk = (raw_response == -1).astype(float)
    return administered, dk

covariates = ["race", "gender", "looking_job", "fear", "age_gt40", "educ_0_1", "sample", "confidence_race", "confidence_gend"]

frames = []
for outcome, (v1, v2) in {
    "pooled_favor_white": ("FirmCont_favor_white", "conduct_favor_white"),
    "pooled_favor_male": ("FirmCont_favor_male", "conduct_favor_male"),
}.items():
    administered, dk = build_dk_indicator(raw, v1, v2)
    cols = ["resp_id", "firm_id"] + covariates
    d = raw.loc[administered, cols].copy()
    d["dk"] = dk[administered].values
    d["outcome"] = outcome
    frames.append(d)

stacked = pd.concat(frames, ignore_index=True)
stacked["is_male_question"] = (stacked["outcome"] == "pooled_favor_male").astype(int)
stacked = stacked.dropna(subset=covariates)  # listwise-delete missing confidence answers

formula = "dk ~ " + " + ".join(covariates) + " + is_male_question"
model = smf.ols(formula, data=stacked).fit(cov_type="cluster", cov_kwds={"groups": stacked["resp_id"]})
print("=== Combined DK regression (both outcomes pooled, question-type fixed effect) ===")
print(f"n_obs={len(stacked)}  n_respondents={stacked['resp_id'].nunique()}")
print(model.summary().tables[1])

stacked["fitted_dk_prob"] = model.predict(stacked)

# One propensity score per respondent: average fitted probability across their own
# administered rows (across both outcomes/firms) -- since covariates are respondent-level,
# this differs across respondents only through the covariates + the mix of which
# outcome(s)/firms they were administered.
propensity = stacked.groupby("resp_id")["fitted_dk_prob"].mean().reset_index().rename(columns={"fitted_dk_prob": "propensity_score"})
respondent_covars = raw[["resp_id"] + covariates].drop_duplicates(subset="resp_id")
propensity = propensity.merge(respondent_covars, on="resp_id", how="left")

median_propensity = propensity["propensity_score"].median()
propensity["group"] = np.where(propensity["propensity_score"] >= median_propensity, "High_DK_Propensity", "Low_DK_Propensity")

print(f"\nMedian propensity score: {median_propensity:.4f}")
print(propensity["group"].value_counts())
print(propensity.groupby("group")["propensity_score"].describe())

propensity.to_csv(f"{OUT_DIR}/dk_propensity_scores.csv", index=False)
print("\nSaved:", f"{OUT_DIR}/dk_propensity_scores.csv")
