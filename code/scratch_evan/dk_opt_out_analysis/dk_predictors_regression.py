import pandas as pd
import statsmodels.api as sm
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

covariates = {
    "race": "Black",
    "gender": "Female",
    "looking_job": "Looking for a Job",
    "fear": "Feared Discrimination",
    "age_gt40": "Age >= 40",
    "educ_0_1": "At Least Some College",
    "sample": "Probability Sample",
    "confidence_race": "Confident (Race)",
    "confidence_gend": "Confident (Gender)",
}
formula_rhs = " + ".join(covariates.keys())

all_results = []
for outcome, (v1, v2) in {
    "pooled_favor_white": ("FirmCont_favor_white", "conduct_favor_white"),
    "pooled_favor_male": ("FirmCont_favor_male", "conduct_favor_male"),
}.items():
    administered, dk = build_dk_indicator(raw, v1, v2)
    cols = ["resp_id", "firm_id"] + list(covariates.keys())
    d = raw.loc[administered, cols].copy()
    d["dk"] = dk[administered].values
    d = d.dropna()  # listwise-delete rows missing a covariate (mainly confidence_race/gend)

    model = smf.ols(f"dk ~ {formula_rhs}", data=d).fit(
        cov_type="cluster", cov_kwds={"groups": d["resp_id"]}
    )

    print(f"\n=== {outcome} : DK rate = {d['dk'].mean():.4f}, n_obs={len(d)}, n_respondents={d['resp_id'].nunique()} ===")
    print(model.summary().tables[1])

    for name, label in covariates.items():
        all_results.append(dict(
            outcome=outcome, covariate=label,
            coef=model.params[name], se_cluster=model.bse[name],
            p_value=model.pvalues[name],
        ))

results_df = pd.DataFrame(all_results)
results_df.to_csv(f"{OUT_DIR}/dk_predictors_lpm_results.csv", index=False)
print("\nSaved:", f"{OUT_DIR}/dk_predictors_lpm_results.csv")
