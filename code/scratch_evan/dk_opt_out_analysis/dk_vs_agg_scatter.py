import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _config import MIRROR_ROOT as MIRROR
from _config import RESULTS_DIR as OUT_DIR

# --- Final firm-level estimates for both aggregation methods (stored, authoritative;
# subset == "all", the same rows Table 4 uses) ---
coef = pd.read_parquet(f"{MIRROR}/output/intermediate/Full_Sample/Coefficients.parquet")
coef = coef[
    (coef["entity_type"] == "Firm")
    & (coef["subset"] == "all")
    & (coef["model"].isin(["OLS_not_recentered", "Borda_not_recentered"]))
    & (coef["outcome"].isin(["pooled_favor_white", "pooled_favor_male"]))
][["entity_id", "model", "outcome", "estimate"]].rename(columns={"entity_id": "firm_id"})
for (model, outcome), g in coef.groupby(["model", "outcome"]):
    assert len(g) == 164, (model, outcome, len(g))

# --- Firm-level DK / prefer-not-to-answer rate per outcome (independent of aggregation
# method -- it's a property of the raw survey responses, not of how they're aggregated),
# same administered/DK logic as before ---
raw = pd.read_csv(f"{MIRROR}/data/processed/long_survey_final_summary_stats.csv", low_memory=False)

def firm_dk_rates(df, v1, v2, label):
    a, b = df[v1], df[v2]
    assert ((a.notna()) & (b.notna())).sum() == 0, f"{v1}/{v2} not mutually exclusive"
    administered = a.notna() | b.notna()
    raw_response = a.where(a.notna(), b)
    dk = administered & (raw_response == -1)
    tmp = pd.DataFrame({"firm_id": df["firm_id"], "administered": administered, "dk": dk})
    g = tmp[tmp["administered"]].groupby("firm_id")["dk"].agg(["sum", "count"]).reset_index()
    g["outcome"] = label
    g["dk_rate"] = g["sum"] / g["count"]
    return g.rename(columns={"sum": "n_dk", "count": "n_administered"})

dk = pd.concat([
    firm_dk_rates(raw, "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white"),
    firm_dk_rates(raw, "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male"),
], ignore_index=True)

merged = coef.merge(dk, on=["firm_id", "outcome"], how="inner")
merged.to_csv(f"{OUT_DIR}/firm_dk_vs_estimate_by_method.csv", index=False)

panel_labels = {
    "OLS_not_recentered": "Panel A: Likert (OLS)",
    "Borda_not_recentered": "Panel B: Borda",
}
outcome_labels = {
    "pooled_favor_white": "Discrimination Black (Pooled)",
    "pooled_favor_male": "Discrimination Female (Pooled)",
}
methods = ["OLS_not_recentered", "Borda_not_recentered"]
outcomes = ["pooled_favor_white", "pooled_favor_male"]

fig, axes = plt.subplots(2, 2, figsize=(11, 9))
results = []
for i, model in enumerate(methods):
    for j, outcome in enumerate(outcomes):
        ax = axes[i, j]
        sub = merged[(merged["model"] == model) & (merged["outcome"] == outcome)].sort_values("dk_rate")
        x = sub["dk_rate"].to_numpy()
        y = sub["estimate"].to_numpy()

        X = sm.add_constant(x)
        fit = sm.OLS(y, X).fit(cov_type="HC1")
        intercept, slope = fit.params
        se_intercept, se_slope = fit.bse
        pval_slope = fit.pvalues[1]

        results.append(dict(model=model, outcome=outcome, n_firms=len(sub),
                             intercept=intercept, slope=slope,
                             se_intercept=se_intercept, se_slope=se_slope,
                             p_value_slope=pval_slope))

        ax.scatter(x, y, alpha=0.7, s=22, color="darkorange", edgecolor="none")
        x_line = np.linspace(x.min(), x.max(), 100)
        y_line = intercept + slope * x_line
        pred = fit.get_prediction(sm.add_constant(x_line))
        ci = pred.conf_int(alpha=0.05)
        ax.plot(x_line, y_line, color="steelblue", linewidth=1.8)
        ax.fill_between(x_line, ci[:, 0], ci[:, 1], color="steelblue", alpha=0.15, linewidth=0)

        ax.text(
            0.03, 0.97,
            f"OLS coef (robust SE): {slope:.3f} ({se_slope:.3f})\np-value: {pval_slope:.3f}",
            transform=ax.transAxes, ha="left", va="top", fontsize=10,
            bbox=dict(boxstyle="round", facecolor="white", edgecolor="lightgray", alpha=0.9),
        )
        ax.set_xlabel("Firm-level DK / prefer-not-to-answer rate")
        ax.set_ylabel(f"{outcome_labels[outcome]}\nfirm-level average")
        ax.set_title(f"{panel_labels[model]} — {outcome_labels[outcome]}", fontsize=11)
        ax.spines[["top", "right"]].set_visible(False)

fig.tight_layout()
fig.savefig(f"{OUT_DIR}/firm_dk_rate_vs_estimate_by_method_scatter.png", dpi=200, facecolor="white")
print("wrote", f"{OUT_DIR}/firm_dk_rate_vs_estimate_by_method_scatter.png")

results_df = pd.DataFrame(results)
print(results_df.to_string(index=False))
results_df.to_csv(f"{OUT_DIR}/firm_dk_vs_estimate_ols_fit_summary.csv", index=False)
