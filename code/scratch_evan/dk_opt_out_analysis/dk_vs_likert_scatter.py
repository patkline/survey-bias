import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

import sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from _config import MIRROR_ROOT as MIRROR
from _config import RESULTS_DIR as OUT_DIR

# --- Final firm-level Likert averages: the stored, authoritative OLS_not_recentered
# firm-level estimates (subset == "all"), same rows cross_sample_signal_corr.R uses for Table 4.
coef = pd.read_parquet(f"{MIRROR}/output/intermediate/Full_Sample/Coefficients.parquet")
coef = coef[
    (coef["entity_type"] == "Firm")
    & (coef["subset"] == "all")
    & (coef["model"] == "OLS_not_recentered")
    & (coef["outcome"].isin(["pooled_favor_white", "pooled_favor_male"]))
][["entity_id", "outcome", "estimate"]].rename(columns={"entity_id": "firm_id", "estimate": "likert_avg"})
assert coef.groupby("outcome").size().eq(164).all(), "expected 164 firms per outcome"

# --- Firm-level DK / prefer-not-to-answer rate, same administered/DK logic as the
# overall table: a row counts as "administered" if exactly one of the two mutually
# exclusive constituent items is non-missing; DK if that administered value is -1.
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

dk_white = firm_dk_rates(raw, "FirmCont_favor_white", "conduct_favor_white", "pooled_favor_white")
dk_male = firm_dk_rates(raw, "FirmCont_favor_male", "conduct_favor_male", "pooled_favor_male")
dk = pd.concat([dk_white, dk_male], ignore_index=True)

merged = coef.merge(dk, on=["firm_id", "outcome"], how="inner")
print(merged.groupby("outcome").size())
merged.to_csv(f"{OUT_DIR}/firm_dk_vs_likert.csv", index=False)

labels = {
    "pooled_favor_white": "Discrimination Black (Pooled) — Likert avg",
    "pooled_favor_male": "Discrimination Female (Pooled) — Likert avg",
}

fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
for ax, outcome in zip(axes, ["pooled_favor_white", "pooled_favor_male"]):
    sub = merged[merged["outcome"] == outcome]
    corr = sub["likert_avg"].corr(sub["dk_rate"])
    ax.scatter(sub["dk_rate"], sub["likert_avg"], alpha=0.7, s=22, color="darkorange", edgecolor="none")
    ax.set_xlabel("Firm-level DK / prefer-not-to-answer rate")
    ax.set_ylabel(labels[outcome])
    ax.set_title(f"{outcome}\ncorr = {corr:.3f}, n = {len(sub)} firms")
    ax.spines[["top", "right"]].set_visible(False)

fig.tight_layout()
fig.savefig(f"{OUT_DIR}/firm_dk_rate_vs_likert_avg_scatter.png", dpi=200, facecolor="white")
print("wrote", f"{OUT_DIR}/firm_dk_rate_vs_likert_avg_scatter.png")
