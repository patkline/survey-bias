# DK / opt-out analysis (2026-07-13)

Exploratory work validating the Table 4 Wald/CMD bootstrap and investigating
"don't know / prefer not to answer" (raw code `-1`) responses: which firms and
respondents opt out, whether it predicts firm-level beliefs, and what happens to
the EIV regressions if you restrict to low-opt-out firms. Scratch work, not part
of the pipeline -- kept here (rather than in the session scratchpad) so it can be
resumed later.

## Setup

**R**: this machine's `.Rprofile` auto-sources `code/globals.R`, which fails for
any user not in `dropbox_roots_by_user` (see below) or on any machine without
`renv` fully synced. Every script here is written to run standalone instead:

```
Rscript --no-init-file code/scratch_evan/dk_opt_out_analysis/<script>.R
```

Needs the `igraph` package (used by `leave_in_connected_set()`); install once with
`install.packages("igraph")`. `arrow` is NOT needed -- parquet reads go through
the Python side instead (R's `arrow` has no prebuilt binary for this R version
and takes ~20-40 min to compile from source; not worth it unless you specifically
want to read parquet from R).

**Python**: a local venv, since the pyenv-managed `python3` on this machine refuses
global `pip install`:

```
python3 -m venv .venv
.venv/bin/pip install -r code/scratch_evan/dk_opt_out_analysis/requirements.txt
```

**Paths**: `_config.R` / `_config.py` define `repo_root` and `mirror_root`
(overridable via `DK_ANALYSIS_REPO_ROOT` / `DK_ANALYSIS_MIRROR_ROOT` env vars).
Defaults match this machine/branch.

## Key background findings (context for anyone picking this up)

- **`data/processed/*.csv` committed in this git repo is stale** (git history:
  last touched 2026-02-19), predating both the industry-covariate rework (Jun 6)
  and the sample-filter loosening (Jul 10, #80) on this branch. It has 6,515
  respondents vs. the current 7,015.
- **The actual current data lives on Dropbox**, at the path in `mirror_root`
  above -- a branch-scoped mirror (`github_data_and_output_mirrors_loosen_sample_filters`)
  per `code/globals.R`'s `data_and_output_storage_location == "dropbox"` switch.
  `evanrose` isn't in `dropbox_roots_by_user`, but the Dropbox folder is synced
  locally anyway (shared folder added under a personal Dropbox account, at a
  path that doesn't match the hardcoded convention).
- **`prepare_bootstrap_score_input()` + `compute_weighted_firm_mean_result()`**
  (extracted from `cross_sample_signal_corr.R`) reproduce the stored
  `Coefficients.parquet` firm-level estimates **exactly** (max|diff| = 0.000000
  across OLS/Borda x race/gender) when run against the current mirror's
  `long_survey_final.csv`. See `verify_reconstruction.R` /
  `reconstruction_vs_stored_comparison.csv`.

## Scripts

- **`bootstrap_calibration_check.R`** -- calibration check for the Table 4
  Wald/CMD bootstrap: repeatedly splits real respondents into two RANDOM groups
  (null true by construction) and checks the bootstrap p-values are ~Uniform(0,1).
  Extracts the actual production functions from `cross_sample_signal_corr.R` via
  `parse()` + selective `eval()` (skips that script's Dropbox-dependent top-level
  code). Config via `CALIBRATION_N_SPLITS` / `CALIBRATION_BOOTSTRAP_REPS` env vars
  (defaults 100 splits x 99 reps, ~30 min). **Result:** bootstrap p-values pass
  (KS test vs Uniform: p=0.39 Wald, p=0.54 CMD); naive chi-squared p-values fail
  decisively (KS p=0.006 / 0.010), over-rejecting ~1.6-2x at nominal thresholds --
  i.e. the bootstrap correction is doing real, necessary work.
- **`verify_reconstruction.R`** -- the exact-match check described above.
- **`compare_sample_filters.R`** -- reproduces the pre-#80 vs. post-#80
  `prepare_pltree_data()` straightline filter from raw data to quantify how much
  the Jul 10 filter-loosening changed eligible-respondent counts.
- **`overall_averages.R`** -- overall sample averages for `pooled_favor_white`/
  `pooled_favor_male` at three levels (raw 1-5 scale, respondent-firm score scale,
  firm-level equal-weighted) -- these differ from each other and that's expected,
  not a bug (see script comments).
- **`borda_ref_check.R`** -- confirms non-reference firms average *exactly* 0.5000
  in the Borda scoring scheme (the affine rescaling in `borda_score.R` has 0.5 as
  a fixed point) while the 3 reference firms (38, 76, 90) do not, explaining why
  firm-level Borda means aren't exactly 0.5.
- **`dk_vs_likert_scatter.py`**, **`dk_vs_agg_scatter.py`** -- firm-level DK/opt-out
  rate vs. firm-level Likert/Borda average, scatter + OLS fit w/ robust (HC1) SE.
  Race: corr=0.268, slope significant (p<0.001 both methods). Gender: corr=0.094,
  slope only Borda-significant (p=0.028), Likert marginal (p=0.067).
- **`dk_predictors_regression.py`** -- linear probability model of the DK
  indicator on respondent demographics (race, gender, looking_job, fear, age_gt40,
  educ_0_1, sample, confidence_race, confidence_gend), cluster-robust SEs by
  respondent. Consistent predictors across both outcomes: fear of discrimination
  and confidence (gender) predict *less* opting out; being female predicts *more*.
  Being Black predicts less opting out on the race question specifically (no
  effect on gender).
- **`dk_propensity_score.py`** -- combines the above into one pooled regression
  (both outcomes stacked, question-type fixed effect), predicts a per-respondent
  DK propensity score, splits at the median (near-even: 3,485 vs 3,452).
- **`dk_propensity_cross_sample_corr.R`** -- runs the actual Table-4-style
  Wald/CMD bootstrap (499 reps) comparing High- vs Low-DK-propensity respondents.
  **Notable result**: for race/OLS, raw firm-level correlation between the two
  groups is only 0.643 (well below every demographic split in the real Table 4,
  all ≥0.92); Wald test rejects equal levels (p=0.002) while CMD cannot reject
  perfect correlation (p=0.166) -- same firm ranking, different overall level.
  Gender shows a much weaker, borderline version of the same pattern.
- **`eiv_below_median_dk.R`** -- runs the actual EIV regression
  (`log_dif ~ pooled_favor_white`, `log_dif_gender ~ pooled_favor_male`, OLS +
  Borda, with/without industry FE, njobs-weighted, Katz-noise-corrected) restricted
  to firms with below-median DK rate, and separately dropping just the top 10
  DK-rate firms among the 97 firms with valid administrative (`log_dif`) data.
  Recomputes the Katz noise specific to each firm subset rather than reusing the
  full-sample value (barely changes: e.g. race/OLS 0.0123 -> 0.0115).
  **Result**: gender coefficients are stable across all three samples. Race
  flips from slightly negative (-0.024, full 97 firms) to positive (+0.095,
  below-median-DK 50 firms) -- but dropping just the top-10 DK outliers barely
  moves it (-0.015), so the shift isn't outlier-driven; it only shows up under
  the full median split, and isn't statistically significant in any spec (wide
  SEs, small N). Treat as suggestive, not confirmed.

## Not yet done / possible next steps

- Within-/between-industry (`_dm_w` / `_im_w`) EIV variants on the DK-restricted
  firm set (only the firm-level `EIV_firm` spec has been run so far).
- Multiple-comparisons correction for the DK-predictor regressions (9 covariates
  x 2 outcomes = 18 tests; a couple of the significant ones are borderline).
- Re-run the calibration check against the current Dropbox-mirror
  `long_survey_final.csv` (7,015 respondents) instead of the stale git-repo copy
  it currently uses, and with split sizes matching real subgroup imbalance
  rather than 50/50.
