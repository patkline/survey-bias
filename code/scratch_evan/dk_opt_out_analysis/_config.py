"""Shared paths for the dk_opt_out_analysis scripts. See _config.R for the same
notes on why this doesn't go through code/globals.R, and why MIRROR_ROOT points
at a Dropbox path rather than the repo's data/processed/ (which was found stale).
"""
import os

REPO_ROOT = os.environ.get(
    "DK_ANALYSIS_REPO_ROOT",
    "/Users/evanrose/Documents/GitHub/survey-bias",
)

MIRROR_ROOT = os.environ.get(
    "DK_ANALYSIS_MIRROR_ROOT",
    "/Users/evanrose/Dropbox/GSI-GSR-Reader/Audit/Survey/consolidated_code/"
    "github_data_and_output_mirrors/github_data_and_output_mirrors_loosen_sample_filters",
)

RESULTS_DIR = os.path.join(REPO_ROOT, "code/scratch_evan/dk_opt_out_analysis/results")
