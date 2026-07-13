# ------------------------------------------------------------------------------
# Purpose: Shared paths for the dk_opt_out_analysis scripts. Deliberately does NOT
# source code/globals.R --- that requires a Dropbox path registered for your user
# in dropbox_roots_by_user, and as of this analysis evanrose was not registered.
# Override via env vars if your setup differs; defaults match the machine/branch
# this analysis was built on.
#
# Created: 2026-07-13
# ------------------------------------------------------------------------------
repo_root <- Sys.getenv(
  "DK_ANALYSIS_REPO_ROOT",
  "/Users/evanrose/Documents/GitHub/survey-bias"
)

# Root of this branch's Dropbox data/output mirror (see code/globals.R's
# dropbox_roots_by_user + dropbox_data_output_mirror_relative_path). This is the
# CURRENT data used by the real pipeline; the data/processed/*.csv committed in
# the git repo itself was found to be stale (Feb 2026 vintage) relative to it.
mirror_root <- Sys.getenv(
  "DK_ANALYSIS_MIRROR_ROOT",
  "/Users/evanrose/Dropbox/GSI-GSR-Reader/Audit/Survey/consolidated_code/github_data_and_output_mirrors/github_data_and_output_mirrors_loosen_sample_filters"
)

results_dir <- file.path(repo_root, "code/scratch_evan/dk_opt_out_analysis/results")
