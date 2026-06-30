# ------------------------------------------------------------------------------------------------
# Purpose: Scratch prototype --- EEO-1 industry-share versions of the Revelio share-controls EIV
# table (EIV_revelio_full_sample_pooled_share_controls), one table per EEO-1 job level
# (all_jobs, mid_off_manager, front_line)
#
# Created: Nico Rotundo 2026-06-22
# ------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# Source the errors-in-variables estimator
source(file.path(analysis, "eivreg.R"))

# Source the EIV build_noise_matrix and run_eiv_suite functions 
source(file.path(analysis, "eiv_functions.R"))

# Source the Revelio helpers load_revelio_firm_measures, revelio_coef_to_firm_wide, add_zero_error_controls
source(file.path(analysis, "revelio_eiv.R"))

# Redirect the Revelio-table output dir to scratch
Sys.setenv(REVELIO_EIV_TABLES_DIR = file.path(code, "scratch_nico", "eeo1_share_controls_tex"))

# Source the table builders fmt3, pull_eiv_value, make_panel_rows, write_latex_grid 
source(file.path(create_tables_figures, "eiv_revelio_composition_tables.R"))

# ------------------------------------------------------------------------------------------------
# Merge industry employment shares for {black, female} x  
# {all_jobs, mid_off_manager, front_line} onto firms
# ------------------------------------------------------------------------------------------------
# Load the firm -> industry-group-name crosswalk from the survey microdata
firm_industry_crosswalk <- readr::read_csv(file.path(processed, "long_survey_final.csv"), show_col_types = FALSE)

# Keep unique firm-industry group observations
firm_industry_crosswalk <- firm_industry_crosswalk |> dplyr::distinct(firm_id, aer_naics2_name)

# Industry-group name must be unique and non-missing within firm
stopifnot(!anyDuplicated(firm_industry_crosswalk$firm_id), !anyNA(firm_industry_crosswalk))

# Load the EEO-1 industry employment shares by demographic and job level
eeo1_industry_shares <- readr::read_csv(file.path(dump, "industry_emp_share_by_demographic_eeo1.csv"), show_col_types = FALSE)

# Rename the EEO-1 industry-title key to match the survey crosswalk
eeo1_industry_shares <- eeo1_industry_shares |> dplyr::rename(aer_naics2_name = sic_two_digit_bin_title_aer)

# Keep the industry key and the black/female shares at each job level
eeo1_industry_shares <- eeo1_industry_shares |> dplyr::select(aer_naics2_name, dplyr::starts_with("share_emp_"))

# Merge the EEO-1 industry shares onto each firm by industry-group name
eeo1_shares_by_firm <- firm_industry_crosswalk |> dplyr::left_join(eeo1_industry_shares, by = "aer_naics2_name")

# Every firm's industry must match an EEO-1 industry row
stopifnot(!anyNA(eeo1_shares_by_firm))

# Rename the firm key to the entity identifier used by the EIV coefficient dataframe
eeo1_shares_by_firm <- eeo1_shares_by_firm |> dplyr::rename(entity_id = firm_id)

# ------------------------------------------------------------------------------------------------
# Assemble the firm-level EIV input: beliefs, audit gaps, industry, job weights, and EEO-1 shares
# ------------------------------------------------------------------------------------------------
# Read the Full_Sample coefficient, variance, and covariance sheets
coef_long <- read_parquet_sheet(full_sample_dir, "Coefficients")
variance_df <- read_parquet_sheet(full_sample_dir, "variance")
covariance_df <- read_parquet_sheet(full_sample_dir, "covariance")
XXstopped here 
# Reshape firm coefficients wide, attach the Revelio measures (for the industry code) and the EEO-1 shares
coef_firm_wide <- revelio_coef_to_firm_wide(coef_long) |>
    dplyr::left_join(load_revelio_firm_measures(), by = "entity_id") |>
    dplyr::left_join(eeo1_shares_by_firm, by = "entity_id")

# Every firm in the EIV sample must carry its EEO-1 shares
stopifnot(!anyNA(dplyr::select(coef_firm_wide, dplyr::starts_with("share_emp_"))))

# ------------------------------------------------------------------------------------------------
# Run the share-controls EIV with the EEO-1 industry share as the control, once per job level
# ------------------------------------------------------------------------------------------------
# Store one EIV result dataframe per EEO-1 job level
eiv_revelio_eeo1_by_level <- list()

# Loop over EEO-1 job level
for (job_level in c("all_jobs", "mid_off_manager", "front_line")) {

    # Build one noise matrix per model, with the level's EEO-1 black and female shares as zero-error controls
    noise_mats <- setNames(vector("list", 2), c("OLS", "Borda"))
    for (model_value in c("OLS", "Borda")) {
        noise_mats[[model_value]] <- build_noise_matrix(variance_df = variance_df, covariance_df = covariance_df, outcomes = revelio_rhs_outcomes, subset_value = "subset97", model_value = model_value) |>
            add_zero_error_controls(c(paste0("share_emp_black_", job_level), paste0("share_emp_female_", job_level)))
    }

    # Run the race and gender share-controls specs using the level's EEO-1 share as the control
    eiv_revelio_eeo1_by_level[[job_level]] <- run_eiv_suite(
        regs = list(
            list(lhs = "log_dif", rhs = c("pooled_favor_white", paste0("share_emp_black_", job_level))),
            list(lhs = "log_dif_gender", rhs = c("pooled_favor_male", paste0("share_emp_female_", job_level)))
        ),
        coef_df_wide = coef_firm_wide,
        noise_mats_97 = noise_mats,
        models = c("OLS", "Borda"),
        id_col = "entity_id",
        model_col = "model",
        fe_col = "aer_naics2",
        weights_col = "njobs",
        use_fe = TRUE
    )
}

# ------------------------------------------------------------------------------------------------
# Build and write one share-controls table per EEO-1 job level (eiv_firm comes from the sourced
# composition script; the with-share industry-FE cells are blank because the industry-constant
# EEO-1 share is collinear with the industry fixed effects)
# ------------------------------------------------------------------------------------------------
# Loop over EEO-1 job level
for (job_level in c("all_jobs", "mid_off_manager", "front_line")) {

    # Race panel (black share), gender panel (female share), and the industry-FE marker row
    write_latex_grid(
        dplyr::bind_rows(
            make_panel_rows("Panel A: Race", eiv_firm, eiv_revelio_eeo1_by_level[[job_level]], "log_dif", "pooled_favor_white", paste0("share_emp_black_", job_level)),
            make_panel_rows("Panel B: Gender", eiv_firm, eiv_revelio_eeo1_by_level[[job_level]], "log_dif_gender", "pooled_favor_male", paste0("share_emp_female_", job_level)),
            tibble::as_tibble(make_value_row("Industry FE", c("", "X", "", "X", "", "X", "", "X"), identity))
        ),
        paste0("EIV_revelio_full_sample_pooled_share_controls_eeo1_", job_level, ".tex")
    )
}
