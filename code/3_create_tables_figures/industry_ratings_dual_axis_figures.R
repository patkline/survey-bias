# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: For each survey measure, plot the Likert and Borda ratings for (1) the 25 highest and 25 lowest
# firms ranked by the Borda empirical Bayes within-industry deviation and (2) every industry
#
# Created: Nico Rotundo 2026-07-03
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Construct the firm-to-industry crosswalk
# -----------------------------------------------------------------------------------------------------------------------------
# Load the firm-to-industry crosswalk from the processed survey data, keeping distinct firm_id x industry code x industry name combinations
firm_industry_crosswalk <- read.csv(file.path(processed, "long_survey_final.csv")) |> dplyr::distinct(firm_id, aer_naics2, aer_naics2_name)

# One industry per firm, none missing
stopifnot(!anyDuplicated(firm_industry_crosswalk$firm_id), !anyNA(firm_industry_crosswalk))

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level rating estimates for industry aggregation
# -----------------------------------------------------------------------------------------------------------------------------
# Load the full-sample firm-level coefficient sheet
firm_level_rating_estimates <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "Coefficients")

# Uniquely identified by subset x aggregation model x survey measure x entity type x entity, none missing
stopifnot(!anyDuplicated(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]))

# Keep firm-level observations
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(entity_type == "Firm")

# Keep the full-sample estimates
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(subset == "all")

# Keep the non-recentered OLS and Borda observations i.e., the raw firm-level ratings
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

# Keep the survey measures plotted
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger"))

# Should be 164 firms x 2 aggregation methods x 3 survey measures = 984 observations remaining
stopifnot(nrow(firm_level_rating_estimates) == 164 * 2 * 3)

# Keep the firms with number of jobs i.e., the 97 audit-sample firms carrying the industry aggregation weights
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(!is.na(njobs))

# Should be 97 firms x 2 aggregation methods x 3 survey measures = 582 observations remaining
stopifnot(nrow(firm_level_rating_estimates) == 97 * 2 * 3)

# Employment counts should be positive so every industry has positive total weight
stopifnot(all(firm_level_rating_estimates$njobs > 0))

# Rating estimates should be non-missing
stopifnot(!anyNA(firm_level_rating_estimates$estimate))

# Keep necessary variables
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::select(entity_id, entity, model, outcome, estimate, njobs)

# Rename variables to be more descriptive
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::rename(firm_id = entity_id, firm = entity, aggregation_method = model, survey_measure = outcome, rating_estimate = estimate)

# Attach each firm's industry code and industry name
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::left_join(firm_industry_crosswalk, by = "firm_id")

# Every firm should have an industry
stopifnot(!anyNA(firm_level_rating_estimates$aer_naics2))

# Sort by survey measure, aggregation method, and firm, fixing the row order the aggregation matrices are built on
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::arrange(survey_measure, aggregation_method, firm_id)

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level robust covariances for industry aggregation
# -----------------------------------------------------------------------------------------------------------------------------
# Firm ids sorted ascending, the order every estimate vector and covariance matrix is aligned to
firm_id_vector <- sort(unique(firm_level_rating_estimates$firm_id))

# Should be the 97 firms with number of jobs
stopifnot(length(firm_id_vector) == 97)

# Open the full-sample robust covariance sheet
firm_level_robust_covariances <- arrow::open_dataset(parquet_sheet_path(file.path(intermediate, "Full_Sample"), "rcov"))

# Keep the full-sample estimates
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(subset == "all")

# Keep the non-recentered OLS and Borda observations i.e., the raw firm-level covariances
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(model %in% c("OLS_not_recentered", "Borda_not_recentered"))

# Keep the survey measures plotted
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger"))

# Keep the covariance entries among the firms with number of jobs
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(entity_id_i %in% firm_id_vector, entity_id_j %in% firm_id_vector)

# Collect the filtered covariance rows
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::collect()

# Should be 97 firms x 97 firms x 2 aggregation methods x 3 survey measures = 56454 firm pairs remaining
stopifnot(nrow(firm_level_robust_covariances) == 97 * 97 * 2 * 3)

# Uniquely identified by aggregation model x survey measure x firm pair, none missing
stopifnot(!anyDuplicated(firm_level_robust_covariances[c("model", "outcome", "entity_id_i", "entity_id_j")]), !anyNA(firm_level_robust_covariances))

# Keep necessary variables
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::select(model, outcome, entity_id_i, entity_id_j, rcov)

# Rename variables to be more descriptive
firm_level_robust_covariances <- firm_level_robust_covariances |> dplyr::rename(aggregation_method = model, survey_measure = outcome, firm_id_i = entity_id_i, firm_id_j = entity_id_j, robust_covariance = rcov)

# -----------------------------------------------------------------------------------------------------------------------------
# Construct the industry aggregation matrices
# -----------------------------------------------------------------------------------------------------------------------------
# Industry codes sorted ascending, the order every industry-level object is aligned to
industry_id_vector <- sort(unique(firm_industry_crosswalk$aer_naics2[firm_industry_crosswalk$firm_id %in% firm_id_vector]))

# Define the firm-by-industry indicator matrix i.e., one row per firm, a 1 in its industry's column
industry_indicator_matrix <- 1 * outer(firm_industry_crosswalk$aer_naics2[match(firm_id_vector, firm_industry_crosswalk$firm_id)], industry_id_vector, FUN = "==")

# Label the indicator matrix rows by firm and columns by industry
dimnames(industry_indicator_matrix) <- list(as.character(firm_id_vector), as.character(industry_id_vector))

# Every firm should sit in exactly one industry
stopifnot(all(rowSums(industry_indicator_matrix) == 1))

# Number of jobs per firm, aligned to firm_id_vector
firm_njobs_vector <- firm_level_rating_estimates |> dplyr::distinct(firm_id, njobs)

# Should be one number of jobs per firm
stopifnot(nrow(firm_njobs_vector) == length(firm_id_vector), all(firm_njobs_vector$firm_id == firm_id_vector))

# Keep the number of jobs as a vector
firm_njobs_vector <- firm_njobs_vector$njobs

# Define the weighted aggregation matrix i.e., one row per industry, holding its firms' njobs shares as averaging weights
weighted_aggregation_matrix <- solve(t(industry_indicator_matrix) %*% diag(firm_njobs_vector) %*% industry_indicator_matrix, t(industry_indicator_matrix) %*% diag(firm_njobs_vector))

# Every industry's averaging weights should sum to one
stopifnot(all(abs(rowSums(weighted_aggregation_matrix) - 1) < 1e-12))

# -----------------------------------------------------------------------------------------------------------------------------
# Aggregate the firm-level ratings to industry means and within-industry deviations
# -----------------------------------------------------------------------------------------------------------------------------
# Load the empirical Bayes shrinkage function
source(file.path(analysis, "EB_procedure.R"))

# Define dataframe to store every industry-level rating estimate
industry_rating_estimates <- data.frame()

# Define dataframe to store every firm's within-industry rating deviation
within_industry_rating_deviations <- data.frame()

# Loop over each survey measure
for (survey_measure_value in c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger")) {
    # Loop over each aggregation method
    for (aggregation_method_value in c("OLS_not_recentered", "Borda_not_recentered")) {

        # Keep this survey measure's firm-level rating estimates
        working_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(survey_measure == survey_measure_value)

        # Keep this aggregation method
        working_rating_estimates <- working_rating_estimates |> dplyr::filter(aggregation_method == aggregation_method_value)

        # Check the firm_id order equals firm_id_vector
        stopifnot(nrow(working_rating_estimates) == length(firm_id_vector), all(working_rating_estimates$firm_id == firm_id_vector))

        # Keep this survey measure's firm-pair robust covariances
        working_robust_covariances <- firm_level_robust_covariances |> dplyr::filter(survey_measure == survey_measure_value)

        # Keep this aggregation method
        working_robust_covariances <- working_robust_covariances |> dplyr::filter(aggregation_method == aggregation_method_value)

        # Should be 97 firms x 97 firms = 9409 firm pairs
        stopifnot(nrow(working_robust_covariances) == 97 * 97)

        # Define a matrix of 0s to hold this cell's robust covariance, rows and columns ordered by firm_id_vector
        firm_robust_covariance_matrix <- matrix(0, nrow = length(firm_id_vector), ncol = length(firm_id_vector), dimnames = list(as.character(firm_id_vector), as.character(firm_id_vector)))

        # Populate the robust covariance matrix from the firm-pair rows
        firm_robust_covariance_matrix[cbind(as.character(working_robust_covariances$firm_id_i), as.character(working_robust_covariances$firm_id_j))] <- working_robust_covariances$robust_covariance
    }
}