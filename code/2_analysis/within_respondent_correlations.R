# ------------------------------------------------------------------------------
# Purpose: Within-respondent correlations across survey measures ---
# for each measure pair, the equal-weighted average across respondents of each
# respondent's covariance between their own Likert or Borda scores across the
# firms they rated on both questions, divided by the square root of the product
# of the corresponding average within-respondent variances:
#
#   corr_c = mean_i[ cov_i(s1, s2) ] / sqrt( mean_i[ var_i(s1) ] * mean_i[ var_i(s2) ] )
#
# This ratio-of-averaged-moments is bounded in [-1, 1] by Cauchy-Schwarz (each
# respondent's moments use the same common firm set) and, unlike an average of
# per-respondent correlations, is stable on coarse scales: a respondent with
# little variance contributes proportionally small moments rather than a +/-1
# correlation, and a respondent with constant scores contributes zeros rather
# than being dropped. The average of per-respondent Pearson correlations is
# retained as a diagnostic column.
#
# These are raw correlations of individual scores: unlike the firm-level
# correlation sheets, no noise correction is (or can be) applied, because each
# respondent x firm x question cell is observed once. Transient rating noise
# attenuates the within-method cells and any common response style shared by a
# question pair inflates them; the OLS_x_Borda diagonal crosses two elicitation
# formats and so is robust to format-specific noise (though rankings are built
# from the ratings, so it sits near one by construction).
#
# Created: Evan K. Rose 2026-08-29
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Build one respondent x firm score panel per outcome and scoring method,
# using the identical score construction as construct_firm_level_estimates():
#   OLS   : flipped 1-5 Likert rating (6 - rating), higher = more preferred
#   Borda : individual normalized win share B, anchored to the reference firms
# The Borda eligible-opponent weight e is intentionally unused --- firms are
# equally weighted within each respondent's correlation.
# ------------------------------------------------------------------------------
build_within_respondent_scores <- function(data, outcomes, ref_firm_ids = c(38, 76, 90)) {

  # Prepare each outcome's long ratings and wide rankings with the pipeline's filters
  prep <- prep_outcomes(data, outcomes)

  # Initialize one score panel list per scoring method
  scores <- list(OLS = list(), Borda = list())

  for (outcome in outcomes) {

    # ---- Likert scores ----
    ratings_long <- prep$long[[outcome]]

    # Require the pipeline's long-rating layout and one row per respondent x firm
    stopifnot(all(c("resp_id", "firm_id", "rating") %in% names(ratings_long)))
    stopifnot(!anyDuplicated(ratings_long[c("resp_id", "firm_id")]), !anyNA(ratings_long$rating))

    # Flip the 1-5 rating so a higher value means a more preferred firm
    scores$OLS[[outcome]] <- data.frame(
      resp_id = as.character(ratings_long$resp_id),
      firm_id = as.integer(ratings_long$firm_id),
      score   = 6 - as.numeric(ratings_long$rating),
      stringsAsFactors = FALSE
    )

    # ---- Borda scores ----
    borda_long <- compute_borda_individual_wide(
      data_wide        = prep$wide[[outcome]],
      id_map           = prep$id_map[[outcome]],
      # Ranks are coded with 1 as best, so lower ranks win pairwise comparisons
      higher_is_better = FALSE,
      # Anchor the Borda scale to the same reference firms as the pipeline
      ref_firm_ids     = ref_firm_ids
    )

    # Require one Borda score per respondent x firm
    stopifnot(!anyDuplicated(borda_long[c("resp_id", "firm_id")]), !anyNA(borda_long$B))

    scores$Borda[[outcome]] <- data.frame(
      resp_id = as.character(borda_long$resp_id),
      firm_id = as.integer(borda_long$firm_id),
      score   = as.numeric(borda_long$B),
      stringsAsFactors = FALSE
    )
  }

  scores
}

# ------------------------------------------------------------------------------
# Compute the within-respondent correlation between two score panels: the
# equal-weighted average across respondents of each respondent's covariance
# across the firms they scored on both, divided by the square root of the
# product of the corresponding average within-respondent variances. Every
# respondent with at least min_common_firms overlapping firms contributes ---
# constant score vectors contribute zero covariance and zero variance rather
# than being dropped. The standard error treats each respondent's
# (covariance, variance, variance) triplet as one iid observation and applies
# the delta method to the ratio.
# ------------------------------------------------------------------------------
compute_within_respondent_pair <- function(scores1, scores2, min_common_firms = 2L) {

  # Sample moments need at least two firms; anything below that is a coding error
  stopifnot(is.numeric(min_common_firms), min_common_firms >= 2L)

  # Keep the respondent x firm cells scored on both questions
  merged <- dplyr::inner_join(scores1, scores2, by = c("resp_id", "firm_id"), suffix = c("_1", "_2"))

  # One row per respondent: overlap size, within-respondent moments across their
  # common firms, and the Pearson correlation retained as a diagnostic (NA when
  # either score vector is constant)
  per_respondent <- merged |>
    dplyr::group_by(resp_id) |>
    dplyr::summarise(
      n_firms = dplyr::n(),
      cov_12  = stats::cov(score_1, score_2),
      var_1   = stats::var(score_1),
      var_2   = stats::var(score_2),
      corr    = suppressWarnings(stats::cor(score_1, score_2)),
      .groups = "drop"
    )

  # Respondents entering the moment averages
  keep <- per_respondent$n_firms >= min_common_firms
  kept_moments <- per_respondent[keep, , drop = FALSE]
  n_respondents <- nrow(kept_moments)

  # Equal-weighted averages of the within-respondent moments
  average_covariance <- if (n_respondents > 0L) mean(kept_moments$cov_12) else NA_real_
  average_variance_1 <- if (n_respondents > 0L) mean(kept_moments$var_1) else NA_real_
  average_variance_2 <- if (n_respondents > 0L) mean(kept_moments$var_2) else NA_real_

  # Ratio-of-averaged-moments correlation; bounded in [-1, 1] by Cauchy-Schwarz
  # because each respondent's three moments use the same common firm set
  corr_c <- if (n_respondents > 0L && average_variance_1 > 0 && average_variance_2 > 0) {
    average_covariance / sqrt(average_variance_1 * average_variance_2)
  } else {
    NA_real_
  }

  # Delta-method standard error over the respondent-level moment triplets
  se_corr <- NA_real_
  if (is.finite(corr_c) && n_respondents > 1L) {
    moment_matrix <- as.matrix(kept_moments[, c("cov_12", "var_1", "var_2")])
    moment_covariance_of_means <- stats::cov(moment_matrix) / n_respondents
    gradient <- c(
      1 / sqrt(average_variance_1 * average_variance_2),
      -average_covariance / (2 * average_variance_1^1.5 * sqrt(average_variance_2)),
      -average_covariance / (2 * sqrt(average_variance_1) * average_variance_2^1.5)
    )
    se_corr <- sqrt(as.numeric(t(gradient) %*% moment_covariance_of_means %*% gradient))
  }

  # Diagnostic: the previous estimand, an equal-weighted average of the
  # respondent-level Pearson correlations over respondents where it is defined
  finite_correlations <- kept_moments$corr[is.finite(kept_moments$corr)]

  list(
    corr_c                       = corr_c,
    se_corr                      = se_corr,
    n_respondents                = as.integer(n_respondents),
    average_covariance           = average_covariance,
    average_variance_1           = average_variance_1,
    average_variance_2           = average_variance_2,
    mean_respondent_corr         = if (length(finite_correlations) > 0L) mean(finite_correlations) else NA_real_,
    median_respondent_corr       = if (length(finite_correlations) > 0L) stats::median(finite_correlations) else NA_real_,
    n_respondents_corr_defined   = as.integer(length(finite_correlations)),
    mean_common_firms            = if (n_respondents > 0L) mean(kept_moments$n_firms) else NA_real_,
    median_common_firms          = if (n_respondents > 0L) stats::median(kept_moments$n_firms) else NA_real_,
    n_dropped_insufficient_firms = as.integer(sum(!keep))
  )
}

# ------------------------------------------------------------------------------
# Assemble one sheet row per question pair x scoring method x subset
# ------------------------------------------------------------------------------
within_respondent_row <- function(lhs, rhs, subset, model, out, min_common_firms) {
  data.frame(
    lhs                          = lhs,
    rhs                          = rhs,
    subset                       = subset,
    model                        = model,
    corr_c                       = out$corr_c,
    se_corr                      = out$se_corr,
    n_respondents                = out$n_respondents,
    average_covariance           = out$average_covariance,
    average_variance_1           = out$average_variance_1,
    average_variance_2           = out$average_variance_2,
    mean_respondent_corr         = out$mean_respondent_corr,
    median_respondent_corr       = out$median_respondent_corr,
    n_respondents_corr_defined   = out$n_respondents_corr_defined,
    mean_common_firms            = out$mean_common_firms,
    median_common_firms          = out$median_common_firms,
    n_dropped_insufficient_firms = out$n_dropped_insufficient_firms,
    min_common_firms             = as.integer(min_common_firms),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# Write the within-respondent correlation sheet --- every unordered outcome
# pair under OLS (Likert) and Borda, plus the OLS x Borda diagonal pairing each
# outcome's Likert scores with its own Borda scores, for the full firm set and
# (when firms97 is supplied) the cells restricted to the 97 experimental firms.
# Borda scores are NOT recomputed within the restricted firm set, mirroring how
# the pipeline recenters rather than re-estimates on subset97.
# ------------------------------------------------------------------------------
write_within_respondent_correlation_sheet <- function(
    data,
    outcomes,
    output_dir,
    sheet_name = "correlation_within_respondent",
    firms97 = NULL,
    min_common_firms = 2L,
    ref_firm_ids = c(38, 76, 90)
) {
  stopifnot(length(outcomes) >= 2L, !anyDuplicated(outcomes))

  # Build the respondent x firm score panels once per outcome and scoring method
  scores <- build_within_respondent_scores(data, outcomes, ref_firm_ids = ref_firm_ids)

  # Define the firm restriction per subset
  subsets <- list(all = NULL)
  if (!is.null(firms97) && length(firms97) > 0) {
    subsets$subset97 <- sort(unique(as.integer(firms97)))
  }

  # Restrict a score panel's respondent x firm cells to a firm set
  restrict_to_firms <- function(score_panel, firm_ids) {
    if (is.null(firm_ids)) return(score_panel)
    score_panel[score_panel$firm_id %in% firm_ids, , drop = FALSE]
  }

  outcome_pairs <- combn(outcomes, 2, simplify = FALSE)

  rows <- list()
  k <- 1L

  for (subset_name in names(subsets)) {
    firm_ids <- subsets[[subset_name]]

    # Within-method cells: every unordered outcome pair under each scoring method
    for (model in c("OLS", "Borda")) {
      for (pair in outcome_pairs) {
        message("Within-respondent correlation for model = ", model, ", subset = ", subset_name,
                ", outcome1 = ", pair[[1]], ", outcome2 = ", pair[[2]])
        out <- compute_within_respondent_pair(
          restrict_to_firms(scores[[model]][[pair[[1]]]], firm_ids),
          restrict_to_firms(scores[[model]][[pair[[2]]]], firm_ids),
          min_common_firms = min_common_firms
        )
        rows[[k]] <- within_respondent_row(pair[[1]], pair[[2]], subset_name, model, out, min_common_firms)
        k <- k + 1L
      }
    }

    # Cross-method diagonal: each outcome's Likert scores against its own Borda scores
    for (outcome in outcomes) {
      message("Within-respondent correlation for model = OLS_x_Borda, subset = ", subset_name,
              ", outcome = ", outcome)
      out <- compute_within_respondent_pair(
        restrict_to_firms(scores$OLS[[outcome]], firm_ids),
        restrict_to_firms(scores$Borda[[outcome]], firm_ids),
        min_common_firms = min_common_firms
      )
      rows[[k]] <- within_respondent_row(outcome, outcome, subset_name, "OLS_x_Borda", out, min_common_firms)
      k <- k + 1L
    }
  }

  correlation_within_respondent_df <- dplyr::bind_rows(rows)

  # Assert one row per cell: pairs x two methods plus the diagonal, per subset
  n_expected_per_subset <- 2L * length(outcome_pairs) + length(outcomes)
  stopifnot(nrow(correlation_within_respondent_df) == n_expected_per_subset * length(subsets))
  stopifnot(!anyDuplicated(correlation_within_respondent_df[c("lhs", "rhs", "subset", "model")]))

  # Every computed correlation must be valid; Cauchy-Schwarz guarantees the
  # bound up to floating-point error
  finite_corr <- correlation_within_respondent_df$corr_c[is.finite(correlation_within_respondent_df$corr_c)]
  stopifnot(all(dplyr::between(finite_corr, -1 - 1e-12, 1 + 1e-12)))

  write_parquet_sheet(output_dir, sheet_name, correlation_within_respondent_df)

  invisible(correlation_within_respondent_df)
}
