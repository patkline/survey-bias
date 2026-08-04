# -----------------------------------------------------------------------------------------------------------------------------
# Purpose: For each of the three discrimination measures plotted in the top/bottom firm rating figures, scatter every firm's
# raw Likert rating against its Likert average mean absolute deviation
#
# Created: Nico Rotundo 2026-08-03
# -----------------------------------------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# -----------------------------------------------------------------------------------------------------------------------------
# Clean firm-level Likert ratings for plotting
# -----------------------------------------------------------------------------------------------------------------------------
# Load the full-sample firm-level coefficient sheet
firm_level_rating_estimates <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "Coefficients")

# Uniquely identified by subset x aggregation model x survey measure x entity type x entity, none missing
stopifnot(!anyDuplicated(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]), !anyNA(firm_level_rating_estimates[c("subset", "model", "outcome", "entity_type", "entity_id")]))

# Keep firm-level observations
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(entity_type == "Firm")

# Keep the full-sample estimates
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(subset == "all")

# Keep the non-recentered OLS observations i.e., the raw firm-level Likert ratings
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(model == "OLS_not_recentered")

# Keep the survey measures plotted i.e., the pooled race, pooled gender, and conduct arm age discrimination measures
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::filter(outcome %in% c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger"))

# Keep the firm identifier, survey measure, and the raw Likert rating
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::select(firm_id = entity_id, outcome, likert_rating = estimate)

# Every survey measure carries all 164 firms, none missing a rating
stopifnot(table(firm_level_rating_estimates$outcome) == 164, !anyNA(firm_level_rating_estimates))

# -----------------------------------------------------------------------------------------------------------------------------
# Merge on the firm-level Likert average mean absolute deviations
# -----------------------------------------------------------------------------------------------------------------------------
# Load the firm-level Likert AMAD sheet; one row per survey measure x firm
firm_level_amad_estimates <- read_parquet_sheet(file.path(intermediate, "Full_Sample"), "belief_likert_amad_firm")

# Uniquely identified by survey measure x firm, none missing
stopifnot(!anyDuplicated(firm_level_amad_estimates[c("outcome", "firm_id")]), !anyNA(firm_level_amad_estimates[c("outcome", "firm_id")]))

# Keep the firm identifier, survey measure, and the AMAD
firm_level_amad_estimates <- firm_level_amad_estimates |> dplyr::select(firm_id, outcome, amad)

# Attach each firm's AMAD to its Likert rating within survey measure
# relationship = "one-to-one" enforces both sides are unique on survey measure x firm; errors otherwise
firm_level_rating_estimates <- firm_level_rating_estimates |> dplyr::inner_join(firm_level_amad_estimates, by = c("firm_id", "outcome"), relationship = "one-to-one")

# The join is one-to-one, so all 164 firms survive for each of the three survey measures, none missing an AMAD
stopifnot(nrow(firm_level_rating_estimates) == 164 * 3, table(firm_level_rating_estimates$outcome) == 164, !anyNA(firm_level_rating_estimates))

# -----------------------------------------------------------------------------------------------------------------------------
# Plot every firm's Likert rating against its AMAD, one figure per survey measure
# -----------------------------------------------------------------------------------------------------------------------------
for (survey_measure in c("pooled_favor_white", "pooled_favor_male", "conduct_favor_younger")) {

  # Name the survey measure plotted, for the horizontal axis label
  survey_measure_label <- c(pooled_favor_white = "Discrimination Black (Pooled)", pooled_favor_male = "Discrimination Female (Pooled)", conduct_favor_younger = "Discrimination Older (Conduct)")[[survey_measure]]

  # Define the scatterplot of firm AMADs on firm Likert ratings
  firm_likert_amad_scatterplot <- ggplot(firm_level_rating_estimates |> dplyr::filter(outcome == survey_measure), aes(x = likert_rating, y = amad)) +

    # Observed firm-level rating-AMAD pairs
    geom_point(color = "steelblue", size = 1.4, alpha = 0.9) +

    # Axis labels name the survey measure rated and the disagreement measure
    labs(x = paste0("Mean Likert rating, ", survey_measure_label), y = "Average mean absolute deviation (AMAD)") +

    # Theme baseline
    theme_minimal(base_size = 11) +

    # Theme adjustments
    theme(
      # No grid lines
      panel.grid = element_blank(),

      # White background
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),

      # Bottom and left axis spines, no ticks
      axis.line = element_line(color = "black"),
      axis.ticks = element_blank()
    )

  # Export the scatterplot, one file per survey measure
  ggsave(file.path(figures, paste0("firm_likert_amad_scatterplots_", survey_measure, ".png")), plot = firm_likert_amad_scatterplot, width = 10, height = 6, dpi = 300, bg = "white")
}
