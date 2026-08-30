# ------------------------------------------------------------------------------
# Within-respondent cross-question AMAD analysis
# ------------------------------------------------------------------------------
# Applies the belief-summary AMAD idea at the person-firm level across
# questions: instead of pairing two respondents on the same question
# (belief_summary_amad.R), each comparison pairs one respondent's answers to
# two questions about the same firm.
#   - Likert: for each firm, the mean across respondents of the absolute
#     difference between their two 1-5 ratings of that firm, averaged equally
#     across firms.
#   - Borda: for each firm j, the mean across respondents and non-anchor
#     opponents k of the absolute difference between the respondent's pairwise
#     win indicator w in {0, 0.5, 1} for (j, k) under the two questions,
#     averaged equally across firms.
# Because both members of a comparison come from the same respondent, the
# framing-arm matching the between-respondent AMAD enforces through pairing
# cells holds automatically; this is asserted, not assumed.
#
# These statistics are directly comparable to the between-respondent AMADs in
# belief_amad_summary: self-disagreement across two questions measuring the
# same construct versus cross-person disagreement on a single question.
#
# Writes one Full_Sample intermediate parquet sheet:
#   - belief_amad_within_respondent
#
# Created: Evan K. Rose 2026-08-29
# ------------------------------------------------------------------------------

BELIEF_AMAD_WITHIN_RESPONDENT_SHEET <- "belief_amad_within_respondent"

# Same-construct question pairs; cross-construct pairs sit on different scale
# levels, so their absolute differences would not measure self-disagreement
BELIEF_AMAD_WITHIN_RESPONDENT_PAIRS <- list(
  c("FirmHire_favor_white", "FirmCont_favor_white"),
  c("FirmHire_favor_male", "FirmCont_favor_male")
)

# ------------------------------------------------------------------------------
# Join one respondent x firm panel per question into common cells, asserting
# each respondent answered both questions under the same framing arm
# ------------------------------------------------------------------------------
join_within_respondent_ratings <- function(prep_1, prep_2) {
  extract_ratings <- function(prep) {
    prep$data_rating_long %>%
      dplyr::transmute(
        resp_id = as.character(.data$resp_id),
        firm_id = suppressWarnings(as.integer(.data$firm_id)),
        rating = suppressWarnings(as.numeric(.data$rating)),
        pairing_cell = as.character(.data$pairing_cell)
      ) %>%
      dplyr::filter(
        !is.na(.data$resp_id), !is.na(.data$firm_id),
        is.finite(.data$rating), !is.na(.data$pairing_cell)
      )
  }
  ratings_1 <- extract_ratings(prep_1)
  ratings_2 <- extract_ratings(prep_2)
  if (anyDuplicated(ratings_1[c("resp_id", "firm_id")]) ||
      anyDuplicated(ratings_2[c("resp_id", "firm_id")])) {
    stop("Within-respondent AMAD input has duplicated respondent-firm ratings")
  }

  joined <- dplyr::inner_join(
    ratings_1, ratings_2,
    by = c("resp_id", "firm_id"), suffix = c("_1", "_2")
  )
  if (nrow(joined) == 0L) {
    stop("Within-respondent AMAD found no respondent-firm cells answering both questions")
  }

  # The framing arm is randomized at the respondent level, so one respondent's
  # two answers must share it; a mismatch means the comparison mixes framings
  if (any(joined$pairing_cell_1 != joined$pairing_cell_2)) {
    stop("Within-respondent AMAD found respondents with mismatched framing arms across questions")
  }

  joined
}

# ------------------------------------------------------------------------------
# Likert: firm-level mean absolute within-respondent cross-question difference,
# averaged equally across firms, with respondent-clustered influence SEs
# ------------------------------------------------------------------------------
compute_within_respondent_cross_question_likert_amad <- function(
    prep_1, prep_2,
    expected_firm_count = BELIEF_AMAD_EXPECTED_FIRM_COUNT) {
  joined <- join_within_respondent_ratings(prep_1, prep_2)
  joined$absolute_difference <- abs(joined$rating_1 - joined$rating_2)

  respondent_ids <- unique(joined$resp_id)
  respondent_count <- length(respondent_ids)
  firm_ids <- sort(unique(joined$firm_id))
  if (length(firm_ids) != expected_firm_count) {
    stop("Within-respondent Likert AMAD expected ", expected_firm_count,
         " firms but found ", length(firm_ids))
  }

  firm_stats <- joined %>%
    dplyr::group_by(.data$firm_id) %>%
    dplyr::summarise(
      cell_count = dplyr::n(),
      amad = mean(.data$absolute_difference),
      probability_different = mean(.data$absolute_difference > 0),
      .groups = "drop"
    )
  stopifnot(all(firm_stats$cell_count > 0))

  # Linearization of the firm-equal-weighted two-stage mean: respondent i's
  # cell at firm j perturbs that firm's mean by (|d_ij| - amad_j) / n_j
  respondent_influences <- joined %>%
    dplyr::left_join(
      firm_stats %>% dplyr::select("firm_id", firm_amad = "amad", "cell_count"),
      by = "firm_id"
    ) %>%
    dplyr::mutate(
      influence = respondent_count *
        (.data$absolute_difference - .data$firm_amad) / .data$cell_count
    ) %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarise(
      influence = sum(.data$influence) / expected_firm_count,
      .groups = "drop"
    )

  amad <- mean(firm_stats$amad)
  probability_different <- mean(firm_stats$probability_different)
  list(
    summary = c(
      estimate = amad,
      se = belief_amad_influence_se(
        respondent_influences$influence, respondent_count
      ),
      probability_different = probability_different,
      gap_given_different = amad / probability_different,
      firm_count = nrow(firm_stats)
    ),
    firms = firm_stats,
    responses = nrow(joined),
    respondents = respondent_count
  )
}

# ------------------------------------------------------------------------------
# Borda: firm-level mean absolute within-respondent cross-question difference
# in pairwise win indicators, averaged equally across firms, with
# respondent-clustered influence SEs
# ------------------------------------------------------------------------------
compute_within_respondent_cross_question_borda_amad <- function(
    prep_1, prep_2,
    anchor_firm_ids = BELIEF_AMAD_ANCHOR_FIRM_IDS,
    expected_firm_count = BELIEF_AMAD_EXPECTED_FIRM_COUNT,
    # Same-construct pairs share one framing arm per respondent and must cover
    # every firm; cross-construct pairs pool arms (each question is already
    # arm-harmonized in the data build) and may miss firms in thin overlaps
    require_matching_pairing_cells = TRUE,
    allow_missing_firms = FALSE,
    # Keep only firm pairs the respondent strictly orders under BOTH questions,
    # the within-respondent analog of the paper's require_different_ratings
    # convention; with w in {0, 1} the AMAD equals the disagree share
    require_strict_orderings = FALSE,
    # "firm" mirrors the paper's between-respondent AMAD (pool within firm,
    # equal-weight across firms); "respondent" computes each respondent's own
    # statistic over their comparisons and equal-weights across respondents,
    # the natural unit for a within-respondent measure
    aggregate_by = c("firm", "respondent")) {
  aggregate_by <- match.arg(aggregate_by)
  extract_rankings <- function(prep) {
    prep$data_wide_pltree %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with("firm"),
        names_to = "firm_column",
        values_to = "rank"
      ) %>%
      dplyr::transmute(
        resp_id = as.character(.data$resp_id),
        pairing_cell = as.character(.data$pairing_cell),
        firm_id = suppressWarnings(as.integer(sub("^firm", "", .data$firm_column))),
        rank = suppressWarnings(as.numeric(.data$rank))
      ) %>%
      dplyr::filter(
        !is.na(.data$resp_id), !is.na(.data$pairing_cell),
        !is.na(.data$firm_id), is.finite(.data$rank), .data$rank > 0
      )
  }
  rankings_1 <- extract_rankings(prep_1)
  rankings_2 <- extract_rankings(prep_2)
  if (anyDuplicated(rankings_1[c("resp_id", "firm_id")]) ||
      anyDuplicated(rankings_2[c("resp_id", "firm_id")])) {
    stop("Within-respondent Borda AMAD input has duplicated respondent-firm rankings")
  }

  # Keep the respondent x firm cells ranked under both questions; an empty join
  # means the two question modules were never assigned to the same respondent
  joined <- dplyr::inner_join(
    rankings_1, rankings_2,
    by = c("resp_id", "firm_id"), suffix = c("_1", "_2")
  )
  if (nrow(joined) == 0L) {
    return(NULL)
  }
  if (require_matching_pairing_cells && any(joined$pairing_cell_1 != joined$pairing_cell_2)) {
    stop("Within-respondent Borda AMAD found respondents with mismatched framing arms across questions")
  }

  respondent_ids <- unique(joined$resp_id)
  respondent_count <- length(respondent_ids)
  firm_ids <- sort(unique(joined$firm_id))
  if (!allow_missing_firms) {
    if (length(firm_ids) != expected_firm_count) {
      stop("Within-respondent Borda AMAD expected ", expected_firm_count,
           " firms but found ", length(firm_ids))
    }
    missing_anchors <- setdiff(anchor_firm_ids, firm_ids)
    if (length(missing_anchors)) {
      stop("Within-respondent Borda AMAD anchor IDs missing from data: ",
           paste(missing_anchors, collapse = ", "))
    }
  }

  # One row per respondent x ordered firm pair (j, k), k non-anchor, with the
  # respondent's win indicator for (j, k) under each question
  win_indicator <- function(rank_j, rank_k) {
    dplyr::case_when(rank_j < rank_k ~ 1, rank_j == rank_k ~ 0.5, TRUE ~ 0)
  }
  comparisons <- merge(
    joined, joined, by = "resp_id",
    suffixes = c("_j", "_k"), sort = FALSE
  ) %>%
    dplyr::filter(
      .data$firm_id_j != .data$firm_id_k,
      !.data$firm_id_k %in% anchor_firm_ids,
      !.env$require_strict_orderings |
        (.data$rank_1_j != .data$rank_1_k & .data$rank_2_j != .data$rank_2_k)
    ) %>%
    dplyr::mutate(
      absolute_difference = abs(
        win_indicator(.data$rank_1_j, .data$rank_1_k) -
          win_indicator(.data$rank_2_j, .data$rank_2_k)
      )
    )
  if (nrow(comparisons) == 0L) {
    return(NULL)
  }

  # Respondent aggregation: each respondent's own statistic over their
  # comparisons, equal-weighted across the respondents who have any. The
  # equal-weighted mean of iid respondent statistics has influence equal to the
  # respondent statistic itself, so the same SE helper applies.
  if (aggregate_by == "respondent") {
    respondent_stats <- comparisons %>%
      dplyr::group_by(.data$resp_id) %>%
      dplyr::summarise(
        comparison_count = dplyr::n(),
        amad = mean(.data$absolute_difference),
        probability_different = mean(.data$absolute_difference > 0),
        .groups = "drop"
      )
    stopifnot(nrow(respondent_stats) > 0, all(respondent_stats$comparison_count > 0))

    amad <- mean(respondent_stats$amad)
    probability_different <- mean(respondent_stats$probability_different)
    return(list(
      summary = c(
        estimate = amad,
        se = belief_amad_influence_se(respondent_stats$amad, nrow(respondent_stats)),
        probability_different = probability_different,
        probability_different_se = belief_amad_influence_se(
          respondent_stats$probability_different, nrow(respondent_stats)
        ),
        gap_given_different = amad / probability_different,
        firm_count = dplyr::n_distinct(comparisons$firm_id_j)
      ),
      respondents_stats = respondent_stats,
      responses = nrow(joined),
      # Respondents without any qualifying comparison have no defined statistic
      # and are excluded from the equal-weighted average
      respondents = nrow(respondent_stats)
    ))
  }

  firm_stats <- comparisons %>%
    dplyr::group_by(firm_id = .data$firm_id_j) %>%
    dplyr::summarise(
      comparison_count = dplyr::n(),
      amad = mean(.data$absolute_difference),
      probability_different = mean(.data$absolute_difference > 0),
      .groups = "drop"
    )
  if (!allow_missing_firms && nrow(firm_stats) != expected_firm_count) {
    stop("Within-respondent Borda AMAD is undefined for a firm without comparisons")
  }
  stopifnot(nrow(firm_stats) > 0, all(firm_stats$comparison_count > 0))
  averaging_firm_count <- nrow(firm_stats)

  # Linearization of the firm-equal-weighted mean over respondent x opponent
  # comparisons at each firm
  respondent_influences <- comparisons %>%
    dplyr::group_by(.data$resp_id, firm_id = .data$firm_id_j) %>%
    dplyr::summarise(
      respondent_abs_sum = sum(.data$absolute_difference),
      respondent_comparison_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      firm_stats %>% dplyr::select("firm_id", firm_amad = "amad", "comparison_count"),
      by = "firm_id"
    ) %>%
    dplyr::mutate(
      influence = respondent_count *
        (.data$respondent_abs_sum -
           .data$firm_amad * .data$respondent_comparison_count) /
        .data$comparison_count
    ) %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarise(
      influence = sum(.data$influence) / averaging_firm_count,
      .groups = "drop"
    ) %>%
    # Respondents whose common firms yield no pairwise comparison still count,
    # with zero influence, mirroring compute_belief_borda_amad()
    dplyr::right_join(
      data.frame(resp_id = respondent_ids, stringsAsFactors = FALSE),
      by = "resp_id"
    ) %>%
    dplyr::mutate(influence = dplyr::coalesce(.data$influence, 0))

  amad <- mean(firm_stats$amad)
  probability_different <- mean(firm_stats$probability_different)
  list(
    summary = c(
      estimate = amad,
      se = belief_amad_influence_se(
        respondent_influences$influence, respondent_count
      ),
      probability_different = probability_different,
      gap_given_different = amad / probability_different,
      firm_count = averaging_firm_count
    ),
    firms = firm_stats,
    responses = nrow(joined),
    respondents = respondent_count
  )
}

# ------------------------------------------------------------------------------
# Borda-only within-respondent AMAD for every pair of the heatmap survey
# measures. The Borda win indicator is scale-free, so cross-construct pairs are
# meaningful: the AMAD measures how differently one respondent orders the same
# firm pair under the two questions. Statistics are computed per respondent
# over their own comparisons and equal-weighted across respondents.
# Module-infeasible pairs (never assigned to the same respondent) produce NA
# rows.
# ------------------------------------------------------------------------------
run_within_respondent_borda_amad_pairs_analysis <- function(
    survey_data,
    outcomes,
    output_dir = file.path(intermediate, "Full_Sample"),
    sheet_name = "belief_amad_within_respondent_borda_pairs") {
  stopifnot(length(outcomes) >= 2L, !anyDuplicated(outcomes))

  # Prepare each outcome once and reuse across its pairs
  outcome_preps <- setNames(vector("list", length(outcomes)), outcomes)
  for (outcome in outcomes) {
    message("Preparing outcome for Borda AMAD pairs: ", outcome)
    prep <- prepare_pltree_data(
      survey_data, outcome, subgroup_var = NULL, subgroup_filter = NULL
    )
    outcome_preps[[outcome]] <- add_belief_amad_pairing_cells(prep, survey_data, outcome)
  }

  outcome_pairs <- combn(outcomes, 2, simplify = FALSE)
  summary_rows <- vector("list", length(outcome_pairs))

  for (position in seq_along(outcome_pairs)) {
    outcome_1 <- outcome_pairs[[position]][[1]]
    outcome_2 <- outcome_pairs[[position]][[2]]
    message("Computing within-respondent Borda AMAD: ", outcome_1, " x ", outcome_2)

    borda <- compute_within_respondent_cross_question_borda_amad(
      outcome_preps[[outcome_1]], outcome_preps[[outcome_2]],
      # Cross-construct pairs legitimately span framing arms within respondent
      require_matching_pairing_cells = FALSE,
      allow_missing_firms = TRUE,
      aggregate_by = "respondent"
    )

    # Strict-ordering variant: only firm pairs the respondent strictly orders
    # under both questions, the paper's disagree-share convention
    borda_strict <- compute_within_respondent_cross_question_borda_amad(
      outcome_preps[[outcome_1]], outcome_preps[[outcome_2]],
      require_matching_pairing_cells = FALSE,
      allow_missing_firms = TRUE,
      require_strict_orderings = TRUE,
      aggregate_by = "respondent"
    )

    # With strict orderings the win-indicator AMAD and the disagree share coincide
    if (!is.null(borda_strict)) {
      stopifnot(isTRUE(all.equal(
        unname(borda_strict$summary["estimate"]),
        unname(borda_strict$summary["probability_different"])
      )))
    }

    summary_rows[[position]] <- data.frame(
      outcome_1 = outcome_1,
      outcome_2 = outcome_2,
      responses = if (is.null(borda)) 0L else borda$responses,
      respondents = if (is.null(borda)) 0L else borda$respondents,
      borda_amad = if (is.null(borda)) NA_real_ else unname(borda$summary["estimate"]),
      borda_amad_se = if (is.null(borda)) NA_real_ else unname(borda$summary["se"]),
      borda_probability_different =
        if (is.null(borda)) NA_real_ else unname(borda$summary["probability_different"]),
      borda_gap_given_different =
        if (is.null(borda)) NA_real_ else unname(borda$summary["gap_given_different"]),
      firm_count = if (is.null(borda)) 0L else unname(borda$summary["firm_count"]),
      borda_strict_disagree_share =
        if (is.null(borda_strict)) NA_real_ else unname(borda_strict$summary["estimate"]),
      borda_strict_disagree_share_se =
        if (is.null(borda_strict)) NA_real_ else unname(borda_strict$summary["se"]),
      borda_strict_firm_count =
        if (is.null(borda_strict)) 0L else unname(borda_strict$summary["firm_count"]),
      stringsAsFactors = FALSE
    )
  }

  summary_data <- dplyr::bind_rows(summary_rows)
  if (nrow(summary_data) != length(outcome_pairs) ||
      anyDuplicated(summary_data[c("outcome_1", "outcome_2")])) {
    stop("Within-respondent Borda AMAD pairs output has incomplete or duplicated pairs")
  }

  write_parquet_sheet(output_dir, sheet_name, summary_data)
  invisible(list(summary = summary_data))
}

# ------------------------------------------------------------------------------
# Run the within-respondent cross-question AMAD analysis over the question
# pairs and write the summary sheet
# ------------------------------------------------------------------------------
run_within_respondent_cross_question_amad_analysis <- function(
    survey_data,
    output_dir = file.path(intermediate, "Full_Sample"),
    question_pairs = BELIEF_AMAD_WITHIN_RESPONDENT_PAIRS) {
  summary_rows <- vector("list", length(question_pairs))

  for (position in seq_along(question_pairs)) {
    outcome_1 <- question_pairs[[position]][[1]]
    outcome_2 <- question_pairs[[position]][[2]]
    message("Computing within-respondent cross-question AMAD: ",
            outcome_1, " x ", outcome_2)

    prepare_outcome <- function(outcome) {
      prep <- prepare_pltree_data(
        survey_data, outcome, subgroup_var = NULL, subgroup_filter = NULL
      )
      add_belief_amad_pairing_cells(prep, survey_data, outcome)
    }
    prep_1 <- prepare_outcome(outcome_1)
    prep_2 <- prepare_outcome(outcome_2)

    likert <- compute_within_respondent_cross_question_likert_amad(prep_1, prep_2)
    borda <- compute_within_respondent_cross_question_borda_amad(prep_1, prep_2)
    if (is.null(borda)) {
      stop("Within-respondent AMAD found no overlap for ", outcome_1, " x ", outcome_2)
    }

    summary_rows[[position]] <- data.frame(
      outcome_1 = outcome_1,
      outcome_2 = outcome_2,
      responses = likert$responses,
      respondents = likert$respondents,
      likert_amad = unname(likert$summary["estimate"]),
      likert_amad_se = unname(likert$summary["se"]),
      likert_probability_different =
        unname(likert$summary["probability_different"]),
      likert_gap_given_different =
        unname(likert$summary["gap_given_different"]),
      borda_amad = unname(borda$summary["estimate"]),
      borda_amad_se = unname(borda$summary["se"]),
      borda_probability_different =
        unname(borda$summary["probability_different"]),
      borda_gap_given_different =
        unname(borda$summary["gap_given_different"]),
      firm_count = unname(likert$summary["firm_count"]),
      stringsAsFactors = FALSE
    )
  }

  summary_data <- dplyr::bind_rows(summary_rows)
  if (nrow(summary_data) != length(question_pairs) ||
      anyDuplicated(summary_data[c("outcome_1", "outcome_2")])) {
    stop("Within-respondent AMAD summary output has incomplete or duplicated pairs")
  }

  write_parquet_sheet(output_dir, BELIEF_AMAD_WITHIN_RESPONDENT_SHEET, summary_data)
  invisible(list(summary = summary_data))
}
