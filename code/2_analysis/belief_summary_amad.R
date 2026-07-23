# ------------------------------------------------------------------------------
# Belief-summary AMAD analysis
# ------------------------------------------------------------------------------
# Computes respondent-pair statistics used by belief_summary_ols_borda.R and
# writes one Full_Sample intermediate parquet sheet:
#   - belief_amad_summary
#
# Respondent pairs for discrimination outcomes must share the same question
# framing. Pooled outcomes must additionally share the same survey arm.
# ------------------------------------------------------------------------------

BELIEF_AMAD_SUMMARY_SHEET <- "belief_amad_summary"
BELIEF_AMAD_EXPECTED_FIRM_COUNT <- 164L
BELIEF_AMAD_ANCHOR_FIRM_IDS <- c(38L, 76L, 90L)

BELIEF_AMAD_OUTCOMES <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male", "FirmHire_favor_male", "conduct_favor_male",
  "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)

# Each named vector maps a source response column to an arm/framing cell.
BELIEF_AMAD_PAIRING_CELL_RULES <- list(
  conduct_favor_white = c(
    conduct_black = "black", conduct_white = "white"
  ),
  FirmCont_favor_white = c(
    FirmCont_black = "black", FirmCont_white = "white"
  ),
  FirmHire_favor_white = c(
    FirmHire_black = "black", FirmHire_white = "white"
  ),
  pooled_favor_white = c(
    FirmCont_black = "contact_black",
    FirmCont_white = "contact_white",
    conduct_black = "conduct_black",
    conduct_white = "conduct_white"
  ),
  conduct_favor_male = c(
    conduct_female = "female", conduct_male = "male"
  ),
  FirmCont_favor_male = c(
    FirmCont_female = "female", FirmCont_male = "male"
  ),
  FirmHire_favor_male = c(
    FirmHire_female = "female", FirmHire_male = "male"
  ),
  pooled_favor_male = c(
    FirmCont_female = "contact_female",
    FirmCont_male = "contact_male",
    conduct_female = "conduct_female",
    conduct_male = "conduct_male"
  ),
  conduct_favor_younger = c(
    conduct_older = "older", conduct_younger = "younger"
  )
)

add_belief_amad_pairing_cells <- function(prep, survey_data, outcome) {
  respondent_ids <- unique(as.character(prep$data_rating_long$resp_id))
  rules <- BELIEF_AMAD_PAIRING_CELL_RULES[[outcome]]

  if (is.null(rules)) {
    respondent_cells <- data.frame(
      resp_id = respondent_ids,
      pairing_cell = "all",
      stringsAsFactors = FALSE
    )
  } else {
    source_columns <- names(rules)
    required_columns <- c("resp_id", outcome, source_columns)
    missing_columns <- setdiff(required_columns, names(survey_data))
    if (length(missing_columns)) {
      stop("Cannot construct AMAD pairing cells for ", outcome,
           "; missing columns: ", paste(missing_columns, collapse = ", "))
    }

    relevant <- survey_data %>%
      dplyr::transmute(
        resp_id = as.character(.data$resp_id),
        outcome_value = suppressWarnings(as.numeric(.data[[outcome]])),
        dplyr::across(
          dplyr::all_of(source_columns),
          ~ suppressWarnings(as.numeric(.x))
        )
      ) %>%
      dplyr::filter(
        .data$resp_id %in% .env$respondent_ids,
        is.finite(.data$outcome_value)
      )

    cell_indicators <- vapply(
      source_columns,
      function(column) is.finite(relevant[[column]]),
      logical(nrow(relevant))
    )
    if (is.null(dim(cell_indicators))) {
      cell_indicators <- matrix(cell_indicators, ncol = 1L)
    }
    if (any(rowSums(cell_indicators) != 1L)) {
      stop("AMAD pairing cell is ambiguous or missing for ", outcome)
    }
    relevant$pairing_cell <- unname(
      rules[max.col(cell_indicators, ties.method = "first")]
    )

    respondent_cells <- relevant %>%
      dplyr::distinct(.data$resp_id, .data$pairing_cell)
    if (anyDuplicated(respondent_cells$resp_id)) {
      stop("Respondents span multiple AMAD pairing cells for ", outcome)
    }
    if (length(setdiff(respondent_ids, respondent_cells$resp_id))) {
      stop("AMAD pairing cell missing for retained respondents in ", outcome)
    }
  }

  prep$data_rating_long <- prep$data_rating_long %>%
    dplyr::mutate(resp_id = as.character(.data$resp_id)) %>%
    dplyr::left_join(respondent_cells, by = "resp_id")
  prep$data_wide_pltree <- prep$data_wide_pltree %>%
    dplyr::mutate(resp_id = as.character(.data$resp_id)) %>%
    dplyr::left_join(respondent_cells, by = "resp_id")
  if (anyNA(prep$data_rating_long$pairing_cell) ||
      anyNA(prep$data_wide_pltree$pairing_cell)) {
    stop("Failed to attach AMAD pairing cells for ", outcome)
  }
  prep
}

# For sorted x, each value's coefficient in sum_{i<i'} |x_i-x_i'| is
# 2i-n-1. This avoids constructing the full pairwise-distance matrix.
belief_amad_pairwise_abs_sum <- function(x) {
  x <- sort(suppressWarnings(as.numeric(x)))
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 2L) return(0)
  sum((2 * seq_len(n) - n - 1) * x)
}

# Return sum_{i' != i} |x_i-x_i'| for every input observation.
belief_amad_row_abs_sums <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  ok <- which(is.finite(x))
  n <- length(ok)
  if (n == 0L) return(out)
  if (n == 1L) {
    out[ok] <- 0
    return(out)
  }

  order_in_group <- order(x[ok])
  x_sorted <- x[ok][order_in_group]
  index <- seq_len(n)
  cumulative <- cumsum(x_sorted)
  left_sum <- x_sorted * (index - 1) - c(0, cumulative[-n])
  right_sum <- (cumulative[n] - cumulative) - x_sorted * (n - index)
  out[ok[order_in_group]] <- left_sum + right_sum
  out
}

belief_amad_influence_se <- function(influence, respondent_count) {
  influence <- suppressWarnings(as.numeric(influence))
  if (length(influence) != respondent_count || respondent_count < 2L ||
      any(!is.finite(influence))) {
    stop("Invalid respondent influence vector for analytical AMAD SE")
  }
  influence <- influence - mean(influence)
  sqrt(sum(influence^2) / (respondent_count * (respondent_count - 1)))
}

clean_belief_likert_ratings <- function(data_rating_long) {
  ratings <- data_rating_long %>%
    dplyr::transmute(
      resp_id = as.character(.data$resp_id),
      firm_id = suppressWarnings(as.integer(.data$firm_id)),
      rating = suppressWarnings(as.numeric(.data$rating)),
      pairing_cell = as.character(.data$pairing_cell)
    ) %>%
    dplyr::filter(
      !is.na(.data$resp_id), !is.na(.data$firm_id), is.finite(.data$rating),
      !is.na(.data$pairing_cell)
    )
  if (anyDuplicated(ratings[c("resp_id", "firm_id")])) {
    stop("Likert AMAD input has duplicated respondent-firm ratings")
  }
  ratings
}

compute_belief_likert_amad <- function(
    data_rating_long,
    expected_firm_count = BELIEF_AMAD_EXPECTED_FIRM_COUNT) {
  ratings <- clean_belief_likert_ratings(data_rating_long)
  respondent_ids <- unique(ratings$resp_id)
  respondent_count <- length(respondent_ids)
  firm_ids <- sort(unique(ratings$firm_id))
  if (length(firm_ids) != expected_firm_count) {
    stop("Likert AMAD expected ", expected_firm_count,
         " firms but found ", length(firm_ids))
  }

  firm_stats <- vector("list", length(firm_ids))
  firm_influences <- vector("list", length(firm_ids))
  for (position in seq_along(firm_ids)) {
    firm_id <- firm_ids[position]
    firm_data <- ratings[ratings$firm_id == firm_id, , drop = FALSE] %>%
      dplyr::group_by(.data$pairing_cell) %>%
      dplyr::mutate(
        cell_n = dplyr::n(),
        respondent_abs_sum = belief_amad_row_abs_sums(.data$rating)
      ) %>%
      dplyr::ungroup()

    cell_stats <- split(firm_data$rating, firm_data$pairing_cell)
    cell_stats <- dplyr::bind_rows(lapply(cell_stats, function(cell_ratings) {
      rating_counts <- table(cell_ratings)
      cell_n <- length(cell_ratings)
      data.frame(
        pair_count = cell_n * (cell_n - 1) / 2,
        equal_pair_count = sum(rating_counts * (rating_counts - 1) / 2)
      )
    }))
    pair_count <- sum(cell_stats$pair_count)
    if (!is.finite(pair_count) || pair_count <= 0) {
      stop("Likert AMAD is undefined for firm ", firm_id)
    }

    abs_diff_sum <- sum(firm_data$respondent_abs_sum) / 2
    different_pair_count <- pair_count - sum(cell_stats$equal_pair_count)
    firm_amad <- abs_diff_sum / pair_count
    firm_stats[[position]] <- data.frame(
      firm_id = firm_id,
      probability_different = different_pair_count / pair_count,
      amad = firm_amad
    )
    firm_influences[[position]] <- data.frame(
      resp_id = firm_data$resp_id,
      influence = respondent_count *
        (firm_data$respondent_abs_sum -
           firm_amad * (firm_data$cell_n - 1)) / pair_count
    )
  }

  firm_stats <- dplyr::bind_rows(firm_stats)
  respondent_influences <- dplyr::bind_rows(firm_influences) %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarise(
      influence = sum(.data$influence) / expected_firm_count,
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      data.frame(resp_id = respondent_ids, stringsAsFactors = FALSE),
      by = "resp_id"
    ) %>%
    dplyr::mutate(influence = dplyr::coalesce(.data$influence, 0))

  probability_different <- mean(firm_stats$probability_different)
  amad <- mean(firm_stats$amad)
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
    firms = firm_stats
  )
}

prepare_belief_borda_comparisons <- function(
    data_wide,
    anchor_firm_ids = BELIEF_AMAD_ANCHOR_FIRM_IDS,
    expected_firm_count = BELIEF_AMAD_EXPECTED_FIRM_COUNT,
    require_different_ratings = FALSE) {
  required_columns <- c("resp_id", "pairing_cell")
  if (length(setdiff(required_columns, names(data_wide)))) {
    stop("Borda AMAD requires resp_id and pairing_cell columns")
  }

  rankings <- data_wide %>%
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
  if (anyDuplicated(rankings[c("resp_id", "firm_id")])) {
    stop("Borda AMAD input has duplicated respondent-firm rankings")
  }

  respondent_ids <- unique(rankings$resp_id)
  firm_ids <- sort(unique(rankings$firm_id))
  if (length(firm_ids) != expected_firm_count) {
    stop("Borda AMAD expected ", expected_firm_count,
         " firms but found ", length(firm_ids))
  }
  missing_anchors <- setdiff(anchor_firm_ids, firm_ids)
  if (length(missing_anchors)) {
    stop("Borda AMAD anchor IDs missing from data: ",
         paste(missing_anchors, collapse = ", "))
  }

  comparisons <- merge(
    rankings, rankings, by = "resp_id",
    suffixes = c("_j", "_k"), sort = FALSE
  ) %>%
    dplyr::filter(
      .data$pairing_cell_j == .data$pairing_cell_k,
      .data$firm_id_j != .data$firm_id_k,
      !.data$firm_id_k %in% anchor_firm_ids,
      !.env$require_different_ratings | .data$rank_j != .data$rank_k
    ) %>%
    dplyr::mutate(
      pairing_cell = .data$pairing_cell_j,
      w = dplyr::case_when(
        .data$rank_j < .data$rank_k ~ 1,
        .data$rank_j == .data$rank_k ~ 0.5,
        TRUE ~ 0
      )
    )

  list(
    comparisons = comparisons,
    respondent_ids = respondent_ids,
    firm_ids = firm_ids
  )
}

compute_belief_borda_amad <- function(
    data_wide,
    anchor_firm_ids = BELIEF_AMAD_ANCHOR_FIRM_IDS,
    expected_firm_count = BELIEF_AMAD_EXPECTED_FIRM_COUNT,
    require_different_ratings = FALSE) {
  borda_data <- prepare_belief_borda_comparisons(
    data_wide,
    anchor_firm_ids,
    expected_firm_count,
    require_different_ratings
  )
  respondent_ids <- borda_data$respondent_ids
  respondent_count <- length(respondent_ids)

  comparisons <- borda_data$comparisons %>%
    dplyr::group_by(
      .data$firm_id_j, .data$firm_id_k, .data$pairing_cell
    ) %>%
    dplyr::mutate(
      n_jk_cell = dplyr::n(),
      respondent_abs_sum = belief_amad_row_abs_sums(.data$w)
    ) %>%
    dplyr::ungroup()

  pair_stats <- comparisons %>%
    dplyr::group_by(
      .data$firm_id_j, .data$firm_id_k, .data$pairing_cell
    ) %>%
    dplyr::summarise(
      n_jk_cell = dplyr::first(.data$n_jk_cell),
      abs_diff_sum = sum(.data$respondent_abs_sum) / 2,
      n_zero = sum(.data$w == 0),
      n_half = sum(.data$w == 0.5),
      n_one = sum(.data$w == 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pair_count = .data$n_jk_cell * (.data$n_jk_cell - 1) / 2,
      different_pair_count = .data$n_zero * .data$n_half +
        .data$n_half * .data$n_one + .data$n_zero * .data$n_one
    )

  firm_stats <- pair_stats %>%
    dplyr::group_by(.data$firm_id_j) %>%
    dplyr::summarise(
      abs_diff_sum = sum(.data$abs_diff_sum),
      pair_count = sum(.data$pair_count),
      different_pair_count = sum(.data$different_pair_count),
      comparison_count = sum(.data$n_jk_cell),
      tie_comparison_count = sum(.data$n_half),
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      data.frame(firm_id_j = borda_data$firm_ids),
      by = "firm_id_j"
    )

  undefined <- !is.finite(firm_stats$pair_count) | firm_stats$pair_count <= 0
  if (any(undefined) && !require_different_ratings) {
    stop("Borda AMAD is undefined for a firm without respondent pairs")
  }
  firm_stats <- firm_stats[!undefined, , drop = FALSE] %>%
    dplyr::transmute(
      firm_id = .data$firm_id_j,
      pair_count = .data$pair_count,
      comparison_count = .data$comparison_count,
      probability_different = .data$different_pair_count / .data$pair_count,
      amad = .data$abs_diff_sum / .data$pair_count,
      tie_share = .data$tie_comparison_count / .data$comparison_count
    )
  averaging_firm_count <- nrow(firm_stats)
  if (averaging_firm_count == 0L) {
    stop("Borda AMAD has no firms with defined respondent-pair denominators")
  }

  respondent_influences <- comparisons %>%
    dplyr::group_by(.data$resp_id, .data$firm_id_j) %>%
    dplyr::summarise(
      respondent_abs_sum = sum(.data$respondent_abs_sum),
      respondent_pair_count = sum(.data$n_jk_cell - 1),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      firm_stats %>%
        dplyr::select(
          firm_id_j = firm_id,
          firm_amad = amad,
          firm_pair_count = pair_count
        ),
      by = "firm_id_j"
    ) %>%
    dplyr::filter(is.finite(.data$firm_amad), is.finite(.data$firm_pair_count)) %>%
    dplyr::mutate(
      influence = respondent_count *
        (.data$respondent_abs_sum -
           .data$firm_amad * .data$respondent_pair_count) /
        .data$firm_pair_count
    ) %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarise(
      influence = sum(.data$influence) / averaging_firm_count,
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      data.frame(resp_id = respondent_ids, stringsAsFactors = FALSE),
      by = "resp_id"
    ) %>%
    dplyr::mutate(influence = dplyr::coalesce(.data$influence, 0))

  tie_share_respondent_influences <- comparisons %>%
    dplyr::group_by(.data$resp_id, .data$firm_id_j) %>%
    dplyr::summarise(
      respondent_tie_count = sum(.data$w == 0.5),
      respondent_comparison_count = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      firm_stats %>%
        dplyr::select(
          firm_id_j = firm_id,
          firm_tie_share = tie_share,
          firm_comparison_count = comparison_count
        ),
      by = "firm_id_j"
    ) %>%
    dplyr::filter(
      is.finite(.data$firm_tie_share),
      is.finite(.data$firm_comparison_count),
      .data$firm_comparison_count > 0
    ) %>%
    dplyr::mutate(
      influence = respondent_count *
        (.data$respondent_tie_count -
           .data$firm_tie_share * .data$respondent_comparison_count) /
        .data$firm_comparison_count
    ) %>%
    dplyr::group_by(.data$resp_id) %>%
    dplyr::summarise(
      influence = sum(.data$influence) / averaging_firm_count,
      .groups = "drop"
    ) %>%
    dplyr::right_join(
      data.frame(resp_id = respondent_ids, stringsAsFactors = FALSE),
      by = "resp_id"
    ) %>%
    dplyr::mutate(influence = dplyr::coalesce(.data$influence, 0))

  probability_different <- mean(firm_stats$probability_different)
  amad <- mean(firm_stats$amad)
  tie_share <- mean(firm_stats$tie_share)
  list(
    summary = c(
      estimate = amad,
      se = belief_amad_influence_se(
        respondent_influences$influence, respondent_count
      ),
      probability_different = probability_different,
      gap_given_different = amad / probability_different,
      tie_share = tie_share,
      tie_share_se = belief_amad_influence_se(
        tie_share_respondent_influences$influence, respondent_count
      ),
      firm_count = averaging_firm_count
    ),
    firms = firm_stats
  )
}

run_belief_summary_amad_analysis <- function(
    survey_data,
    output_dir = file.path(intermediate, "Full_Sample"),
    outcomes = BELIEF_AMAD_OUTCOMES) {
  summary_rows <- vector("list", length(outcomes))

  for (position in seq_along(outcomes)) {
    outcome <- outcomes[position]
    message("Computing belief AMAD diagnostics: ", outcome)
    prep <- prepare_pltree_data(
      survey_data, outcome, subgroup_var = NULL, subgroup_filter = NULL
    )
    prep <- add_belief_amad_pairing_cells(prep, survey_data, outcome)

    likert <- compute_belief_likert_amad(prep$data_rating_long)
    borda <- compute_belief_borda_amad(prep$data_wide_pltree)
    borda_different <- compute_belief_borda_amad(
      prep$data_wide_pltree,
      require_different_ratings = TRUE
    )

    summary_rows[[position]] <- data.frame(
      outcome = outcome,
      responses = nrow(prep$data_rating_long),
      respondents = dplyr::n_distinct(prep$data_rating_long$resp_id),
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
      borda_tie_share = unname(borda$summary["tie_share"]),
      borda_tie_share_se = unname(borda$summary["tie_share_se"]),
      borda_amad_different_ratings =
        unname(borda_different$summary["estimate"]),
      borda_amad_different_ratings_se =
        unname(borda_different$summary["se"]),
      borda_amad_different_ratings_firm_count =
        unname(borda_different$summary["firm_count"]),
      stringsAsFactors = FALSE
    )
  }

  summary_data <- dplyr::bind_rows(summary_rows)
  if (nrow(summary_data) != length(outcomes) ||
      anyDuplicated(summary_data$outcome)) {
    stop("Belief AMAD summary output has incomplete or duplicated outcomes")
  }

  write_parquet_sheet(output_dir, BELIEF_AMAD_SUMMARY_SHEET, summary_data)
  invisible(list(summary = summary_data))
}
