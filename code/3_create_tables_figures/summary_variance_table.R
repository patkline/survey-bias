# -------------------------------------------------------------------
# Cross-outcome SD / bias-corrected variance table.
# Reads the `variance` parquet sheet, auto-detects which models are
# present, and writes one CSV + one LaTeX table containing the columns
# for every model found (Borda + OLS today; PL / OL / OLSC if added).
#
# Two variants are written for each call:
#   1) "unweighted": full firm sample (subset == "all"), no reweighting
#      — uses values from the variance sheet as-is.
#   2) "njobs_weighted": selected firm sample (subset == "subset97",
#      i.e. firms with valid njobs) reweighted by njobs using the rcov
#      sheet, parallel to summary_variance_within_between.R.
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))
source(file.path(analysis, "katz_correct.R"))
source(file.path(analysis, "leave_in_connected.R"))
source(file.path(analysis, "create_wide_rankings.R"))

# Per-model column display names + recognized label set
MODEL_DISPLAY_NAMES <- c(
  PL    = "Plackett--Luce",
  Borda = "Borda",
  OLS   = "Likert",
  OLSC  = "Likert Centered",
  OL    = "Ordered Logit"
)

# How many trailing columns each model contributes to the LaTeX table.
# PL gets an ICC; Borda gets a Normed Variance; everything else gets the
# 3-column SD / Signal SD / T-stat block.
model_n_cols <- function(model) {
  if (model == "PL")    return(4L)  # SD, Signal SD, T-stat, ICC
  if (model == "Borda") return(4L)  # SD, Signal SD, T-stat, Normed Variance
  3L                                # SD, Signal SD, T-stat
}

# -------------------------------------------------------------------
# njobs reweighting of variance/noise/sigma2_hat/Vhat/signal/t_stat
# for the (model, outcome) row(s) in `var_df` using firm-level
# estimates + njobs from `coef_df` and the J x J robust cov from
# `rcov_df`, all filtered to subset == "subset97" + entity_type=="Firm".
# Logic mirrors summary_variance_within_between.R.
# -------------------------------------------------------------------
reweight_variance_njobs <- function(var_df, coef_df, rcov_df) {
  for (i in seq_len(nrow(var_df))) {
    mdl <- var_df$model[i]
    out <- var_df$outcome[i]

    ent_rows <- coef_df %>%
      dplyr::filter(.data$subset == "subset97",
                    .data$entity_type == "Firm",
                    .data$model == mdl,
                    .data$outcome == out) %>%
      dplyr::mutate(estimate  = as.numeric(estimate),
                    njobs     = as.numeric(njobs),
                    entity_id = as.integer(entity_id)) %>%
      dplyr::arrange(entity_id)

    if (nrow(ent_rows) == 0 || all(is.na(ent_rows$njobs))) {
      stop(sprintf("No usable njobs for model=%s outcome=%s at subset97", mdl, out))
    }

    ok <- is.finite(ent_rows$njobs) & ent_rows$njobs > 0
    ent_rows <- ent_rows[ok, , drop = FALSE]
    if (nrow(ent_rows) < 2L) {
      stop(sprintf("Fewer than 2 firms with valid njobs for model=%s outcome=%s", mdl, out))
    }

    ids  <- ent_rows$entity_id
    beta <- ent_rows$estimate
    w    <- ent_rows$njobs / sum(ent_rows$njobs)

    rcov_sub <- rcov_df %>%
      dplyr::filter(.data$subset == "subset97",
                    .data$model == mdl,
                    .data$outcome == out,
                    .data$entity_id_i %in% ids,
                    .data$entity_id_j %in% ids)

    if (nrow(rcov_sub) != length(ids)^2) {
      stop(sprintf("rcov sheet incomplete for model=%s outcome=%s: %d rows, expected %d",
                   mdl, out, nrow(rcov_sub), length(ids)^2))
    }

    Sigma <- matrix(NA_real_, nrow = length(ids), ncol = length(ids),
                    dimnames = list(as.character(ids), as.character(ids)))
    Sigma[cbind(match(rcov_sub$entity_id_i, ids),
                match(rcov_sub$entity_id_j, ids))] <- as.numeric(rcov_sub$rcov)

    # Center beta with njobs weights so variance is around the weighted mean
    beta_c <- beta - sum(w * beta)

    wb <- w * beta_c
    DSigmaD <- (w %o% w) * Sigma

    var_df$variance[i]   <- sum(w * beta_c^2, na.rm = TRUE)
    var_df$noise[i]      <- sum(w * diag(Sigma), na.rm = TRUE)
    var_df$sigma2_hat[i] <- var_df$variance[i] - var_df$noise[i]
    var_df$Vhat[i]       <- 4 * as.numeric(t(wb) %*% Sigma %*% wb) -
                            2 * sum(DSigmaD * Sigma)
    var_df$signal[i]     <- katz_correct(var_df$sigma2_hat[i], var_df$Vhat[i])
    var_df$t_stat[i]     <- ifelse(
      is.finite(var_df$Vhat[i]) & var_df$Vhat[i] > 0,
      var_df$sigma2_hat[i] / sqrt(var_df$Vhat[i]),
      NA_real_
    )
  }
  var_df
}

write_variance_table <- function(dir_path,
                                 outcomes,
                                 tables_dir,
                                 label_mapping = NULL,
                                 csv_name = "variance_biascorrected.csv",
                                 tex_name = "variance_biascorrected.tex",
                                 latex_decimals = 3,
                                 borda_mult = 1,
                                 variant = c("unweighted", "njobs_weighted")) {

  variant <- match.arg(variant)

  var_df <- tryCatch(read_parquet_sheet(dir_path, "variance"),
                     error = function(e) NULL)
  if (is.null(var_df)) {
    stop("Could not read 'variance' sheet from: ", dir_path)
  }

  need_cols <- c("subset", "model", "outcome", "variance",
                 "signal", "sigma2_hat", "Vhat")
  missing <- setdiff(need_cols, names(var_df))
  if (length(missing)) {
    stop("variance sheet missing columns: ", paste(missing, collapse = ", "))
  }

  subset_keep <- if (variant == "unweighted") "all" else "subset97"
  var_df <- var_df %>% dplyr::filter(.data$subset == subset_keep)
  if (length(outcomes)) {
    var_df <- dplyr::filter(var_df, .data$outcome %in% outcomes)
  }

  var_df <- var_df %>%
    dplyr::mutate(
      t_stat = ifelse(
        is.finite(Vhat) & Vhat > 0,
        sigma2_hat / sqrt(Vhat),
        NA_real_
      )
    )

  # Apply njobs reweighting for the weighted variant
  if (variant == "njobs_weighted") {
    coef_df <- read_parquet_sheet(dir_path, "Coefficients")
    rcov_df <- read_parquet_sheet(dir_path, "rcov")
    var_df  <- reweight_variance_njobs(var_df, coef_df, rcov_df)
  }

  # Models to emit columns for (sourced from summary_outcomes_config.R)
  display_order <- c("PL", "Borda", "OL", "OLS", "OLSC")
  present_models <- intersect(display_order, models)
  if (!length(present_models)) {
    stop("No recognized models in `models` config: ",
         paste(models, collapse = ", "))
  }
  missing_in_sheet <- setdiff(present_models, unique(as.character(var_df$model)))
  if (length(missing_in_sheet)) {
    stop("Models requested but absent from variance sheet at ", dir_path,
         ": ", paste(missing_in_sheet, collapse = ", "))
  }

  build_model_cols <- function(df, mdl, mult = 1) {
    mdf <- df %>% dplyr::filter(.data$model == mdl)
    if (nrow(mdf) == 0) return(NULL)
    mdf %>% dplyr::transmute(
      outcome,
      !!paste0(mdl, "_sd")                := sqrt(pmax(variance, 0)) * mult,
      !!paste0(mdl, "_sd_bias_corrected") := sqrt(pmax(signal, 0)) * mult,
      !!paste0(mdl, "_t_stat")            := as.numeric(t_stat)
    )
  }

  tab <- NULL
  for (mdl in present_models) {
    mult <- if (mdl == "Borda") borda_mult else 1
    mdl_df <- build_model_cols(var_df, mdl, mult = mult)
    if (is.null(mdl_df)) next

    if (mdl == "PL") {
      pl_raw <- var_df %>% dplyr::filter(.data$model == "PL")
      if ("reliability" %in% names(pl_raw)) {
        mdl_df$PL_reliability <- as.numeric(pl_raw$reliability)
      } else {
        sig <- pl_raw$signal
        rel <- ifelse(is.finite(sig), (sig / 1.64) / (1 + sig / 1.64), NA_real_)
        rel[!is.finite(rel)] <- NA_real_
        mdl_df$PL_reliability <- rel
      }
    }
    if (mdl == "Borda") {
      borda_raw <- var_df %>% dplyr::filter(.data$model == "Borda")
      mdl_df$Borda_var_norm <- as.numeric(pmax(borda_raw$signal, 0)) / 0.0844
    }

    tab <- if (is.null(tab)) mdl_df else dplyr::full_join(tab, mdl_df, by = "outcome")
  }

  if (length(outcomes)) {
    tab <- dplyr::full_join(
      data.frame(outcome = unique(outcomes), stringsAsFactors = FALSE),
      tab, by = "outcome"
    )
  }

  tab$Outcome_display <- map_label(tab$outcome, label_mapping)
  tab <- tab %>% dplyr::arrange(.data$Outcome_display, .data$outcome)

  # ---- CSV ----
  csv_cols <- list(Outcome = tab$Outcome_display)
  for (mdl in present_models) {
    disp <- if (mdl == "OLS") "Likert"
            else if (mdl == "OLSC") "Likert Centered"
            else if (mdl == "OL") "OL"
            else if (mdl == "PL") "PL"
            else mdl
    csv_cols[[paste0(disp, ": sd")]]                 <- tab[[paste0(mdl, "_sd")]]
    csv_cols[[paste0(disp, ": bias corrected sd")]]  <- tab[[paste0(mdl, "_sd_bias_corrected")]]
    csv_cols[[paste0(disp, ": t-stat")]]             <- tab[[paste0(mdl, "_t_stat")]]
    if (mdl == "PL"    && "PL_reliability" %in% names(tab)) {
      csv_cols[["PL: reliability"]] <- tab$PL_reliability
    }
    if (mdl == "Borda" && "Borda_var_norm" %in% names(tab)) {
      csv_cols[["Borda: normed variance"]] <- tab$Borda_var_norm
    }
  }
  csv_out_path <- file.path(tables_dir, csv_name)
  utils::write.csv(as.data.frame(csv_cols, check.names = FALSE),
                   csv_out_path, row.names = FALSE)

  # ---- LaTeX ----
  latex_cols <- list(Outcome = tab$Outcome_display)
  align_str <- c("l", "l")
  cmid_parts <- character(0)
  hdr_groups <- character(0)
  hdr_cols   <- character(0)
  col_idx <- 1L

  for (mdl in present_models) {
    n <- model_n_cols(mdl)
    disp <- MODEL_DISPLAY_NAMES[[mdl]]
    hdr_groups <- c(hdr_groups, sprintf("\\multicolumn{%d}{c}{%s}", n, disp))
    cmid_parts <- c(cmid_parts,
                    sprintf("\\cmidrule(lr){%d-%d}", col_idx + 1L, col_idx + n))

    latex_cols[[paste0(mdl, " SD")]]      <- fmt_dec(tab[[paste0(mdl, "_sd")]], latex_decimals)
    latex_cols[[paste0(mdl, " Sig SD")]]  <- fmt_dec(tab[[paste0(mdl, "_sd_bias_corrected")]], latex_decimals)
    latex_cols[[paste0(mdl, " t")]]       <- fmt_dec(tab[[paste0(mdl, "_t_stat")]], latex_decimals)
    hdr_cols <- c(hdr_cols,
                  "Std Dev",
                  "\\shortstack{Signal\\\\Std Dev}",
                  "\\shortstack{T-stat\\\\no signal}")

    if (mdl == "PL") {
      latex_cols[["PL ICC"]] <- fmt_dec(tab$PL_reliability, latex_decimals)
      hdr_cols <- c(hdr_cols, "ICC")
    }
    if (mdl == "Borda") {
      latex_cols[["Borda NV"]] <- fmt_dec(tab$Borda_var_norm, latex_decimals)
      hdr_cols <- c(hdr_cols, "\\shortstack{Normed\\\\Variance}")
    }

    align_str <- c(align_str, rep("c", n))
    col_idx <- col_idx + n
  }

  latex_df <- as.data.frame(latex_cols, check.names = FALSE)
  xt <- xtable::xtable(latex_df, align = align_str)

  header <- paste0(
    "\\toprule\n",
    " & ", paste(hdr_groups, collapse = " & "), " \\\\\n",
    paste(cmid_parts, collapse = " "), "\n",
    "Outcome & ", paste(hdr_cols, collapse = " & "), " \\\\\n",
    "\\midrule\n"
  )

  tex_out_path <- file.path(tables_dir, tex_name)
  print(
    xt,
    include.rownames = FALSE,
    include.colnames = FALSE,
    file = tex_out_path,
    add.to.row = list(pos = list(0), command = header),
    sanitize.text.function = identity,
    booktabs = TRUE,
    floating = FALSE,
    comment = FALSE
  )
  cat("Variance / SD table saved:",
      basename(csv_out_path), "and", basename(tex_out_path), "\n")

  invisible(list(csv = csv_out_path, tex = tex_out_path, data = tab))
}

# Helper: emit both unweighted and njobs-weighted variants of one table.
write_variance_table_both <- function(dir_path, outcomes, tables_dir, label_mapping,
                                      csv_base, tex_base,
                                      latex_decimals = 3, borda_mult = 1) {
  write_variance_table(
    dir_path      = dir_path,
    outcomes      = outcomes,
    tables_dir    = tables_dir,
    label_mapping = label_mapping,
    csv_name      = paste0(csv_base, ".csv"),
    tex_name      = paste0(tex_base, ".tex"),
    latex_decimals = latex_decimals,
    borda_mult    = borda_mult,
    variant       = "unweighted"
  )
  write_variance_table(
    dir_path      = dir_path,
    outcomes      = outcomes,
    tables_dir    = tables_dir,
    label_mapping = label_mapping,
    csv_name      = paste0(csv_base, "_njobs_weighted.csv"),
    tex_name      = paste0(tex_base, "_njobs_weighted.tex"),
    latex_decimals = latex_decimals,
    borda_mult    = borda_mult,
    variant       = "njobs_weighted"
  )
}

write_csv_checked <- function(x, out_path, label = "CSV output") {
  tmp_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(out_path)), "_"),
    fileext = ".csv"
  )
  on.exit(unlink(tmp_path), add = TRUE)

  utils::write.csv(x, tmp_path, row.names = FALSE)
  copy_checked_output(tmp_path, out_path, label = label)
}

latex_escape_text <- function(x) {
  out <- as.character(x)
  for (ch in c("&", "%", "$", "#", "_", "{", "}")) {
    out <- gsub(ch, paste0("\\", ch), out, fixed = TRUE)
  }
  out
}

format_count <- function(x) {
  z <- suppressWarnings(as.integer(x))
  out <- rep("", length(z))
  ok <- !is.na(z)
  out[ok] <- formatC(z[ok], format = "d", big.mark = ",")
  out
}

write_belief_summary_ols_borda <- function(dir_path,
                                           outcomes,
                                           tables_dir,
                                           label_mapping = NULL,
                                           csv_name = "belief_summary_ols_borda.csv",
                                           tex_name = "belief_summary_ols_borda.tex",
                                           latex_decimals = 3) {
  coef_df <- read_parquet_sheet(dir_path, "Coefficients")

  required_cols <- c("subset", "model", "outcome", "entity_type", "estimate", "rse")
  missing_cols <- setdiff(required_cols, names(coef_df))
  if (length(missing_cols)) {
    stop("Coefficients sheet missing columns: ", paste(missing_cols, collapse = ", "))
  }

  survey_path <- file.path(processed, "long_survey_final.csv")
  if (!file.exists(survey_path)) {
    stop("Could not find prepared survey data for belief summary table: ", survey_path)
  }
  survey_data <- utils::read.csv(survey_path, stringsAsFactors = FALSE)

  counts <- dplyr::bind_rows(lapply(outcomes, function(outcome) {
    prep <- prepare_pltree_data(
      data            = survey_data,
      rank_col        = outcome,
      subgroup_var    = NULL,
      subgroup_filter = NULL
    )
    data_long <- prep$data_rating_long

    data.frame(
      outcome     = outcome,
      Responses   = nrow(data_long),
      Respondents = dplyr::n_distinct(data_long$resp_id),
      stringsAsFactors = FALSE
    )
  }))

  model_preference <- data.frame(
    model = c("OLS_not_recentered", "OLS", "Borda_not_recentered", "Borda"),
    table_model = c("OLS", "OLS", "Borda", "Borda"),
    preference = c(1L, 2L, 1L, 2L),
    stringsAsFactors = FALSE
  )

  metrics <- coef_df %>%
    dplyr::inner_join(model_preference, by = "model") %>%
    dplyr::filter(
      .data$subset == "all",
      .data$entity_type == "Firm",
      .data$outcome %in% outcomes
    ) %>%
    dplyr::group_by(.data$outcome, .data$table_model) %>%
    dplyr::filter(.data$preference == min(.data$preference, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      estimate = suppressWarnings(as.numeric(.data$estimate)),
      rse      = suppressWarnings(as.numeric(.data$rse))
    ) %>%
    dplyr::group_by(.data$outcome, .data$table_model) %>%
    dplyr::summarise(
      mean   = mean(.data$estimate, na.rm = TRUE),
      avg_se = mean(.data$rse, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = "table_model",
      values_from = c("mean", "avg_se")
    )

  tab <- data.frame(outcome = outcomes, stringsAsFactors = FALSE) %>%
    dplyr::left_join(counts, by = "outcome") %>%
    dplyr::left_join(metrics, by = "outcome") %>%
    dplyr::mutate(Outcome = map_label(.data$outcome, label_mapping)) %>%
    dplyr::arrange(.data$Outcome, .data$outcome) %>%
    dplyr::transmute(
      Outcome,
      Responses,
      Respondents,
      `Likert: mean`       = .data$mean_OLS,
      `Likert: Average SE` = .data$avg_se_OLS,
      `Borda: mean`        = .data$mean_Borda,
      `Borda: Average SE`  = .data$avg_se_Borda
    )

  csv_out_path <- file.path(tables_dir, csv_name)
  write_csv_checked(as.data.frame(tab, check.names = FALSE), csv_out_path,
                    label = "belief summary CSV")

  latex_df <- data.frame(
    Outcome     = latex_escape_text(tab$Outcome),
    Responses   = format_count(tab$Responses),
    Respondents = format_count(tab$Respondents),
    `Likert Mean` = fmt_dec(tab$`Likert: mean`, latex_decimals),
    `Likert SE`   = fmt_dec(tab$`Likert: Average SE`, latex_decimals),
    `Borda Mean`  = fmt_dec(tab$`Borda: mean`, latex_decimals),
    `Borda SE`    = fmt_dec(tab$`Borda: Average SE`, latex_decimals),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  xt <- xtable::xtable(latex_df, align = c("l", "l", rep("c", 6)))
  header <- paste0(
    "  \\toprule\n",
    "  \\toprule\n",
    " & \\multicolumn{2}{c}{Sample} & \\multicolumn{4}{c}{Aggregate Firm Scores} \\\\\n",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-7}\n",
    " & & & \\multicolumn{2}{c}{Likert} & \\multicolumn{2}{c}{Borda} \\\\\n",
    "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}\n",
    "Outcome & Responses & Respondents & Mean & Average SE & Mean & Average SE \\\\\n",
    "\\midrule\n",
    " \\midrule\n"
  )

  tex_out_path <- file.path(tables_dir, tex_name)
  write_xtable_checked(
    xt,
    tex_out_path,
    include.rownames = FALSE,
    include.colnames = FALSE,
    add.to.row = list(pos = list(0), command = header),
    sanitize.text.function = identity,
    booktabs = TRUE,
    floating = FALSE,
    comment = FALSE,
    label = "belief summary LaTeX"
  )

  cat("Belief summary table saved:",
      basename(csv_out_path), "and", basename(tex_out_path), "\n")

  invisible(list(csv = csv_out_path, tex = tex_out_path, data = tab))
}

# -------------------------------
# Standard outcomes
# -------------------------------
write_variance_table_both(
  dir_path      = dir_path,
  outcomes      = outs,
  tables_dir    = tables,
  label_mapping = label_mapping,
  csv_base      = "variance_biascorrected",
  tex_base      = "variance_biascorrected"
)

write_belief_summary_ols_borda(
  dir_path      = dir_path,
  outcomes      = outs,
  tables_dir    = tables,
  label_mapping = label_mapping
)

# -------------------------------
# Alternate framings
# -------------------------------
write_variance_table_both(
  dir_path      = dir_path,
  outcomes      = alternate_framings,
  tables_dir    = tables,
  label_mapping = alternate_label_mapping,
  csv_base      = "variance_biascorrected_alternate",
  tex_base      = "variance_biascorrected_alternate"
)
