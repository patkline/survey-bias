# -------------------------------------------------------------------
# Within/between-industry SD / bias-corrected variance table.
#
# Two variants are written for each call:
#   1) "unweighted": full firm sample. Reads the `variance` parquet
#      sheet at subset == "all" for the *_dm (within) and *_im
#      (between) outcomes (deviations / means computed with equal
#      weights across firms). The between-industry variance components
#      weight each industry by its number of underlying firms so they
#      decompose the full firm-level variance.
#   2) "njobs_weighted": selected firm sample (firms with valid
#      njobs). Reads subset == "subset97" for *_dm_w / *_im_w
#      outcomes, then reweights variance / noise / Vhat / signal /
#      t-stat by njobs using the rcov sheet (logic preserved from
#      the previous single-variant version).
#
# Auto-detects which models live in the variance sheet (Borda + OLS
# today; PL etc. picked up automatically if added).
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))
source(file.path(analysis, "katz_correct.R"))

# Per-model column count: PL gets ICC, Borda gets Normed Variance, others 3 cols
model_n_cols_wb <- function(model) {
  if (model == "PL")    return(4L)
  if (model == "Borda") return(4L)
  3L
}

MODEL_DISPLAY_NAMES_WB <- c(
  PL    = "Plackett--Luce",
  Borda = "Borda",
  OLS   = "Likert",
  OLSC  = "Likert Centered",
  OL    = "Ordered Logit"
)

write_variance_within_between <- function(dir_path,
                                          outcomes,
                                          tables_dir,
                                          label_mapping = NULL,
                                          csv_name = "variance_biascorrected_within_between_industry.csv",
                                          tex_name = "variance_biascorrected_within_between_industry.tex",
                                          latex_decimals = 3,
                                          borda_mult = 1,
                                          variant = c("unweighted",
                                                      "njobs_weighted")) {
  variant <- match.arg(variant)

  var_df <- tryCatch(read_parquet_sheet(dir_path, "variance"),
                     error = function(e) NULL)
  if (is.null(var_df) || !"model" %in% names(var_df)) {
    message("Could not read variance sheet (with model column) from: ", dir_path)
    return(invisible(NULL))
  }

  need_cols <- c("subset", "model", "outcome", "variance",
                 "signal", "sigma2_hat", "Vhat")
  missing <- setdiff(need_cols, names(var_df))
  if (length(missing)) {
    message("variance sheet missing columns: ", paste(missing, collapse = ", "))
    return(invisible(NULL))
  }

  # Pick subset / outcome suffixes based on variant.
  if (variant == "unweighted") {
    subset_keep   <- "all"
    suffix_dm     <- "_dm"
    suffix_im     <- "_im"
  } else {
    subset_keep   <- "subset97"
    suffix_dm     <- "_dm_w"
    suffix_im     <- "_im_w"
  }

  target_outcomes <- c(paste0(outcomes, suffix_dm),
                       paste0(outcomes, suffix_im))

  var_df <- var_df %>%
    dplyr::filter(.data$subset == subset_keep,
                  .data$outcome %in% target_outcomes) %>%
    dplyr::mutate(
      t_stat = ifelse(
        is.finite(Vhat) & Vhat > 0,
        sigma2_hat / sqrt(Vhat),
        NA_real_
      )
    )

  if (nrow(var_df) == 0) {
    message("No within/between-industry outcomes found in variance sheet ",
            "for variant '", variant, "'.")
    return(invisible(NULL))
  }

  # Reweight the between-industry rows so each industry receives weight equal
  # to the number of underlying firms. For the unweighted table, njobs on an
  # *_im industry row stores that firm count (set in make_industry_means.R).
  # The njobs-weighted table continues to use job counts for both panels.
  if (variant %in% c("unweighted", "njobs_weighted")) {
    coef_df <- tryCatch(read_parquet_sheet(dir_path, "Coefficients"),
                        error = function(e) NULL)
    rcov_df <- tryCatch(read_parquet_sheet(dir_path, "rcov"),
                        error = function(e) NULL)
    if (is.null(coef_df) || is.null(rcov_df)) {
      stop("Firm-weighted within/between table requires Coefficients and rcov sheets.")
    }

    if (!is.null(coef_df) && !is.null(rcov_df)) {
      dm_outcomes <- paste0(outcomes, suffix_dm)
      im_outcomes <- paste0(outcomes, suffix_im)

      for (i in seq_len(nrow(var_df))) {
        mdl <- var_df$model[i]
        out <- var_df$outcome[i]

        # In the unweighted table, retain the ordinary firm-level calculation
        # for Panel A and reweight only Panel B by industry firm counts.
        if (variant == "unweighted" && !out %in% im_outcomes) next

        etype <- if (out %in% dm_outcomes) "Firm"
                 else if (out %in% im_outcomes) "Industry"
                 else next

        ent_rows <- coef_df %>%
          dplyr::filter(.data$subset == subset_keep,
                        .data$entity_type == etype,
                        .data$model == mdl,
                        .data$outcome == out) %>%
          dplyr::mutate(estimate  = as.numeric(estimate),
                        njobs     = as.numeric(njobs),
                        entity_id = as.integer(entity_id)) %>%
          dplyr::arrange(entity_id)

        if (nrow(ent_rows) == 0 || all(is.na(ent_rows$njobs))) {
          stop(sprintf("No usable weights for model=%s outcome=%s subset=%s",
                       mdl, out, subset_keep))
        }

        ok <- is.finite(ent_rows$njobs) & ent_rows$njobs > 0
        ent_rows <- ent_rows[ok, , drop = FALSE]
        if (nrow(ent_rows) < 2L) {
          stop(sprintf("Fewer than 2 weighted entities for model=%s outcome=%s",
                       mdl, out))
        }

        ids  <- ent_rows$entity_id
        beta <- ent_rows$estimate
        w    <- ent_rows$njobs / sum(ent_rows$njobs)

        rcov_sub <- rcov_df %>%
          dplyr::filter(.data$subset == subset_keep,
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

        # Center at the weighted mean. For *_im this makes the statistic the
        # between component of the corresponding firm-level variance.
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
    }
  }

  # ---- Build per-model column blocks (uses `models` from config) ----
  present_models <- intersect(summary_model_display_order, models)
  if (!length(present_models)) {
    message("No recognized models in `models` config: ",
            paste(models, collapse = ", "))
    return(invisible(NULL))
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
    if (mdl == "Borda") {
      mdl_df$Borda_var_norm <- ifelse(
        is.finite(mdl_df$Borda_sd_bias_corrected),
        (mdl_df$Borda_sd_bias_corrected / borda_mult)^2 / 0.0844,
        NA_real_
      )
    }
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
    tab <- if (is.null(tab)) mdl_df else dplyr::full_join(tab, mdl_df, by = "outcome")
  }

  tab <- dplyr::full_join(
    data.frame(outcome = target_outcomes, stringsAsFactors = FALSE),
    tab, by = "outcome"
  )

  dm_pattern <- paste0(suffix_dm, "$")
  im_pattern <- paste0(suffix_im, "$")
  strip_pattern <- paste0("(", suffix_dm, "|", suffix_im, ")$")

  tab <- tab %>%
    dplyr::mutate(
      panel = dplyr::case_when(
        grepl(dm_pattern, .data$outcome) ~ "Panel A: Within-industry",
        grepl(im_pattern, .data$outcome) ~ "Panel B: Between-industry",
        TRUE                             ~ "Panel ?: Unknown"
      ),
      base_outcome = sub(strip_pattern, "", .data$outcome)
    )
  tab$Outcome_display <- map_label(tab$base_outcome, label_mapping)

  tab <- tab %>%
    dplyr::mutate(
      panel_order   = ifelse(.data$panel == "Panel A: Within-industry", 1L, 2L),
      outcome_order = match(.data$base_outcome, outcomes),
      outcome_order = ifelse(is.na(.data$outcome_order), Inf, .data$outcome_order)
    ) %>%
    dplyr::arrange(.data$panel_order, .data$outcome_order, .data$base_outcome)

  # ---- CSV ----
  csv_cols <- list(Panel = tab$panel, Outcome = tab$Outcome_display)
  for (mdl in present_models) {
    disp <- if (mdl == "OLS") "Likert"
            else if (mdl == "OLSC") "Likert Centered"
            else mdl
    csv_cols[[paste0(disp, ": sd")]]                <- tab[[paste0(mdl, "_sd")]]
    csv_cols[[paste0(disp, ": bias corrected sd")]] <- tab[[paste0(mdl, "_sd_bias_corrected")]]
    csv_cols[[paste0(disp, ": t-stat")]]            <- tab[[paste0(mdl, "_t_stat")]]
    if (mdl == "PL"    && "PL_reliability"  %in% names(tab)) {
      csv_cols[["PL: reliability"]] <- tab$PL_reliability
    }
    if (mdl == "Borda" && "Borda_var_norm"  %in% names(tab)) {
      csv_cols[["Borda: normed variance"]] <- tab$Borda_var_norm
    }
  }
  csv_out_path <- file.path(tables_dir, csv_name)
  utils::write.csv(as.data.frame(csv_cols, check.names = FALSE),
                   csv_out_path, row.names = FALSE)

  # ---- LaTeX (two-panel) ----
  hdr_groups <- character(0)
  hdr_cols   <- character(0)
  cmid_parts <- character(0)
  align_str  <- "l"
  col_idx    <- 1L

  for (mdl in present_models) {
    n <- model_n_cols_wb(mdl)
    hdr_groups <- c(hdr_groups,
                    sprintf("\\multicolumn{%d}{c}{%s}", n, MODEL_DISPLAY_NAMES_WB[[mdl]]))
    cmid_parts <- c(cmid_parts,
                    sprintf("\\cmidrule(lr){%d-%d}", col_idx + 1L, col_idx + n))
    hdr_cols <- c(hdr_cols,
                  "Std Dev",
                  "\\shortstack{Signal\\\\Std Dev}",
                  "\\shortstack{T-stat\\\\no signal}")
    if (mdl == "PL")    hdr_cols <- c(hdr_cols, "ICC")
    if (mdl == "Borda") hdr_cols <- c(hdr_cols, "\\shortstack{Normed\\\\Variance}")
    align_str <- c(align_str, rep("c", n))
    col_idx <- col_idx + n
  }

  total_cols <- col_idx  # Outcome + per-model columns

  panel_rows <- function(df_panel) {
    if (nrow(df_panel) == 0) return(character(0))
    parts <- character(nrow(df_panel))
    for (i in seq_len(nrow(df_panel))) {
      cells <- df_panel$Outcome_display[i]
      for (mdl in present_models) {
        cells <- c(cells,
                   fmt_dec(df_panel[[paste0(mdl, "_sd")]][i], latex_decimals),
                   fmt_dec(df_panel[[paste0(mdl, "_sd_bias_corrected")]][i], latex_decimals),
                   fmt_dec(df_panel[[paste0(mdl, "_t_stat")]][i], latex_decimals))
        if (mdl == "PL"    && "PL_reliability" %in% names(df_panel)) {
          cells <- c(cells, fmt_dec(df_panel$PL_reliability[i], latex_decimals))
        }
        if (mdl == "Borda" && "Borda_var_norm" %in% names(df_panel)) {
          cells <- c(cells, fmt_dec(df_panel$Borda_var_norm[i], latex_decimals))
        }
      }
      parts[i] <- paste0("    ", paste(cells, collapse = " & "), " \\\\")
    }
    parts
  }

  panel_a <- tab %>% dplyr::filter(.data$panel == "Panel A: Within-industry")
  panel_b <- tab %>% dplyr::filter(.data$panel == "Panel B: Between-industry")

  latex_lines <- c(
    "  \\centering",
    sprintf("  \\begin{tabular}{%s}", paste(align_str, collapse = "")),
    "    \\toprule",
    paste0("    & ", paste(hdr_groups, collapse = " & "), " \\\\"),
    paste0("    ", paste(cmid_parts, collapse = " ")),
    paste0("    Outcome & ", paste(hdr_cols, collapse = " & "), " \\\\"),
    "    \\midrule",
    sprintf("    \\multicolumn{%d}{l}{\\textbf{Panel A: Within-industry}}\\\\", total_cols),
    panel_rows(panel_a),
    "    \\addlinespace",
    sprintf("    \\multicolumn{%d}{l}{\\textbf{Panel B: Between-industry}}\\\\", total_cols),
    panel_rows(panel_b),
    "    \\bottomrule",
    "  \\end{tabular}"
  )

  tex_out_path <- file.path(tables_dir, tex_name)
  writeLines(latex_lines, tex_out_path)

  cat("Within/between-industry variance table saved:",
      basename(csv_out_path), "and", basename(tex_out_path), "\n")

  invisible(list(csv = csv_out_path, tex = tex_out_path, data = tab))
}

# Full firm sample: unweighted firm residuals in Panel A and industry means
# weighted by their firm counts in Panel B.
write_variance_within_between(
  dir_path      = dir_path,
  outcomes      = outs,
  tables_dir    = tables,
  label_mapping = label_mapping,
  csv_name      = "variance_biascorrected_within_between_industry.csv",
  tex_name      = "variance_biascorrected_within_between_industry.tex",
  borda_mult    = 1,
  variant       = "unweighted"
)

# njobs-weighted (selected firm sample, _dm_w / _im_w at subset97)
write_variance_within_between(
  dir_path      = dir_path,
  outcomes      = outs,
  tables_dir    = tables,
  label_mapping = label_mapping,
  csv_name      = "variance_biascorrected_within_between_industry_njobs_weighted.csv",
  tex_name      = "variance_biascorrected_within_between_industry_njobs_weighted.tex",
  borda_mult    = 1,
  variant       = "njobs_weighted"
)
