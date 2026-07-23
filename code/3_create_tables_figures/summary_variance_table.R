# -------------------------------------------------------------------
# Cross-outcome SD / bias-corrected variance table.
# Reads the `variance` parquet sheet, auto-detects which models are
# present, and writes LaTeX tables containing the columns
# for every model found (OLS + Borda today; PL / OL / OLSC if added).
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

# Per-model column display names + recognized label set
MODEL_DISPLAY_NAMES <- c(
  PL    = "Plackett--Luce",
  Borda = "Borda",
  OLS   = "Likert",
  OLSC  = "Likert Centered",
  OL    = "Ordered Logit"
)

# How many trailing columns each model contributes to the LaTeX table.
# Likert and Borda include reliability, defined as
# (Signal SD / naive SD)^2; PL keeps its ICC column.
model_n_cols <- function(model) {
  if (model == "PL")                 return(4L)  # SD, Signal SD, T-stat, ICC
  if (model %in% c("OLS", "Borda")) return(4L)  # SD, Signal SD, Reliability, T-stat
  3L                                             # SD, Signal SD, T-stat
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
                                 tex_name = "variance_biascorrected.tex",
                                 latex_decimals = 3,
                                 borda_mult = 1,
                                 outcome_groups = NULL,
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
  present_models <- intersect(summary_model_display_order, models)
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
      !!paste0(mdl, "_reliability")       := ifelse(
        is.finite(variance) & variance > 0 & is.finite(signal),
        pmax(signal, 0) / variance,
        NA_real_
      ),
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
    hdr_cols <- c(hdr_cols,
                  "Std Dev",
                  "\\shortstack{Signal\\\\Std Dev}")

    if (mdl %in% c("OLS", "Borda")) {
      latex_cols[[paste0(mdl, " Reliability")]] <-
        fmt_dec(tab[[paste0(mdl, "_reliability")]], latex_decimals)
      hdr_cols <- c(hdr_cols, "Reliability")
    }

    latex_cols[[paste0(mdl, " t")]] <- fmt_dec(tab[[paste0(mdl, "_t_stat")]], latex_decimals)
    hdr_cols <- c(hdr_cols, "\\shortstack{T-stat\\\\no signal}")

    if (mdl == "PL") {
      latex_cols[["PL ICC"]] <- fmt_dec(tab$PL_reliability, latex_decimals)
      hdr_cols <- c(hdr_cols, "ICC")
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
  if (is.null(outcome_groups)) {
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
  } else {
    body_lines <- grouped_summary_table_rows(
      outcomes       = tab$outcome,
      display_labels = tab$Outcome_display,
      formatted_data = latex_df[, -1, drop = FALSE],
      outcome_groups = outcome_groups
    )
    latex_lines <- c(
      sprintf("\\begin{tabular}{%s}", paste(align_str[-1], collapse = "")),
      "  \\toprule",
      paste0(" & ", paste(hdr_groups, collapse = " & "), " \\\\"),
      paste(cmid_parts, collapse = " "),
      paste0("Outcome & ", paste(hdr_cols, collapse = " & "), " \\\\"),
      " \\midrule",
      body_lines,
      "   \\bottomrule",
      "\\end{tabular}"
    )
    writeLines(latex_lines, tex_out_path)
  }
  cat("Variance / SD table saved:", basename(tex_out_path), "\n")

  invisible(list(tex = tex_out_path, data = tab))
}

# Helper: emit both unweighted and njobs-weighted variants of one table.
write_variance_table_both <- function(dir_path, outcomes, tables_dir, label_mapping,
                                      tex_base,
                                      latex_decimals = 3, borda_mult = 1,
                                      outcome_groups = NULL) {
  write_variance_table(
    dir_path      = dir_path,
    outcomes      = outcomes,
    tables_dir    = tables_dir,
    label_mapping = label_mapping,
    tex_name      = paste0(tex_base, ".tex"),
    latex_decimals = latex_decimals,
    borda_mult    = borda_mult,
    outcome_groups = outcome_groups,
    variant       = "unweighted"
  )
  write_variance_table(
    dir_path      = dir_path,
    outcomes      = outcomes,
    tables_dir    = tables_dir,
    label_mapping = label_mapping,
    tex_name      = paste0(tex_base, "_njobs_weighted.tex"),
    latex_decimals = latex_decimals,
    borda_mult    = borda_mult,
    outcome_groups = outcome_groups,
    variant       = "njobs_weighted"
  )
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

# Build grouped LaTeX rows shared by Table 3 and Table 4. The first cell is
# indented and a small vertical gap follows every group except the last.
grouped_summary_table_rows <- function(outcomes,
                                       display_labels,
                                       formatted_data,
                                       outcome_groups,
                                       formatted_se_data = NULL) {
  grouped_outcomes <- unname(unlist(outcome_groups, use.names = FALSE))
  missing_outcomes <- setdiff(grouped_outcomes, outcomes)
  extra_outcomes   <- setdiff(outcomes, grouped_outcomes)
  if (length(missing_outcomes) || length(extra_outcomes)) {
    stop(
      "Grouped summary-table outcomes do not match table outcomes. Missing: ",
      paste(missing_outcomes, collapse = ", "),
      "; extra: ", paste(extra_outcomes, collapse = ", ")
    )
  }
  if (!is.null(formatted_se_data) &&
      (nrow(formatted_se_data) != length(outcomes) ||
       ncol(formatted_se_data) != ncol(formatted_data))) {
    stop("Grouped summary-table SE data must match formatted point-estimate data")
  }

  lines <- character(0)
  group_names <- names(outcome_groups)
  for (g in seq_along(outcome_groups)) {
    group_outcomes <- outcome_groups[[g]]
    idx <- match(group_outcomes, outcomes)
    lines <- c(lines, paste0("\\textbf{", group_names[g], "} \\\\"))
    for (j in seq_along(idx)) {
      i <- idx[j]
      cells <- c(
        paste0("\\quad ", latex_escape_text(display_labels[i])),
        as.character(formatted_data[i, , drop = TRUE])
      )
      group_row_end <- if (j == length(idx) && g < length(outcome_groups)) {
        " \\\\[0.5em]"
      } else {
        " \\\\"
      }
      if (is.null(formatted_se_data)) {
        lines <- c(lines, paste0(paste(cells, collapse = " & "), group_row_end))
      } else {
        se_cells <- c("", as.character(formatted_se_data[i, , drop = TRUE]))
        lines <- c(
          lines,
          paste0(paste(cells, collapse = " & "), " \\\\"),
          paste0(paste(se_cells, collapse = " & "), group_row_end)
        )
      }
    }
  }
  lines
}

# -------------------------------
# Standard outcomes
# -------------------------------
variance_tables_dir <- Sys.getenv("VARIANCE_TABLES_DIR", unset = tables)

write_variance_table_both(
  dir_path      = dir_path,
  outcomes      = outs,
  tables_dir    = variance_tables_dir,
  label_mapping = label_mapping,
  tex_base      = "variance_biascorrected",
  outcome_groups = standard_outcome_groups
)

# -------------------------------
# Alternate framings
# -------------------------------
write_variance_table_both(
  dir_path      = dir_path,
  outcomes      = alternate_framings,
  tables_dir    = variance_tables_dir,
  label_mapping = alternate_label_mapping,
  tex_base      = "variance_biascorrected_alternate"
)
