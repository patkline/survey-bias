# -------------------------------------------------------------------
# Cross-outcome SD / bias-corrected variance table.
# Reads the `variance` parquet sheet, auto-detects which models are
# present, and writes one CSV + one LaTeX table containing the columns
# for every model found (Borda + OLS today; PL / OL / OLSC if added).
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(create_tables_figures, "summary_outcomes_config.R"))

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

write_variance_table <- function(dir_path,
                                 outcomes,
                                 tables_dir,
                                 label_mapping = NULL,
                                 csv_name = "variance_biascorrected.csv",
                                 tex_name = "variance_biascorrected.tex",
                                 latex_decimals = 3,
                                 borda_mult = 1) {

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

  var_df <- var_df %>% dplyr::filter(.data$subset == "all")
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
  col_idx <- 2L

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

# -------------------------------
# Standard outcomes
# -------------------------------
write_variance_table(
  dir_path     = dir_path,
  outcomes     = outs,
  tables_dir   = tables,
  label_mapping = label_mapping,
  csv_name     = "variance_biascorrected.csv",
  tex_name     = "variance_biascorrected.tex"
)

# -------------------------------
# Alternate framings
# -------------------------------
write_variance_table(
  dir_path      = dir_path,
  outcomes      = alternate_framings,
  tables_dir    = tables,
  label_mapping = alternate_label_mapping,
  csv_name      = "variance_biascorrected_alternate.csv",
  tex_name      = "variance_biascorrected_alternate.tex"
)
