# ------------------------------------------------------------------------------
# Purpose: EIV tables with Revelio firm outcomes on the LHS
#
# Section 2 writes EIV_revelio_firm sheets. This script formats the no-share-
# control specifications where Revelio shares or wage gaps are regressed on
# contact, conduct, and pooled discrimination beliefs.
# ------------------------------------------------------------------------------

source("code/globals.R")

tables_out <- Sys.getenv("REVELIO_EIV_TABLES_DIR", unset = tables)

default_filemap <- tibble::tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination",
             "40 Years or Older",
             "Less than 40 Years Old"),
  subdir = c("Full_Sample",
             "Subset_Black",
             "Subset_White",
             "Subset_Female",
             "Subset_Male",
             "Subset_Looking",
             "Subset_Not_Looking",
             "Subset_Feared_Discrimination_1",
             "Subset_Feared_Discrimination_0",
             "Subset_Age_gte40",
             "Subset_Age_lt40")
)

table_configs <- list(
  list(
    lhs = "black_share",
    rhs_contact = "FirmCont_favor_white",
    rhs_conduct = "conduct_favor_white",
    rhs_pooled = "pooled_favor_white",
    out_file = "EIV_revelio_black_share_race_beliefs.tex"
  ),
  list(
    lhs = "female_share",
    rhs_contact = "FirmCont_favor_male",
    rhs_conduct = "conduct_favor_male",
    rhs_pooled = "pooled_favor_male",
    out_file = "EIV_revelio_female_share_gender_beliefs.tex"
  ),
  list(
    lhs = "race_wage_gap",
    rhs_contact = "FirmCont_favor_white",
    rhs_conduct = "conduct_favor_white",
    rhs_pooled = "pooled_favor_white",
    out_file = "EIV_revelio_race_wage_gap_race_beliefs.tex"
  ),
  list(
    lhs = "gender_wage_gap",
    rhs_contact = "FirmCont_favor_male",
    rhs_conduct = "conduct_favor_male",
    rhs_pooled = "pooled_favor_male",
    out_file = "EIV_revelio_gender_wage_gap_gender_beliefs.tex"
  )
)

fmt3 <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))
}

fmt_se <- function(x) {
  x <- as.numeric(x)
  if (any(x < -sqrt(.Machine$double.eps), na.rm = TRUE)) {
    stop("Negative standard error found in EIV table inputs.", call. = FALSE)
  }
  ifelse(is.na(x), "NA", sprintf("%.3f", abs(x)))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

read_revelio_eiv_for_subdir <- function(subdir) {
  read_parquet_sheet(file.path(intermediate, subdir), "EIV_revelio_firm")
}

pull_eiv_value <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num,
                           formula_filter = rhs_var,
                           statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  if (!nrow(eiv_df)) return(NA_real_)

  eiv_df$coef <- suppressWarnings(as.numeric(eiv_df$coef))

  out <- eiv_df %>%
    dplyr::filter(
      .data$model == model_filter,
      .data$lhs == lhs_var,
      .data$rhs == rhs_var,
      .data$coef == coef_num
    )

  if (!is.null(formula_filter) && "formula" %in% names(out)) {
    out <- out %>% dplyr::filter(.data$formula == formula_filter)
  }

  if (!nrow(out)) return(NA_real_)

  if (statistic == "estimate") {
    suppressWarnings(as.numeric(out$sample_est[1]))
  } else {
    suppressWarnings(as.numeric(out$sample_se[1]))
  }
}

pull_eiv_cell <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num) {
  est <- pull_eiv_value(
    eiv_df = eiv_df,
    lhs_var = lhs_var,
    rhs_var = rhs_var,
    model_filter = model_filter,
    coef_num = coef_num,
    statistic = "estimate"
  )
  se <- pull_eiv_value(
    eiv_df = eiv_df,
    lhs_var = lhs_var,
    rhs_var = rhs_var,
    model_filter = model_filter,
    coef_num = coef_num,
    statistic = "se"
  )

  if (is.na(est) || is.na(se)) return("NA (NA)")
  paste0(fmt3(est), " (", fmt_se(se), ")")
}

build_panel_df <- function(eiv_by_subdir, model_filter, lhs, rhs_contact,
                           rhs_conduct, rhs_pooled,
                           filemap = default_filemap) {
  filemap %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      `(1) Contact` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_contact,
        model_filter,
        1L
      ),
      `(2) Contact (Industry FE)` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_contact,
        model_filter,
        2L
      ),
      `(3) Conduct` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_conduct,
        model_filter,
        1L
      ),
      `(4) Conduct (Industry FE)` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_conduct,
        model_filter,
        2L
      ),
      `(5) Pooled` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_pooled,
        model_filter,
        1L
      ),
      `(6) Pooled (Industry FE)` = pull_eiv_cell(
        eiv_by_subdir[[.data$subdir]],
        lhs,
        rhs_pooled,
        model_filter,
        2L
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-subdir)
}

write_two_panel_table <- function(cfg, eiv_by_subdir) {
  sections <- list(
    "Panel A: Likert" = build_panel_df(
      eiv_by_subdir = eiv_by_subdir,
      model_filter = "OLS",
      lhs = cfg$lhs,
      rhs_contact = cfg$rhs_contact,
      rhs_conduct = cfg$rhs_conduct,
      rhs_pooled = cfg$rhs_pooled
    ),
    "Panel B: Borda" = build_panel_df(
      eiv_by_subdir = eiv_by_subdir,
      model_filter = "Borda",
      lhs = cfg$lhs,
      rhs_contact = cfg$rhs_contact,
      rhs_conduct = cfg$rhs_conduct,
      rhs_pooled = cfg$rhs_pooled
    )
  )

  col_names <- names(sections[[1]])
  lines <- c(
    "\\begin{tabular}{lcccccc}",
    "\\toprule",
    paste(latex_escape(col_names), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

  for (section_name in names(sections)) {
    section_df <- sections[[section_name]]
    lines <- c(
      lines,
      "\\addlinespace[0.3em]",
      paste0("\\multicolumn{7}{l}{\\textbf{", latex_escape(section_name), "}} \\\\")
    )

    for (i in seq_len(nrow(section_df))) {
      row_values <- unname(unlist(section_df[i, ], use.names = FALSE))
      row_values[1] <- paste0("\\hspace{1em}", latex_escape(row_values[1]))
      lines <- c(
        lines,
        paste(c(row_values[1], latex_escape(row_values[-1])), collapse = " & "),
        "\\\\"
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(tables_out, cfg$out_file)
  write_lines_checked(lines, out_tex, label = "Revelio outcome EIV table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading Revelio EIV outputs from: ", intermediate)
message("Writing Revelio outcome EIV tables to: ", tables_out)

eiv_by_subdir <- setNames(
  lapply(default_filemap$subdir, read_revelio_eiv_for_subdir),
  default_filemap$subdir
)

invisible(lapply(table_configs, write_two_panel_table, eiv_by_subdir = eiv_by_subdir))

message("Revelio outcome EIV tables complete.")
