# ------------------------------------------------------------------------------
# Purpose: EIV tables using Revelio firm-level share controls
#
# The data build creates data/processed/revelio_firm_measures.csv, and section 2
# writes EIV_revelio_firm parquet sheets. This script only reads analysis outputs
# and formats the full-sample main table plus subsample appendix tables.
# ------------------------------------------------------------------------------

source("code/globals.R")

fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", as.numeric(x)))
}

fmt_se <- function(x) {
  x <- as.numeric(x)
  if (any(x < -sqrt(.Machine$double.eps), na.rm = TRUE)) {
    stop("Negative standard error found in EIV table inputs.", call. = FALSE)
  }
  ifelse(is.na(x), "", paste0("(", sprintf("%.3f", abs(x)), ")"))
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

tables_out <- Sys.getenv("REVELIO_EIV_TABLES_DIR", unset = tables)
full_sample_dir <- file.path(intermediate, "Full_Sample")

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

read_full_sample_sheet <- function(sheet) {
  read_parquet_sheet(full_sample_dir, sheet)
}

appendix_filemap <- default_filemap

read_appendix_firm_for_subdir <- function(subdir) {
  read_parquet_sheet(file.path(intermediate, subdir), "EIV_firm")
}

pull_eiv_value <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num,
                           formula_filter = NULL,
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

rhs_formula <- function(rhs_var, control = NULL) {
  paste(c(rhs_var, control), collapse = " + ")
}

new_empty_row <- function(label = "") {
  as.list(stats::setNames(c(label, rep("", 8)), c("row_label", paste0("col", 1:8))))
}

main_cells <- function(eiv_firm, eiv_revelio, lhs, belief_rhs, share_rhs,
                       statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, 8)
  names(out) <- paste0("col", 1:8)

  col_index <- 1L
  for (model in c("OLS", "Borda")) {
    for (with_share_control in c(FALSE, TRUE)) {
      for (coef_num in c(1L, 2L)) {
        if (with_share_control) {
          out[col_index] <- pull_eiv_value(
            eiv_df = eiv_revelio,
            lhs_var = lhs,
            rhs_var = belief_rhs,
            model_filter = model,
            coef_num = coef_num,
            formula_filter = rhs_formula(belief_rhs, share_rhs),
            statistic = statistic
          )
        } else {
          out[col_index] <- pull_eiv_value(
            eiv_df = eiv_firm,
            lhs_var = lhs,
            rhs_var = belief_rhs,
            model_filter = model,
            coef_num = coef_num,
            formula_filter = belief_rhs,
            statistic = statistic
          )
        }
        col_index <- col_index + 1L
      }
    }
  }

  out
}

main_share_cells <- function(eiv_revelio, lhs, belief_rhs, share_rhs,
                             statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  out <- rep(NA_real_, 8)
  names(out) <- paste0("col", 1:8)

  col_index <- 1L
  for (model in c("OLS", "Borda")) {
    for (with_share_control in c(FALSE, TRUE)) {
      for (coef_num in c(1L, 2L)) {
        if (with_share_control) {
          out[col_index] <- pull_eiv_value(
            eiv_df = eiv_revelio,
            lhs_var = lhs,
            rhs_var = share_rhs,
            model_filter = model,
            coef_num = coef_num,
            formula_filter = rhs_formula(belief_rhs, share_rhs),
            statistic = statistic
          )
        }
        col_index <- col_index + 1L
      }
    }
  }

  out
}

make_value_row <- function(label, values, formatter) {
  as.list(stats::setNames(
    c(label, formatter(values)),
    c("row_label", paste0("col", 1:8))
  ))
}

make_panel_rows <- function(panel_label, eiv_firm, eiv_revelio, lhs, belief_rhs,
                            share_rhs) {
  belief_est <- main_cells(eiv_firm, eiv_revelio, lhs, belief_rhs, share_rhs, "estimate")
  belief_se <- main_cells(eiv_firm, eiv_revelio, lhs, belief_rhs, share_rhs, "se")
  share_est <- main_share_cells(eiv_revelio, lhs, belief_rhs, share_rhs, "estimate")
  share_se <- main_share_cells(eiv_revelio, lhs, belief_rhs, share_rhs, "se")

  tibble::as_tibble(dplyr::bind_rows(
    new_empty_row(panel_label),
    make_value_row("Beliefs", belief_est, fmt3),
    make_value_row("", belief_se, fmt_se),
    make_value_row("Shares", share_est, fmt3),
    make_value_row("", share_se, fmt_se),
    new_empty_row("")
  ))
}

make_main_table_df <- function(eiv_firm, eiv_revelio) {
  dplyr::bind_rows(
    make_panel_rows(
      panel_label = "Panel A: Race",
      eiv_firm = eiv_firm,
      eiv_revelio = eiv_revelio,
      lhs = "log_dif",
      belief_rhs = "pooled_favor_white",
      share_rhs = "black_share"
    ),
    make_panel_rows(
      panel_label = "Panel B: Gender",
      eiv_firm = eiv_firm,
      eiv_revelio = eiv_revelio,
      lhs = "log_dif_gender",
      belief_rhs = "pooled_favor_male",
      share_rhs = "female_share"
    ),
    tibble::as_tibble(dplyr::bind_rows(
      make_value_row("Industry FE", c("", "X", "", "X", "", "X", "", "X"), identity)
    ))
  )
}

pull_eiv_cell <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num,
                          formula_filter = NULL) {
  est <- pull_eiv_value(
    eiv_df = eiv_df,
    lhs_var = lhs_var,
    rhs_var = rhs_var,
    model_filter = model_filter,
    coef_num = coef_num,
    formula_filter = formula_filter,
    statistic = "estimate"
  )
  se <- pull_eiv_value(
    eiv_df = eiv_df,
    lhs_var = lhs_var,
    rhs_var = rhs_var,
    model_filter = model_filter,
    coef_num = coef_num,
    formula_filter = formula_filter,
    statistic = "se"
  )

  if (is.na(est) || is.na(se)) return("NA (NA)")
  paste(fmt3(est), fmt_se(se))
}

appendix_specs <- tibble::tribble(
  ~col_name, ~model_filter, ~rhs_type, ~coef_num,
  "col1", "OLS", "contact", 1L,
  "col2", "OLS", "contact", 2L,
  "col3", "OLS", "conduct", 1L,
  "col4", "OLS", "conduct", 2L,
  "col5", "Borda", "contact", 1L,
  "col6", "Borda", "contact", 2L,
  "col7", "Borda", "conduct", 1L,
  "col8", "Borda", "conduct", 2L
)

build_subsample_appendix_panel <- function(eiv_by_subdir, lhs, rhs_contact,
                                           rhs_conduct,
                                           filemap = appendix_filemap) {
  pull_appendix_col <- function(subdir, model_filter, rhs_type, coef_num) {
    rhs <- if (rhs_type == "contact") rhs_contact else rhs_conduct
    pull_eiv_cell(
      eiv_df = eiv_by_subdir[[subdir]],
      lhs_var = lhs,
      rhs_var = rhs,
      model_filter = model_filter,
      coef_num = coef_num,
      formula_filter = rhs
    )
  }

  out <- filemap %>%
    dplyr::select(Sample, subdir)

  for (i in seq_len(nrow(appendix_specs))) {
    spec <- appendix_specs[i, ]
    out[[spec$col_name]] <- vapply(
      out$subdir,
      pull_appendix_col,
      character(1),
      model_filter = spec$model_filter,
      rhs_type = spec$rhs_type,
      coef_num = spec$coef_num
    )
  }

  out %>%
    dplyr::select(-subdir)
}

make_subsample_appendix_sections <- function(eiv_by_subdir) {
  list(
    "Panel A: Race Discrimination/Beliefs" = build_subsample_appendix_panel(
      eiv_by_subdir = eiv_by_subdir,
      lhs = "log_dif",
      rhs_contact = "FirmCont_favor_white",
      rhs_conduct = "conduct_favor_white"
    ),
    "Panel B: Gender Discrimination/Beliefs" = build_subsample_appendix_panel(
      eiv_by_subdir = eiv_by_subdir,
      lhs = "log_dif_gender",
      rhs_contact = "FirmCont_favor_male",
      rhs_conduct = "conduct_favor_male"
    )
  )
}

write_grouped_header <- function(lines, first_label, subheaders) {
  c(
    lines,
    " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    paste(latex_escape(c(first_label, subheaders)), collapse = " & "),
    "\\\\",
    "\\midrule"
  )
}

write_latex_grid <- function(table_df, out_file) {
  align <- paste0("l", paste(rep("c", 8), collapse = ""))
  lines <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule"
  )
  lines <- write_grouped_header(lines, "", paste0("(", 1:8, ")"))

  for (i in seq_len(nrow(table_df))) {
    row_label <- table_df$row_label[i]
    cells <- unname(unlist(table_df[i, paste0("col", 1:8)], use.names = FALSE))

    if (grepl("^Panel ", row_label)) {
      lines <- c(
        lines,
        paste0("\\multicolumn{9}{l}{\\textbf{", latex_escape(row_label), "}} \\\\")
      )
    } else if (!nzchar(row_label) && all(!nzchar(cells))) {
      lines <- c(lines, "\\addlinespace")
    } else {
      lines <- c(
        lines,
        paste(latex_escape(c(row_label, cells)), collapse = " & "),
        "\\\\"
      )
    }
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(tables_out, out_file)
  write_lines_checked(lines, out_tex, label = "Revelio EIV LaTeX table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

write_latex_sections <- function(sections, out_file) {
  subheaders <- rep(
    c("Contact", "Contact (Industry FE)", "Conduct", "Conduct (Industry FE)"),
    2
  )
  align <- paste0("l", paste(rep("c", length(subheaders)), collapse = ""))
  lines <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule"
  )
  lines <- write_grouped_header(lines, "Sample", subheaders)

  for (section_name in names(sections)) {
    section_df <- sections[[section_name]]
    lines <- c(
      lines,
      paste0("\\multicolumn{9}{l}{\\textbf{", latex_escape(section_name), "}} \\\\")
    )

    for (i in seq_len(nrow(section_df))) {
      lines <- c(
        lines,
        paste(latex_escape(unname(unlist(section_df[i, ], use.names = FALSE))), collapse = " & "),
        "\\\\"
      )
    }

    lines <- c(lines, "\\addlinespace")
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")

  out_tex <- file.path(tables_out, out_file)
  write_lines_checked(lines, out_tex, label = "Revelio EIV appendix LaTeX table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading full-sample EIV sheets from: ", full_sample_dir)
message("Writing Revelio EIV tables to: ", tables_out)

eiv_firm <- read_full_sample_sheet("EIV_firm")
eiv_revelio <- read_full_sample_sheet("EIV_revelio_firm")

main_table <- make_main_table_df(eiv_firm, eiv_revelio)

write_latex_grid(
  main_table,
  "EIV_revelio_full_sample_pooled_share_controls.tex"
)

eiv_by_subdir <- setNames(
  lapply(appendix_filemap$subdir, read_appendix_firm_for_subdir),
  appendix_filemap$subdir
)

appendix_sections <- make_subsample_appendix_sections(eiv_by_subdir)

write_latex_sections(
  appendix_sections,
  "EIV_revelio_contact_conduct_subsamples_appendix.tex"
)

message("Revelio EIV composition tables complete.")
