# ------------------------------------------------------------------------------
# Purpose: EIV appendix table comparing contact and conduct belief specifications
#
# This table uses the standard EIV_firm sheets. It does not require Revelio or
# EEO-1 share-control EIV outputs.
# ------------------------------------------------------------------------------

source("code/globals.R")

tables_out <- Sys.getenv("EIV_TABLES_DIR", unset = tables)

full_sample_filemap <- tibble::tibble(
  Sample = "Full Sample",
  subdir = "Full_Sample"
)

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

read_firm_eiv_for_subdir <- function(subdir) {
  read_parquet_sheet(file.path(intermediate, subdir), "EIV_firm")
}

pull_eiv_value <- function(eiv_df, lhs_var, rhs_var, model_filter, coef_num,
                           formula_filter = NULL,
                           statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  if (!nrow(eiv_df)) return(NA_real_)

  eiv_df$coef <- suppressWarnings(as.numeric(eiv_df$coef))

  out <- eiv_df |>
    dplyr::filter(
      .data$model == model_filter,
      .data$lhs == lhs_var,
      .data$rhs == rhs_var,
      .data$coef == coef_num
    )

  if (!is.null(formula_filter) && "formula" %in% names(out)) {
    out <- out |> dplyr::filter(.data$formula == formula_filter)
  }

  if (!nrow(out)) return(NA_real_)

  if (statistic == "estimate") {
    suppressWarnings(as.numeric(out$sample_est[1]))
  } else {
    suppressWarnings(as.numeric(out$sample_se[1]))
  }
}

pull_eiv_formatted_value <- function(eiv_df, lhs_var, rhs_var, model_filter,
                                     coef_num, formula_filter = NULL,
                                     statistic = c("estimate", "se")) {
  statistic <- match.arg(statistic)
  value <- pull_eiv_value(
    eiv_df = eiv_df,
    lhs_var = lhs_var,
    rhs_var = rhs_var,
    model_filter = model_filter,
    coef_num = coef_num,
    formula_filter = formula_filter,
    statistic = statistic
  )

  if (statistic == "estimate") fmt3(value) else fmt_se(value)
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

build_appendix_panel <- function(eiv_by_subdir, lhs, rhs_contact, rhs_conduct,
                                 filemap = full_sample_filemap) {
  pull_appendix_col <- function(subdir, model_filter, rhs_type, coef_num,
                                statistic) {
    rhs <- if (rhs_type == "contact") rhs_contact else rhs_conduct
    pull_eiv_formatted_value(
      eiv_df = eiv_by_subdir[[subdir]],
      lhs_var = lhs,
      rhs_var = rhs,
      model_filter = model_filter,
      coef_num = coef_num,
      formula_filter = rhs,
      statistic = statistic
    )
  }

  make_row <- function(label, statistic) {
    out <- filemap |>
      dplyr::select(Sample, subdir) |>
      dplyr::mutate(Sample = label)

    for (i in seq_len(nrow(appendix_specs))) {
      spec <- appendix_specs[i, ]
      out[[spec$col_name]] <- vapply(
        out$subdir,
        pull_appendix_col,
        character(1),
        model_filter = spec$model_filter,
        rhs_type = spec$rhs_type,
        coef_num = spec$coef_num,
        statistic = statistic
      )
    }

    out |>
      dplyr::select(-subdir)
  }

  dplyr::bind_rows(
    make_row("Beliefs", "estimate"),
    make_row("", "se")
  )
}

make_appendix_sections <- function(eiv_by_subdir, filemap = full_sample_filemap) {
  list(
    "Panel A: Race" = build_appendix_panel(
      eiv_by_subdir = eiv_by_subdir,
      lhs = "log_dif",
      rhs_contact = "FirmCont_favor_white",
      rhs_conduct = "conduct_favor_white",
      filemap = filemap
    ),
    "Panel B: Gender" = build_appendix_panel(
      eiv_by_subdir = eiv_by_subdir,
      lhs = "log_dif_gender",
      rhs_contact = "FirmCont_favor_male",
      rhs_conduct = "conduct_favor_male",
      filemap = filemap
    )
  )
}

write_latex_sections <- function(sections, out_file) {
  align <- paste0("l", paste(rep("c", 8), collapse = ""))
  lines <- c(
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    " & \\multicolumn{4}{c}{Likert} & \\multicolumn{4}{c}{Borda} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    " & \\multicolumn{2}{c}{Contact} & \\multicolumn{2}{c}{Conduct} & \\multicolumn{2}{c}{Contact} & \\multicolumn{2}{c}{Conduct} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7} \\cmidrule(lr){8-9}",
    paste(latex_escape(c("", paste0("(", 1:8, ")"))), collapse = " & "),
    "\\\\",
    "\\midrule"
  )

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

  lines <- c(
    lines,
    "Industry FE &  & X &  & X &  & X &  & X",
    "\\\\",
    "\\bottomrule",
    "\\end{tabular}"
  )

  out_tex <- file.path(tables_out, out_file)
  write_lines_checked(lines, out_tex, label = "contact/conduct EIV appendix LaTeX table")
  message("Saved: ", out_tex)
  invisible(out_tex)
}

message("Reading firm EIV sheets from: ", file.path(intermediate, "Full_Sample"))
message("Writing contact/conduct EIV appendix table to: ", tables_out)

eiv_by_subdir <- setNames(
  lapply(full_sample_filemap$subdir, read_firm_eiv_for_subdir),
  full_sample_filemap$subdir
)

appendix_sections <- make_appendix_sections(eiv_by_subdir)

write_latex_sections(
  appendix_sections,
  "EIV_contact_conduct_subsamples_appendix.tex"
)

message("Contact/conduct EIV appendix table complete.")
