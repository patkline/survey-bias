library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(knitr)

make_confidence_table <- function(
    df,
    id_var = "ResponseId",
    # confidence variables (rows)
    conf_vars = c(
      "confidence_age_conduct",
      "confidence_gend_conduct",
      "confidence_race_conduct",
      "confidence_gend_names",
      "confidence_race_names"
    ),
    # pretty row labels
    conf_labels = c(
      "Discrimination Older (Conduct)",
      "Discrimination Female (Conduct)",
      "Discrimination Black (Conduct)",
      "Discrimination Female (Contact)",
      "Discrimination Black (Contact)"
    ),
    # corresponding variables to check for restriction
    restrict_vars = c(
      "conduct_favor_younger",  # for confidence_age_conduct
      "conduct_favor_male",     # for confidence_gend_conduct
      "conduct_black",          # for confidence_race_conduct
      "FirmCont_favor_male",    # for confidence_gend_names
      "FirmCont_favor_white"    # for confidence_race_names
    ),
    digits   = 1,
    out_tex  = file.path("summary", "confidence_table.tex")  # output .tex file
) {
  stopifnot(length(conf_vars)    == length(conf_labels))
  stopifnot(length(conf_vars)    == length(restrict_vars))
  stopifnot(id_var %in% names(df))
  stopifnot(all(conf_vars    %in% names(df)))
  stopifnot(all(restrict_vars %in% names(df)))
  
  # Column order
  order_levels <- c(
    "Not at all confident",
    "Slightly confident",
    "Somewhat confident",
    "Very confident",
    "Extremely confident",
    "Missing"
  )
  
  # Helper: label confidence strings
  label_conf <- function(x) {
    out <- dplyr::case_when(
      is.na(x)                      ~ "Missing",
      str_trim(x) == ""             ~ "Missing",
      x == "Not at all confident"   ~ "Not at all confident",
      x == "Slightly confident"     ~ "Slightly confident",
      x == "Somewhat confident"     ~ "Somewhat confident",
      x == "Very confident"         ~ "Very confident",
      x == "Extremely confident"    ~ "Extremely confident",
      TRUE                          ~ "Missing"   # catch unexpected values
    )
    factor(out, levels = order_levels)
  }
  
  # lists to collect rows and Ns
  rows_list <- vector("list", length(conf_vars))
  Ns        <- numeric(length(conf_vars))
  
  for (i in seq_along(conf_vars)) {
    conf_var     <- conf_vars[i]
    conf_label   <- conf_labels[i]
    restrict_var <- restrict_vars[i]
    
    # ---- 1. Compute restriction per respondent for this restrict_var ----
    restrict_by_id <- df %>%
      dplyr::group_by(id = .data[[id_var]]) %>%
      dplyr::summarise(
        n_nonmiss = {
          vals <- suppressWarnings(as.numeric(.data[[restrict_var]]))
          sum(!is.na(vals))
        },
        min_val = {
          vals <- suppressWarnings(as.numeric(.data[[restrict_var]]))
          if (all(is.na(vals))) NA_real_ else min(vals, na.rm = TRUE)
        },
        max_val = {
          vals <- suppressWarnings(as.numeric(.data[[restrict_var]]))
          if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
        },
        .groups = "drop"
      )
    
    # keep ids with at least 3 non-missing and non-constant ratings
    ids_keep <- restrict_by_id %>%
      dplyr::filter(
        .data$n_nonmiss >= 3,
        !is.na(.data$min_val),
        !is.na(.data$max_val),
        .data$min_val != .data$max_val
      ) %>%
      dplyr::pull(.data$id)
    
    # if no one passes, create an all-NA row and continue
    if (length(ids_keep) == 0) {
      warning("No respondents passed restriction for ", conf_var, "; row will be all NA.")
      row_empty <- tibble(
        Measure = conf_label,
        `Not at all confident` = NA_real_,
        `Slightly confident`   = NA_real_,
        `Somewhat confident`   = NA_real_,
        `Very confident`       = NA_real_,
        `Extremely confident`  = NA_real_,
        `Missing`              = NA_real_
      )
      rows_list[[i]] <- row_empty
      Ns[i]          <- 0
      next
    }
    
    # ---- 2. Collapse to ONE confidence response per respondent ----
    resp_df <- df %>%
      dplyr::filter(.data[[id_var]] %in% ids_keep) %>%
      dplyr::group_by(id = .data[[id_var]]) %>%
      dplyr::summarise(
        raw_conf = {
          v <- .data[[conf_var]]
          v_clean <- v[!is.na(v) & str_trim(v) != ""]
          if (length(v_clean) == 0) NA_character_ else v_clean[1]
        },
        .groups = "drop"
      ) %>%
      dplyr::mutate(label = label_conf(.data$raw_conf))
    
    N_resp <- nrow(resp_df)
    Ns[i]  <- N_resp
    
    # ---- 3. Counts + percentages for this variable ----
    counts <- resp_df %>%
      dplyr::count(.data$label, name = "n") %>%
      dplyr::right_join(
        tibble(label = factor(order_levels, levels = order_levels)),
        by = "label"
      ) %>%
      dplyr::mutate(
        n     = dplyr::coalesce(.data$n, 0L),
        share = 100 * .data$n / N_resp,
        share = round(.data$share, digits)
      )
    
    # ---- 4. One row with columns for each level ----
    row_i <- counts %>%
      dplyr::select(label, share) %>%
      tidyr::pivot_wider(names_from = label, values_from = share)
    
    row_i <- row_i %>%
      dplyr::mutate(Measure = conf_label) %>%
      dplyr::relocate(Measure)
    
    rows_list[[i]] <- row_i
  }
  
  # Bind all rows into a table
  out_tab <- dplyr::bind_rows(rows_list)
  
  # Ensure columns are in desired order
  out_tab <- out_tab %>%
    dplyr::select(Measure, dplyr::all_of(order_levels))
  
  # ----- Write LaTeX tabular to file -----
  col_names <- c(
    "Measure",
    "Not at all confident",
    "Slightly confident",
    "Somewhat confident",
    "Very confident",
    "Extremely confident",
    "Missing"
  )
  
  tex_code <- knitr::kable(
    out_tab,
    format    = "latex",
    booktabs  = TRUE,
    align     = c("l", rep("c", length(order_levels))),
    col.names = col_names,
    linesep   = ""
  )
  
  dir.create(dirname(out_tex), showWarnings = FALSE, recursive = TRUE)
  writeLines(tex_code, out_tex)
  
  message("✓ Confidence table saved to: ", out_tex)
  
  invisible(list(
    table        = out_tab,         # percentages by row/column
    Ns           = setNames(Ns, conf_labels),  # N respondents used per row
    tex          = tex_code,
    out_tex      = out_tex
  ))
}
