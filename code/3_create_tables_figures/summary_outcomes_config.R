# -------------------------------------------------------------------
# Shared config + small helpers for the per-outcome summary scripts
# (variance tables, summary tables, Top/Bottom plots, tri-scatter).
# Sourced by every script in the summary_item_worths family.
# -------------------------------------------------------------------
source("code/globals.R")

# Path to the parquet "sheets" the analysis pipeline writes
dir_path <- file.path(intermediate, "Full_Sample")

# -------------------------------------------------------------------
# Models the summary scripts emit columns / plots for.
# Mirrors the convention in cross_model_corr.R. Adding "PL" (or "OL",
# "OLSC") here is the single switch that turns those models back on
# across every summary script.
# -------------------------------------------------------------------
models <- c("Borda", "OLS")

# Standard outcomes set
outs <- c(
  "FirmCont_favor_white", "FirmHire_favor_white", "conduct_favor_white",
  "FirmCont_favor_male",  "FirmHire_favor_male",  "conduct_favor_male",
  "conduct_favor_younger", "discretion", "FirmSelective", "FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)

label_mapping <- c(
  "discretion"            = "Manager Discretion",
  "FirmSelective"         = "Firm Selectivity",
  "FirmDesire"            = "Firm Desirability",
  "conduct_favor_white"   = "Discrimination Black (Conduct)",
  "conduct_favor_younger" = "Discrimination Older (Conduct)",
  "conduct_favor_male"    = "Discrimination Female (Conduct)",
  "FirmHire_favor_male"   = "Discrimination Female (Hire)",
  "FirmHire_favor_white"  = "Discrimination Black (Hire)",
  "FirmCont_favor_male"   = "Discrimination Female (Contact)",
  "FirmCont_favor_white"  = "Discrimination Black (Contact)",
  "pooled_favor_white"    = "Discrimination Black (Pooled)",
  "pooled_favor_male"     = "Discrimination Female (Pooled)"
)

# Alternate framings outcomes set (separate variance table only)
alternate_framings <- c(
  "FirmCont_favor_white", "FirmCont_black",  "FirmCont_white",
  "FirmHire_favor_white", "FirmHire_black",  "FirmHire_white",
  "conduct_favor_white",  "conduct_black",   "conduct_white",
  "FirmCont_favor_male",  "FirmCont_male",   "FirmCont_female",
  "FirmHire_favor_male",  "FirmHire_male",   "FirmHire_female",
  "conduct_favor_male",   "conduct_male",    "conduct_female",
  "conduct_favor_younger", "conduct_younger", "conduct_older"
)

alternate_label_mapping <- c(
  "FirmCont_favor_white"   = "Discrimination Black (Contact)",
  "FirmCont_black"         = "Discrimination Black - Black Wording (Contact)",
  "FirmCont_white"         = "Discrimination Black - White Wording (Contact)",
  "FirmHire_favor_white"   = "Discrimination Black (Hire)",
  "FirmHire_black"         = "Discrimination Black - Black Wording (Hire)",
  "FirmHire_white"         = "Discrimination Black - White Wording (Hire)",
  "conduct_favor_white"    = "Discrimination Black (Conduct)",
  "conduct_black"          = "Discrimination Black - Black Wording (Conduct)",
  "conduct_white"          = "Discrimination Black - White Wording (Conduct)",
  "FirmCont_favor_male"    = "Discrimination Female (Contact)",
  "FirmCont_male"          = "Discrimination Female - Male Wording (Contact)",
  "FirmCont_female"        = "Discrimination Female - Female Wording (Contact)",
  "FirmHire_favor_male"    = "Discrimination Female (Hire)",
  "FirmHire_male"          = "Discrimination Female - Male Wording (Hire)",
  "FirmHire_female"        = "Discrimination Female - Female Wording (Hire)",
  "conduct_favor_male"     = "Discrimination Female (Conduct)",
  "conduct_male"           = "Discrimination Female - Male Wording (Conduct)",
  "conduct_female"         = "Discrimination Female - Female Wording (Conduct)",
  "conduct_favor_younger"  = "Discrimination Older (Conduct)",
  "conduct_younger"        = "Discrimination Older - Younger Wording (Conduct)",
  "conduct_older"          = "Discrimination Older - Older Wording (Conduct)"
)

# Outcomes that get the OLS-EB vs Borda-EB dual-axis plot
ols_borda_dualaxis_outcomes <- c(
  "pooled_favor_white", "pooled_favor_male",
  "conduct_favor_younger", "FirmSelective", "discretion"
)

# -------------------------------------------------------------------
# Display label helper used by every variance-table script
# -------------------------------------------------------------------
map_label <- function(x, mapping = NULL) {
  if (!is.null(mapping)) {
    lbl <- unname(mapping[x])
    lbl[is.na(lbl)] <- x[is.na(lbl)]
    return(lbl)
  }
  x
}

# Fixed-decimal formatter (padded zeros) used by all LaTeX writers
fmt_dec <- function(x, k = 3) {
  z <- suppressWarnings(as.numeric(x))
  out <- rep("", length(z))
  ok <- is.finite(z)
  out[ok] <- formatC(z[ok], format = "f", digits = k, drop0trailing = FALSE)
  out
}

# -------------------------------------------------------------------
# Convert the long Coefficients parquet sheet into the wide format
# keyed on (firm, firm_id) used by the per-outcome scripts. The current
# pipeline stores both the MLE point estimate and its empirical-Bayes
# shrunk version on the same row, so `value_col` selects which one to
# pivot ("estimate" for MLE, "eb" for EB). Returns NULL if the
# requested model / value_col isn't usable.
# -------------------------------------------------------------------
to_wide_coef <- function(df, model_name, value_col = "estimate") {
  if (is.null(df) || !"model" %in% names(df)) return(NULL)
  if (!value_col %in% names(df)) return(NULL)
  model_df <- df %>% dplyr::filter(.data$model == model_name)
  if (nrow(model_df) == 0) return(NULL)

  if ("outcome" %in% names(model_df)) {
    if ("subset" %in% names(model_df)) {
      model_df <- model_df %>% dplyr::filter(.data$subset == "all")
    }
    if ("entity_type" %in% names(model_df)) {
      model_df <- model_df %>%
        dplyr::filter(tolower(as.character(.data$entity_type)) == "firm")
    }

    id_col <- if ("firm_id" %in% names(model_df)) "firm_id"
              else if ("entity_id" %in% names(model_df)) "entity_id"
              else NULL
    name_col <- if ("firm" %in% names(model_df)) "firm"
                else if ("entity" %in% names(model_df)) "entity"
                else NULL

    if (!is.null(id_col) && !is.null(name_col)) {
      return(
        model_df %>%
          dplyr::transmute(
            firm    = as.character(.data[[name_col]]),
            firm_id = suppressWarnings(as.integer(.data[[id_col]])),
            outcome = as.character(.data$outcome),
            value   = suppressWarnings(as.numeric(.data[[value_col]]))
          ) %>%
          dplyr::filter(!is.na(.data$firm_id), !is.na(.data$outcome)) %>%
          dplyr::distinct(.data$firm_id, .data$outcome, .keep_all = TRUE) %>%
          tidyr::pivot_wider(names_from = outcome, values_from = value)
      )
    }
  }

  model_df %>% dplyr::select(-model)
}
