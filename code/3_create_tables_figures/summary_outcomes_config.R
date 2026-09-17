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
# -------------------------------------------------------------------
models <- c("Borda", "OLS")

# Canonical left-to-right model order for all summary variance tables.
summary_model_display_order <- c("OLS", "Borda")

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

# Add outcome group headers
standard_outcome_groups <- list(
  Race = c(
    "conduct_favor_white", "FirmCont_favor_white",
    "FirmHire_favor_white", "pooled_favor_white"
  ),
  Gender = c(
    "conduct_favor_male", "FirmCont_favor_male",
    "FirmHire_favor_male", "pooled_favor_male"
  ),
  Age = "conduct_favor_younger",
  `Firm characteristics` = c("FirmDesire", "FirmSelective", "discretion")
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

# Group headers for the alternate-framings variance table, matching the Race/Gender/Age
# grouping convention of standard_outcome_groups above
alternate_outcome_groups <- list(
  Race = c(
    "FirmCont_favor_white", "FirmCont_black",  "FirmCont_white",
    "FirmHire_favor_white", "FirmHire_black",  "FirmHire_white",
    "conduct_favor_white",  "conduct_black",   "conduct_white"
  ),
  Gender = c(
    "FirmCont_favor_male",  "FirmCont_male",   "FirmCont_female",
    "FirmHire_favor_male",  "FirmHire_male",   "FirmHire_female",
    "conduct_favor_male",   "conduct_male",    "conduct_female"
  ),
  Age = c("conduct_favor_younger", "conduct_younger", "conduct_older")
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
