# ------------------------------------------------------------------------------
# Purpose: Clean Experimental outcomes
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
clean_experimental <- function(data, outcomes) {
  required_columns <- c("firm", "firm_id", outcomes)
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop("Missing experimental columns: ", paste(missing_columns, collapse = ", "))
  }

  experimental_data <- data %>%
    dplyr::select(dplyr::all_of(required_columns)) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(outcomes), ~ !is.na(.x)))

  if (anyDuplicated(experimental_data$firm_id)) {
    stop("Experimental outcomes are not unique by firm_id.")
  }

  experimental_data
}
