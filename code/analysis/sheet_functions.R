# ------------------------------------------------------------------------------
# Purpose: Sheet Functions
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------
sanitize_sheet <- function(x) {
  x <- gsub("[\\\\/*?:\\[\\]]", "_", x)
  substr(x, 1, 31)
}

remove_sheet_safely <- function(wb, sheet) {
  if (sheet %in% openxlsx::sheets(wb)) {
    openxlsx::removeWorksheet(wb, sheet)
  }
}

write_matrix_sheet <- function(wb, sheet, x) {
  sheet <- sanitize_sheet(sheet)
  remove_sheet_safely(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, x)
}
