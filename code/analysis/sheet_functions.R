# ------------------------------------------------------------------------------
# Purpose: Sheet (parquet) helper functions
#
# Intermediate results are stored as a directory of parquet files, one per
# "sheet". A sheet name may contain characters that are invalid on disk, so
# we sanitize it into a filename via sanitize_sheet_filename(). Reads and
# writes both go through the same sanitizer so they stay in sync.
#
# Created: Jordan Cammarota 03-06-2026
# ------------------------------------------------------------------------------

# Sanitize an arbitrary sheet name into a safe filesystem filename (no ext).
sanitize_sheet_filename <- function(x) {
  x <- gsub("[\\\\/*?:\"<>|\\[\\]]", "_", x)
  x <- gsub("[()]", "", x)
  x <- gsub("\\s+", "_", x)
  x
}

# Resolve the full path to the parquet file for a given sheet within output_dir.
parquet_sheet_path <- function(output_dir, sheet) {
  file.path(output_dir, paste0(sanitize_sheet_filename(sheet), ".parquet"))
}

# Write a single "sheet" (data frame) to a parquet file inside output_dir.
# Creates output_dir if it doesn't already exist. Overwrites existing files.
write_parquet_sheet <- function(output_dir, sheet, x) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  path <- parquet_sheet_path(output_dir, sheet)
  arrow::write_parquet(as.data.frame(x), path)
  invisible(path)
}

# Read a single "sheet" from its parquet file inside output_dir. Errors loudly
# if the file doesn't exist — code should break on missing data, never silently
# continue.
read_parquet_sheet <- function(output_dir, sheet) {
  path <- parquet_sheet_path(output_dir, sheet)
  if (!file.exists(path)) {
    stop("read_parquet_sheet(): parquet file not found: ", path)
  }
  as.data.frame(arrow::read_parquet(path))
}

# List the sheet names (stems) available in output_dir.
list_parquet_sheets <- function(output_dir) {
  if (!dir.exists(output_dir)) return(character(0))
  files <- list.files(output_dir, pattern = "\\.parquet$", full.names = FALSE)
  sub("\\.parquet$", "", files)
}
