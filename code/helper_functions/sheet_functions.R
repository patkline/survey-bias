# ------------------------------------------------------------------------------
# Purpose: Sheet (parquet) helper functions
#
# Intermediate results are stored as a directory of parquet files, one per
# "sheet". A sheet name may contain characters that are invalid on disk, so
# we sanitize it into a filename via sanitize_sheet_filename(). Reads and
# writes both go through the same sanitizer so they stay in sync.
#
# Created: Jordan Cammarota 03-06-2026
# Edited to Parquet: Monica Essig Aberg 04-09-2026
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
  out_info <- file.info(path)
  message(
    "✓ Wrote parquet sheet '", sheet, "' to: ", path,
    " | size=", out_info$size,
    " | mtime=", format(out_info$mtime, "%Y-%m-%d %H:%M:%S")
  )
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

# Write text output via a temporary file, then copy into place and verify the
# final file is non-empty. This catches Dropbox/File Provider placeholder writes.
copy_checked_output <- function(tmp_path, out_path, label = "output") {
  tmp_size <- file.info(tmp_path)$size
  if (is.na(tmp_size) || tmp_size == 0L) {
    stop("Temporary ", label, " is empty before copy: ", tmp_path)
  }

  out_dir <- dirname(out_path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (file.exists(out_path)) {
    unlink_status <- unlink(out_path)
    if (unlink_status != 0L && file.exists(out_path)) {
      stop("Could not remove existing ", label, " before overwrite: ", out_path)
    }
  }

  if (!file.copy(tmp_path, out_path, overwrite = TRUE)) {
    stop("Could not copy ", label, " into place: ", out_path)
  }

  out_size <- file.info(out_path)$size
  if (is.na(out_size) || out_size == 0L) {
    stop(label, " is empty after write: ", out_path)
  }

  invisible(out_path)
}

write_lines_checked <- function(lines, out_path, label = "text output") {
  if (length(lines) == 0L) {
    stop("Refusing to write empty ", label, ": ", out_path)
  }

  tmp_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(out_path)), "_"),
    fileext = paste0(".", tools::file_ext(out_path))
  )
  on.exit(unlink(tmp_path), add = TRUE)

  writeLines(lines, tmp_path, useBytes = TRUE)
  copy_checked_output(tmp_path, out_path, label = label)
}

write_xtable_checked <- function(xt, out_path, ..., label = "LaTeX output") {
  tmp_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(basename(out_path)), "_"),
    fileext = ".tex"
  )
  on.exit(unlink(tmp_path), add = TRUE)

  print(xt, file = tmp_path, ...)
  copy_checked_output(tmp_path, out_path, label = label)
}
