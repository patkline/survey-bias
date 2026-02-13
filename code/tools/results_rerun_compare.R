# ------------------------------------------------------------------------------
# Purpose: Rerun results and generate a compact before/after comparison bundle.
#
# Baseline definition:
# - Baseline is the git-tracked contents of output/ (i.e., what a fresh clone sees)
#
# What it does:
# 1) Ensures output/ matches git baseline (prompting to revert if needed)
# 2) Copies baseline output/{tables,figures,excel} into a run folder (fast clone on macOS when available)
# 3) Reruns results via code/create_tables_figures/metafile.R (overwriting output/)
# 4) Compares baseline vs new output and writes:
#    - changes.csv (file-level + cell-level diffs for .tex/.xlsx)
#    - comparison.tex (+ comparison.pdf if pdflatex is available) for changed tables/figures
#    - old/ and new/ containing only the changed files needed for comparison
# 5) If there are zero changes, deletes the run folder.
# ------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  source("code/globals.R")
}))

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) y else x

stop_quietly <- function(message_text, status = 1) {
  message(message_text)
  quit(save = "no", status = status, runLast = FALSE)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

safe_system <- function(cmd, intern = TRUE) {
  out <- tryCatch(
    suppressWarnings(system(cmd, intern = intern)),
    error = function(e) NULL
  )
  if (is.null(out)) return(NA_character_)
  if (!intern) return(NA_character_)
  if (length(out) == 0) return("")
  paste(out, collapse = "\n")
}

git_info <- function() {
  sha <- safe_system("git rev-parse --short HEAD")
  dirty <- safe_system("git status --porcelain")
  list(
    sha = ifelse(is.na(sha) || sha == "", NA_character_, sha),
    dirty = !is.na(dirty) && nzchar(trimws(dirty))
  )
}

copy_outputs_subdirs <- function(src_root, dest_root, subdirs = c("tables", "figures", "excel")) {
  ensure_dir(dest_root)
  for (sd in subdirs) {
    src <- file.path(src_root, sd)
    if (!dir.exists(src)) next
    dest <- file.path(dest_root, sd)
    copy_tree_fast(src, dest)
  }
  invisible(TRUE)
}

reset_output_to_git_baseline <- function(baseline_dirs) {
  suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "restore", baseline_dirs)))
  suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "clean", "-fd", baseline_dirs)))
  invisible(TRUE)
}

parse_args <- function(argv) {
  opts <- list(
    `run-name` = NULL,
    `skip-rerun` = FALSE,
    `confirm-reset` = NULL
  )

  i <- 1
  while (i <= length(argv)) {
    tok <- argv[[i]]
    if (!startsWith(tok, "--")) {
      stop_quietly(paste0("Unexpected token: ", tok))
    }
    key <- substring(tok, 3)

    if (key %in% c("skip-rerun")) {
      opts[[key]] <- TRUE
      i <- i + 1
      next
    }

    if (i == length(argv)) stop_quietly(paste0("Missing value for ", tok))
    opts[[key]] <- argv[[i + 1]]
    i <- i + 2
  }

  opts
}

prompt_yes <- function(message_text) {
  message(message_text)
  ans <- readline(prompt = "Type YES to continue: ")
  identical(ans, "YES")
}

confirm_or_prompt_yes <- function(message_text, confirm_value = NULL) {
  if (identical(confirm_value, "YES")) {
    message(message_text)
    message("🎃 (auto-confirmed via --confirm-reset YES)")
    return(TRUE)
  }
  prompt_yes(message_text)
}

list_files_recursive <- function(root_dir) {
  if (!dir.exists(root_dir)) return(character(0))
  list.files(root_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
}

rel_from <- function(file_paths, root_dir) {
  root_dir <- normalizePath(root_dir, winslash = "/", mustWork = TRUE)
  file_paths <- normalizePath(file_paths, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root_dir, "/")
  vapply(
    file_paths,
    function(p) {
      if (startsWith(p, prefix)) substring(p, nchar(prefix) + 1) else p
    },
    character(1)
  )
}

copy_tree_fallback <- function(src_dir, dest_dir) {
  ensure_dir(dest_dir)
  files <- list_files_recursive(src_dir)
  if (length(files) == 0) return(invisible(TRUE))
  rel <- rel_from(files, src_dir)
  dest_files <- file.path(dest_dir, rel)
  unique_dirs <- unique(dirname(dest_files))
  for (d in unique_dirs) ensure_dir(d)
  ok <- file.copy(from = files, to = dest_files, overwrite = TRUE, copy.mode = TRUE)
  if (!all(ok)) warning(paste0("Some files failed to copy from ", src_dir))
  invisible(all(ok))
}

copy_tree_fast <- function(src_dir, dest_dir) {
  # On macOS/APFS, "cp -c" requests a copy-on-write clone (fast + space-efficient).
  # If unsupported, fall back to a normal recursive copy.
  cp <- Sys.which("cp")
  if (nzchar(cp)) {
    ensure_dir(dirname(dest_dir))
    res <- suppressWarnings(system2(cp, args = c("-cR", src_dir, dest_dir)))
    if (isTRUE(res == 0)) return(invisible(TRUE))
  }
  copy_tree_fallback(src_dir, dest_dir)
}

md5_map <- function(files) {
  if (length(files) == 0) return(setNames(character(0), character(0)))
  vals <- tryCatch(unname(tools::md5sum(files)), error = function(e) rep(NA_character_, length(files)))
  setNames(as.character(vals), files)
}

manifest_for_roots <- function(old_root, new_root, subdirs = c("tables", "figures", "excel")) {
  old_root <- normalizePath(old_root, winslash = "/", mustWork = TRUE)
  new_root <- normalizePath(new_root, winslash = "/", mustWork = TRUE)

  md5_cache <- new.env(parent = emptyenv())
  md5_one <- function(path) {
    if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NA_character_)
    if (exists(path, envir = md5_cache, inherits = FALSE)) return(get(path, envir = md5_cache, inherits = FALSE))
    val <- tryCatch(as.character(unname(tools::md5sum(path))), error = function(e) NA_character_)
    assign(path, val, envir = md5_cache)
    val
  }

  collect <- function(root, which) {
    out <- list()
    for (sd in subdirs) {
      d <- file.path(root, sd)
      files <- list_files_recursive(d)
      if (length(files) == 0) next
      rel <- file.path(sd, rel_from(files, d))
      info <- file.info(files)
      out[[sd]] <- data.frame(
        which = which,
        relpath = rel,
        abspath = files,
        size = as.numeric(info$size),
        md5 = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    if (length(out) == 0) {
      data.frame(which = character(0), relpath = character(0), abspath = character(0), size = numeric(0), md5 = character(0), stringsAsFactors = FALSE)
    } else {
      do.call(rbind, out)
    }
  }

  old_m <- collect(old_root, "old")
  new_m <- collect(new_root, "new")

  all_paths <- sort(unique(c(old_m$relpath, new_m$relpath)))

  old_by <- split(old_m, old_m$relpath)
  new_by <- split(new_m, new_m$relpath)

  rows <- lapply(all_paths, function(p) {
    o <- old_by[[p]]
    n <- new_by[[p]]

    in_old <- !is.null(o) && nrow(o) > 0
    in_new <- !is.null(n) && nrow(n) > 0

    status <- if (!in_old && in_new) {
      "added"
    } else if (in_old && !in_new) {
      "removed"
    } else {
      # Both exist: avoid hashing unless we need it.
      if (!isTRUE(is.na(o$size[[1]]) || is.na(n$size[[1]])) && !identical(o$size[[1]], n$size[[1]])) {
        "changed"
      } else {
        old_md5 <- md5_one(o$abspath[[1]])
        new_md5 <- md5_one(n$abspath[[1]])
        if (identical(old_md5, new_md5)) "same" else "changed"
      }
    }

    old_md5_val <- if (in_old) md5_one(o$abspath[[1]]) else NA_character_
    new_md5_val <- if (in_new) md5_one(n$abspath[[1]]) else NA_character_

    data.frame(
      relpath = p,
      status = status,
      ext = tolower(tools::file_ext(p)),
      old_md5 = old_md5_val,
      new_md5 = new_md5_val,
      old_size = if (in_old) o$size[[1]] else NA_real_,
      new_size = if (in_new) n$size[[1]] else NA_real_,
      old_abspath = if (in_old) o$abspath[[1]] else NA_character_,
      new_abspath = if (in_new) n$abspath[[1]] else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

normalize_tex_cell <- function(x) {
  x <- gsub("%.*$", "", x)
  x <- gsub("\\\\textbf{", "", x, fixed = TRUE)
  x <- gsub("\\\\emph{", "", x, fixed = TRUE)
  x <- gsub("\\\\textit{", "", x, fixed = TRUE)
  x <- gsub("\\\\texttt{", "", x, fixed = TRUE)
  x <- gsub("\\\\multicolumn\u007b[^}]*\u007d\u007b[^}]*\u007d\u007b", "", x, perl = TRUE)
  x <- gsub("\\\\", "", x, fixed = TRUE)
  x <- gsub("\\$", "", x)
  x <- gsub("[{}]", "", x)
  x <- gsub("\\\\s+", " ", x)
  trimws(x)
}

extract_first_tabular_inside <- function(path) {
  # Returns list(env, begin_line, end_line, inside_lines) for the first tabular-like env.
  if (!file.exists(path)) return(NULL)
  x <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(NULL)

  begin_re <- "\\\\begin\\{(tabularx|tabular|longtable)\\}"
  begin_idx <- grep(begin_re, x, perl = TRUE)
  if (length(begin_idx) == 0) return(NULL)
  begin_idx <- begin_idx[[1]]

  env <- sub(paste0(".*", begin_re, ".*"), "\\1", x[[begin_idx]], perl = TRUE)
  if (!env %in% c("tabular", "tabularx", "longtable")) return(NULL)

  end_re <- paste0("\\\\end\\{", env, "\\}")
  tail_match <- grep(end_re, x[begin_idx:length(x)], perl = TRUE)
  if (length(tail_match) == 0) return(NULL)
  end_idx <- begin_idx + tail_match[[1]] - 1
  if (end_idx <= begin_idx + 1) return(NULL)

  list(
    env = env,
    begin_line = x[[begin_idx]],
    end_line = x[[end_idx]],
    inside_lines = x[(begin_idx + 1):(end_idx - 1)]
  )
}

tex_tabular_rows <- function(inside_lines) {
  # Build row strings by accumulating lines until a row terminator '\\' appears at end-of-line.
  if (length(inside_lines) == 0) return(character(0))

  rows <- character(0)
  buf <- ""

  for (ln in inside_lines) {
    ln <- gsub("%.*$", "", ln)
    if (!nzchar(trimws(ln))) next
    buf <- if (nzchar(buf)) paste0(buf, " ", trimws(ln)) else trimws(ln)

    # End-of-row marker at end-of-line (avoids splitting on \ inside \shortstack etc.)
    if (grepl("\\\\\\\\\\s*$", buf, perl = TRUE)) {
      buf <- sub("\\\\\\\\\\s*$", "", buf, perl = TRUE)
      rows <- c(rows, trimws(buf))
      buf <- ""
    }
  }

  # If the file omits trailing \\ for the last row, keep whatever we saw.
  if (nzchar(trimws(buf))) rows <- c(rows, trimws(buf))
  rows
}

split_tex_row_cells <- function(row_text) {
  # Split on column separators '&' that are not escaped as '\&'.
  if (!nzchar(row_text)) return(character(0))
  unlist(strsplit(row_text, "(?<!\\\\)&", perl = TRUE))
}

tex_cells_matrix <- function(path) {
  tab <- extract_first_tabular_inside(path)
  if (is.null(tab)) return(matrix(character(0), nrow = 0, ncol = 0))

  row_texts <- tex_tabular_rows(tab$inside_lines)
  if (length(row_texts) == 0) return(matrix(character(0), nrow = 0, ncol = 0))

  rows <- list()
  for (rt in row_texts) {
    if (!grepl("&", rt, fixed = TRUE)) next
    cells <- split_tex_row_cells(rt)
    cells <- vapply(cells, normalize_tex_cell, character(1))
    if (length(cells) == 0) next
    rows[[length(rows) + 1]] <- cells
  }

  if (length(rows) == 0) return(matrix(character(0), nrow = 0, ncol = 0))
  max_c <- max(vapply(rows, length, integer(1)))
  mat <- matrix("", nrow = length(rows), ncol = max_c)
  for (i in seq_along(rows)) {
    mat[i, seq_along(rows[[i]])] <- rows[[i]]
  }
  mat
}

tex_cells_matrix_raw <- function(path) {
  # Similar to tex_cells_matrix(), but preserves LaTeX content as much as possible.
  tab <- extract_first_tabular_inside(path)
  if (is.null(tab)) return(matrix(character(0), nrow = 0, ncol = 0))

  row_texts <- tex_tabular_rows(tab$inside_lines)
  if (length(row_texts) == 0) return(matrix(character(0), nrow = 0, ncol = 0))

  rows <- list()
  for (rt in row_texts) {
    if (!grepl("&", rt, fixed = TRUE)) next
    cells <- split_tex_row_cells(rt)
    cells <- vapply(cells, function(x) trimws(x), character(1))
    if (length(cells) == 0) next
    rows[[length(rows) + 1]] <- cells
  }

  if (length(rows) == 0) return(matrix(character(0), nrow = 0, ncol = 0))
  max_c <- max(vapply(rows, length, integer(1)))
  mat <- matrix("", nrow = length(rows), ncol = max_c)
  for (i in seq_along(rows)) {
    mat[i, seq_along(rows[[i]])] <- rows[[i]]
  }
  mat
}

xlsx_cell_changes <- function(old_path, new_path, relpath) {
  # Cell diffs only make sense when both versions exist.
  if (!file.exists(old_path) || !file.exists(new_path)) return(data.frame())

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("openxlsx not available; skipping xlsx cell diffs")
    return(data.frame())
  }

  sheet_names <- character(0)
  if (file.exists(old_path)) sheet_names <- unique(c(sheet_names, openxlsx::getSheetNames(old_path)))
  if (file.exists(new_path)) sheet_names <- unique(c(sheet_names, openxlsx::getSheetNames(new_path)))

  out <- list()

  read_sheet <- function(path, sheet) {
    if (!file.exists(path)) return(matrix("", nrow = 0, ncol = 0))
    df <- tryCatch(openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE), error = function(e) NULL)
    if (is.null(df)) return(matrix("", nrow = 0, ncol = 0))
    if (nrow(df) == 0 && ncol(df) == 0) return(matrix("", nrow = 0, ncol = 0))
    m <- as.matrix(df)
    m[is.na(m)] <- ""
    apply(m, c(1, 2), function(x) trimws(as.character(x)))
  }

  for (sheet in sheet_names) {
    om <- read_sheet(old_path, sheet)
    nm <- read_sheet(new_path, sheet)

    nr <- max(nrow(om), nrow(nm))
    nc <- max(ncol(om), ncol(nm))

    if (nr == 0 || nc == 0) next

    om2 <- matrix("", nrow = nr, ncol = nc)
    nm2 <- matrix("", nrow = nr, ncol = nc)
    if (nrow(om) > 0 && ncol(om) > 0) om2[seq_len(nrow(om)), seq_len(ncol(om))] <- om
    if (nrow(nm) > 0 && ncol(nm) > 0) nm2[seq_len(nrow(nm)), seq_len(ncol(nm))] <- nm

    diffs <- which(om2 != nm2, arr.ind = TRUE)
    if (nrow(diffs) == 0) next

    out[[length(out) + 1]] <- data.frame(
      diff_level = "cell",
      relpath = relpath,
      status = "changed",
      sheet = sheet,
      row = diffs[, 1],
      col = diffs[, 2],
      old_value = om2[diffs],
      new_value = nm2[diffs],
      stringsAsFactors = FALSE
    )
  }

  if (length(out) == 0) data.frame() else do.call(rbind, out)
}

tex_cell_changes <- function(old_path, new_path, relpath) {
  om <- tex_cells_matrix(old_path)
  nm <- tex_cells_matrix(new_path)
  nr <- max(nrow(om), nrow(nm))
  nc <- max(ncol(om), ncol(nm))
  if (nr == 0 || nc == 0) return(data.frame())

  om2 <- matrix("", nrow = nr, ncol = nc)
  nm2 <- matrix("", nrow = nr, ncol = nc)
  if (nrow(om) > 0 && ncol(om) > 0) om2[seq_len(nrow(om)), seq_len(ncol(om))] <- om
  if (nrow(nm) > 0 && ncol(nm) > 0) nm2[seq_len(nrow(nm)), seq_len(ncol(nm))] <- nm

  diffs <- which(om2 != nm2, arr.ind = TRUE)
  if (nrow(diffs) == 0) return(data.frame())

  data.frame(
    diff_level = "cell",
    relpath = relpath,
    status = "changed",
    sheet = NA_character_,
    row = diffs[, 1],
    col = diffs[, 2],
    old_value = om2[diffs],
    new_value = nm2[diffs],
    stringsAsFactors = FALSE
  )
}

write_comparison_tex <- function(run_dir, changed_df) {
  tex_path <- file.path(run_dir, "comparison.tex")

  changed_fig <- changed_df[changed_df$ext %in% c("png", "jpg", "jpeg", "pdf") & grepl("^figures/", changed_df$relpath), , drop = FALSE]
  changed_tab <- changed_df[changed_df$ext %in% c("tex") & grepl("^tables/", changed_df$relpath), , drop = FALSE]

  detok <- function(p) paste0("\\detokenize{", gsub("\\\\", "/", p), "}")

  extract_tabular_block <- function(src_abs, dest_abs) {
    # Extract a tabular-like environment from a .tex file so we can safely box/scale it.
    # Returns a list(ok=TRUE/FALSE, env="tabular"|"tabularx"|"longtable"|NA)
    if (is.na(src_abs) || !nzchar(src_abs) || !file.exists(src_abs)) {
      return(list(ok = FALSE, env = NA_character_))
    }

    x <- tryCatch(readLines(src_abs, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(x) || length(x) == 0) return(list(ok = FALSE, env = NA_character_))

    begin_re <- "\\\\begin\\{(tabularx|tabular|longtable)\\}"
    begin_idx <- grep(begin_re, x, perl = TRUE)
    if (length(begin_idx) == 0) return(list(ok = FALSE, env = NA_character_))
    begin_idx <- begin_idx[[1]]

    env <- sub(paste0(".*", begin_re, ".*"), "\\1", x[[begin_idx]], perl = TRUE)
    if (!env %in% c("tabular", "tabularx", "longtable")) env <- NA_character_
    if (is.na(env)) return(list(ok = FALSE, env = NA_character_))

    end_re <- paste0("\\\\end\\{", env, "\\}")
    tail_match <- grep(end_re, x[begin_idx:length(x)], perl = TRUE)
    if (length(tail_match) == 0) return(list(ok = FALSE, env = env))
    end_idx <- begin_idx + tail_match[[1]] - 1

    ensure_dir(dirname(dest_abs))
    ok <- tryCatch({
      writeLines(x[begin_idx:end_idx], con = dest_abs)
      TRUE
    }, error = function(e) FALSE)

    list(ok = isTRUE(ok), env = env)
  }

  lines <- c(
    "% Auto-generated by code/tools/results_rerun_compare.R",
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=0.75in]{geometry}",
    "\\usepackage{graphicx}",
    "\\usepackage{grffile}",
    "\\usepackage{xcolor}",
    "\\usepackage{hyperref}",
    "\\usepackage{booktabs}",
    "\\usepackage{adjustbox}",
    "\\usepackage{caption}",
    "\\newcommand{\\chg}[2]{\\textcolor{gray}{#1}$\\rightarrow$#2}",
    "\\makeatletter",
    "% Render \\begin{table} blocks inline (avoid floats / nesting errors)",
    "\\renewenvironment{table}[1][]%",
    "{\\par\\medskip\\begingroup\\centering\\def\\@captype{table}}%",
    "{\\par\\endgroup\\medskip}",
    "% Some outputs use table*",
    "\\renewenvironment{table*}[1][]%",
    "{\\par\\medskip\\begingroup\\centering\\def\\@captype{table}}%",
    "{\\par\\endgroup\\medskip}",
    "\\makeatother",
    "\\title{Output comparison}",
    "\\date{}",
    "\\begin{document}",
    "\\maketitle",
    "\\section*{Overview}",
    paste0("Run folder: ", detok(basename(run_dir)), "\\\\\\\\"),
    paste0("Generated: ", as.character(Sys.time()), "\\\\\\\\"),
    "Only changed tables/figures are included below.",
    ""
  )

  sparse_diff_table_result <- function(old_abs, new_abs, relpath) {
    # Build a sparse table that only shows changed cells as old->new.
    # Returns list(status="sparse"|"no_diffs"|"unsafe"|"parse_fail", lines=character()).
    # Heuristic: treat first parsed row as header.
    # For multi-row or fancy headers (e.g., \multicolumn), we copy the header lines verbatim
    # from the source tabular and only reconstruct the body rows.
    old_raw <- tex_cells_matrix_raw(old_abs)
    new_raw <- tex_cells_matrix_raw(new_abs)
    old_norm <- tex_cells_matrix(old_abs)
    new_norm <- tex_cells_matrix(new_abs)

    # Extract the tabular block so we can reuse its begin/end + header rules.
    tabular_block_lines <- function(path) {
      x <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
      if (is.null(x) || length(x) == 0) return(NULL)
      begin_re <- "\\\\begin\\{(tabularx|tabular|longtable)\\}"
      begin_idx <- grep(begin_re, x, perl = TRUE)
      if (length(begin_idx) == 0) return(NULL)
      begin_idx <- begin_idx[[1]]
      env <- sub(paste0(".*", begin_re, ".*"), "\\1", x[[begin_idx]], perl = TRUE)
      if (!env %in% c("tabular", "tabularx", "longtable")) return(NULL)
      end_re <- paste0("\\\\end\\{", env, "\\}")
      tail_match <- grep(end_re, x[begin_idx:length(x)], perl = TRUE)
      if (length(tail_match) == 0) return(NULL)
      end_idx <- begin_idx + tail_match[[1]] - 1
      list(env = env, begin_line = x[[begin_idx]], end_line = x[[end_idx]], inside = x[(begin_idx + 1):(end_idx - 1)])
    }

    tab <- tabular_block_lines(new_abs)
    if (is.null(tab) || identical(tab$env, "longtable")) return(list(status = "unsafe", lines = character(0)))

    # Determine how many header rows there are by finding the first rule boundary.
    hdr_end <- NA_integer_
    mid_idx <- grep("\\\\midrule", tab$inside, perl = TRUE)
    if (length(mid_idx) > 0) {
      hdr_end <- mid_idx[[1]]
    } else {
      hl_idx <- grep("\\\\hline", tab$inside, perl = TRUE)
      if (length(hl_idx) >= 2) hdr_end <- hl_idx[[2]] else if (length(hl_idx) == 1) hdr_end <- hl_idx[[1]]
    }
    if (is.na(hdr_end)) return(list(status = "unsafe", lines = character(0)))

    header_lines <- tab$inside[seq_len(hdr_end)]
    header_rows_count <- sum(grepl("&", header_lines, fixed = TRUE) & grepl("\\\\\\\\", header_lines))

    nr <- max(nrow(old_norm), nrow(new_norm), nrow(old_raw), nrow(new_raw))
    nc <- max(ncol(old_norm), ncol(new_norm), ncol(old_raw), ncol(new_raw))
    if (nr == 0 || nc == 0) return(list(status = "parse_fail", lines = character(0)))

    pad <- function(m) {
      mm <- matrix("", nrow = nr, ncol = nc)
      if (nrow(m) > 0 && ncol(m) > 0) mm[seq_len(nrow(m)), seq_len(ncol(m))] <- m
      mm
    }

    on <- pad(old_norm)
    nn <- pad(new_norm)
    or <- pad(old_raw)
    nrw <- pad(new_raw)

    diffs <- which(on != nn, arr.ind = TRUE)
    if (nrow(diffs) == 0) return(list(status = "no_diffs", lines = character(0)))

    strip_leading_rules <- function(x) {
      x <- trimws(x)
      x <- gsub("^\\\\\\s*(hline|midrule|toprule|bottomrule)\\s*", "", x, perl = TRUE)
      trimws(x)
    }

    keep_rows <- sort(unique(c(seq_len(max(1, header_rows_count)), diffs[, 1])))
    keep_rows <- keep_rows[keep_rows >= 1 & keep_rows <= nr]
    keep_cols <- seq_len(nc)

    # Create sparse matrix with header row intact.
    sparse <- matrix("", nrow = length(keep_rows), ncol = length(keep_cols))
    colnames(sparse) <- NULL
    rownames(sparse) <- NULL

    rmap <- setNames(seq_along(keep_rows), as.character(keep_rows))

    # Preserve header by copying verbatim lines from the source tabular.

    # For each diff cell (except header rows), add row label and changed cell content.
    for (k in seq_len(nrow(diffs))) {
      rr <- diffs[k, 1]
      cc <- diffs[k, 2]
      if (rr <= max(1, header_rows_count)) next
      if (!as.character(rr) %in% names(rmap)) next
      out_r <- rmap[[as.character(rr)]]
      # row label = first column
      sparse[out_r, 1] <- strip_leading_rules(if (nzchar(or[rr, 1])) or[rr, 1] else nrw[rr, 1])
      old_val <- or[rr, cc]
      new_val <- nrw[rr, cc]
      if (!nzchar(old_val)) old_val <- ""
      if (!nzchar(new_val)) new_val <- ""
      sparse[out_r, cc] <- paste0("\\chg{\\detokenize{", strip_leading_rules(old_val), "}}{\\detokenize{", strip_leading_rules(new_val), "}}")
    }

    # If we ended up with no body rows, nothing useful.
    body_keep <- keep_rows[keep_rows > max(1, header_rows_count)]
    if (length(body_keep) == 0) return(list(status = "no_diffs", lines = character(0)))

    # Determine bottom lines (e.g., \bottomrule) to close the table nicely.
    bottom_start <- NA_integer_
    br_idx <- grep("\\\\bottomrule", tab$inside, perl = TRUE)
    if (length(br_idx) > 0) bottom_start <- br_idx[[1]]
    if (is.na(bottom_start)) {
      hl_idx <- grep("\\\\hline", tab$inside, perl = TRUE)
      if (length(hl_idx) > 0) bottom_start <- tail(hl_idx, 1)
    }
    bottom_lines <- if (!is.na(bottom_start) && bottom_start <= length(tab$inside)) tab$inside[bottom_start:length(tab$inside)] else character(0)

    out <- c(
      "\\subsection*{Table (changed cells only)}",
      paste0("\\noindent\\texttt{", detok(relpath), "}\\\\"),
      "\\begingroup",
      "\\footnotesize",
      "\\setlength{\\tabcolsep}{3pt}",
      "\\renewcommand{\\arraystretch}{0.9}",
      "\\begin{center}",
      "\\begin{adjustbox}{max width=\\linewidth, max totalheight=0.85\\textheight, keepaspectratio}",
      tab$begin_line
    )

    # Header lines verbatim
    out <- c(out, header_lines)

    # Body rows (sparse)
    for (rr in body_keep) {
      out_r <- rmap[[as.character(rr)]]
      row_cells <- sparse[out_r, keep_cols]
      out <- c(out, paste0(paste(row_cells, collapse = " & "), " \\\\"))
    }

    out <- c(out,
      bottom_lines,
      tab$end_line,
      "\\end{adjustbox}",
      "\\end{center}",
      "\\endgroup",
      "\\clearpage"
    )
    list(status = "sparse", lines = out)
  }

  if (nrow(changed_fig) > 0) {
    lines <- c(lines, "\\section*{Changed figures}", "\\clearpage")
    for (i in seq_len(nrow(changed_fig))) {
      rp <- changed_fig$relpath[[i]]
      old_p <- file.path("old", rp)
      new_p <- file.path("new", rp)
      lines <- c(lines,
        # Ensure the very first figure starts on a fresh page so Old/New fit together.
        if (i == 1) "\\clearpage" else NULL,
        paste0("\\subsection*{", detok(rp), "}"),
        "\\begingroup",
        "\\centering",
        "\\textbf{Old}\\par\\medskip",
        if (file.exists(file.path(run_dir, old_p))) {
          paste0("\\includegraphics[width=\\linewidth,height=0.40\\textheight,keepaspectratio]{", detok(old_p), "}\\par")
        } else {
          "\\textit{(missing)}\\par"
        },
        "\\vfill",
        "\\textbf{New}\\par\\medskip",
        if (file.exists(file.path(run_dir, new_p))) {
          paste0("\\includegraphics[width=\\linewidth,height=0.40\\textheight,keepaspectratio]{", detok(new_p), "}\\par")
        } else {
          "\\textit{(missing)}\\par"
        },
        "\\endgroup",
        "\\clearpage"
      )
    }
  }

  if (nrow(changed_tab) > 0) {
    lines <- c(lines, "\\section*{Changed tables (.tex)}", "\\clearpage")
    omitted_metadata_only <- character(0)
    tables_added <- character(0)
    tables_removed <- character(0)
    for (i in seq_len(nrow(changed_tab))) {
      rp <- changed_tab$relpath[[i]]
      old_p <- file.path("old", rp)
      new_p <- file.path("new", rp)

      old_abs <- file.path(run_dir, old_p)
      new_abs <- file.path(run_dir, new_p)

      have_old <- file.exists(old_abs)
      have_new <- file.exists(new_abs)
      if (!have_old && have_new) {
        tables_added <- c(tables_added, rp)
        next
      }
      if (have_old && !have_new) {
        tables_removed <- c(tables_removed, rp)
        next
      }

      old_tab_abs <- file.path(run_dir, "old_tabular", rp)
      new_tab_abs <- file.path(run_dir, "new_tabular", rp)

      old_ex <- extract_tabular_block(old_abs, old_tab_abs)
      new_ex <- extract_tabular_block(new_abs, new_tab_abs)

      old_inc <- if (isTRUE(old_ex$ok)) file.path("old_tabular", rp) else old_p
      new_inc <- if (isTRUE(new_ex$ok)) file.path("new_tabular", rp) else new_p

      use_box_old <- isTRUE(old_ex$ok) && !identical(old_ex$env, "longtable")
      use_box_new <- isTRUE(new_ex$ok) && !identical(new_ex$env, "longtable")

      # Prefer a sparse "changed-cells-only" table. Fall back to old/new if we can't build it.
      sparse_res <- NULL
      if (file.exists(old_abs) && file.exists(new_abs)) {
        sparse_res <- sparse_diff_table_result(old_abs, new_abs, rp)
      }

      if (!is.null(sparse_res) && identical(sparse_res$status, "sparse")) {
        lines <- c(lines, sparse_res$lines)
        next
      }
      if (!is.null(sparse_res) && identical(sparse_res$status, "no_diffs")) {
        omitted_metadata_only <- c(omitted_metadata_only, rp)
        next
      }

      lines <- c(lines,
        "\\subsection*{Table}",
        paste0("\\noindent\\texttt{", detok(rp), "}\\\\"),
        "\\subsubsection*{Old}",
        if (file.exists(file.path(run_dir, old_p))) {
          c(
            "\\begingroup",
            "\\footnotesize",
            "\\setlength{\\tabcolsep}{3pt}",
            "\\renewcommand{\\arraystretch}{0.9}",
            if (isTRUE(use_box_old)) paste0("\\begin{adjustbox}{max width=\\linewidth, max totalheight=0.40\\textheight, keepaspectratio}") else NULL,
            paste0("\\input{", detok(old_inc), "}"),
            if (isTRUE(use_box_old)) "\\end{adjustbox}" else NULL,
            "\\endgroup",
            "\\vfill"
          )
        } else {
          "\\textit{(missing)}"
        },
        "\\subsubsection*{New}",
        if (file.exists(file.path(run_dir, new_p))) {
          c(
            "\\begingroup",
            "\\footnotesize",
            "\\setlength{\\tabcolsep}{3pt}",
            "\\renewcommand{\\arraystretch}{0.9}",
            if (isTRUE(use_box_new)) paste0("\\begin{adjustbox}{max width=\\linewidth, max totalheight=0.40\\textheight, keepaspectratio}") else NULL,
            paste0("\\input{", detok(new_inc), "}"),
            if (isTRUE(use_box_new)) "\\end{adjustbox}" else NULL,
            "\\endgroup"
          )
        } else {
          "\\textit{(missing)}"
        },
        "\\clearpage"
      )
    }

    if (length(tables_added) > 0 || length(tables_removed) > 0) {
      lines <- c(lines,
        "\\section*{Tables added/removed}",
        "These .tex tables were added or removed relative to baseline (no meaningful cell-level diff table can be constructed).",
        if (length(tables_added) > 0) c("\\subsection*{Added}", "\\begin{itemize}", paste0("\\item \\texttt{", vapply(tables_added, detok, character(1)), "}"), "\\end{itemize}") else NULL,
        if (length(tables_removed) > 0) c("\\subsection*{Removed}", "\\begin{itemize}", paste0("\\item \\texttt{", vapply(tables_removed, detok, character(1)), "}"), "\\end{itemize}") else NULL,
        "\\clearpage"
      )
    }

    if (length(omitted_metadata_only) > 0) {
      lines <- c(lines,
        "\\section*{Tables omitted (no cell-level changes detected)}",
        "These .tex files differed only in metadata/formatting (e.g., xtable timestamps), so they are omitted from the table comparison view.",
        "\\begin{itemize}",
        paste0("\\item \\texttt{", vapply(omitted_metadata_only, detok, character(1)), "}"),
        "\\end{itemize}",
        "\\clearpage"
      )
    }
  }

  lines <- c(lines, "\\end{document}")
  writeLines(lines, con = tex_path)
  tex_path
}

compile_tex_to_pdf <- function(run_dir, tex_path) {
  pdflatex <- Sys.which("pdflatex")
  if (!nzchar(pdflatex)) {
    message("🪦 pdflatex not found; wrote comparison.tex but did not compile a PDF.")
    return(invisible(FALSE))
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(run_dir)

  res <- suppressWarnings(system2(pdflatex, args = c("-interaction=nonstopmode", "-halt-on-error", basename(tex_path))))
  if (!isTRUE(res == 0)) {
    message("🪦 pdflatex failed; see .log in the run folder.")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))

  baseline_dirs <- c("output/tables", "output/figures", "output/excel")

  if (!nzchar(Sys.which("git"))) stop_quietly("git not found on PATH")

  gi <- git_info()
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sha_tag <- ifelse(is.na(gi$sha), "nogit", gi$sha)
  name_tag <- opts[["run-name"]] %||% ""
  if (nzchar(name_tag)) name_tag <- paste0("_", gsub("[^A-Za-z0-9._-]", "-", name_tag))

  results_build_runs_root <- file.path(git_survey_bias_root, "output", "results_build_runs")
  ensure_dir(results_build_runs_root)
  run_dir <- file.path(results_build_runs_root, paste0(stamp, "_", sha_tag, name_tag))
  ensure_dir(run_dir)
  message("🎃 Run folder: ", run_dir)

  output_root <- file.path(git_survey_bias_root, "output")

  # 1) Establish baseline snapshot (old_full) = git-tracked output/{tables,figures,excel}
  #    - If --skip-rerun and output is dirty, back up current outputs to new_full,
  #      temporarily reset output/ to baseline to snapshot old_full, then restore outputs.
  out_status <- safe_system(paste("git -C", shQuote(git_survey_bias_root), "status --porcelain", paste(baseline_dirs, collapse = " ")))
  is_dirty_out <- !is.na(out_status) && nzchar(trimws(out_status))

  new_full <- if (isTRUE(opts[["skip-rerun"]])) file.path(run_dir, "new_full") else NULL
  if (isTRUE(opts[["skip-rerun"]])) {
    message("🎃 --skip-rerun: snapshotting current output/ as 'new'...")
    ensure_dir(new_full)
    copy_outputs_subdirs(output_root, new_full)
  }

  if (is_dirty_out) {
    ok <- confirm_or_prompt_yes(
      paste0(
        "Some files in output/{tables,figures,excel} differ from the git baseline.\n",
        if (isTRUE(opts[["skip-rerun"]])) {
          "To compute baseline='old' safely, this tool will TEMPORARILY reset output/{tables,figures,excel} to baseline, snapshot it, then restore your current outputs.\n"
        } else {
          "🧌 To rerun from a clean baseline (fresh-clone behavior), this tool will reset output/{tables,figures,excel} before rerunning.\n"
        },
        "This will run: git restore output/tables output/figures output/excel  AND  git clean -fd output/tables output/figures output/excel\n"
      ),
      confirm_value = opts[["confirm-reset"]]
    )

    if (!ok) stop_quietly("Aborted.", status = 2)
    message("🎃 Resetting output/ to git baseline...")
    reset_output_to_git_baseline(baseline_dirs)

    out_status2 <- safe_system(paste("git -C", shQuote(git_survey_bias_root), "status --porcelain", paste(baseline_dirs, collapse = " ")))
    if (!is.na(out_status2) && nzchar(trimws(out_status2))) {
      stop_quietly("output/ is still not clean after reset; aborting.")
    }
  }

  old_full <- file.path(run_dir, "old_full")
  ensure_dir(old_full)
  message("🎃 Snapshotting baseline output/ as 'old'...")
  copy_outputs_subdirs(output_root, old_full)

  # If we were in --skip-rerun mode and output was dirty, restore the user's output now.
  if (isTRUE(opts[["skip-rerun"]]) && is_dirty_out) {
    message("🎃 Restoring your pre-existing output/ (skip-rerun mode)...")
    for (sd in c("tables", "figures", "excel")) {
      tgt <- file.path(output_root, sd)
      if (dir.exists(tgt)) unlink(tgt, recursive = TRUE, force = TRUE)
    }
    copy_outputs_subdirs(new_full, output_root)
  }

  # 2) Rerun results (overwrites output/)
  if (!isTRUE(opts[["skip-rerun"]])) {
    message("🎃 Running results metafile...")
    source("code/create_tables_figures/metafile.R")
  } else {
    message("🎃 skip-rerun enabled; not running results.")
  }

  # 3) Compare baseline vs new
  new_root <- if (isTRUE(opts[["skip-rerun"]])) new_full else output_root
  message("🎃 Comparing baseline vs new output (hashing only when needed)...")
  manifest <- manifest_for_roots(old_full, new_root)

  file_level <- data.frame(
    diff_level = "file",
    relpath = manifest$relpath,
    status = manifest$status,
    ext = manifest$ext,
    old_md5 = manifest$old_md5,
    new_md5 = manifest$new_md5,
    old_size = manifest$old_size,
    new_size = manifest$new_size,
    sheet = NA_character_,
    row = NA_integer_,
    col = NA_integer_,
    old_value = NA_character_,
    new_value = NA_character_,
    stringsAsFactors = FALSE
  )

  changed_files <- manifest[manifest$status != "same", , drop = FALSE]
  n_changed <- sum(manifest$status == "changed")
  n_added <- sum(manifest$status == "added")
  n_removed <- sum(manifest$status == "removed")

  if ((n_changed + n_added + n_removed) == 0) {
    message("🎃 No changes relative to baseline.")
    unlink(run_dir, recursive = TRUE, force = TRUE)
    return(invisible(NULL))
  }

  # Write file-level changes immediately (cell-level diffs can be slow)
  changes_path <- file.path(run_dir, "changes.csv")
  write.csv(file_level, file = changes_path, row.names = FALSE)

  # Copy only changed files into old/ and new/
  ensure_dir(file.path(run_dir, "old"))
  ensure_dir(file.path(run_dir, "new"))

  for (i in seq_len(nrow(changed_files))) {
    rp <- changed_files$relpath[[i]]
    old_p <- changed_files$old_abspath[[i]]
    new_p <- changed_files$new_abspath[[i]]

    if (!is.na(old_p) && file.exists(old_p)) {
      dest <- file.path(run_dir, "old", rp)
      ensure_dir(dirname(dest))
      file.copy(old_p, dest, overwrite = TRUE, copy.mode = TRUE)
    }

    if (!is.na(new_p) && file.exists(new_p)) {
      dest <- file.path(run_dir, "new", rp)
      ensure_dir(dirname(dest))
      file.copy(new_p, dest, overwrite = TRUE, copy.mode = TRUE)
    }
  }

  # old_full/new_full are big; remove them as soon as we've extracted changed files.
  unlink(old_full, recursive = TRUE, force = TRUE)
  if (isTRUE(opts[["skip-rerun"]]) && !is.null(new_full)) unlink(new_full, recursive = TRUE, force = TRUE)

  # Write comparison.tex/pdf early so it exists even if cell diffs take time
  changed_for_tex <- manifest[manifest$status != "same", , drop = FALSE]
  tex_path <- write_comparison_tex(run_dir, changed_for_tex)
  compile_tex_to_pdf(run_dir, tex_path)

  # Cell diffs for .tex and .xlsx
  append_cell_diffs <- function(df, ext) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
    df$ext <- ext
    df$old_md5 <- NA_character_
    df$new_md5 <- NA_character_
    df$old_size <- NA_real_
    df$new_size <- NA_real_

    # Ensure column set/order matches file_level
    want <- names(file_level)
    for (nm in setdiff(want, names(df))) df[[nm]] <- NA
    df <- df[, want, drop = FALSE]

    utils::write.table(
      df,
      file = changes_path,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE,
      quote = TRUE
    )
    invisible(NULL)
  }

  for (i in seq_len(nrow(changed_files))) {
    rp <- changed_files$relpath[[i]]
    ext <- tolower(tools::file_ext(rp))
    st <- changed_files$status[[i]]

    old_p <- file.path(run_dir, "old", rp)
    new_p <- file.path(run_dir, "new", rp)

    if (ext == "xlsx") {
      if (!identical(st, "changed")) {
        message("🎃 Skipping .xlsx cell diffs (status=", st, "): ", rp)
      } else if (!file.exists(old_p) || !file.exists(new_p)) {
        message("🎃 Skipping .xlsx cell diffs (missing old/new): ", rp)
      } else {
        message("🎃 Computing .xlsx cell diffs for: ", rp)
        df <- xlsx_cell_changes(old_p, new_p, rp)
        append_cell_diffs(df, "xlsx")
      }
    }

    if (ext == "tex") {
      message("🎃 Skipping .tex cell diffs (handled in comparison.pdf): ", rp)
    }
  }

  # changes.csv already written (file-level) and appended (cell-level)

  # Full snapshots already pruned above.

  # Minimal metadata
  meta <- list(
    created_at = as.character(Sys.time()),
    git_sha_short = gi$sha,
    git_dirty = gi$dirty,
    n_changed = n_changed,
    n_added = n_added,
    n_removed = n_removed
  )
  tryCatch(jsonlite::write_json(meta, file.path(run_dir, "meta.json"), auto_unbox = TRUE, pretty = TRUE), error = function(e) {
    dput(meta, file = file.path(run_dir, "meta.R"))
  })

  message("🎃 Run bundle written to: ", run_dir)
  message("🎃 - changes.csv")
  message("🎃 - comparison.tex (and comparison.pdf if pdflatex is installed)")

  invisible(run_dir)
}

main()
