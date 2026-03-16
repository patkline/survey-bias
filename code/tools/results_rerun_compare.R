# ------------------------------------------------------------------------------
# Purpose: Rerun results and generate a compact before/after comparison bundle.
#
# Baseline definition:
# - Default baseline is git-tracked output/ from `origin/main`
# - Use --baseline origin-current to compare against `origin/<current_branch>`
# - Use --baseline current to compare against the local current branch
#
# What it does:
# 1) Creates a run folder under output/results_build_runs (or Dropbox mirror output root).
# 2) For origin-based baselines, verifies ref sync + baseline LFS availability before proceeding.
# 3) Snapshots baseline output/{tables,figures,excel} into old_full
#    (via temporary worktree for origin-based refs; direct copy for baseline=current).
# 4) If --skip-rerun is set, snapshots current output as new_full.
#    Otherwise runs code/create_tables_figures/metafile.R and uses active output/ as "new".
# 5) Compares old vs new.
#    If differences exist, writes:
#    - changes.csv (file-level diffs; optional .xlsx cell-level diffs with --with-xlsx-cell-diffs)
#    - comparison.tex (+ comparison.pdf if pdflatex is available) for changed tables/figures
#    - old/ and new/ containing only the changed files needed for comparison
# 6) If there are zero changes, deletes the run folder.
# ------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  source("code/globals.R")
}))

# ------------------------------------------------------------------------------
# Utility helpers
# ------------------------------------------------------------------------------
# Return fallback value y when x is NULL/empty/NA.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) y else x

# Exit script immediately with a message and status code.
stop_quietly <- function(message_text, status = 1) {
  message(message_text)
  quit(save = "no", status = status, runLast = FALSE)
}

# Create directory recursively iff missing.
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

# Wrapper around system() that returns a single string and never throws.
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

# ------------------------------------------------------------------------------
# Git helpers
# ------------------------------------------------------------------------------
# Collect short SHA and dirty status for metadata/run naming.
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

# Check whether a git ref exists locally/remotely.
git_ref_exists <- function(ref) {
  res <- suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "rev-parse", "--verify", "--quiet", ref), stdout = FALSE, stderr = FALSE))
  isTRUE(res == 0)
}

resolve_baseline <- function(baseline_opt, current_branch) {
  if (identical(baseline_opt, "current")) {
    branch_label <- ifelse(nzchar(current_branch), current_branch, "HEAD")
    return(list(mode = "current", ref = "HEAD", label = paste0("current branch (", branch_label, ")")))
  }

  if (identical(baseline_opt, "origin-current")) {
    if (!nzchar(current_branch)) {
      stop_quietly("🧌 Could not determine current branch name for --baseline origin-current. Switch to a branch and retry, or use --baseline main.")
    }

    remote_ref <- paste0("origin/", current_branch)
    if (!git_ref_exists(remote_ref)) {
      stop_quietly(paste0("🧌 Could not resolve baseline ref `", remote_ref, "`. Run `git fetch origin ", current_branch, "` (or push the branch) and retry, or use --baseline main/current."))
    }

    return(list(mode = "ref", ref = remote_ref, label = paste0(remote_ref, " (current branch on origin)")))
  }

  if (!identical(baseline_opt, "main")) {
    stop_quietly("Invalid --baseline value. Use --baseline main (default), --baseline origin-current, or --baseline current.")
  }

  # Use remote-tracking main so baseline reflects origin by default.
  if (git_ref_exists("origin/main")) return(list(mode = "ref", ref = "origin/main", label = "origin/main"))

  stop_quietly("🧌 Could not resolve baseline ref `origin/main`. Run `git fetch origin main` and retry, or use --baseline origin-current/current.")
}

# Return full SHA for a git ref (NA when missing).
git_ref_sha <- function(ref) {
  out <- tryCatch(
    suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "rev-parse", "--verify", ref), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (length(out) == 0) return(NA_character_)
  trimws(out[[1]])
}

# Return branch name for refs of the form origin/<branch>; otherwise NA.
origin_branch_from_ref <- function(ref) {
  if (!startsWith(ref, "origin/")) return(NA_character_)
  branch <- sub("^origin/", "", ref)
  if (!nzchar(branch)) return(NA_character_)
  branch
}

# Query origin for the latest SHA of a branch; returns NA when unavailable.
remote_origin_branch_sha <- function(branch) {
  out <- tryCatch(
    suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "ls-remote", "--heads", "origin", branch), stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  if (length(out) == 0) return(NA_character_)
  hit <- out[grepl(paste0("refs/heads/", branch, "$"), out)]
  if (length(hit) == 0) return(NA_character_)
  fields <- strsplit(trimws(hit[[1]]), "\\s+")[[1]]
  if (length(fields) == 0) return(NA_character_)
  trimws(fields[[1]])
}

# Check whether local origin/<branch> matches current remote head.
check_origin_ref_synced <- function(origin_ref) {
  branch <- origin_branch_from_ref(origin_ref)
  if (is.na(branch) || !nzchar(branch)) {
    return(list(applicable = FALSE, synced = TRUE, status = "not-origin", branch = NA_character_, local_sha = NA_character_, remote_sha = NA_character_))
  }

  local_sha <- git_ref_sha(origin_ref)
  remote_sha <- remote_origin_branch_sha(branch)

  if (is.na(local_sha) || !nzchar(local_sha)) {
    return(list(applicable = TRUE, synced = FALSE, status = "missing-local", branch = branch, local_sha = local_sha, remote_sha = remote_sha))
  }

  if (is.na(remote_sha) || !nzchar(remote_sha)) {
    return(list(applicable = TRUE, synced = FALSE, status = "unknown-remote", branch = branch, local_sha = local_sha, remote_sha = remote_sha))
  }

  synced <- identical(local_sha, remote_sha)
  list(
    applicable = TRUE,
    synced = synced,
    status = if (synced) "synced" else "stale",
    branch = branch,
    local_sha = local_sha,
    remote_sha = remote_sha
  )
}

# Parse git-lfs pointer text into oid + size; NULL when file is not an LFS pointer.
parse_lfs_pointer <- function(file_text) {
  if (is.null(file_text) || !nzchar(file_text)) return(NULL)
  lines <- trimws(unlist(strsplit(file_text, "\n", fixed = TRUE), use.names = FALSE))
  if (!any(lines == "version https://git-lfs.github.com/spec/v1")) return(NULL)

  oid_line <- lines[grepl("^oid sha256:[0-9a-f]{64}$", lines)][1]
  size_line <- lines[grepl("^size [0-9]+$", lines)][1]
  if (is.na(oid_line) || is.na(size_line)) return(NULL)

  oid <- sub("^oid sha256:", "", oid_line)
  size <- suppressWarnings(as.numeric(sub("^size ", "", size_line)))
  if (!nzchar(oid) || is.na(size)) return(NULL)

  list(oid = oid, size = size)
}

# List all files under a ref for selected paths.
git_ls_tree_paths <- function(ref, rel_paths) {
  out <- tryCatch(
    suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "ls-tree", "-r", "--name-only", ref, "--", rel_paths), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  out <- trimws(out)
  out[nzchar(out)]
}

# Check local LFS cache coverage for a ref under selected output subpaths.
lfs_availability_for_ref <- function(ref, rel_paths) {
  files <- git_ls_tree_paths(ref, rel_paths)
  if (length(files) == 0) {
    return(list(
      total_lfs_objects = 0L,
      missing_objects = 0L,
      total_lfs_bytes = 0,
      missing_bytes = 0,
      missing_preview = character(0)
    ))
  }

  ptr_rows <- vector("list", length(files))
  n_ptr <- 0L

  for (rp in files) {
    blob <- tryCatch(
      suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "show", paste0(ref, ":", rp)), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    if (length(blob) == 0) next

    ptr <- parse_lfs_pointer(paste(blob, collapse = "\n"))
    if (is.null(ptr)) next

    n_ptr <- n_ptr + 1L
    ptr_rows[[n_ptr]] <- data.frame(
      relpath = rp,
      oid = ptr$oid,
      size = as.numeric(ptr$size),
      stringsAsFactors = FALSE
    )
  }

  if (n_ptr == 0L) {
    return(list(
      total_lfs_objects = 0L,
      missing_objects = 0L,
      total_lfs_bytes = 0,
      missing_bytes = 0,
      missing_preview = character(0)
    ))
  }

  ptr_df <- do.call(rbind, ptr_rows[seq_len(n_ptr)])
  uniq <- ptr_df[!duplicated(ptr_df$oid), c("oid", "size"), drop = FALSE]

  obj_root <- file.path(git_survey_bias_root, ".git", "lfs", "objects")
  uniq$local_path <- file.path(obj_root, substr(uniq$oid, 1, 2), substr(uniq$oid, 3, 4), uniq$oid)
  uniq$present <- file.exists(uniq$local_path)

  missing <- uniq[!uniq$present, , drop = FALSE]

  list(
    total_lfs_objects = as.integer(nrow(uniq)),
    missing_objects = as.integer(nrow(missing)),
    total_lfs_bytes = sum(uniq$size, na.rm = TRUE),
    missing_bytes = sum(missing$size, na.rm = TRUE),
    missing_preview = if (nrow(missing) == 0) character(0) else missing$oid[seq_len(min(5, nrow(missing)))]
  )
}

# Convert bytes to compact human-readable units.
format_bytes <- function(bytes) {
  if (is.na(bytes) || !is.finite(bytes) || bytes < 0) return("unknown size")
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  idx <- 1L
  while (value >= 1024 && idx < length(units)) {
    value <- value / 1024
    idx <- idx + 1L
  }
  if (idx == 1L) {
    paste0(format(round(value, 0), big.mark = ","), " ", units[[idx]])
  } else {
    paste0(format(round(value, 2), nsmall = 2, trim = TRUE), " ", units[[idx]])
  }
}

# Enforce that remote baselines are synced locally (or block/repair based on storage mode).
ensure_baseline_ref_synced <- function(baseline_cfg, storage_mode) {
  if (!identical(baseline_cfg$mode, "ref")) return(invisible(TRUE))

  sync <- check_origin_ref_synced(baseline_cfg$ref)
  if (!isTRUE(sync$applicable)) return(invisible(TRUE))
  if (isTRUE(sync$synced)) {
    message("🎃 Baseline ref is synced with origin: ", baseline_cfg$ref, " @ ", substr(sync$local_sha, 1, 12))
    return(invisible(TRUE))
  }

  stale_reason <- switch(
    sync$status,
    "missing-local" = paste0("Local ref `", baseline_cfg$ref, "` is missing."),
    "unknown-remote" = paste0("Could not verify remote SHA for `origin/", sync$branch, "` (network/remote unavailable)."),
    "stale" = paste0(
      "Local `", baseline_cfg$ref, "` is stale.\n",
      "Local SHA:  ", sync$local_sha, "\n",
      "Remote SHA: ", sync$remote_sha
    ),
    paste0("Baseline ref `", baseline_cfg$ref, "` is not verified.")
  )

  fetch_cmd <- paste0("git fetch origin ", sync$branch)
  if (identical(storage_mode, "dropbox")) {
    stop_quietly(
      paste0(
        "🧌 Baseline sync check failed: ", stale_reason, "\n",
        "Dropbox mode blocks comparison when baseline origin refs are not verified/synced.\n",
        "Run `", fetch_cmd, "` and retry."
      )
    )
  }

  ok <- confirm_or_prompt_yes(
    paste0(
      "🧌 Baseline sync check failed: ", stale_reason, "\n",
      "To continue in github mode, this tool must run:\n",
      fetch_cmd, "\n"
    )
  )
  if (!ok) stop_quietly("Aborted.", status = 2)

  res <- suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "fetch", "origin", sync$branch)))
  if (!isTRUE(res == 0)) {
    stop_quietly(
      paste0(
        "🧌 Failed to sync baseline ref via `", fetch_cmd, "`.\n",
        "Fix connectivity/permissions and retry."
      )
    )
  }

  sync2 <- check_origin_ref_synced(baseline_cfg$ref)
  if (!isTRUE(sync2$synced)) {
    stop_quietly(
      paste0(
        "🧌 Baseline ref still not synced after `", fetch_cmd, "`.\n",
        "Local SHA:  ", sync2$local_sha %||% "NA", "\n",
        "Remote SHA: ", sync2$remote_sha %||% "NA", "\n",
        "Retry after resolving remote/ref issues."
      )
    )
  }

  message("🎃 Baseline ref synced: ", baseline_cfg$ref, " @ ", substr(sync2$local_sha, 1, 12))
  invisible(TRUE)
}

# Enforce that baseline LFS objects are locally available (or block/repair by mode).
ensure_baseline_lfs_available <- function(baseline_cfg, storage_mode, rel_paths) {
  if (!identical(baseline_cfg$mode, "ref")) return(invisible(TRUE))

  if (!nzchar(Sys.which("git-lfs"))) {
    stop_quietly("🧌 git-lfs not found on PATH; cannot verify baseline LFS object availability.")
  }

  check <- lfs_availability_for_ref(baseline_cfg$ref, rel_paths)
  if (check$total_lfs_objects == 0L) {
    message("🎃 No LFS-tracked files detected under baseline output paths.")
    return(invisible(TRUE))
  }

  if (check$missing_objects == 0L) {
    message("🎃 Baseline LFS cache check passed (all required objects are local).")
    return(invisible(TRUE))
  }

  branch <- origin_branch_from_ref(baseline_cfg$ref)
  include_spec <- paste(rel_paths, collapse = ",")
  fetch_cmd <- if (!is.na(branch) && nzchar(branch)) {
    paste0("git lfs fetch --include=\"", include_spec, "\" origin ", branch)
  } else {
    paste0("git lfs fetch --include=\"", include_spec, "\" origin ", baseline_cfg$ref)
  }

  missing_summary <- paste0(
    check$missing_objects, " missing LFS object(s), estimated download ",
    format_bytes(check$missing_bytes), "."
  )

  if (identical(storage_mode, "dropbox")) {
    stop_quietly(
      paste0(
        "🧌 Baseline LFS cache check failed: ", missing_summary, "\n",
        "Dropbox mode blocks this comparison to avoid LFS downloads.\n",
        "Switch to github mode or prefetch manually with:\n",
        fetch_cmd
      )
    )
  }

  ok <- confirm_or_prompt_yes(
    paste0(
      "🧌 Baseline LFS cache check failed: ", missing_summary, "\n",
      "To continue in github mode, this tool must download the missing baseline objects via:\n",
      fetch_cmd, "\n"
    )
  )
  if (!ok) stop_quietly("Aborted.", status = 2)

  fetch_args <- c("-C", git_survey_bias_root, "lfs", "fetch", paste0("--include=", include_spec))
  if (!is.na(branch) && nzchar(branch)) {
    fetch_args <- c(fetch_args, "origin", branch)
  } else {
    fetch_args <- c(fetch_args, "origin", baseline_cfg$ref)
  }

  res <- suppressWarnings(system2("git", args = fetch_args))
  if (!isTRUE(res == 0)) {
    stop_quietly(
      paste0(
        "🧌 Failed to fetch baseline LFS objects.\n",
        "Run `", fetch_cmd, "` manually and retry."
      )
    )
  }

  check2 <- lfs_availability_for_ref(baseline_cfg$ref, rel_paths)
  if (check2$missing_objects > 0L) {
    stop_quietly(
      paste0(
        "🧌 Baseline LFS objects are still missing after fetch (",
        check2$missing_objects, " object(s), ",
        format_bytes(check2$missing_bytes), ").\n",
        "Retry `", fetch_cmd, "` and verify access to LFS objects."
      )
    )
  }

  message("🎃 Baseline LFS cache check passed after fetch.")
  invisible(TRUE)
}

# Create temporary detached worktree for baseline snapshots.
add_temp_worktree <- function(ref) {
  wt_dir <- tempfile(pattern = "results_compare_baseline_")
  # Disable hooks for this temporary internal checkout so fail-closed repo hooks do not block baseline extraction.
  res <- suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "-c", "core.hooksPath=/dev/null", "worktree", "add", "--detach", wt_dir, ref)))
  if (!isTRUE(res == 0)) {
    stop_quietly(paste0("Failed to create temporary worktree for baseline ref `", ref, "`."))
  }
  wt_dir
}

# Replace LFS pointer files in a temporary baseline worktree using local LFS cache only.
materialize_worktree_lfs <- function(worktree_dir, rel_paths) {
  if (!nzchar(Sys.which("git-lfs"))) return(invisible(TRUE))
  res <- suppressWarnings(system2("git", args = c("-C", worktree_dir, "lfs", "checkout", "--", rel_paths)))
  if (!isTRUE(res == 0)) {
    stop_quietly(
      paste0(
        "🧌 Failed to materialize LFS files in temporary baseline worktree.\n",
        "Run `git lfs checkout -- ", paste(rel_paths, collapse = " "), "` and retry."
      )
    )
  }
  invisible(TRUE)
}

remove_temp_worktree <- function(path) {
  if (is.null(path) || !nzchar(path)) return(invisible(NULL))
  suppressWarnings(system2("git", args = c("-C", git_survey_bias_root, "worktree", "remove", "--force", path)))
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Argument and prompt helpers
# ------------------------------------------------------------------------------
# Parse command-line options and validate baseline mode.
parse_args <- function(argv) {
  opts <- list(
    `run-name` = NULL,
    `skip-rerun` = FALSE,
    `with-xlsx-cell-diffs` = FALSE,
    baseline = "main"
  )

  i <- 1
  while (i <= length(argv)) {
    tok <- argv[[i]]
    if (!startsWith(tok, "--")) {
      stop_quietly(paste0("Unexpected token: ", tok))
    }
    key <- substring(tok, 3)

    if (key %in% c("skip-rerun", "with-xlsx-cell-diffs")) {
      opts[[key]] <- TRUE
      i <- i + 1
      next
    }

    if (i == length(argv)) stop_quietly(paste0("Missing value for ", tok))
    opts[[key]] <- argv[[i + 1]]
    i <- i + 2
  }

  opts[["baseline"]] <- tolower(trimws(as.character(opts[["baseline"]] %||% "main")))
  if (!(opts[["baseline"]] %in% c("main", "origin-current", "current"))) {
    stop_quietly("Invalid --baseline value. Use --baseline main (default), --baseline origin-current, or --baseline current.")
  }

  opts
}

# Interactive confirmation helper used before fetch/download actions.
prompt_yes <- function(message_text) {
  message(message_text)
  ans <- readline(prompt = "Type YES to continue: ")
  identical(ans, "YES")
}

confirm_or_prompt_yes <- function(message_text) prompt_yes(message_text)

# ------------------------------------------------------------------------------
# Path, copy, and manifest helpers
# ------------------------------------------------------------------------------
# Recursively list files under root; return empty vector when root is missing.
list_files_recursive <- function(root_dir) {
  if (!dir.exists(root_dir)) return(character(0))
  files <- list.files(root_dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
  # Ignore Office lock files (~$...) so transient editor artifacts do not pollute diffs.
  files[!grepl("^~\\$", basename(files))]
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
  # Paths with spaces are routed directly to fallback copy to avoid shell splitting issues in cp invocation.
  if (grepl("\\s", src_dir) || grepl("\\s", dest_dir)) {
    return(copy_tree_fallback(src_dir, dest_dir))
  }

  cp <- Sys.which("cp")
  if (nzchar(cp)) {
    ensure_dir(dirname(dest_dir))
    res <- suppressWarnings(system2(cp, args = c("-cR", src_dir, dest_dir)))
    if (isTRUE(res == 0)) return(invisible(TRUE))
  }
  copy_tree_fallback(src_dir, dest_dir)
}

# Map absolute file paths to md5 hashes.
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

# ------------------------------------------------------------------------------
# Table parsing and cell-diff helpers
# ------------------------------------------------------------------------------
# Normalize LaTeX cell text for stable comparisons.
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

# ------------------------------------------------------------------------------
# Comparison bundle rendering helpers
# ------------------------------------------------------------------------------
# Build comparison.tex showing changed figures/tables.
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

# ------------------------------------------------------------------------------
# Main execution
# ------------------------------------------------------------------------------
# Build snapshots, rerun outputs (optional), and produce comparison bundle.
main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))

  # Resolve output root from globals.R so behavior follows storage mode.
  output_root <- normalizePath(output, winslash = "/", mustWork = FALSE)
  output_subdirs <- c("tables", "figures", "excel")

  # Determine whether output root lives inside the git repo.
  repo_root_norm <- normalizePath(git_survey_bias_root, winslash = "/", mustWork = TRUE)
  repo_prefix <- paste0(repo_root_norm, "/")
  output_in_repo <- startsWith(output_root, repo_prefix)

  # Build output path relative to repo root for git commands/worktree baselines.
  output_rel <- if (output_in_repo) {
    substring(output_root, nchar(repo_prefix) + 1)
  } else {
    basename(output_root)
  }

  # Build target subdirectories used for git status checks and baseline LFS preflight scope.
  baseline_dirs <- file.path(output_rel, output_subdirs)
  baseline_dirs_cmd <- paste(baseline_dirs, collapse = " ")

  if (!nzchar(Sys.which("git"))) stop_quietly("git not found on PATH")

  gi <- git_info()
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  sha_tag <- ifelse(is.na(gi$sha), "nogit", gi$sha)
  name_tag <- opts[["run-name"]] %||% ""
  if (nzchar(name_tag)) name_tag <- paste0("_", gsub("[^A-Za-z0-9._-]", "-", name_tag))

  results_build_runs_root <- file.path(output_root, "results_build_runs")
  ensure_dir(results_build_runs_root)
  run_dir <- file.path(results_build_runs_root, paste0(stamp, "_", sha_tag, name_tag))
  ensure_dir(run_dir)
  message("🎃 Run folder: ", run_dir)

  current_branch <- trimws(safe_system(paste("git -C", shQuote(git_survey_bias_root), "branch --show-current")) %||% "")
  baseline_cfg <- resolve_baseline(opts[["baseline"]], current_branch)
  message("🎃 Comparison baseline: ", baseline_cfg$label)

  # Use globals switch as storage mode source of truth.
  storage_mode <- tolower(trimws(as.character(data_and_output_storage_location %||% ifelse(output_in_repo, "github", "dropbox"))))
  if (!(storage_mode %in% c("github", "dropbox"))) {
    stop_quietly("🧌 Invalid data_and_output_storage_location in globals.R. Use \"github\" or \"dropbox\".")
  }
  message("🎃 Storage mode: ", storage_mode)

  # Preflight checks for ref-based baselines:
  # 1) ensure local origin ref is synced with remote when applicable
  # 2) ensure required baseline LFS objects are present locally (or explicitly fetched in github mode)
  if (identical(baseline_cfg$mode, "ref")) {
    ensure_baseline_ref_synced(baseline_cfg, storage_mode)
    ensure_baseline_lfs_available(baseline_cfg, storage_mode, baseline_dirs)
  }

  # 1) Warn when output is dirty in repo mode; do not auto-reset/clean.
  out_status <- ""
  is_dirty_out <- FALSE

  if (output_in_repo) {
    out_status <- safe_system(paste("git -C", shQuote(git_survey_bias_root), "status --porcelain", baseline_dirs_cmd))
    is_dirty_out <- !is.na(out_status) && nzchar(trimws(out_status))
    if (is_dirty_out) {
      if (isTRUE(opts[["skip-rerun"]])) {
        message(
          "🧌 output subdirectories are dirty and --skip-rerun is set.\n",
          "Comparison will use current local output exactly as-is (including stale/untracked files)."
        )
      } else {
        message(
          "🧌 output subdirectories are dirty.\n",
          "Rerun will proceed without reset/clean; stale/untracked files may appear as changes.\n",
          "If you want clean-start rerun semantics, run manually:\n",
          "git -C ", git_survey_bias_root, " restore ", baseline_dirs_cmd, "\n",
          "git -C ", git_survey_bias_root, " clean -fd ", baseline_dirs_cmd
        )
      }
    }
  } else {
    message("🎃 Output path is outside the git repo; skipping repo-dirty checks.")
  }

  new_full <- if (isTRUE(opts[["skip-rerun"]])) file.path(run_dir, "new_full") else NULL
  if (isTRUE(opts[["skip-rerun"]])) {
    message("🎃 --skip-rerun: snapshotting current output/ as 'new'...")
    ensure_dir(new_full)
    copy_outputs_subdirs(output_root, new_full)
  }

  baseline_output_root <- output_root
  baseline_worktree_dir <- NULL
  if (!identical(baseline_cfg$mode, "current")) {
    message("🎃 Preparing baseline outputs from ref: ", baseline_cfg$ref)
    baseline_worktree_dir <- add_temp_worktree(baseline_cfg$ref)
    on.exit(remove_temp_worktree(baseline_worktree_dir), add = TRUE)
    # Ensure baseline binaries come from local LFS cache rather than pointer text files.
    materialize_worktree_lfs(baseline_worktree_dir, baseline_dirs)
    # Mirror output root location inside worktree for baseline extraction from main/origin-main.
    baseline_output_root <- file.path(baseline_worktree_dir, output_rel)
  }

  old_full <- file.path(run_dir, "old_full")
  ensure_dir(old_full)
  message("🎃 Snapshotting baseline output/ as 'old'...")
  copy_outputs_subdirs(baseline_output_root, old_full)

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

  # Optional cell diffs for .xlsx.
  if (!isTRUE(opts[["with-xlsx-cell-diffs"]])) {
    message("🎃 --with-xlsx-cell-diffs not set; skipping .xlsx cell-level diffs.")
  }

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
      if (isTRUE(opts[["with-xlsx-cell-diffs"]])) {
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
    baseline_mode = opts[["baseline"]],
    xlsx_cell_diffs_enabled = isTRUE(opts[["with-xlsx-cell-diffs"]]),
    baseline_label = baseline_cfg$label,
    baseline_ref = baseline_cfg$ref,
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
