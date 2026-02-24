# ------------------------------------------------------------------------------
# Purpose: Build a firm-by-SIC crosswalk from RefUSA and 
# write industry_map.xlsx
#
# Created: Nico Rotundo 2026-02-23 
# ------------------------------------------------------------------------------

# Source project globals so required packages are loaded.
source("code/globals.R")

# ------------------------------------------------------------------------------
# Import and prepare long survey data for crosswalk construction
# ------------------------------------------------------------------------------

# Import long_survey.csv.
long_survey <- read.csv("data/processed/long_survey.csv", stringsAsFactors = FALSE)

# Confirm ResponseId and option_number columns exist in long_survey.
if (!all(c("ResponseId", "option_number") %in% names(long_survey))) {
  # Stop execution when required long_survey row-id columns are missing.
  stop("long_survey.csv must contain columns: ResponseId, option_number")
}

# Confirm ResponseId and option_number uniquely identifies observations.
stopifnot(nrow(long_survey) == dplyr::n_distinct(long_survey$ResponseId, long_survey$option_number))

# Confirm firm_clean column exists in long_survey.
if (!("firm_clean" %in% names(long_survey))) {
  # Stop execution when firm_clean column is missing.
  stop("long_survey.csv must contain column: firm_clean")
}

# Count rows where firm_clean is missing or blank.
firm_clean_missing_n <- sum(is.na(long_survey$firm_clean) | trimws(long_survey$firm_clean) == "")

# Stop when any firm_clean value is missing or blank.
if (firm_clean_missing_n > 0) {
  # Abort because firm_clean must be complete by design.
  stop(sprintf("firm_clean has %s missing/blank rows; expected 0", firm_clean_missing_n))
}

# Report that firm_clean is complete after import.
message(sprintf(
  "Confirmed long_survey unique firm identifier column is firm_clean with %s non-missing rows.",
  nrow(long_survey)
))

# Trim whitespace from firm_clean values.
long_survey$firm_clean <- trimws(long_survey$firm_clean)

# Sort by firm_clean to mimic Stata's: sort firm_clean.
long_survey <- long_survey %>% dplyr::arrange(firm_clean)

# Keep the first row within each firm_clean to mimic: by firm_clean: keep if _n==1.
long_survey_unique_firms <- long_survey %>%
  # Group by firm_clean before taking the first row.
  dplyr::group_by(firm_clean) %>%
  # Keep the first row in each firm_clean group.
  dplyr::slice(1L) %>%
  # Remove grouping after deduplication.
  dplyr::ungroup()

# Verify isid firm_clean after deduplication.
if (nrow(long_survey_unique_firms) != dplyr::n_distinct(long_survey_unique_firms$firm_clean)) {
  # Stop if firm_clean is not unique after dedupe.
  stop("firm_clean is not unique after deduplication")
}

# Report that firm_clean is a unique identifier in the deduplicated firm list.
message(sprintf(
  "Confirmed isid firm_clean after deduplication; %s unique firms.",
  nrow(long_survey_unique_firms)
))

# ------------------------------------------------------------------------------
# Import and validate RefUSA structure for matching
# ------------------------------------------------------------------------------

# Import only the RefUSA header to validate columns immediately after import.
refusa_header <- readr::read_csv(
  # Read the RefUSA text file path directly.
  file = "data/external/2019_Business_Academic_QCQ.txt.gz",
  # Read zero data rows to get only column names.
  n_max = 0,
  # Suppress column type printout.
  show_col_types = FALSE
)

# Confirm RefUSA required columns exist after import.
if (!all(c("IDCode", "ABI", "Company", "Primary SIC Code") %in% names(refusa_header))) {
  # Stop execution when required RefUSA columns are missing.
  stop("RefUSA must contain columns: IDCode, ABI, Company, Primary SIC Code")
}

# Initialize hashed storage for seen ABI values in uniqueness assertion.
refusa_abi_seen <- new.env(hash = TRUE, parent = emptyenv())

# Initialize duplicate flag for ABI uniqueness assertion.
refusa_abi_duplicate_found <- FALSE

# Initialize missing ABI counter for ABI uniqueness assertion.
refusa_abi_missing_n <- 0L

# Read RefUSA ABI column in chunks for uniqueness assertion.
readr::read_csv_chunked(
  # Read RefUSA file path directly.
  file = "data/external/2019_Business_Academic_QCQ.txt.gz",
  # Use callback to check ABI uniqueness.
  callback = readr::DataFrameCallback$new(function(chunk, pos) {
    # Trim ABI values in current chunk.
    abi_values <- trimws(chunk$ABI)
    # Convert missing ABI values to empty strings.
    abi_values[is.na(abi_values)] <- ""
    # Add missing ABI count from this chunk.
    refusa_abi_missing_n <<- refusa_abi_missing_n + sum(abi_values == "")
    # Skip duplicate scan when duplicate already found.
    if (refusa_abi_duplicate_found) return(invisible(NULL))
    # Keep only non-empty ABI values for uniqueness checks.
    abi_values <- abi_values[abi_values != ""]
    # Iterate through ABI values in this chunk.
    for (abi in abi_values) {
      # Flag duplicate when ABI already exists in seen set.
      if (exists(abi, envir = refusa_abi_seen, inherits = FALSE)) {
        # Set duplicate flag to TRUE.
        refusa_abi_duplicate_found <<- TRUE
        # Exit loop after finding first duplicate.
        break
      }
      # Record ABI in seen set.
      assign(abi, TRUE, envir = refusa_abi_seen)
    }
    # Return invisibly from callback.
    invisible(NULL)
  }),
  # Set chunk size for throughput and memory balance.
  chunk_size = 250000,
  # Restrict import to ABI column only for uniqueness checks.
  col_types = readr::cols_only(
    # Read ABI as character.
    ABI = readr::col_character()
  ),
  # Disable progress bar for cleaner logs.
  progress = FALSE
)

# Confirm ABI has no missing values.
stopifnot(refusa_abi_missing_n == 0L)

# Confirm ABI uniquely identifies observations.
stopifnot(!refusa_abi_duplicate_found)

# ------------------------------------------------------------------------------
# Build firm-level matching keys from long_survey firm_clean values
# ------------------------------------------------------------------------------

# Create one-row-per-firm dataset from deduplicated long_survey firms.
target_firms <- long_survey_unique_firms %>%
  # Keep only the firm name for crosswalk construction.
  dplyr::transmute(firm = firm_clean)

# Create exact uppercase match keys from firm names.
target_firms <- target_firms %>%
  # Build exact match key as uppercase trimmed firm name.
  dplyr::mutate(firm_key_exact = toupper(trimws(firm)))

# Define a regex pattern for trailing corporate suffixes.
suffix_pattern <- "(\\s+(INC|INCORPORATED|CO|COMPANY|CORP|CORPORATION|LLC|LTD|PLC|HOLDINGS|HOLDING|GROUP|COS|THE))+$"

# Start normalized key from exact key.
target_firms <- target_firms %>%
  # Initialize normalized key from exact key.
  dplyr::mutate(firm_key_norm = firm_key_exact)

# Replace ampersands in normalized keys.
target_firms$firm_key_norm <- gsub("&", " AND ", target_firms$firm_key_norm, fixed = TRUE)

# Remove non-alphanumeric characters except spaces from normalized keys.
target_firms$firm_key_norm <- gsub("[^A-Z0-9 ]+", " ", target_firms$firm_key_norm)

# Collapse repeated spaces in normalized keys.
target_firms$firm_key_norm <- gsub("\\s+", " ", target_firms$firm_key_norm)

# Remove trailing corporate suffix tokens from normalized keys.
target_firms$firm_key_norm <- gsub(suffix_pattern, "", target_firms$firm_key_norm)

# Trim normalized keys after suffix removal.
target_firms$firm_key_norm <- trimws(target_firms$firm_key_norm)

# Create compact keys by removing all non-alphanumeric characters.
target_firms$firm_key_compact <- gsub("[^A-Z0-9]+", "", target_firms$firm_key_exact)

# ------------------------------------------------------------------------------
# Validate key uniqueness and construct lookup vectors
# ------------------------------------------------------------------------------

# Check for exact-key collisions across different firms.
exact_collision_n <- target_firms %>%
  # Count distinct firms per exact key.
  dplyr::group_by(firm_key_exact) %>%
  # Compute number of firms sharing each exact key.
  dplyr::summarise(n_firms = dplyr::n_distinct(firm), .groups = "drop") %>%
  # Keep only collided keys.
  dplyr::filter(n_firms > 1) %>%
  # Count collided keys.
  nrow()

# Stop if any exact-key collisions exist.
if (exact_collision_n > 0) {
  # Abort because key collisions make matching ambiguous.
  stop(sprintf("Found %s exact-key collisions across firms", exact_collision_n))
}

# Check for normalized-key collisions across different firms.
norm_collision_n <- target_firms %>%
  # Count distinct firms per normalized key.
  dplyr::group_by(firm_key_norm) %>%
  # Compute number of firms sharing each normalized key.
  dplyr::summarise(n_firms = dplyr::n_distinct(firm), .groups = "drop") %>%
  # Keep only collided keys.
  dplyr::filter(n_firms > 1) %>%
  # Count collided keys.
  nrow()

# Stop if any normalized-key collisions exist.
if (norm_collision_n > 0) {
  # Abort because normalized collisions make fallback matching ambiguous.
  stop(sprintf("Found %s normalized-key collisions across firms", norm_collision_n))
}

# Check for compact-key collisions across different firms.
compact_collision_n <- target_firms %>%
  # Count distinct firms per compact key.
  dplyr::group_by(firm_key_compact) %>%
  # Compute number of firms sharing each compact key.
  dplyr::summarise(n_firms = dplyr::n_distinct(firm), .groups = "drop") %>%
  # Keep only collided keys.
  dplyr::filter(n_firms > 1) %>%
  # Count collided keys.
  nrow()

# Stop if any compact-key collisions exist.
if (compact_collision_n > 0) {
  # Abort because compact collisions make fallback matching ambiguous.
  stop(sprintf("Found %s compact-key collisions across firms", compact_collision_n))
}

# Build exact lookup vector from exact key to firm.
exact_lookup <- target_firms$firm

# Assign exact lookup names as exact keys.
names(exact_lookup) <- target_firms$firm_key_exact

# Build normalized lookup vector from normalized key to firm.
norm_lookup <- target_firms$firm

# Assign normalized lookup names as normalized keys.
names(norm_lookup) <- target_firms$firm_key_norm

# Build compact lookup vector from compact key to firm.
compact_lookup <- target_firms$firm

# Assign compact lookup names as compact keys.
names(compact_lookup) <- target_firms$firm_key_compact

# Initialize SIC count matrix with one row per firm and columns 1..99.
sic_counts <- matrix(0L, nrow = nrow(target_firms), ncol = 99)

# Label SIC count matrix rows by firm name.
rownames(sic_counts) <- target_firms$firm

# Build fast row-index lookup from firm to matrix row index.
row_index <- seq_len(nrow(target_firms))

# Name row-index lookup by firm names.
names(row_index) <- target_firms$firm

# ------------------------------------------------------------------------------
# Scan RefUSA in chunks and accumulate firm-by-SIC counts
# ------------------------------------------------------------------------------

# Define chunk callback that updates SIC counts by matched firm.
callback <- readr::DataFrameCallback$new(function(chunk, pos) {
  # Build uppercase exact keys from RefUSA company names.
  company_exact <- toupper(trimws(chunk$Company))

  # Extract digits from RefUSA primary SIC codes.
  sic_digits <- gsub("\\D", "", chunk[["Primary SIC Code"]])

  # Parse two-digit SIC from extracted digits.
  sic_two <- suppressWarnings(as.integer(substr(sic_digits, 1, 2)))

  # Mark invalid rows where company key or SIC code is unusable.
  valid <- !is.na(company_exact) & company_exact != "" & !is.na(sic_two) & sic_two >= 1 & sic_two <= 99

  # Return immediately when no valid rows are present in this chunk.
  if (!any(valid)) return(invisible(NULL))

  # Keep only valid company keys.
  company_exact <- company_exact[valid]

  # Keep only valid SIC codes.
  sic_two <- sic_two[valid]

  # Attempt exact matching from company key to firm.
  matched_firm <- unname(exact_lookup[company_exact])

  # Identify rows still unmatched after exact matching.
  miss_norm <- is.na(matched_firm)

  # Apply normalized-key matching to rows still unmatched.
  if (any(miss_norm)) {
    # Copy unmatched company keys for normalization.
    company_norm <- company_exact[miss_norm]
    # Replace ampersands in unmatched keys.
    company_norm <- gsub("&", " AND ", company_norm, fixed = TRUE)
    # Remove non-alphanumeric characters except spaces.
    company_norm <- gsub("[^A-Z0-9 ]+", " ", company_norm)
    # Collapse repeated spaces.
    company_norm <- gsub("\\s+", " ", company_norm)
    # Remove trailing corporate suffix tokens.
    company_norm <- gsub(suffix_pattern, "", company_norm)
    # Trim normalized keys.
    company_norm <- trimws(company_norm)
    # Fill matches using normalized lookup.
    matched_firm[miss_norm] <- unname(norm_lookup[company_norm])
  }

  # Identify rows still unmatched after normalized matching.
  miss_compact <- is.na(matched_firm)

  # Apply compact-key matching to rows still unmatched.
  if (any(miss_compact)) {
    # Build compact keys for still-unmatched rows.
    company_compact <- gsub("[^A-Z0-9]+", "", company_exact[miss_compact])
    # Fill matches using compact lookup.
    matched_firm[miss_compact] <- unname(compact_lookup[company_compact])
  }

  # Identify rows that successfully matched to a firm.
  keep <- !is.na(matched_firm)

  # Return immediately when no rows matched in this chunk.
  if (!any(keep)) return(invisible(NULL))

  # Convert matched firm names into SIC matrix row indices.
  row_ids <- row_index[matched_firm[keep]]

  # Keep SIC column indices for matched rows.
  col_ids <- sic_two[keep]

  # Build row-by-SIC frequency table for this chunk.
  chunk_tab <- table(
    # Cast row ids as a fixed-level factor for full matrix alignment.
    factor(row_ids, levels = seq_len(nrow(target_firms))),
    # Cast SIC ids as a fixed-level factor from 1 to 99.
    factor(col_ids, levels = 1:99)
  )

  # Add chunk frequencies into the global SIC count matrix.
  sic_counts <<- sic_counts + unclass(chunk_tab)

  # Return invisibly from callback.
  invisible(NULL)
})

# Read RefUSA in chunks and update SIC counts through callback.
readr::read_csv_chunked(
  # Read RefUSA file path directly.
  file = "data/external/2019_Business_Academic_QCQ.txt.gz",
  # Use callback that updates SIC counts per chunk.
  callback = callback,
  # Set chunk size for throughput and memory balance.
  chunk_size = 250000,
  # Restrict read to required columns only.
  col_types = readr::cols_only(
    # Read Company column as character.
    Company = readr::col_character(),
    # Read Primary SIC Code column as character.
    `Primary SIC Code` = readr::col_character()
  ),
  # Disable progress bar for cleaner logs.
  progress = FALSE
)

# ------------------------------------------------------------------------------
# Construct final SIC variables from modal RefUSA assignments
# ------------------------------------------------------------------------------

# Compute modal SIC code per firm with deterministic tie-break to smallest SIC.
sic_code_two_digit_refusa <- apply(
  # Use SIC count matrix as input.
  sic_counts,
  # Apply function across rows (firms).
  1,
  # Select modal SIC per firm.
  function(v) {
    # Get maximum count within firm's SIC distribution.
    max_n <- max(v)
    # Return NA when a firm has no matched RefUSA rows.
    if (max_n == 0) return(NA_integer_)
    # Return first SIC index with maximum count (smallest SIC in ties).
    which(v == max_n)[1]
  }
)

# Aggregate SIC bins to match required AER-style bucket rules.
aer_sic_code_two_digit_refusa_aggregation <- dplyr::case_when(
  # Keep missing SIC values as missing.
  is.na(sic_code_two_digit_refusa) ~ NA_real_,
  # Bucket SIC 24-35 to 24.
  sic_code_two_digit_refusa >= 24 & sic_code_two_digit_refusa <= 35 ~ 24,
  # Bucket SIC 42-47 to 42.
  sic_code_two_digit_refusa >= 42 & sic_code_two_digit_refusa <= 47 ~ 42,
  # Bucket SIC 50-51 to 50.
  sic_code_two_digit_refusa >= 50 & sic_code_two_digit_refusa <= 51 ~ 50,
  # Bucket SIC 61-64 to 61.
  sic_code_two_digit_refusa >= 61 & sic_code_two_digit_refusa <= 64 ~ 61,
  # Bucket SIC 65-70 to 65.
  sic_code_two_digit_refusa >= 65 & sic_code_two_digit_refusa <= 70 ~ 65,
  # Bucket SIC 72-73 to 72.
  sic_code_two_digit_refusa >= 72 & sic_code_two_digit_refusa <= 73 ~ 72,
  # Bucket SIC 75-76 to 75.
  sic_code_two_digit_refusa >= 75 & sic_code_two_digit_refusa <= 76 ~ 75,
  # Bucket SIC 80-87 to 80.
  sic_code_two_digit_refusa >= 80 & sic_code_two_digit_refusa <= 87 ~ 80,
  # Keep all other SIC codes unchanged.
  TRUE ~ as.numeric(sic_code_two_digit_refusa)
)

# ------------------------------------------------------------------------------
# Assemble final crosswalk output and write industry_map.xlsx
# ------------------------------------------------------------------------------

# Build final industry_map output with firm and new SIC variable only.
industry_map <- target_firms %>%
  # Keep only final output columns.
  dplyr::transmute(
    # Keep firm identifier as firm name string.
    firm = firm,
    # Store raw modal two-digit SIC assignment from RefUSA.
    sic_code_two_digit_refusa = as.numeric(sic_code_two_digit_refusa),
    # Store AER-style aggregation of raw two-digit SIC assignment.
    aer_sic_code_two_digit_refusa_aggregation = as.numeric(aer_sic_code_two_digit_refusa_aggregation)
  )

# Write updated crosswalk to processed industry_map.xlsx.
writexl::write_xlsx(industry_map, "data/processed/industry_map.xlsx")

# Report output write path and match coverage.
message(sprintf(
  "Wrote updated industry map to data/processed/industry_map.xlsx with %s/%s matched firms.",
  sum(!is.na(industry_map$sic_code_two_digit_refusa)),
  nrow(industry_map)
))
