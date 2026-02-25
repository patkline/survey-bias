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

# Import long_survey.csv
long_survey <- read.csv(file.path(processed, "long_survey.csv"))

# Confirm ResponseId and option_number uniquely identifies observations i.e., unique values of response id x unique values of option number = total rows
stopifnot(nrow(long_survey) == dplyr::n_distinct(long_survey$ResponseId, long_survey$option_number))

# Keep necessary variables i.e., firm_clean variable 
long_survey <- long_survey %>% dplyr::select(firm_clean)

# Trim leading and trailing whitespace from firm_clean values
long_survey$firm_clean <- trimws(long_survey$firm_clean)

# Confirm firm_clean is never missing i.e., confirm that all rows have non-NA and non-blank firm_clean values
stopifnot(all(!is.na(long_survey$firm_clean) & long_survey$firm_clean != ""))

# Keep one observation per unique firm_clean value to recover list of unique firms in our data, and keep only the firm_clean variable 
long_survey <- long_survey %>% dplyr::distinct(firm_clean)

# Should be 165 unique firms uniquely identified off firm_clean 
stopifnot(nrow(long_survey) == 165L | n_distinct(long_survey$firm_clean) == 165L)

# ------------------------------------------------------------------------------
# Import and assess structure of RefUSA data interactively
# ------------------------------------------------------------------------------
# Read in first 10000 rows of RefUSA to browse interactively 
refusa_first_10000_rows <- readr::read_csv(  
# Filepath
file = file.path(external, "2019_Business_Academic_QCQ.txt.gz"),

# Number of rows
n_max = 10000,

# Suprress column type printout
show_col_types = FALSE
)

# View sic variables 
#XXI think we want to be using "Primary SIC Code" for our matching, but should confirm with the data documentation 
#View(refusa_first_10000_rows %>% select(matches("(?i)sic")))

# ------------------------------------------------------------------------------
# Confirm that ABI variable is non-missing and uniquely identifies 
# rows in RefUSA
#
# 1. Load in RefUSA in chunks, and for each chunk, 
#   i. Check whether there are NA or blank values in the ABI 
#     column, and throw an error if so
#   ii. Check whether there are within-chunk duplicate values in 
#     the ABI column, and throw an error if so 
#   iii. Check whether there are across-chunk duplicate values in 
#     the ABI column by comparing to a global set of seen ABI values, 
#       and throw an error if so
#        b. if not, add ABI values from this chunk to a global set of seen ABI values for cross-chunk duplicate checks #        and proceed 
# ------------------------------------------------------------------------------
# XXCommenting out fow now --- uncomment out at end of file construction
if (FALSE) {
# Initialize hashed storage environment/dictionary that adds seen ABI values as keys
refusa_abi_seen <- new.env(hash = TRUE, parent = emptyenv())

# Read RefUSA ABI column in chunks for uniqueness assertion
readr::read_csv_chunked(
# Filepath for the RefUSA gzipped text file
file = file.path(external, "2019_Business_Academic_QCQ.txt.gz"),

# Use callback that enforces within-chunk and across-chunk uniqueness of ABI values 
callback = readr::DataFrameCallback$new(function(chunk, pos) {
  # Trim leading and trailing whitespace in ABI values in current chunk
  chunk$ABI <- trimws(chunk$ABI)

  # Fail immediately if any ABI in this chunk is missing (NA) or blank
  if (any(is.na(chunk$ABI) | chunk$ABI == "")) {
    stop(sprintf("🪦 Found missing ABI values in chunk at position %s", pos))
  }

  # Fail immediately if there are within-chunk duplicate ABI values
  if (any(duplicated(chunk$ABI))) {
    stop(sprintf("🪦 Found duplicate ABI values within chunk at position %s", pos))
  }

  # For each ABI value in this chunk, check whether it already exists in the global seen set and store results in a logical vector
  exists_vec <- vapply(chunk$ABI, exists, logical(1), envir = refusa_abi_seen, inherits = FALSE)
  
  # Check whether any of these ABI values are already present in the global seen set by seeing if any values in the logical vector are TRUE
  if (any(exists_vec)) {
    stop(sprintf("🪦 Found duplicate ABI across chunks at position %s", pos))
  }

  # Add the unique ABI keys from this chunk to the global seen set
  for (k in chunk$ABI) assign(k, TRUE, envir = refusa_abi_seen)

  # Return invisibly from callback
  invisible(NULL)
}),

# Set chunk size for throughput and memory balance
chunk_size = 250000,

# Restrict import to ABI column only for uniqueness checks
col_types = readr::cols_only(
  # Read ABI as character to preserve formatting
  ABI = readr::col_character()
),

# Disable progress bar for cleaner logs
progress = FALSE
)
}
# ------------------------------------------------------------------------------
# Build firm-level matching keys from long_survey firm_clean values
# ------------------------------------------------------------------------------
# Build exact match key, starting with making firm names uppercase
long_survey <- long_survey %>% dplyr::mutate(firm_clean_normalized = toupper(firm_clean))

# Replace ampersands in normalized keys i.e., treat & as AND token
long_survey$firm_clean_normalized <- gsub("&", " AND ", long_survey$firm_clean_normalized, fixed = TRUE)

# Remove non-alphanumeric characters except spaces from normalized keys
long_survey$firm_clean_normalized <- gsub("[^A-Z0-9 ]+", " ", long_survey$firm_clean_normalized)

# Collapse repeated spaces to one space in normalized keys
long_survey$firm_clean_normalized <- gsub("\\s+", " ", long_survey$firm_clean_normalized)

# Define a regex pattern for trailing corporate suffixes
suffix_pattern <- "(\\s+(INC|INCORPORATED|CO|COMPANY|CORP|CORPORATION|LLC|LTD|PLC|HOLDINGS|HOLDING|GROUP|COS|THE))+$$" 

# Remove trailing corporate suffix tokens from normalized keys.
long_survey$firm_clean_normalized <- gsub(suffix_pattern, "", long_survey$firm_clean_normalized)

# Trim leading and trailing whitespace in normalized keys after suffix removal
long_survey$firm_clean_normalized <- trimws(long_survey$firm_clean_normalized)

# Create compact keys by removing all whitespace from the normalized key
long_survey$firm_clean_normalized_compact <- gsub(" ", "", long_survey$firm_clean_normalized, fixed = TRUE)

# Assert that normalized and compact keys are (i) non-missing and non-blank and (ii) unique
for (variable in c("firm_clean_normalized", "firm_clean_normalized_compact")) {
# Assert non-missing and non-blank values in given variable 
stopifnot(all(!is.na(long_survey[[variable]]) & long_survey[[variable]] != ""))

# Assert unique values in given variable
stopifnot(nrow(long_survey) == n_distinct(long_survey[[variable]]))
}

XXstopped here 
# ------------------------------------------------------------------------------
# Validate key uniqueness and construct lookup vectors
# ------------------------------------------------------------------------------

# Check for exact-key collisions across different firms.
exact_collision_n <- long_survey %>%
  # Count distinct firms per exact key.
  dplyr::group_by(firm_clean_uppercase) %>%
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
  dplyr::group_by(firm_clean_normalized) %>%
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
  dplyr::group_by(firm_clean_normalized_compact) %>%
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
names(exact_lookup) <- target_firms$firm_clean_uppercase

# Build normalized lookup vector from normalized key to firm.
norm_lookup <- target_firms$firm

# Assign normalized lookup names as normalized keys.
names(norm_lookup) <- target_firms$firm_clean_normalized

# Build compact lookup vector from compact key to firm.
compact_lookup <- target_firms$firm

# Assign compact lookup names as compact keys.
names(compact_lookup) <- target_firms$firm_clean_normalized_compact

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

e
# Write updated crosswalk to processed industry_map.xlsx.
writexl::write_xlsx(industry_map, "data/processed/industry_map.xlsx")

# Report output write path and match coverage.
message(sprintf(
  "Wrote updated industry map to data/processed/industry_map.xlsx with %s/%s matched firms.",
  sum(!is.na(industry_map$sic_code_two_digit_refusa)),
  nrow(industry_map)
))
