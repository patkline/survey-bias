source("code/globals.R")

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pretty 3-decimal formatter
fmt3 <- function(x) ifelse(is.na(x), "NA", sprintf("%.3f", as.numeric(x)))

# Read one coef/se pair and (optionally) divide both by 100 for reporting
# Fix: .data pronoun only works inside dplyr verbs. Use base indexing inside pull_est.

pull_est <- function(root, file, lhs_var, rhs_var, coef_num,
                     sheet = "EIV_BS", divide_by_100 = FALSE) {
  path <- file.path(root, file)
  dat <- tryCatch(readxl::read_xlsx(path, sheet = sheet),
                  error = function(e) tibble::tibble())
  if (!nrow(dat)) return("NA (NA)")

  # coerce columns safely
  if (!("coef" %in% names(dat))) dat$coef <- NA_real_
  dat$coef <- suppressWarnings(as.numeric(dat$coef))

  # guard missing columns
  need <- c("rhs","lhs","coef","sample_est","sample_se")
  if (!all(need %in% names(dat))) return("NA (NA)")

  out <- dat[dat$rhs == rhs_var & dat$lhs == lhs_var & dat$coef == coef_num, , drop = FALSE]
  if (!nrow(out)) return("NA (NA)")

  est <- suppressWarnings(as.numeric(out$sample_est[1]))
  se  <- suppressWarnings(as.numeric(out$sample_se[1]))

  if (isTRUE(divide_by_100)) {
    est <- est / 100
    se  <- se  / 100
  }
  paste0(fmt3(est), " (", fmt3(se), ")")
}


# Default sample
default_filemap <- tibble(
  Sample = c("Full Sample",
             "Black",
             "White",
             "Female",
             "Male",
             "Looking for a Job",
             "Not Looking for a Job",
             "Feared Discrimination",
             "Did Not Fear Discrimination"),
  file  = c("Plackett_Luce_Full_Sample.xlsx",
            "Plackett_Luce_Subset_Black.xlsx",
            "Plackett_Luce_Subset_White.xlsx",
            "Plackett_Luce_Subset_Female.xlsx",
            "Plackett_Luce_Subset_Male.xlsx",
            "Plackett_Luce_Subset_Looking.xlsx",
            "Plackett_Luce_Subset_Not_Looking.xlsx",
            "Plackett_Luce_Subset_Feared_Discrimination_1.xlsx",
            "Plackett_Luce_Subset_Feared_Discrimination_0.xlsx")
)

lhs_var  <- "cb_central_full"
rhs_var  <- "discretion"
coef_num <- 1L

# "0.123 (0.045)" -> \makecell{0.123 \\ (0.045)}
cell_from_pair <- function(x) {
  if (is.na(x) || x == "NA (NA)") return("\\makecell{NA \\\\ (NA)}")
  m <- str_match(x, "^\\s*(.+?)\\s*\\((.+?)\\)\\s*$")
  if (any(is.na(m))) return(paste0("\\makecell{", x, "}"))
  paste0("\\makecell{", m[2], " \\\\ (", m[3], ")}")
}

# stack long headers
stack_header <- function(s) {
  if (s == "Looking for a Job")        return("\\makecell{Looking for\\\\a Job}")
  if (s == "Not Looking for a Job")    return("\\makecell{Not Looking\\\\for a Job}")
  if (s == "Feared Discrimination")    return("\\makecell{Feared\\\\Discrim.}")
  if (s == "Did Not Fear Discrimination") return("\\makecell{Did Not Fear\\\\Discrim.}")
  s
}

cells <- default_filemap %>%
  mutate(pair = vapply(file, function(f)
    pull_est(root = excel, file = f,
             lhs_var = lhs_var, rhs_var = rhs_var, coef_num = coef_num,
             sheet = "EIV_BS", divide_by_100 = FALSE),
    character(1)
  )) %>%
  mutate(tex_cell = vapply(pair, cell_from_pair, character(1)))

col_headers <- vapply(default_filemap$Sample, stack_header, character(1))

tab_lines <- c(
  paste0(
    "\\begin{tabular}{l",
    paste(rep("c", nrow(default_filemap)), collapse = ""),
    "}"
  ),
  "\\hline",
  paste(c("", col_headers), collapse = " & "), " \\\\",
  "\\hline",
  paste(c("", cells$tex_cell), collapse = " & "), " \\\\",
  "\\hline",
  "\\end{tabular}"
)


writeLines(tab_lines, file.path(tables, "EIV_central_discretion.tex"))

