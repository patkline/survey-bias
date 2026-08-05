# -------------------------------------------------------------------
# Purpose: Bar graphs of debiased cross-valence correlations
#          Two graphs per model (race & gender), 3 bars each
#          Uses the pairwise multivariate-Katz signal correlation.
# -------------------------------------------------------------------
source("code/globals.R")
source(file.path(analysis, "katz_correct.R"))
source(file.path(analysis, "eiv_functions.R"))
source(file.path(analysis, "correlation_function.R"))

library(dplyr)
library(ggplot2)

# --- Config -----------------------------------------------------------
full_sample_dir <- file.path(intermediate, "Full_Sample")
subset_filter <- "all"
model_title_map <- c(OLS = "Likert", Borda = "Borda", OLSC = "Likert Centered")

# Pairs: list of (outcome_a, outcome_b, bar_label)
race_pairs <- list(
  list(a = "FirmCont_black",  b = "FirmCont_white",  label = "Names: Contact"),
  list(a = "FirmHire_black",  b = "FirmHire_white",  label = "Names: Hiring"),
  list(a = "conduct_black",   b = "conduct_white",   label = "Conduct")
)

gender_pairs <- list(
  list(a = "FirmCont_male",  b = "FirmCont_female",  label = "Names: Contact"),
  list(a = "FirmHire_male",  b = "FirmHire_female",  label = "Names: Hiring"),
  list(a = "conduct_male",   b = "conduct_female",   label = "Conduct")
)

# --- Build multivariate-Katz correlations for the displayed pairs -----
pair_key <- function(lhs, rhs) paste(pmin(lhs, rhs), pmax(lhs, rhs), sep = "||")
displayed_pairs <- c(race_pairs, gender_pairs)
displayed_pair_keys <- vapply(
  displayed_pairs,
  function(pair) pair_key(pair$a, pair$b),
  character(1)
)

variance_input <- read_parquet_sheet(full_sample_dir, "variance")
covariance_input <- read_parquet_sheet(full_sample_dir, "covariance") %>%
  dplyr::filter(
    subset == subset_filter,
    model %in% c("OLS", "Borda"),
    pair_key(lhs, rhs) %in% displayed_pair_keys
  )
stopifnot(nrow(covariance_input) == length(displayed_pair_keys) * 2)

corr_df <- build_correlation_from_varcov(
  var_df = variance_input,
  cov_df = covariance_input,
  use_multivariate_katz = TRUE
)
stopifnot(all(dplyr::between(corr_df$corr_c, -1, 1)))

models <- intersect(c("OLS", "Borda"), unique(as.character(corr_df$model)))
if (length(models) == 0L) {
  stop("No OLS/Borda rows found in correlation sheet: ", full_sample_dir)
}

# --- Helper: look up corr_c for a pair -------------------------------
lookup_corr_c <- function(corr_df, model_name, outcome_a, outcome_b) {
  row <- corr_df %>%
    dplyr::filter(
      model == model_name,
      (lhs == outcome_a & rhs == outcome_b) |
        (lhs == outcome_b & rhs == outcome_a)
    )
  if (nrow(row) == 0) return(NA_real_)
  row$corr_c[1]
}

# --- Build results data frame -----------------------------------------
results <- list()

for (m in models) {
  for (pair_set in list(list(pairs = race_pairs, dim = "Race"),
                        list(pairs = gender_pairs, dim = "Gender"))) {
    corrs <- sapply(pair_set$pairs, function(p) {
      lookup_corr_c(corr_df, m, p$a, p$b)
    })

    results[[length(results) + 1]] <- data.frame(
      model       = m,
      dimension   = pair_set$dim,
      bar_label   = sapply(pair_set$pairs, `[[`, "label"),
      correlation = corrs,
      stringsAsFactors = FALSE
    )
  }
}

plot_df <- bind_rows(results)

# Fix bar order
plot_df$bar_label <- factor(plot_df$bar_label,
                            levels = c("Names: Contact", "Names: Hiring", "Conduct"))

# --- Plot function ----------------------------------------------------
make_bar_plot <- function(df_sub, model_name, dimension) {
  ggplot(df_sub, aes(x = bar_label, y = correlation)) +
    geom_col(fill = "steelblue", width = 0.6) +
    geom_text(aes(label = sprintf("%.3f", correlation)),
              vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(min(0, min(df_sub$correlation, na.rm = TRUE) - 0.1),
                                  max(1, max(df_sub$correlation, na.rm = TRUE) + 0.1))) +
    labs(title = paste0(model_name, " \u2014 ", dimension,
                        " Cross-Valence Debiased Correlations"),
         x = NULL,
         y = "Debiased Correlation") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5))
}

# --- Generate and save plots -----------------------------------------
for (m in models) {
  for (dim in c("Race", "Gender")) {
    df_sub <- plot_df %>%
      dplyr::filter(model == m, dimension == dim)

    if (all(is.na(df_sub$correlation))) {
      warning("All corr_c values are NA for model=",
              m, ", dimension=", dim,
              " -- plot will be empty.")
    }

    m_title <- unname(model_title_map[m])
    if (is.na(m_title)) m_title <- m
    p <- make_bar_plot(df_sub, m_title, dim)

    fname <- paste0("valence_corr_", m, "_",
                     tolower(dim), ".png")
    ggsave(file.path(figures, fname), plot = p,
           width = 6, height = 4, dpi = 300)
    message("Saved: ", fname)
  }
}

message("All valence correlation bar graphs complete.")
