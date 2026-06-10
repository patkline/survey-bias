# ------------------------------------------------------------------------------------------
# Purpose: Generate coefficient plots for EIV results 
# Created: Nico Rotundo 2026-06-08
# ------------------------------------------------------------------------------------------
# Run globals
source("code/globals.R")

# ------------------------------------------------------------------------------------------
# Append {OLS, Borda} x {univariate} x lhs = {log_dif (race), log_dif_gender (gender)} x rhs = {pooled_favor_white, pooled_favor_male} x {no industry fe, industry fe} rows together for each subgroup into one dataframe
# ------------------------------------------------------------------------------------------
# Define empty list to store dataframes for each subgroup 
eiv_dataframe <- list() 

# Loop over subgroups
for (microdata_sample in c("Full_Sample", 
    "Subset_Black", "Subset_White",
    "Subset_Female", "Subset_Male",
    "Subset_Looking", "Subset_Not_Looking",
    "Subset_Feared_Discrimination_1", "Subset_Feared_Discrimination_0",
    "Subset_Age_gte40", "Subset_Age_lt40")) {

    # Import EIV coefficients and standard errors for given subgroup 
    eiv_temp_dataframe <- read_parquet_sheet(file.path(intermediate, microdata_sample), "EIV_firm")

    # Check unique identifers 
    stopifnot(!anyDuplicated(eiv_temp_dataframe[c("model", "lhs", "formula", "rhs", "coef")]), !anyNA(eiv_temp_dataframe[c("model", "lhs", "formula", "rhs", "coef")]))

    # Generate new variable for whether industry FE are included or not based on value of "coef"
    eiv_temp_dataframe$industry_fe_indicator <- c("1" = "no", "2" = "yes")[as.character(eiv_temp_dataframe$coef)]

    # Check that indicator is never missing 
    stopifnot(!anyNA(eiv_temp_dataframe$industry_fe_indicator))

    # Generate new variable for whether regression is univariate or multivariate based on whether formula == rhs
    eiv_temp_dataframe$univariate_indicator <- ifelse(eiv_temp_dataframe$formula == eiv_temp_dataframe$rhs, "yes", "no")

    # Check that indicator is never missing 
    stopifnot(!anyNA(eiv_temp_dataframe$univariate_indicator))

    # Keep necessary observations 
    eiv_temp_dataframe <- eiv_temp_dataframe |>
    dplyr::filter(
        # OLS and Borda aggregation methods
        model %in% c("OLS", "Borda"),
        
        # Univariate regression specs
        univariate_indicator %in% c("yes"),

        # LHS = contact rate gaps from the 97-firm audit experiment 
        lhs %in% c("log_dif", "log_dif_gender"),

        # RHS = pooled belief measures
        rhs %in% c("pooled_favor_white", "pooled_favor_male")
    )

    # Confirm formula is equivalent to rhs for all remaining observations
    stopifnot(all(eiv_temp_dataframe$formula == eiv_temp_dataframe$rhs))

    # Keep necessary columns 
    eiv_temp_dataframe <- eiv_temp_dataframe |>
    dplyr::select(model, univariate_indicator, lhs, rhs,
                  industry_fe_indicator, n, sample_est, sample_se)

    # Generate column for the sample
    eiv_temp_dataframe <- eiv_temp_dataframe |>
    dplyr::mutate(microdata_sample = .env$microdata_sample)

    # Order variables 
    eiv_temp_dataframe <- eiv_temp_dataframe |>
    dplyr::relocate(microdata_sample, model, univariate_indicator, lhs, rhs,
                    industry_fe_indicator, n, sample_est, sample_se)

    # Add to eiv_dataframe list
    eiv_dataframe[[microdata_sample]] <- eiv_temp_dataframe

    # Drop eiv_temp_dataframe 
    rm(eiv_temp_dataframe)
}

# Append rows of eiv_dataframe list into one dataframe
eiv_dataframe <- dplyr::bind_rows(eiv_dataframe)

# Check unique identifiers 
stopifnot(!anyDuplicated(eiv_dataframe[c("microdata_sample", "model", "lhs", "rhs", "industry_fe_indicator")]), !anyNA(eiv_dataframe[c("microdata_sample", "model", "lhs", "rhs", "industry_fe_indicator")]))

# Assert 88 rows for 11 subsamples x 2 models x 2 {lhs vars x rhs} pairs x 2 industry fe indicators 
stopifnot(nrow(eiv_dataframe) == 88)

# ------------------------------------------------------------------------------------------
# For a given model x {lhs, rhs} pair, generate coefficient plot 
# ------------------------------------------------------------------------------------------
# Loop over models
for (model in c("OLS", "Borda")) {
    
    # Loop over {lhs, rhs} pairs
    for (lhs_var in c("log_dif", "log_dif_gender")) {
        
        # Set locals based on lhs_rhs_pair 
        if (lhs_var == "log_dif") {
            # File name suffix 
            lhs_rhs_pair <- "lhs_log_dif_rhs_pooled_favor_white"
            
            # Label for x-axis on plot 
            gap_label <- "white-Black"
        
        } else if (lhs_var == "log_dif_gender") {
            # File name suffix
            lhs_rhs_pair <- "lhs_log_dif_gender_rhs_pooled_favor_male"
            
            # Label for x-axis on plot
            gap_label <- "male-female"
        
        } else {
            stop("🪦 Unexpected lhs_var value")
        }

        # Restrict to given model and {lhs, rhs} pair
        eiv_coefplot_dataframe <- eiv_dataframe |>
        dplyr::filter(model == .env$model, lhs == .env$lhs_var)

        # Generate confidence interval bounds 
        eiv_coefplot_dataframe <- eiv_coefplot_dataframe |>
        dplyr::mutate(ci_lower_bound = sample_est - 1.96 * sample_se, ci_upper_bound = sample_est + 1.96 * sample_se)

        # Convert microdata_sample to factor variable 
        eiv_coefplot_dataframe <- eiv_coefplot_dataframe |>
        dplyr::mutate(microdata_sample = factor(microdata_sample, levels = rev(unique(microdata_sample))))

        # Generate labels for industry FE pointws 
        eiv_coefplot_dataframe <- eiv_coefplot_dataframe |>
        dplyr::mutate(
            fe_label = factor(industry_fe_indicator, levels = c("yes", "no"),
                            labels = c("Industry FE", "No Controls"))
        )

        # Generate coefplot 
        eiv_coefplot <- ggplot(eiv_coefplot_dataframe, aes(x = sample_est, y = microdata_sample, color = fe_label, shape = fe_label)) +

            # Vertical dashed reference line at x = 0 
            geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +

            # Point estimates with error bars for confidence intervals, offset on y-axis by industry FE indicator
            geom_pointrange(aes(xmin = ci_lower_bound, xmax = ci_upper_bound), position = position_dodge(width = 0.6), size = 0.5) +

            # Set colors 
            scale_color_manual(values = c("No Controls" = "steelblue", "Industry FE" = "darkorange"), breaks = c("No Controls", "Industry FE")) +

            # Set shapes 
            scale_shape_manual(values = c("No Controls" = 16, "Industry FE" = 17), breaks = c("No Controls", "Industry FE")) +

            # Set y-axis tick labels 
            scale_y_discrete(labels = c(
                Full_Sample = "Full Sample",
                Subset_Black = "Black", Subset_White = "White",
                Subset_Female = "Female", Subset_Male = "Male",
                Subset_Looking = "Looking for a Job", Subset_Not_Looking = "Not Looking for a Job",
                Subset_Feared_Discrimination_1 = "Feared Discrimination",
                Subset_Feared_Discrimination_0 = "Did Not Fear Discrimination",
                Subset_Age_gte40 = "40 Years or Older", Subset_Age_lt40 = "Less than 40 Years Old")
            ) +

            # Set axis and legend labels 
            labs(x = paste0("EIV coefficient on log ", gap_label, " contact gap"), y = NULL, color = NULL, shape = NULL) +

            # Theme selection and font size 
            theme_minimal(base_size = 14) +

            # Theme adjustments 
            theme(
                # No grid lines 
                panel.grid = element_blank(),

                # Background 
                panel.background = element_rect(fill = "white", color = NA),

                # Add bottom and left axes spines 
                axis.line = element_line(color = "black"),                    

                # Legend coordinates inside plot 
                legend.position = c(0.82, 0.12),

                # One column legend
                legend.direction = "vertical",

                # No background for plot 
                plot.background = element_rect(fill = "white", color = NA),

                # No box behind legend 
                legend.background = element_blank(),

                # Legend key with no background
                legend.key = element_blank()
            )

        # Export 
        ggsave(
            file.path(figures, paste0("eiv_coefplot_", tolower(model), "_", lhs_rhs_pair, ".png")),
            plot = eiv_coefplot, width = 9, height = 6, dpi = 300, bg = "transparent"
        ) 

        # Print plot 
        print(eiv_coefplot)

        # Drop coefplot and dataframe 
        rm(eiv_coefplot, eiv_coefplot_dataframe)

    }
}