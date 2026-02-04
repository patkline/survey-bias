# -------------------------------------------------------------------
# Build plots/tables using precomputed sheets (no model fitting)
# Requires: readxl, dplyr, tidyr, ggplot2, xtable, scales, rlang
# Prev. name: 4a_summary_item_worths_v6.R
# -------------------------------------------------------------------
# Run globals
source("code/globals.R")

create_plots_and_tables_from_sheets <- function(excel_path,
                                                outcomes,
                                                plots_dir,
                                                tables_dir,
                                                label_mapping = NULL,
                                                make_overlay_plot = TRUE,
                                                n_keep_overlay = 25,
                                                add_lm_line = TRUE,
                                                add_45_line   = TRUE,
                                                ws_type = NULL,   # optional WS filter
                                                gap_width = 3,    # <-- # of blank slots between bottom/top groups
                                                scale_borda_to_100 = FALSE) {  # rescale Borda 0–1 -> 0–100
  
  # scale controls for Borda
  borda_mult   <- if (isTRUE(scale_borda_to_100)) 100 else 1
  borda_max    <- if (isTRUE(scale_borda_to_100)) 100 else 1
  borda_breaks <- if (borda_max == 100) seq(0, 100, 10) else seq(0, 1, 0.1)
  
  # read core sheets (must have columns: firm, firm_id, <outcomes...>)
  df_coef <- read_excel(excel_path, sheet = "Coefficients")
  df_eb   <- read_excel(excel_path, sheet = "Coefficients (EB)")
  df_eb2  <- tryCatch(read_excel(excel_path, sheet = "Coefficients (EB) V2"), error = function(e) NULL)
  df_avg  <- read_excel(excel_path, sheet = "Average Ratings")
  df_n    <- read_excel(excel_path, sheet = "Ratings Observations")
  # Borda sheets (wide: firm, firm_id, <outcomes...>)
  df_borda    <- tryCatch(read_excel(excel_path, sheet = "borda_score"),    error = function(e) NULL)
  df_borda_eb <- tryCatch(read_excel(excel_path, sheet = "borda_score_eb"), error = function(e) NULL)
  
  # Try combined Win_Share sheet (older style); filter by ws_type if present
  df_ws_combined <- tryCatch(read_excel(excel_path, sheet = "Win_Share"), error = function(e) NULL)
  if (!is.null(df_ws_combined) && !is.null(ws_type) && ("type" %in% names(df_ws_combined))) {
    df_ws_combined <- dplyr::filter(df_ws_combined, .data$type == ws_type)
  }
  
  # helper for unique file suffix by settings
  suffix <- if (!is.null(ws_type) && nzchar(ws_type)) paste0("_", ws_type) else ""
  
  # helper to pick EB key (prefer Two-Step)
  choose_pl_eb_key <- function(df_old, df_new) {
    if (!is.null(df_new) && "Estimate_eb_new" %in% names(df_new) &&
        !all(is.na(df_new$Estimate_eb_new))) {
      df_new %>% dplyr::select(firm, firm_id, eb_key = Estimate_eb_new)
    } else {
      df_old %>% dplyr::select(firm, firm_id, eb_key = Estimate_eb_old)
    }
  }
  
  # helper: pick expected-borda sheet name under Excel's 31-char cap
  pick_expected_sheet <- function(path, prefix, outcome, max_len = 31L) {
    sheets  <- readxl::excel_sheets(path)
    allowed <- max_len - nchar(prefix)
    if (allowed <= 0) return(NA_character_)
    full  <- paste0(prefix, outcome)
    trunc <- paste0(prefix, substr(outcome, 1, allowed))
    cand <- c(full, trunc)
    hit  <- cand[cand %in% sheets]
    if (length(hit) >= 1) return(hit[1])
    prefix_start <- substr(outcome, 1, max(3L, min(nchar(outcome), allowed)))
    esc <- function(x) gsub("([][(){}.^$|*+?\\\\])", "\\\\\\1", x)
    pattern <- paste0("^", esc(prefix), esc(prefix_start))
    fuzzy <- sheets[grepl(pattern, sheets)]
    if (length(fuzzy)) return(fuzzy[[which.max(nchar(fuzzy))]])
    NA_character_
  }
  
  # -------------------------------------------------------------------
  # Helper: 5-col PL vs Borda *standard deviation* table
  # - Uses label_mapping for the Outcome column
  # - Sorted alphabetically by displayed label
  # - Formats numbers with EXACTLY 3 decimals (padded)
  # - If scale_borda_to_100 = TRUE, scales Borda SD-like terms by 100 (not 100^2)
  # -------------------------------------------------------------------
  write_variance_biascorrected_table <- function(excel_path,
                                                 outcomes = NULL,
                                                 tables_dir,
                                                 label_mapping = NULL,
                                                 csv_name = "variance_biascorrected_pl_borda.csv",
                                                 tex_name = "variance_biascorrected_pl_borda.tex",
                                                 latex_decimals = 3,
                                                 borda_mult = 1) {
    ssn <- tryCatch(readxl::read_excel(excel_path, sheet = "sum_signal_noise"),
                    error = function(e) NULL)
    if (is.null(ssn)) {
      message("⚠️ Could not read sheet 'sum_signal_noise' from: ", excel_path)
      return(invisible(NULL))
    }
    
    need_cols <- c("outcome","variance","signal","Borda","PL")
    if (!all(need_cols %in% names(ssn))) {
      message("⚠️ 'sum_signal_noise' must contain columns: ",
              paste(need_cols, collapse = ", "))
      return(invisible(NULL))
    }
    
    # If all_firms column exists, keep only all_firms == TRUE
    if ("all_firms" %in% names(ssn)) {
      ssn <- ssn %>%
        dplyr::filter(all_firms %in% TRUE)
    }
    
    # normalize logicals
    to_logical <- function(x) {
      if (is.logical(x)) return(x)
      if (is.numeric(x)) return(x != 0)
      if (is.character(x)) return(toupper(trimws(x)) %in% "TRUE")
      rep(NA, length(x))
    }
    ssn$Borda <- to_logical(ssn$Borda)
    ssn$PL    <- to_logical(ssn$PL)
    
    if (!is.null(outcomes) && length(outcomes)) {
      ssn <- dplyr::filter(ssn, .data$outcome %in% outcomes)
    }
    
    # --- Build PL SDs + reliability (PL == TRUE rows only) ---
    pl_df <- ssn %>%
      dplyr::filter(.data$PL %in% TRUE)
    
    if ("reliability" %in% names(pl_df)) {
      # use reliability from the sheet if present
      pl_df <- pl_df %>%
        dplyr::transmute(
          outcome,
          PL_sd                   = sqrt(pmax(variance, 0)),
          PL_sd_bias_corrected    = sqrt(pmax(signal,   0)),
          PL_reliability          = as.numeric(reliability)
        )
    } else {
      # otherwise compute reliability from signal
      pl_df <- pl_df %>%
        dplyr::transmute(
          outcome,
          PL_sd                   = sqrt(pmax(variance, 0)),
          PL_sd_bias_corrected    = sqrt(pmax(signal,   0)),
          PL_reliability          = {
            sig <- signal
            rel <- ifelse(is.finite(sig), (sig / 1.64) / (1 + sig / 1.64), NA_real_)
            rel[!is.finite(rel)] <- NA_real_
            rel
          }
        )
    }
    
    # --- Build Borda SDs (Borda == TRUE rows only) ---
    borda_df <- ssn %>%
      dplyr::filter(.data$Borda %in% TRUE) %>%
      dplyr::transmute(
        outcome,
        Borda_sd                = sqrt(pmax(variance, 0)) * borda_mult,
        Borda_sd_bias_corrected = sqrt(pmax(signal,   0)) * borda_mult
      )
    
    # Merge; ensure requested outcomes present
    tab <- dplyr::full_join(pl_df, borda_df, by = "outcome")
    if (!is.null(outcomes) && length(outcomes)) {
      tab <- dplyr::full_join(
        data.frame(outcome = unique(outcomes), stringsAsFactors = FALSE),
        tab, by = "outcome"
      )
    }
    
    # Display label mapping
    map_label <- function(x) {
      if (!is.null(label_mapping)) {
        lbl <- unname(label_mapping[x])
        lbl[is.na(lbl)] <- x[is.na(lbl)]
        return(lbl)
      }
      x
    }
    tab$Outcome_display <- map_label(tab$outcome)
    
    # Sort by displayed label then raw id
    tab <- tab %>%
      dplyr::arrange(.data$Outcome_display, .data$outcome)
    
    # CSV (numeric, full precision)
    csv_out_path <- file.path(tables_dir, csv_name)
    out_csv <- tab %>%
      dplyr::transmute(
        Outcome = .data$Outcome_display,
        `PL: sd`                         = .data$PL_sd,
        `PL: bias corrected sd`          = .data$PL_sd_bias_corrected,
        `PL: reliability`                = .data$PL_reliability,
        `Borda: sd`                      = .data$Borda_sd,
        `Borda: bias corrected sd`       = .data$Borda_sd_bias_corrected
      )
    utils::write.csv(out_csv, csv_out_path, row.names = FALSE)
    
    # --- formatter: fixed decimals, padded zeros ---
    fmt_dec <- function(x, k = latex_decimals) {
      z <- suppressWarnings(as.numeric(x))
      out <- rep("", length(z))
      ok <- is.finite(z)
      out[ok] <- formatC(z[ok], format = "f", digits = k, drop0trailing = FALSE)
      out
    }
    
    # LaTeX table
    latex_df <- tab %>%
      dplyr::transmute(
        Outcome = .data$Outcome_display,
        `Standard deviation`                   = fmt_dec(.data$PL_sd,                   latex_decimals),
        `Bias-corrected standard deviation`    = fmt_dec(.data$PL_sd_bias_corrected,    latex_decimals),
        `Reliability`                          = fmt_dec(.data$PL_reliability,          latex_decimals),
        `Standard deviation.2`                 = fmt_dec(.data$Borda_sd,                latex_decimals),
        `Bias-corrected standard deviation.2`  = fmt_dec(.data$Borda_sd_bias_corrected, latex_decimals)
      )
    
    xt <- xtable::xtable(latex_df, align = c("l","l","c","c","c","c","c"))
    
    header <- paste0(
      "\\toprule\n",
      " & \\multicolumn{3}{c}{Plackett--Luce} & \\multicolumn{2}{c}{Borda} \\\\\n",
      "\\cmidrule(lr){2-4} \\cmidrule(lr){5-6}\n",
      "Outcome & Std Dev & ",
      "\\shortstack{Signal\\\\Std Dev} & ",
      "Reliability & ",
      "Std Dev & ",
      "\\shortstack{Signal\\\\Std Dev} \\\\\n",
      "\\midrule\n"
    )
    
    tex_out_path <- file.path(tables_dir, tex_name)
    print(
      xt,
      include.rownames = FALSE,
      include.colnames = FALSE,
      file = tex_out_path,
      add.to.row = list(pos = list(0), command = header),
      sanitize.text.function = identity,
      booktabs = TRUE,
      floating = FALSE,
      comment = FALSE
    )
    cat("✅ Variance / SD table with PL reliability (all_firms == TRUE only) saved:",
        basename(csv_out_path), "and", basename(tex_out_path), "\n")
    
    invisible(list(csv = csv_out_path, tex = tex_out_path, data = tab))
  }
  
  
  # Call remains the same:
  write_variance_biascorrected_table(
    excel_path    = excel_path,
    outcomes      = outcomes,
    tables_dir    = tables_dir,
    label_mapping = label_mapping,
    borda_mult    = borda_mult
  )
  
  for (new_outcome in outcomes) {
    # ---- pull per-outcome columns and unify names
    cols_needed <- c("firm", "firm_id", new_outcome)
    get_one <- function(df, nm, out_name) {
      stopifnot(all(cols_needed %in% names(df)))
      out <- df[, cols_needed]
      names(out) <- c("firm", "firm_id", out_name)
      out
    }
    
    coeff_df        <- get_one(df_coef, new_outcome, "Estimate")
    coeff_df_eb_old <- get_one(df_eb,   new_outcome, "Estimate_eb_old")
    if (!is.null(df_eb2) && all(cols_needed %in% names(df_eb2))) {
      coeff_df_eb_new <- get_one(df_eb2, new_outcome, "Estimate_eb_new")
    } else {
      coeff_df_eb_new <- coeff_df %>%
        transmute(firm, firm_id, Estimate_eb_new = NA_real_)
    }
    avg_df          <- get_one(df_avg,  new_outcome, "avg_rating")
    nobs_df         <- get_one(df_n,    new_outcome, "num_observations")
    
    # merge the core metrics
    merged_df <- coeff_df %>%
      inner_join(coeff_df_eb_old, by = c("firm","firm_id")) %>%
      left_join(coeff_df_eb_new,  by = c("firm","firm_id")) %>%
      inner_join(avg_df,          by = c("firm","firm_id")) %>%
      inner_join(nobs_df,         by = c("firm","firm_id"))
    
    # ---- Borda columns for this outcome ----
    borda_cols_needed <- c("firm", "firm_id", new_outcome)
    if (!is.null(df_borda) && all(borda_cols_needed %in% names(df_borda))) {
      borda_df <- df_borda[, borda_cols_needed]
      names(borda_df) <- c("firm","firm_id","Borda")
      borda_df$Borda <- as.numeric(borda_df$Borda) * borda_mult  # apply scaling
    } else {
      borda_df <- merged_df %>% transmute(firm, firm_id, Borda = NA_real_)
      message("⚠️ Sheet 'borda_score' missing or outcome column absent for ", new_outcome)
    }
    if (!is.null(df_borda_eb) && all(borda_cols_needed %in% names(df_borda_eb))) {
      borda_eb_df <- df_borda_eb[, borda_cols_needed]
      names(borda_eb_df) <- c("firm","firm_id","Borda_EB")
      borda_eb_df$Borda_EB <- as.numeric(borda_eb_df$Borda_EB) * borda_mult  # apply scaling
    } else {
      borda_eb_df <- merged_df %>% transmute(firm, firm_id, Borda_EB = NA_real_)
      message("⚠️ Sheet 'borda_score_eb' missing or outcome column absent for ", new_outcome)
    }
    
    if (!"std_dev" %in% names(merged_df)) merged_df$std_dev <- NA_real_
    
    # Win Share: prefer per-outcome sheet "Win_Share_<outcome>", else use combined if present
    df_ws_outcome <- tryCatch(read_excel(excel_path, sheet = paste0("Win_Share_", new_outcome)),
                              error = function(e) NULL)
    if (!is.null(df_ws_outcome)) {
      ws_df <- df_ws_outcome
    } else if (!is.null(df_ws_combined) && "outcome" %in% names(df_ws_combined)) {
      ws_df <- dplyr::filter(df_ws_combined, .data$outcome == !!new_outcome)
    } else {
      ws_df <- NULL
    }
    if (!is.null(ws_df) && (new_outcome %in% names(ws_df))) {
      ws_df2 <- ws_df %>%
        select(firm, firm_id, !!rlang::sym(new_outcome)) %>%
        rename(win_share = !!rlang::sym(new_outcome))
      merged_df <- merged_df %>%
        left_join(ws_df2, by = c("firm","firm_id")) %>%
        mutate(win_share = suppressWarnings(as.numeric(win_share)))
    } else {
      merged_df$win_share <- NA_real_
    }
    
    # keep an unnormalized copy for table/logit plot
    merged_base <- merged_df
    
    merged_df <- merged_df %>%
      arrange(avg_rating) %>%
      mutate(
        new_firm_id        = dplyr::row_number(),
        Estimate_neg       = -Estimate,
        Estimate_neg_eb    = -Estimate_eb_old,
        Estimate_neg_eb_v2 = -Estimate_eb_new
      )
    
    # pretty label
    outcome_label <- if (!is.null(label_mapping) && new_outcome %in% names(label_mapping)) {
      label_mapping[[new_outcome]]
    } else new_outcome
    
    # -------------------------------
    # Summary table (LaTeX + CSV)
    # -------------------------------
    cap_ws <- if (!is.null(ws_type) && nzchar(ws_type)) paste("(WS:", ws_type, ")") else ""
    summary_table <- merged_base %>%
      mutate(
        N                     = as.integer(num_observations),
        `Item Worth`          = sprintf("%.2f", Estimate),
        `Item Worth (EB)`     = sprintf("%.2f", Estimate_eb_old),
        `Avg Rating`          = ifelse(is.na(std_dev),
                                       sprintf("%.2f", avg_rating),
                                       sprintf("%.2f (%.2f)", avg_rating, std_dev)),
        `Win Share`           = ifelse(!is.na(win_share),
                                       sprintf("%.3f", win_share),
                                       "")
      ) %>%
      select(firm, N, `Item Worth`, `Item Worth (EB)`, `Avg Rating`, `Win Share`)
    
    table_file_tex <- file.path(tables_dir, paste0("summary_table_", new_outcome, suffix, ".tex"))
    print(xtable(summary_table,
                 caption = paste("Summary Table -", outcome_label, cap_ws)),
          file = table_file_tex, include.rownames = FALSE)
    table_file_csv <- file.path(tables_dir, paste0("summary_table_", new_outcome, suffix, ".csv"))
    write.csv(summary_table, table_file_csv, row.names = FALSE)
    cat("✅ Tables saved:", basename(table_file_tex), "and", basename(table_file_csv), "\n")
    
    # -------------------------------
    # NEW) Top/Bottom N by Borda EB (x sorted by Borda EB) ...
    # -------------------------------
    pl_eb_key2 <- choose_pl_eb_key(
      df_old = coeff_df_eb_old,
      df_new = if (!all(is.na(coeff_df_eb_new$Estimate_eb_new))) coeff_df_eb_new else NULL
    ) %>% dplyr::rename(EB_PL = eb_key)
    
    eb_dual <- pl_eb_key2 %>%
      dplyr::left_join(borda_eb_df, by = c("firm","firm_id")) %>%
      dplyr::select(firm, firm_id, EB_PL, Borda_EB)
    
    if (all(is.na(eb_dual$EB_PL)) || all(is.na(eb_dual$Borda_EB))) {
      message("⚠️ Missing EB PL or Borda EB for outcome: ", new_outcome, " — skipping dual-axis Top/Bottom plot.")
    } else {
      mean_borda_all <- mean(eb_dual$Borda_EB, na.rm = TRUE)
      ranked_dual <- eb_dual %>% dplyr::mutate(.rank_key = Borda_EB)
      topN_dual <- ranked_dual %>% dplyr::slice_max(order_by = .rank_key, n = n_keep_overlay, with_ties = FALSE)
      botN_dual <- ranked_dual %>% dplyr::slice_min(order_by = .rank_key, n = n_keep_overlay, with_ties = FALSE)
      botN_dual <- botN_dual %>% arrange(Borda_EB, firm)
      topN_dual <- topN_dual %>% arrange(Borda_EB, firm)
      
      spacer <- tibble(
        firm            = paste0("__gap", seq_len(gap_width), "__"),
        firm_id        = NA_integer_,
        EB_PL          = NA_real_,
        Borda_EB       = NA_real_,
        Borda_scaled_to_PL = NA_real_
      )
      subset_dual <- bind_rows(botN_dual, spacer, topN_dual)
      
      s_pl <- stats::sd(subset_dual$EB_PL,    na.rm = TRUE); if (!is.finite(s_pl) || s_pl == 0) s_pl <- 1
      s_bd <- stats::sd(subset_dual$Borda_EB, na.rm = TRUE); if (!is.finite(s_bd) || s_bd == 0) s_bd <- 1
      a <- s_pl / s_bd
      b <- -a * mean_borda_all                      # sec-axis maps y=0 -> mean Borda EB
      
      subset_dual <- subset_dual %>%
        mutate(
          firm = factor(firm, levels = firm, ordered = TRUE),
          Borda_scaled_to_PL = a * Borda_EB + b
        )
      inv_to_borda <- function(y) (y - b) / a  # primary -> Borda_EB (sec axis)
      
      guides_dual <- subset_dual %>%
        group_by(firm) %>%
        summarise(
          ymin = suppressWarnings(pmin(EB_PL, Borda_scaled_to_PL, na.rm = TRUE)),
          ymax = suppressWarnings(pmax(EB_PL, Borda_scaled_to_PL, na.rm = TRUE)),
          .groups = "drop"
        )
      n_bottom <- nrow(botN_dual)
      gap_start <- n_bottom + 0.5
      gap_end   <- n_bottom + gap_width + 0.5
      hide_gap_labels <- function(x) ifelse(grepl("^__gap\\d+__$", x), "", x)
      
      p_tb_bordaEB_dual <- ggplot(subset_dual, aes(x = firm)) +
        geom_segment(data = guides_dual,
                     aes(x = firm, xend = firm, y = ymin, yend = ymax),
                     inherit.aes = FALSE, linewidth = 0.3, alpha = 0.35) +
        geom_point(aes(y = EB_PL,            color = "PL (EB)"), size = 2.6, alpha = 0.9) +
        geom_line (aes(y = EB_PL,            color = "PL (EB)", group = 1), linewidth = 0.7, alpha = 0.9) +
        geom_point(aes(y = Borda_scaled_to_PL, color = "Borda (EB)"), size = 2.6, alpha = 0.9, shape = 17) +
        geom_line (aes(y = Borda_scaled_to_PL, color = "Borda (EB)", group = 1),
                   linewidth = 0.7, alpha = 0.9, linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
        geom_vline(xintercept = gap_start, linetype = "dashed", linewidth = 0.6, color = "grey55") +
        geom_vline(xintercept = gap_end,   linetype = "dashed", linewidth = 0.6, color = "grey55") +
        scale_y_continuous(
          name     = "Plackett–Luce Item Worth (EB)",
          sec.axis = sec_axis(~ inv_to_borda(.), name = "Borda Score (EB)")
        ) +
        scale_color_manual(values = c("PL (EB)" = "steelblue", "Borda (EB)" = "darkorange")) +
        labs(title = "", x = "Firm (sorted by Borda EB)", color = "") +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = "black"),
          plot.background  = element_rect(fill = "white", color = NA),
          plot.margin      = margin(t = 10, r = 20, b = 80, l = 90)
        ) +
        scale_x_discrete(labels = hide_gap_labels, expand = expansion(add = 0.8)) +
        coord_cartesian(clip = "off")
      
      ggsave(file.path(plots_dir, paste0("topbottom_by_bordaEB_dualaxis_", new_outcome, suffix, ".png")),
             p_tb_bordaEB_dual, width = 16, height = 8, dpi = 300)
      cat("✅ Saved: ", paste0("topbottom_by_bordaEB_dualaxis_", new_outcome, suffix, ".png "), "\n")
    }
    
    # -------------------------------
    # 6.5) Borda (y) vs Expected Borda (x), axes flipped; 0–100 if scaled
    # -------------------------------
    # expected_sheet <- pick_expected_sheet(excel_path, "pl_to_borda_", new_outcome, max_len = 31L)
    # df_e_raw <- if (!is.na(expected_sheet)) {
    #   tryCatch(read_excel(excel_path, sheet = expected_sheet), error = function(e) NULL)
    # } else NULL
    # 
    # if (!is.null(df_e_raw) && all(c("firm_id", "borda_score") %in% names(df_e_raw))) {
    #   e_col <- paste0("e_", new_outcome)
    #   df_e <- df_e_raw %>%
    #     transmute(
    #       firm_id   = suppressWarnings(as.integer(firm_id)),
    #       !!e_col := suppressWarnings(as.numeric(borda_score)) * borda_mult
    #     )
    #   
    #   borda_actual <- if (!is.null(df_borda) && all(c("firm","firm_id", new_outcome) %in% names(df_borda))) {
    #     df_borda %>%
    #       transmute(
    #         firm,
    #         firm_id = suppressWarnings(as.integer(firm_id)),
    #         Borda   = suppressWarnings(as.numeric(.data[[new_outcome]])) * borda_mult
    #       )
    #   } else NULL
    #   
    #   if (!is.null(borda_actual)) {
    #     bvse <- borda_actual %>% left_join(df_e, by = "firm_id")
    #     
    #     if (e_col %in% names(bvse)) {
    #       e_sym <- rlang::sym(e_col)
    #       
    #       p_bvse <- ggplot(
    #         bvse %>% dplyr::filter(is.finite(Borda), is.finite(!!e_sym)),
    #         aes(x = !!e_sym, y = Borda)
    #       ) +
    #         geom_point(alpha = 0.8, size = 2) +
    #         geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #         coord_equal() +
    #         scale_x_continuous(limits = c(0, borda_max), breaks = borda_breaks, expand = c(0, 0)) +
    #         scale_y_continuous(limits = c(0, borda_max), breaks = borda_breaks, expand = c(0, 0)) +
    #         theme_minimal(base_size = 14) +
    #         theme(
    #           panel.grid.major = element_blank(),
    #           panel.grid.minor = element_blank(),
    #           panel.background = element_rect(fill = "white", color = "black"),
    #           plot.background  = element_rect(fill = "white", color = NA),
    #           legend.position  = "none"
    #         ) +
    #         labs(title = "",
    #              x = "Expected Borda Score",
    #              y = "Borda Score")
    #       
    #       
    #       ggsave(file.path(plots_dir, paste0("borda_vs_expected_", new_outcome, suffix, ".png")),
    #              p_bvse, width = 8, height = 6, dpi = 300)
    #       
    #       cat("✅ Saved: ", paste0("borda_vs_expected_", new_outcome, suffix, ".png "),
    #           " (sheet: ", expected_sheet, ")\n")
    #     } else {
    #       message("⚠️ Expected column '", e_col, "' not found after merge — skipping. (sheet: ", expected_sheet, ")")
    #     }
    #   } else {
    #     message("⚠️ 'borda_score' sheet missing or outcome column absent for ", new_outcome,
    #             " — cannot make Borda vs Expected plot.")
    #   }
    # } else {
    #   message("⚠️ Missing or malformed expected sheet for ", new_outcome,
    #           " under 31-char cap (tried full & truncated): '", expected_sheet,
    #           "' — needs columns: firm_id, borda_score.")
    # }
    
    # -------------------------------
    # 7) Tri-series (scatter + best-fit) unchanged (labels note ×100 if scaled)
    # -------------------------------
    tri_df <- coeff_df %>%
      dplyr::select(firm, firm_id, Estimate) %>%                                  # PL MLE
      dplyr::left_join(
        if (!all(is.na(coeff_df_eb_new$Estimate_eb_new))) {
          coeff_df_eb_new %>% dplyr::select(firm, firm_id, EB_PL = Estimate_eb_new)
        } else {
          coeff_df_eb_old %>% dplyr::select(firm, firm_id, EB_PL = Estimate_eb_old)
        },
        by = c("firm","firm_id")
      ) %>%
      dplyr::left_join(borda_df,    by = c("firm","firm_id")) %>%                 # Borda (regular)
      dplyr::left_join(borda_eb_df, by = c("firm","firm_id")) %>%                 # Borda (EB) for X
      dplyr::rename(Borda = Borda, Borda_EB = Borda_EB) %>%
      dplyr::filter(is.finite(Estimate), is.finite(EB_PL), is.finite(Borda), is.finite(Borda_EB))
    
    if (nrow(tri_df) == 0) {
      message("⚠️ Missing data for tri-series scatter: ", new_outcome, " — skipping.")
    } else {
      s_pl <- stats::sd(tri_df$Estimate, na.rm = TRUE);  if (!is.finite(s_pl) || s_pl == 0) s_pl <- 1
      m_pl <- mean(tri_df$Estimate, na.rm = TRUE)
      s_bd <- stats::sd(tri_df$Borda,    na.rm = TRUE);  if (!is.finite(s_bd) || s_bd == 0) s_bd <- 1
      m_bd <- mean(tri_df$Borda,    na.rm = TRUE)
      
      tri_df <- tri_df %>%
        dplyr::mutate(Borda_scaled_to_PL = (Borda - m_bd) / s_bd * s_pl + m_pl)
      inv_to_borda <- function(y) (y - m_pl) / s_pl * s_bd + m_bd   # primary -> Borda (sec axis)
      slope45 <- if (s_bd > 0) s_pl / s_bd else 0
      intercept45 <- m_pl - slope45 * m_bd
      
      p_tri <- ggplot(tri_df, aes(x = Borda_EB)) +
        geom_point(aes(y = EB_PL,             color = "Plackett Luce (EB)"),  alpha = 0.6, size = 1.8) +
        geom_point(aes(y = Estimate,          color = "Plackett Luce (MLE)"), alpha = 0.6, size = 1.8) +
        geom_point(aes(y = Borda_scaled_to_PL, color = "Borda Score"),        alpha = 0.6, size = 1.8) +
        geom_smooth(aes(y = EB_PL,             color = "Plackett Luce (EB)"),
                    method = "lm", se = FALSE, linewidth = 0.9) +
        geom_smooth(aes(y = Estimate,          color = "Plackett Luce (MLE)"),
                    method = "lm", se = FALSE, linewidth = 0.9) +
        geom_smooth(aes(y = Borda_scaled_to_PL, color = "Borda Score"),
                    method = "lm", se = FALSE, linewidth = 0.9) +
        geom_abline(slope = slope45, intercept = intercept45,
                    linetype = "dashed", color = "black") +
        scale_y_continuous(
          name = "Plackett–Luce Item Worth",
          sec.axis = sec_axis(~ inv_to_borda(.), name = "Borda Score")
        ) +
        scale_x_continuous(
          name = "Borda Score (EB)",
          expand = c(0, 0)
        ) +
        scale_color_manual(values = c(
          "Plackett Luce (EB)"  = "#cc7a00",
          "Plackett Luce (MLE)" = "#4b72a6",
          "Borda Score"         = "#800000"
        )) +
        labs(color = "") +
        theme_minimal(base_size = 14) +
        theme(
          legend.position  = "top",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", color = "black"),
          plot.background  = element_rect(fill = "white", color = NA)
        )
      
      ggsave(file.path(plots_dir, paste0("tri_dualaxis_bordaEBx_", new_outcome, suffix, ".png")),
             p_tri, width = 12, height = 7, dpi = 300)
      cat("✅ Saved: ", paste0("tri_dualaxis_bordaEBx_", new_outcome, suffix, ".png "), "\n")
    }
    
  } # end for outcomes
}



# -------------------------------
# Example calls
# -------------------------------
# excel_dir <- "/path/to/excels"
excel_path  <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")

label_mapping <- c(
  "discretion" = "Manager Discretion",
  "FirmSelective" = "Firm Selectivity",
  "FirmDesire" = "Firm Desirability",
  "conduct_black" = "Discrimination Black (Conduct)",
  "conduct_favor_younger" = "Discrimination Older (Conduct)",
  "conduct_favor_male" = "Discrimination Female (Conduct)",
  "FirmHire_favor_male" = "Discrimination Female (Hire)",
  "FirmHire_favor_white" = "Discrimination Black (Hire)",
  "FirmCont_favor_male" = "Discrimination Female (Contact)",
  "FirmCont_favor_white" = "Discrimination Black (Contact)",
  "pooled_favor_white"   = "Discrimination Black (Pooled)",
  "pooled_favor_male"    = "Discrimination Female (Pooled)"
)

outs <- c(
  "FirmCont_favor_white","FirmHire_favor_white","conduct_black",
  "FirmCont_favor_male","FirmHire_favor_male","conduct_favor_male",
  "conduct_favor_younger","discretion","FirmSelective","FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)

# Example: rescale Borda to 0–100 everywhere
create_plots_and_tables_from_sheets(
  excel_path, outs, figures, tables,
  label_mapping = label_mapping,
  add_45_line = TRUE,
  scale_borda_to_100 = FALSE
)
