# Run just the within/between-industry variance table
source("code/globals.R")
source(file.path(analysis, "katz_correct.R"))
source("code/create_tables_figures/summary_item_worths.R")

excel_path <- file.path(excel, "Plackett_Luce_Full_Sample.xlsx")

label_mapping <- c(
  "discretion" = "Manager Discretion",
  "FirmSelective" = "Firm Selectivity",
  "FirmDesire" = "Firm Desirability",
  "conduct_favor_white" = "Discrimination Black (Conduct)",
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
  "FirmCont_favor_white","FirmHire_favor_white","conduct_favor_white",
  "FirmCont_favor_male","FirmHire_favor_male","conduct_favor_male",
  "conduct_favor_younger","discretion","FirmSelective","FirmDesire",
  "pooled_favor_white", "pooled_favor_male"
)

write_variance_biascorrected_ols_borda_within_between(
  excel_path = excel_path,
  outcomes = outs,
  tables_dir = tables,
  label_mapping = label_mapping,
  borda_mult = 1
)
