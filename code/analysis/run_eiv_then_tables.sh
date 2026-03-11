#!/bin/bash
# 1. Wait for current subsample run (4 models) to finish
# 2. Build tables (OL will show as NA)
# 3. Run OL-only EIV for all files
# 4. Rebuild tables with OL data

cd /Users/monicahea/Desktop/survey-bias/survey-bias

echo "$(date): Waiting for subsample EIV (PID 61591) to finish..."
while kill -0 61591 2>/dev/null; do
    sleep 30
done
echo "$(date): Subsample EIV finished."

echo "$(date): Waiting for Full Sample EIV (PID 78097) to finish..."
while kill -0 78097 2>/dev/null; do
    sleep 30
done
echo "$(date): Full Sample EIV finished."

echo "$(date): Building tables (first pass, OL will be NA for subsamples)..."
Rscript code/create_tables_figures/eiv_table_panels.R 2>&1
Rscript code/create_tables_figures/eiv_table_discretion.R 2>&1
Rscript code/create_tables_figures/cross_sample_signal_corr.R 2>&1
echo "$(date): First table build complete."

echo "$(date): Running OL-only EIV for all files..."
Rscript code/analysis/run_eiv_ol_only.R 2>&1 | tee output/run_eiv_ol_only.log
echo "$(date): OL EIV complete."

echo "$(date): Rebuilding tables with OL data..."
Rscript code/create_tables_figures/eiv_table_panels.R 2>&1
Rscript code/create_tables_figures/eiv_table_discretion.R 2>&1
Rscript code/create_tables_figures/cross_sample_signal_corr.R 2>&1
echo "$(date): All done!"
