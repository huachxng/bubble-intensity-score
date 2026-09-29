# run_all.R: rebuild every table and figure of the paper.
#
#   Rscript run_all.R             reproduce the paper from data-clean/ (no internet needed)
#   Rscript run_all.R --refresh   download the newest data and recompute everything in latest/
#
# In an R session, run it from this folder with source("run_all.R"), or
# BIS_REFRESH <- TRUE; source("run_all.R") for a refresh.

source("R/00_setup.R")

if (REFRESH) {
  source("R/01_load_raw.R")      # download the raw data into latest/raw/
  source("R/02_build_panel.R")   # six monthly series into latest/clean/
}
source("R/03_pillars.R")         # z-scores, monthly panel, four pillars
source("R/04_bis.R")             # composite index, analysis windows, v0.1 anchor check
source("R/05_backtest.R")        # backtest scorecard, hand check, sensitivity tests
source("R/06_figures.R")         # figures
source("R/07_fama_test.R")       # Fama's forecast-date test

message("\nDone. Results are in ", DIR_OUT, "/")
