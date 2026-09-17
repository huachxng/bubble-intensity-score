# =====================================================================
# BIS v1.0  —  run_all.R      the whole pipeline, top to bottom
# =====================================================================
# Open bis-r.Rproj first, then either press Source on this file or run:
#     source("run_all.R")
#
# Which code runs is controlled by USE_SOLUTIONS in R/00_setup.R:
#   FALSE -> your scripts in R/            (default)
#   TRUE  -> the finished ones in R/solutions/
#
# Which pillar recipe is built is controlled by BIS_VERSION, same file:
#   "v0.1" -> the Python prototype's four pillars. Build this FIRST and
#             pass the Phase 4 regression test in 04.
#   "v1.0" -> adds NFCI leverage to L and GPR to S.
# =====================================================================

source("R/00_setup.R")

source_stage("01_load_raw.R")     # L0  raw -> data-raw/
source_stage("02_build_panel.R")  # L1  clean -> data-clean/   (4 TODOs)
source_stage("03_pillars.R")      # L2-4 panel, z-scores, pillars (3 TODOs)
source_stage("04_bis.R")          # L5  composite + regression test (1 TODO)
source_stage("05_backtest.R")     # backtest, hand check, sensitivity (3 TODOs)
source_stage("06_figures.R")      # every exhibit

message("\ndone. outputs in ", DIR_OUT, "/")
