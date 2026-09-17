# =====================================================================
# BIS v1.0  —  00_setup.R
# Shared configuration. EVERY other script starts by sourcing this file.
# Nothing here is a TODO — this is plumbing. Read it once, then move on.
# =====================================================================

library(tidyverse)
library(readxl)


# ---- THE SWITCH -----------------------------------------------------
# FALSE = run YOUR scripts in R/            <- coach mode, the default
# TRUE  = run the finished ones in R/solutions/
#
# Stuck on a TODO? Flip this to TRUE, run the pipeline to see the number
# it should produce, then flip back to FALSE and make yours match.
USE_SOLUTIONS <- FALSE

# ---- Paths (relative — they work because you opened bis-r.Rproj) ----
DIR_RAW   <- "data-raw"     # L0:0 downloads. NEVER edited, by you or by code.
DIR_CLEAN <- "data-clean"   # L1: one tidy file per indicator, all same shape
DIR_OUT   <- "output"       # L2-L5: panel, tables, scorecard
DIR_FIG   <- file.path(DIR_OUT, "figures")

for (d in c(DIR_RAW, DIR_CLEAN, DIR_OUT, DIR_FIG)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- Method constants ------------------------------------------------
# Changing any of these invalidates the Phase 4 regression test.
Z_WINDOW    <- 120      # trailing months, INCLUDING the current month t
Z_MIN_OBS   <- 36       # minimum months of history before a z-score is defined
PANEL_START <- "1985-01"  # GPR's first month, and exactly 120 months before
                          # the Dot-com window opens in 1995-01
DANGER_LINE <- 1.5      # the threshold line drawn on every BIS chart

# ---- Which pillar recipe to build ------------------------------------
# "v0.1" = one indicator per pillar, exactly as the Python prototype.
#          Build this FIRST and pass the Phase 4 regression test.
# "v1.0" = adds NFCI leverage to L and GPR to S. Only switch after v0.1 passes.
BIS_VERSION <- "v1.0"

# Sign of the NFCI leverage subindex, decided empirically in 02 (sign check).
# +1 = higher reading means more leverage (expected). -1 = flip it.
NFCI_SIGN <- 1

# ---- The three analysis windows --------------------------------------
WINDOWS <- tibble::tribble(
  ~name,    ~start,    ~end,      ~ref_month, ~ref_event,
  "dotcom", "1995-01", "2003-12", "2000-03",  "Nasdaq peak, 2000-03-10",
  "gfc",    "2003-01", "2010-12", "2007-08",  "BNP Paribas freeze, 2007-08-09",
  "ai",     "2019-01", NA,        NA,         "out-of-sample - no known outcome"
)

# ---- v0.1 ANCHORS: the regression test (Phase 4) ---------------------
# Produced by bis-prototype/replicate_bis.py on 2026-06-11. NA = the source
# notes recorded only the composite for that month.
#
# close_band is how far a value may drift from the recorded anchor and still
# count as a faithful port. It is NOT slack for code bugs — it covers changes
# in the DATA since June 2026:
#   - historical rows (0.10): each quarterly Z.1 release revises debt history
#     a little, and mirror sources round differently than FRED;
#   - the 2026-05 row (0.50): its L was forward-filled from whatever the
#     newest quarter said in June — the March-vintage Z.1. The June release
#     then revised 2026Q1 debt growth upward, moving L from -0.50 to ~-0.05.
#     That is the data changing, not the method. (multpl also revises the
#     newest CAPE months as earnings settle.)
# A genuine code bug (wrong sd, join-before-z, monthly-last) throws numbers
# far outside these bands — the self-tests in 03 catch those directly.
ANCHORS <- tibble::tribble(
  ~month,    ~V,    ~L,    ~S,    ~C,   ~BIS, ~close_band,
  "2000-03",  2.04,  0.89, -0.74,  5.06,  1.81, 0.10,
  "1999-12",  NA,    NA,    NA,    NA,    1.73, 0.10,
  "2002-09",  NA,    NA,    NA,    NA,   -1.65, 0.10,
  "2007-07", -0.53,  1.43,  0.53, -0.23,  0.30, 0.10,
  "2026-05",  2.29, -0.50,  0.20,  1.74,  0.93, 0.50
)

# ---- Month helpers ---------------------------------------------------
# Months are CHARACTER "YYYY-MM" everywhere. Not Date, not yearmon.
# Character joins never silently mis-align, and it matches the v0.1 CSVs.

as_month <- function(x) format(as.Date(x), "%Y-%m")

# integer index so month arithmetic is exact (no 30.4-day approximations)
month_index <- function(m) {
  as.integer(substr(m, 1, 4)) * 12L + as.integer(substr(m, 6, 7))
}

months_between <- function(a, b) month_index(a) - month_index(b)

month_seq <- function(from, to) {
  format(seq(as.Date(paste0(from, "-01")),
             as.Date(paste0(to,   "-01")), by = "month"), "%Y-%m")
}

# Collapse a daily/weekly series to a monthly MEAN.
# Mean, not last-of-month: v0.1 used FRED's "&fam=avg" monthly average, and
# a last-value monthly series will NOT reproduce the anchors.
to_monthly_mean <- function(df) {
  df %>%
    mutate(month = format(date, "%Y-%m")) %>%
    group_by(month) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(month)
}

# ---- Safe versions of things that blow up on all-NA input ------------
# Half-built pipelines hit these constantly: max() of nothing is -Inf,
# which.max() of nothing is integer(0), and cor(use="complete.obs")
# throws an ERROR rather than returning NA. These return NA instead, so a
# missing indicator shows up as a blank cell you can see, not a stack trace.
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

which_max_month <- function(months, x) {
  if (all(is.na(x))) NA_character_ else months[which.max(x)]
}

safe_cor <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3) NA_real_ else suppressWarnings(stats::cor(a[ok], b[ok]))
}

# ---- The two-column contract ----------------------------------------
# Every cleaned indicator is exactly: month (chr "YYYY-MM") + value (dbl).
# This function enforces it, and is called before every write to data-clean/.
check_contract <- function(df, label) {
  if (nrow(df) == 0) {
    stop("'", label, "' came out EMPTY (0 rows).\n",
         "  Nearly always this means a TODO above it is still returning NA, ",
         "so filter(!is.na(value)) threw every row away.\n",
         "  Fill in the TODO, or set USE_SOLUTIONS <- TRUE in 00_setup.R to ",
         "see what it should produce.", call. = FALSE)
  }
  stopifnot(identical(names(df), c("month", "value")))
  stopifnot(is.character(df$month), is.numeric(df$value))
  stopifnot(!any(duplicated(df$month)))
  stopifnot(all(grepl("^[0-9]{4}-[0-9]{2}$", df$month)))
  message(sprintf("  OK  %-22s %4d rows  %s .. %s",
                  label, nrow(df), min(df$month), max(df$month)))
  df
}

write_clean <- function(df, filename, label = filename) {
  df %>% check_contract(label) %>% readr::write_csv(file.path(DIR_CLEAN, filename))
  invisible(df)
}

# ---- Stage runner: obeys USE_SOLUTIONS -------------------------------
source_stage <- function(file) {
  path <- if (USE_SOLUTIONS) file.path("R", "solutions", file) else file.path("R", file)
  message("\n=== ", path, " ===")
  source(path, local = FALSE)
}

message("setup loaded | mode: ", if (USE_SOLUTIONS) "SOLUTIONS" else "coach (your code)",
        " | wd: ", basename(getwd()))
