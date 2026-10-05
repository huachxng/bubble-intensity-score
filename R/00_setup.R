# 00_setup.R: packages, settings, folders and helper functions used by every script.

library(tidyverse)
library(readxl)

# ---- Run mode -----------------------------------------------------------
# Default run: start from the committed monthly series in data-clean/ and
# reproduce the paper (26 August 2026 data vintage) in output/.
# Refresh run: download the newest data into latest/ and recompute there.
# Start it with `Rscript run_all.R --refresh`, or in an R session with
# BIS_REFRESH <- TRUE before source("run_all.R").
REFRESH <- isTRUE(get0("BIS_REFRESH", envir = globalenv(), ifnotfound = FALSE)) ||
  "--refresh" %in% commandArgs(trailingOnly = TRUE)

# ---- Index version ------------------------------------------------------
# "v1.0": the paper's index. L averages corporate debt growth and the NFCI
#         leverage subindex; S averages the flipped VIX and the flipped GPR.
# "v0.1": the earlier one-indicator-per-pillar prototype (V = CAPE,
#         L = corporate debt growth, S = flipped VIX, C = Nasdaq/S&P).
#         Its results go to a v0.1/ subfolder so the paper's files are kept.
BIS_VERSION <- "v1.0"

# ---- Folders --------------------------------------------------------------
if (REFRESH) {
  DIR_RAW   <- file.path("latest", "raw")
  DIR_CLEAN <- file.path("latest", "clean")
  DIR_OUT   <- file.path("latest", "output")
} else {
  DIR_RAW   <- "data-raw"     # raw files of the paper's vintage
  DIR_CLEAN <- "data-clean"   # one monthly series per indicator: month, value
  DIR_OUT   <- "output"       # panel, tables and figures
}
if (BIS_VERSION == "v0.1") DIR_OUT <- file.path(DIR_OUT, "v0.1")
DIR_FIG <- file.path(DIR_OUT, "figures")

for (d in c(DIR_RAW, DIR_CLEAN, DIR_OUT, DIR_FIG)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# ---- Method constants -----------------------------------------------------
Z_WINDOW    <- 120        # trailing months in a z-score, including the current month
Z_MIN_OBS   <- 36         # fewer observations than this gives NA
PANEL_START <- "1985-01"  # first GPR month; 120 months before the Dot-com window
DANGER_LINE <- 1.5        # warning threshold drawn on the BIS charts

# Sign of the NFCI leverage subindex. The subindex rises into 2007-08, so a
# higher reading already means more leverage and is not flipped.
NFCI_SIGN <- 1

# ---- Analysis windows -------------------------------------------------------
# The AI window opens with the launch of ChatGPT (2022-11-30) and has no end
# date. Up to v1.1.0 it opened in 2019-01, which put the index's 2020 warning
# (August and December 2020, the pandemic period) inside the AI era.
WINDOWS <- tibble::tribble(
  ~name,    ~start,    ~end,      ~ref_month, ~ref_event,
  "dotcom", "1995-01", "2003-12", "2000-03",  "Nasdaq peak, 2000-03-10",
  "gfc",    "2003-01", "2010-12", "2007-08",  "BNP Paribas freeze, 2007-08-09",
  "ai",     "2022-11", NA,        NA,         "out-of-sample - no known outcome"
)

# ---- v0.1 anchors -----------------------------------------------------------
# Values recorded from the Python prototype (replicate_bis.py) on 2026-06-11,
# used to verify the R code in 04_bis.R. NA = only the composite was recorded.
# A result within 0.01 is exact. close_band is the drift allowed for data
# revisions since June 2026: 0.10 for historical rows (Z.1 revises debt
# history each quarter; sources round differently), 0.50 for 2026-05, whose
# L was carried forward from the March-vintage Z.1 before the June release
# revised 2026Q1 debt growth upward (L moved from -0.50 to about -0.05).
ANCHORS <- tibble::tribble(
  ~month,    ~V,    ~L,    ~S,    ~C,   ~BIS, ~close_band,
  "2000-03",  2.04,  0.89, -0.74,  5.06,  1.81, 0.10,
  "1999-12",  NA,    NA,    NA,    NA,    1.73, 0.10,
  "2002-09",  NA,    NA,    NA,    NA,   -1.65, 0.10,
  "2007-07", -0.53,  1.43,  0.53, -0.23,  0.30, 0.10,
  "2026-05",  2.29, -0.50,  0.20,  1.74,  0.93, 0.50
)

# ---- Month helpers ----------------------------------------------------------
# Months are character "YYYY-MM" throughout: joins on them cannot mis-align,
# and they sort correctly as text.
as_month <- function(x) format(as.Date(x), "%Y-%m")

# Month as an integer, so month arithmetic is exact.
month_index <- function(m) {
  as.integer(substr(m, 1, 4)) * 12L + as.integer(substr(m, 6, 7))
}

months_between <- function(a, b) month_index(a) - month_index(b)

month_seq <- function(from, to) {
  format(seq(as.Date(paste0(from, "-01")),
             as.Date(paste0(to,   "-01")), by = "month"), "%Y-%m")
}

# Daily or weekly series to monthly MEAN (not last value), as in the
# prototype, which used FRED's monthly averages.
to_monthly_mean <- function(df) {
  df %>%
    mutate(month = format(date, "%Y-%m")) %>%
    group_by(month) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(month)
}

# ---- NA-safe summaries --------------------------------------------------------
# Return NA instead of -Inf, integer(0) or an error when no value is present.
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

which_max_month <- function(months, x) {
  if (all(is.na(x))) NA_character_ else months[which.max(x)]
}

safe_cor <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 3) NA_real_ else suppressWarnings(stats::cor(a[ok], b[ok]))
}

# ---- Clean-series format ------------------------------------------------------
# Every cleaned indicator has exactly two columns: month (chr "YYYY-MM") and
# value (dbl), one row per month.
check_contract <- function(df, label) {
  if (nrow(df) == 0) {
    stop("'", label, "' has no rows after cleaning. Check its raw file in ",
         DIR_RAW, "/.", call. = FALSE)
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

# ---- Shiller data -------------------------------------------------------------
# Monthly S&P 500 price, dividend and CAPE (PE10) from the Shiller data mirror.
read_shiller <- function(path = file.path(DIR_RAW, "shiller_sp500.csv")) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) %>%
    transmute(month    = format(as.Date(Date), "%Y-%m"),
              sp500    = as.numeric(SP500),
              pe10     = as.numeric(PE10),
              dividend = as.numeric(Dividend)) %>%
    mutate(pe10 = if_else(pe10 == 0, NA_real_, pe10)) %>%   # the mirror writes 0 for missing CAPE
    filter(!is.na(month)) %>%
    arrange(month)
}

message("BIS ", BIS_VERSION, " | ",
        if (REFRESH) "refresh run: newest data" else "default run: paper data (26 Aug 2026 vintage)",
        " | results in ", DIR_OUT, "/")
