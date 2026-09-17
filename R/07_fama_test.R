# =====================================================================
# BIS v1.0  —  07_fama_test.R      Fama's forecast-date test, applied to the BIS
# =====================================================================
# Not part of the TODO pipeline. Run it after run_all.R:
#     source("R/07_fama_test.R")
#
# Fama (2014, pp. 1476-1477) judges a bubble warning by the market's level on
# the date of the FIRST warning: the warning only counts if prices later fell
# below that level. Shiller's December 1996 warning fails his test. This script
# applies the same test to the BIS's own warning dates — the strongest possible
# form of the counterargument, answered with the index's own numbers.
#
# Stand-in data: the S&P 500 from the Shiller file (monthly averages), where
# Fama used the CRSP value-weight index on exact days. The calibration row
# re-runs Fama's Shiller example so the substitution can be checked.
# =====================================================================

source("R/00_setup.R")

sh <- readr::read_csv(file.path(DIR_RAW, "shiller_sp500.csv"), show_col_types = FALSE) %>%
  transmute(month = format(as.Date(Date), "%Y-%m"),
            P = as.numeric(SP500),
            D = as.numeric(Dividend)) %>%
  filter(!is.na(P)) %>%
  arrange(month) %>%
  tidyr::fill(D, .direction = "down")

# Shiller's Dividend column is annualised, so one month of dividends is D / 12.
# Fama's test uses wealth with dividends reinvested; the price-only change is
# reported alongside it.
sh$TR <- cumprod(c(1, (sh$P[-1] + sh$D[-1] / 12) / sh$P[-nrow(sh)]))

add_months <- function(m, k) {
  i <- month_index(m) + k
  sprintf("%04d-%02d", (i - 1) %/% 12, (i - 1) %% 12 + 1)
}

fama_test <- function(case, signal, horizon_end) {
  horizon_end <- min(horizon_end, max(sh$month))
  s  <- sh %>% filter(month == signal)
  w  <- sh %>% filter(month > signal, month <= horizon_end)
  tp <- w %>% slice_min(P,  n = 1, with_ties = FALSE)
  tt <- w %>% slice_min(TR, n = 1, with_ties = FALSE)
  back <- sh %>% filter(month > tp$month, P >= s$P) %>% slice_head(n = 1)
  tibble(
    case                 = case,
    signal_month         = signal,
    window_end           = horizon_end,
    sp500_at_signal      = round(s$P),
    trough_month         = tp$month,
    sp500_at_trough      = round(tp$P),
    price_change_pct     = round(100 * (tp$P / s$P - 1), 1),
    with_dividends_pct   = round(100 * (tt$TR / s$TR - 1), 1),
    price_level_regained = if (nrow(back)) back$month else NA_character_
  )
}

# ---- The BIS's warning episodes, read from the pipeline output ---------
# An episode is a run of months at or above the danger line; runs less than a
# year apart are one episode. Each episode is tested twice: at its first
# warning month and at its peak.
panel_bis <- readr::read_csv(file.path(DIR_OUT, "bis_panel_monthly.csv"), show_col_types = FALSE) %>%
  select(month, BIS) %>%
  filter(month >= "1995-01", !is.na(BIS)) %>%
  arrange(month)

hot <- panel_bis %>% filter(BIS >= DANGER_LINE)
episodes <- hot %>%
  mutate(episode = cumsum(c(1, diff(month_index(month)) > 12))) %>%
  group_by(episode) %>%
  summarise(first_warning = first(month),
            peak          = month[which.max(BIS)],
            .groups = "drop")

HORIZON_MONTHS <- 48

fama_results <- bind_rows(
  fama_test("Calibration: Shiller's Dec-1996 warning (Fama, 2014, p. 1476)", "1996-12", "2003-12"),
  fama_test("Comparison: GSADF dot-com origination (Phillips et al., 2015, p. 1066)", "1995-11", "2003-12"),
  purrr::map_dfr(seq_len(nrow(episodes)), function(i) {
    e <- episodes[i, ]
    bind_rows(
      fama_test(paste0("BIS episode ", i, ": first warning"), e$first_warning,
                add_months(e$first_warning, HORIZON_MONTHS)),
      fama_test(paste0("BIS episode ", i, ": peak"), e$peak,
                add_months(e$peak, HORIZON_MONTHS))
    )
  })
)

print(fama_results, width = Inf)
readr::write_csv(fama_results, file.path(DIR_OUT, "fama_forecast_test.csv"))
message("  wrote ", file.path(DIR_OUT, "fama_forecast_test.csv"))
