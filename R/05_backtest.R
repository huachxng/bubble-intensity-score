# 05_backtest.R: the pre-registered backtest scorecard, a hand check of one
# month, and the weighting, lookback and GPR sensitivity tests.

message("\n== 05 backtest and sensitivity (", BIS_VERSION, ")")

# ---- Pre-registered criteria ----------------------------------------------------
# 1. Dot-com: the BIS peaks within +/- 6 months of 2000-03 (Nasdaq top).
# 2. Dot-com: that peak is above the 90th percentile of all BIS readings
#    from 1985 on.
# 3. GFC: the leverage pillar peaks within +/- 12 months of 2007-08
#    (BNP Paribas freezes its funds, 2007-08-09).
# 4. GFC: the composite BIS stays below +1.5 before the crash.
# 5. AI era: reported, not scored; the outcome is not known.

# Peak value of one column in a window, and its month.
peak_of <- function(df, col) {
  v <- df[[col]]
  if (all(is.na(v))) return(list(value = NA_real_, month = NA_character_))

  i <- which.max(v)
  list(
    value = v[i],
    month = df$month[i]
  )
}

# Percentile rank (0-100) of x among the non-missing values.
pct_rank <- function(x, all_values) {
  v <- all_values[!is.na(all_values)]
  if (length(v) == 0 || is.na(x)) return(NA_real_)

  (sum(v <= x) / length(v)) * 100
}

# ---- Scorecard ----------------------------------------------------------------------
win <- function(nm) bis_windows %>% filter(window == nm)

pk_dot   <- peak_of(win("dotcom"), "BIS")
pk_gfc_L <- peak_of(win("gfc"),    "L")
pk_ai    <- peak_of(win("ai"),     "BIS")
gfc_max  <- safe_max(win("gfc")$BIS)

# isTRUE() turns a missing value into FAIL instead of an error.
scorecard <- tibble::tribble(
  ~criterion, ~target, ~observed, ~result,

  "1. Dot-com BIS peak within +/-6m of 2000-03",
  "|lead/lag| <= 6",
  paste0(pk_dot$month, " (", sprintf("%+0.2f", pk_dot$value), ")"),
  if_else(isTRUE(abs(months_between(pk_dot$month, "2000-03")) <= 6), "PASS", "FAIL"),

  "2. Dot-com peak above 90th percentile",
  ">= 90",
  paste0(round(pct_rank(pk_dot$value, panel$BIS), 1), "th"),
  if_else(isTRUE(pct_rank(pk_dot$value, panel$BIS) >= 90), "PASS", "FAIL"),

  "3. GFC leverage pillar peaks within +/-12m of 2007-08",
  "|lead/lag| <= 12",
  paste0(pk_gfc_L$month, " (", sprintf("%+0.2f", pk_gfc_L$value), ")"),
  if_else(isTRUE(abs(months_between(pk_gfc_L$month, "2007-08")) <= 12), "PASS", "FAIL"),

  "4. GFC composite stays below +1.5 pre-crash",
  "< 1.5",
  sprintf("%+0.2f", gfc_max),
  if_else(isTRUE(gfc_max < 1.5), "PASS", "FAIL"),

  "5. AI era (reported, not scored)",
  "-",
  paste0(pk_ai$month, " (", sprintf("%+0.2f", pk_ai$value), ")"),
  "n/a"
)

print(scorecard, n = Inf, width = Inf)
readr::write_csv(scorecard, file.path(DIR_OUT, "backtest_scorecard.csv"))

# ---- Hand check ----------------------------------------------------------------------
# Prints, for month m, the number of values in the trailing window, their mean
# and population SD and the resulting z-score, then the four pillars and
# their average, so a reading can be redone on a calculator.
hand_check <- function(m, series = ind_v1, label = "CAPE") {
  i  <- which(series$month == m)
  if (!length(i)) stop("month not in series: ", m)
  xs <- series$value[max(1, i - Z_WINDOW + 1):i]
  xs <- xs[!is.na(xs)]
  mu <- mean(xs); sd_pop <- sqrt(mean((xs - mu)^2))

  cat("\n--- hand check:", label, "at", m, "---\n")
  cat("  months in trailing window :", length(xs), "\n")
  cat("  value this month          :", sprintf("%.4f", series$value[i]), "\n")
  cat("  mean of window            :", sprintf("%.4f", mu), "\n")
  cat("  population sd of window   :", sprintf("%.4f", sd_pop), "\n")
  cat("  => z = (value - mean)/sd  :", sprintf("%+.4f", (series$value[i] - mu) / sd_pop), "\n")

  p <- panel %>% filter(month == m)
  if (nrow(p) == 1) {
    cat("\n  pillars this month: V", sprintf("%+.2f", p$V), " L", sprintf("%+.2f", p$L),
        " S", sprintf("%+.2f", p$S), " C", sprintf("%+.2f", p$C), "\n")
    cat("  by hand: (", paste(sprintf("%+.2f", c(p$V, p$L, p$S, p$C)), collapse = " "),
        ") / 4 =", sprintf("%+.4f", mean(c(p$V, p$L, p$S, p$C), na.rm = TRUE)), "\n")
    cat("  R says BIS =", sprintf("%+.4f", p$BIS), "\n")
  }
  invisible(NULL)
}

hand_check("2000-03")   # z(CAPE) = +2.04; BIS = +2.19 under v1.0, +1.80 under v0.1

# ---- Weighting sensitivity -----------------------------------------------------------
# Peak BIS in each window when one pillar gets weight 0.40 and the others 0.20.
WEIGHT_SETS <- list(
  equal            = c(V = .25, L = .25, S = .25, C = .25),
  valuation_heavy  = c(V = .40, L = .20, S = .20, C = .20),
  leverage_heavy   = c(V = .20, L = .40, S = .20, C = .20),
  sentiment_heavy  = c(V = .20, L = .20, S = .40, C = .20)
)

weighted_bis <- function(df, w) {
  m  <- as.matrix(df[, c("V", "L", "S", "C")])
  wm <- matrix(w[c("V", "L", "S", "C")], nrow = nrow(m), ncol = 4, byrow = TRUE)
  ok <- !is.na(m)
  num <- rowSums(ifelse(ok, m * wm, 0))   # only pillars that exist contribute
  den <- rowSums(ifelse(ok, wm,     0))   # and their weights are renormalised
  ifelse(den > 0, num / den, NA_real_)
}

sensitivity <- purrr::map_dfr(names(WEIGHT_SETS), function(nm) {
  w <- WEIGHT_SETS[[nm]]
  purrr::map_dfr(WINDOWS$name, function(win_nm) {
    d <- win(win_nm)
    d$b <- weighted_bis(d, w)
    tibble(weighting = nm, window = win_nm,
           peak = round(safe_max(d$b), 2),
           peak_month = which_max_month(d$month, d$b))
  })
})

cat("\n--- sensitivity: peak BIS by weighting ---\n")
sensitivity %>%
  select(weighting, window, peak) %>%
  pivot_wider(names_from = window, values_from = peak) %>%
  print()

readr::write_csv(sensitivity, file.path(DIR_OUT, "sensitivity.csv"))

# ---- Lookback sensitivity (Table B2) ---------------------------------------------------
# The one-indicator-per-pillar (v0.1) composite recomputed with 60-, 120- and
# 180-month z-score windows; peak of each analysis window.
lookback_sensitivity <- function(lookbacks = c(60, 120, 180)) {
  purrr::map_dfr(lookbacks, function(L) {
    zL <- function(df) { df %>% arrange(month) %>%
        transmute(month, z = trailing_z(value, window = L)) }
    p <- tibble(month = month_seq(PANEL_START, max(panel$month))) %>%
      left_join(zL(ind_v1) %>% rename(V = z), by = "month") %>%
      left_join(zL(ind_l1) %>% rename(L = z), by = "month") %>%
      left_join(zL(ind_s1) %>% rename(S = z), by = "month") %>%
      left_join(zL(ind_c1) %>% rename(C = z), by = "month") %>%
      mutate(S = -S, BIS = rowMeans(cbind(V, L, S, C), na.rm = TRUE))
    purrr::map_dfr(WINDOWS$name, function(nm) {
      w  <- WINDOWS %>% filter(name == nm)
      hi <- if (is.na(w$end)) max(p$month) else w$end
      d  <- p %>% filter(month >= w$start, month <= hi)
      tibble(lookback = L, window = nm,
             peak = round(safe_max(d$BIS), 2),
             peak_month = which_max_month(d$month, d$BIS))
    })
  })
}

lookback <- lookback_sensitivity()
cat("\n--- lookback sensitivity (v0.1 pillars) ---\n")
print(lookback, n = Inf)
readr::write_csv(lookback, file.path(DIR_OUT, "lookback_sensitivity.csv"))

# ---- GPR variance test (v1.0 only) --------------------------------------------------------
# In each window: SD of the sentiment pillar with the VIX alone (S_old) and
# with VIX and GPR averaged (S_v2), and the correlation of the two flipped
# indicators.
if (BIS_VERSION != "v0.1") {
  gpr_test <- purrr::map_dfr(WINDOWS$name, function(nm) {
    w  <- WINDOWS %>% filter(name == nm)
    hi <- if (is.na(w$end)) max(panel$month) else w$end
    d  <- panel %>% filter(month >= w$start, month <= hi)
    tibble(
      window   = nm,
      sd_S_old = round(sd(d$s_vix_adj, na.rm = TRUE), 3),
      sd_S_v2  = round(sd(d$S,         na.rm = TRUE), 3),
      rho      = round(safe_cor(d$s_vix_adj, d$s_gpr_adj), 3),
      peak_month = which_max_month(d$month, d$BIS)
    )
  }) %>% mutate(variance_reduced = sd_S_v2 < sd_S_old)

  cat("\n--- GPR variance test ---\n"); print(gpr_test)
  readr::write_csv(gpr_test, file.path(DIR_OUT, "gpr_variance_test.csv"))
}
