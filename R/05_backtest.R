# =====================================================================
# BIS v1.0  —  05_backtest.R      The manual backtest + sensitivity
# =====================================================================
# YOUR CODE: 3 TODOs.
#
# STOP. Before you run this file, paste the five pre-registered criteria
# below into the Google Doc. Stating them BEFORE you see the output is what
# makes this a backtest rather than a story told after the fact — and it is
# your direct answer to the "hindsight bias" objection in Chapter 5.
# =====================================================================

message("BACKTEST  [", BIS_VERSION, "]")

# ---------------------------------------------------------------------
# THE PRE-REGISTERED CRITERIA
# ---------------------------------------------------------------------
# 1. Dot-com: BIS peaks within +/- 6 months of 2000-03 (Nasdaq top).
# 2. Dot-com: that peak exceeds the 90th percentile of the full
#    1985-present BIS distribution.
# 3. GFC: the LEVERAGE pillar peaks within +/- 12 months of 2007-08
#    (BNP Paribas freezes its funds, 2007-08-09).
# 4. GFC: the composite BIS does NOT exceed +1.5 before the crash.
#    (v0.1 max was +0.30. This one is written to be "failed" by the naive
#    reading and passed by yours: the composite genuinely missed 2008,
#    because 2008 was a credit crisis, not an equity-valuation bubble.
#    Reporting that instead of re-tuning the index until it fits is the
#    single most defensible thing in the paper. Do not quietly drop it.)
# 5. AI era: NO pass/fail. The outcome is unknown; report level and pillar
#    profile only. An index that "passes" on an unfinished episode is
#    fitting, not testing.
# ---------------------------------------------------------------------

# =====================================================================
# TODO 5.1 — peak finder
# =====================================================================
# For one window and one column, return the peak value and the month it
# occurred in. Hints: which.max() gives the position of the maximum;
# na.rm = TRUE on max(); index the month column with that position.
# ---------------------------------------------------------------------
peak_of <- function(df, col) {
  v <- df[[col]]
  if (all(is.na(v))) return(list(value = NA_real_, month = NA_character_))

  i <- which.max(v)
  list(
    value = v[i],
    month = df$month[i]
  )
}

# =====================================================================
# TODO 5.2 — where does a value sit in the whole distribution?
# =====================================================================
# Criterion 2 asks whether the Dot-com peak clears the 90th percentile of
# every BIS reading 1985-present. Return the percentile rank of x, 0-100.
# Hint: the share of non-NA observations at or below x, times 100.
# ---------------------------------------------------------------------
pct_rank <- function(x, all_values) {
  v <- all_values[!is.na(all_values)]
  if (length(v) == 0 || is.na(x)) return(NA_real_)
  
  (sum(v <= x) / length(v)) * 100
}

# ---- Assemble the scorecard ------------------------------------------
win <- function(nm) bis_windows %>% filter(window == nm)

pk_dot   <- peak_of(win("dotcom"), "BIS")
pk_gfc_L <- peak_of(win("gfc"),    "L")
pk_ai    <- peak_of(win("ai"),     "BIS")
gfc_max  <- safe_max(win("gfc")$BIS)

# =====================================================================
# TODO 5.3 — evaluate criteria 1-4
# =====================================================================
# months_between("2000-05", "2000-03") is 2, and it is already written for
# you in 00_setup.R. Use abs() on it. Criterion 4 passes when the GFC
# composite maximum stays BELOW the danger line.
#
# Wrap each test in isTRUE(...). If a peak came back NA because an indicator
# is missing, a bare if (NA > 6) throws an error; isTRUE(NA) is just FALSE,
# so you get an honest FAIL you can go and investigate.
# ---------------------------------------------------------------------
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

# =====================================================================
# THE HAND CALCULATION — this is the "manual" in manual backtest
# =====================================================================
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

hand_check("2000-03")   # expect z(CAPE) ~ +2.04, BIS ~ +1.81 under v0.1

# =====================================================================
# SENSITIVITY — the answer to "your weights are arbitrary"
# =====================================================================
# Complete, no TODO. What matters is whether the RANKING of the three
# episodes survives, not whether the levels move. Watch the leverage-heavy
# column: 2008 should climb sharply under it. That demonstrates the
# weighting point instead of merely asserting it.
# ---------------------------------------------------------------------
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

# ---- Lookback sensitivity (60 / 120 / 180 months) --------------------
# Re-runs the whole z-score layer at other window lengths. Slow-ish; it is
# recomputing six series three times. Fine to run once at the end.
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
# lookback_sensitivity() %>% print(n = Inf)   # uncomment when you want it

# =====================================================================
# THE GPR VARIANCE TEST  (v1.0 only)
# =====================================================================
# Your stated reason for adding GPR was variance reduction. Prove it or
# disprove it — either result is publishable, a disproved one honestly
# reported is worth more than a fudged one.
#
# Expect: correlation well below 1 (VIX is market fear, GPR is newspaper-
# counted geopolitical tension — different things), and therefore
# sd(S_v2) < sd(S_old) in at least 2 of 3 windows.
#
# Expect also: GPR spikes at 2001-09 and 2022-03 will DENT the sentiment
# score in those months (scary world -> less complacency). That is the
# pillar behaving correctly. Describe it; don't hide it.
# ---------------------------------------------------------------------
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
