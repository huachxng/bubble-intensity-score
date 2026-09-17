# =====================================================================
# BIS v1.0  —  03_pillars.R      LAYERS 2-4: panel, z-scores, pillars
# =====================================================================
# YOUR CODE: 3 TODOs. This is the statistical heart of the whole paper.
# If you can write and explain these three blocks, you can defend the BIS
# at a whiteboard — which is the actual point of doing it in R yourself.
# =====================================================================

message("LAYERS 2-4 - panel, z-scores, pillars  [", BIS_VERSION, "]")

read_clean <- function(f) {
  readr::read_csv(file.path(DIR_CLEAN, f), show_col_types = FALSE,
                  progress = FALSE, col_types = readr::cols(
                    month = readr::col_character(), value = readr::col_double()))
}

ind_v1 <- read_clean("v1_cape.csv")           # CAPE
ind_l1 <- read_clean("l1_corpdebt_yoy.csv")   # corp debt yoy
ind_l2 <- read_clean("l2_nfci_leverage.csv")  # NFCI leverage
ind_s1 <- read_clean("s1_vix.csv")            # VIX
ind_s2 <- read_clean("s2_gpr.csv")            # GPR
ind_c1 <- read_clean("c1_nasdaq_sp.csv")      # Nasdaq/S&P

# =====================================================================
# TODO 3.1 — the trailing z-score
# =====================================================================
# For each month t: take the trailing Z_WINDOW (120) months INCLUDING t,
# and express this month as "how many standard deviations from the average
# of its own past decade". Below 36 observations of history, return NA.
#
#     z_t = (x_t - mean(window)) / sd(window)
#
#   R's sd() is the SAMPLE standard deviation (divides by n-1).
#   Python/pandas used the POPULATION one (divides by n) via ddof=0.
#   You must use the population version:
#       sd_pop <- sqrt(mean((xs - mean(xs))^2))
#
# ---------------------------------------------------------------------
trailing_z <- function(x, window = Z_WINDOW, min_obs = Z_MIN_OBS) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    xs <- x[max(1, t - window + 1):t]
    xs <- xs[!is.na(xs)]
    if (length(xs) >= min_obs) {

      # ---- YOUR CODE HERE (2 lines) ----
      # 1. mu and sd_pop from xs
      # 2. out[t] <- (this month's value - mu) / sd_pop
      mu <- mean(xs)
      sd_pop <- sqrt(mean((xs - mean(xs))^2))
      out[t] <- (x[t] - mu) / sd_pop
      
    }
  }
  out
}

# ---- SELF-TEST: run this before going further ------------------------
# Feed it 1, 2, ... 40. Mean = 20.5, population sd = 11.543396, so the
# last value scores (40 - 20.5) / 11.543396 = 1.689278.
#
# This test is built to catch the sd trap specifically: with R's sd() you
# would get 1.668028 instead. If you see that number, that's your bug.
local({
  got <- tail(trailing_z(1:40), 1)
  if (isTRUE(all.equal(got, 1.689278, tolerance = 1e-5))) {
    message("  trailing_z self-test PASSED")
  } else if (isTRUE(all.equal(got, 1.668028, tolerance = 1e-5))) {
    warning("  trailing_z self-test FAILED: got 1.668028. That is the ",
            "SAMPLE sd. Use sqrt(mean((xs - mean(xs))^2)) instead of sd().",
            call. = FALSE)
  } else {
    warning("  trailing_z self-test FAILED: got ", round(got, 6),
            ", expected 1.689278.", call. = FALSE)
  }
})

# =====================================================================
# ORDER OF OPERATIONS — read this, it is the #1 way the port goes wrong
# =====================================================================
# z-score each series at its OWN FULL LENGTH first, and only THEN join
# into the panel. Do NOT join first and z-score the panel columns.
#
# Why: CAPE runs from 1871 and VIX from 1990. The panel starts 1985. If you
# join first, CAPE's trailing window gets truncated at 1985 and every early
# z-score shifts — your anchors will be close but wrong, which is the worst
# kind of wrong. v0.1 z-scored full-history series and sliced at the end.
# ---------------------------------------------------------------------
zt <- function(df, name) {
  df %>% arrange(month) %>%
    transmute(month, !!name := trailing_z(value))
}

z_v1 <- zt(ind_v1, "z_cape")
z_l1 <- zt(ind_l1, "z_corpdebt")
z_l2 <- zt(ind_l2, "z_nfci")
z_s1 <- zt(ind_s1, "z_vix")
z_s2 <- zt(ind_s2, "z_gpr")
z_c1 <- zt(ind_c1, "z_ndqsp")

# ---- LAYER 2: the master panel, one row per month --------------------
last_month <- max(c(z_v1$month, z_s1$month, z_c1$month))

panel <- tibble(month = month_seq(PANEL_START, last_month)) %>%
  left_join(z_v1, by = "month") %>%
  left_join(z_l1, by = "month") %>%
  left_join(z_l2, by = "month") %>%
  left_join(z_s1, by = "month") %>%
  left_join(z_s2, by = "month") %>%
  left_join(z_c1, by = "month")

# =====================================================================
# TODO 3.2 — sign conventions
# =====================================================================
# Every pillar must point the same way: HIGHER = MORE BUBBLY.
# Two of the six indicators naturally point the other way.
#
#   VIX  high = fear      -> bubbles inflate in CALM markets, so flip it
#   GPR  high = scary world -> a quiet world is what complacency looks
#                              like, so flip it too
#
# If you left GPR un-flipped, your sentiment pillar would be mixing "calm
# markets" with "scary world" and would mean nothing. Say that explicitly
# in your methods section — reviewers look for it.
#
# NFCI: you decided this empirically in 02. NFCI_SIGN is set in 00_setup.R.
# ---------------------------------------------------------------------
panel <- panel %>%
  mutate(
    s_vix_adj  = -z_vix,
    s_gpr_adj  = -z_gpr,
    l_nfci_adj = z_nfci * NFCI_SIGN
  )

# =====================================================================
# TODO 3.3 — the four pillars
# =====================================================================
# A pillar is the MEAN of its member indicators. Where a pillar has one
# indicator it is just that indicator.
#
#   v0.1 (build and validate this first):
#       V = z_cape      L = z_corpdebt      S = flipped VIX     C = z_ndqsp
#
#   v1.0 (after the regression test passes):
#       V = z_cape
#       L = mean(z_corpdebt, l_nfci_adj)
#       S = mean(s_vix_adj,  s_gpr_adj)
#       C = z_ndqsp
#
# Use na.rm = TRUE inside the pillar means, so a pillar still reports when
# only one of its two indicators exists that month (GPR before 1985, NFCI
# gaps). Disclose that rule in Chapter 4.
# Hint for a two-column row mean: rowMeans(cbind(a, b), na.rm = TRUE)
# ---------------------------------------------------------------------
if (BIS_VERSION == "v0.1") {
  
  panel <- panel %>% mutate(
    V = z_cape,
    L = z_corpdebt,
    S = s_vix_adj,
    C = z_ndqsp
  )
  
} else {
  
  panel <- panel %>% mutate(
    V = z_cape,
    L = rowMeans(cbind(z_corpdebt, l_nfci_adj), na.rm = TRUE),
    S = rowMeans(cbind(s_vix_adj, s_gpr_adj), na.rm = TRUE),
    C = z_ndqsp
  )
  
}

# ---- CHECKPOINT ------------------------------------------------------
# panel %>% filter(month == "2000-03") %>% select(V, L, S, C)
#   v0.1 must give roughly  V 2.04 | L 0.89 | S -0.74 | C 5.06
# Nothing downstream is meaningful until that line is right.
# ---------------------------------------------------------------------
