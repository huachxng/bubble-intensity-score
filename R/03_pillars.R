# 03_pillars.R: z-score each indicator, join them into a monthly panel from
# 1985-01 and build the four pillars V, L, S and C.

message("\n== 03 z-scores and pillars (", BIS_VERSION, ")")

read_clean <- function(f) {
  readr::read_csv(file.path(DIR_CLEAN, f), show_col_types = FALSE,
                  progress = FALSE, col_types = readr::cols(
                    month = readr::col_character(), value = readr::col_double()))
}

ind_v1 <- read_clean("v1_cape.csv")           # CAPE
ind_l1 <- read_clean("l1_corpdebt_yoy.csv")   # corporate debt, yoy growth
ind_l2 <- read_clean("l2_nfci_leverage.csv")  # NFCI leverage
ind_s1 <- read_clean("s1_vix.csv")            # VIX
ind_s2 <- read_clean("s2_gpr.csv")            # GPR
ind_c1 <- read_clean("c1_nasdaq_sp.csv")      # Nasdaq/S&P

# ---- Trailing z-score -----------------------------------------------------------
# Each month is scored against the Z_WINDOW months ending with it (itself
# included); NA until Z_MIN_OBS values exist. The SD is the population SD
# (divide by n), matching the pandas prototype (ddof = 0); R's sd() divides by n - 1.
trailing_z <- function(x, window = Z_WINDOW, min_obs = Z_MIN_OBS) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    xs <- x[max(1, t - window + 1):t]
    xs <- xs[!is.na(xs)]
    if (length(xs) >= min_obs) {
      mu <- mean(xs)
      sd_pop <- sqrt(mean((xs - mean(xs))^2))
      out[t] <- (x[t] - mu) / sd_pop
    }
  }
  out
}

# For 1..40 the last z-score is (40 - 20.5) / sqrt(133.25) = 1.689278 with the
# population SD (the sample SD would give 1.668028).
stopifnot("trailing_z() self-test failed: expected 1.689278 for 1..40" =
            isTRUE(all.equal(tail(trailing_z(1:40), 1), 19.5 / sqrt(133.25))))

# Each series is z-scored over its full history and only then joined. Joining
# first would cut CAPE's history (from 1871) at the 1985 panel start and shift
# every early z-score.
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

# ---- Monthly panel, one row per month ------------------------------------------------
last_month <- max(c(z_v1$month, z_s1$month, z_c1$month))

panel <- tibble(month = month_seq(PANEL_START, last_month)) %>%
  left_join(z_v1, by = "month") %>%
  left_join(z_l1, by = "month") %>%
  left_join(z_l2, by = "month") %>%
  left_join(z_s1, by = "month") %>%
  left_join(z_s2, by = "month") %>%
  left_join(z_c1, by = "month")

# ---- Signs: higher = more bubbly for every indicator ------------------------------
# VIX and GPR are flipped: calm markets and a quiet world read as complacency.
panel <- panel %>%
  mutate(
    s_vix_adj  = -z_vix,
    s_gpr_adj  = -z_gpr,
    l_nfci_adj = z_nfci * NFCI_SIGN
  )

# ---- Pillars ----------------------------------------------------------------------------
# A pillar is the mean of its indicators that have a value that month
# (na.rm = TRUE), so it still reports when one of two is missing.
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

# rowMeans() of a row with no values is NaN; store it as NA.
panel <- panel %>% mutate(across(c(V, L, S, C), ~ if_else(is.nan(.x), NA_real_, .x)))
