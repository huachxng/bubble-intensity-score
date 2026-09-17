# =====================================================================
# BIS v1.0  —  03_pillars.R      *** REFERENCE SOLUTION ***
# Same file as R/03_pillars.R with the 3 TODOs filled in.
# =====================================================================

message("LAYERS 2-4 - panel, z-scores, pillars  [", BIS_VERSION, ", solution]")

read_clean <- function(f) {
  readr::read_csv(file.path(DIR_CLEAN, f), show_col_types = FALSE,
                  progress = FALSE, col_types = readr::cols(
                    month = readr::col_character(), value = readr::col_double()))
}

ind_v1 <- read_clean("v1_cape.csv")
ind_l1 <- read_clean("l1_corpdebt_yoy.csv")
ind_l2 <- read_clean("l2_nfci_leverage.csv")
ind_s1 <- read_clean("s1_vix.csv")
ind_s2 <- read_clean("s2_gpr.csv")
ind_c1 <- read_clean("c1_nasdaq_sp.csv")

# ---- SOLUTION 3.1: trailing z-score, POPULATION sd -------------------
trailing_z <- function(x, window = Z_WINDOW, min_obs = Z_MIN_OBS) {
  n   <- length(x)
  out <- rep(NA_real_, n)
  for (t in seq_len(n)) {
    xs <- x[max(1, t - window + 1):t]
    xs <- xs[!is.na(xs)]
    if (length(xs) >= min_obs) {
      mu     <- mean(xs)
      sd_pop <- sqrt(mean((xs - mu)^2))       # ddof = 0, matching pandas
      out[t] <- (x[t] - mu) / sd_pop
    }
  }
  out
}

local({
  got <- tail(trailing_z(1:40), 1)
  exp <- (40 - mean(1:40)) / sqrt(mean((1:40 - mean(1:40))^2))
  if (isTRUE(all.equal(got, exp, tolerance = 1e-9))) {
    message("  trailing_z self-test PASSED (", round(got, 5), ")")
  } else {
    warning("  trailing_z self-test FAILED", call. = FALSE)
  }
})

# ---- z-score at NATIVE length, then join -----------------------------
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

last_month <- max(c(z_v1$month, z_s1$month, z_c1$month))

panel <- tibble(month = month_seq(PANEL_START, last_month)) %>%
  left_join(z_v1, by = "month") %>%
  left_join(z_l1, by = "month") %>%
  left_join(z_l2, by = "month") %>%
  left_join(z_s1, by = "month") %>%
  left_join(z_s2, by = "month") %>%
  left_join(z_c1, by = "month")

# ---- SOLUTION 3.2: sign conventions ----------------------------------
panel <- panel %>%
  mutate(
    s_vix_adj  = -z_vix,                 # calm markets = complacency = higher
    s_gpr_adj  = -z_gpr,                 # quiet world  = complacency = higher
    l_nfci_adj = NFCI_SIGN * z_nfci      # sign decided empirically in 02
  )

# ---- SOLUTION 3.3: the four pillars ----------------------------------
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
    S = rowMeans(cbind(s_vix_adj,  s_gpr_adj),  na.rm = TRUE),
    C = z_ndqsp
  )

}

# rowMeans on an all-NA row returns NaN; normalise those back to NA.
panel <- panel %>% mutate(across(c(V, L, S, C), ~ if_else(is.nan(.x), NA_real_, .x)))
