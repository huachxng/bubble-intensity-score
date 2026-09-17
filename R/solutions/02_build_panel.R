# =====================================================================
# BIS v1.0  —  02_build_panel.R      *** REFERENCE SOLUTION ***
# Same file as R/02_build_panel.R with the 4 TODOs filled in.
# =====================================================================

message("LAYER 1 - clean indicators  [solution]")

# ---- V1: Shiller CAPE, mirror preferred, multpl fills the tail -------
v1_cape <- raw_shill %>%
  select(month, mirror = pe10) %>%
  full_join(raw_capeR %>% rename(recent = value), by = "month") %>%
  arrange(month) %>%
  mutate(
    value = coalesce(mirror, recent)          # SOLUTION 2.1
  ) %>%
  filter(!is.na(value)) %>%
  select(month, value)

write_clean(v1_cape, "v1_cape.csv", "V1 CAPE")

# ---- L1: corporate debt, yoy growth, quarterly -> monthly ------------
l1_quarterly <- raw_debt %>%
  mutate(month = format(date, "%Y-%m")) %>%
  arrange(month) %>%
  mutate(
    yoy = value / lag(value, 4) - 1           # SOLUTION 2.2a  (4 quarters = 1 year)
  ) %>%
  filter(!is.na(yoy)) %>%
  select(month, value = yoy)

# Grid runs to the current month: the last quarterly value carries forward
# to the present, matching v0.1 (else L goes NA for the newest months).
l1_corpdebt_yoy <- tibble(
    month = month_seq(min(l1_quarterly$month), format(Sys.Date(), "%Y-%m"))
  ) %>%
  left_join(l1_quarterly, by = "month") %>%
  tidyr::fill(value, .direction = "down") %>%  # SOLUTION 2.2b  (= pandas ffill)
  filter(!is.na(value))

write_clean(l1_corpdebt_yoy, "l1_corpdebt_yoy.csv", "L1 corp debt yoy")

# ---- L2: NFCI leverage, weekly -> monthly mean -----------------------
l2_nfci_leverage <- to_monthly_mean(raw_nfci)
write_clean(l2_nfci_leverage, "l2_nfci_leverage.csv", "L2 NFCI leverage")

# ---- S1: VIX, monthly mean -------------------------------------------
s1_vix <- to_monthly_mean(raw_vix)
write_clean(s1_vix, "s1_vix.csv", "S1 VIX")

# ---- S2: GPR, already monthly ----------------------------------------
s2_gpr <- raw_gpr
write_clean(s2_gpr, "s2_gpr.csv", "S2 GPR")

# ---- C1: Nasdaq / S&P ratio ------------------------------------------
c1_nasdaq_sp <- to_monthly_mean(raw_ndq) %>%
  rename(nasdaq = value) %>%
  inner_join(raw_shill %>% select(month, sp500), by = "month") %>%
  mutate(
    value = nasdaq / sp500                    # SOLUTION 2.3
  ) %>%
  filter(!is.na(value), is.finite(value)) %>%
  select(month, value)

write_clean(c1_nasdaq_sp, "c1_nasdaq_sp.csv", "C1 Nasdaq/S&P")
