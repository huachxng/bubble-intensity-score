# 02_build_panel.R: turn the raw downloads into six monthly series of the same
# shape, month (chr) and value (dbl), in latest/clean/ (refresh run only).

if (!REFRESH) {
  stop("02_build_panel.R rebuilds data-clean/ from new downloads and runs only ",
       "in a refresh run: Rscript run_all.R --refresh", call. = FALSE)
}

message("\n== 02 clean indicators into ", DIR_CLEAN, "/")

# Drop the month of retrieval: it is not over yet.
first_partial <- as.Date(format(RETRIEVED_ON, "%Y-%m-01"))
raw_vix   <- raw_vix   %>% filter(date < first_partial)
raw_ndq   <- raw_ndq   %>% filter(date < first_partial)
raw_nfci  <- raw_nfci  %>% filter(date < first_partial)
raw_shill <- raw_shill %>% filter(month < format(first_partial, "%Y-%m"))
raw_capeR <- raw_capeR %>% filter(month < format(first_partial, "%Y-%m"))
raw_gpr   <- raw_gpr   %>% filter(month < format(first_partial, "%Y-%m"))

# ---- V1: Shiller CAPE ---------------------------------------------------------
# The mirror is used wherever it has a value; multpl.com fills the rest.
v1_cape <- raw_shill %>%
  select(month, mirror = pe10) %>%
  full_join(raw_capeR %>% rename(recent = value), by = "month") %>%
  arrange(month) %>%
  mutate(
    value = coalesce(mirror, recent)
  ) %>%
  filter(!is.na(value)) %>%
  select(month, value)

write_clean(v1_cape, "v1_cape.csv", "V1 CAPE")

# ---- L2: Chicago Fed NFCI leverage subindex, weekly to monthly mean ------------
l2_nfci_leverage <- to_monthly_mean(raw_nfci)
write_clean(l2_nfci_leverage, "l2_nfci_leverage.csv", "L2 NFCI leverage")

# ---- S1: VIX, monthly mean of daily closes ---------------------------------------
s1_vix <- to_monthly_mean(raw_vix)
write_clean(s1_vix, "s1_vix.csv", "S1 VIX")

# ---- S2: Geopolitical Risk index, already monthly --------------------------------
s2_gpr <- raw_gpr
write_clean(s2_gpr, "s2_gpr.csv", "S2 GPR")

# ---- C1: Nasdaq Composite / S&P 500 ----------------------------------------------
# Monthly mean of the Nasdaq divided by the Shiller S&P 500 level for the same
# month; a proxy for concentration in technology stocks.
c1_nasdaq_sp <- to_monthly_mean(raw_ndq) %>%
  rename(nasdaq = value) %>%
  inner_join(raw_shill %>% select(month, sp500), by = "month") %>%
  mutate(
    value = nasdaq / sp500
  ) %>%
  filter(!is.na(value), is.finite(value)) %>%
  select(month, value)

write_clean(c1_nasdaq_sp, "c1_nasdaq_sp.csv", "C1 Nasdaq/S&P")

# ---- L1: nonfinancial corporate debt, year-over-year growth ----------------------
# The quarterly debt level becomes growth over the same quarter a year earlier
# (4 rows back).
l1_quarterly <- raw_debt %>%
  mutate(month = format(date, "%Y-%m")) %>%
  arrange(month) %>%
  mutate(
    yoy = value / lag(value, 4) - 1
  ) %>%
  filter(!is.na(yoy)) %>%
  select(month, value = yoy)

# Monthly grid up to the last month of the other monthly series, each quarter's
# value carried forward (as in the prototype). Z.1 is published about ten weeks
# after a quarter ends, so the latest months use the last published quarter.
data_end <- max(c(v1_cape$month, s1_vix$month, c1_nasdaq_sp$month))

l1_corpdebt_yoy <- tibble(
    month = month_seq(min(l1_quarterly$month), data_end)
  ) %>%
  left_join(l1_quarterly, by = "month") %>%
  fill(value, .direction = "down") %>%
  filter(!is.na(value))

write_clean(l1_corpdebt_yoy, "l1_corpdebt_yoy.csv", "L1 corp debt yoy")
