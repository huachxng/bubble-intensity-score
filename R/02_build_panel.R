# =====================================================================
# BIS v1.0  —  02_build_panel.R      LAYER 1: one tidy file per indicator
# =====================================================================
# YOUR CODE: 4 TODOs, all data transformations.
#
# Goal of this file: take seven messy raw sources and turn them into SIX
# files that all have the exact same shape — month (chr) + value (dbl).
# That uniformity is the whole trick. Once every indicator looks the same,
# the pillar maths in 03 is the same three lines for all of them.
#
# Run line by line with Cmd+Enter. Look at the Environment pane after each.
# =====================================================================

message("LAYER 1 - clean indicators")

# ---------------------------------------------------------------------
# V1 — Shiller CAPE
# ---------------------------------------------------------------------
# Two sources: the GitHub mirror (long history, stops ~2023-09) and the
# multpl.com table (recent months only). The mirror is the authority where
# it exists; multpl only fills the tail.
# ---------------------------------------------------------------------
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

# ---------------------------------------------------------------------
# L1 — Nonfinancial corporate debt, year-over-year growth
# ---------------------------------------------------------------------
# BCNSDODNS is a quarterly LEVEL (billions of dollars). A level can't go in
# the index — it only ever rises. What matters for a bubble is how FAST it
# is growing, so convert to year-over-year growth first.
#
# Quarterly data means "one year ago" is 4 rows back, not 12.
#
# --- TODO 2.2a ------------------------------------------------------
# Compute yoy growth. Hint: dplyr::lag(value, 4) gives the value 4 rows up.
#   growth = this quarter / same quarter last year - 1
# ---------------------------------------------------------------------
l1_quarterly <- raw_debt %>%
  mutate(month = format(date, "%Y-%m")) %>%
  arrange(month) %>%
  mutate(
    yoy = value / lag(value, 4) - 1
  ) %>%
  filter(!is.na(yoy)) %>%
  select(month, value = yoy)

# --- TODO 2.2b ------------------------------------------------------
# The BIS is monthly but this series only has Jan/Apr/Jul/Oct. Expand it to
# every month and carry the last known value forward (Feb and Mar both take
# January's number). This matches what v0.1 did in Python.
# Hints: month_seq(from, to) builds the full month list for you;
#        left_join() then tidyr::fill(value, .direction = "down").
# ---------------------------------------------------------------------
# The month grid runs to the CURRENT month, not just to the last quarterly
# observation. Z.1 data arrives with a ~10-week lag, so the newest months
# always sit past the last release; v0.1 carried the last known value forward
# to the present, and the L pillar must do the same here or it goes NA for
# recent months and the composite quietly averages three pillars instead of four.
l1_corpdebt_yoy <- tibble(
    month = month_seq(min(l1_quarterly$month), format(Sys.Date(), "%Y-%m"))
  ) %>%
  left_join(l1_quarterly, by = "month") %>%
  fill(value, .direction = "down") %>%
  filter(!is.na(value))

write_clean(l1_corpdebt_yoy, "l1_corpdebt_yoy.csv", "L1 corp debt yoy")

# ---------------------------------------------------------------------
# L2 — Chicago Fed NFCI Leverage Subindex  (the v1.0 upgrade)
# ---------------------------------------------------------------------
# Weekly -> monthly mean. to_monthly_mean() is in 00_setup.R.
l2_nfci_leverage <- to_monthly_mean(raw_nfci)
write_clean(l2_nfci_leverage, "l2_nfci_leverage.csv", "L2 NFCI leverage")

# ---- SIGN CHECK — do this now, it decides a line in 03 ---------------
# A positive NFCI leverage reading means leverage is HIGHER than average.
# Confirm that empirically before you trust the sign:
#
#   l2_nfci_leverage %>% filter(month >= "2003-01", month <= "2010-12") %>%
#     ggplot(aes(month, value, group = 1)) + geom_line()
#
# It should RISE into 2007-2008. If it does -> no flip (higher = more
# bubbly). If it falls -> flip it in 03 and write down why.
# Record what you saw; it is one sentence in Chapter 4.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# S1 — VIX, monthly mean
# ---------------------------------------------------------------------
# MEAN of the daily closes, not the last close. v0.1 used FRED's monthly
# average; a last-value series will not reproduce the anchors.
s1_vix <- to_monthly_mean(raw_vix)
write_clean(s1_vix, "s1_vix.csv", "S1 VIX")

# ---------------------------------------------------------------------
# S2 — Geopolitical Risk index
# ---------------------------------------------------------------------
# Already monthly and already the right shape.
s2_gpr <- raw_gpr
write_clean(s2_gpr, "s2_gpr.csv", "S2 GPR")

# ---------------------------------------------------------------------
# C1 — Nasdaq / S&P 500 ratio  (concentration proxy)
# ---------------------------------------------------------------------
# When tech runs far ahead of the broad market, this ratio climbs. It is a
# proxy for true top-10 market-cap share, which needs data you don't have
# free access to — disclose that in Chapter 4.
#
# --- TODO 2.3 -------------------------------------------------------
# Divide the Nasdaq monthly mean by the S&P price for the SAME month.
# Join on month, keep only months where both exist.
# ---------------------------------------------------------------------
c1_nasdaq_sp <- to_monthly_mean(raw_ndq) %>%
  rename(nasdaq = value) %>%
  inner_join(raw_shill %>% select(month, sp500), by = "month") %>%
  mutate(
    value = nasdaq / sp500
  ) %>%
  filter(!is.na(value), is.finite(value)) %>%
  select(month, value)

write_clean(c1_nasdaq_sp, "c1_nasdaq_sp.csv", "C1 Nasdaq/S&P")

# ---- CHECKPOINT ------------------------------------------------------
# Six files now in data-clean/. write_clean() already checked the contract
# (right columns, right types, no duplicate months) and printed the row
# count and date range for each. Compare those against the coverage column
# of the data-inventory table in README.md. If a range starts later than it
# should, the problem is here — fix it before going to 03.
# ---------------------------------------------------------------------
