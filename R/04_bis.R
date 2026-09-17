# =====================================================================
# BIS v1.0  —  04_bis.R      LAYER 5: the composite + the regression test
# =====================================================================
# YOUR CODE: 1 TODO — the equation the whole paper is named after.
# =====================================================================

message("LAYER 5 - composite  [", BIS_VERSION, "]")

# =====================================================================
# TODO 4.1 — the Bubble Intensity Score
# =====================================================================
#     BIS_t = (V + L + S + C) / 4        (equal weights, w = 1/4 each)
#
# One subtlety that matters: use na.rm = TRUE. v0.1 used pandas'
# skipna=True, so a month where one pillar has no data averages the three
# that do. Without it, a single missing pillar wipes out the whole month
# and your early years go blank.
#
# Equal weights are a DESIGN CHOICE, not laziness: optimised weights would
# be fitted to past crises, which is precisely the critique you level at
# Financial Conditions Indexes in Chapter 2. You test the choice in 06.
# Hint: rowMeans(cbind(...), na.rm = TRUE)
# ---------------------------------------------------------------------
panel <- panel %>%
  mutate(
    BIS = rowMeans(cbind(V, L, S, C), na.rm = TRUE)
  ) %>%
  mutate(BIS = if_else(is.nan(BIS), NA_real_, BIS))

# ---- Write the master panel ------------------------------------------
readr::write_csv(panel, file.path(DIR_OUT, "bis_panel_monthly.csv"))
message("  wrote output/bis_panel_monthly.csv  (", nrow(panel), " months)")

# ---- Slice the three windows -----------------------------------------
slice_window <- function(nm) {
  w  <- WINDOWS %>% filter(name == nm)
  hi <- if (is.na(w$end)) max(panel$month) else w$end
  panel %>%
    filter(month >= w$start, month <= hi) %>%
    select(month, V, L, S, C, BIS) %>%
    mutate(window = nm, .before = 1)
}

bis_windows <- purrr::map_dfr(WINDOWS$name, slice_window)
readr::write_csv(bis_windows, file.path(DIR_OUT, "bis_windows.csv"))

bis_windows %>%
  group_by(window) %>%
  summarise(n = n(),
            peak     = round(safe_max(BIS), 2),
            peak_at  = which_max_month(month, BIS),
            latest   = round(last(BIS), 2), .groups = "drop") %>%
  print()

# =====================================================================
# PHASE 4 — THE REGRESSION TEST
# =====================================================================
# Your R port must reproduce the Python prototype before you trust any new
# number it produces. Run with BIS_VERSION <- "v0.1" in 00_setup.R.
# Tolerance is 0.01 because the anchors were recorded to 2 decimals.
# ---------------------------------------------------------------------
check_anchors <- function(panel, tol = 0.011) {
  got <- panel %>% select(month, V, L, S, C, BIS) %>%
    filter(month %in% ANCHORS$month)

  cmp <- ANCHORS %>%
    left_join(got, by = "month", suffix = c("_want", "_got")) %>%
    rowwise() %>%
    mutate(worst = max(abs(c_across(ends_with("_want")) -
                           c_across(ends_with("_got"))), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(result = case_when(
      is.na(worst)         ~ "FAIL",
      worst <= tol         ~ "PASS",
      worst <= close_band  ~ "CLOSE",   # per-row band, defined with ANCHORS
      TRUE                 ~ "FAIL"
    ))

  cat("\n--- Phase 4 regression test vs v0.1 ---\n")
  for (i in seq_len(nrow(cmp))) {
    r <- cmp[i, ]
    cat(sprintf("  %s  %-4s  want BIS %+0.2f  got %s  (worst diff %s)\n",
                r$month, r$result, r$BIS_want,
                if (is.na(r$BIS_got)) "  NA " else sprintf("%+0.2f", r$BIS_got),
                if (is.na(r$worst))   "  NA " else sprintf("%.3f", r$worst)))
  }
  n_pass  <- sum(cmp$result == "PASS")
  n_close <- sum(cmp$result == "CLOSE")
  cat(sprintf("  --> %d exact, %d close, %d fail (of %d anchors)\n", n_pass, n_close,
              nrow(cmp) - n_pass - n_close, nrow(cmp)))
  cat("  CLOSE = within that row's close_band (see ANCHORS in 00_setup.R):\n",
      " upstream data has been revised since the anchors were recorded in\n",
      " June 2026 - multpl revises recent CAPE months, each Z.1 release\n",
      " revises debt history, and the 2026-05 row's L was filled from a\n",
      " quarter that has since been revised (-0.50 -> ~-0.05). Details in\n",
      " data-raw/PROVENANCE.md.\n",
      " PASS + CLOSE on all five = the port is faithful; a FAIL is a code\n",
      " bug until proven otherwise.\n\n")

  if (n_pass + n_close < nrow(cmp)) {
    cat("  Debug in this order (most common cause first):\n",
        "   1. sample vs POPULATION sd in trailing_z    (03, TODO 3.1)\n",
        "   2. joined into the panel BEFORE z-scoring   (03)\n",
        "   3. monthly LAST instead of monthly MEAN     (02, VIX/Nasdaq)\n",
        "   4. missing na.rm = TRUE on the composite    (04, TODO 4.1)\n",
        "   5. CAPE splice not reaching 2026            (01, multpl fallback)\n\n")
  }
  invisible(cmp)
}

anchor_report <- check_anchors(panel)
readr::write_csv(anchor_report, file.path(DIR_OUT, "anchor_check.csv"))

if (BIS_VERSION != "v0.1") {
  message("NOTE: anchors are defined for v0.1. Running them against ",
          BIS_VERSION, " is expected to differ on L and S — that is the upgrade.")
}
