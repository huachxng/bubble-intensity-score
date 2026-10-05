# 04_bis.R: the composite index, the three analysis windows and the check
# against the v0.1 prototype's recorded values.

message("\n== 04 composite index (", BIS_VERSION, ")")

# BIS = equal-weighted mean of the four pillars. na.rm = TRUE matches the
# prototype (pandas skipna): a month with a missing pillar averages the others.
panel <- panel %>%
  mutate(
    BIS = rowMeans(cbind(V, L, S, C), na.rm = TRUE)
  ) %>%
  mutate(BIS = if_else(is.nan(BIS), NA_real_, BIS))

readr::write_csv(panel, file.path(DIR_OUT, "bis_panel_monthly.csv"))
message("  wrote ", file.path(DIR_OUT, "bis_panel_monthly.csv"), " (", nrow(panel), " months)")

# ---- The three analysis windows -------------------------------------------------------
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

# Descriptive statistics of each window (Section 4.5); sd_bis is the sample SD.
bis_windows %>%
  group_by(window) %>%
  summarise(n = n(),
            mean_bis = round(mean(BIS, na.rm = TRUE), 2),
            sd_bis   = round(sd(BIS, na.rm = TRUE), 2),
            min_bis  = round(-safe_max(-BIS), 2),
            min_at   = which_max_month(month, -BIS),
            peak     = round(safe_max(BIS), 2),
            peak_at  = which_max_month(month, BIS),
            latest   = round(last(BIS), 2), .groups = "drop") %>%
  print(width = Inf)

# ---- Signature month of the AI era ------------------------------------------------------
# The AI window is still open and has no peak to report, so it is read at the
# latest month in which all four pillars have a value and the sentiment pillar
# has both of its indicators (VIX and GPR; under v0.1 the VIX alone). The
# newest months are left out because some of their indicators are not
# published yet.
ai_complete <- panel %>%
  filter(month %in% bis_windows$month[bis_windows$window == "ai"],
         !is.na(V), !is.na(L), !is.na(S), !is.na(C),
         !is.na(s_vix_adj), BIS_VERSION == "v0.1" | !is.na(s_gpr_adj))
stopifnot("the AI window has no month with all four pillars and both sentiment indicators" =
            nrow(ai_complete) > 0)
ai_signature <- max(ai_complete$month)
message("  AI-era signature month: ", ai_signature)

# ---- Check against the v0.1 prototype -----------------------------------------------------
# The anchors were recorded from the one-indicator-per-pillar prototype, so
# they are compared with the v0.1 pillars under either BIS_VERSION. PASS =
# every recorded value within 0.011 (they were rounded to 2 decimals); CLOSE =
# within the row's close_band (see ANCHORS in 00_setup.R); otherwise FAIL.
v01 <- panel %>%
  transmute(month, V = z_cape, L = z_corpdebt, S = -z_vix, C = z_ndqsp) %>%
  mutate(BIS = rowMeans(cbind(V, L, S, C), na.rm = TRUE),
         BIS = if_else(is.nan(BIS), NA_real_, BIS))

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
      worst <= close_band  ~ "CLOSE",
      TRUE                 ~ "FAIL"
    ))

  cat("\n--- v0.1 anchor check ---\n")
  for (i in seq_len(nrow(cmp))) {
    r <- cmp[i, ]
    cat(sprintf("  %s  %-5s  recorded BIS %+0.2f  now %s  (largest difference %s)\n",
                r$month, r$result, r$BIS_want,
                if (is.na(r$BIS_got)) "  NA " else sprintf("%+0.2f", r$BIS_got),
                if (is.na(r$worst))   "  NA " else sprintf("%.3f", r$worst)))
  }
  n_pass  <- sum(cmp$result == "PASS")
  n_close <- sum(cmp$result == "CLOSE")
  cat(sprintf("  %d pass, %d close, %d fail (of %d anchors)\n", n_pass, n_close,
              nrow(cmp) - n_pass - n_close, nrow(cmp)))
  invisible(cmp)
}

anchor_report <- check_anchors(v01)
readr::write_csv(anchor_report, file.path(DIR_OUT, "anchor_check.csv"))
