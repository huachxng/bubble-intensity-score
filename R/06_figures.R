# 06_figures.R: the paper's figures, written to output/figures/ at 160 dpi,
# and the three-way comparison table.

message("\n== 06 figures (", BIS_VERSION, ")")

as_date <- function(m) as.Date(paste0(m, "-01"))
month_label <- function(m) paste(month.abb[as.integer(substr(m, 6, 7))], substr(m, 1, 4))

PAL <- c(V = "#c1121f", L = "#0b3954", S = "#7209b7", C = "#f77f00")
THEME <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey35"),
        plot.caption  = element_text(colour = "grey45", hjust = 0))

save_fig <- function(p, name, w = 10, h = 5.5) {
  path <- file.path(DIR_FIG, name)
  ggsave(path, p, width = w, height = h, dpi = 160, bg = "white")
  message("  wrote ", path)
}

# Shaded bands of Figure 1: the three analysis windows, read from WINDOWS.
EPISODES <- tibble(
  label = c("Dot-com", "GFC", "AI era"),
  xmin  = as_date(WINDOWS$start),
  xmax  = as_date(coalesce(WINDOWS$end, max(panel$month)))
)

# The 2020 warning (the months of 2020 at or above the danger line) lies
# between the GFC and AI windows. Its peak is labelled on Figure 1 so that it
# is not read as part of the AI era. The v0.1 index has no such month and
# gets no label (adding NULL to a plot changes nothing).
pk_2020 <- panel %>%
  filter(substr(month, 1, 4) == "2020", BIS >= DANGER_LINE) %>%
  slice_max(BIS, n = 1, with_ties = FALSE)

label_2020 <- NULL
if (nrow(pk_2020) == 1) {
  label_2020 <- annotate("text", x = as_date(pk_2020$month), y = pk_2020$BIS + .12,
                         label = paste0("pandemic-era peak, ", month_label(pk_2020$month)),
                         hjust = 1, size = 3.1, colour = "grey35")
}

# ---- Figure 1: the BIS since 1995 ----------------------------------------------
p1 <- panel %>%
  filter(month >= "1995-01", !is.na(BIS)) %>%
  mutate(d = as_date(month)) %>%
  ggplot(aes(d, BIS)) +
  geom_rect(data = EPISODES, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey85", alpha = .35) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
  geom_hline(yintercept = DANGER_LINE, linetype = "dashed", colour = PAL["V"]) +
  geom_line(colour = PAL["L"], linewidth = .7) +
  annotate("text", x = as_date("1995-06"), y = DANGER_LINE + .12,
           label = paste0("danger line +", DANGER_LINE),
           hjust = 0, size = 3.1, colour = PAL["V"]) +
  label_2020 +
  labs(title = "Bubble Intensity Score, 1995-present",
       subtitle = paste0(BIS_VERSION, " - equal-weighted mean of four trailing ",
                         Z_WINDOW, "-month pillar z-scores"),
       x = NULL, y = "BIS (standard deviations)",
       caption = "Shaded: the three analysis windows. Sources: Shiller, FRED, Caldara & Iacoviello (2022).") +
  THEME
save_fig(p1, "fig_bis_history.png")

# ---- Figures 2a-2c: pillar decomposition, one file per window ----------------
for (nm in WINDOWS$name) {
  d <- bis_windows %>% filter(window == nm)
  p <- d %>%
    select(month, V, L, S, C) %>%
    pivot_longer(-month, names_to = "pillar", values_to = "z") %>%
    mutate(d = as_date(month)) %>%
    ggplot(aes(d, z, colour = pillar)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
    geom_line(linewidth = .65, na.rm = TRUE) +   # a pillar can be missing in the newest month
    geom_line(data = d %>% mutate(d = as_date(month)),
              aes(d, BIS), inherit.aes = FALSE,
              colour = "black", linewidth = .9, linetype = "solid") +
    scale_colour_manual(values = PAL, name = "pillar") +
    labs(title = paste0("Pillar decomposition - ", nm, " window"),
         subtitle = "Black line = the composite BIS. Read the pillars, not just the headline number.",
         x = NULL, y = "z-score") +
    THEME
  save_fig(p, paste0("fig_pillars_", nm, ".png"))
}

# ---- Figure 3: the three-way comparison ----------------------------------------
# Signature month: the BIS peak for Dot-com, the leverage pillar's peak for
# the GFC, and for the AI era the latest month with all four pillars and both
# sentiment indicators (ai_signature, from 04_bis.R).
signature_month <- function(nm) {
  if (nm == "ai") return(ai_signature)
  d <- bis_windows %>% filter(window == nm)
  if (nm == "gfc") which_max_month(d$month, d$L) else which_max_month(d$month, d$BIS)
}

threeway <- purrr::map_dfr(WINDOWS$name, function(nm) {
  m <- signature_month(nm)
  bis_windows %>% filter(window == nm, month == m) %>%
    mutate(episode = paste0(nm, "\n", m))
})

readr::write_csv(threeway, file.path(DIR_OUT, "bis_comparison_table.csv"))
cat("\n--- three-way comparison ---\n"); print(threeway %>% select(-window))

EPISODE_LABELS <- c(dotcom = "Dot-com", gfc = "GFC", ai = "AI era")   # chronological order

p3 <- threeway %>%
  mutate(panel_label = factor(EPISODE_LABELS[window], levels = EPISODE_LABELS)) %>%
  select(panel_label, V, L, S, C) %>%
  pivot_longer(-panel_label, names_to = "pillar", values_to = "z") %>%
  mutate(pillar = factor(pillar, levels = c("V", "L", "S", "C"))) %>%
  ggplot(aes(pillar, z, fill = pillar)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = .4) +
  geom_col(width = .7) +
  facet_wrap(~panel_label, nrow = 1) +
  scale_fill_manual(values = PAL, guide = "none") +
  labs(title = "Pillar profile of the three episodes",
       subtitle = paste0("Pillar z-scores at each episode's signature month: ",
                         paste(EPISODE_LABELS[threeway$window], month_label(threeway$month),
                               collapse = ", ")),
       caption = paste0("Signature month: the composite's peak for Dot-com, the leverage pillar's peak for the GFC,\n",
                        "the latest month with all four pillars and both sentiment indicators for the AI era."),
       x = NULL, y = "z-score") +
  THEME
save_fig(p3, "fig_threeway.png", w = 10, h = 4.5)

# ---- Figure 4: the sentiment pillar with and without GPR (v1.0 only) ----------
if (BIS_VERSION != "v0.1" && "s_gpr_adj" %in% names(panel)) {
  p4 <- panel %>%
    filter(month >= "1995-01") %>%
    select(month, `VIX only (v0.1)` = s_vix_adj, `VIX + GPR (v1.0)` = S) %>%
    pivot_longer(-month, names_to = "version", values_to = "z") %>%
    filter(!is.na(z)) %>%
    mutate(d = as_date(month)) %>%
    ggplot(aes(d, z, colour = version)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
    geom_line(linewidth = .6) +
    scale_colour_manual(values = c("grey65", PAL[["S"]])) +
    labs(title = "Sentiment pillar: before and after adding Geopolitical Risk",
         subtitle = "Averaging two weakly correlated risk gauges damps the idiosyncratic noise in each",
         x = NULL, y = "z-score", colour = NULL,
         caption = "Dents at 2001-09 and 2022-03 are GPR working as intended: a frightening world is not a complacent one.") +
    THEME
  save_fig(p4, "fig_sentiment_v2.png")
}

# ---- Catalyst timeline: S&P 500 with three external triggers ---------------------
CATALYSTS <- tibble(
  d = as.Date(c("1973-10-01", "2000-03-01", "2008-09-01")),
  label = c("1973 oil embargo", "2000 Fed tightening", "2008 Lehman")
)

p5 <- read_shiller() %>%
  filter(month >= "1970-01", !is.na(sp500)) %>%
  mutate(d = as_date(month)) %>%
  ggplot(aes(d, sp500)) +
  geom_line(colour = PAL[["L"]], linewidth = .6) +
  geom_vline(data = CATALYSTS, aes(xintercept = d),
             linetype = "dashed", colour = PAL[["V"]]) +
  geom_text(data = CATALYSTS, aes(x = d, y = Inf, label = label),
            inherit.aes = FALSE, angle = 90, vjust = -0.4, hjust = 1.05,
            size = 3, colour = PAL[["V"]]) +
  scale_y_log10() +
  labs(title = "Every build-up needed a pin",
       subtitle = "S&P 500 (log scale) with the three canonical external triggers",
       x = NULL, y = "S&P 500 (log)",
       caption = "A shock does not create a bubble - it exposes one that was already stretched. This is why the BIS scores four structural pillars and treats the trigger as exogenous.") +
  THEME
save_fig(p5, "fig_catalyst_timeline.png")

message("  all figures written to ", DIR_FIG)
