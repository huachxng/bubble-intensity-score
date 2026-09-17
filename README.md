# Bubble Intensity Score (BIS) — replication code

R code and data for the working paper **"The Bubble Intensity Score: A Composite Index for Situating the AI Market Among Historical Bubbles"** (Sasipat Tejahempinyo, 2026). This repository is Appendix C of the paper.

**The index.** BIS<sub>t</sub> = ¼ (V<sub>t</sub> + L<sub>t</sub> + S<sub>t</sub> + C<sub>t</sub>): the equal-weighted mean of four pillar z-scores — **valuation** (Shiller CAPE), **leverage** (year-over-year growth of nonfinancial corporate debt, and the Chicago Fed NFCI leverage subindex), **sentiment** (the VIX and the Geopolitical Risk index, both sign-flipped so that calm reads positive) and **concentration** (Nasdaq Composite ÷ S&P 500) — each indicator scored against its own trailing 120-month history. Monthly, January 1985 to the present.

**Reproduce every table and figure in the paper**

1. Open `bis-r.Rproj` in RStudio (R ≥ 4.3; packages `tidyverse`, `readxl`, `rvest`, `scales`).
2. Download the Geopolitical Risk index workbook (`data_gpr_export.xls`) from <https://www.matteoiacoviello.com/gpr.htm> into `data-raw/`. Its authors ask that the download date be cited.
3. In `R/00_setup.R` set `USE_SOLUTIONS <- TRUE` and `BIS_VERSION <- "v1.0"`, then run `source("run_all.R")`. Everything the paper reports is written to `output/`: `bis_panel_monthly.csv`, `bis_windows.csv`, `backtest_scorecard.csv`, `bis_comparison_table.csv`, `sensitivity.csv`, `gpr_variance_test.csv`, `anchor_check.csv` and `figures/`. `source("R/07_fama_test.R")` reproduces the first-warning-date test of Section 5.4.

`R/` is the pipeline written as a set of exercises with `TODO` markers, completed by the author; `R/solutions/` is the reference implementation, and the two produce identical output. `BIS_VERSION <- "v0.1"` reproduces the earlier single-indicator specification used for the regression test in Section 4.6.

**Data.** `data-clean/` (one tidy file per indicator) and `output/` are the exact files behind the paper, built on 8 September 2026. `data-raw/` ships the caches that are free to redistribute — the Federal Reserve Z.1 corporate-debt series, the Chicago Fed NFCI leverage subindex, and the Shiller series from the open `datasets/s-and-p-500` mirror. The VIX (Cboe), Nasdaq Composite (Nasdaq) and multpl.com CAPE caches are not redistributed; `run_all.R` downloads them on the first run. `data-raw/PROVENANCE.md` records how each cache was built.

**Citing.** See `CITATION.cff`. Code is released under the MIT License; the data remain under their providers' terms.

---

*The rest of this file is the step-by-step build guide the pipeline was written from.*

# BIS v1.0 — RStudio build guide

Everything needed to compute the Bubble Intensity Score yourself, in R, and to backtest it by hand. Written for the September draft.

**The 60-second version:** six public data series → one monthly panel → four pillar z-scores → one composite number → a scorecard that says whether the index behaved as you predicted *before* you ran it.

------------------------------------------------------------------------

## 0. First-time setup

1.  **Double-click `bis-r.Rproj`.** Always open the project this way. It sets the working directory, which is why every path in the code is short and relative. Check it worked: type `getwd()` in the Console — it must end in `/bis-r`.
2.  `tidyverse`, `readxl` and `rvest` are already installed on your machine. Only extra: `install.packages("scales")`.
3.  Everything is already in `data-raw/` — the GPR file plus all five scripted sources. FRED was down on 2026-08-26 (the "Stream error in the HTTP/2 framing layer" message), so the four FRED caches were built from each series' primary source instead — CBOE, the Chicago Fed, the Federal Reserve's Z.1 release, Yahoo — converted to FRED's exact format. `data-raw/PROVENANCE.md` records what came from where; cite those primary sources in Ch.4. The pipeline only downloads what is missing, so it will not touch FRED again unless you delete a file.

**Three panes you'll live in:** Script (top-left, where you write), Console (bottom-left, where answers appear), Environment (top-right, what's loaded). `Cmd+Enter` runs the line your cursor is on — that's how to work through this, one line at a time.

------------------------------------------------------------------------

## 1. Two switches, both in `R/00_setup.R`

``` r
USE_SOLUTIONS <- FALSE   # FALSE = your code in R/ ; TRUE = finished code in R/solutions/
BIS_VERSION   <- "v0.1"  # "v0.1" = the Python prototype's pillars ; "v1.0" = + NFCI + GPR
```

**Use them like this.** Work in coach mode (`FALSE`). When a TODO won't come, flip to `TRUE`, run, see the number it *should* produce, flip back, make yours match. That's not cheating — it's how you get a target to aim at.

**Build `v0.1` first and pass the regression test before touching `v1.0`.** Adding new indicators on top of a broken port means debugging two things at once.

------------------------------------------------------------------------

## 2. The map

```         
bis-r/
├── data-raw/     L0  downloads. Never edited, by you or by code.
├── data-clean/   L1  six files, every one shaped (month, value)
├── output/       L2-L5  panel, windows, tables, scorecard, figures/
└── R/            the seven stages + solutions/
```

Data only ever flows downhill. Delete `data-clean/` and `output/` any time — `run_all.R` rebuilds both from `data-raw/` without touching the network.

**The one rule everything rests on:** every indicator, whatever its source, becomes exactly two columns — `month` (character `"YYYY-MM"`) and `value` (double). Once all six look identical, the pillar maths is the same three lines for each. `check_contract()` enforces it and refuses to write a file that breaks it.

------------------------------------------------------------------------

## 3. Data inventory

| Pillar | Indicator | Source | Coverage | Higher = more bubbly? |
|----|----|----|----|----|
| **V** | Shiller CAPE (PE10) | GitHub mirror + multpl.com from 2023-10 | 1871→ | yes |
| **L** | Nonfin. corporate debt, yoy | FRED `BCNSDODNS`, quarterly | 1951Q4→ | yes |
| **L** | NFCI Leverage Subindex | FRED `NFCILEVERAGE`, weekly | 1971-01-08→ | **check in step 2** |
| **S** | VIX, monthly mean | FRED `VIXCLS`, daily | 1990-01→ | no — **flip** |
| **S** | Geopolitical Risk | matteoiacoviello.com/gpr.htm | 1985-01→ | no — **flip** |
| **C** | Nasdaq ÷ S&P 500 | FRED `NASDAQCOM` ÷ Shiller | 1971-02→ | yes |

```         
V   = z(CAPE)
L   = mean( z(corp debt yoy), z(NFCI leverage) )
S   = mean( -z(VIX), -z(GPR) )
C   = z(Nasdaq/S&P)
BIS = mean(V, L, S, C)
```

Panel starts **1985-01** — GPR's first month, and exactly 120 months before the Dot-com window opens, so every scored month has a full trailing decade behind it.

**Deferred, and worth one line in Ch.6 as future work:** FINRA margin debt (starts 1997, would blank most of the Dot-com window), market-cap-to-GDP, capex-to-revenue from EDGAR, true top-10 concentration share.

------------------------------------------------------------------------

## 4. The build, stage by stage

Run `source("run_all.R")`, or work through the files one at a time.

| Stage | File | TODOs | What you check when it finishes |
|----|----|----|----|
| L0 | `01_load_raw.R` | 0 | Row counts print. GPR line must read `2001-09 499`. |
| L1 | `02_build_panel.R` | 4 | Six `OK` lines, each with a date range matching §3. |
| L2–4 | `03_pillars.R` | 3 | `trailing_z self-test PASSED`, then 2000-03 pillars. |
| L5 | `04_bis.R` | 1 | The regression test — see §5. |
| — | `05_backtest.R` | 3 | Scorecard, hand check, sensitivity. |
| — | `06_figures.R` | 0 | Seven PNGs in `output/figures/`. |

### The sign check in stage 2 — don't skip it

NFCI is the one indicator whose direction you decide from the data rather than from theory. Plot it over 2003–2010:

``` r
l2_nfci_leverage %>% filter(month >= "2003-01", month <= "2010-12") %>%
  ggplot(aes(month, value, group = 1)) + geom_line()
```

Rises into 2007–08 → leave `NFCI_SIGN <- 1`. Falls → set `-1` and write down why. Either way it's one sentence in Chapter 4, and a reviewer will look for it.

### The trap in stage 3

R's `sd()` is the **sample** standard deviation (÷ n−1). Python used the **population** one (÷ n). Use `sqrt(mean((xs - mean(xs))^2))`. The self-test in `03` is built to catch exactly this: right answer `1.689278`, the sd() answer `1.668028`. If you see the second number, that's your bug.

And: **z-score each series at its own full length, then join.** Not the other way round. CAPE runs from 1871; if you join into the 1985 panel first, its trailing window gets truncated and every early z-score shifts slightly — close but wrong, which is the worst kind of wrong.

------------------------------------------------------------------------

## 5. The regression test (Phase 4)

With `BIS_VERSION <- "v0.1"`, `04_bis.R` checks your output against the Python prototype. All five must reproduce to 2 dp before you go further.

| Month   | V     | L         | S     | C     | BIS       |
|---------|-------|-----------|-------|-------|-----------|
| 2000-03 | +2.04 | +0.89     | −0.74 | +5.06 | **+1.81** |
| 1999-12 |       |           |       |       | +1.73     |
| 2002-09 |       |           |       |       | −1.65     |
| 2007-07 | −0.53 | **+1.43** | +0.53 | −0.23 | +0.30     |
| 2026-05 | +2.29 | −0.50     | +0.20 | +1.74 | +0.93     |

### If they don't match — debug in this order

| \# | Symptom | Cause |
|----|----|----|
| 1 | Everything off by \~0.5% | sample vs population sd (`03`, TODO 3.1) |
| 2 | Early years off, recent years fine | joined before z-scoring (`03`) |
| 3 | S and C off, V and L fine | monthly *last* instead of monthly *mean* (`02`) |
| 4 | Whole months blank | missing `na.rm = TRUE` on the composite (`04`) |
| 5 | 2026-05 is NA | multpl CAPE splice didn't reach 2026 (`01` — manual fallback in the comments) |
| 6 | GPR pillar silently all NA | `read_excel` guessed the column as logical — already fixed via `guess_max`, but check the `GPR ok:` line printed |

**PASS and CLOSE both count.** The anchors were recorded from June-2026 data; multpl has since revised recent CAPE months and the June Z.1 release revised debt history, so exact-to-the-cent is no longer attainable on four of the five rows. Each row's allowed drift (`close_band`) is defined next to ANCHORS in `00_setup.R` with the reasons. The big one: 2026-05's L moved from −0.50 to ≈ −0.05 because the June Z.1 release revised 2026Q1 corporate-debt growth upward — the data changed, not the method. A FAIL is still a code bug until proven otherwise.

Once all five come back PASS or CLOSE, switch `BIS_VERSION <- "v1.0"` and re-run. L and S are now *expected* to differ — that's the upgrade, not a failure.

------------------------------------------------------------------------

## 6. The manual backtest

**Write the criteria into the Google Doc before you look at any output.** Stating them first is what makes this a test rather than a story told afterwards, and it's your answer to the hindsight-bias objection.

1.  Dot-com: BIS peaks within ±6 months of 2000-03.
2.  Dot-com: that peak clears the 90th percentile of the whole distribution.
3.  GFC: the **L pillar** peaks within ±12 months of 2007-08.
4.  GFC: the composite does **not** exceed +1.5 pre-crash.
5.  AI era: reported, not scored — the outcome isn't known yet.

**Criterion 4 is the honest one.** v0.1's GFC maximum was +0.30: the composite missed 2008 entirely, while the leverage pillar hit +1.43 the month before the credit crunch. 2008 was a credit crisis, not an equity-valuation bubble, so an equity-centric composite is the wrong lens for it — and the pillar decomposition shows precisely that. Report it. An index that nails all three episodes looks fitted; one that misses in an explainable way looks honest.

**The hand calculation.** Run `hand_check("2000-03")`. It prints the 120 raw CAPE values' mean and population sd so you can redo the z-score on a calculator, then the four pillars so you can average them yourself:

```         
(2.04 + 0.89 − 0.74 + 5.06) / 4 = 7.25 / 4 = 1.8125 → +1.81   ✓
(2.29 − 0.50 + 0.20 + 1.74) / 4 = 3.73 / 4 = 0.9325 → +0.93   ✓
```

When you can do that unaided you can defend the index at a whiteboard, which is the whole reason for building it in R yourself.

------------------------------------------------------------------------

## 7. Outputs, and where each one goes in the paper

| File | Goes into |
|----|----|
| `bis_panel_monthly.csv` | Appendix C (replication) |
| `bis_windows.csv` | Ch.4 descriptive statistics |
| `anchor_check.csv` | Ch.4 — evidence the port is faithful |
| `backtest_scorecard.csv` | **Ch.5, the central exhibit** |
| `bis_comparison_table.csv` | **Ch.5, the three-way comparison** |
| `sensitivity.csv` | Appendix B (robustness) |
| `gpr_variance_test.csv` | Ch.3 methods — justifies the GPR upgrade |
| `figures/fig_threeway.png` | Ch.5 — one look = the whole paper |
| `figures/fig_pillars_*.png` | Ch.5 — "read the decomposition, not the headline" |
| `figures/fig_catalyst_timeline.png` | Ch.2 Indicator 5 — original, replaces the borrowed image |

Paste-ready prose for Ch.4 and Ch.5 is in `doc-sections/`.

------------------------------------------------------------------------

## 8. Things to disclose in the paper

- **C is a proxy.** Nasdaq/S&P ratio, not true top-10 market-cap share.
- **L is listed-market only.** It misses private credit, vendor financing and circular financing of AI capex. NFCI helps (it reaches shadow banking) but doesn't close the gap. This is the AI-era blind spot — and the strongest version of your Ch.6 self-critique.
- **V splices two sources** at 2023-10; both Shiller-derived.
- **GPR is a Western-press measure** — English-language newspapers — and starts 1985, so the S pillar is VIX-only before then.
- **Pillars average over available indicators**, so a pillar still reports when only one of its two exists that month.
- **N = 3 episodes.** The index is illustrative and pedagogical, not statistically validated. Say so plainly.
