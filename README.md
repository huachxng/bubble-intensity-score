# Bubble Intensity Score (BIS)

Replication code and data for the working paper "The Bubble Intensity Score: A Composite Index for Situating the AI Market Among Historical Bubbles" (Sasipat Tejahempinyo, 2026).

## What the index is

The Bubble Intensity Score is a monthly index of how stretched the U.S. stock market is compared with its own recent past. It averages four pillars: valuation (V), leverage (L), sentiment (S) and concentration (C). Each indicator is scored as a z-score against its own trailing 120 months, so a BIS of +1.5 means the four pillars sit, on average, 1.5 standard deviations above their ten-year norms. The index runs monthly from January 1985.

```
V   = z(CAPE)
L   = mean( z(corporate debt growth), z(NFCI leverage) )
S   = mean( -z(VIX), -z(GPR) )
C   = z(Nasdaq Composite / S&P 500)
BIS = (V + L + S + C) / 4
```

The VIX and the Geopolitical Risk index (GPR) enter with a minus sign because calm markets and a quiet world are what complacency looks like. A pillar averages the indicators that have a value in a given month, and the BIS averages the pillars that have one (August 2026, which has no concentration value, is the mean of three).

## How to run it

1. Install R 4.6 from <https://cran.r-project.org> and the three packages it needs:

   ```r
   install.packages(c("tidyverse", "readxl", "rvest"))
   ```

2. Download this repository (on GitHub: Code, then Download ZIP, then unzip it) or clone it:

   ```
   git clone https://github.com/huachxng/bubble-intensity-score.git
   ```

3. Open a terminal in the repository folder and run:

   ```
   Rscript run_all.R
   ```

   From R or RStudio instead, make the repository folder the working directory (opening `bis-r.Rproj` in RStudio does this) and run `source("run_all.R")`.

The run takes a few seconds and needs no internet connection.

## What the default run reproduces

The default run rebuilds every table and figure of the paper, including the Fama forecast-date test, and writes them to `output/`. It starts from the monthly series in `data-clean/`, which hold the paper's data vintage (retrieved 26 August 2026; the GPR file on 4 August 2026), and reads one raw file, `data-raw/shiller_sp500.csv`, for the S&P 500 in the catalyst figure and the Fama test. It downloads nothing.

The tables it writes match the files committed in `output/`; on some computers the last of the 15 to 17 printed digits can differ. The figures show the same content, but the image files can differ slightly between computers because fonts and graphics libraries differ.

The run also prints a hand check for March 2000: the number of CAPE values in the trailing window, their mean and population standard deviation, the resulting z-score, and the four pillars with their average, so the reading can be redone on a calculator. After `source("run_all.R")` in R, `hand_check("2008-09")` repeats it for any month.

## Newer data: the refresh run

```
Rscript run_all.R --refresh
```

In R or RStudio: `BIS_REFRESH <- TRUE; source("run_all.R")`. Set `BIS_REFRESH <- FALSE` (or restart R) to go back to the default run.

The refresh downloads the newest data from the publishers into `latest/raw/`, rebuilds the six monthly series in `latest/clean/` and writes every table and figure to `latest/output/`. It never changes `data-raw/`, `data-clean/` or `output/`, and `latest/` is not tracked by git. Nothing updates on its own: new months appear only when you run the refresh.

How the refresh gets its data:

- Every file is downloaded again on each run.
- VIX, Nasdaq Composite, corporate debt and NFCI leverage come from FRED. If FRED does not respond, VIX is taken from Cboe's own file and NFCI leverage from the Chicago Fed's own file. Nasdaq and corporate debt have no automatic fallback: the run stops and lists, for each missing file, where to get it and what columns it needs. Save the file in `latest/raw/manual/` and run the refresh again. A file in `latest/raw/manual/` is used only when its download fails.
- CAPE comes from the Shiller data mirror and from multpl.com, GPR from its authors' website.
- `latest/raw/RETRIEVED.csv` records, for every series, the URL or manual file used, the date, the newest observation in the raw file, and whether it came from FRED, a fallback, a manual file, or failed.
- The current calendar month is dropped because it is not over yet. multpl.com's table also shows a live intraday value for it, which is never used.
- Corporate debt is quarterly and is published about ten weeks after a quarter ends, so its last published quarter is carried forward to the newest month.

Refreshed results will differ from the paper. New months are added at the end, and the sources revise their history: GPR revises recent months, multpl.com revises recent CAPE values as earnings are reported, and each quarterly Z.1 release revises corporate debt. In a test refresh on 29 September 2026 (FRED was unreachable, so a Nasdaq file from Yahoo Finance and the paper's corporate debt file were supplied by hand), the sentiment pillar changed from April 2025 on and valuation from May 2026 on, the composite moved by up to 0.21, and all Dot-com and GFC results stayed the same. The Chicago Fed's NFCI file still ended in April 2026, so the leverage pillar used corporate debt alone after that month.

## Index versions and the prototype check

`BIS_VERSION` in `R/00_setup.R` is `"v1.0"`, the paper's index. Setting it to `"v0.1"` builds the earlier one-indicator-per-pillar prototype (V = CAPE, L = corporate debt growth, S = flipped VIX, C = Nasdaq/S&P) and writes its results to `output/v0.1/` (`latest/output/v0.1/` in a refresh), so the paper's files are kept.

Every run compares the prototype's pillars with the values recorded for five months from the original Python prototype in June 2026 and writes the result to `anchor_check.csv`. With the paper's data, one month matches to two decimals and four are within their tolerance bands. The differences come from data revisions since June 2026; the bands and their reasons are next to `ANCHORS` in `R/00_setup.R`.

## Files

```
run_all.R            runs everything, in order
R/00_setup.R         settings, folders and shared helper functions
R/01_load_raw.R      downloads the raw data (refresh run only)
R/02_build_panel.R   builds the six monthly series (refresh run only)
R/03_pillars.R       z-scores, the monthly panel and the four pillars
R/04_bis.R           the composite, the three analysis windows and the prototype check
R/05_backtest.R      backtest scorecard, hand check and sensitivity tests
R/06_figures.R       figures and the three-way comparison table
R/07_fama_test.R     Fama's forecast-date test
data-raw/            the raw files that may be redistributed, and PROVENANCE.md
data-clean/          one monthly series per indicator: the paper's inputs
output/              the paper's tables and figures
latest/              created by a refresh run; not tracked
```

Every file in `data-clean/` has two columns: `month` (text, `YYYY-MM`) and `value`.

## Data

| Pillar | Indicator | Source | Frequency | Coverage in this package | Raw file included |
|---|---|---|---|---|---|
| V | CAPE (Shiller PE10) | Robert Shiller's data via the `datasets/s-and-p-500` mirror; multpl.com Shiller PE table where the mirror has no CAPE (1871 to 1880 and from 2023-10) | monthly | 1871-02 to 2026-08 | mirror: yes; multpl table: no |
| L | Nonfinancial corporate debt, year-over-year growth | Federal Reserve Z.1, series FL104104005.Q (FRED: BCNSDODNS) | quarterly | 1970Q1 to 2026Q1 (growth from 1971Q1) | yes |
| L | NFCI leverage subindex | Federal Reserve Bank of Chicago (FRED: NFCILEVERAGE) | weekly | 1971-01-08 to 2026-04-24 | yes |
| S | VIX | Cboe (FRED: VIXCLS) | daily | 1990-01-02 to 2026-08-25 | no |
| S | Geopolitical Risk index, benchmark series | Caldara and Iacoviello (2022), matteoiacoviello.com | monthly | 1985-01 to 2026-06 | no |
| C | Nasdaq Composite | Nasdaq, via Yahoo Finance ^IXIC (FRED: NASDAQCOM) | daily | 1971-02-05 to 2026-08-17 | no |
| C | S&P 500 | Robert Shiller's data via the same mirror | monthly | 1871-01 to 2026-07 | yes |

Daily and weekly series are averaged within each month. The Nasdaq to S&P 500 ratio ends in July 2026 because the S&P 500 series does, so the concentration pillar has no value for August 2026. `data-clean/l1_corpdebt_yoy.csv` carries the last quarter forward to September 2026, the month the file was built; the panel uses it through August 2026.

The raw VIX, Nasdaq, multpl.com and GPR files are not redistributed here; the refresh run downloads them. The monthly series of all six indicators are in `data-clean/`. `data-raw/PROVENANCE.md` records how each raw file was obtained and converted. The panel starts in January 1985, the first GPR month and 120 months before the Dot-com window opens.

## Outputs and where they appear in the paper

| File in `output/` | In the paper |
|---|---|
| `bis_panel_monthly.csv` | Every monthly reading: indicator z-scores, pillars and the BIS (Chapters 4 to 6, Figure 1) |
| `bis_windows.csv` | The three analysis windows (Figures 2a to 2c); the run prints their Section 4.5 descriptive statistics |
| `backtest_scorecard.csv` | Section 5.1 scorecard, Table B3 |
| `bis_comparison_table.csv` | Section 5.5 three-way comparison, Figure 3 |
| `sensitivity.csv` | Section 5.6 weighting table, Table B1 |
| `lookback_sensitivity.csv` | Table B2 (the Dot-com rows) |
| `gpr_variance_test.csv` | Section 5.6, the sentiment upgrade |
| `fama_forecast_test.csv` | Sections 2.4 and 5.4, Fama's forecast-date test |
| `anchor_check.csv` | Section 4.6, the check against the prototype |
| `figures/fig_bis_history.png` | Figure 1 |
| `figures/fig_pillars_dotcom.png`, `fig_pillars_gfc.png`, `fig_pillars_ai.png` | Figures 2a, 2b, 2c |
| `figures/fig_threeway.png` | Figure 3 |
| `figures/fig_sentiment_v2.png` | Figure 4 |
| `figures/fig_catalyst_timeline.png` | The S&P 500 with three external triggers (Section 2.3); not a numbered figure |

## Limitations

- Concentration is a proxy: the Nasdaq to S&P 500 ratio, not the top-ten share of market capitalization, which is not free for the full period.
- Leverage misses much of private credit, vendor financing and circular financing between AI firms and their suppliers. The NFCI subindex reaches into shadow banking but does not close the gap.
- Valuation joins two Shiller-based sources at 2023-10.
- GPR is built from English-language newspapers and starts in 1985, which is why the panel starts there.
- Three episodes is a small sample. The index is descriptive; it has not been validated as a forecasting model.
- Refreshed results depend on revisions by the sources and on FRED being reachable, and the multpl.com table is read from a web page whose layout can change.

## How to cite

GitHub's "Cite this repository" button uses `CITATION.cff`. In text:

Tejahempinyo, S. (2026). *The Bubble Intensity Score: A Composite Index for Situating the AI Market Among Historical Bubbles*. Working paper. Code and data: https://github.com/huachxng/bubble-intensity-score

Please also cite the data sources listed above. The GPR authors ask that the download date be cited.

## License

The code is released under the MIT License (see `LICENSE`). The data remain under their providers' terms.
