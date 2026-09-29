# Data provenance

This file records how the raw files behind the paper's data vintage were obtained. The monthly series in `data-clean/` were built from them, and the default run starts from `data-clean/`.

FRED could not be reached on 26 August 2026: its servers reset the connections. The four files named `fred_*.csv` were therefore built that day from other sources, the VIX, NFCI leverage and corporate debt from their primary publishers (the institutions whose data FRED republishes) and the Nasdaq Composite from Yahoo Finance, and written in FRED's CSV layout (a column `observation_date` and a column named after the FRED series code).

| File | Included here | Retrieved | Source | Notes |
|---|---|---|---|---|
| `fred_VIXCLS.csv` | no | 2026-08-26 | Cboe, `cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv`, column CLOSE | Daily close, 1990-01-02 to 2026-08-25. Same values as FRED's VIXCLS, which republishes Cboe. |
| `fred_NASDAQCOM.csv` | no | 2026-08-26 | Nasdaq Composite (^IXIC) from the Yahoo Finance chart API | Daily close, 1971-02-05 to 2026-08-17. |
| `fred_NFCILEVERAGE.csv` | yes | 2026-08-26 | Federal Reserve Bank of Chicago, `chicagofed.org/-/media/publications/nfci/nfci-data-series-csv.csv`, column Leverage | Weekly, 1971-01-08 to 2026-04-24. The Chicago Fed's own file ends there, so after April 2026 the v1.0 leverage pillar rests on corporate debt alone. |
| `fred_BCNSDODNS.csv` | yes | 2026-08-26 | Federal Reserve Z.1 release (Data Download Program, full release), series FL104104005.Q | Quarterly, 1970Q1 to 2026Q1. Converted from millions to billions of dollars, and from quarter-end dates to FRED's quarter-start dates (2026-03-31 becomes 2026-01-01). Without the date conversion the leverage pillar shifts by two months. |
| `shiller_sp500.csv` | yes | 2026-08-26 | `github.com/datasets/s-and-p-500`, a mirror of Robert Shiller's data | Monthly S&P 500, dividends and CAPE (PE10). S&P 500 1871-01 to 2026-07; CAPE 1881-01 to 2023-09. The mirror writes 0 where CAPE is missing. |
| `multpl_cape.csv` | no | 2026-08-26 | multpl.com Shiller PE table by month | Monthly, 1871-02 to 2026-08, used where the mirror has no CAPE. The 2026-08 value (41.98) is the table's live intraday reading of 26 August 2026, not a full-month value; the table later gave 41.13 for August 2026 (as of 29 September 2026). |
| `data_gpr_export.xls` | no | 2026-08-04 | Caldara and Iacoviello, `matteoiacoviello.com/gpr.htm` | Monthly benchmark GPR series (column GPR), 1985-01 to 2026-06. |

The files marked "no" are not redistributed here because of their providers' terms. Their monthly series are in `data-clean/`.

FRED rounds some series when it republishes them, and the Z.1 data are revised with each quarterly release. These files can therefore differ slightly from what the Python prototype used in June 2026. This affects only the check against the prototype's recorded values (see `ANCHORS` in `R/00_setup.R`), not any result in the paper.

## Newer data

`Rscript run_all.R --refresh` downloads every source again into `latest/raw/` and never changes this folder. It tries FRED first. If FRED does not respond, it takes the VIX from Cboe and NFCI leverage from the Chicago Fed, using the URLs above, and asks for the Nasdaq and corporate debt files to be saved by hand in `latest/raw/manual/`. It keeps only the monthly rows of the multpl.com table (dropping the live intraday row), drops the unfinished current month, and writes `latest/raw/RETRIEVED.csv` with the URL, date and newest observation of every file.
