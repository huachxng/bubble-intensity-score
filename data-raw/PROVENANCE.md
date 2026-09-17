# Data provenance — what is in this folder and where it actually came from

FRED was down on **2026-08-26** (its CDN was killing connections — the same
outage that produced the "Stream error in the HTTP/2 framing layer" message in
RStudio). The four `fred_*.csv` caches were therefore built that day from each
series' **primary source** — the institution FRED itself re-serves — and
converted to FRED's CSV layout so the pipeline cannot tell the difference.

| File | Retrieved | Primary source used | Notes |
|---|---|---|---|
| `fred_VIXCLS.csv` | 2026-08-26 | CBOE, `cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv` | Daily CLOSE, 1990-01-02 → 2026-08-25. Identical values to FRED's VIXCLS (FRED republishes CBOE). |
| `fred_NASDAQCOM.csv` | 2026-08-26 | Nasdaq Composite (^IXIC) via Yahoo Finance chart API | Daily close, 1971-02-05 → 2026-08-17. |
| `fred_NFCILEVERAGE.csv` | 2026-08-26 | Federal Reserve Bank of Chicago, `chicagofed.org/-/media/publications/nfci/nfci-data-series-csv.csv`, column `Leverage` | Weekly, 1971-01-08 → **2026-04-24** — the Chicago Fed's own published file ends there. Months after 2026-04 have no NFCI reading, so the v1.0 L pillar falls back to corporate debt alone for those months (by design, `na.rm = TRUE`). |
| `fred_BCNSDODNS.csv` | 2026-08-26 | Federal Reserve Z.1 release (Data Download Program, full-release zip), series `FL104104005.Q` | Quarterly, 1970Q1 → 2026Q1. Two conversions applied to match FRED: millions → billions (÷1000), and quarter-END dates → FRED's quarter-START convention (e.g. 2026-03-31 → 2026-01-01). The date convention matters: without it the leverage pillar shifts two months and the anchors fail. |
| `shiller_sp500.csv` | 2026-08-26 | `github.com/datasets/s-and-p-500` mirror of Robert Shiller's Yale dataset | Monthly SP500 + PE10, 1871-01 → mirror's last month. |
| `multpl_cape.csv` | 2026-08-26 | multpl.com Shiller-PE monthly table | Fills CAPE from where the mirror stops through the current month. |
| `data_gpr_export.xls` | 2026-07/08 (user download) | Caldara & Iacoviello, matteoiacoviello.com/gpr.htm | Monthly GPR, 1985-01 → 2026-06. |

**For the paper (Ch.4 / Appendix A):** cite the primary sources above, with these
retrieval dates. Citing CBOE, the Chicago Fed, and the Federal Reserve Z.1
directly is, if anything, stronger than citing FRED's re-publication of them.

**To refresh later:** delete the file you want refreshed and re-run
`R/01_load_raw.R` — it re-downloads only what is missing, from FRED (working
again by then, presumably). If FRED is still down, the URLs above are the manual
fallback: download, and reshape to two columns (`observation_date`, value) with
ISO dates.

**One nuance to disclose if numbers shift at the 2nd decimal:** FRED applies its
own rounding when re-serving these series, and Z.1 data is revised each
quarterly release. Values retrieved 2026-08-26 from the primary sources can
therefore differ from what the Python prototype saw in June 2026 by rounding or
revision — relevant only to the regression-test anchors, not to any conclusion.
