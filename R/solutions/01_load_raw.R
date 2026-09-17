# =====================================================================
# BIS v1.0  —  01_load_raw.R      LAYER 0: get raw data onto disk
# =====================================================================
# No TODOs in this file. It is pure plumbing and is identical in
# R/ and R/solutions/. Its only job: every source ends up as a file in
# data-raw/, so that from here on the build never needs the internet.
#
# Run it once. Re-running is free — anything already downloaded is skipped.
# =====================================================================

UA <- c("User-Agent" = "Mozilla/5.0 (BIS research replication, R)")

# ---------------------------------------------------------------------
# fred() — download a FRED series once, cache it, return (date, value)
# ---------------------------------------------------------------------
fred <- function(series, start = "1970-01-01") {
  dest <- file.path(DIR_RAW, paste0("fred_", series, ".csv"))

  # A cached file must actually be a FRED CSV. A failed download can leave a
  # 0-byte or HTML file behind, which would then be "cached" forever.
  looks_ok <- function(p) {
    file.exists(p) && file.size(p) > 100 &&
      grepl("observation_date|DATE", readLines(p, n = 1, warn = FALSE)[1])
  }

  if (!looks_ok(dest)) {
    if (file.exists(dest)) file.remove(dest)
    url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=%s",
                   series, start)
    message("  downloading FRED ", series, " ...")

    # Try R's default downloader; on failure retry over HTTP/1.1 via the curl
    # binary — FRED's CDN intermittently kills HTTP/2 streams ("Stream error
    # in the HTTP/2 framing layer"), and forcing HTTP/1.1 usually cures it.
    ok <- tryCatch({
      utils::download.file(url, dest, quiet = TRUE, headers = UA); TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)

    if (!ok || !looks_ok(dest)) {
      if (file.exists(dest)) file.remove(dest)
      message("    default downloader failed - retrying over HTTP/1.1 ...")
      status <- suppressWarnings(system2(
        "curl", c("--http1.1", "-sSL", "--max-time", "120",
                  "-A", shQuote("Mozilla/5.0 (BIS research replication, R)"),
                  "-o", shQuote(dest), shQuote(url)),
        stdout = FALSE, stderr = FALSE))
      if (!identical(status, 0L) || !looks_ok(dest)) {
        if (file.exists(dest)) file.remove(dest)
        stop("Could not download FRED series ", series, ".\n",
             "  If other sites work but FRED does not, FRED itself is likely ",
             "down (it happens - check https://fred.stlouisfed.org in a ",
             "browser) - wait an hour and re-run; anything already in ",
             "data-raw/ is kept and skipped.\n",
             "  data-raw/PROVENANCE.md lists the primary-source mirror for ",
             "every series as a manual fallback.", call. = FALSE)
      }
    }
  }
  df <- readr::read_csv(dest, show_col_types = FALSE, progress = FALSE)
  # FRED's first column is "observation_date" (new) or "DATE" (old), and the
  # second is the series id. Rename by POSITION so both formats work.
  names(df)[1:2] <- c("date", "value")
  df %>%
    transmute(date  = as.Date(date),
              value = suppressWarnings(as.numeric(value))) %>%   # "." -> NA
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date)
}

# ---------------------------------------------------------------------
# shiller() — monthly S&P 500 price + CAPE (PE10) from the public mirror
# ---------------------------------------------------------------------
shiller <- function() {
  dest <- file.path(DIR_RAW, "shiller_sp500.csv")
  if (!file.exists(dest)) {
    message("  downloading Shiller mirror ...")
    utils::download.file(
      "https://raw.githubusercontent.com/datasets/s-and-p-500/main/data/data.csv",
      dest, quiet = TRUE, headers = UA)
  }
  readr::read_csv(dest, show_col_types = FALSE, progress = FALSE) %>%
    transmute(month = format(as.Date(Date), "%Y-%m"),
              sp500 = as.numeric(SP500),
              pe10  = as.numeric(PE10)) %>%
    mutate(pe10 = if_else(pe10 == 0, NA_real_, pe10)) %>%   # mirror uses 0 for missing
    filter(!is.na(month)) %>%
    arrange(month)
}

# ---------------------------------------------------------------------
# multpl_cape() — recent CAPE, for the months the mirror hasn't got yet
# ---------------------------------------------------------------------
# The mirror stops around 2023-09. v0.1 spliced multpl.com on top from
# 2023-10. Keep this splice identical or the 2026-05 anchor won't reproduce.
#
# IF THE SCRAPE FAILS (site layout changed, no network): do it by hand.
#   1. open https://www.multpl.com/shiller-pe/table/by-month
#   2. copy the table into a spreadsheet, keep two columns, name them
#      exactly  month,value   with month written as YYYY-MM
#   3. save as  data-raw/multpl_cape.csv
# The function below picks that file up automatically and never scrapes again.
# ---------------------------------------------------------------------
multpl_cape <- function() {
  dest <- file.path(DIR_RAW, "multpl_cape.csv")

  if (!file.exists(dest)) {
    message("  scraping multpl.com CAPE table ...")
    ok <- tryCatch({
      tbl <- rvest::read_html("https://www.multpl.com/shiller-pe/table/by-month") %>%
        rvest::html_table() %>%
        .[[1]]
      names(tbl)[1:2] <- c("date_txt", "value_txt")
      out <- tibble(
        month = format(as.Date(tbl$date_txt, format = "%b %d, %Y"), "%Y-%m"),
        value = readr::parse_number(as.character(tbl$value_txt))
      ) %>%
        filter(!is.na(month), !is.na(value)) %>%
        distinct(month, .keep_all = TRUE) %>%
        arrange(month)
      readr::write_csv(out, dest)
      TRUE
    }, error = function(e) { message("  !! scrape failed: ", conditionMessage(e)); FALSE })

    if (!ok) {
      warning("multpl CAPE unavailable. Follow the manual steps in the comment ",
              "above, then re-run. CAPE will stop at the mirror's last month ",
              "until you do, and the 2026-05 anchor will not reproduce.",
              call. = FALSE)
      return(tibble(month = character(), value = numeric()))
    }
  }

  readr::read_csv(dest, show_col_types = FALSE, progress = FALSE) %>%
    transmute(month = as.character(month), value = as.numeric(value)) %>%
    arrange(month)
}

# ---------------------------------------------------------------------
# gpr() — Geopolitical Risk index (Caldara & Iacoviello 2022)
# ---------------------------------------------------------------------
# MANUAL SOURCE. Already sitting in data-raw/data_gpr_export.xls.
# If you ever need a fresh copy: https://www.matteoiacoviello.com/gpr.htm
# Cite the download date in your references, as the authors ask.
#
# Columns in that file: month, GPR (benchmark, 10 papers, 1985+) <- the one
# you want; GPRT (threats), GPRA (acts), GPRH (historical, 3 papers, 1900+).
# ---------------------------------------------------------------------
gpr <- function() {
  path <- file.path(DIR_RAW, "data_gpr_export.xls")
  if (!file.exists(path)) {
    stop("Missing ", path, "\n  Download data_gpr_export.xls from ",
         "https://www.matteoiacoviello.com/gpr.htm and put it in data-raw/",
         call. = FALSE)
  }
  # guess_max matters here and is not optional. The file starts in 1900 for
  # the GPRH column, but GPR itself is blank until 1985 — so with readxl's
  # default 1000-row type guess the GPR column comes back LOGICAL, and
  # as.numeric() then silently turns the whole series into garbage. You would
  # get a GPR pillar of all NA and never be told. Read enough rows to guess right.
  raw <- readxl::read_excel(path, guess_max = 100000)
  stopifnot("GPR" %in% names(raw))
  if (!is.numeric(raw$GPR)) {
    stop("GPR column read as ", class(raw$GPR)[1], ", not numeric. ",
         "Raise guess_max or pass col_types explicitly.", call. = FALSE)
  }
  out <- raw %>%
    transmute(month = format(as.Date(month), "%Y-%m"),
              value = as.numeric(GPR)) %>%
    filter(!is.na(month), !is.na(value)) %>%
    arrange(month)

  # Loud sanity check, so a bad read can never pass silently.
  spike <- function(m) { v <- out$value[out$month == m]; if (length(v)) v else NA_real_ }
  if (!isTRUE(spike("2001-09") > 2 * mean(out$value))) {
    warning("GPR does not spike at 2001-09 (9/11). Check you read the 'GPR' ",
            "column and not GPRH/GPRT/GPRA.", call. = FALSE)
  }
  message(sprintf("  GPR ok: %d months %s..%s | mean %.0f | 2001-09 %.0f | 2022-03 %.0f",
                  nrow(out), min(out$month), max(out$month), mean(out$value),
                  spike("2001-09"), spike("2022-03")))
  out
}

# ---------------------------------------------------------------------
# Pull everything. Each object stays in memory for 02_build_panel.R.
# ---------------------------------------------------------------------
message("LAYER 0 - raw data")

raw_vix   <- fred("VIXCLS",       "1990-01-01")  # S: daily volatility
raw_ndq   <- fred("NASDAQCOM",    "1971-02-01")  # C: Nasdaq Composite, daily
raw_debt  <- fred("BCNSDODNS",    "1970-01-01")  # L: nonfin corp debt, quarterly
raw_nfci  <- fred("NFCILEVERAGE", "1971-01-01")  # L: Chicago Fed leverage, weekly
raw_shill <- shiller()                           # V + C: CAPE and S&P price
raw_capeR <- multpl_cape()                       # V: recent CAPE splice
raw_gpr   <- gpr()                               # S: geopolitical risk, monthly

message(sprintf("  vix %d | nasdaq %d | corpdebt %d | nfci %d | shiller %d | multpl %d | gpr %d rows",
                nrow(raw_vix), nrow(raw_ndq), nrow(raw_debt), nrow(raw_nfci),
                nrow(raw_shill), nrow(raw_capeR), nrow(raw_gpr)))

# ---- CHECKPOINT ------------------------------------------------------
# Before moving on, confirm in the Console:
#   range(raw_gpr$month)                  ~ "1985-01" .. current month
#   raw_gpr %>% filter(month %in% c("2001-09","2022-03"))
#       -> both should be far above the series average (9/11, Ukraine).
#          If they aren't, you're reading the wrong column.
#   range(raw_nfci$date)                  ~ 1971-01-08 .. current week
# ---------------------------------------------------------------------
