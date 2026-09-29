# 01_load_raw.R: download the newest raw data into latest/raw/ (refresh run only).

if (!REFRESH) {
  stop("01_load_raw.R downloads new data and runs only in a refresh run: ",
       "Rscript run_all.R --refresh", call. = FALSE)
}

message("\n== 01 raw data: downloading into ", DIR_RAW, "/")

UA           <- "Mozilla/5.0 (BIS research replication, R)"
RETRIEVED_ON <- Sys.Date()
DIR_MANUAL   <- file.path(DIR_RAW, "manual")   # files saved here by hand are used when a download fails
retrievals   <- list()                         # rows of RETRIEVED.csv
failures     <- character()                    # series name -> where to get the file by hand
fred_down    <- FALSE

# Download url into dest, keeping the file only if valid(path) is TRUE.
fetch <- function(url, dest, valid) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  ok <- tryCatch({
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb",
                         headers = c("User-Agent" = UA))
    isTRUE(valid(tmp))
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    # FRED's CDN sometimes resets HTTP/2 connections; one retry over HTTP/1.1
    # with the curl command-line tool, giving up after 30 s without data.
    unlink(tmp)
    status <- tryCatch(suppressWarnings(system2(
      "curl", c("--http1.1", "-sSL", "--connect-timeout", "20",
                "--speed-limit", "1", "--speed-time", "30",
                "-A", shQuote(UA), "-o", shQuote(tmp), shQuote(url)),
      stdout = FALSE, stderr = FALSE)), error = function(e) 1L)
    ok <- identical(as.integer(status), 0L) && isTRUE(tryCatch(valid(tmp), error = function(e) FALSE))
  }
  if (ok) file.copy(tmp, dest, overwrite = TRUE)
  ok
}

first_line <- function(p) {
  if (!file.exists(p) || file.size(p) == 0) return("")
  readLines(p, n = 1, warn = FALSE)[1]
}

# Use a hand-saved copy from latest/raw/manual/ when the download failed.
use_manual <- function(file, dest, valid) {
  p <- file.path(DIR_MANUAL, file)
  if (!file.exists(p) || !isTRUE(valid(p))) return(NULL)
  file.copy(p, dest, overwrite = TRUE)
  p
}

# A manual file is dated by its modification time, a download by today's date.
log_retrieval <- function(series, url, last_obs, note) {
  when <- if (grepl("manual file", note)) as.Date(file.mtime(url)) else RETRIEVED_ON
  retrievals[[series]] <<- tibble(series = series, url = url,
                                  retrieved = format(when),
                                  last_observation = last_obs, note = note)
}

# ---- FRED series, with the publishers' own files as fallbacks -----------------
is_fred_csv <- function(p) {
  file.exists(p) && file.size(p) > 100 && grepl("observation_date|DATE", first_line(p))
}

read_fred_csv <- function(p) {
  df <- readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
  names(df)[1:2] <- c("date", "value")      # "observation_date" or "DATE", then the series id
  df %>%
    transmute(date  = as.Date(date),
              value = suppressWarnings(as.numeric(value))) %>%   # FRED writes "." for missing
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date)
}

# Cboe's VIX history, written in FRED's layout (daily close).
vix_from_cboe <- function(dest) {
  url <- "https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv"
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  if (!fetch(url, tmp, function(p) grepl("^DATE,.*CLOSE", first_line(p)))) return(NULL)
  readr::read_csv(tmp, show_col_types = FALSE, progress = FALSE) %>%
    transmute(observation_date = as.Date(DATE, format = "%m/%d/%Y"),
              VIXCLS = as.numeric(CLOSE)) %>%
    filter(!is.na(observation_date), !is.na(VIXCLS)) %>%
    readr::write_csv(dest)
  url
}

# The Chicago Fed's NFCI file, Leverage column, written in FRED's layout.
nfci_from_chicagofed <- function(dest) {
  url <- "https://www.chicagofed.org/-/media/publications/nfci/nfci-data-series-csv.csv"
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  if (!fetch(url, tmp, function(p) grepl("Friday_of_Week.*Leverage", first_line(p)))) return(NULL)
  readr::read_csv(tmp, show_col_types = FALSE, progress = FALSE) %>%
    transmute(observation_date = as.Date(Friday_of_Week, format = "%m/%d/%Y"),
              NFCILEVERAGE = as.numeric(Leverage)) %>%
    filter(!is.na(observation_date), !is.na(NFCILEVERAGE)) %>%
    readr::write_csv(dest)
  url
}

fred <- function(series, start, fallback = NULL, manual_source) {
  file <- paste0("fred_", series, ".csv")
  dest <- file.path(DIR_RAW, file)
  url  <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=%s", series, start)
  note <- "FRED"
  if (fred_down || !fetch(url, dest, is_fred_csv)) {
    if (!fred_down) message("  FRED did not respond; the remaining FRED series go straight to their fallbacks")
    fred_down <<- TRUE
    alt <- if (is.null(fallback)) NULL else fallback(dest)
    if (!is.null(alt)) {
      url  <- alt
      note <- "FRED unreachable; publisher's own file"
    } else if (!is.null(m <- use_manual(file, dest, is_fred_csv))) {
      url  <- m
      note <- "download failed; manual file"
    } else {
      failures[series] <<- paste0(file, ": ", manual_source)
      log_retrieval(series, url, NA_character_, "failed")
      return(NULL)
    }
  }
  df <- read_fred_csv(dest)
  log_retrieval(series, url, format(max(df$date)), note)
  message(sprintf("  %-13s %s .. %s  (%s)", series, min(df$date), max(df$date), note))
  df
}

# ---- Other sources --------------------------------------------------------------
# Download from url, or use a manual copy; NULL (and a recorded failure) if neither works.
get_file <- function(series, url, file, valid, manual_source) {
  dest <- file.path(DIR_RAW, file)
  if (fetch(url, dest, valid)) return(list(dest = dest, url = url, note = "download"))
  m <- use_manual(file, dest, valid)
  if (!is.null(m)) return(list(dest = dest, url = m, note = "download failed; manual file"))
  failures[series] <<- paste0(file, ": ", manual_source)
  log_retrieval(series, url, NA_character_, "failed")
  NULL
}

SHILLER_URL <- "https://raw.githubusercontent.com/datasets/s-and-p-500/main/data/data.csv"

is_shiller_csv <- function(p) {
  x <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE),
                error = function(e) NULL)
  !is.null(x) && all(c("Date", "SP500", "Dividend", "PE10") %in% names(x)) &&
    nrow(x) >= 1800 && format(min(as.Date(x$Date)), "%Y-%m") == "1871-01" &&
    is.numeric(x$SP500) && all(x$SP500 > 0, na.rm = TRUE)
}

shiller <- function() {
  f <- get_file("Shiller", SHILLER_URL, "shiller_sp500.csv", is_shiller_csv,
                "data.csv from https://github.com/datasets/s-and-p-500 (a mirror of Robert Shiller's data), saved unchanged")
  if (is.null(f)) return(NULL)
  df <- read_shiller(f$dest)
  log_retrieval("Shiller", f$url, max(df$month), f$note)
  message(sprintf("  %-13s %s .. %s  (%s)", "Shiller", min(df$month), max(df$month), f$note))
  df
}

# multpl.com's monthly CAPE table fills the months where the mirror has no
# CAPE (from 2023-10). Rows dated the 1st are monthly values; the newest row
# is a live intraday reading and is dropped.
MULTPL_URL <- "https://www.multpl.com/shiller-pe/table/by-month"

is_multpl_csv <- function(p) {
  x <- tryCatch(readr::read_csv(p, show_col_types = FALSE, progress = FALSE,
                                col_types = "cd"), error = function(e) NULL)
  !is.null(x) && identical(names(x), c("month", "value")) && nrow(x) > 1000 &&
    all(grepl("^[0-9]{4}-[0-9]{2}$", x$month))
}

multpl_cape <- function() {
  file <- "multpl_cape.csv"
  dest <- file.path(DIR_RAW, file)
  html <- tempfile(fileext = ".html")
  on.exit(unlink(html))
  scraped <- fetch(MULTPL_URL, html, function(p) file.size(p) > 10000) && isTRUE(tryCatch({
    tbl <- rvest::read_html(html) %>% rvest::html_table() %>% .[[1]]
    txt <- trimws(tbl[[1]])                                            # e.g. "Sep 1, 2026"
    tibble(mon   = match(substr(txt, 1, 3), month.abb),                # English month names in any locale
           day   = suppressWarnings(as.integer(sub("^[A-Za-z]+ ([0-9]+),.*$", "\\1", txt))),
           yr    = suppressWarnings(as.integer(sub("^.*, ([0-9]{4})$", "\\1", txt))),
           value = readr::parse_number(as.character(tbl[[2]]))) %>%
      filter(!is.na(mon), !is.na(yr), day == 1, !is.na(value)) %>%
      mutate(month = sprintf("%04d-%02d", yr, mon)) %>%
      distinct(month, .keep_all = TRUE) %>%
      select(month, value) %>%
      arrange(month) %>%
      readr::write_csv(dest)
    is_multpl_csv(dest)
  }, error = function(e) FALSE))
  url <- MULTPL_URL
  note <- "download"
  if (!scraped) {
    m <- use_manual(file, dest, is_multpl_csv)
    if (is.null(m)) {
      failures["multpl CAPE"] <<- paste0(file, ": the table at ", MULTPL_URL,
                                         ", saved as two columns month,value with month as YYYY-MM")
      log_retrieval("multpl CAPE", MULTPL_URL, NA_character_, "failed")
      return(NULL)
    }
    url <- m
    note <- "download failed; manual file"
  }
  df <- readr::read_csv(dest, show_col_types = FALSE, progress = FALSE, col_types = "cd") %>%
    arrange(month)
  log_retrieval("multpl CAPE", url, max(df$month), note)
  message(sprintf("  %-13s %s .. %s  (%s)", "multpl CAPE", min(df$month), max(df$month), note))
  df
}

# Geopolitical Risk index (Caldara and Iacoviello 2022). Column GPR is the
# benchmark series (10 newspapers, from 1985).
GPR_URL <- "https://www.matteoiacoviello.com/gpr_files/data_gpr_export.xls"

# guess_max: the file starts in 1900 but GPR is blank until 1985, so readxl's
# default 1000-row type guess reads the column as logical and loses every value.
read_gpr_xls <- function(p) readxl::read_excel(p, guess_max = 100000)

is_gpr_xls <- function(p) {
  x <- tryCatch(read_gpr_xls(p), error = function(e) NULL)
  !is.null(x) && all(c("month", "GPR") %in% names(x)) && is.numeric(x$GPR)
}

gpr <- function() {
  f <- get_file("GPR", GPR_URL, "data_gpr_export.xls", is_gpr_xls,
                "data_gpr_export.xls from https://www.matteoiacoviello.com/gpr.htm")
  if (is.null(f)) return(NULL)
  out <- read_gpr_xls(f$dest) %>%
    transmute(month = format(as.Date(month), "%Y-%m"),
              value = as.numeric(GPR)) %>%
    filter(!is.na(month), !is.na(value)) %>%
    arrange(month)
  # 11 September 2001 is the largest spike in the series; its absence means
  # the wrong column was read.
  stopifnot("GPR series has no spike in 2001-09" =
              isTRUE(out$value[out$month == "2001-09"] > 2 * mean(out$value)))
  log_retrieval("GPR", f$url, max(out$month), f$note)
  message(sprintf("  %-13s %s .. %s  (%s)", "GPR", min(out$month), max(out$month), f$note))
  out
}

# ---- Download everything ----------------------------------------------------------
raw_vix   <- fred("VIXCLS", "1990-01-01", vix_from_cboe,          # S: VIX, daily
                  paste("VIX daily close, columns observation_date,VIXCLS, from",
                        "https://fred.stlouisfed.org/series/VIXCLS or the CLOSE column of",
                        "https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv"))
raw_ndq   <- fred("NASDAQCOM", "1971-02-01", NULL,                # C: Nasdaq Composite, daily
                  paste("Nasdaq Composite daily close, columns observation_date,NASDAQCOM, from",
                        "https://fred.stlouisfed.org/series/NASDAQCOM or ^IXIC on Yahoo Finance"))
raw_debt  <- fred("BCNSDODNS", "1970-01-01", NULL,                # L: nonfinancial corporate debt, quarterly
                  paste("nonfinancial corporate debt, billions of dollars, quarterly, columns",
                        "observation_date,BCNSDODNS with quarter-start dates, from",
                        "https://fred.stlouisfed.org/series/BCNSDODNS (Federal Reserve Z.1 series",
                        "FL104104005.Q, https://www.federalreserve.gov/releases/z1/)"))
raw_nfci  <- fred("NFCILEVERAGE", "1971-01-01", nfci_from_chicagofed,   # L: NFCI leverage, weekly
                  paste("NFCI leverage subindex, weekly, columns observation_date,NFCILEVERAGE, from",
                        "https://fred.stlouisfed.org/series/NFCILEVERAGE or the Leverage column of",
                        "https://www.chicagofed.org/-/media/publications/nfci/nfci-data-series-csv.csv"))
raw_shill <- shiller()                                            # V and C: CAPE and S&P 500
raw_capeR <- multpl_cape()                                        # V: recent CAPE
raw_gpr   <- gpr()                                                # S: geopolitical risk, monthly

readr::write_csv(bind_rows(retrievals), file.path(DIR_RAW, "RETRIEVED.csv"))
message("  retrieval log: ", file.path(DIR_RAW, "RETRIEVED.csv"))

# The file list goes to message(): R cuts error messages off at 1000 characters.
if (length(failures)) {
  dir.create(DIR_MANUAL, showWarnings = FALSE, recursive = TRUE)
  message("\nCould not download: ", paste(names(failures), collapse = ", "), ".\n",
          "To finish the refresh, save each file below in ", DIR_MANUAL,
          "/ and run the refresh again:\n",
          paste0("  ", failures, collapse = "\n"))
  stop("Refresh stopped: ", length(failures), " file(s) missing, listed above.", call. = FALSE)
}
