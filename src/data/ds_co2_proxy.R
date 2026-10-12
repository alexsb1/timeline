# Atmospheric CO2 over the last ~420 million years, reconstructed from geochemical and fossil
# proxies, with uncertainty: Foster, Royer & Lunt (2017). Registers a SOURCE only; the CO2 panel
# (ds_co2.R) joins this to the ice-core record.
#
# Where the data lives: the journal's own supplementary file ("Supplementary Data 2: LOESS fit to
# the CO2 data set"), an Excel workbook on Springer Nature's static media server. Nothing better
# exists as a plain-text file: NOAA NCEI and Zenodo (the CenCO2PIP "Paleo-CO2 Data Archive") hold
# the individual proxy estimates, not this smoothed curve with its probability intervals, and the
# CenCO2PIP products stop at 66 Ma. This workbook is therefore a runtime dependency on `readxl`.

CO2_PROXY_URLS <- c(
  "https://media.springernature.com/original/springer-static/esm/art%3A10.1038%2Fncomms14845/MediaObjects/41467_2017_BFncomms14845_MOESM2875_ESM.xlsx",
  # The older host for the same file, in case the first is retired.
  "https://static-content.springer.com/esm/art%3A10.1038%2Fncomms14845/MediaObjects/41467_2017_BFncomms14845_MOESM2875_ESM.xlsx"
)

# The workbook's column headings, in order, and the tidy names we give them.
CO2_PROXY_HEADINGS <- c("Age (Ma)", "pCO2 probability maximum", "lw95%", "lw68%", "up68%", "up95%")
CO2_PROXY_COLUMNS <- c("age_ma", "co2_ppm", "lower95", "lower68", "upper68", "upper95")

# Download one of `urls` to a temporary .xlsx file and return its path. A binary download, so
# http_get_text() is no use; this retries and tries each URL in turn.
download_co2_proxy_workbook <- function(urls = CO2_PROXY_URLS, tries = 2) {
  path <- tempfile(fileext = ".xlsx")
  last_problem <- "unknown error"
  for (url in urls) {
    for (attempt in seq_len(tries)) {
      handle <- curl::new_handle(timeout = 30, connecttimeout = 15, useragent = USER_AGENT, followlocation = TRUE)
      result <- tryCatch(curl::curl_download(url, path, handle = handle, quiet = TRUE), error = identity)
      if (inherits(result, "error")) {
        last_problem <- conditionMessage(result)
      } else if (!identical(readBin(path, "raw", 2), charToRaw("PK"))) {
        # An .xlsx is a zip archive. Anything else is an error or consent page served with HTTP 200.
        last_problem <- "the download is not an Excel workbook"
      } else {
        return(path)
      }
      if (attempt < tries) Sys.sleep(2)
    }
  }
  stop("Could not download the Foster et al. (2017) CO2 workbook (", last_problem, ")", call. = FALSE)
}

# Returns age_ma, co2_ppm (the most probable value), and the 68% and 95% probability limits, with
# NA where there is no interval. The workbook's single sheet has a one-line title, then a
# header row, then 840 rows of 0.5-million-year steps from 0.0039 to 419.5 Ma.
fetch_co2_proxy <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("the 'readxl' package is needed to read the CO2 workbook (install.packages(\"readxl\"))", call. = FALSE)
  }
  path <- download_co2_proxy_workbook()
  on.exit(unlink(path), add = TRUE)

  raw <- readxl::read_excel(path, sheet = 1, skip = 1, col_types = "numeric", .name_repair = "minimal")
  if (!identical(str_squish(names(raw)), CO2_PROXY_HEADINGS)) {
    stop("CO2 workbook columns changed: expected ", paste(CO2_PROXY_HEADINGS, collapse = " | "),
         "; got ", paste(names(raw), collapse = " | "), call. = FALSE)
  }

  co2 <- raw |>
    set_names(CO2_PROXY_COLUMNS) |>
    filter(!is.na(age_ma)) |>
    arrange(age_ma) |>
    # Column order as the contract asks: best estimate, then the 68% limits, then the 95%.
    select(age_ma, co2_ppm, lower68, upper68, lower95, upper95)

  # The 95% lower limit of this fit dips below zero ppm in the deep past (a symmetric-looking
  # interval on a quantity that cannot be negative). It is kept as published; whoever draws the
  # band should use the 68% limits or clip at zero.
  if (nrow(co2) < 500) stop("CO2 workbook has only ", nrow(co2), " rows (expected about 840)", call. = FALSE)
  if (anyNA(co2$co2_ppm)) stop("CO2 workbook has missing CO2 estimates", call. = FALSE)
  if (min(co2$age_ma) < 0 || max(co2$age_ma) > 600) stop("CO2 ages fall outside 0-600 Ma", call. = FALSE)
  if (min(co2$age_ma) > 5 || max(co2$age_ma) < 400) {
    stop("CO2 record no longer spans from near 0 to ~420 Ma (", min(co2$age_ma), "-", max(co2$age_ma), " Ma)", call. = FALSE)
  }
  if (anyDuplicated(co2$age_ma)) stop("CO2 workbook has repeated ages", call. = FALSE)
  if (any(co2$co2_ppm < 50 | co2$co2_ppm > 10000)) {
    stop("CO2 estimates outside 50-10,000 ppm: ", paste(range(round(co2$co2_ppm)), collapse = " to "), call. = FALSE)
  }
  # The limits must nest around the best estimate, or the columns have shifted.
  nested <- co2 |>
    mutate(ok = lower95 <= lower68 & lower68 <= co2_ppm & co2_ppm <= upper68 & upper68 <= upper95)
  if (!all(nested$ok, na.rm = TRUE)) stop("CO2 probability limits are not nested around the estimate", call. = FALSE)

  co2
}

register_source(new_source(
  id = "co2_proxy",
  title = "Phanerozoic atmospheric CO2 from proxies (Foster et al. 2017)",
  url = "https://doi.org/10.1038/ncomms14845",
  citation = "Foster, G. L., Royer, D. L. & Lunt, D. J. (2017). Future climate forcing potentially without precedent in the last 420 million years. Nature Communications 8, 14845. doi:10.1038/ncomms14845. Supplementary Data 2 (LOESS fit to the CO2 compilation). Licence: CC BY 4.0.",
  fetch = fetch_co2_proxy,
  max_age_days = 180,  # a published reconstruction: it does not change
  min_rows = 500L
))
