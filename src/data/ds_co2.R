# Atmospheric CO2: the 800,000-year ice-core composite, the Mauna Loa instrument record, and
# (when available) the deeper proxy reconstruction from ds_co2_proxy.R.

ICE_CO2_URL <- "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/antarctica2015co2composite.txt"
MAUNA_LOA_URL <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.csv"

# NOAA's composite of Antarctic ice cores (Bereiter et al. 2015): a text header, then tab-separated
# rows of gas age (years before 1950), CO2 (ppm) and its uncertainty.
fetch_co2_ice <- function() {
  lines <- str_split_1(http_get_text(ICE_CO2_URL), "\r?\n")
  rows <- lines[str_detect(lines, "^[0-9.]+\\t[0-9.]+\\t[0-9.]+\\s*$")]
  if (length(rows) < 1000) stop("only ", length(rows), " data rows found; the file layout has changed")
  read_tsv(I(paste(rows, collapse = "\n")), col_names = c("age_bp", "co2_ppm", "sigma_ppm"), col_types = "ddd")
}

# NOAA GML's monthly mean CO2 at Mauna Loa (1958 to the latest complete month). Comment lines
# start with "#"; -9.99 marks a missing month.
fetch_co2_modern <- function() {
  lines <- str_split_1(http_get_text(MAUNA_LOA_URL), "\r?\n")
  body <- lines[!str_starts(lines, "#") & nzchar(lines)]
  data <- read_csv(I(paste(body, collapse = "\n")), show_col_types = FALSE, progress = FALSE)
  required <- c("decimal date", "average")
  if (!all(required %in% names(data))) stop("Mauna Loa file is missing expected columns: ", paste(setdiff(required, names(data)), collapse = ", "))
  data |>
    transmute(decimal_year = `decimal date`, co2_ppm = average) |>
    filter(co2_ppm > 0)
}

register_source(new_source(
  id = "co2_ice",
  title = "Antarctic ice-core CO2 composite (800,000 years)",
  url = "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/antarctica2015co2composite.txt",
  citation = "Bereiter, B. et al. (2015). Revision of the EPICA Dome C CO2 record from 800 to 600 kyr before present. Geophysical Research Letters 42, 542-549. NOAA NCEI.",
  fetch = fetch_co2_ice,
  max_age_days = 180,
  min_rows = 1000
))

register_source(new_source(
  id = "co2_modern",
  title = "Mauna Loa monthly CO2",
  url = "https://gml.noaa.gov/ccgg/trends/",
  citation = "Lan, X., Tans, P. & Thoning, K. W. NOAA Global Monitoring Laboratory, Mauna Loa monthly mean CO2 (gml.noaa.gov/ccgg/trends).",
  fetch = fetch_co2_modern,
  max_age_days = 7,  # a new month is published every few weeks
  min_rows = 600
))

register_metric(
  "co2", needs = c("co2_ice", "co2_modern"), optional = "co2_proxy",
  build = function(src) {
    series <- list(
      ice = src$co2_ice |>
        transmute(series = "ice", t = bp_to_year(age_bp), y = co2_ppm, ymin = NA_real_, ymax = NA_real_),
      modern = src$co2_modern |>
        transmute(series = "modern", t = decimal_year, y = co2_ppm, ymin = NA_real_, ymax = NA_real_)
    )
    legend <- tibble(
      series = c("ice", "modern"),
      label = c("Ice cores (800,000 years)", "Mauna Loa (direct measurement)"),
      colour = c(okabe_ito[["vermillion"]], okabe_ito[["black"]])
    )

    # The older proxy reconstruction is optional: the panel is still useful without it.
    if (!is.null(src$co2_proxy)) {
      has_band <- !anyNA(src$co2_proxy$lower68) && !anyNA(src$co2_proxy$upper68)
      series$proxy <- src$co2_proxy |>
        transmute(
          series = "proxy", t = ma_to_year(age_ma), y = co2_ppm,
          ymin = if (has_band) lower68 else NA_real_, ymax = if (has_band) upper68 else NA_real_
        )
      legend <- bind_rows(
        tibble(series = "proxy", label = "Proxy reconstruction (68% range)", colour = okabe_ito[["blue"]]),
        legend
      )
    }

    new_metric(
      id = "co2", label = "Carbon dioxide (CO₂)", group = "climate", kind = "line",
      data = list_rbind(unname(series)) |> arrange(series, t),
      y_title = "CO<sub>2</sub> (ppm)", y_unit = "ppm", y_format = ",.0f",
      legend = legend, default_on = TRUE,
      description = "Atmospheric CO₂ from the geological past to today: proxy reconstructions over hundreds of millions of years, Antarctic ice cores over the last 800,000, and direct measurement at Mauna Loa since 1958.",
      sources = c("co2_ice", "co2_modern", if (!is.null(src$co2_proxy)) "co2_proxy")
    )
  }
)
