# Temperature: Antarctic temperature from the EPICA Dome C ice core (800,000 years) and NASA's
# global instrumental record. They are different quantities on different baselines, so they are
# two labelled series rather than one stitched curve.

EPICA_TEMP_URL <- "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt"
GISS_URL <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"

# Free-text header, then rows of: sample number, depth (m), gas age on the EDC3 scale (years
# before 1950), deuterium (per mil), temperature difference from the last millennium (deg C).
# The top few rows have no deuterium or temperature, so only five-column rows are kept.
fetch_temp_epica <- function() {
  lines <- str_split_1(http_get_text(EPICA_TEMP_URL), "\r?\n")
  rows <- lines[str_detect(lines, "^\\s*[0-9]+\\s+[0-9.]+\\s+[0-9.]+\\s+-?[0-9.]+\\s+-?[0-9.]+\\s*$")]
  if (length(rows) < 4000) stop("only ", length(rows), " data rows found; the file layout has changed")
  read_table(
    I(paste(rows, collapse = "\n")),
    col_names = c("sample", "depth_m", "age_bp", "deuterium", "delta_temp_c"), col_types = "ddddd"
  )
}

# NASA GISTEMP v4, global land-ocean anomaly against 1951-1980, one column per month. "***" marks
# months not yet published. Reshaped to one row per month.
fetch_temp_giss <- function() {
  lines <- str_split_1(http_get_text(GISS_URL), "\r?\n")
  header <- str_which(lines, "^Year,Jan,")
  if (length(header) == 0) stop("GISTEMP header row not found; the file layout has changed")

  read_csv(
    I(paste(lines[header[1]:length(lines)], collapse = "\n")),
    na = c("***", ""), show_col_types = FALSE, progress = FALSE
  ) |>
    select(Year, all_of(month.abb)) |>
    pivot_longer(all_of(month.abb), names_to = "month", values_to = "anomaly_c") |>
    transmute(decimal_year = Year + (match(month, month.abb) - 0.5) / 12, anomaly_c) |>
    drop_na()
}

register_source(new_source(
  id = "temp_epica",
  title = "EPICA Dome C temperature (800,000 years)",
  url = "https://www.ncei.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/",
  citation = "Jouzel, J. et al. (2007). Orbital and millennial Antarctic climate variability over the past 800,000 years. Science 317, 793-797. NOAA NCEI.",
  fetch = fetch_temp_epica,
  max_age_days = 180,
  min_rows = 4000
))

register_source(new_source(
  id = "temp_giss",
  title = "NASA GISTEMP global temperature",
  url = "https://data.giss.nasa.gov/gistemp/",
  citation = "GISTEMP Team. GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies. Lenssen, N. et al. (2019), J. Geophys. Res. Atmos. 124.",
  fetch = fetch_temp_giss,
  max_age_days = 7,  # updated monthly
  min_rows = 1500
))

register_metric("temperature", needs = c("temp_epica", "temp_giss"), build = function(src) {
  data <- bind_rows(
    src$temp_epica |> transmute(series = "epica", t = bp_to_year(age_bp), y = delta_temp_c),
    src$temp_giss |> transmute(series = "giss", t = decimal_year, y = anomaly_c)
  ) |>
    arrange(series, t)

  new_metric(
    id = "temperature", label = "Temperature", group = "climate", kind = "line",
    data = data,
    y_title = "Temperature anomaly (°C)", y_unit = "°C", y_format = "+.2f",
    y_zeroline = TRUE, heat = "zero_centred", y_top_label = "warmer", y_bottom_label = "cooler",
    # Points are coloured blue (below the baseline) to red (above), so the key is a neutral grey.
    legend = tibble(
      series = c("epica", "giss"),
      label = c("Antarctica, EPICA Dome C (vs the last 1,000 years)", "Global, NASA GISS (vs 1951–1980)"),
      colour = "#6c757d"
    ),
    default_on = TRUE,
    description = "Temperature relative to a baseline: Antarctica over the last 800,000 years from an ice core (Antarctic changes were larger than the global average), and the global average since 1880 from thermometers. Blue is cooler than the baseline, red warmer.",
    sources = c("temp_epica", "temp_giss")
  )
})
