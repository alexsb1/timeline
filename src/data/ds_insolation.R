# Milankovitch forcing: July sunshine at 65 degrees north, from Berger & Loutre's (1991) orbital
# solution as published by NOAA. The cycles in this curve (set by the shape and tilt of Earth's
# orbit and the wobble of its axis) are what paces the ice ages.

ORBIT_URL <- "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/orbit91"

# A short header, then one row per thousand years back to 5 million years ago:
# time (kyr, negative for the past; 0 = 1950 CE), eccentricity, longitude of perihelion, obliquity,
# precession, then mid-month insolation (W/m2) at 65N in July, 65S in January, 15N in July, 15S in January.
fetch_insolation <- function() {
  lines <- str_split_1(http_get_text(ORBIT_URL), "\r?\n")
  rows <- lines[str_detect(lines, "^\\s*-?[0-9]+(\\s+-?[0-9.]+){8}\\s*$")]
  if (length(rows) < 4000) stop("only ", length(rows), " data rows found; the file layout has changed")
  read_table(
    I(paste(rows, collapse = "\n")),
    col_names = c("age_kyr", "eccentricity", "perihelion", "obliquity", "precession", "july_65n", "jan_65s", "july_15n", "jan_15s"),
    col_types = "ddddddddd"
  )
}

register_source(new_source(
  id = "insolation",
  title = "Orbital variations and insolation (Berger & Loutre 1991)",
  url = "https://www.ncei.noaa.gov/pub/data/paleo/climate_forcing/orbital_variations/insolation/",
  citation = "Berger, A. & Loutre, M. F. (1991). Insolation values for the climate of the last 10 million years. Quaternary Science Reviews 10, 297-317. NOAA NCEI.",
  fetch = fetch_insolation,
  max_age_days = 180,
  min_rows = 4000
))

register_metric("insolation", needs = "insolation", build = function(src) {
  new_metric(
    id = "insolation", label = "Summer sunshine at 65°N (Milankovitch)", group = "climate", kind = "line",
    # age_kyr is negative for the past, so the year is 1950 plus (age x 1000).
    data = src$insolation |>
      transmute(series = "insolation", t = bp_to_year(-age_kyr * 1e3), y = july_65n) |>
      arrange(t),
    y_title = "July insolation, 65°N (W/m²)", y_unit = "W/m²", y_format = ".0f",
    legend = tibble(series = "insolation", label = "July insolation at 65°N", colour = "#2E8B57"),
    description = "Sunshine reaching 65°N in July, which varies as Earth's orbit and tilt change. These Milankovitch cycles pace the ice ages: cool summers in the far north let snow survive and ice sheets grow.",
    sources = "insolation"
  )
})
