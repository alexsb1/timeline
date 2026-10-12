# Meteorite impact structures on Earth: where and when, and how big.
#
# Source: Wikipedia's "List of impact structures on Earth". It is the maintained, machine-
# readable descendant of the Earth Impact Database (PASSC, University of New Brunswick), which
# the first version of this panel used: that database lived at passc.net, and the domain now
# serves a parking page (the University's own pages still link to it), so it can no longer be
# fetched. Wikidata was tried and rejected: it has diameters but almost no ages for craters.
# The Wikipedia list covers the same ~190 confirmed structures, with ages that are often more
# recent than the database's, but they come in many shapes ("66.051 ± 0.031", "480–500",
# "less than 120", "~250"), so most of this file is about reading them honestly.

IMPACT_PAGE <- "List of impact structures on Earth"

# A structure is placed on the timeline only if its age is known well enough to place. Each age
# is a window (youngest to oldest bound): a plain or "±" age is a zero-width window, "480–500"
# is 480 to 500, and "less than 120" is 0 to 120. The window's half-width relative to its
# midpoint, (oldest - youngest) / (oldest + youngest), must not exceed this, i.e. the oldest
# bound is at most five thirds of the youngest. That keeps "453–457" and "48–56" but drops
# "34–541" and every bare "less than ...", which would otherwise be pinned to an arbitrary spot.
# Structures kept at a window's midpoint say so in their tooltip.
IMPACT_MAX_RELATIVE_WIDTH <- 0.25

# Numbers in a piece of text: "66.051 ± 0.031" -> 66.051, 0.031 (thousands separators ignored).
impact_numbers <- function(x) map(str_extract_all(str_remove_all(x, ","), "[0-9]+(?:\\.[0-9]+)?"), as.numeric)

# Read the age column. `unit_years` is the length of the column's unit (1,000 for ka, 1e6 for Ma).
# Returns how the age is stated ("point", "range", "limit", or NA when it is not a number), the
# window (age_min..age_max, in years before present) and, for "x ± e", the error.
parse_impact_age <- function(text, unit_years) {
  numbers <- impact_numbers(text)
  first <- map_dbl(numbers, \(n) n[1])   # NA when there is no number
  second <- map_dbl(numbers, \(n) n[2])  # NA when there is only one

  is_limit <- str_detect(text, regex("^(likely\\s+)?(less than|<)", ignore_case = TRUE))
  is_range <- !is_limit & str_detect(text, "[0-9]\\s*[–-]\\s*[0-9]")
  kind <- case_when(is.na(first) ~ NA_character_, is_limit ~ "limit", is_range ~ "range", .default = "point")

  tibble(
    age_kind = kind,
    age_min_years = case_when(kind == "limit" ~ 0, kind == "range" ~ pmin(first, second), kind == "point" ~ first) * unit_years,
    age_max_years = case_when(kind == "limit" ~ first, kind == "range" ~ pmax(first, second), kind == "point" ~ first) * unit_years,
    age_error_years = if_else(kind == "point" & str_detect(text, "±"), second, NA_real_) * unit_years
  )
}

# The first table has a "Date" column for the youngest structures, in calendar years ("~1800 AD",
# "1500 BC"); a bound such as "< 5000 BC" is not a date and is left to the age column.
parse_impact_date <- function(text) {
  parts <- str_match(text, "^~?\\s*([0-9]+)\\s*(AD|BC)")
  calendar_to_year(as.numeric(parts[, 2]), era = if_else(parts[, 3] == "BC", "BCE", "CE"))
}

# A diameter in km, which may be a range ("65–75") or approximate ("~25"): the midpoint.
parse_impact_diameter <- function(text) {
  map_dbl(impact_numbers(text), \(n) if (length(n) == 0) NA_real_ else mean(head(n, 2)))
}

# One of the article's tables of confirmed structures. The tables are split by age, and the
# unit of the age column (thousand or million years) is only in its heading.
parse_impact_table <- function(table) {
  raw <- rvest::html_table(table, convert = FALSE)
  names(raw) <- str_squish(names(raw))

  find_column <- function(pattern) {
    column <- str_subset(names(raw), pattern)
    if (length(column) != 1) stop("impact table lacks a single column matching ", pattern, call. = FALSE)
    column
  }
  age_column <- find_column("^Age")
  age_unit <- case_when(
    str_detect(age_column, regex("\\bka\\b", ignore_case = TRUE)) ~ "ka",
    str_detect(age_column, regex("million|\\bMa\\b", ignore_case = TRUE)) ~ "Ma",
    .default = NA_character_
  )
  if (is.na(age_unit)) stop("cannot tell the unit of the impact age column '", age_column, "'", call. = FALSE)
  unit_years <- if (age_unit == "ka") 1e3 else 1e6

  age_text <- clean_text(raw[[age_column]])
  diameter_text <- clean_text(raw[[find_column("^Diameter")]])
  date_text <- if ("Date" %in% names(raw)) clean_text(raw$Date) else rep(NA_character_, nrow(raw))

  bind_cols(
    tibble(
      name = clean_text(raw$Name), location = clean_text(raw$Location), country = clean_text(raw$Country),
      diameter_km = parse_impact_diameter(diameter_text), diameter_text = diameter_text,
      age_text = age_text, age_unit = age_unit
    ),
    parse_impact_age(age_text, unit_years),
    tibble(date_year = parse_impact_date(date_text))
  )
}

# Returns one row per confirmed structure: name, location, country, diameter_km (the midpoint of
# a stated range) and diameter_text as published, then the age as published (age_text, age_unit),
# as numbers (age_kind, age_min_years, age_max_years, age_error_years, in years before present)
# and, for the youngest, a calendar date (date_year). Structures without a usable age are kept;
# the metric decides which to draw.
fetch_impacts <- function() {
  doc <- wikipedia_html(IMPACT_PAGE)
  # Wikipedia pads numbers with hidden text so that columns sort ("00" before "0.9"). It is
  # invisible on the page but ends up in the text, so remove it, and the inline stylesheets.
  xml2::xml_remove(xml2::xml_find_all(
    doc, "//style | //span[@aria-hidden='true'] | //span[contains(@style, 'display:none')]"
  ))
  # Only the tables under the "Confirmed impact structures" heading; later ones list inferred
  # events and statistics.
  tables <- xml2::xml_find_all(
    doc, "//table[contains(@class, 'wikitable')][preceding::h2[1][contains(., 'Confirmed')]]"
  )
  if (length(tables) < 4) stop("expected several tables of confirmed impact structures, found ", length(tables))

  impacts <- map(tables, parse_impact_table) |> list_rbind() |> filter(!is.na(name))

  if (nrow(impacts) < 150) stop("only ", nrow(impacts), " impact structures found (expected about 200)")
  if (mean(!is.na(impacts$diameter_km)) < 0.95) stop("many impact diameters could not be read")
  if (any(impacts$diameter_km < 0.005 | impacts$diameter_km > 400, na.rm = TRUE)) {
    stop("impact diameters outside 5 m to 400 km")
  }
  if (mean(!is.na(impacts$age_kind)) < 0.9) stop("many impact ages could not be read")
  if (any(impacts$age_max_years > 4.6e9, na.rm = TRUE)) stop("an impact is older than the Earth")

  # Two landmarks catch shifted columns or a wrong age unit.
  landmark <- \(pattern) filter(impacts, str_detect(name, pattern))
  chicxulub <- landmark("^Chicxulub")
  vredefort <- landmark("^Vredefort")
  if (nrow(chicxulub) != 1 || abs(chicxulub$age_min_years - 66e6) > 3e6 || abs(chicxulub$diameter_km - 150) > 40) {
    stop("Chicxulub is missing or no longer 66 Ma and about 150 km")
  }
  if (nrow(vredefort) != 1 || abs(vredefort$age_min_years - 2023e6) > 100e6 || abs(vredefort$diameter_km - 160) > 40) {
    stop("Vredefort is missing or no longer 2023 Ma and about 160 km")
  }

  impacts
}

register_source(new_source(
  id = "impacts",
  title = "Confirmed impact structures on Earth (Wikipedia)",
  url = "https://en.wikipedia.org/wiki/List_of_impact_structures_on_Earth",
  citation = "Wikipedia contributors, “List of impact structures on Earth”, Wikipedia, The Free Encyclopedia (CC BY-SA 4.0). Compiled from the Earth Impact Database (Planetary and Space Science Centre, University of New Brunswick) and the primary literature.",
  fetch = fetch_impacts,
  max_age_days = 30,
  min_rows = 150L
))

register_metric("meteorites", needs = "impacts", build = function(src) {
  data <- src$impacts |>
    mutate(
      relative_width = (age_max_years - age_min_years) / (age_max_years + age_min_years),
      # A calendar date wins; otherwise the middle of the age window, if the window is narrow.
      t = case_when(
        !is.na(date_year) ~ date_year,
        age_kind %in% c("point", "range") & relative_width <= IMPACT_MAX_RELATIVE_WIDTH ~
          bp_to_year((age_min_years + age_max_years) / 2)
      ),
      # The Ma/ka figure and its error or range, only when it says more than the "When" line.
      age_note = case_when(
        is.na(date_year) & age_kind == "range" ~ paste0(age_text, " ", age_unit, " (midpoint shown)"),
        is.na(date_year) & !str_detect(age_text, "^[0-9.]+$") ~ paste(age_text, age_unit)
      ),
      place = if_else(is.na(country) | country == location, location, paste(location, country, sep = ", "))
    ) |>
    filter(!is.na(t), !is.na(diameter_km)) |>
    arrange(t) |>
    transmute(
      t, t0 = t, t1 = t, y = diameter_km, size = marker_size(diameter_km), colour = "#C2185B",
      title = name,
      body = hover_lines(
        Location = wrap_html(place),
        Diameter = paste(htmltools::htmlEscape(diameter_text), "km"),
        `Published age` = wrap_html(age_note)
      )
    )

  new_metric(
    id = "meteorites", label = "Meteorite impact craters", group = "earth", kind = "events",
    data = data, y_title = "Crater diameter (km)", y_type = "log", default_on = FALSE,
    description = "Confirmed meteorite impact structures on Earth, from giant ancient basins to craters made within the last few thousand years. Height and marker size both show crater diameter; structures whose age is only loosely known are left out.",
    sources = "impacts"
  )
})
