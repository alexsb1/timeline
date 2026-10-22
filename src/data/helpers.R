# Shared helpers for the data layer: fetching, cleaning, and describing datasets.
#
# HOW DATA FLOWS
#   source  - one downloadable dataset. `fetch()` downloads it and returns a TIDY tibble.
#             Cached in data/sources/<id>.rds and refreshed when older than `max_age_days`.
#   metric  - one panel in the app. `build(src)` turns the tidy source tibbles it `needs` into
#             a metric (see new_metric()). Cheap; runs at every start-up.
#   Each dataset lives in its own file, src/data/ds_<name>.R, which registers its source(s) and
#   metric with register_source() / register_metric(). Panel order follows src/data/registry.R.
#
# TIME
#   Everything is stored as a CALENDAR YEAR, `t`: a decimal year in astronomical numbering
#   (1 CE = 1, 1 BCE = 0, 2 BCE = -1, ...), so 1066-10-14 is about 1066.8 and 66 million years
#   ago is -65,998,050. This does not depend on today's date; the app converts to its x-axis
#   (years elapsed since the Earth formed) and to "years ago" when it draws.
#   Use the converters below rather than doing the arithmetic by hand.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
})

USER_AGENT <- "ClimateTimeline/2.0 (educational Shiny app; https://github.com/alexsb1/timeline)"
CURATED_DIR <- "curated"

# `%||%` is only in base R from 4.4.
`%||%` <- function(x, y) if (is.null(x)) y else x


# ---- fetching -------------------------------------------------------------

# GET a URL and return the body as UTF-8 text. Retries a couple of times; stops with a clear
# message otherwise. `query` is a named list of URL parameters (escaped for you).
http_get_text <- function(url, query = list(), headers = list(), timeout = 45, tries = 3) {
  if (length(query) > 0) {
    pairs <- paste0(names(query), "=", map_chr(query, \(v) curl::curl_escape(as.character(v))))
    url <- paste0(url, if (str_detect(url, fixed("?"))) "&" else "?", paste(pairs, collapse = "&"))
  }

  last_problem <- "unknown error"
  for (attempt in seq_len(tries)) {
    handle <- curl::new_handle(
      timeout = timeout, connecttimeout = 15, useragent = USER_AGENT, followlocation = TRUE
    )
    if (length(headers) > 0) curl::handle_setheaders(handle, .list = headers)

    response <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = identity)
    if (inherits(response, "error")) {
      last_problem <- conditionMessage(response)
    } else if (response$status_code == 200) {
      text <- rawToChar(response$content)
      Encoding(text) <- "UTF-8"
      return(str_remove(text, paste0("^", intToUtf8(0xFEFF))))  # drop a byte-order mark
    } else {
      last_problem <- paste("HTTP", response$status_code)
    }
    if (attempt < tries) Sys.sleep(2 * attempt)
  }
  stop("Could not fetch ", url, " (", last_problem, ")", call. = FALSE)
}

# Run a SPARQL query against Wikidata and return the result as a tibble (every column is read
# as it comes: dates are ISO strings such as "1066-10-14T00:00:00Z"; BCE years are negative and
# in ASTRONOMICAL numbering, so "-0043-03-15" is 15 March 44 BCE. Convert with iso_to_year()).
# Wikidata is CC0. Keep queries selective: the public endpoint times out after 60 seconds.
wikidata_query <- function(sparql, timeout = 90) {
  text <- http_get_text(
    "https://query.wikidata.org/sparql",
    query = list(query = sparql),
    headers = list(Accept = "text/csv"),
    timeout = timeout
  )
  read_csv(I(text), show_col_types = FALSE, progress = FALSE, na = "")
}

# The HTML of a Wikipedia article (as an xml2 document), with footnote markers removed.
wikipedia_html <- function(page) {
  response <- http_get_text(
    "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "parse", page = page, prop = "text", format = "json", formatversion = "2",
      redirects = "1"
    )
  )
  doc <- xml2::read_html(jsonlite::fromJSON(response)$parse$text)
  xml2::xml_remove(xml2::xml_find_all(doc, "//sup[contains(@class, 'reference')]"))
  doc
}

# Every `wikitable` in a Wikipedia article, as a list of tibbles in page order.
wikipedia_tables <- function(page) {
  wikipedia_html(page) |>
    rvest::html_elements("table.wikitable") |>
    map(\(table) rvest::html_table(table, fill = TRUE))
}

# A hand-written input kept in curated/ (for things no source publishes, e.g. the religion layer).
read_curated <- function(file) {
  read_csv(file.path(CURATED_DIR, file), show_col_types = FALSE, progress = FALSE) |>
    mutate(across(where(is.character), clean_text))
}


# ---- cleaning -------------------------------------------------------------

# Collapse non-breaking spaces and stray whitespace; empty strings become NA.
clean_text <- function(x) {
  odd_spaces <- paste0("[", intToUtf8(c(160, 8201, 8239)), "]")
  x |>
    str_replace_all(odd_spaces, " ") |>
    str_squish() |>
    na_if("")
}

# Numbers written with thousands separators, units or footnote marks: "1,234 km" -> 1234.
as_number <- function(x) parse_number(as.character(x), na = c("", "NA", "N/A", "-", "?"))


# ---- time -----------------------------------------------------------------
# Every converter returns a calendar year `t` (see the note at the top).

# "Before present" counts back from 1950 CE by convention (ice cores, radiocarbon, geology).
bp_to_year <- function(years_bp) 1950 - years_bp
ka_to_year <- function(ka) bp_to_year(ka * 1e3)

# Millions of years ago. An age of exactly 0 means "the present", which for something ongoing
# (the Phanerozoic, the last glacial cycle...) is today, not 1950. The 76 years between the two
# are invisible at every other age, so only 0 needs the special case.
ma_to_year <- function(ma) if_else(ma == 0, today_year(), bp_to_year(ma * 1e6))

# A year as written in history books ("1066 CE", "44 BCE"), plus an optional month and day.
# There is no year 0, so 1 BCE is astronomical year 0.
calendar_to_year <- function(year, era = "CE", month = NA, day = NA) {
  astronomical <- if_else(rep_len(era, length(year)) == "BCE", 1 - year, year)
  days_before_month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  # as.integer() matters: indexing with a *logical* NA would return all 12 elements.
  within_year <- coalesce((days_before_month[as.integer(month)] + coalesce(day, 1) - 1) / 365, 0)
  astronomical + within_year
}

# ISO date strings as Wikidata returns them ("1066-10-14T00:00:00Z", "-0043-03-15T00:00:00Z").
# Astronomical numbering already, so the sign needs no adjustment. Missing month/day -> January 1.
iso_to_year <- function(iso) {
  parts <- str_match(iso, "^(-?\\d+)-(\\d{2})-(\\d{2})")
  calendar_to_year(
    as.numeric(parts[, 2]), era = "CE",
    month = suppressWarnings(as.integer(parts[, 3])),
    day = suppressWarnings(as.integer(parts[, 4]))
  )
}

# A Date (or date-time) as a decimal year.
date_to_year <- function(date) {
  date <- as.Date(date)
  calendar_to_year(
    as.numeric(format(date, "%Y")), "CE",
    month = as.integer(format(date, "%m")), day = as.integer(format(date, "%d"))
  )
}


# ---- colour ---------------------------------------------------------------

# Named R colours ("hotpink") and hex strings -> "#rrggbb".
to_hex <- function(x) {
  rgb <- grDevices::col2rgb(x)
  grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255)
}

# Black or white label text, whichever is readable on the given fill.
text_on <- function(fill_hex) {
  rgb <- grDevices::col2rgb(fill_hex)
  luminance <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  if_else(luminance > 150, "#1f2328", "#ffffff")
}

# Okabe-Ito: a colour-blind-safe palette for lines and categories.
okabe_ito <- c(
  orange = "#E69F00", sky = "#56B4E9", green = "#009E73", yellow = "#F0E442",
  blue = "#0072B2", vermillion = "#D55E00", purple = "#CC79A7", black = "#222222"
)

# n pastel fills for labelled bars (dark text stays readable on these).
bar_palette <- function(n) grDevices::hcl.colors(max(n, 1), "Pastel 1")[seq_len(n)]


# ---- text -----------------------------------------------------------------

# Escape, then wrap long text to a width that fits a plotly tooltip.
wrap_html <- function(x, width = 60) {
  wrapped <- x |>
    htmltools::htmlEscape() |>
    str_wrap(width = width) |>
    str_replace_all("\n", "<br>")
  if_else(is.na(x), NA_character_, wrapped)
}

shorten <- function(x, n = 30) str_trunc(x, n, ellipsis = "…")

# The detail lines of a tooltip: one "label value" line per non-missing field, each starting
# with a line break. Values must already be safe HTML (use wrap_html() on free text).
#   body = hover_lines(Location = location, Diameter = paste(d, "km"))
# The app adds the bold title and the "When" line (computed from today's date) itself.
hover_lines <- function(...) {
  lines <- imap(list(...), function(value, label) {
    if_else(
      is.na(value),
      "",
      paste0("<br><span style=\"color:#8a8f98\">", label, "</span> ", value)
    )
  })
  if (length(lines) == 0) "" else do.call(paste0, unname(lines))
}

# Plain number, no scientific notation, trailing zeros dropped (0.00007, not 7e-05).
plain_number <- function(x) {
  format(x, scientific = FALSE, big.mark = ",", trim = TRUE, drop0trailing = TRUE)
}


# ---- layout ---------------------------------------------------------------

# Greedy interval scheduling: give overlapping spans different lanes so bars never draw on top
# of each other. Returns a 0-based lane per row; the earliest span claims lane 0.
assign_lanes <- function(start, end) {
  lane_free_at <- numeric(0)
  lane <- integer(length(start))
  for (i in order(start, end)) {
    free <- which(lane_free_at <= start[i])
    lane[i] <- if (length(free)) free[[1]] else length(lane_free_at) + 1L
    lane_free_at[lane[i]] <- end[i]
  }
  lane - 1L
}

# Lane packing for spans that fall into coloured groups (regions, royal houses, religions...), so
# that every row of the panel holds ONE group and so one colour:
#   - each group gets its own block of rows, the blocks stacked in `group_order`;
#   - inside a block, overlapping spans go on separate rows directly below one another (the
#     fewest rows that avoid any overlap), and a span reuses a free row rather than opening a new
#     one, so gaps between same-group spans are preferred to extra rows.
# `group_order` defaults to the order in which the groups first start. Returns list(lane, labels):
# a 0-based lane per span, and one axis label per lane (the group's name on the first row of its
# block, "" on the rest; pass it to new_metric(lane_labels = )).
pack_by_group <- function(start, end, group, group_order = NULL) {
  group <- as.character(group)
  group_order <- group_order %||% unique(group[order(start)])
  unknown <- setdiff(unique(group), group_order)
  if (length(unknown) > 0) stop("pack_by_group: groups not in group_order: ", paste(unknown, collapse = ", "), call. = FALSE)

  lane <- integer(length(start))
  labels <- character()
  for (g in group_order) {
    rows <- which(group == g)
    if (length(rows) == 0) next
    within <- assign_lanes(start[rows], end[rows])
    lane[rows] <- length(labels) + within
    labels <- c(labels, g, rep("", max(within)))
  }
  list(lane = lane, labels = labels)
}

# Marker diameters (px) scaled by the square root of a magnitude, so *area* tracks the value.
marker_size <- function(magnitude, range = c(7, 26)) {
  root <- sqrt(magnitude)
  span <- diff(range(root, na.rm = TRUE))
  if (span == 0) return(rep(mean(range), length(root)))
  range[1] + (root - min(root, na.rm = TRUE)) / span * diff(range)
}


# ---- metrics --------------------------------------------------------------
# A "metric" is one panel in the app. Three kinds, each with a fixed data contract (columns
# beyond these are allowed but ignored):
#
#   line    t, y, series                        a value through time. Optional ymin, ymax draw an
#                                               uncertainty band behind the line.
#   spans   t0, t1, lane, label, fill,          bars with a start and an end. `lane` is a 0-based
#           text_colour, instant, title, body   row; `instant` TRUE draws a marker instead of a bar
#                                               (events lasting a year or less). RULE: a row holds one
#                                               colour. Spans in coloured groups use pack_by_group(),
#                                               which gives each group its own rows; uncoloured spans
#                                               use assign_lanes() and take their colour from the lane.
#   events  t, t0, t1, y, size, colour,         a marker per event at height `y`; t0..t1 is its
#           title, body                         duration, drawn as a line when t1 > t0. `t` is where
#                                               the marker sits (usually the middle).
#
# `title` is the tooltip heading (plain text), `body` the extra tooltip lines (hover_lines()).
# `legend` is tibble(series, label, colour); it names the lines of a `line` metric, and for the
# other kinds is shown as a colour key when it has more than one row.

required_columns <- list(
  line = c("t", "y", "series"),
  spans = c("t0", "t1", "lane", "label", "fill", "text_colour", "instant", "title", "body"),
  events = c("t", "t0", "t1", "y", "size", "colour", "title", "body")
)

# heat: colour a line's points from blue (cold) to red (warm). "low_is_warm" suits d18O,
# "high_is_warm" a value that rises with temperature, "zero_centred" an anomaly (below the
# baseline is blue, above is red).
HEAT_MODES <- c("low_is_warm", "high_is_warm", "zero_centred")

new_metric <- function(id, label, group, kind, data, description,
                       y_title = NULL, y_type = "linear", y_unit = "", y_format = ",.1f",
                       y_zeroline = FALSE, y_hidden = FALSE, y_reversed = FALSE,
                       y_top_label = NULL, y_bottom_label = NULL, heat = NULL,
                       lane_labels = NULL, legend = NULL, sources = character(),
                       default_on = FALSE) {
  stopifnot(kind %in% names(required_columns), is.null(heat) || heat %in% HEAT_MODES)
  missing_cols <- setdiff(required_columns[[kind]], names(data))
  if (length(missing_cols)) {
    stop(id, ": data is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (nrow(data) == 0) stop(id, ": no rows", call. = FALSE)

  time_cols <- switch(kind, line = "t", spans = c("t0", "t1"), events = c("t0", "t1", "t"))
  times <- unlist(data[time_cols])
  if (anyNA(times) || any(!is.finite(times))) stop(id, ": missing or infinite times", call. = FALSE)
  # Allow a little slack at both ends of the Earth's lifetime (1950-based dates vs today).
  if (any(times < bp_to_year(EARTH_AGE) - 1000 | times > today_year() + 1)) {
    stop(id, ": some times fall outside the Earth's lifetime or in the future", call. = FALSE)
  }
  if (kind == "line" && anyNA(data$y)) stop(id, ": line data contains missing y values", call. = FALSE)
  if (kind != "line" && any(data$t1 < data$t0)) stop(id, ": a span ends before it starts", call. = FALSE)
  if (kind != "line" && anyNA(data$title)) stop(id, ": every row needs a title", call. = FALSE)

  structure(
    list(
      id = id, label = label, group = group, kind = kind, description = description,
      data = data, legend = legend, sources = sources, default_on = default_on,
      y_title = y_title %||% label, y_type = y_type, y_unit = y_unit, y_format = y_format,
      y_zeroline = y_zeroline, y_hidden = y_hidden, y_reversed = y_reversed,
      y_top_label = y_top_label, y_bottom_label = y_bottom_label, heat = heat,
      lane_labels = lane_labels,
      n_lanes = if (kind == "spans") max(data$lane) + 1L else NA_integer_,
      t_min = min(times), t_max = max(times)
    ),
    class = "timeline_metric"
  )
}


# ---- sources and the registry ---------------------------------------------

# A downloadable dataset. `fetch()` must return a tidy tibble with at least `min_rows` rows or
# stop() with a clear message; the loader keeps the last good copy if a refresh fails.
#   max_age_days: refetch at start-up when the cached copy is older than this.
#   local: TRUE for hand-written files in curated/ (read on every start-up, never cached).
#   url: the human-facing page (shown in the app's Sources panel). citation: how to credit it.
new_source <- function(id, title, url, citation, fetch, max_age_days = 30, min_rows = 5L,
                       local = FALSE) {
  structure(
    list(
      id = id, title = title, url = url, citation = citation, fetch = fetch,
      max_age_days = max_age_days, min_rows = min_rows, local = local
    ),
    class = "timeline_source"
  )
}

.registry <- new.env(parent = emptyenv())
.registry$sources <- list()
.registry$metrics <- list()

register_source <- function(source) {
  .registry$sources[[source$id]] <- source
  invisible(source)
}

# `needs`: ids of the sources `build` must have. `optional`: sources it can do without; if one
# is unavailable its entry in `src` is NULL and `build` should cope. `build(src)` receives a
# named list of tidy tibbles and returns new_metric(...). Metrics appear in the order they are
# registered.
register_metric <- function(id, needs, build, optional = character()) {
  .registry$metrics[[id]] <- list(id = id, needs = needs, optional = optional, build = build)
  invisible(NULL)
}
