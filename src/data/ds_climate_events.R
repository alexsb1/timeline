# Named climate episodes and mass extinctions, from Wikipedia's "List of periods and events in
# climate history" (CC BY-SA 4.0). The article has three wikitables (deep time in Ma, then the
# Pleistocene and the Holocene in years BC) and a bulleted list for the Common Era, so each is
# read in its own unit.
#
# The parsers it relies on for human-written dates (hist_*) are in helpers_history.R.

# ---- the climate events source -----------------------------------------------

CLIMATE_EVENTS_PAGE <- "List of periods and events in climate history"

# A short name for an event whose description starts with it: the opening link if there is one,
# else the text up to the first comma, semicolon or bracket ("Last Glacial Period, not to be
# confused with..." -> "Last Glacial Period"). A comma inside a number ("30,000") does not count.
climate_clause <- function(text) str_match(text, "^(.+?)(?:, |; |: | \\(|\\. |$)")[, 2]

# Rows of one wikitable: the first column is a date in the table's unit, the second the event.
climate_table_rows <- function(table) {
  header <- table |> xml2::xml_find_first(".//th") |> xml2::xml_text() |> clean_text()
  unit <- case_when(
    str_detect(header, "\\bMya?\\b|\\bMa\\b") ~ "Ma",
    str_detect(header, "\\bBC") ~ "BC",
    .default = NA_character_
  )
  # A table whose dates are in a unit that is not understood would otherwise vanish silently.
  if (is.na(unit)) stop("a table in the climate-history article has a date column '", header, "' that is neither Ma nor BC")

  section <- table |>
    xml2::xml_find_first("preceding::*[self::h2 or self::h3][1]") |>
    xml2::xml_text() |>
    clean_text()

  xml2::xml_find_all(table, ".//tr[td]") |>
    map_dfr(\(row) {
      cells <- xml2::xml_find_all(row, "./td")
      if (length(cells) < 2) return(NULL)
      html <- hist_own_html(cells[[2]])
      tibble(
        section = section, unit = unit, date_text = hist_text(hist_own_html(cells[[1]])),
        text = hist_text(html), link = hist_first_link(html)
      )
    })
}

# Rows of the Common Era list: each bullet has its own dates somewhere in the line, "900-1300
# Medieval Warm Period, ..." or "Great Famine of 1315-1317 in Europe", and some are nested.
climate_list_rows <- function(doc) {
  list_node <- xml2::xml_find_first(doc, "//*[self::h2 or self::h3][contains(., 'Common Era')]/following::ul[1]")
  if (inherits(list_node, "xml_missing")) stop("the Common Era list was not found in the climate-history article")

  xml2::xml_find_all(list_node, ".//li") |>
    map_dfr(\(item) {
      html <- hist_own_html(item)
      text <- hist_text(html)
      # Where the dates are written varies by item: leading ("900-1300 Medieval Warm Period"),
      # inside the name ("Great Famine of 1315-1317") or in a bracket ("(535-536 AD)").
      leading <- str_extract(text, "^[0-9]{3,4}(?:\\s*[-–]\\s*(?:[0-9]{3,4}|present))?(?=\\s|$)")
      inside <- str_extract(text, "\\b[0-9]{3,4}\\s*[-–]\\s*[0-9]{3,4}\\b")
      # "Various dates between 1250 and 1550 or later ... ending ... around 1850": take the
      # earliest proposed start and the stated end.
      vague <- str_match(text, "between ([0-9]{3,4}) and [0-9]{3,4}.*(?:around|about) ([0-9]{3,4})\\b")

      # A leading date is not part of the name, so the name starts after it. A bracketed date
      # ("(535-536 AD)") is dropped from the description: the tooltip states the time itself.
      if (!is.na(leading)) {
        html <- str_replace(html, fixed(leading), "")
        text <- hist_text(html)
      }
      text <- str_remove(text, "\\s*\\([0-9]{3,4}\\s*[-–]\\s*[0-9]{3,4}\\s*(?:AD|CE)?\\)")
      tibble(
        section = "Common Era", unit = "CE",
        date_text = coalesce(leading, inside, if_else(is.na(vague[1, 1]), NA_character_, paste0(vague[1, 2], "-", vague[1, 3]))),
        text = text, link = hist_first_link(html)
      )
    })
}

fetch_climate_events <- function() {
  doc <- wikipedia_html(CLIMATE_EVENTS_PAGE)

  tables <- xml2::xml_find_all(doc, "//table[contains(@class, 'wikitable')]") |>
    map(climate_table_rows) |>
    list_rbind()
  if (nrow(tables) == 0 || !all(c("Ma", "BC") %in% tables$unit)) {
    stop("the climate-history tables were not found with their Ma / BC date columns; the article layout has changed")
  }
  rows <- bind_rows(tables, climate_list_rows(doc))

  events <- rows |>
    mutate(
      name = coalesce(link, climate_clause(text)),
      label = shorten(name, 30),
      title = shorten(climate_clause(text), 60)
    ) |>
    bind_cols(hist_parse_range(rows$date_text, default_era = rows$unit))

  undated <- filter(events, is.na(t0) & !open_start)
  # An undated row is a cross-reference or a note ("Bond Event 2: possibly triggering ...") and
  # can only be dropped, but if many are, the date parsing has broken.
  if (nrow(undated) > 0.15 * nrow(events)) {
    stop(nrow(undated), " of ", nrow(events), " climate events have dates that could not be read, e.g. '",
         undated$date_text[[1]], "'")
  }
  if (nrow(undated) > 0) {
    message("[climate_events] skipped ", nrow(undated), " row(s) without a readable date: ",
            paste0("'", coalesce(undated$date_text, "(none)"), "' ", shorten(undated$text, 30), collapse = "; "))
  }
  events <- filter(events, !(is.na(t0) & !open_start))

  # Every part of the article must still contribute: the deep-time table, the two BC tables and
  # the Common Era list.
  per_unit <- table(factor(events$unit, levels = c("Ma", "BC", "CE")))
  if (any(per_unit < c(10, 10, 5))) {
    stop("too few dated climate events per section (Ma, BC, CE): ", paste(per_unit, collapse = ", "), "; the article layout has changed")
  }

  # Each section's dates must fall where its unit says: a BC column read as years ago (or the
  # reverse) puts rows in the wrong epoch without any other sign.
  plausible <- events |>
    mutate(
      lo = coalesce(t0, t1), hi = coalesce(t1, t0),
      ok = case_when(
        unit == "Ma" ~ lo < -9e5 & hi < 0,
        unit == "BC" ~ lo > -2e5 & hi < 600,
        unit == "CE" ~ lo >= 400 & lo < today_year() + 1,
        .default = FALSE
      )
    )
  if (!all(plausible$ok)) {
    bad <- filter(plausible, !ok)
    stop("climate events outside the range their section implies, e.g. '", bad$date_text[[1]], "' (", bad$unit[[1]], "): ", bad$name[[1]])
  }

  events |>
    transmute(section, unit, date_text, name, label, title, text, t0, t1, open_start, open_end)
}

register_source(new_source(
  id = "climate_events",
  title = "List of periods and events in climate history",
  url = "https://en.wikipedia.org/wiki/List_of_periods_and_events_in_climate_history",
  citation = "Wikipedia contributors, 'List of periods and events in climate history', Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0.",
  fetch = fetch_climate_events,
  max_age_days = 30,
  min_rows = 40
))

register_metric("climate_events", needs = "climate_events", build = function(src) {
  # "Present" is today, and "before X" (open at the old end) reaches back to the Earth's
  # formation; both belong to the moment the panel is built rather than to the cached source.
  events <- src$climate_events |>
    mutate(
      t0 = if_else(open_start, bp_to_year(EARTH_AGE), t0),
      t1 = if_else(open_end, today_year(), t1),
      # A year or less is a moment, drawn as a diamond at its start.
      instant = !open_end & (t1 - t0) <= 1,
      t1 = if_else(instant, t0, t1)
    ) |>
    arrange(t0, t1)

  # Diamonds have no length, so identical dates (the Deccan Traps and the end-Cretaceous impact
  # both sit at 66 Ma) would pile into one row. Pack them as if they had a width of 0.5% of their
  # age, which keeps near neighbours apart without touching their dates.
  pack_end <- if_else(events$instant, events$t0 + pmax(1, 0.005 * (today_year() - events$t0)), events$t1)
  lane <- assign_lanes(events$t0, pack_end)
  fill <- bar_palette(max(lane) + 1)[lane + 1]

  data <- events |>
    mutate(
      lane = lane, fill = fill, text_colour = text_on(fill),
      body = hover_lines(
        # The deep-time table's own heading ("Before 1 million years ago") is wrong for its
        # last rows (Quaternary glaciation, 2.5 Ma), so name only the later sections.
        Period = if_else(unit == "Ma", NA_character_, section),
        Details = if_else(text != title, wrap_html(text), NA_character_),
        Duration = if_else(instant | open_end | open_start, NA_character_, format_duration(t1 - t0)),
        Status = if_else(open_end, "Ongoing", NA_character_)
      )
    ) |>
    select(t0, t1, lane, label, fill, text_colour, instant, title, body)

  new_metric(
    id = "climate_events", label = "Climate events", group = "climate", kind = "spans",
    data = data,
    description = "Named climate episodes and mass extinctions through Earth's history, from the Faint Young Sun to the Little Ice Age. Events lasting a year or less are drawn as diamonds.",
    sources = "climate_events"
  )
})
