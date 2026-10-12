# Bond events: the cold, ice-rafting spells of the Holocene North Atlantic (Bond et al. 1997),
# from the table in Wikipedia's "Bond event" article (CC BY-SA 4.0). Numbered 0 (most recent)
# to 8 (oldest). Uses the text helpers (hist_*) defined in ds_climate_events.R.

BOND_PAGE <- "Bond event"

# Each event's date appears twice in the table, as a calendar year ("~ 800 BC") and in thousands of
# years from 2000 CE ("~ -2.8 ka"). Reading both lets the two be checked against each other.
fetch_bond_events <- function() {
  doc <- wikipedia_html(BOND_PAGE)

  table <- xml2::xml_find_all(doc, "//table[contains(@class, 'wikitable')]") |>
    keep(\(node) str_detect(xml2::xml_text(node), fixed("Time (AD, BC)"))) |>
    head(1)
  if (length(table) == 0) stop("the Bond event table (with a 'Time (AD, BC)' column) was not found; the article layout has changed")

  headers <- xml2::xml_find_all(table[[1]], ".//th") |> map_chr(\(th) hist_text(hist_own_html(th)))
  column <- \(pattern) {
    found <- str_which(headers, pattern)
    if (length(found) == 0) stop("the Bond event table has no column matching '", pattern, "'; the article layout has changed")
    found[[1]]
  }
  cols <- c(no = column("^No"), ka = column("\\(BP\\)"), calendar = column("AD, BC"), gap = column("^Gap"), notes = column("^Notes"))

  rows <- xml2::xml_find_all(table[[1]], ".//tr[td]") |>
    map(\(row) xml2::xml_find_all(row, "./td") |> map_chr(\(td) hist_text(hist_own_html(td)) %||% NA_character_))
  rows <- keep(rows, \(cells) length(cells) >= max(cols))

  events <- tibble(
    no = map_chr(rows, cols[["no"]]) |> as.integer(),
    calendar_text = map_chr(rows, cols[["calendar"]]),
    ka_text = map_chr(rows, cols[["ka"]]),
    gap_text = map_chr(rows, cols[["gap"]]),
    notes = map_chr(rows, cols[["notes"]])
  ) |>
    bind_cols(hist_parse_range(map_chr(rows, cols[["calendar"]]))) |>
    mutate(
      t = t0,
      # "-2.8 ka" counts back from 2000 CE (the sign is a real minus, U+2212).
      ka = parse_number(str_replace(ka_text, intToUtf8(0x2212), "-")),
      gap_years = as.integer(str_extract(gap_text, "[0-9]+"))
    ) |>
    select(no, t, calendar_text, ka, gap_years, notes)

  if (nrow(events) < 8 || anyNA(events$no) || anyNA(events$t) || anyDuplicated(events$no)) {
    stop("the Bond event table did not read as numbered events with calendar dates; the article layout has changed")
  }
  # Older events carry higher numbers, and the two date columns must agree.
  events <- arrange(events, no)
  if (any(diff(events$t) >= 0)) stop("Bond events are not in order of age; the table columns may have shifted")
  disagree <- !is.na(events$ka) & abs(2000 + events$ka * 1000 - events$t) > 150
  if (any(disagree)) {
    stop("the calendar and ka columns of the Bond event table disagree for event(s) ", paste(events$no[disagree], collapse = ", "))
  }
  events
}

register_source(new_source(
  id = "bond_events",
  title = "Bond event",
  url = "https://en.wikipedia.org/wiki/Bond_event",
  citation = "Wikipedia contributors, 'Bond event', Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0. Events after Bond, G. et al. (1997), 'A pervasive millennial-scale cycle in North Atlantic Holocene and glacial climates', Science 278, 1257-1266, doi:10.1126/science.278.5341.1257.",
  fetch = fetch_bond_events,
  max_age_days = 30,
  min_rows = 8
))

register_metric("bond_events", needs = "bond_events", build = function(src) {
  data <- src$bond_events |>
    transmute(
      t, t0 = t, t1 = t, y = no, size = 13, colour = okabe_ito[["blue"]],
      title = paste("Bond event", no),
      # `gap_years` is the interval to the next older event (the table's "gap from previous event").
      body = hover_lines(
        Interval = if_else(is.na(gap_years), NA_character_, paste0(plain_number(gap_years), " years after Bond event ", no + 1)),
        Notes = wrap_html(notes)
      )
    )

  new_metric(
    id = "bond_events", label = "Bond events", group = "climate", kind = "events",
    data = data, y_title = "Bond event number", y_format = ".0f",
    description = "Cold spells recorded as ice-rafted debris in North Atlantic sediments through the Holocene. Gerard Bond's team saw them recur about every 1,500 years; later work puts the main rhythm nearer 1,000.",
    sources = "bond_events"
  )
})
