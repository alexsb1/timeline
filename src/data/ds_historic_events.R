# Major secular historical events with a recorded date, from Wikidata.
# One source (SPARQL queries against the live Wikidata service, then a notability cut) and one
# metric (a single row of event markers; lasting events get a line from start to end).
# Uses wd_year() from ds_monarchs.R for dates.
#
# WHICH CLASSES. Wikidata items that are instances of (or of a subclass of) the classes in
# EVENT_TYPES below: the kinds of event the brief asks for -- wars, battles, revolutions,
# treaties, coups, major natural disasters, famines -- plus two technological milestones that
# Wikidata can date cleanly (crewed spaceflights, nuclear tests). Wikidata has no usable class
# for "scientific discovery" (discoveries and inventions are dated on the *thing*, with a
# different property, and mix in chemical elements, minerals, moons and so on), so general
# science milestones are not available. Left out on purpose: terrorist attacks, massacres and
# genocide (not in the brief, and recent items dominate), epidemics (they have their own panel),
# and everything below.
#
# SECULAR AND EVIDENCE-BASED ONLY. Anything that is an instance of "narrative event" (Q122397289:
# "event known only from narrations ... includes fictional, mythical, legendary or religious
# events") or of one of its subclasses is dropped; that class covers mythological, religious and
# fictional events (the Genesis flood, fictional battles and wars). Ordinary wars and battles
# fought for religious reasons (the Crusades) are real history and stay.
#
# NOTABILITY. The number of Wikimedia sitelinks (articles in other language editions, and so on)
# of an item is the proxy. Each type has a minimum (35 to start with) and the final list takes
# the top PER_ERA items of each historical era (ERA_BREAKS), so that no era, and in particular
# the last century, takes over. Within an era at most ERA_FAMILY_QUOTA items of each family
# (conflicts, treaties, natural disasters, technology) are kept, or wars would fill every slot.
# Types with a cap (nuclear tests, spaceflights) are trimmed first, and numbered members of a
# series ("First Crusade") are dropped when the series ("Crusades") is listed.
#
# DATES. Wikidata dates events inconsistently ("point in time" P585, or "start time" P580 and
# "end time" P582, sometimes both, sometimes a placeholder year). Per item the most precise of
# the point/start candidates is used (ties go to the property natural for the type: point in time
# for battles and treaties, start time for wars and revolutions; the launch date for spaceflights);
# the end is the end time, or for a war with only a point in time after its start, that point.
# Points need month precision or better, spans at least year precision. An event whose name
# contains a year that its date contradicts ("2001-09-11" for the Alamo) is dropped.

# Priority order: an item in several classes (a revolution that is also a rebellion) takes the
# first row that matches. `dates`: which Wikidata date is the event's natural start (see above).
# `closure`: include subclasses (TRUE) or just the class itself (FALSE; battles and sieges have
# many subclasses and the full closure is slow). min_sl: minimum sitelinks. cap: most items of
# this type kept overall (NA = no limit).
#   family: used for the per-era quota below, so that wars do not crowd out treaties and disasters.
# Civil wars are labelled "War": Wikidata files several ordinary wars (the War of the Spanish
# Succession) under "civil war" as well, and a civil war is a war.
EVENT_TYPES <- tribble(
  ~root,      ~type,                ~dates,  ~closure, ~min_sl, ~cap, ~family,
  "Q10931",   "Revolution",         "span",  TRUE,     35L,     NA,   "conflict",
  "Q45382",   "Coup d'état",        "point", TRUE,     35L,     NA,   "conflict",
  "Q124734",  "Rebellion",          "span",  TRUE,     35L,     NA,   "conflict",
  "Q8465",    "War",                "span",  TRUE,     35L,     NA,   "conflict",  # civil war
  "Q198",     "War",                "span",  TRUE,     35L,     NA,   "conflict",
  "Q188055",  "Siege",              "span",  FALSE,    35L,     NA,   "conflict",
  "Q178561",  "Battle",             "point", FALSE,    35L,     NA,   "conflict",
  "Q1261499", "Naval battle",       "point", FALSE,    35L,     NA,   "conflict",
  "Q131569",  "Treaty",             "point", TRUE,     35L,     NA,   "treaty",
  "Q7944",    "Earthquake",         "point", TRUE,     35L,     NA,   "nature",
  "Q7692360", "Volcanic eruption",  "point", TRUE,     30L,     NA,   "nature",
  "Q8070",    "Tsunami",            "point", TRUE,     35L,     NA,   "nature",
  "Q8068",    "Flood",              "point", TRUE,     35L,     NA,   "nature",
  "Q168247",  "Famine",             "span",  TRUE,     35L,     NA,   "nature",
  "Q210112",  "Nuclear test",       "point", TRUE,     30L,     2L,   "technology",
  "Q752783",  "Crewed spaceflight", "point", TRUE,     60L,     3L,   "technology"
)
NARRATIVE_EVENT <- "Q122397289"  # fictional, mythical, legendary or religious events

# Most events of each family kept per era (before the PER_ERA cut), so the mix stays varied.
ERA_FAMILY_QUOTA <- c(conflict = 8L, treaty = 3L, nature = 3L, technology = 2L)

# "First Crusade", "Second Punic War" ...: numbered members of a series are dropped when the
# series itself ("Crusades", "Punic Wars") is in the list.
SERIES_ORDINALS <- c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth")

# Eras for the "top N per era" cut, as calendar years (astronomical numbering).
ERA_BREAKS <- c(-Inf, -500, 0, 500, 1000, 1300, 1500, 1650, 1750, 1800, 1850, 1900, 1930, 1950, 1980, 2000, Inf)
PER_ERA <- 12L

# An unfinished span with no end date is "still going" only if it began this recently.
EVENT_ONGOING_WITHIN_YEARS <- 15


# Request 1: candidates (item, sitelinks, which class matched). One UNION branch per type; each
# starts from a constant class, which is what keeps the service fast.
events_candidates_query <- function() {
  branch <- \(root, closure, min_sl, ...) sprintf(
    "  { %s ?item wdt:P31 %s ; wikibase:sitelinks ?sl . FILTER(?sl >= %d) BIND(wd:%s AS ?root) }",
    if (closure) paste0("?cls wdt:P279* wd:", root, " .") else "",
    if (closure) "?cls" else paste0("wd:", root),
    min_sl, root
  )
  paste0(
    "SELECT ?item ?sl ?root WHERE {\n",
    paste(pmap_chr(EVENT_TYPES, branch), collapse = "\n  UNION\n"),
    "\n}"
  )
}

# Request 2 (in chunks of 350 items; listing the items first in VALUES is what keeps it fast):
# name and every date value, one row per (item, role): p = point in time, s = start, e = end,
# l = launch (spaceflights). Dates come as y/m/d, precision and a Julian flag, as in wd_year().
EVENTS_DATES_QUERY <- '
SELECT ?item ?label ?role ?y ?m ?d ?prec ?jul WHERE {
  VALUES ?item { %s }
  ?item rdfs:label ?label . FILTER(LANG(?label) = "en")
  VALUES (?claim ?psv ?role) { (p:P585 psv:P585 "point") (p:P580 psv:P580 "start") (p:P582 psv:P582 "end") (p:P619 psv:P619 "launch") }
  ?item ?claim ?st . ?st ?psv ?time .
  ?time wikibase:timeValue ?t ; wikibase:timePrecision ?prec ; wikibase:timeCalendarModel ?cal .
  BIND(YEAR(?t) AS ?y) BIND(MONTH(?t) AS ?m) BIND(DAY(?t) AS ?d)
  BIND(IF(?cal = wd:Q1985786, 1, 0) AS ?jul)
}'

# Request 3: for the chosen events only: place and the larger event it belongs to, and whether
# it is a narrative (mythical, religious, fictional) event. Location (P276) is used when given,
# otherwise country (P17); "part of" is P361.
EVENTS_DETAILS_QUERY <- '
SELECT ?item (GROUP_CONCAT(DISTINCT ?loc; separator = "|") AS ?locations)
             (GROUP_CONCAT(DISTINCT ?cty; separator = "|") AS ?countries)
             (GROUP_CONCAT(DISTINCT ?part; separator = "|") AS ?parts)
             (MAX(?is_narrative) AS ?narrative) WHERE {
  VALUES ?item { %s }
  OPTIONAL { ?item wdt:P31/wdt:P279* wd:%s . BIND(1 AS ?is_narrative) }
  OPTIONAL { ?item wdt:P276 ?l . ?l rdfs:label ?loc . FILTER(LANG(?loc) = "en") }
  OPTIONAL { ?item wdt:P17 ?c . ?c rdfs:label ?cty . FILTER(LANG(?cty) = "en") }
  OPTIONAL { ?item wdt:P361 ?p . ?p rdfs:label ?part . FILTER(LANG(?part) = "en") }
}
GROUP BY ?item'

# For one item: its start, end and the precision of the start (see DATES in the header).
# `d` has one row per date value: role (point/start/end/launch), t, prec.
event_dates <- function(d, style) {
  preference <- if (style == "point") c("point", "launch", "start") else c("start", "point", "launch")
  starts <- d |>
    filter(role != "end") |>
    mutate(rank_role = match(role, preference)) |>
    arrange(desc(prec), rank_role, t)
  if (nrow(starts) == 0) return(tibble(t0 = NA_real_, t1 = NA_real_, prec = NA_real_))
  start <- starts[1, ]
  ends <- d |> filter(role == "end", t >= start$t)
  # a point in time after the start of a span is usually its end (American Civil War: 1865)
  points_after <- d |> filter(style == "span", role == "point", start$role != "point", t > start$t)
  end <- if (nrow(ends) > 0) max(ends$t) else if (nrow(points_after) > 0) max(points_after$t) else NA_real_
  tibble(t0 = start$t, t1 = end, prec = start$prec)
}

# One row per chosen event: item, name, type, sitelinks, t0, t1 (NA while unfinished),
# place, part_of. See the header for the rules.
fetch_historic_events <- function() {
  today <- today_year()

  candidates <- wikidata_query(events_candidates_query()) |>
    mutate(item = str_extract(item, "Q[0-9]+"), root = str_extract(root, "Q[0-9]+")) |>
    left_join(select(EVENT_TYPES, root, type, dates, cap, family), by = "root") |>
    mutate(type_rank = match(root, EVENT_TYPES$root)) |>
    arrange(type_rank) |>
    distinct(item, .keep_all = TRUE)  # an item in several classes keeps its highest-priority type
  if (nrow(candidates) < 400) stop("historic events: only ", nrow(candidates), " candidate events found (expected 400 or more)")

  # Types with a cap keep only their most-linked items before any dates are fetched.
  candidates <- candidates |>
    mutate(cap_rank = row_number(desc(sl)), .by = type) |>
    filter(is.na(cap) | cap_rank <= cap)

  chunks <- split(candidates$item, ceiling(seq_along(candidates$item) / 350))
  dated <- map(chunks, \(ids) {
    Sys.sleep(0.5)  # be gentle with the service between requests
    wikidata_query(sprintf(EVENTS_DATES_QUERY, paste0("wd:", ids, collapse = " ")))
  }) |>
    list_rbind() |>
    mutate(item = str_extract(item, "Q[0-9]+"))
  needed <- c("item", "label", "role", "y", "m", "d", "prec", "jul")
  if (!all(needed %in% names(dated))) stop("historic events: the dates query is missing columns: ", paste(setdiff(needed, names(dated)), collapse = ", "))

  dates_long <- dated |>
    mutate(t = wd_year(y, m, d, prec, jul)) |>
    filter(!is.na(t)) |>
    distinct(item, role, t, prec)
  names_tbl <- dated |> distinct(item, label)

  events <- candidates |>
    select(item, sitelinks = sl, type, dates, family) |>
    inner_join(names_tbl, by = "item")
  by_item <- split(dates_long, dates_long$item)
  when <- map2(events$item, events$dates, \(it, style) {
    if (is.null(by_item[[it]])) tibble(t0 = NA_real_, t1 = NA_real_, prec = NA_real_) else event_dates(by_item[[it]], style)
  }) |>
    list_rbind()
  events <- bind_cols(events, when) |>
    mutate(
      name = label,
      # a name that carries a year must agree with the date ("1556 Shaanxi earthquake")
      name_years = map(name, \(n) as.numeric(str_extract_all(n, "(?<![0-9])[0-9]{4}(?![0-9])")[[1]])),
      year_ok = map2_lgl(name_years, t0, \(y, t) length(y) == 0 || (floor(t) >= min(y) - 1 && floor(t) <= max(y) + 1)),
      # an end date in the future is a placeholder: unknown
      t1 = if_else(!is.na(t1) & t1 > today, NA_real_, t1)
    ) |>
    filter(
      !is.na(t0), t0 <= today, year_ok,
      if_else(dates == "point", prec >= 10, prec >= 9)
    )

  # Numbered members of a series go when the series is listed (First Crusade under Crusades).
  series <- str_remove(events$name, paste0("^(", paste(SERIES_ORDINALS, collapse = "|"), ") "))
  in_series <- series != events$name & (series %in% events$name | paste0(series, "s") %in% events$name)
  events <- events[!in_series, ]

  # Top PER_ERA of each era by sitelinks, at most ERA_FAMILY_QUOTA of each family.
  chosen <- events |>
    mutate(era = cut(t0, ERA_BREAKS, right = FALSE)) |>
    arrange(desc(sitelinks)) |>
    mutate(family_rank = row_number(), .by = c(era, family)) |>
    filter(family_rank <= ERA_FAMILY_QUOTA[family]) |>
    slice_max(sitelinks, n = PER_ERA, by = era, with_ties = FALSE) |>
    arrange(t0)
  if (nrow(chosen) < 100) stop("historic events: only ", nrow(chosen), " events after the notability cut (expected 100 or more)")

  Sys.sleep(0.5)
  details <- wikidata_query(sprintf(EVENTS_DETAILS_QUERY, paste0("wd:", chosen$item, collapse = " "), NARRATIVE_EVENT)) |>
    mutate(item = str_extract(item, "Q[0-9]+"))

  chosen |>
    left_join(details, by = "item") |>
    filter(is.na(narrative)) |>  # drop mythical, religious and fictional events (see the header)
    transmute(
      item, name, type, sitelinks, t0, t1,
      place = coalesce(na_if(locations, ""), na_if(countries, ""), NA_character_),
      part_of = na_if(parts, "")
    ) |>
    validate_events()
}

validate_events <- function(events) {
  if (anyNA(events$name) || anyNA(events$t0)) stop("historic events: an event has no name or date")
  if (any(events$t1 < events$t0, na.rm = TRUE)) stop("historic events: an event ends before it starts")
  if (anyDuplicated(events$item)) stop("historic events: duplicate events")
  eras_covered <- n_distinct(cut(events$t0, ERA_BREAKS, right = FALSE))
  if (eras_covered < 10) stop("historic events: events cover only ", eras_covered, " eras (expected 10 or more)")
  events
}

register_source(new_source(
  id = "wikidata_historic_events",
  title = "Major historical events (Wikidata)",
  url = "https://www.wikidata.org/wiki/Q13418847",
  citation = "Wikidata contributors. Instances of war, battle, revolution, treaty, coup d'etat, earthquake, famine and related classes, ranked by number of sitelinks, with point/start/end times, location and 'part of'. Wikidata, CC0 1.0. Queried live from query.wikidata.org.",
  fetch = fetch_historic_events,
  max_age_days = 14,
  min_rows = 100L
))

EVENT_COLOUR <- "#6A5ACD"

register_metric("historic_events", needs = "wikidata_historic_events", build = function(src) {
  today <- today_year()

  # place names are only shown when there are at most three (a war "located" in a dozen countries
  # says nothing)
  short_list <- \(x, max_n) map_chr(x, \(s) {
    if (is.na(s)) return(NA_character_)
    parts <- str_split_1(s, "\\|")
    if (length(parts) > max_n) NA_character_ else paste(parts, collapse = ", ")
  })

  data <- src$wikidata_historic_events |>
    mutate(
      ongoing = is.na(t1) & type %in% c("War", "Civil war", "Rebellion", "Revolution", "Siege", "Famine") &
        t0 > today - EVENT_ONGOING_WITHIN_YEARS,
      t1 = case_when(!is.na(t1) ~ t1, ongoing ~ today, .default = t0)
    ) |>
    transmute(
      t = (t0 + t1) / 2, t0, t1, y = 0, size = 9, colour = EVENT_COLOUR,
      title = name,
      body = hover_lines(
        Type = type,
        `Part of` = wrap_html(short_list(part_of, 2)),
        Place = wrap_html(short_list(place, 3))
      )
    )

  new_metric(
    id = "historic_events", label = "Major historical events", group = "human", kind = "events",
    data = data, y_title = "Events", y_hidden = TRUE,
    description = "Major secular historical events from Wikidata: wars, battles, revolutions, coups, treaties, major earthquakes and famines, and a few technological milestones. The most widely documented events of each era, so no period dominates; events with an end date are drawn as a line.",
    sources = "wikidata_historic_events", default_on = FALSE
  )
})
