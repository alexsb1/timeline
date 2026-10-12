# Notable epidemics and pandemics with an estimated death toll, from Wikidata.
# One source (a SPARQL query against the live Wikidata service) and one metric (events on a log
# axis: height = deaths, a line from start to end). Uses wd_year() from ds_monarchs.R for dates.
#
# WHAT COUNTS. An item is a candidate if it is an instance of one of the outbreak classes below,
# has at least WIKIDATA_MIN_SITELINKS sitelinks (Wikidata has thousands of local outbreaks and a
# page for every country's COVID-19 epidemic; the number of Wikimedia articles about an item is
# the notability filter) and has a "number of deaths" (P1120) statement of 5,000 or more. Then:
#   * deaths: if a preferred-rank statement exists it is used (Wikidata's own "best value", e.g.
#     the 1918 flu: 50 million, not the 675,000 also listed). Otherwise, if the statements are
#     dated (a running count, as for COVID-19), the latest. Otherwise every value is kept and the
#     height is the geometric midpoint of the lowest and highest estimate (Plague of Justinian:
#     15-100 million), the natural centre on a log axis; the tooltip gives the range.
#   * double counting: regional parts of a bigger listed outbreak are dropped, both by Wikidata's
#     "part of" (P361) and by name ("Black Death in England" sits under "Black Death").
# Final cut: 10,000 deaths or more, and a start date.
#
# LIMITS OF THE DATA: the pandemic item for COVID-19 carries no death toll (only its country and
# continent pages do), so the toll is read from the COVID-19 disease item (DEATHS_FROM below);
# Wikidata's latest figure there is from the end of 2021, and the tooltip says "as of". Several
# well-known outbreaks (Antonine Plague, Plague of Cyprian, the cholera pandemics, the Third
# Plague Pandemic) have no death toll on Wikidata and so cannot appear.

# Instances of these are candidate outbreaks. Not "Northern/Southern Hemisphere flu season"
# (annual statistics, not events) nor the broader disease classes.
OUTBREAK_CLASSES <- c(
  "Q44512",     # epidemic
  "Q12184",     # pandemic
  "Q3241045",   # disease outbreak
  "Q1516910",   # plague epidemic
  "Q959265",    # cholera outbreak
  "Q30103545",  # smallpox epidemic
  "Q2723958",   # influenza pandemic
  "Q63472138"   # cocoliztli epidemic
)
WIKIDATA_MIN_SITELINKS <- 8L
MIN_DEATHS <- 10000

# Outbreaks whose own item has no death toll, mapped to the item that does: the COVID-19 pandemic
# (Q81068910) to the COVID-19 disease item (Q84263196), which carries dated global counts.
DEATHS_FROM <- c(Q81068910 = "Q84263196")

# Request 1: the candidate outbreaks (a second of work for the service).
PANDEMIC_CANDIDATES_QUERY <- '
SELECT DISTINCT ?item ?sl ?label WHERE {
  { VALUES ?class { %s }
    ?item wdt:P31 ?class ; wikibase:sitelinks ?sl .
    FILTER(?sl >= %d)
    ?item p:P1120/ps:P1120 ?anyDeaths . FILTER(?anyDeaths >= 5000) }
  UNION
  { VALUES ?item { %s } ?item wikibase:sitelinks ?sl . }
  ?item rdfs:label ?label . FILTER(LANG(?label) = "en")
}'

# Request 2, for those items only (listing them first in VALUES keeps the service from scanning
# every statement on Wikidata): one row per (outbreak, field): deaths statements (num, pref =
# preferred rank, date of the count as y/m/d), start/end/point in time (when there are several
# start dates, as for COVID-19's early cases, the earliest is used), causes (P828, labelled) and
# the larger outbreak it belongs to (P361).
PANDEMIC_FIELDS_QUERY <- '
SELECT ?item ?field ?text ?num ?pref ?y ?m ?d ?prec ?jul WHERE {
  VALUES ?item { %s }
  OPTIONAL { VALUES (?item ?alt) { %s } }
  BIND(COALESCE(?alt, ?item) AS ?deathsFrom)
  {
    ?deathsFrom p:P1120 ?st .
    ?st ps:P1120 ?num ; wikibase:rank ?rank .
    FILTER(?rank != wikibase:DeprecatedRank)
    BIND(IF(?rank = wikibase:PreferredRank, 1, 0) AS ?pref)
    OPTIONAL { ?st pq:P585 ?asof . BIND(YEAR(?asof) AS ?y) BIND(MONTH(?asof) AS ?m) BIND(DAY(?asof) AS ?d) }
    BIND("deaths" AS ?field)
  } UNION {
    VALUES (?claim ?psv ?field) { (p:P580 psv:P580 "start") (p:P582 psv:P582 "end") (p:P585 psv:P585 "point") }
    ?item ?claim ?st . ?st ?psv ?time .
    ?time wikibase:timeValue ?t ; wikibase:timePrecision ?prec ; wikibase:timeCalendarModel ?cal .
    BIND(YEAR(?t) AS ?y) BIND(MONTH(?t) AS ?m) BIND(DAY(?t) AS ?d)
    BIND(IF(?cal = wd:Q1985786, 1, 0) AS ?jul)
  } UNION {
    ?item wdt:P828 ?cause . ?cause rdfs:label ?text . FILTER(LANG(?text) = "en")
    BIND("cause" AS ?field)
  } UNION {
    ?item wdt:P361 ?whole . BIND(STRAFTER(STR(?whole), "entity/") AS ?text)
    BIND("part_of" AS ?field)
  }
}'

# Deaths for one outbreak from its statements (rule in the header).
choose_deaths <- function(num, pref, asof) {
  keep <- if (any(pref == 1)) {
    pref == 1
  } else if (any(!is.na(asof))) {
    !is.na(asof) & asof == max(asof, na.rm = TRUE)
  } else {
    rep(TRUE, length(num))
  }
  tibble(
    deaths_low = min(num[keep]), deaths_high = max(num[keep]),
    deaths_asof = if (any(keep & !is.na(asof))) floor(max(asof[keep], na.rm = TRUE)) else NA_real_
  )
}

# One row per outbreak: item, name, sitelinks, deaths_low/high/asof, t0, t1 (NA if no end date),
# causes ("a|b"), part_of ("Q..|Q.."). Outbreaks under MIN_DEATHS, without a start date, or nested
# inside another listed outbreak are dropped here.
fetch_pandemics <- function() {
  candidates <- wikidata_query(sprintf(
    PANDEMIC_CANDIDATES_QUERY,
    paste0("wd:", OUTBREAK_CLASSES, collapse = " "), WIKIDATA_MIN_SITELINKS,
    paste0("wd:", names(DEATHS_FROM), collapse = " ")
  )) |>
    mutate(item = str_extract(item, "Q[0-9]+")) |>
    distinct(item, .keep_all = TRUE)
  if (nrow(candidates) < 12) stop("pandemics: only ", nrow(candidates), " candidate outbreaks found (expected 12 or more)")

  Sys.sleep(1)  # be gentle: two requests, a second apart
  overrides <- paste0("(wd:", names(DEATHS_FROM), " wd:", DEATHS_FROM, ")", collapse = " ")
  raw <- wikidata_query(sprintf(
    PANDEMIC_FIELDS_QUERY, paste0("wd:", candidates$item, collapse = " "), overrides
  )) |>
    mutate(item = str_extract(item, "Q[0-9]+"))
  needed <- c("item", "field", "text", "num", "pref", "y", "m", "d", "prec", "jul")
  if (!all(needed %in% names(raw))) stop("pandemics: the query result is missing columns: ", paste(setdiff(needed, names(raw)), collapse = ", "))

  names_sl <- candidates |> select(item, label, sl)

  deaths <- raw |>
    filter(field == "deaths", !is.na(num)) |>
    mutate(asof = calendar_to_year(y, "CE", month = as.integer(m), day = as.integer(d))) |>
    reframe(choose_deaths(num, pref, asof), .by = item)

  dates <- raw |>
    filter(field %in% c("start", "end", "point"), !is.na(y)) |>
    mutate(t = wd_year(y, m, d, prec, jul)) |>
    summarise(t = if (first(field) == "end") max(t) else min(t), .by = c(item, field)) |>
    pivot_wider(names_from = field, values_from = t)
  dates[setdiff(c("start", "end", "point"), names(dates))] <- NA_real_

  # "Has cause" is sometimes an event rather than a pathogen (Wikidata gives World War I as a cause
  # of the 1918 flu). A closure query to find events in general is too slow for the service when
  # combined with the rest, so the one case seen is filtered by name.
  tags <- \(f) raw |>
    filter(field == f, !is.na(text), !(f == "cause" & str_detect(text, "^World War"))) |>
    summarise(x = paste(sort(unique(text)), collapse = "|"), .by = item)

  outbreaks <- names_sl |>
    inner_join(deaths, by = "item") |>
    left_join(dates, by = "item") |>
    left_join(rename(tags("cause"), causes = x), by = "item") |>
    left_join(rename(tags("part_of"), part_of = x), by = "item") |>
    transmute(
      item, name = label, sitelinks = sl, deaths_low, deaths_high, deaths_asof,
      t0 = coalesce(start, point),
      # an end before the start is a data error: treat as unknown
      t1 = coalesce(end, if_else(is.na(start), point, NA_real_)),
      t1 = if_else(t1 < t0, NA_real_, t1),
      causes = coalesce(causes, ""), part_of = coalesce(part_of, "")
    ) |>
    filter(!is.na(t0), sqrt(deaths_low * deaths_high) >= MIN_DEATHS)

  # Double counting: drop an outbreak nested in another listed one, by Wikidata's "part of" or by
  # name ("Black Death in England" under "Black Death").
  nested_by_link <- map_lgl(outbreaks$part_of, \(parts) any(str_split_1(parts, "\\|") %in% outbreaks$item))
  nested_by_name <- map_lgl(outbreaks$name, \(n) any(str_starts(n, fixed(paste0(outbreaks$name, " in ")))))
  outbreaks <- outbreaks[!nested_by_link & !nested_by_name, ] |> arrange(t0)

  if (nrow(outbreaks) < 12) stop("pandemics: only ", nrow(outbreaks), " outbreaks with a death toll found (expected 12 or more)")
  if (anyDuplicated(outbreaks$item)) stop("pandemics: duplicate outbreak rows")
  if (any(outbreaks$t0 > today_year() + 1)) stop("pandemics: an outbreak starts in the future")
  outbreaks
}

register_source(new_source(
  id = "wikidata_pandemics",
  title = "Epidemics and pandemics with death tolls (Wikidata)",
  url = "https://www.wikidata.org/wiki/Q12184",
  citation = "Wikidata contributors. Instances of epidemic, pandemic and related classes with number of deaths (P1120), start and end times, and cause. Wikidata, CC0 1.0. Queried live from query.wikidata.org.",
  fetch = fetch_pandemics,
  max_age_days = 14,
  min_rows = 12L
))

# A line of text for the tooltip: "75,000,000", "15,000,000 to 100,000,000", plus "(as of 2021)".
deaths_text <- function(low, high, asof) {
  amount <- if_else(low == high, format_count(low), paste(format_count(low), "to", format_count(high)))
  if_else(is.na(asof), amount, paste0(amount, " (as of ", asof, ")"))
}

# An outbreak with a start but no end date is still going if it began within this many years;
# for older ones the end is simply unknown and it is drawn as a point at its start.
ONGOING_WITHIN_YEARS <- 100

register_metric("pandemics", needs = "wikidata_pandemics", build = function(src) {
  today <- today_year()

  data <- src$wikidata_pandemics |>
    mutate(
      ongoing = is.na(t1) & t0 > today - ONGOING_WITHIN_YEARS,
      t1 = case_when(!is.na(t1) ~ t1, ongoing ~ today, .default = t0),
      deaths = sqrt(deaths_low * deaths_high),  # geometric midpoint (= the value when low == high)
      # at most three causes, first letter capitalised ("plague" -> "Plague", "SARS-CoV-2" as is)
      causes = map_chr(str_split(causes, "\\|"), \(x) paste(head(x, 3), collapse = ", ")),
      causes = str_replace(causes, "^[a-z]", str_to_upper)
    ) |>
    transmute(
      t = (t0 + t1) / 2, t0, t1,
      y = deaths, size = marker_size(deaths), colour = okabe_ito[["vermillion"]],
      title = name,
      body = hover_lines(
        Disease = wrap_html(na_if(causes, "")),
        `Estimated deaths` = deaths_text(deaths_low, deaths_high, deaths_asof),
        Status = if_else(ongoing, "Ongoing (no end date recorded)", NA_character_)
      )
    )

  new_metric(
    id = "pandemics", label = "Pandemics and epidemics", group = "human", kind = "events",
    data = data, y_title = "Estimated deaths", y_type = "log", y_format = ",.0f",
    description = "Notable epidemics and pandemics by estimated death toll, from Wikidata. Height is the toll on a log scale; a line shows how long each lasted. Only outbreaks with a recorded toll of 10,000 or more are included.",
    sources = "wikidata_pandemics", default_on = FALSE
  )
})
