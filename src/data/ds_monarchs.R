# Monarchs of England, Great Britain and the United Kingdom, 871 CE to the present, from Wikidata.
# One source (Wikidata, queried live with SPARQL) and one metric (a lane-packed bar per reign).
#
# The Wikidata date helpers it uses (wd_year, gregorian_to_julian) are in helpers_wikidata.R.

# ---- The source ---------------------------------------------------------------------------
# Wikidata "position held" (P39) items for the throne. The realm changed name three times and
# a husband can be joint monarch "jure uxoris" (by right of his wife), which has its own item.
MONARCH_POSITIONS <- tribble(
  ~position,    ~realm,           ~jure_uxoris,
  "Q18810062",  "England",        FALSE,  # Monarch of England
  "Q111513255", "England",        TRUE,   # King of England (jure uxoris): Philip II of Spain, William III
  "Q110324075", "Great Britain",  FALSE,  # Monarch of Great Britain (1707-1801)
  "Q111722535", "United Kingdom", FALSE,  # monarch of the United Kingdom of Great Britain and Ireland
  "Q9134365",   "United Kingdom", FALSE   # Monarch of the United Kingdom (from 1927)
)

# Every holder of those positions, with the start (P580) and end (P582) of each term. One row
# per (term, start|end); dates come back as parts plus precision and calendar (see above).
# The pattern `VALUES (?qualifier ?role)` reads both qualifiers in one pass.
MONARCH_REIGNS_QUERY <- '
SELECT ?statement ?person ?personLabel ?position ?role ?y ?m ?d ?prec ?jul WHERE {
  VALUES ?position { %s }
  ?person p:P39 ?statement .
  ?statement ps:P39 ?position .
  VALUES (?qualifier ?role) { (pqv:P580 "start") (pqv:P582 "end") }
  ?statement ?qualifier ?time .
  ?time wikibase:timeValue ?t ; wikibase:timePrecision ?prec ; wikibase:timeCalendarModel ?cal .
  BIND(YEAR(?t) AS ?y) BIND(MONTH(?t) AS ?m) BIND(DAY(?t) AS ?d)
  BIND(IF(?cal = wd:Q1985786, 1, 0) AS ?jul)
  SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}'

# The same people: royal house(s) (P53), and birth and death (best-ranked statement).
MONARCH_PEOPLE_QUERY <- '
SELECT DISTINCT ?person ?field ?text ?y ?m ?d ?prec ?jul WHERE {
  VALUES ?position { %s }
  ?person p:P39/ps:P39 ?position .
  {
    ?person wdt:P53 ?house .
    ?house rdfs:label ?text . FILTER(LANG(?text) = "en")
    BIND("house" AS ?field)
  } UNION {
    VALUES (?claim ?psv ?field) { (p:P569 psv:P569 "birth") (p:P570 psv:P570 "death") }
    ?person ?claim ?statement .
    ?statement a wikibase:BestRank ; ?psv ?time .
    ?time wikibase:timeValue ?t ; wikibase:timePrecision ?prec ; wikibase:timeCalendarModel ?cal .
    BIND(YEAR(?t) AS ?y) BIND(MONTH(?t) AS ?m) BIND(DAY(?t) AS ?d)
    BIND(IF(?cal = wd:Q1985786, 1, 0) AS ?jul)
  }
}'

# Returns one row per reign statement: person_id, name, realm, jure_uxoris, t0, t1 (NA while the
# reign continues), houses ("a|b"), born, died (years) and death_t.
fetch_monarchs <- function() {
  positions <- paste0("wd:", MONARCH_POSITIONS$position, collapse = " ")

  reigns <- wikidata_query(sprintf(MONARCH_REIGNS_QUERY, positions)) |>
    mutate(t = wd_year(y, m, d, prec, jul), person_id = str_extract(person, "Q[0-9]+")) |>
    summarise(t = min(t), .by = c(statement, person_id, personLabel, position, role)) |>
    pivot_wider(names_from = role, values_from = t)
  reigns[setdiff(c("start", "end"), names(reigns))] <- NA_real_

  Sys.sleep(1)  # be gentle: two requests, a second apart
  people <- wikidata_query(sprintf(MONARCH_PEOPLE_QUERY, positions)) |>
    mutate(person_id = str_extract(person, "Q[0-9]+"), t = wd_year(y, m, d, prec, jul))
  houses <- people |>
    filter(field == "house") |>
    summarise(houses = paste(sort(unique(text)), collapse = "|"), .by = person_id)
  lives <- people |>
    filter(field %in% c("birth", "death")) |>
    summarise(t = first(t), .by = c(person_id, field)) |>
    pivot_wider(names_from = field, values_from = t)
  lives[setdiff(c("birth", "death"), names(lives))] <- NA_real_

  reigns |>
    filter(!is.na(start)) |>
    mutate(position = str_extract(position, "Q[0-9]+")) |>
    left_join(houses, by = "person_id") |>
    left_join(lives, by = "person_id") |>
    left_join(MONARCH_POSITIONS, by = "position") |>
    transmute(
      person_id, name = personLabel, realm, jure_uxoris, t0 = start, t1 = end,
      houses = coalesce(houses, ""), born = floor(birth), died = floor(death), death_t = death
    ) |>
    arrange(t0) |>
    validate_monarchs()
}

# Fail loudly if the query result stops looking like the English throne.
validate_monarchs <- function(reigns) {
  if (any(is.na(reigns$realm))) stop("monarchs: a reign has a position outside MONARCH_POSITIONS")
  if (min(reigns$t0) > 900) stop("monarchs: the line should start with Alfred (871 CE) but starts ", floor(min(reigns$t0)))
  if (max(reigns$t0) < 2020) stop("monarchs: no reign has begun since 2020; the present monarch is missing")
  open <- reigns |> filter(is.na(t1), is.na(death_t))
  if (n_distinct(open$person_id) != 1) {
    stop("monarchs: expected exactly one living monarch with an open reign, found ", n_distinct(open$person_id))
  }
  if (any(reigns$t1 < reigns$t0, na.rm = TRUE)) stop("monarchs: a reign ends before it starts")
  if (n_distinct(reigns$person_id) < 55) stop("monarchs: only ", n_distinct(reigns$person_id), " monarchs found (expected 55 or more)")
  reigns
}

register_source(new_source(
  id = "wikidata_monarchs",
  title = "Monarchs of England, Great Britain and the UK (Wikidata)",
  url = "https://www.wikidata.org/wiki/Q18810062",
  citation = "Wikidata contributors. Position held (P39) of Monarch of England, Great Britain and the United Kingdom, with start/end times, royal house and life dates. Wikidata, CC0 1.0. Queried live from query.wikidata.org.",
  fetch = fetch_monarchs,
  max_age_days = 14,
  min_rows = 55L
))


# ---- Royal houses and colours ---------------------------------------------------------------
# Wikidata spells and splits houses inconsistently (the Stuarts appear as "House of Stuart" and
# "House of Stewart di Darnley", Henry II as Plantagenet, Normandy and Angevin all at once), and
# no source holds colours. So: a small documented table, in chronological order. `pattern`
# matches the Wikidata house label; when a monarch has several, the LATEST row here wins, which
# picks the cadet branch (Henry IV is Lancaster, not Plantagenet; George V is Windsor, not
# Saxe-Coburg). A house not listed gets grey rather than failing the panel.
ROYAL_HOUSES <- tribble(
  ~house,            ~pattern,               ~fill,
  "Wessex",          "Wessex",               "#8DB6D9",
  "Knýtlinga",       "Kn.tlinga",            "#C9B79C",
  "Godwin",          "Godwin",               "#D8D36A",
  "Normandy",        "Normandy",             "#F0A07C",
  "Blois",           "Blois",                "#B7A9D6",
  "Plantagenet",     "Plantagenet|Angevin",  "#F6C56B",
  "Lancaster",       "Lancaster",            "#E57C7C",
  "York",            "York",                 "#EFEBD8",
  "Tudor",           "Tudor",                "#7CC48C",
  "Habsburg",        "Habsburg|Hapsburg",    "#E8A3C0",  # Philip II of Spain, joint monarch with Mary I
  "Grey",            "^Grey",                "#C79BD1",
  "Stuart",          "Stuart|Stewart",       "#5AA9B8",
  "Orange-Nassau",   "Orange",               "#F29E4C",
  "Hanover",         "Hanover",              "#6F87C9",
  "Saxe-Coburg and Gotha", "Saxe-Coburg",    "#C9A0A0",
  "Windsor",         "Windsor",              "#6FC2B0"
)
OTHER_HOUSE <- "Other"
OTHER_HOUSE_FILL <- "#CCCCCC"

# Names as Wikidata labels them carry the realm ("Henry VIII of England", "John, King of
# England"); the regnal name is what comes before.
regnal_name <- function(label) {
  label |>
    str_remove(", King of England$") |>
    str_remove(" of (England|Great Britain|the United Kingdom)$")
}

# The house to show for each person: the latest-listed house that matches one of their labels.
assign_house <- function(houses) {
  map_chr(houses, \(labels) {
    hits <- map_lgl(ROYAL_HOUSES$pattern, \(p) str_detect(labels, p))
    if (any(hits)) ROYAL_HOUSES$house[max(which(hits))] else OTHER_HOUSE
  })
}

# A reign is joined to the next one of the same person when it ends the day the next begins
# (Anne: England, then Great Britain; William III: jure uxoris, then in his own right).
# Reigns with a gap, such as Edward IV's restoration, stay separate.
CONTIGUOUS_YEARS <- 0.01

# Wikidata dates the early reigns to the year only, so two kings can overlap by a few months
# on paper (Aethelred and Sweyn in 1014). Overlaps shorter than this are not treated as joint
# reigns when packing lanes.
OVERLAP_TOLERANCE_YEARS <- 0.5

register_metric("monarchs", needs = "wikidata_monarchs", build = function(src) {
  today <- today_year()

  reigns <- src$wikidata_monarchs |>
    mutate(
      # A missing end means the reign continues; for someone who has died and has no end date
      # recorded, fall back to the date of death.
      t1 = coalesce(t1, death_t, today),
      house = assign_house(houses)
    ) |>
    arrange(person_id, t0) |>
    mutate(block = cumsum(t0 > coalesce(lag(t1), -Inf) + CONTIGUOUS_YEARS), .by = person_id) |>
    summarise(
      name = first(name), house = first(house), born = first(born), died = first(died),
      realm = paste(unique(realm), collapse = ", then "),
      # (before t1 is overwritten by the summary below)
      jure_end = if (any(jure_uxoris)) max(t1[jure_uxoris]) else NA_real_,
      all_jure = all(jure_uxoris),
      t0 = min(t0), t1 = max(t1),
      .by = c(person_id, block)
    ) |>
    arrange(t0) |>
    mutate(
      n_reigns = n(), reign_no = row_number(), .by = person_id
    ) |>
    mutate(
      title = if_else(
        n_reigns > 1,
        paste0(regnal_name(name), " (", reign_no, if_else(reign_no == 1, "st", if_else(reign_no == 2, "nd", "th")), " reign)"),
        regnal_name(name)
      ),
      fill = coalesce(ROYAL_HOUSES$fill[match(house, ROYAL_HOUSES$house)], OTHER_HOUSE_FILL),
      lane = assign_lanes(t0, t1 - OVERLAP_TOLERANCE_YEARS),
      lived = case_when(
        is.na(born) & is.na(died) ~ NA_character_,
        is.na(died) ~ paste0(born, "–present"),
        is.na(born) ~ paste0("?–", died),
        .default = paste0(born, "–", died)
      ),
      note = case_when(
        all_jure ~ "Joint monarch through marriage (jure uxoris)",
        !is.na(jure_end) ~ paste("Joint monarch through marriage (jure uxoris) until", floor(jure_end)),
        .default = NA_character_
      )
    )

  data <- reigns |>
    transmute(
      t0, t1, lane, label = title, fill, text_colour = text_on(fill),
      # Reigns of a year or less would be invisible bars at most zoom levels.
      instant = (t1 - t0) < 1,
      title,
      body = hover_lines(
        House = house, Lived = lived, Realm = realm, Note = note
      )
    )

  houses_present <- ROYAL_HOUSES |>
    filter(house %in% reigns$house) |>
    transmute(series = house, label = paste("House of", house), colour = fill)
  if (OTHER_HOUSE %in% reigns$house) {
    houses_present <- bind_rows(houses_present, tibble(series = OTHER_HOUSE, label = "Other house", colour = OTHER_HOUSE_FILL))
  }

  new_metric(
    id = "monarchs", label = "Monarchs of England and the UK", group = "human", kind = "spans",
    data = data, legend = houses_present,
    description = "Reigns of the monarchs of England, then Great Britain, then the United Kingdom, from Alfred the Great, coloured by royal house. Joint reigns and restorations are stacked in separate rows; the current reign runs to today.",
    sources = "wikidata_monarchs", default_on = FALSE
  )
})
