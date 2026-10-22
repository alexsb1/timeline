# Prehistory: the Stone, Copper, Bronze and Iron Ages, read from the English Wikipedia article on
# each period (CC BY-SA 4.0). Uses the date and markup helpers (hist_*) defined in
# ds_climate_events.R.
#
# No single source lists these periods with dates. Wikidata has start and end times for them, but
# several disagreeing ones per item (one per region, tagged "applies to part") and a Stone Age with
# no start, so it cannot give one bar per period. Wikipedia's "Timeline of human prehistory" is now
# a list of events, and its old period table is gone. What is left is what each period's own article
# states, in one of four shapes:
#   infobox  a "Dates" row, or the date range under the title ("300,000 to 50,000 BP")
#   regions  an infobox that gives one range per region (the Mesolithic)
#   lead     no infobox: the first paragraph says "from around X years ago until around Y years ago"
#   bronze   the Bronze Age article's list of Near East divisions ("Early Bronze Age: 3300-2100 BC")
# Every one of these is regional, which the tooltip says.

PREHISTORY_PAGES <- tribble(
  ~page,                ~age,         ~how,
  "Lower Paleolithic",  "Stone Age",  "lead",
  "Middle Paleolithic", "Stone Age",  "infobox",
  "Upper Paleolithic",  "Stone Age",  "infobox",
  "Mesolithic",         "Stone Age",  "regions",
  "Neolithic",          "Stone Age",  "infobox",
  "Chalcolithic",       "Copper Age", "infobox",
  "Bronze Age",         "Bronze Age", "bronze",
  "Iron Age",           "Iron Age",   "infobox"
)

# "It spans the time from around 3.3 million years ago ... until around 300,000 years ago":
# the first paragraph that reads like this, as a tibble(t0, t1).
prehistory_from_lead <- function(doc) {
  paragraphs <- xml2::xml_find_all(doc, "//p") |> map_chr(\(p) hist_text(hist_own_html(p)))
  number <- "(?:around |about |roughly |c\\. ?)?([0-9][0-9.,]*)( million)? years ago"
  m <- str_match(paragraphs, regex(paste0("from ", number, ".*?(?:until|to) ", number), ignore_case = TRUE))
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  if (nrow(m) == 0) return(NULL)
  years_ago <- \(n, million) as.numeric(str_remove_all(n, ",")) * if_else(is.na(million), 1, 1e6)
  tibble(
    t0 = bp_to_year(years_ago(m[1, 2], m[1, 3])), t1 = bp_to_year(years_ago(m[1, 4], m[1, 5])),
    date_text = paste0(
      str_squish(paste(m[1, 2], coalesce(m[1, 3], ""))), " years ago to ",
      str_squish(paste(m[1, 4], coalesce(m[1, 5], ""))), " years ago"
    )
  )
}

# The Near East divisions of the Bronze Age, from the bullets under the "Near East Bronze Age
# divisions" heading, which read "Middle Bronze Age (MBA or MB) or Intermediate Bronze Age (IBA or
# IB): 2100-1550 BC".
prehistory_bronze_divisions <- function(doc) {
  list_node <- xml2::xml_find_first(doc, "//*[self::h2 or self::h3][contains(., 'Near East')]/following::ul[1]")
  if (inherits(list_node, "xml_missing")) return(NULL)
  items <- xml2::xml_find_all(list_node, ".//li") |> map_chr(\(li) hist_text(hist_own_html(li)))
  parts <- str_match(items, "^(Early|Middle|Late) Bronze Age[^:]*:\\s*(.+)$")
  parts <- parts[!is.na(parts[, 1]), , drop = FALSE]
  if (nrow(parts) == 0) return(NULL)
  tibble(
    name = paste(parts[, 2], "Bronze Age"),
    aka = str_match(parts[, 1], "or (Intermediate Bronze Age)")[, 2],
    region = "Near East", date_text = parts[, 3]
  ) |>
    bind_cols(hist_parse_range(parts[, 3])) |>
    filter(!is.na(t0))
}

# One page's periods as tibble(name, age, region, aka, date_text, t0, t1, page).
prehistory_read_page <- function(page, age, how) {
  doc <- wikipedia_html(page)
  name <- page

  found <- switch(how,
    infobox = hist_infobox_dates(doc) |>
      transmute(name = name, region = NA_character_, aka = NA_character_, date_text = line, t0, t1),
    regions = hist_infobox_dates(doc, first_only = FALSE) |>
      mutate(region = str_match(line, "\\(([^)]+)\\)")[, 2]) |>
      filter(!is.na(region)) |>
      transmute(name = paste0(name, " (", region, ")"), region, aka = NA_character_, date_text = line, t0, t1),
    lead = prehistory_from_lead(doc) |>
      mutate(name = name, region = NA_character_, aka = NA_character_),
    bronze = prehistory_bronze_divisions(doc)
  )
  if (is.null(found) || nrow(found) == 0) {
    stop("no dates found for '", page, "' (", how, "); the article layout has changed")
  }
  found |>
    mutate(age = age, page = page) |>
    select(name, age, region, aka, date_text, t0, t1, page)
}

fetch_prehistory <- function() {
  periods <- pmap(PREHISTORY_PAGES, prehistory_read_page) |> list_rbind() |> arrange(t0)

  if (anyDuplicated(periods$name)) stop("a prehistoric period was found twice: ", paste(periods$name[duplicated(periods$name)], collapse = ", "))
  if (any(periods$t1 <= periods$t0)) stop("a prehistoric period ends before it starts: ", paste(periods$name[periods$t1 <= periods$t0], collapse = ", "))
  if (any(periods$t0 < bp_to_year(5e6) | periods$t1 > -500)) {
    stop("a prehistoric period falls outside 5 million years ago to 500 BCE; the date units may have been misread")
  }
  # Each period in the chain must start later than the one before it, whatever the numbers are;
  # a unit misread (BP for BC) breaks it.
  chain <- periods$t0[match(c("Lower Paleolithic", "Middle Paleolithic", "Upper Paleolithic", "Neolithic", "Early Bronze Age", "Late Bronze Age", "Iron Age"), periods$name)]
  if (anyNA(chain) || any(diff(chain) <= 0)) {
    stop("the prehistoric periods are not in the expected order (Lower Paleolithic ... Iron Age); the dates may have been misread")
  }
  periods
}

register_source(new_source(
  id = "prehistory",
  title = "Periods of prehistory (Wikipedia)",
  url = "https://en.wikipedia.org/wiki/Three-age_system",
  citation = "Wikipedia contributors, articles 'Lower Paleolithic', 'Middle Paleolithic', 'Upper Paleolithic', 'Mesolithic', 'Neolithic', 'Copper Age', 'Bronze Age' and 'Iron Age', Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0.",
  fetch = fetch_prehistory,
  max_age_days = 30,
  min_rows = 9
))

register_metric("prehistory", needs = "prehistory", build = function(src) {
  # One block of rows per age (Stone, Copper, Bronze, Iron), earliest first, so a row is one colour.
  packed <- pack_by_group(src$prehistory$t0, src$prehistory$t1, src$prehistory$age)
  ages <- packed$labels[packed$labels != ""]
  colours <- set_names(bar_palette(length(ages)), ages)

  data <- src$prehistory |>
    mutate(
      lane = packed$lane,
      fill = colours[age], text_colour = text_on(fill),
      label = name, title = name, instant = FALSE,
      body = hover_lines(
        `Part of` = age,
        Region = region,
        `Also called` = aka,
        Duration = format_duration(t1 - t0),
        Caveat = if_else(
          is.na(region),
          "Dates are approximate and differ from region to region.",
          paste0("Dates are for the ", region, " only; other regions differ.")
        )
      )
    ) |>
    select(t0, t1, lane, label, fill, text_colour, instant, title, body)

  new_metric(
    id = "prehistory", label = "Prehistory", group = "human", kind = "spans",
    data = data, lane_labels = packed$labels,
    legend = tibble(series = ages, label = ages, colour = unname(colours)),
    description = "The Stone, Copper, Bronze and Iron Ages. Each period began and ended at different times in different places, so the bars show approximate dates and overlap.",
    sources = "prehistory"
  )
})
