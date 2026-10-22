# Ancient civilisations and their lifetimes, read from each one's English Wikipedia article
# (CC BY-SA 4.0). Uses the date and markup helpers (hist_*) defined in ds_climate_events.R.
#
# Nothing publishes a table of these. The article that used to ("Ancient history") has none now, and
# Wikidata's "ancient civilization" items are too patchy to use: most have no dates, and the ones that
# do disagree with the articles (Ancient Egypt starts in 4000 BCE, Sumer in 5500 BCE). What every
# article does carry is a date range in its infobox, so the page list below (which civilisations
# count as major is an editorial choice, and so is written here) names the articles, and the dates,
# one-line description and the rest come from Wikipedia. The tooltip's region is a broad grouping
# from this list, used for the bar colours too.

CIVILISATION_PAGES <- tribble(
  ~page,                        ~name,                       ~region,
  "Sumer",                      "Sumer",                     "West Asia",
  "Akkadian Empire",            "Akkadian Empire",           "West Asia",
  "Babylonia",                  "Babylonia",                 "West Asia",
  "Assyria",                    "Assyria",                   "West Asia",
  "Elam",                       "Elam",                      "West Asia",
  "Hittites",                   "Hittites",                  "West Asia",
  "Phoenicia",                  "Phoenicia",                 "West Asia",
  "Achaemenid Empire",          "Achaemenid Empire",         "West Asia",
  "Parthian Empire",            "Parthian Empire",           "West Asia",
  "Sasanian Empire",            "Sasanian Empire",           "West Asia",
  "Ancient Egypt",              "Ancient Egypt",             "Africa",
  "Kingdom of Kush",            "Kush (Nubia)",              "Africa",
  "Ancient Carthage",           "Carthage",                  "Africa",
  "Kingdom of Aksum",           "Aksum",                     "Africa",
  "Indus Valley Civilisation",  "Indus Valley",              "South Asia",
  "Maurya Empire",              "Maurya Empire",             "South Asia",
  "Gupta Empire",               "Gupta Empire",              "South Asia",
  "Minoan civilization",        "Minoans",                   "Europe",
  "Mycenaean Greece",           "Mycenaean Greece",          "Europe",
  "Ancient Greece",             "Ancient Greece",            "Europe",
  "Etruscan civilization",      "Etruscans",                 "Europe",
  "Ancient Rome",               "Ancient Rome",              "Europe",
  "Shang dynasty",              "Shang dynasty",             "East Asia",
  "Zhou dynasty",               "Zhou dynasty",              "East Asia",
  "Qin dynasty",                "Qin dynasty",               "East Asia",
  "Han dynasty",                "Han dynasty",               "East Asia",
  "Norte Chico civilization",   "Norte Chico (Caral)",       "Americas",
  "Olmecs",                     "Olmecs",                    "Americas",
  "Chavín culture",        "Chavín",               "Americas",
  "Zapotec civilization",       "Zapotec",                   "Americas",
  "Nazca culture",              "Nazca",                     "Americas",
  "Moche culture",              "Moche",                     "Americas",
  "Maya civilization",          "Maya",                      "Americas",
  "Wari Empire",                "Wari",                      "Americas"
)

# Region colours: Okabe-Ito, which stays readable for colour-blind viewers, in the order the
# regions first appear in the list above.
CIVILISATION_REGION_COLOURS <- c(
  `West Asia` = "#E69F00", Africa = "#56B4E9", `South Asia` = "#009E73", Europe = "#CC79A7",
  `East Asia` = "#D55E00", Americas = "#0072B2"
)

# The lead section of each article (which is where the infobox is) fetched four at a time:
# thirty-odd whole articles one after another would run to half a minute. A page that fails here is
# tried once more on its own. Returns a named list of parsed documents (NULL for a page that failed).
civilisation_lead_sections <- function(pages, concurrency = 4) {
  pool <- curl::new_pool(total_con = concurrency, host_con = concurrency)
  responses <- set_names(vector("list", length(pages)), pages)

  for (page in pages) {
    url <- paste0(
      "https://en.wikipedia.org/w/api.php?action=parse&prop=text&section=0&format=json&formatversion=2&redirects=1&page=",
      curl::curl_escape(page)
    )
    handle <- curl::new_handle(useragent = USER_AGENT, timeout = 45, connecttimeout = 15, followlocation = TRUE)
    local({
      this <- page
      curl::curl_fetch_multi(
        url, handle = handle, pool = pool,
        done = \(response) responses[[this]] <<- response,
        fail = \(message) NULL
      )
    })
  }
  curl::multi_run(pool = pool)

  imap(responses, \(response, page) {
    if (!is.null(response) && response$status_code == 200) {
      html <- rawToChar(response$content)
      Encoding(html) <- "UTF-8"
      return(xml2::read_html(jsonlite::fromJSON(html)$parse$text))
    }
    tryCatch(wikipedia_html(page), error = \(e) NULL)
  })
}

fetch_civilisations <- function() {
  pages <- CIVILISATION_PAGES
  documents <- civilisation_lead_sections(pages$page)

  dates <- map(documents, \(doc) {
    if (is.null(doc)) return(NULL)
    # Dates with no era written (Gupta Empire "c. 240-c. 579") are Common Era.
    found <- hist_infobox_dates(doc, default_era = "CE")
    if (nrow(found) == 0) NULL else found
  })
  readable <- !map_lgl(dates, is.null)

  if (any(!readable)) {
    message("[civilisations] no date range in the infobox of: ", paste(pages$page[!readable], collapse = ", "))
  }
  if (sum(readable) < 0.85 * nrow(pages)) {
    stop("could not read a date range for ", sum(!readable), " of ", nrow(pages), " civilisations (",
         paste(head(pages$page[!readable], 5), collapse = ", "), "...); the article infoboxes may have changed")
  }

  descriptions <- tryCatch(hist_short_descriptions(pages$page), error = \(e) rep(NA_character_, nrow(pages)))

  civilisations <- pages |>
    mutate(
      date_text = map_chr(dates, \(d) if (is.null(d)) NA_character_ else d$line),
      t0 = map_dbl(dates, \(d) if (is.null(d)) NA_real_ else d$t0),
      t1 = map_dbl(dates, \(d) if (is.null(d)) NA_real_ else d$t1),
      open_end = map_lgl(dates, \(d) !is.null(d) && d$open_end),
      description = unname(descriptions)
    ) |>
    filter(readable) |>
    arrange(t0)

  # A civilisation that is still ongoing, or that ends before it starts, or that falls outside the
  # age of cities, means the dates were misread (an era taken for the wrong one, say).
  if (any(civilisations$open_end | civilisations$t1 <= civilisations$t0)) {
    stop("a civilisation has no end date or ends before it starts: ",
         paste(civilisations$name[civilisations$open_end | civilisations$t1 <= civilisations$t0], collapse = ", "))
  }
  if (any(civilisations$t0 < -6000 | civilisations$t1 > 1700)) {
    bad <- civilisations$name[civilisations$t0 < -6000 | civilisations$t1 > 1700]
    stop("civilisation dates outside 6000 BCE to 1700 CE: ", paste(bad, collapse = ", "), "; the date reading may have gone wrong")
  }
  civilisations |> select(page, name, region, date_text, t0, t1, description)
}

register_source(new_source(
  id = "civilisations",
  title = "Ancient civilisations (Wikipedia)",
  url = "https://en.wikipedia.org/wiki/Ancient_history",
  citation = "Wikipedia contributors, one article per civilisation (for example 'Ancient Egypt', 'Sumer', 'Maya civilization'), Wikipedia, The Free Encyclopedia. Dates from each article's infobox; text available under CC BY-SA 4.0.",
  fetch = fetch_civilisations,
  max_age_days = 30,
  min_rows = 15
))

# Wikipedia's one-line descriptions often repeat the dates ("State in Mesopotamia (c. 2334-2154 BC)",
# "Civilization in southern Peru, c. 100 BC-800 AD"); the tooltip already has a When line, so drop them.
civilisation_blurb <- function(description) {
  description |>
    str_remove_all("\\s*\\([^()]*[0-9][^()]*\\)") |>
    str_remove(",\\s*(?:c\\.\\s*)?[0-9][^,]*$") |>
    clean_text()
}

register_metric("civilisations", needs = "civilisations", build = function(src) {
  # One block of rows per region, the region that starts earliest first, so a row is one colour.
  packed <- pack_by_group(src$civilisations$t0, src$civilisations$t1, src$civilisations$region)

  data <- src$civilisations |>
    mutate(
      lane = packed$lane,
      fill = CIVILISATION_REGION_COLOURS[region], text_colour = text_on(fill),
      label = name, title = name, instant = FALSE,
      body = hover_lines(
        Region = region,
        About = wrap_html(civilisation_blurb(description)),
        Duration = format_duration(t1 - t0)
      )
    ) |>
    select(t0, t1, lane, label, fill, text_colour, instant, title, body)

  regions <- packed$labels[packed$labels != ""]
  new_metric(
    id = "civilisations", label = "Ancient civilisations", group = "human", kind = "spans",
    data = data, lane_labels = packed$labels,
    legend = tibble(series = regions, label = regions, colour = unname(CIVILISATION_REGION_COLOURS[regions])),
    description = "Major ancient civilisations and empires, coloured by region. Start and end dates are the ones given in each civilisation's Wikipedia article and are approximate; many have no sharp beginning or end.",
    sources = "civilisations"
  )
})
