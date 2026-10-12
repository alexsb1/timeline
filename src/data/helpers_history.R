# Shared parsers for human-written dates and Wikipedia markup, used by the datasets that read
# history from Wikipedia and Wikidata (ds_climate_events.R, ds_bond_events.R, ds_prehistory.R,
# ds_civilisations.R). Every name starts `hist_`. Needs helpers.R.

# Dashes of every width, ready for a regex character class (hyphen last).
HIST_DASH <- paste0("[", intToUtf8(c(0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2212)), "-]")
HIST_ERA <- "BCE|BC|AD|CE|BP|MYA|Ma|kya|ka"

# One calendar year `t` from a number and the era it is written in. `century` = "century" reads
# the number as an ordinal century ("12th century BC"): a start edge is the first year of it and
# an end edge the last, so "12th century BC - 6th century AD" runs 1200 BC to 600 AD.
hist_year <- function(number, era, century = NA_character_, edge = "start") {
  era <- tolower(era)
  if (!is.na(century)) {
    bc <- era %in% c("bc", "bce")
    return(case_when(
      bc & edge == "start" ~ 1 - number * 100,
      bc ~ 1 - ((number - 1) * 100 + 1),
      edge == "start" ~ (number - 1) * 100 + 1,
      .default = number * 100
    ))
  }
  # "3000 - 0": there is no year 0 BC or AD, so a bare 0 means the turn of the era.
  if (number == 0 && era %in% c("bc", "bce", "ad", "ce")) return(0)
  switch(era,
    bc = , bce = 1 - number,
    ad = , ce = number,
    bp = bp_to_year(number),
    mya = , ma = ma_to_year(number),
    kya = , ka = ka_to_year(number),
    NA_real_
  )
}

# One end of a range: "3,150 BC", "AD 1521", "12th century BC", "476/480 AD", "300,000 BP". The
# number is mandatory and anything after it comes back as `rest`, so prose is easy to refuse.
hist_endpoint <- function(text) {
  m <- str_match(str_squish(text), regex(paste0(
    "^(?:(", HIST_ERA, ")\\s+)?",                    # era first: "AD 1521"
    "([0-9][0-9,]*(?:\\.[0-9]+)?)",                  # the number
    "(?:/[0-9]+)?",                                  # "476/480": keep the first
    "(?:\\s*(?:st|nd|rd|th)\\s+(century))?",         # "12th century"
    "(?:\\s*(", HIST_ERA, ")\\b)?",                  # era last: "3150 BC"
    "\\s*(.*)$"
  ), ignore_case = TRUE))
  if (is.na(m[1])) return(NULL)
  list(
    number = as.numeric(str_remove_all(m[3], ",")), era = coalesce(m[2], m[5]),
    century = m[4], rest = m[6]
  )
}

# One span such as "650-600", "c. 3150 BC - 30 BC", "2.5 to present" or "From 10,000". Eras
# carry over ("1200-546 BC" is BC at both ends) and `default_era` fills in when none is written.
# Returns list(t0, t1, open_start, open_end) or NULL if the text is not a date or a range:
# prose is refused rather than guessed at.
hist_parse_span <- function(x, default_era = NA_character_) {
  x <- x |>
    str_remove("\\s*\\(.*$") |>                       # "(much discussed)"
    str_remove(regex("^(?:c|ca|circa|about|around|approximately)\\.?\\s*(?=[0-9]|AD|BC|CE)", ignore_case = TRUE)) |>
    str_replace_all(regex("(?<=[-\\s])(?:c|ca|circa)\\.?\\s*(?=[0-9]|AD|BC|CE)", ignore_case = TRUE), "") |>
    str_squish()
  open_start <- str_detect(x, regex("^before\\s", ignore_case = TRUE))
  open_end <- str_detect(x, regex("^from\\s", ignore_case = TRUE)) ||
    str_detect(x, regex("(?:-|\\bto)\\s*(?:the\\s+)?(?:present|now)\\s*$", ignore_case = TRUE))
  x <- x |>
    str_remove(regex("^(?:before|from)\\s+", ignore_case = TRUE)) |>
    str_remove(regex("\\s*(?:-|\\bto)\\s*(?:the\\s+)?(?:present|now)\\s*$", ignore_case = TRUE))

  parts <- str_split_fixed(x, "\\s*(?:-|\\bto\\b)\\s*", 2)
  a <- hist_endpoint(parts[1])
  b <- if (nzchar(parts[2])) hist_endpoint(parts[2])
  if (is.null(a) || nzchar(a$rest) || (nzchar(parts[2]) && is.null(b))) return(NULL)
  if (!is.null(b) && nzchar(b$rest) && !str_detect(b$rest, "^[,;]")) return(NULL)

  era_b <- coalesce(b$era %||% NA_character_, default_era)
  era_a <- coalesce(a$era, era_b, default_era)
  if (is.na(era_a) || (!is.null(b) && is.na(era_b))) return(NULL)
  t0 <- hist_year(a$number, era_a, a$century, "start")
  t1 <- if (is.null(b)) t0 else hist_year(b$number, era_b, b$century, "end")
  if (anyNA(c(t0, t1))) return(NULL)

  if (open_start) return(list(t0 = NA_real_, t1 = t0, open_start = TRUE, open_end = FALSE))
  if (open_end) return(list(t0 = t0, t1 = NA_real_, open_start = FALSE, open_end = TRUE))
  # Some sources list the end before the start, so order the pair.
  list(t0 = min(t0, t1), t1 = max(t0, t1), open_start = FALSE, open_end = FALSE)
}

# Vectorised: a tibble(t0, t1, open_start, open_end) with one row per text, all NA where the
# text could not be read. `default_era` may be one value or one per text. Several spans
# separated by ";" ("202 BC - 9 AD; 25-220 AD") are merged into the one that covers them all.
hist_parse_range <- function(text, default_era = NA_character_) {
  unreadable <- tibble(t0 = NA_real_, t1 = NA_real_, open_start = FALSE, open_end = FALSE)
  if (length(text) == 0) return(unreadable[0, ])
  map2_dfr(text, rep_len(default_era, length(text)), \(one, era) {
    if (is.na(one)) return(unreadable)
    spans <- one |>
      str_replace_all(HIST_DASH, "-") |>
      str_remove_all("[≈~?]") |>
      str_split_1("\\s*;\\s*") |>
      map(\(piece) hist_parse_span(piece, era)) |>
      compact()
    if (length(spans) == 0) return(unreadable)
    tibble(
      t0 = if (any(map_lgl(spans, \(s) s$open_start))) NA_real_ else min(map_dbl(spans, "t0")),
      t1 = if (any(map_lgl(spans, \(s) s$open_end))) NA_real_ else max(map_dbl(spans, "t1")),
      open_start = any(map_lgl(spans, \(s) s$open_start)),
      open_end = any(map_lgl(spans, \(s) s$open_end))
    )
  })
}


# ---- shared: reading Wikipedia markup -----------------------------------------

# The text of a piece of an article with a space between neighbouring elements (the article has
# "</i><i>and" in places, which plain text extraction glues into one word), tidied. `html` is
# markup, as returned by hist_own_html().
hist_text <- function(html) {
  hist_fragment(html) |>
    rvest::html_text2() |>
    str_replace_all("\\s+(?=[,.;:!?)\\]])", "") |>   # the spaces added between elements, before punctuation
    str_replace_all("(?<=[(\\[])\\s+", "") |>
    clean_text()
}

# Markup (which may be plain text with no tags at all) as a parsed document, with a space put
# between neighbouring elements. Wrapped in a <div> so that read_html() never mistakes plain
# text for a file name.
hist_fragment <- function(html) {
  xml2::read_html(paste0("<div>", str_replace_all(html, "><", "> <"), "</div>"))
}

# The markup of a node's own content, leaving out nested lists (they are items in their own right).
hist_own_html <- function(node) {
  xml2::xml_find_all(node, "./node()[not(self::ul or self::ol)]") |>
    map_chr(as.character) |>
    paste(collapse = "")
}

# The text of the link that opens a piece of markup, or NA when the first words are not in one.
hist_first_link <- function(html) {
  link <- hist_fragment(html) |>
    xml2::xml_find_first("(//body//text()[normalize-space(.) != ''])[1]/ancestor::a[1]")
  if (inherits(link, "xml_missing")) NA_character_ else clean_text(xml2::xml_text(link))
}

# The first infobox's date lines: a tibble(label, line). `label` is the row heading (NA for the
# unlabelled row under the title, where many infoboxes put the date range) and each <br>-separated
# line of the value is its own row. Styles and footnote markers are removed first, or their text
# leaks into the dates.
hist_infobox_lines <- function(doc) {
  box <- xml2::xml_find_first(doc, "//table[contains(@class, 'infobox')]")
  if (inherits(box, "xml_missing")) return(tibble(label = character(), line = character()))
  xml2::xml_remove(xml2::xml_find_all(box, ".//style | .//sup"))
  xml2::xml_find_all(box, ".//tr[td]") |>
    map_dfr(\(row) {
      label <- xml2::xml_find_first(row, "./th")
      value <- xml2::xml_find_first(row, "./td")
      tibble(
        label = if (inherits(label, "xml_missing")) NA_character_ else clean_text(xml2::xml_text(label)),
        line = str_split_1(rvest::html_text2(hist_fragment(as.character(value))), "\n") |>
          clean_text()
      )
    }) |>
    filter(!is.na(line))
}

# The date range of an article, from the first infobox row that is labelled like one ("Dates")
# or sits unlabelled under the title and reads as a range. A single date is not a range here.
# Returns the lines that parsed, as tibble(label, line, t0, t1, open_start, open_end); empty if none.
hist_infobox_dates <- function(doc, default_era = NA_character_, first_only = TRUE) {
  lines <- hist_infobox_lines(doc) |>
    filter(is.na(label) | str_detect(label, regex("^(?:dates?|date range|period|lifespan|existed)$", ignore_case = TRUE)))
  parsed <- bind_cols(lines, hist_parse_range(lines$line, default_era)) |>
    filter(!is.na(t0) | open_start | open_end) |>
    filter(str_detect(line, paste0("[0-9].*(?:", HIST_DASH, "|\\bto\\b).*[0-9]")))
  if (first_only) head(parsed, 1) else parsed
}

# The one-line Wikipedia description ("Bronze Age civilization on Crete") of each page, in one
# request. Named by the requested titles; NA where a page has none.
hist_short_descriptions <- function(titles) {
  response <- http_get_text(
    "https://en.wikipedia.org/w/api.php",
    query = list(
      action = "query", prop = "description", titles = paste(titles, collapse = "|"),
      redirects = "1", format = "json", formatversion = "2"
    )
  ) |>
    jsonlite::fromJSON()
  pages <- response$query$pages
  redirects <- response$query$redirects
  # A requested title may have been redirected to another title; map back to what was asked for.
  asked <- if (is.null(redirects)) pages$title else coalesce(redirects$from[match(pages$title, redirects$to)], pages$title)
  descriptions <- set_names(pages$description %||% rep(NA_character_, nrow(pages)), asked)
  descriptions[titles]
}
