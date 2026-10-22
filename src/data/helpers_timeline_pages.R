# Readers for Wikipedia's long dated lists ("Timeline of Christianity"), for the datasets built from
# them (ds_religious_events.R). Needs helpers.R and helpers_history.R (the hist_* parsers).
#
# These articles are laid out differently page to page: bullet lists ("1517 The Ninety-five
# Theses..."), definition lists (a date, then its text) and tables (Year | History | Image).
# timeline_page_items() reads all three into one table.

TIMELINE_MONTHS <- "January|February|March|April|May|June|July|August|September|October|November|December"

# A heading that ends the article's content.
TIMELINE_END_HEADINGS <- "^(See also|References|Footnotes|Notes|Sources|Bibliography|External links|Further reading)\\b"

# The era the dates under a heading are in. A heading that says BCE/BC, or CE/AD or names a
# century, settles it; any other heading ("Second Temple period") leaves the era as it was.
timeline_heading_era <- function(heading, current) {
  case_when(
    str_detect(heading, "\\b(?:BCE|BC)\\b") ~ "BCE",
    str_detect(heading, "\\b(?:CE|AD)\\b|\\bcentury\\b|\\bcenturies\\b") ~ "CE",
    .default = current
  )
}

# The first `n` articles an item links to, as tibble(title, anchor): the article's title and the
# words of the item that link to it. Links that are only dates ("325", "AD 325", "March 12",
# "Anno Domini") and links to other namespaces (File:, Help:) are left out.
TIMELINE_LINKS_KEPT <- 3
timeline_links <- function(html, n = TIMELINE_LINKS_KEPT) {
  anchors <- hist_fragment(html) |> xml2::xml_find_all("//a[starts-with(@href, '/wiki/')]")
  links <- tibble(
    title = xml2::xml_attr(anchors, "href") |>
      str_remove("^/wiki/") |>
      str_remove("#.*$") |>
      map_chr(utils::URLdecode) |>
      str_replace_all("_", " "),
    anchor = clean_text(xml2::xml_text(anchors))
  )
  is_date <- str_detect(links$title, regex(
    paste0(
      "^(?:(?:AD|CE|BC|BCE)\\s+)?[0-9]{1,4}(?:\\s+(?:AD|CE|BC|BCE))?$",
      "|^(?:", TIMELINE_MONTHS, ")(?:\\s+[0-9]{1,2})?$|^Anno Domini$|^Common Era$"
    ),
    ignore_case = TRUE
  ))
  links |>
    filter(!is_date, !str_detect(title, ":"), nzchar(title)) |>
    distinct(title, .keep_all = TRUE) |>
    head(n)
}

# Split the date an entry opens with from the rest. "325, 20 May-19 June: The First Council" is
# year 325 and text "The First Council"; "December 167 BCE: ..." is 167 BCE; "7-26 Brief period"
# is the range 7-26. Returns list(date, text), with date NA when the text does not open with one.
timeline_split_date <- function(text) {
  text <- str_replace_all(text, HIST_DASH, "-")

  # A date written day-first or month-first ("17 October 2016 - Battle of Mosul", "October 17, 2016:")
  # has its year last; the leading number is the day, not a year.
  full <- str_match(text, regex(paste0(
    "^(?:[0-9]{1,2}\\s+(?:", TIMELINE_MONTHS, ")\\b,?|(?:", TIMELINE_MONTHS, ")\\b\\s+[0-9]{1,2},?)\\s+([0-9]{3,4})\\b(.*)$"
  ), ignore_case = TRUE))
  if (!is.na(full[1])) {
    return(list(date = full[2], text = str_remove(full[3], "^[\\s,;:.-]+")))
  }

  m <- str_match(text, regex(paste0(
    "^(?:(?:c|ca|circa|around|about)\\.?\\s*)?",
    "(?:(?:", TIMELINE_MONTHS, ")\\s+(?:[0-9]{1,2},?\\s+)?)?",
    "(?:(?:AD|CE)\\s+)?",
    "([0-9]{1,4})",
    "(?:\\s*(?:or|to|-)\\s*(?:c\\.?\\s*)?([0-9]{1,4}))?",
    "(?:\\s*(BCE|BC|CE|AD)\\b)?",
    "(.*)$"
  ), ignore_case = TRUE))
  if (is.na(m[1])) return(list(date = NA_character_, text = text))

  rest <- m[5] |>
    str_remove("^[\\s,;:.-]+") |>
    # a day and month after the year: "20 May-19 June:", "12 March -" (\\b: not the start of "Augustine")
    str_remove(regex(paste0(
      "^(?:[0-9]{1,2}\\s+)?(?:", TIMELINE_MONTHS, ")\\b(?:\\s+[0-9]{1,2}\\b)?",
      "(?:\\s*-\\s*(?:[0-9]{1,2}\\s+)?(?:(?:", TIMELINE_MONTHS, ")\\b)?(?:\\s+[0-9]{1,2}\\b)?)?\\s*[:-]?\\s*"
    ), ignore_case = TRUE)) |>
    str_remove("^[\\s:;-]+")
  list(
    date = paste0(m[2], if (!is.na(m[3])) paste0("-", m[3]), if (!is.na(m[4])) paste0(" ", m[4])),
    text = rest
  )
}

# Dates as these pages write them that hist_parse_range() does not read, reduced to a year or
# range it does: "7 October 2023", "December 30, 1066" and "1066 December 30" are the year, and
# "1350s" is "1350-1359". Anything else (such as "Mid-19th century") is left as it is.
timeline_normalise_date <- function(x) {
  month <- paste0("(?:", TIMELINE_MONTHS, ")")
  year_after <- str_match(x, regex(paste0("^(?:[0-9]{1,2}\\s+)?", month, "\\s+(?:[0-9]{1,2},?\\s+)?([0-9]{3,4})\\b"), ignore_case = TRUE))[, 2]
  year_before <- str_match(x, regex(paste0("^([0-9]{3,4})\\s+", month, "\\b"), ignore_case = TRUE))[, 2]
  decade <- str_match(x, "^([0-9]{3})0s$")[, 2]
  coalesce(year_after, year_before, if_else(is.na(decade), NA_character_, paste0(decade, "0-", decade, "9")), x)
}

# A short title for an entry: its text up to the first comma, semicolon, colon or bracket ("The
# Ninety-five Theses of Martin Luther, nailed to the door..." -> "The Ninety-five Theses of Martin
# Luther"), cut at a word if still long. A lead that says nothing on its own ("In Iraq", "At some
# point during this period") is replaced by `subject`, the title of the article the entry is about,
# when there is one.
timeline_title <- function(text, subject = NA_character_, max = 80) {
  clause <- str_match(text, "^(.{3,}?)(?:, |; |: | \\(| - |$)")[, 2] |>
    coalesce(text) |>
    str_remove("[.\\s]+$")
  weak <- str_count(clause, "\\S+") < 3 |
    str_detect(clause, regex("^(?:at|in|on|by|during|prior|because|after|before|around|about|when|with|from|following|since|while|according|unpaid)\\b", ignore_case = TRUE))
  title <- if_else(weak & !is.na(subject), subject, clause)
  if_else(
    nchar(title) > max,
    str_replace(paste0(str_trunc(title, max, ellipsis = ""), "…"), "\\s+\\S*…$", "…"),
    title
  )
}

# All the dated entries of a timeline article, as tibble(section, date_text, era, text, links):
#   section  the nearest heading above the entry
#   era      BCE or CE, from the headings (date_text may still say otherwise: "167 BCE")
#   text     the entry without its date, tidied (footnote marks are already gone)
#   links    list-column: the first few articles the entry links to, tibble(title, anchor), which
#            say what the entry is about and so how notable it is (see helpers_notability.R)
# Entries with no date are left out. `skip` is a regex for headings whose sections to ignore;
# `era` is the era before any heading says otherwise. `dated_by_heading` is for pages whose
# headings are years ("1844 AD / 1 BE") with undated bullets beneath: each bullet takes its
# heading's year.
timeline_page_items <- function(doc, skip = "^$", era = "CE", dated_by_heading = FALSE) {
  body <- xml2::xml_find_first(doc, "//div[contains(@class, 'mw-parser-output')]")
  if (inherits(body, "xml_missing")) body <- xml2::xml_root(doc)

  section <- NA_character_
  heading_year <- NA_character_
  skipping <- FALSE
  rows <- list()
  add <- function(date_text, html, entry_era) {
    split <- if (is.na(date_text)) {
      timeline_split_date(hist_text(html))
    } else {
      list(date = timeline_normalise_date(date_text), text = hist_text(html))
    }
    rows[[length(rows) + 1]] <<- tibble(
      section = section, date_text = split$date, era = entry_era, text = split$text,
      links = list(timeline_links(html))
    )
  }

  for (node in xml2::xml_children(body)) {
    name <- xml2::xml_name(node)
    classes <- coalesce(xml2::xml_attr(node, "class"), "")

    if (name == "div" && str_detect(classes, "mw-heading")) {
      heading <- coalesce(str_remove(clean_text(xml2::xml_text(node)), "\\s*\\[edit\\]$"), "")
      if (str_detect(heading, TIMELINE_END_HEADINGS)) break
      section <- heading
      heading_year <- str_extract(heading, "^[0-9]{3,4}")
      skipping <- str_detect(heading, skip)
      era <- timeline_heading_era(heading, era)
      next
    }
    if (skipping || str_detect(classes, "sidebar|navbox|hatnote|box-")) next

    if (name == "ul") {
      for (li in xml2::xml_find_all(node, ".//li")) {
        add(if (dated_by_heading) heading_year else NA_character_, hist_own_html(li), era)
      }
    } else if (name == "dl") {
      date_text <- NA_character_
      for (item in xml2::xml_children(node)) {
        if (xml2::xml_name(item) == "dt") {
          date_text <- clean_text(xml2::xml_text(item))
        } else if (xml2::xml_name(item) == "dd") {
          add(date_text, hist_own_html(item), era)
        }
      }
    } else if (name == "table" && str_detect(classes, "wikitable")) {
      headers <- xml2::xml_text(xml2::xml_find_all(node, "(.//tr[th])[1]/th"))
      text_col <- match(TRUE, str_detect(headers, regex("history|event|description", ignore_case = TRUE)))
      if (is.na(text_col)) text_col <- 2L
      for (tr in xml2::xml_find_all(node, ".//tr[td]")) {
        cells <- xml2::xml_find_all(tr, "./td")
        if (length(cells) < text_col) next
        add(clean_text(xml2::xml_text(cells[[1]])), hist_own_html(cells[[text_col]]), era)
      }
    }
  }

  list_rbind(rows) |> filter(!is.na(date_text), nzchar(text))
}

# Fetch several Wikipedia articles at once (a few requests in flight at a time) and parse each.
# Returns a named list of parsed documents, NULL for any that could not be fetched.
wikipedia_html_many <- function(pages, concurrency = 4) {
  pool <- curl::new_pool(total_con = concurrency, host_con = concurrency)
  responses <- set_names(vector("list", length(pages)), pages)

  for (page in pages) {
    url <- paste0(
      "https://en.wikipedia.org/w/api.php?action=parse&prop=text&format=json&formatversion=2&redirects=1&page=",
      curl::curl_escape(page)
    )
    handle <- curl::new_handle(useragent = USER_AGENT, timeout = 60, connecttimeout = 15, followlocation = TRUE)
    local({
      this <- page
      curl::curl_fetch_multi(
        url, handle = handle, pool = pool,
        done = \(response) responses[[this]] <<- response, fail = \(message) NULL
      )
    })
  }
  curl::multi_run(pool = pool)

  imap(responses, \(response, page) {
    parse_one <- \(html) {
      doc <- xml2::read_html(jsonlite::fromJSON(html)$parse$text)
      xml2::xml_remove(xml2::xml_find_all(doc, "//sup[contains(@class, 'reference')]"))
      doc
    }
    if (!is.null(response) && response$status_code == 200) {
      html <- rawToChar(response$content)
      Encoding(html) <- "UTF-8"
      doc <- tryCatch(parse_one(html), error = \(e) NULL)
      if (!is.null(doc)) return(doc)
    }
    tryCatch(wikipedia_html(page), error = \(e) NULL)  # one more try, on its own
  })
}
