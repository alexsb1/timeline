# Religious events, as a layer of their own: the Abrahamic religions, laid out as one block of rows
# per tradition so the split is visible. The rest of the timeline is the secular record (natural
# science and documented history); what a religious tradition says happened is not in the same
# category, so it is never mixed into those panels.
#
# WHERE IT COMES FROM (everything is Wikipedia, CC BY-SA 4.0, unless it says otherwise)
#   Shared        the Masoretic table in "Chronology of the Bible": the events Jews, Christians and
#                 Muslims all tell (Creation, Abraham, the Exodus...), dated by that chronology.
#   Christianity  "Timeline of Christianity"
#   Judaism       "Timeline of Jewish history" (from the return from exile, 539 BCE)
#   Islam         "Timeline of the history of Islam", one article per century
# Each of those timelines runs to hundreds of entries, most of them minor, so the MAJOR religious
# ones are chosen (see select_major_entries()). Everything is fetched once and cached for a year: it
# is history, it does not change. The panel can only be as good as the articles it is drawn from.
#
# HAND-WRITTEN, in curated/ (read at every start-up, never cached)
#   religious_events.csv    extra events to plot. Columns: event, tradition (Common, Christianity,
#                           Judaism, Islam or Other), start_year, start_era (CE or BCE), end_year,
#                           end_era (both blank for a single moment), notes, source. Used for the
#                           Flood, and for the dates of Jesus' life, which the Christianity
#                           timeline leaves out.
#   religious_excluded.csv  articles to drop from the chosen entries (column `subject`: the title
#                           of the Wikipedia article the entry is about, shown in its tooltip as
#                           "About"). The selection is by rules, and rules misfire; this is the fix.
#
# ADDING A RELIGION: add its group to RELIGION_GROUPS ("Other" is there, empty, for this), then
# either add a timeline to RELIGION_TIMELINES or put its events in religious_events.csv.

# ---- the groups, top to bottom -----------------------------------------------------------------

RELIGION_GROUPS <- tribble(
  ~group,         ~axis_label,    ~legend_label,                    ~colour,
  "Common",       "Shared",       "Shared Abrahamic accounts",      "#BDC3CD",
  "Christianity", "Christianity", "Christianity",                   "#9DBDE8",
  "Judaism",      "Judaism",      "Judaism",                        "#EFCB68",
  "Islam",        "Islam",        "Islam",                          "#88CDA3",
  "Other",        "Other",        "Other Abrahamic religions",      "#D3A5DB"
)

century_page <- function(n) {
  suffix <- if_else(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  paste0("Timeline of the history of Islam (", n, suffix, " century)")
}

# The timelines read from Wikipedia, one source each. `pages` are English Wikipedia article titles;
# `skip` is a regex for headings whose sections are not events of the religion; `era` the era before
# a heading says otherwise; `earliest` the first year the religion can have events (an entry dated
# before it has been misread); `min_entries` guards against a page that has been restructured.
# (`dated_by_heading = TRUE` is for pages whose headings are years, such as the Bahai timeline.)
RELIGION_TIMELINES <- list(
  christianity = list(
    group = "Christianity", pages = "Timeline of Christianity", era = "CE", earliest = -10, min_entries = 400,
    # The first sections are Judea's political background, Jesus' ministry (undated) and prophecies.
    skip = "^(Background historical timeline|Timeline of Jesus|Prophecies)"
  ),
  judaism = list(
    group = "Judaism", pages = "Timeline of Jewish history", era = "BCE", earliest = -600, min_entries = 150,
    # Ancient Israel is the shared tradition (above); the last section is a bare list of years.
    skip = "^(Ancient Israel|Years in the State of Israel)"
  ),
  islam = list(
    group = "Islam", pages = century_page(6:21), era = "CE", earliest = 500, min_entries = 500, skip = "^$"
  )
)


# ---- reading the timelines -------------------------------------------------------------------

# One tradition's timeline: every dated entry with the articles it links to, what Wikidata says
# each of those is, and how many language editions each exists in. Nothing is chosen here: that
# happens when the panel is built (select_major_entries), so the rules can change without a
# re-download.
fetch_religion_timeline <- function(spec) {
  pages <- spec$pages
  docs <- if (length(pages) == 1) set_names(list(wikipedia_html(pages)), pages) else wikipedia_html_many(pages)
  failed <- names(docs)[map_lgl(docs, is.null)]
  if (length(failed) > 0.25 * length(pages)) {
    stop("could not fetch ", length(failed), " of ", length(pages), " pages (", paste(head(failed, 3), collapse = "; "), "...)")
  }
  if (length(failed) > 0) message("[religion] could not fetch: ", paste(failed, collapse = "; "))

  entries <- imap(compact(docs), \(doc, page) {
    timeline_page_items(doc, skip = spec$skip, era = spec$era, dated_by_heading = isTRUE(spec$dated_by_heading)) |>
      mutate(page = page)
  }) |>
    list_rbind()
  if (nrow(entries) < spec$min_entries) {
    stop("only ", nrow(entries), " entries found in ", paste(head(pages, 2), collapse = ", "), " (expected ", spec$min_entries, " or more); the layout may have changed")
  }

  # "Before 44" is a date, not a range; "1948-present" has no end yet (resolved when the panel is built).
  entries <- bind_cols(entries, hist_parse_range(entries$date_text, entries$era)) |>
    mutate(t0 = if_else(open_start, t1, t0)) |>
    filter(!is.na(t0), t0 >= spec$earliest, t0 < today_year() + 1, open_end | (!is.na(t1) & t1 >= t0))
  if (nrow(entries) < 0.7 * spec$min_entries) {
    stop("only ", nrow(entries), " entries have a readable date; the dates may be written differently now")
  }

  facts <- wikipedia_article_facts(unique(unlist(map(entries$links, "title"))))
  entries |>
    mutate(links = map(links, \(l) left_join(l, distinct(facts, title, qid, sitelinks, classes), by = "title"))) |>
    transmute(group = spec$group, page, section, date_text, t0, t1, open_end, text, links)
}

for (id in names(RELIGION_TIMELINES)) {
  local({
    spec <- RELIGION_TIMELINES[[id]]
    first_page <- spec$pages[[1]]
    several <- length(spec$pages) > 1
    register_source(new_source(
      id = paste0("religion_", id),
      title = paste0(spec$group, ": ", first_page, if (several) " (and the other centuries)" else ""),
      url = paste0("https://en.wikipedia.org/wiki/", str_replace_all(first_page, " ", "_")),
      citation = paste0(
        "Wikipedia contributors, '", first_page, "'", if (several) " and the articles for the other centuries", ", ",
        "Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0. Entries chosen by how many language editions ",
        "their subject has on Wikipedia (Wikidata)."
      ),
      fetch = \() fetch_religion_timeline(spec),
      max_age_days = 365,
      min_rows = 20
    ))
  })
}


# ---- the shared accounts ---------------------------------------------------------------------

# The Masoretic table in "Chronology of the Bible": year of the world, the BCE year it works out as
# (written "AM 1948 (2216 BCE)"), the event and a note.
fetch_religion_common <- function() {
  doc <- wikipedia_html("Chronology of the Bible")
  table <- NULL
  for (candidate in xml2::xml_find_all(doc, "//table[contains(@class, 'wikitable')]")) {
    cells <- rvest::html_table(candidate, fill = TRUE, header = FALSE)
    if (nrow(cells) > 0 && any(str_detect(unlist(cells[1, ]), regex("^event$", ignore_case = TRUE)))) {
      table <- cells
      break
    }
  }
  if (is.null(table)) stop("the table of Masoretic dates (with an 'Event' column) was not found")

  events <- tibble(date = clean_text(table[[1]]), event = clean_text(table[[2]]), note = clean_text(table[[3]])) |>
    slice(-1) |>
    mutate(
      parts = str_match(date, "AM\\s*([0-9,]+)\\s*\\(\\s*([0-9,]+)\\s*BC"),
      anno_mundi = as.numeric(str_remove_all(parts[, 2], ",")),
      bce = as.numeric(str_remove_all(parts[, 3], ","))
    ) |>
    filter(!is.na(bce)) |>
    select(event, anno_mundi, bce, note)

  if (nrow(events) < 6) stop("only ", nrow(events), " dated events found in the Masoretic table (expected 6 or more)")
  if (is.unsorted(events$anno_mundi) || any(events$bce > 5000 | events$bce < 100)) {
    stop("the Masoretic dates are out of order or implausible; the table may have changed")
  }
  events
}

register_source(new_source(
  id = "religion_common",
  title = "Shared Abrahamic accounts: Chronology of the Bible (Masoretic Text)",
  url = "https://en.wikipedia.org/wiki/Chronology_of_the_Bible",
  citation = "Wikipedia contributors, 'Chronology of the Bible', Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0. Dates are those of the Masoretic Text chronology; other chronologies (Septuagint, Samaritan, Seder Olam, Ussher) differ by centuries.",
  fetch = fetch_religion_common,
  max_age_days = 365,
  min_rows = 6
))


# ---- hand-written additions and exclusions ---------------------------------------------------

register_source(new_source(
  id = "religious_events",
  title = "Religious events added by hand (curated/religious_events.csv)",
  url = "curated/religious_events.csv",
  citation = "Hand-curated in curated/religious_events.csv; each row names its source. Dates are those of the traditions' own accounts or of scholars' estimates, not established history.",
  fetch = \() read_curated("religious_events.csv"),
  local = TRUE,
  min_rows = 0
))

register_source(new_source(
  id = "religious_excluded",
  title = "Entries left out by hand (curated/religious_excluded.csv)",
  url = "curated/religious_excluded.csv",
  citation = "Hand-curated in curated/religious_excluded.csv.",
  fetch = \() read_curated("religious_excluded.csv"),
  local = TRUE,
  min_rows = 0
))


# ---- choosing the major religious entries ----------------------------------------------------
# The timelines mention places and people ("First Christians reported in Austria") as often as they
# name the event itself, so an entry's SUBJECT is chosen by what Wikidata says its links are:
#   - the first linked article that is an event (a council, battle, schism, treaty, persecution...),
#   - failing that, the first that is a religious institution, text or building;
# never a person, place, people, language, religion, award or the like. The subject's number of
# language editions says how notable it is. Then, so that a general history is not mistaken for a
# religious one, the entry must use the vocabulary of religion.

EVENT_CLASSES <- paste0(
  "\\b(?:wars?|battle|siege|councils?|synod|schism|controvers\\w*|crusade|conquest|revolt|rebellion|revolution|",
  "uprising|massacre|persecution|pogrom|expulsion|treaty|peace|edict|decree|concordat|reformation|disputation|",
  "excommunication|inquisition|migration|emigration|pilgrimage|exodus|captivity|destruction|invasion|coup|",
  "genocide|famine|pandemic|plague|earthquake|historical period|incident|attack|execution|trial|election|",
  "conclave|coronation|assassination|declaration|agreement|conference|congress|apparition|revelation|",
  "miracle|conversion|martyrdom|occupation|raid|riot|conflict|reforms?)\\b"
)
RELIGIOUS_THING_CLASSES <- paste0(
  "\\b(?:religious|church|denomination|sect|cathedral|basilica|abbey|monastery|temple|synagogue|mosque|shrine|",
  "scripture|sacred|creed|confession of faith|canon|liturgy|sacrament|doctrine|dogma|feast|catholic|christian|",
  "jewish|islamic|muslim|orthodox|protestant|book of the bible)\\b"
)
NOT_SUBJECT_CLASSES <- paste0(
  "\\b(?:human|country|state|city|town|settlement|municipality|region|province|island|river|mountain|",
  "ethnic group|ethnoreligious|language|calendar|religion|dynasty|empire|kingdom|continent|territory|village|",
  "colony|republic|fictional|given name|family|species|award|prize|sports?|team|club|company|business|",
  "university|school|museum|newspaper|occupation|profession|position|title|honorific|rank|group)\\b"
)
# Words that make an entry religious. The names of the traditions themselves ("Jews", "Muslim") are
# left out on purpose: every entry of a Jewish or Islamic timeline has them.
RELIGIOUS_TERMS <- paste0(
  "\\b(?:church\\w*|catholic\\w*|orthodox\\w*|protestant\\w*|christendom|papal|pope|popes|bishop\\w*|archbishop\\w*|",
  "patriarch\\w*|cardinal\\w*|clergy|priest\\w*|monk\\w*|monastic|monasteries|monastery|abbey|convent|councils?|synod\\w*|",
  "schism\\w*|heres\\w+|heretic\\w*|excommunicat\\w*|crusad\\w*|inquisition|reformation|jesuit\\w*|franciscan\\w*|",
  "dominican\\w*|benedictine\\w*|gospel\\w*|bible\\w*|biblical|scripture\\w*|testament|apostle\\w*|disciple\\w*|",
  "saints?|martyr\\w*|baptis\\w+|baptist\\w*|theolog\\w*|creed\\w*|doctrine\\w*|dogma\\w*|canon\\w*|liturgy|",
  "sacrament\\w*|missionar\\w*|conversion|converted|convert|converts|temple\\w*|synagogue\\w*|torah|talmud\\w*|",
  "mishnah|midrash|rabbi\\w*|kabbal\\w*|zohar|hasid\\w*|halakh\\w*|haskalah|judaism|messiah\\w*|messianic|sabbath|",
  "passover|sanhedrin|pharisee\\w*|karaite\\w*|mosques?|islam|caliph\\w*|imams?|qur'?an\\w*|koran|hadith|",
  "sunni\\w*|shi'?a\\w*|shi'?ite\\w*|sufi\\w*|hajj|mecca|medina|hijra|prophets?|muhammad|mohammed|sharia|jihad\\w*|",
  "mahdi|khariji\\w*|ulema|madrasa\\w*|fatwa|ramadan|pilgrim\\w*|sects?|denomination\\w*|faith|religio\\w*|holy|",
  "sacred|worship\\w*|prayer\\w*|revelation|miracle\\w*|apparition\\w*|persecution\\w*|pogrom\\w*|reconquista|",
  "toleration|inquisitor\\w*|ecumenical|anathema|iconoclas\\w*|schisms?)\\b"
)

# For each entry (a row of `entries`: `text` and `links`, as fetch_religion_timeline() leaves them)
# its subject: tibble(entry, subject, qid, sitelinks, classes, opens_entry); the entries with no
# subject are not in it. `qid` is the subject's Wikidata item (two links that are redirects of one
# article share it) and `opens_entry` says the entry begins with the subject's name.
timeline_subjects <- function(entries) {
  without_article <- \(x) str_remove(str_to_lower(x), "^(?:the|a|an)\\s+")
  tibble(entry = seq_len(nrow(entries)), text = entries$text, links = entries$links) |>
    unnest(links) |>
    mutate(
      opens_entry = coalesce(startsWith(without_article(text), without_article(anchor)), FALSE),
      kind = case_when(
        is.na(classes) | classes == "" ~ "unknown",
        str_detect(classes, NOT_SUBJECT_CLASSES) ~ "excluded",
        str_detect(classes, EVENT_CLASSES) ~ "event",
        str_detect(classes, RELIGIOUS_THING_CLASSES) ~ "religious",
        .default = "other"
      )
    ) |>
    filter(kind %in% c("event", "religious")) |>
    arrange(entry, kind != "event") |>
    slice_head(n = 1, by = entry) |>
    select(entry, subject = title, qid, sitelinks, classes, opens_entry)
}

# Time bins, so the picks spread across history instead of crowding into the modern era, which
# has the best-known articles. Finer where the early history of Islam and Christianity is packed.
RELIGION_ERA_BREAKS <- c(-Inf, 0, 300, 600, 650, 800, 1000, 1200, 1400, 1600, 1800, 1900, 2000, Inf)
RELIGION_PER_ERA <- 6L
RELIGION_MIN_SITELINKS <- 20L  # language editions: a well-known event, not a footnote

# Choose the major entries of one tradition's timeline:
#   - the entry has a subject (above) with enough language editions, and is about religion;
#   - one entry per subject, preferring the entry that names it (opens with it; the shorter, the
#     more of a headline), then the earliest;
#   - at most RELIGION_PER_ERA per era, the best-known first;
#   - nothing in `excluded` (curated/religious_excluded.csv: columns subject, and optionally the
#     group, such as Christianity, that the exclusion is for; blank means every group).
select_major_entries <- function(entries, excluded = tibble(subject = character(), group = character())) {
  if (nrow(entries) == 0) return(tibble())
  entries |>
    mutate(entry = row_number()) |>
    inner_join(timeline_subjects(entries), by = "entry") |>
    filter(
      sitelinks >= RELIGION_MIN_SITELINKS,
      str_detect(str_to_lower(paste(text, subject, classes)), RELIGIOUS_TERMS)
    ) |>
    anti_join(excluded, by = c("subject", "group")) |>
    anti_join(filter(excluded, is.na(group)), by = "subject") |>
    arrange(desc(opens_entry), nchar(text), t0) |>
    distinct(qid, .keep_all = TRUE) |>
    mutate(era = cut(t0, RELIGION_ERA_BREAKS)) |>
    slice_max(sitelinks, n = RELIGION_PER_ERA, by = era, with_ties = FALSE) |>
    arrange(t0)
}


# ---- the panel --------------------------------------------------------------------------------

register_metric(
  "religious_events",
  needs = character(),
  optional = c(paste0("religion_", names(RELIGION_TIMELINES)), "religion_common", "religious_events", "religious_excluded"),
  build = function(src) {
    today <- today_year()
    excluded <- tibble(subject = character(), group = character())
    if (!is.null(src$religious_excluded) && nrow(src$religious_excluded) > 0) {
      listed <- src$religious_excluded
      excluded <- tibble(
        subject = as.character(listed$subject),
        group = if ("group" %in% names(listed)) as.character(listed$group) else NA_character_
      )
    }

    # Entries chosen from each Wikipedia timeline.
    chosen <- map(paste0("religion_", names(RELIGION_TIMELINES)), \(id) src[[id]]) |>
      compact() |>
      map(\(entries) select_major_entries(entries, excluded)) |>
      list_rbind()
    from_timelines <- if (nrow(chosen) == 0) NULL else chosen |>
      mutate(title = timeline_title(text, subject)) |>
      transmute(
        group, t0, t1 = if_else(open_end, today, t1), title,
        about = if_else(str_to_lower(subject) == str_to_lower(title), NA_character_, subject),
        # (nothing to add when the entry is just its title)
        detail = if_else(
          str_to_lower(str_remove(text, "[.\\s]+$")) == str_to_lower(title), NA_character_, wrap_html(shorten(text, 330))
        )
      )

    # The shared accounts (the Masoretic table).
    common <- if (is.null(src$religion_common)) NULL else src$religion_common |>
      transmute(
        group = "Common", t0 = 1 - bce, t1 = 1 - bce, title = event, about = NA_character_,
        detail = wrap_html(paste0(
          "Year ", format(anno_mundi, big.mark = ","), " of the world in the Masoretic chronology. ", shorten(note, 250)
        ))
      )

    # Added by hand.
    added <- if (is.null(src$religious_events) || nrow(src$religious_events) == 0) NULL else src$religious_events |>
      mutate(
        t0 = calendar_to_year(start_year, start_era),
        t1 = coalesce(calendar_to_year(end_year, coalesce(end_era, start_era)), t0),
        group = if_else(tradition %in% RELIGION_GROUPS$group, tradition, "Other")
      ) |>
      transmute(
        group, t0, t1, title = event, about = NA_character_,
        detail = wrap_html(paste(notes, if_else(is.na(source), "", paste0("Source: ", source))))
      )

    events <- bind_rows(common, from_timelines, added)
    if (is.null(events) || nrow(events) == 0) stop("no religious events could be loaded")

    events <- events |>
      mutate(
        t1 = pmax(t0, t1),
        # A year or less is a moment, drawn as a diamond.
        instant = (t1 - t0) <= 1,
        t1 = if_else(instant, t0, t1)
      ) |>
      arrange(match(group, RELIGION_GROUPS$group), t0)

    # One block of rows per religion, in the order above. Diamonds have no length, so give each a
    # little (two years) when packing, to keep the events of neighbouring years apart.
    present <- RELIGION_GROUPS |> filter(group %in% events$group)
    packed <- pack_by_group(events$t0, if_else(events$instant, events$t0 + 2, events$t1), events$group, present$group)
    colour_of <- set_names(present$colour, present$group)

    data <- events |>
      mutate(
        lane = packed$lane, fill = unname(colour_of[group]), text_colour = text_on(fill),
        label = shorten(title, 34),
        body = hover_lines(Tradition = str_replace(group, "^Common$", "Shared by all three"), About = about, Details = detail)
      ) |>
      select(t0, t1, lane, label, fill, text_colour, instant, title, body)

    new_metric(
      id = "religious_events", label = "Religious events (Abrahamic)", group = "religion", kind = "spans",
      data = data, lane_labels = coalesce(present$axis_label[match(packed$labels, present$group)], ""),
      legend = tibble(series = present$group, label = present$legend_label, colour = present$colour),
      default_on = FALSE,
      description = "Events of the Abrahamic religions, split by tradition: first the accounts all three share, then Christianity, Judaism and Islam. Chosen from Wikipedia's timelines as the best-known events about religion, so it is a selection, not a complete history. Kept apart from the secular record: the early dates are those of the traditions' own chronologies.",
      sources = intersect(
        c("religion_common", paste0("religion_", names(RELIGION_TIMELINES)), "religious_events", "religious_excluded"),
        names(src)[!map_lgl(src, is.null)]
      )
    )
  }
)
