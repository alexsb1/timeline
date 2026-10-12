# Supercontinents and when they existed, from the table in Wikipedia's "Supercontinent" article
# (CC BY-SA 4.0). The ages are millions of years ago, written "3,636-2,803" (older first), and the
# list includes several that are "not generally regarded" as supercontinents, which the comment
# column says and the tooltip repeats.

SUPERCONTINENT_PAGE <- "Supercontinent"

fetch_supercontinents <- function() {
  tables <- wikipedia_tables(SUPERCONTINENT_PAGE)

  # Find the table by its columns rather than its position in the article.
  wanted <- c(name = "^Supercontinent", age = "^Age", range = "Period|Era", comment = "^Comment")
  is_ours <- \(table) all(map_lgl(wanted[c("name", "age")], \(pattern) any(str_detect(names(table), pattern))))
  table <- keep(tables, is_ours) |> head(1)
  if (length(table) == 0) stop("the supercontinent table (columns 'Supercontinent name' and 'Age (Ma)') was not found; the article layout has changed")
  table <- table[[1]]
  column <- \(key) names(table)[str_detect(names(table), wanted[[key]])][1]

  dashes <- paste0("[", intToUtf8(c(0x2010, 0x2011, 0x2012, 0x2013, 0x2014, 0x2212)), "]")
  ages <- table[[column("age")]] |>
    clean_text() |>
    str_replace_all(dashes, "-") |>
    str_match("^([0-9][0-9,.]*)\\s*-\\s*([0-9][0-9,.]*)$")

  found <- tibble(
    name = clean_text(table[[column("name")]]),
    age_a = as_number(ages[, 2]),
    age_b = as_number(ages[, 3]),
    period_range = if (is.na(column("range"))) NA_character_ else clean_text(table[[column("range")]]),
    comment = if (is.na(column("comment"))) NA_character_ else clean_text(table[[column("comment")]])
  )

  unreadable <- filter(found, is.na(name) | is.na(age_a) | is.na(age_b))
  if (nrow(unreadable) > 0) {
    stop("could not read the age range of: ", paste(coalesce(unreadable$name, "(unnamed)"), collapse = ", "),
         "; expected 'start-end' in millions of years")
  }

  supercontinents <- found |>
    # Each range is written oldest first, but order the pair rather than trust it.
    mutate(start_ma = pmax(age_a, age_b), end_ma = pmin(age_a, age_b)) |>
    select(name, start_ma, end_ma, period_range, comment)

  # An age in millions of years must lie within the Earth's lifetime, and the youngest
  # supercontinent (Pangaea) must be younger than the oldest of them all by a wide margin.
  if (any(supercontinents$start_ma > EARTH_AGE / 1e6 | supercontinents$end_ma < 0)) {
    stop("a supercontinent age falls outside the Earth's lifetime; the table units may have changed")
  }
  if (!"Pangaea" %in% supercontinents$name || min(supercontinents$end_ma) > 300) {
    stop("the supercontinent table no longer includes Pangaea (youngest, under 300 Ma); the layout may have changed")
  }
  supercontinents
}

register_source(new_source(
  id = "supercontinents",
  title = "Supercontinent",
  url = "https://en.wikipedia.org/wiki/Supercontinent",
  citation = "Wikipedia contributors, 'Supercontinent', Wikipedia, The Free Encyclopedia. Text available under CC BY-SA 4.0.",
  fetch = fetch_supercontinents,
  max_age_days = 30,
  min_rows = 8
))

register_metric("supercontinents", needs = "supercontinents", build = function(src) {
  fill <- bar_palette(nrow(src$supercontinents))

  data <- src$supercontinents |>
    mutate(
      t0 = ma_to_year(start_ma), t1 = ma_to_year(end_ma),
      lane = assign_lanes(t0, t1),
      fill = fill, text_colour = text_on(fill),
      label = name, title = name, instant = FALSE,
      body = hover_lines(
        Duration = format_duration((start_ma - end_ma) * 1e6),
        `Geological range` = str_replace_all(period_range, "-", intToUtf8(0x2013)),
        Comment = wrap_html(comment)
      )
    ) |>
    select(t0, t1, lane, label, fill, text_colour, instant, title, body)

  new_metric(
    id = "supercontinents", label = "Supercontinents", group = "earth", kind = "spans",
    data = data,
    description = "When the continents were gathered into a single landmass, from the possible first supercratons of the Archean to Pangaea. Some of these are not generally counted as supercontinents; the tooltips say which.",
    sources = "supercontinents"
  )
})
