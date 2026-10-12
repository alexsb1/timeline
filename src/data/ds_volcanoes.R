# The largest known volcanic eruptions and large igneous provinces, by erupted volume.
#
# Source: Wikipedia's "List of largest volcanic eruptions": three tables (explosive eruptions,
# effusive eruptions, large igneous provinces), each with an age in millions of years, a place, a
# volume and notes. No better structured source exists: the Smithsonian Global Volcanism Program
# database stops at the Holocene and gives VEI rather than volume, and Wikidata has no volumes.
# The article's volume column mixes plain numbers, ranges ("2,000–13,200") and lower limits
# (">2,800"), and the igneous-province table counts in MILLIONS of km3 (the earlier version of
# this panel plotted those as plain km3), so the parsing below is explicit about all three.

VOLCANO_PAGE <- "List of largest volcanic eruptions"

# The article's section headings -> our names for the three kinds, in the order they are shown.
VOLCANO_TYPES <- c(
  Explosive = "explosive", Effusive = "effusive", `Large igneous provinces` = "igneous"
)

# The colour of each type (Okabe-Ito, so the three are distinguishable in colour-blind vision).
VOLCANO_COLOURS <- c(
  Explosive = okabe_ito[["vermillion"]],
  Effusive = okabe_ito[["green"]],
  `Large igneous provinces` = okabe_ito[["purple"]]
)

# Numbers in a piece of text, ignoring thousands separators: "2,000–13,200" -> 2000, 13200.
volcano_numbers <- function(x) map(str_extract_all(str_remove_all(x, ","), "[0-9]+(?:\\.[0-9]+)?"), as.numeric)

# One representative number for a cell that may be a range. `geometric` takes the midpoint on a
# log scale, which is how volumes are plotted and suits quantities spanning an order of magnitude
# ("2,000-13,200" -> 5,100); ages, plotted on a linear axis, use the plain midpoint. A lower limit
# (">2,800") and "~15.5" are used as they stand.
volcano_midpoint <- function(text, geometric = FALSE) {
  map_dbl(volcano_numbers(text), function(n) {
    n <- head(n[!is.na(n)], 2)
    if (length(n) == 0) return(NA_real_)
    if (geometric && all(n > 0)) exp(mean(log(n))) else mean(n)
  })
}

# One of the article's tables, tagged with the kind of eruption it lists. The columns are the
# same in each (name, age, place, volume, notes, references); only the volume's unit differs.
parse_volcano_table <- function(table, type) {
  raw <- rvest::html_table(table, convert = FALSE)
  headings <- str_squish(names(raw))
  if (ncol(raw) < 5 || !str_detect(headings[2], "^Age") || !str_detect(headings[4], "^Volume") || !str_detect(headings[5], "^Notes")) {
    stop("volcano table columns changed: ", paste(headings, collapse = " | "), call. = FALSE)
  }
  # "Volume (millions of km3)" for the igneous provinces, "Volume (km3)" otherwise.
  millions <- str_detect(headings[4], regex("million", ignore_case = TRUE))
  volume_cell <- clean_text(raw[[4]])

  tibble(
    name = clean_text(raw[[1]]),
    type = type,
    age_text = clean_text(raw[[2]]),
    age_ma = volcano_midpoint(age_text),
    location = clean_text(raw[[3]]),
    volume_text = if_else(is.na(volume_cell), NA_character_, paste(volume_cell, if (millions) "million km³" else "km³")),
    volume_km3 = volcano_midpoint(volume_cell, geometric = TRUE) * if (millions) 1e6 else 1,
    notes = clean_text(raw[[5]])
  )
}

# Returns one row per eruption or province: name, type (as the article's section heading), the
# age as published (age_text) and in Ma (age_ma, the midpoint of a range), location, the volume
# as published with its unit (volume_text) and in km3 (volume_km3, the geometric midpoint of a
# range; a lower limit is taken as it stands), and notes.
fetch_volcanoes <- function() {
  doc <- wikipedia_html(VOLCANO_PAGE)
  tables <- xml2::xml_find_all(doc, "//table[contains(@class, 'wikitable')]")
  # Which section is each table under?
  sections <- map_chr(tables, \(table) {
    str_squish(xml2::xml_text(xml2::xml_find_first(table, "preceding::h2[1]")))
  })
  type_of <- \(section) {
    match <- names(VOLCANO_TYPES)[map_lgl(VOLCANO_TYPES, \(key) str_detect(section, regex(key, ignore_case = TRUE)))]
    if (length(match) == 1) match else NA_character_
  }
  types <- map_chr(sections, type_of)
  if (!setequal(na.omit(types), names(VOLCANO_TYPES))) {
    stop("expected volcano tables for ", paste(names(VOLCANO_TYPES), collapse = ", "), "; found sections: ",
         paste(sections, collapse = ", "))
  }

  volcanoes <- map2(tables[!is.na(types)], types[!is.na(types)], parse_volcano_table) |>
    list_rbind() |>
    filter(!is.na(name), !is.na(age_ma), !is.na(volume_km3))

  if (nrow(volcanoes) < 50) stop("only ", nrow(volcanoes), " volcanic eruptions found (expected about 70)")
  counts <- count(volcanoes, type)
  if (nrow(counts) < 3 || any(counts$n < 5)) stop("a volcano table has almost no rows: ", paste(counts$type, counts$n, collapse = ", "))
  if (any(volcanoes$age_ma < 0 | volcanoes$age_ma > 4600)) stop("an eruption has an impossible age")
  # An eruption is hundreds to tens of thousands of km3; a province is millions. A unit slip
  # (millions of km3 read as km3) would put a province far below this.
  eruptions <- filter(volcanoes, type != "Large igneous provinces")
  provinces <- filter(volcanoes, type == "Large igneous provinces")
  if (any(eruptions$volume_km3 < 100 | eruptions$volume_km3 > 1e5)) stop("an eruption volume is outside 100 to 100,000 km3")
  if (any(provinces$volume_km3 < 1e5 | provinces$volume_km3 > 2e8)) stop("a province volume is outside 0.1 to 200 million km3")
  # Two landmarks catch shifted columns: Toba and the Siberian Traps.
  toba <- filter(volcanoes, str_detect(name, "Toba"), age_ma < 0.2)
  siberia <- filter(provinces, str_detect(name, "Siberian"))
  if (nrow(toba) == 0 || nrow(siberia) != 1 || abs(siberia$age_ma - 250) > 10) {
    stop("Toba or the Siberian Traps are missing, or no longer in the expected place")
  }

  volcanoes
}

register_source(new_source(
  id = "volcanoes",
  title = "Largest volcanic eruptions (Wikipedia)",
  url = "https://en.wikipedia.org/wiki/List_of_largest_volcanic_eruptions",
  citation = "Wikipedia contributors, “List of largest volcanic eruptions”, Wikipedia, The Free Encyclopedia (CC BY-SA 4.0). Volumes and ages are compiled there from the primary literature, which is cited in the article.",
  fetch = fetch_volcanoes,
  max_age_days = 30,
  min_rows = 50L
))

register_metric("volcanoes", needs = "volcanoes", build = function(src) {
  data <- src$volcanoes |>
    mutate(
      t = ma_to_year(age_ma),
      type = factor(type, levels = names(VOLCANO_COLOURS))
    ) |>
    arrange(t) |>
    transmute(
      t, t0 = t, t1 = t, y = volume_km3,
      # Height already shows volume on a log axis, and the provinces are thousands of times larger
      # than the eruptions, so size follows the number of digits (log10) or every eruption would
      # be drawn at the minimum.
      size = marker_size(log10(volume_km3)),
      colour = unname(VOLCANO_COLOURS[as.character(type)]),
      title = name,
      body = hover_lines(
        Type = as.character(type),
        Location = wrap_html(location),
        Volume = htmltools::htmlEscape(volume_text),
        Notes = wrap_html(shorten(notes, 280))
      )
    )

  new_metric(
    id = "volcanoes", label = "Volcanic eruptions", group = "earth", kind = "events",
    data = data, y_title = "Erupted volume (km³)", y_type = "log", default_on = FALSE,
    legend = tibble(
      series = names(VOLCANO_COLOURS), label = names(VOLCANO_COLOURS), colour = unname(VOLCANO_COLOURS)
    ),
    description = "The largest known volcanic eruptions and large igneous provinces, by erupted volume. The flood-basalt provinces are thousands of times larger than any single eruption.",
    sources = "volcanoes"
  )
})
