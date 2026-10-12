# The geological time scale: eons, eras, periods, epochs and ages, from Macrostrat's copy of the
# International Commission on Stratigraphy (ICS) chart. Macrostrat serves each rank as its own
# "timescale" with the official ICS colours, so no parent/child bookkeeping is needed.

GEO_RANKS <- tibble(
  rank = c("Eon", "Era", "Period", "Epoch", "Age"),
  timescale = c(
    "international eons", "international eras", "international periods",
    "international epochs", "international ages"
  )
)

# Returns rank, name, start_ma (older end), end_ma (younger end), colour.
fetch_geo_timescale <- function() {
  pmap(GEO_RANKS, \(rank, timescale) {
    response <- http_get_text(
      "https://macrostrat.org/api/v2/defs/intervals",
      query = list(timescale = timescale, format = "json")
    )
    intervals <- jsonlite::fromJSON(response)$success$data
    if (is.null(intervals) || nrow(intervals) == 0) stop("Macrostrat returned no intervals for ", timescale)
    tibble(
      rank = rank, name = clean_text(intervals$name),
      start_ma = intervals$b_age, end_ma = intervals$t_age, colour = intervals$color
    )
  }) |>
    list_rbind()
}

register_source(new_source(
  id = "geo_timescale",
  title = "International Chronostratigraphic Chart (via Macrostrat)",
  url = "https://macrostrat.org/api/v2/defs/intervals",
  citation = "International Commission on Stratigraphy, International Chronostratigraphic Chart; served by Macrostrat (macrostrat.org), CC BY 4.0.",
  fetch = fetch_geo_timescale,
  max_age_days = 90,
  min_rows = 150
))

register_metric("geo_timescale", needs = "geo_timescale", build = function(src) {
  data <- src$geo_timescale |>
    mutate(lane = match(rank, GEO_RANKS$rank) - 1L, fill = to_hex(colour)) |>
    transmute(
      t0 = ma_to_year(start_ma), t1 = ma_to_year(end_ma), lane,
      label = shorten(name, 22), fill, text_colour = text_on(fill), instant = FALSE,
      title = name,
      body = hover_lines(Rank = rank, Duration = format_duration(t1 - t0))
    )

  new_metric(
    id = "geo_timescale", label = "Geological time scale", group = "earth", kind = "spans",
    data = data, lane_labels = GEO_RANKS$rank, default_on = TRUE,
    description = "Eons, eras, periods, epochs and ages of Earth history, in the official colours of the International Chronostratigraphic Chart.",
    sources = "geo_timescale"
  )
})
