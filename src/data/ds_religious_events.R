# Religious events, as a layer of their own. The rest of the timeline is the secular record:
# natural science and documented history. What religious traditions say happened, and when
# they date it, is not in the same category, so it is never mixed into those panels.
#
# There is no published dataset of this, so it is a hand-written list: curated/religious_events.csv.
# Add a row to plot another event on the same time axis. Columns:
#   event, tradition, start_year, start_era (CE or BCE), end_year, end_era (both blank for a
#   single moment), notes, source (the text or chronology the date comes from).

register_source(new_source(
  id = "religious_events",
  title = "Religious events (hand-curated list)",
  url = "curated/religious_events.csv",
  citation = "Hand-curated in curated/religious_events.csv. Dates are those of the religious traditions' own accounts, not established history.",
  fetch = \() read_curated("religious_events.csv"),
  local = TRUE,
  min_rows = 1
))

register_metric("religious_events", needs = "religious_events", build = function(src) {
  data <- src$religious_events |>
    mutate(
      t0 = calendar_to_year(start_year, start_era),
      t1 = coalesce(calendar_to_year(end_year, coalesce(end_era, start_era)), t0)
    ) |>
    transmute(
      t = (t0 + t1) / 2, t0, t1, y = 0, size = 11, colour = "#8E5EA2",
      title = event,
      body = hover_lines(Tradition = tradition, Notes = wrap_html(notes), Source = wrap_html(source))
    ) |>
    arrange(t)

  new_metric(
    id = "religious_events", label = "Religious events", group = "religion", kind = "events",
    data = data, y_title = "Events", y_hidden = TRUE, default_on = FALSE,
    description = "Events as told by religious traditions, dated by those traditions' own chronologies. Kept apart from the secular record: these are accounts, not established history.",
    sources = "religious_events"
  )
})
