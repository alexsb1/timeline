# One function per metric kind. Each draws a metric into one panel of the shared plotly figure.
#
# All panels live in a single plotly object (not subplot()), each on its own pair of axes
# (x, y), (x2, y2), ... so the layout code can place and link them exactly.
#
# The data holds calendar years (`t`). Here they become positions on the x-axis (years elapsed
# since the Earth formed) using today's date, and tooltips are written with "years ago" counted
# from today.

# A calendar year as a position on the x-axis, rounded to the precision the data really has: whole
# years for anything older than 10,000 years, tenths up to 1,000 years, then hundredths (about
# four days, enough for monthly series). Full double precision would add ~6 bytes per point.
to_x <- function(t, today) {
  years_ago <- today - t
  digits <- dplyr::case_when(years_ago >= 1e4 ~ 0, years_ago >= 1e3 ~ 1, .default = 2)
  round(year_to_elapsed(t, today), digits)
}

# "#0072B2" -> "rgba(0,114,178,0.18)"
with_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%s)", rgb[1, ], rgb[2, ], rgb[3, ], alpha)
}

# Blue (cold) through pale yellow to red (hot), a ColorBrewer RdYlBu ramp.
HEAT_COLOURSCALE <- list(
  list(0, "#4575b4"), list(0.25, "#91bfdb"), list(0.5, "#fee090"),
  list(0.75, "#fc8d59"), list(1, "#d73027")
)

# The values that map to the two ends of the colour ramp, ignoring the extreme 2% each side.
heat_limits <- function(values, mode) {
  limits <- stats::quantile(values, c(0.02, 0.98), names = FALSE)
  if (mode == "zero_centred") c(-1, 1) * max(abs(limits)) else limits
}

# A continuous series through time. Optional ymin/ymax draw an uncertainty band beneath. A metric
# with `heat` colours each point from blue (cold) to red (warm).
add_line_traces <- function(p, metric, x_axis, y_axis, today) {
  legend <- metric$legend

  for (i in seq_len(nrow(legend))) {
    series <- metric$data[metric$data$series == legend$series[i], ]
    x <- to_x(series$t, today)
    colour <- legend$colour[i]

    if ("ymin" %in% names(series) && !anyNA(series$ymin) && !anyNA(series$ymax)) {
      p <- add_trace(
        p, type = "scatter", mode = "lines",
        x = c(x, rev(x)), y = c(series$ymax, rev(series$ymin)),
        fill = "toself", fillcolor = with_alpha(colour, 0.2), line = list(width = 0),
        hoverinfo = "skip", showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
      )
    }

    common <- list(
      x = x, y = series$y,
      # The x axis counts up from the Earth's formation, so tell people how long ago it was.
      customdata = format_when(series$t, today),
      hovertemplate = paste0(
        "<b>", legend$label[i], "</b><br>%{y:", metric$y_format, "} ", metric$y_unit,
        "<br>%{customdata}<extra></extra>"
      ),
      # Marks the main line so the browser-side y-fit also considers the points either side of
      # the visible window (otherwise a sparse series zoomed between two points would not fit).
      meta = list(role = "series"),
      showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )

    if (is.null(metric$heat)) {
      style <- list(type = "scatter", mode = "lines", line = list(color = colour, width = 1.6))
    } else {
      limits <- heat_limits(series$y, metric$heat)
      style <- list(
        type = "scatter", mode = "lines+markers",
        line = list(color = "rgba(110,110,110,0.35)", width = 0.8),
        marker = list(
          size = 5, color = signif(series$y, 2), colorscale = HEAT_COLOURSCALE,
          # For d18O, a LOW value is warm, so the ramp runs the other way.
          reversescale = metric$heat == "low_is_warm",
          cmin = limits[1], cmax = limits[2], showscale = FALSE, line = list(width = 0)
        )
      )
    }
    p <- do.call(add_trace, c(list(p), style, common))
  }
  p
}

# Horizontal bars between a start and an end, one row ("lane") per overlap level.
# Spans of a year or less are drawn as diamonds, since a bar that thin would be invisible.
add_span_traces <- function(p, metric, x_axis, y_axis, today) {
  data <- metric$data
  data$x0 <- pmax(to_x(data$t0, today), 0)
  data$x1 <- to_x(data$t1, today)
  data$hover <- hover_text(data$title, data$t0, data$t1, today, data$body)

  bars <- data[!data$instant, ]
  points <- data[data$instant, ]

  if (nrow(bars) > 0) {
    p <- add_trace(
      p, type = "bar", orientation = "h",
      x = bars$x1 - bars$x0, base = bars$x0, y = bars$lane, width = 0.84,
      marker = list(color = bars$fill, line = list(color = "rgba(0,0,0,0.3)", width = 0.5)),
      hovertext = bars$hover, hoverinfo = "text",
      showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )

    # Labels are a separate text layer, not the bars' own text: plotly centres a bar's text on
    # the whole bar, which is off-screen once you zoom inside a long span (an eon, a reign).
    # timeline.js moves each label to the middle of the part of its bar that is in view, and
    # shortens or hides it to fit. The bar ends and the full label text travel in `meta`
    # (the trace's own `text` is rewritten as the window changes).
    p <- add_trace(
      p, type = "scatter", mode = "text",
      x = (bars$x0 + bars$x1) / 2, y = bars$lane, text = bars$label,
      textposition = "middle center", textfont = list(color = bars$text_colour, size = 11),
      meta = list(role = "bar-labels", x0 = bars$x0, x1 = bars$x1, labels = bars$label),
      hoverinfo = "skip", showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )
  }

  if (nrow(points) > 0) {
    p <- add_trace(
      p, type = "scatter", mode = "markers",
      x = points$x0, y = points$lane,
      marker = list(
        symbol = "diamond", size = 11, color = points$fill,
        line = list(color = "rgba(0,0,0,0.6)", width = 1)
      ),
      hovertext = points$hover, hoverinfo = "text",
      showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )

    # A name beside each diamond. timeline.js shows it only where there is room before the next
    # marker in the same row, so names appear as you zoom in (the full names are in `meta`; the
    # trace's own `text` is rewritten as the window changes). (No `marker` here: even an invisible
    # one makes plotly draw default-coloured markers over the diamonds.)
    p <- add_trace(
      p, type = "scatter", mode = "text",
      x = points$x0, y = points$lane, text = points$label,
      textposition = "middle right", textfont = list(color = "#343a40", size = 10.5),
      meta = list(role = "point-labels", labels = points$label),
      hoverinfo = "skip", showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )
  }
  p
}

# A marker per event, at a height given by its magnitude. Events with a duration also get a
# line from start to end.
add_event_traces <- function(p, metric, x_axis, y_axis, today) {
  events <- metric$data
  events$x <- to_x(events$t, today)
  events$x0 <- to_x(events$t0, today)
  events$x1 <- to_x(events$t1, today)
  events$hover <- hover_text(events$title, events$t0, events$t1, today, events$body)

  # An event is "lasting" if it spans enough years to show as a line on any plausible zoom.
  lasting <- events[events$t1 - events$t0 > 1, ]

  if (nrow(lasting) > 0) {
    # `NA` rows break the line between events so each is drawn as its own segment.
    p <- add_trace(
      p, type = "scatter", mode = "lines",
      x = as.vector(rbind(lasting$x0, lasting$x1, NA)),
      y = as.vector(rbind(lasting$y, lasting$y, NA)),
      line = list(color = "rgba(80,80,80,0.55)", width = 3),
      hoverinfo = "skip", showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
    )
  }

  add_trace(
    p, type = "scatter", mode = "markers",
    x = events$x, y = events$y,
    marker = list(
      size = events$size, color = events$colour, opacity = 0.85,
      line = list(color = "white", width = 0.8)
    ),
    hovertext = events$hover, hoverinfo = "text",
    showlegend = FALSE, xaxis = x_axis, yaxis = y_axis
  )
}

add_metric_traces <- function(p, metric, x_axis, y_axis, today) {
  switch(
    metric$kind,
    line = add_line_traces(p, metric, x_axis, y_axis, today),
    spans = add_span_traces(p, metric, x_axis, y_axis, today),
    events = add_event_traces(p, metric, x_axis, y_axis, today)
  )
}
