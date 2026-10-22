# Assemble the selected metrics into one stacked plotly figure.
#
# Every panel gets its own x axis, linked to the first with `matches`, so a zoom or pan on any
# panel moves them all, and each panel carries its own tick labels. The y axes are independent,
# one per metric, and are refitted to the visible time window in the browser (timeline.js).

PANEL_GAP_PX <- 58    # above each panel: its title, plus the tick labels of the panel above
PLOT_TOP_PX <- 4
PLOT_BOTTOM_PX <- 58  # below the last panel: tick labels and the x axis title
PLOT_LEFT_PX <- 88

panel_height_px <- function(metric) {
  if (metric$kind != "spans") return(170)
  # Panels with many rows (one block per group) use slightly thinner rows to stay compact.
  row_px <- if (metric$n_lanes > 10) 20 else 24
  max(100, row_px * metric$n_lanes + 44)
}

# The height the plot needs; the UI sizes the container to match.
plot_height_px <- function(metrics) {
  sum(map_dbl(metrics, panel_height_px)) + PANEL_GAP_PX * length(metrics) +
    PLOT_TOP_PX + PLOT_BOTTOM_PX
}

# Plotly names the first axis "x"/"xaxis" and the rest "x2"/"xaxis2", ...
axis_id <- function(letter, i) if (i == 1) letter else paste0(letter, i)
axis_key <- function(letter, i) paste0(letter, "axis", if (i == 1) "" else i)

x_axis_spec <- function(i, n, x_range) {
  # The axis counts years elapsed since the Earth formed (so that events line up), but the labels
  # say how long ago that was. timeline.js recomputes them whenever the window changes.
  ticks <- year_ticks(x_range[1], x_range[2])
  spec <- list(
    anchor = axis_id("y", i), domain = c(0, 1), range = x_range, type = "linear",
    tickmode = "array", tickvals = ticks$values, ticktext = ticks$text,
    showgrid = TRUE, gridcolor = "#e9ecef", zeroline = FALSE,
    showline = TRUE, linecolor = "#ced4da",
    ticks = "outside", ticklen = 4, tickcolor = "#ced4da",
    tickfont = list(size = 10, color = "#6c757d")
  )
  if (i > 1) spec$matches <- "x"
  if (i == n) {
    spec$title <- list(
      text = "Years ago (k = thousand, m = million, b = billion)",
      font = list(size = 11, color = "#495057"), standoff = 8
    )
  }
  spec
}

y_axis_spec <- function(metric, i, domain) {
  spec <- list(
    anchor = axis_id("x", i), domain = domain, fixedrange = TRUE,
    tickfont = list(size = 10, color = "#6c757d"), ticks = "outside", ticklen = 3,
    tickcolor = "#ced4da", gridcolor = "#e9ecef", zeroline = FALSE,
    exponentformat = "none", separatethousands = TRUE
  )

  if (metric$kind == "spans") {
    # Lanes are rows numbered from 0; reversing the range puts lane 0 at the top.
    spec$range <- c(metric$n_lanes - 0.5, -0.5)
    spec$showgrid <- FALSE
    spec$ticks <- ""
    if (is.null(metric$lane_labels)) {
      spec$showticklabels <- FALSE
    } else {
      spec$tickvals <- seq_along(metric$lane_labels) - 1
      spec$ticktext <- metric$lane_labels
    }
    return(spec)
  }

  spec$type <- metric$y_type
  spec$title <- list(
    text = metric$y_title, font = list(size = 11, color = "#495057"), standoff = 6
  )
  spec$zeroline <- metric$y_zeroline
  spec$zerolinecolor <- "#adb5bd"

  if (metric$y_hidden) {
    # A single row of events: a y axis would mean nothing.
    spec$range <- c(-1, 1)
    spec$showticklabels <- FALSE
    spec$showgrid <- FALSE
    spec$ticks <- ""
    spec$title <- NULL
  } else {
    # Start from plotly's own fit; timeline.js takes over once the plot is drawn.
    spec$autorange <- if (metric$y_reversed) "reversed" else TRUE
  }
  spec
}

# plotly copies every single-value style setting (hoverinfo, bar width, a trace's hover template,
# ...) into one entry per data point. For 8,000 points and a 110-character hover template that
# is nearly a megabyte of the same string sent to the browser. Put them back to single values.
# Only style settings are touched: a data column that happens to be constant (the y of a single
# row of events) must stay an array.
collapse_repeats <- function(widget) {
  is_repeated <- \(x) is.atomic(x) && length(x) > 1 && length(unique(x)) == 1
  collapse <- \(x) if (is_repeated(x)) x[[1]] else x

  widget$x$data <- map(widget$x$data, \(trace) {
    for (name in intersect(c("hovertemplate", "hoverinfo", "width", "textposition", "insidetextanchor"), names(trace))) {
      trace[[name]] <- collapse(trace[[name]])
    }
    for (parent in intersect(c("marker", "textfont"), names(trace))) {
      for (name in intersect(c("color", "size"), names(trace[[parent]]))) {
        trace[[parent]][[name]] <- collapse(trace[[parent]][[name]])
      }
    }
    trace
  })
  widget
}

# Panel heading: the metric's name, then a coloured key when it draws more than one series.
panel_title <- function(metric) {
  key <- ""
  if (!is.null(metric$legend) && nrow(metric$legend) > 1) {
    key <- paste0(
      "   ",
      paste0(
        "<span style=\"color:", metric$legend$colour, "\">●</span> ",
        "<span style=\"color:#6c757d;font-size:11px\">", metric$legend$label, "</span>",
        collapse = "   "
      )
    )
  }
  paste0("<b>", metric$label, "</b>", key)
}

# metrics: the selected metrics, top to bottom. x_range: c(from, to) in elapsed years.
# widget_js: the text of timeline.js, run in the browser once the plot is drawn.
build_timeline <- function(metrics, x_range, widget_js) {
  today <- today_year()  # "years ago" is counted back from the day the plot is drawn
  n <- length(metrics)
  heights <- map_dbl(metrics, panel_height_px)
  plot_area <- sum(heights) + PANEL_GAP_PX * n

  # Panel positions as fractions of the plot area (paper coordinates run bottom-to-top).
  top_px <- PANEL_GAP_PX * seq_len(n) + c(0, cumsum(heights)[-n])
  y_top <- 1 - top_px / plot_area
  y_bottom <- 1 - (top_px + heights) / plot_area

  p <- plot_ly()
  for (i in seq_len(n)) {
    p <- add_metric_traces(p, metrics[[i]], axis_id("x", i), axis_id("y", i), today)
  }

  axes <- list()
  for (i in seq_len(n)) {
    axes[[axis_key("x", i)]] <- x_axis_spec(i, n, x_range)
    axes[[axis_key("y", i)]] <- y_axis_spec(metrics[[i]], i, c(y_bottom[i], y_top[i]))
  }

  titles <- map(seq_len(n), \(i) list(
    xref = "paper", yref = "paper", x = 0, y = y_top[i], xanchor = "left", yanchor = "bottom",
    yshift = 8, showarrow = FALSE, align = "left", text = panel_title(metrics[[i]]),
    font = list(size = 13, color = "#212529")
  ))
  # Shown by timeline.js when the visible window holds none of a panel's data.
  notes <- map(seq_len(n), \(i) list(
    xref = "paper", yref = "paper", x = 0.5, y = (y_top[i] + y_bottom[i]) / 2,
    xanchor = "center", yanchor = "middle", showarrow = FALSE, visible = FALSE,
    text = "No data in this time range", font = list(size = 12, color = "#868e96")
  ))

  # "warmer" / "cooler" at the ends of a temperature-like axis, in the colours of the points.
  end_labels <- compact(map(seq_len(n), \(i) {
    metric <- metrics[[i]]
    if (is.null(metric$y_top_label)) return(NULL)
    label <- function(text, colour, y, anchor) list(
      xref = "paper", yref = "paper", x = 0.004, y = y, xanchor = "left", yanchor = anchor,
      showarrow = FALSE, bgcolor = "rgba(248,249,250,0.75)", borderpad = 2,
      text = text, font = list(size = 10.5, color = colour)
    )
    list(
      label(paste0("<b>↑ ", metric$y_top_label, "</b>"), "#b2182b", y_top[i], "top"),
      label(paste0("<b>↓ ", metric$y_bottom_label, "</b>"), "#2166ac", y_bottom[i], "bottom")
    )
  })) |>
    list_flatten()

  # What timeline.js needs to know about each panel (positions are on the elapsed-years axis).
  panels <- map(seq_len(n), \(i) list(
    yId = axis_id("y", i), yKey = axis_key("y", i), xKey = axis_key("x", i),
    fit = metrics[[i]]$kind != "spans" && !metrics[[i]]$y_hidden,
    reversed = isTRUE(metrics[[i]]$y_reversed),
    dataMin = year_to_elapsed(metrics[[i]]$t_min, today),
    dataMax = year_to_elapsed(metrics[[i]]$t_max, today),
    note = n + i - 1  # index of this panel's "no data" annotation
  ))

  p <- do.call(
    layout,
    c(
      list(p), axes,
      list(
        annotations = c(titles, notes, end_labels),
        # `token` lets timeline.js tell this figure's layout apart from a previous one's.
        meta = list(
          token = paste(sample(c(letters, 0:9), 10, replace = TRUE), collapse = ""),
          earthAge = EARTH_AGE, panels = panels
        ),
        margin = list(l = PLOT_LEFT_PX, r = 20, t = PLOT_TOP_PX, b = PLOT_BOTTOM_PX),
        autosize = TRUE, showlegend = FALSE, dragmode = "zoom", hovermode = "closest",
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#f8f9fa",
        font = list(family = "system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif", size = 11),
        hoverlabel = list(
          align = "left", bgcolor = "white", bordercolor = "#adb5bd",
          font = list(size = 12, color = "#212529")
        )
      )
    )
  )

  plotly_build(p) |>
    collapse_repeats() |>
    config(
      displaylogo = FALSE, responsive = TRUE, scrollZoom = FALSE, doubleClick = "reset",
      modeBarButtonsToRemove = c(
        "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian",
        "hoverCompareCartesian", "toggleSpikelines"
      ),
      toImageButtonOptions = list(format = "png", filename = "climate-timeline", scale = 2)
    ) |>
    htmlwidgets::onRender(widget_js) |>
    drop_build_leftovers()
}

# plotly keeps a complete second copy of every trace's data in `x$attrs` (so the figure can be
# modified later in R) and sends it to the browser along with the real traces: about half the
# payload, all of it unused there.
drop_build_leftovers <- function(widget) {
  widget$x$attrs <- NULL
  widget$x$visdat <- NULL
  widget$x$cur_data <- NULL
  widget
}
