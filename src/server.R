# The server logic. Defines `server`.
# Needs `catalog`, `metrics`, `timeline` and `timeline_js` from app.R.

server <- function(input, output, session) {
  # ---- which metrics are on ------------------------------------------------

  # Panels always stack in catalog order, however the boxes were ticked.
  selected_ids <- reactive({
    ticked <- unlist(map(timeline$groups$id, \(group) input[[paste0("sel_", group)]]))
    catalog$id[catalog$id %in% ticked]
  })

  observeEvent(input$show_all, {
    walk(timeline$groups$id, \(group) {
      updateCheckboxGroupInput(session, paste0("sel_", group), selected = catalog$id[catalog$group == group])
    })
  })
  observeEvent(input$hide_all, {
    walk(timeline$groups$id, \(group) {
      updateCheckboxGroupInput(session, paste0("sel_", group), selected = character())
    })
  })

  # ---- the visible time window ---------------------------------------------

  # timeline.js reports the window whenever it changes; before that, it is the whole history.
  current_range <- reactive({
    if (is.null(input$x_range)) zoom_range(EARTH_AGE) else input$x_range
  })

  output$range_readout <- renderUI({
    # The window can overhang either end slightly, so clamp it before describing it.
    window <- pmin(pmax(current_range(), 0), EARTH_AGE)
    readout <- \(label, value) div(tags$span(class = "range-label", label), tags$strong(value))
    years_ago <- \(elapsed) format_years_ago(EARTH_AGE - elapsed)

    div(
      class = "d-flex flex-wrap column-gap-4 row-gap-1",
      readout("Time", paste(years_ago(window[1]), "→", years_ago(window[2]))),
      readout("Spanning", format_duration(diff(window))),
      readout("Years elapsed since the Earth formed", paste(format_count(window[1]), "→", format_count(window[2])))
    )
  })

  # Jump to a preset window. Setting the range from R (rather than redrawing) keeps the
  # panels in place; timeline.js sees the change and refits the y axes.
  pwalk(zoom_presets, \(id, years_ago, ...) {
    observeEvent(input[[id]], {
      range <- zoom_range(years_ago)
      panel_count <- length(selected_ids())
      req(panel_count > 0)

      # Each panel has its own (linked) x axis, so set them all explicitly.
      update <- rep(list(range), panel_count)
      names(update) <- paste0("xaxis", c("", seq_len(panel_count)[-1]), ".range")
      plotlyProxy("timeline", session) |> plotlyProxyInvoke("relayout", update)
    })
  })

  # ---- the plot ------------------------------------------------------------

  output$plot_area <- renderUI({
    ids <- selected_ids()
    if (length(ids) == 0) {
      return(div(class = "empty-state", "Tick at least one metric in the sidebar to draw the timeline."))
    }
    plotlyOutput("timeline", height = paste0(plot_height_px(metrics[ids]), "px"))
  })

  output$timeline <- renderPlotly({
    ids <- selected_ids()
    req(length(ids) > 0)
    # Redraw in the window the user is looking at; reading it must not itself trigger a redraw.
    build_timeline(metrics[ids], isolate(current_range()), timeline_js)
  })
}
