# The page: a header, a sidebar of controls, and the plot. Defines `ui`.
# Needs `catalog`, `metrics` and `timeline` from app.R.

# One accordion section per group, each with a checkbox per metric. The tooltip on every
# checkbox is the metric's description.
metric_group_panel <- function(group_id, group_label) {
  in_group <- catalog[catalog$group == group_id, ]

  accordion_panel(
    title = group_label,
    value = group_id,
    checkboxGroupInput(
      inputId = paste0("sel_", group_id),
      label = NULL,
      choiceNames = map2(in_group$label, in_group$description, \(label, tip) tags$span(label, title = tip)),
      choiceValues = in_group$id,
      selected = in_group$id[in_group$default_on]
    )
  )
}

# How fresh a source's data is, in words.
retrieval_note <- function(status, fetched_at, note) {
  when <- if (is.na(fetched_at)) "never" else format(fetched_at, "%d %b %Y")
  switch(
    status,
    live = paste("Downloaded", when),
    cached = paste("Downloaded", when),
    deferred = paste0("Downloaded ", when, " (a newer download is due)"),
    stale = paste0("Showing the copy from ", when, " (the latest download failed)"),
    paste("Unavailable:", note)
  )
}

# Where each metric's data comes from: the source, how to cite it, and when it was downloaded.
sources_panel <- function() {
  entries <- map(metrics, \(metric) {
    sources <- timeline$sources[match(metric$sources, timeline$sources$id), ]
    items <- pmap(sources, \(title, url, citation, fetched_at, status, note, ...) {
      tags$li(
        if (str_detect(url, "^https?://")) tags$a(href = url, target = "_blank", rel = "noopener", title) else title,
        tags$br(), tags$span(class = "text-body-secondary", citation),
        tags$br(), tags$em(class = "text-body-secondary", retrieval_note(status, fetched_at, note))
      )
    })
    div(
      class = "mb-3", tags$strong(metric$label),
      tags$p(class = "small mb-1", metric$description),
      tags$ul(class = "small mb-0", items)
    )
  })

  unavailable <- timeline$problems
  if (length(unavailable) > 0) {
    entries <- c(entries, list(div(
      class = "mb-3", tags$strong("Not available right now"),
      tags$ul(
        class = "small mb-0",
        imap(unavailable, \(reason, id) tags$li(paste0(id, ": ", reason)))
      )
    )))
  }

  accordion_panel("Sources & notes", value = "sources", !!!unname(entries))
}

ui <- page_sidebar(
  title = div(
    class = "d-flex flex-wrap align-items-baseline column-gap-3",
    tags$span(class = "app-title", "Climate Timeline"),
    tags$span(class = "app-subtitle text-body-secondary", "Earth's climate and human history on one axis")
  ),
  theme = bs_theme(version = 5),
  fillable = FALSE,
  tags$head(includeCSS("www/styles.css")),

  sidebar = sidebar(
    width = 340,
    open = "desktop",

    div(
      tags$h6("Zoom to"),
      div(
        class = "d-grid gap-1 zoom-buttons",
        pmap(zoom_presets, \(id, label, ...) actionButton(id, label, class = "btn-sm btn-outline-secondary"))
      )
    ),

    div(
      div(
        class = "d-flex justify-content-between align-items-baseline",
        tags$h6("Metrics"),
        div(
          class = "small",
          actionLink("show_all", "all"), " · ", actionLink("hide_all", "none")
        )
      ),
      accordion(
        id = "metric_groups", multiple = TRUE,
        # The religion layer starts collapsed (and every metric in it starts off).
        open = setdiff(timeline$groups$id, "religion"),
        !!!map2(timeline$groups$id, timeline$groups$label, metric_group_panel)
      )
    ),

    accordion(id = "notes", open = FALSE, sources_panel()),

    tags$p(
      class = "small text-body-secondary mb-0",
      "Drag across a panel to zoom in time. Double-click to reset. Hover for details."
    )
  ),

  div(class = "range-readout", uiOutput("range_readout")),
  uiOutput("plot_area")
)
