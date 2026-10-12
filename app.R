# Climate Timeline: Earth's climate and human history on one time axis.
#
# Entry point. Run with shiny::runApp(), or the "Run App" button in RStudio.
#
# The data comes from live sources (NOAA, NASA, Macrostrat, Our World in Data, Wikidata,
# Wikipedia...). Each source is cached in data/sources/ and refreshed at start-up only when its
# cached copy is older than its refresh interval, so the app is always reasonably current but
# rarely waits on the network. See src/data/ and the README.

library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(purrr)
library(stringr)

# The data layer: constants, time formatting, helpers, the dataset files, and the loader.
source("src/data/index.R", local = TRUE, encoding = "UTF-8")

source("src/zoom.R", local = TRUE, encoding = "UTF-8")
source("src/plot_panels.R", local = TRUE, encoding = "UTF-8")
source("src/plot_timeline.R", local = TRUE, encoding = "UTF-8")

# ---- data ----
# Refreshes any stale sources that fit in the start-up time budget; a failed download falls back
# to the cached copy. The budget (seconds) can be changed with the TIMELINE_REFRESH_SECONDS
# environment variable; 0 turns refreshing off and always uses the cached copies.
timeline <- load_timeline(budget_seconds = as.numeric(Sys.getenv("TIMELINE_REFRESH_SECONDS", "40")))
metrics <- timeline$metrics
if (length(metrics) == 0) stop("No datasets could be loaded: ", paste(names(timeline$problems), timeline$problems, sep = ": ", collapse = "; "))

# One row per metric, in display order: groups top to bottom, then the order they were registered in.
catalog <- tibble(
  id = names(metrics),
  label = unname(map_chr(metrics, "label")),  # unname(): shiny rejects named choice vectors
  group = unname(map_chr(metrics, "group")),
  description = unname(map_chr(metrics, "description")),
  default_on = unname(map_lgl(metrics, "default_on"))
) |>
  arrange(match(group, timeline$groups$id))

# The browser-side script that refits the y axes (see the notes at the top of the file).
timeline_js <- paste(readLines("src/timeline.js", encoding = "UTF-8"), collapse = "\n")

# ---- app ----
source("src/ui.R", local = TRUE, encoding = "UTF-8")
source("src/server.R", local = TRUE, encoding = "UTF-8")

shinyApp(ui, server)
