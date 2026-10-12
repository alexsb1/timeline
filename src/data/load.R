# Load the timeline: read each source from its cache, refresh any that are stale, and assemble
# the metrics. Needs helpers.R and registry.R.
#
# Cached copies live in data/sources/<id>.rds and are shipped with the app, so start-up only
# touches the network for sources older than their `max_age_days`. If a refresh fails the last
# good copy is kept and the problem is reported, so a down website never breaks the app.

SOURCE_CACHE_DIR <- file.path("data", "sources")

cache_path <- function(id) file.path(SOURCE_CACHE_DIR, paste0(id, ".rds"))

read_cache <- function(id) {
  path <- cache_path(id)
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = \(e) NULL)
}

write_cache <- function(id, entry) {
  # The folder may be read-only where the app is hosted; the in-memory copy still works.
  tryCatch({
    dir.create(SOURCE_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(entry, cache_path(id), compress = "xz")
  }, error = \(e) NULL, warning = \(w) NULL)
  invisible(entry)
}

# One source's entry: list(id, data, fetched_at, status, note).
#   status: "live"        downloaded just now
#           "cached"      cached copy still within max_age_days
#           "deferred"    cached copy is past its refresh interval, but the last download took
#                         longer than the start-up budget has left, so it was not attempted
#           "stale"       refresh failed; using an older cached copy (see `note`)
#           "unavailable" refresh failed and there is no cached copy
# `budget_left` is the seconds this start-up may still spend downloading. Each cache entry records
# how long its download took (`fetch_seconds`), so a slow source is deferred to the next
# `processing/run_all.R` instead of making a visitor wait. With no cached copy it is always fetched.
refresh_source <- function(source, force = FALSE, budget_left = Inf) {
  cached <- if (source$local) NULL else read_cache(source$id)
  age_days <- if (is.null(cached)) Inf else as.numeric(difftime(Sys.time(), cached$fetched_at, units = "days"))
  due <- source$local || force || age_days > source$max_age_days

  if (!due) return(modifyList(cached, list(status = "cached")))

  expected_seconds <- cached$fetch_seconds %||% 0
  if (!force && !is.null(cached) && expected_seconds > budget_left) {
    message(sprintf("[data] %s: refresh due but it took %.0fs last time; deferred", source$id, expected_seconds))
    return(modifyList(cached, list(
      status = "deferred",
      note = sprintf("a newer download is due (it takes about %.0f s)", expected_seconds)
    )))
  }

  message("[data] fetching ", source$id, " ...")
  started <- Sys.time()
  result <- tryCatch({
    data <- source$fetch()
    if (!is.data.frame(data) || nrow(data) < source$min_rows) {
      stop("expected at least ", source$min_rows, " rows, got ", if (is.data.frame(data)) nrow(data) else "no table")
    }
    list(ok = TRUE, data = data)
  }, error = \(e) list(ok = FALSE, message = conditionMessage(e)))
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  if (result$ok) {
    message(sprintf("[data] %s: %d rows in %.1fs", source$id, nrow(result$data), seconds))
    entry <- list(
      id = source$id, data = result$data, fetched_at = Sys.time(), fetch_seconds = seconds,
      status = "live", note = NA_character_
    )
    if (!source$local) write_cache(source$id, entry)
    return(entry)
  }

  message("[data] ", source$id, " FAILED: ", result$message)
  if (!is.null(cached)) {
    return(modifyList(cached, list(status = "stale", note = result$message)))
  }
  list(id = source$id, data = NULL, fetched_at = as.POSIXct(NA), status = "unavailable", note = result$message)
}

# Refresh every registered source that is due. `budget_seconds` caps the time spent downloading
# at start-up (see refresh_source()): sources that would not fit keep their cached copy.
load_sources <- function(force = FALSE, budget_seconds = 40) {
  started <- Sys.time()
  map(.registry$sources, \(source) {
    used <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    refresh_source(source, force = force, budget_left = if (force) Inf else max(budget_seconds - used, 0))
  })
}

# Turn source entries into metrics. A metric whose source is missing, or whose builder fails, is
# left out and listed under `problems`; the rest still load.
assemble_timeline <- function(entries) {
  metrics <- list()
  problems <- character()

  for (spec in .registry$metrics) {
    inputs <- map(set_names(c(spec$needs, spec$optional)), \(id) entries[[id]]$data)
    missing <- intersect(names(inputs)[map_lgl(inputs, is.null)], spec$needs)
    if (length(missing) > 0) {
      problems[[spec$id]] <- paste("no data for source(s):", paste(missing, collapse = ", "))
      next
    }
    built <- tryCatch(spec$build(inputs), error = identity)
    if (inherits(built, "error")) {
      problems[[spec$id]] <- conditionMessage(built)
      message("[data] metric ", spec$id, " FAILED: ", conditionMessage(built))
    } else {
      metrics[[spec$id]] <- built
    }
  }

  sources <- map(.registry$sources, \(source) {
    entry <- entries[[source$id]]
    tibble(
      id = source$id, title = source$title, url = source$url, citation = source$citation,
      fetched_at = entry$fetched_at, status = entry$status, note = entry$note %||% NA_character_
    )
  }) |>
    list_rbind()

  list(
    meta = list(built_at = Sys.time()),
    groups = METRIC_GROUPS[METRIC_GROUPS$id %in% map_chr(metrics, "group"), ],
    metrics = metrics,
    sources = sources,
    problems = problems
  )
}

load_timeline <- function(force = FALSE, budget_seconds = 40) {
  assemble_timeline(load_sources(force = force, budget_seconds = budget_seconds))
}
