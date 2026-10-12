# Fetch, build and inspect one or more datasets, without touching the cache.
#
#   Rscript processing/check_dataset.R monarchs pandemics      (run from the project root)
#
# For each metric id it downloads the sources the metric needs (live), prints what came back,
# builds the metric (which runs the contract checks in new_metric()), and prints a summary and
# a few sample tooltips as the app would show them today. Use it while writing a ds_*.R file.

options(width = 200, pillar.width = 200, warn = 1)
suppressWarnings(source("src/data/index.R", local = TRUE, encoding = "UTF-8"))

summarise_time <- function(t) {
  today <- today_year()
  paste(format_when(min(t), today), "...", format_when(max(t), today))
}

for (id in commandArgs(trailingOnly = TRUE)) {
  spec <- .registry$metrics[[id]]
  if (is.null(spec)) stop("No metric registered as '", id, "'. Registered: ", paste(names(.registry$metrics), collapse = ", "))
  cat("\n==================== metric:", id, "\n")

  inputs <- map(set_names(c(spec$needs, spec$optional)), \(source_id) {
    source <- .registry$sources[[source_id]]
    if (is.null(source) && source_id %in% spec$optional) {
      cat("--- optional source", source_id, "is not registered; building without it\n")
      return(NULL)
    }
    if (is.null(source)) stop("metric needs source '", source_id, "' but none is registered")
    started <- Sys.time()
    data <- tryCatch(source$fetch(), error = \(e) { cat("  source", source_id, "FAILED:", conditionMessage(e), "\n"); NULL })
    if (!is.null(data)) {
      cat(sprintf("--- source %s: %d rows x %d cols in %.1fs  [%s]\n", source_id, nrow(data), ncol(data), as.numeric(difftime(Sys.time(), started, units = "secs")), source$url))
      print(head(data, 4), n = 4)
    }
    data
  })

  metric <- spec$build(inputs)
  today <- today_year()
  cat("\n--- metric:", metric$label, " | kind:", metric$kind, " | group:", metric$group, " | rows:", nrow(metric$data), " | default_on:", metric$default_on, "\n")
  time_cols <- switch(metric$kind, line = "t", c("t0", "t1"))
  cat("    covers:", summarise_time(unlist(metric$data[time_cols])), "\n")
  if (metric$kind == "spans") cat("    lanes:", metric$n_lanes, "| instants:", sum(metric$data$instant), "\n")
  if (!is.null(metric$legend)) print(metric$legend)

  sample_rows <- unique(round(seq(1, nrow(metric$data), length.out = min(4, nrow(metric$data)))))
  if (metric$kind != "line") {
    t0 <- if (metric$kind == "events") metric$data$t0 else metric$data$t0
    t1 <- metric$data$t1
    tooltips <- hover_text(metric$data$title, t0, t1, today, metric$data$body)
    cat("\n    sample tooltips:\n")
    walk(tooltips[sample_rows], \(x) cat("     *", str_replace_all(str_replace_all(x, "<br>", " | "), "<[^>]+>", ""), "\n"))
  } else {
    print(metric$data[sample_rows, ])
  }
}
