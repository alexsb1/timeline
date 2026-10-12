# Download every data source, tidy it, and cache it in data/sources/. Run from the project root:
#
#   Rscript processing/run_all.R                 re-download everything
#   Rscript processing/run_all.R --stale-only    only sources older than their refresh interval
#
# Do this before deploying, so the app ships with fresh caches and starts without waiting on the
# network. (The app also refreshes stale sources itself at start-up; this just does it up front.)

options(width = 200, pillar.width = 200)
suppressWarnings(source("src/data/index.R", local = TRUE, encoding = "UTF-8"))

force <- !("--stale-only" %in% commandArgs(trailingOnly = TRUE))
started <- Sys.time()
timeline <- load_timeline(force = force, budget_seconds = Inf)

cat("\n==== sources ====\n")
print(
  timeline$sources |> transmute(id, status, fetched = format(fetched_at, "%Y-%m-%d %H:%M"), note = str_trunc(coalesce(note, ""), 70)),
  n = Inf
)

cat("\n==== metrics ====\n")
print(
  tibble(
    id = names(timeline$metrics),
    group = map_chr(timeline$metrics, "group"),
    kind = map_chr(timeline$metrics, "kind"),
    rows = map_int(timeline$metrics, \(m) nrow(m$data))
  ),
  n = Inf
)

if (length(timeline$problems) > 0) {
  cat("\n==== PROBLEMS: these metrics could not be built ====\n")
  iwalk(timeline$problems, \(reason, id) cat(sprintf("  %-18s %s\n", id, reason)))
}

cat(sprintf(
  "\n%d of %d metrics ready; cache is %.0f KB; took %.0f s\n",
  length(timeline$metrics), length(.registry$metrics),
  sum(file.size(list.files(SOURCE_CACHE_DIR, full.names = TRUE))) / 1024,
  as.numeric(difftime(Sys.time(), started, units = "secs"))
))
