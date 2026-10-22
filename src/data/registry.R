# The panel groups, and the dataset files that register the sources and metrics.
#
# Each file src/data/ds_<name>.R describes one dataset: where it comes from (register_source),
# and how it becomes a panel (register_metric). Panels appear in the order of DATASET_FILES,
# within the group they declare. To add a dataset, write a file and add its name here.

METRIC_GROUPS <- tibble(
  id = c("earth", "climate", "human", "religion"),
  label = c(
    "Earth & deep time", "Climate", "Life & human history", "Religion (Abrahamic)"
  )
)

DATASET_FILES <- c(
  # Earth & deep time
  "ds_geo_timescale", "ds_supercontinents", "ds_meteorites", "ds_volcanoes",
  # Climate
  "ds_co2", "ds_co2_proxy", "ds_temperature", "ds_lr04", "ds_insolation",
  "ds_climate_events", "ds_bond_events",
  # Life & human history
  "ds_prehistory", "ds_civilisations", "ds_monarchs", "ds_world_population",
  "ds_pandemics", "ds_historic_events",
  # Religion: kept apart from the secular record above
  "ds_religious_events"
)

# A dataset file that fails to load is skipped (and reported) rather than taking the app down.
for (dataset in DATASET_FILES) {
  path <- file.path("src", "data", paste0(dataset, ".R"))
  if (!file.exists(path)) next
  tryCatch(
    source(path, local = TRUE, encoding = "UTF-8"),
    error = \(e) message("[data] could not load ", dataset, ": ", conditionMessage(e))
  )
}
rm(dataset, path)
