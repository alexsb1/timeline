# The time windows offered as one-click zooms, and how a window maps onto the x axis.
# Needs constants.R.

zoom_presets <- tibble::tribble(
  ~id, ~label, ~years_ago,
  "zoom_all", "All of Earth's history", EARTH_AGE,
  "zoom_phanerozoic", "Last 541 million years", 541e6,
  "zoom_pliocene", "Last 5.3 million years", 5.3e6,
  "zoom_ice_cores", "Last 800,000 years", 800e3,
  "zoom_holocene", "Last 12,000 years", 12e3,
  "zoom_recent", "Last 2,000 years", 2e3
)

# The x range showing the most recent `years_ago` years, with a sliver of room either side so
# markers at the very edges are not cut in half.
zoom_range <- function(years_ago) {
  pad <- 0.005 * years_ago
  c(EARTH_AGE - years_ago - pad, EARTH_AGE + pad)
}
