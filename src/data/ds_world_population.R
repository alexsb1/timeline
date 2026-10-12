# World population since 10,000 BCE, from Our World in Data's compilation of HYDE, Gapminder and
# the UN World Population Prospects.

fetch_world_population <- function() {
  text <- http_get_text(
    "https://ourworldindata.org/grapher/population.csv",
    query = list(country = "~OWID_WRL", csvType = "filtered")
  )
  data <- read_csv(I(text), show_col_types = FALSE, progress = FALSE)

  if (!all(c("Year", "Population") %in% names(data))) stop("unexpected columns: ", paste(names(data), collapse = ", "))
  if (!all(data$Entity == "World")) stop("expected only the World series")
  select(data, year = Year, population = Population)
}

register_source(new_source(
  id = "world_population",
  title = "World population (Our World in Data)",
  url = "https://ourworldindata.org/grapher/population",
  citation = "HYDE (2023); Gapminder (2022); UN WPP (2024), with major processing by Our World in Data. Population, ourworldindata.org/grapher/population. CC BY 4.0.",
  fetch = fetch_world_population,
  max_age_days = 30,  # revised about once a year
  min_rows = 200
))

register_metric("world_population", needs = "world_population", build = function(src) {
  new_metric(
    id = "world_population", label = "World population", group = "human", kind = "line",
    # Our World in Data writes BCE years as negatives (-10000 is 10,000 BCE).
    data = src$world_population |>
      transmute(
        series = "pop",
        t = calendar_to_year(abs(year), if_else(year < 0, "BCE", "CE")),
        y = population / 1e6
      ) |>
      arrange(t),
    y_title = "World population (millions)", y_unit = "million", y_format = ",.0f",
    legend = tibble(series = "pop", label = "World population", colour = okabe_ito[["blue"]]),
    description = "Estimated number of people alive, from 10,000 BCE to the latest published estimate.",
    sources = "world_population"
  )
})
