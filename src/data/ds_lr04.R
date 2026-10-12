# Deep-ocean temperature: the LR04 benthic d18O stack (Lisiecki & Raymo 2005), from NOAA.
# A worked example of a dataset file: one source (download + tidy) and one metric (the panel).

LR04_URL <- "https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/lisiecki2005/lisiecki2005.txt"

# The NOAA file is free text with the stack as a block of three numeric columns under a
# "Time d18O Error" heading, followed by other material. Returns age_ka, d18o, se.
fetch_lr04 <- function() {
  lines <- str_split_1(http_get_text(LR04_URL), "\r?\n")

  header <- str_which(lines, "^\\s*Time\\s+d18O\\s+Error")
  if (length(header) == 0) stop("LR04 data heading not found; the file layout has changed")

  block <- lines[(header[1] + 1):length(lines)]
  is_data <- str_detect(block, "^\\s*[0-9.]+\\s+[0-9.]+\\s+[0-9.]+\\s*$")
  stack <- block[seq_len(match(FALSE, is_data) - 1)]  # the first unbroken run of numbers

  read_table(I(paste(stack, collapse = "\n")), col_names = c("age_ka", "d18o", "se"), col_types = "ddd")
}

register_source(new_source(
  id = "lr04",
  title = "LR04 benthic δ18O stack",
  url = "https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/lisiecki2005/",
  citation = "Lisiecki, L. E. & Raymo, M. E. (2005). A Pliocene-Pleistocene stack of 57 globally distributed benthic δ18O records. Paleoceanography 20, PA1003. doi:10.1029/2004PA001071. NOAA NCEI.",
  fetch = fetch_lr04,
  max_age_days = 180,  # a published record: it does not change
  min_rows = 2000
))

register_metric("lr04", needs = "lr04", build = function(src) {
  new_metric(
    id = "lr04", label = "Deep-sea temperature (benthic δ¹⁸O)", group = "climate", kind = "line",
    data = src$lr04 |>
      transmute(series = "lr04", t = ka_to_year(age_ka), y = d18o) |>
      arrange(t),
    # Heavier oxygen (higher d18O) means colder oceans and more ice, so the axis runs upside
    # down, as is conventional, to put warm at the top. Points are coloured blue (cold) to red.
    y_title = "Benthic δ<sup>18</sup>O (‰)", y_unit = "‰", y_format = ".2f",
    y_reversed = TRUE, heat = "low_is_warm", y_top_label = "warmer", y_bottom_label = "cooler",
    legend = tibble(series = "lr04", label = "Benthic δ¹⁸O stack (LR04)", colour = "#8C510A"),
    default_on = TRUE,
    description = "Oxygen-isotope ratios in the shells of deep-sea organisms: a record of ice volume and deep-ocean temperature over the last 5.3 million years. Plotted upside down, as is conventional, so that warmer is higher.",
    sources = "lr04"
  )
})
