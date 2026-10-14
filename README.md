# Climate Timeline

An interactive Shiny app that puts Earth's climate and human history on one time axis. Tick
metrics on and off, drag across any panel to zoom, and every panel follows. Each panel keeps its
own y-axis, refitted to whatever is in view.

- **Axis.** Positions are *years elapsed since the formation of the Earth* (0 = the Earth forms,
  4.6 billion = today), so that events that happened together share an x position. The labels
  say how long ago that was: `12k`, `66m`, `4.5b` years ago (k = thousand, m = million,
  b = billion). Tooltips give the same, plus the calendar year for recent history.
- **Today.** "Years ago" is always counted back from today's date.
- **Live data.** Every dataset is downloaded from a maintained source (NOAA, NASA, Macrostrat,
  Our World in Data, Wikidata, Wikipedia, ...), cleaned, and cached; see *Where the data comes from*.

## Run it

```r
shiny::runApp()
```

Needs R >= 4.1 and these packages: `shiny`, `bslib`, `plotly`, `dplyr` (>= 1.1), `purrr`,
`stringr`, `tidyr`, `readr`, `tibble`, `curl`, `jsonlite`, `htmltools`, `htmlwidgets`, `rvest`,
`xml2`, `readxl`.

## How the data stays current

```
  web source  --fetch()-->  tidy table  --cached-->  data/sources/<id>.rds
                                                          |
   app start-up: reads each cache; re-downloads only the sources older than their refresh interval
                                                          |
                                           build(src) --> metric (one panel) --> the plot
```

- Each **source** is one download with a cleaning function, a refresh interval (`max_age_days`),
  a citation, and validation that fails loudly if the site's layout changes.
- The cache is shipped with the app, so start-up normally touches the network not at all. A source
  past its refresh interval is re-downloaded at start-up; if that fails (site down, layout changed)
  the last good copy is kept and the Sources & notes panel says so. A source with no cache and no
  network drops only its own panel.
- **Start-up never waits long.** Each cache records how long its download took. At start-up, a
  stale source is only re-downloaded if that fits in the remaining time budget (40 s by default;
  set `TIMELINE_REFRESH_SECONDS`, and `0` means never refresh). A slow source that does not fit
  keeps its cached copy, marked "a newer download is due" in the Sources panel, until the next
  deploy-time refresh. The slowest source takes about 10 s today, so in practice everything fits.
- **Before deploying, refresh everything up front** so the app ships with fresh caches:

  ```
  Rscript processing/run_all.R                 # re-download all sources (about 30 s)
  Rscript processing/run_all.R --stale-only    # only the ones past their interval
  ```

### Where the data comes from

| panel | source | refreshed after |
| --- | --- | --- |
| Geological time scale | International Chronostratigraphic Chart, via the [Macrostrat](https://macrostrat.org) API | 90 days |
| Supercontinents, Meteorite impacts, Volcanic eruptions | Wikipedia articles ([Supercontinent](https://en.wikipedia.org/wiki/Supercontinent), [List of impact structures on Earth](https://en.wikipedia.org/wiki/List_of_impact_structures_on_Earth), [List of largest volcanic eruptions](https://en.wikipedia.org/wiki/List_of_largest_volcanic_eruptions)) | 30 days |
| CO2 | NOAA GML Mauna Loa (monthly); NOAA NCEI ice-core composite; Foster et al. (2017) proxy reconstruction (an Excel supplement, so `readxl`) | 7 / 180 / 180 days |
| Temperature | NASA GISTEMP (monthly); EPICA Dome C ice core (NOAA NCEI) | 7 / 180 days |
| Deep-sea temperature (d18O) | LR04 stack, NOAA NCEI | 180 days |
| Summer sunshine at 65N | Berger & Loutre (1991) orbital solution, NOAA NCEI | 180 days |
| Climate events, Bond events | Wikipedia ("List of periods and events in climate history", "Bond event") | 30 days |
| Prehistory, Ancient civilisations | Wikipedia: each period's / civilisation's own article (infobox dates) | 30 days |
| Monarchs, Pandemics, Major historical events | Wikidata (SPARQL) | 14 days |
| World population | Our World in Data (HYDE, Gapminder, UN WPP) | 30 days |
| Religious events | `curated/religious_events.csv` (hand-written; read every start-up) | n/a |

Each source's citation and download date is shown in the app under **Sources & notes**.

## Layout

```
app.R                    entry point: loads the data, sources src/*.R, starts the app
src/
  constants.R            EARTH_AGE, and today_year(): the only place "today" enters
  time_format.R          calendar year -> "12k years ago (10000 BCE)", axis ticks, tooltips
  zoom.R                 the one-click zoom windows
  plot_panels.R          one trace builder per metric kind (line / spans / events)
  plot_timeline.R        stacks the selected metrics into a single plotly figure
  timeline.js            browser side: refits y-axes, relabels the x-axis, places bar labels
  ui.R, server.R         the page and its logic
  data/
    helpers.R            fetching, cleaning, time converters, the metric contract, the registry
    helpers_history.R    parsers for human-written dates and Wikipedia markup
    helpers_wikidata.R   reading dates out of Wikidata
    registry.R           panel groups and the order of the dataset files
    load.R               cache, refresh-when-stale, assemble the metrics
    ds_<name>.R          one file per dataset: its source(s) and its panel
www/styles.css
data/sources/            the cache: one .rds per source (shipped)
curated/                 hand-written inputs no one publishes (the religion layer)
processing/              command-line tools (not shipped)
  run_all.R              refresh every source into data/sources/
  check_dataset.R        fetch, build and inspect one dataset: Rscript processing/check_dataset.R lr04
```

## Adding a dataset

Write `src/data/ds_<name>.R` (copy `ds_lr04.R`, which is annotated as a worked example), add its
name to `DATASET_FILES` in `src/data/registry.R`, and check it:

```
Rscript processing/check_dataset.R <name>
```

A dataset file registers a **source** (`new_source()`: where it comes from, how to fetch and tidy
it) and a **metric** (`register_metric()`: how the tidy table becomes a panel). Every metric is one
of three kinds with a fixed set of columns, checked when it is built (see the header of
`src/data/helpers.R`):

| kind | looks like | columns |
| --- | --- | --- |
| `line` | CO2, temperature, population | `t, y, series` (+ `ymin, ymax` for a band) |
| `spans` | geological periods, reigns | `t0, t1, lane, label, fill, text_colour, instant, title, body` |
| `events` | meteorites, pandemics | `t, t0, t1, y, size, colour, title, body` |

Times are stored as calendar years `t` (decimal, 1 BCE = 0), which do not depend on today's date;
the app converts to the x-axis and to "years ago" when it draws.


## Notes on the data

- **`raw_data/`** holds the original hand-made CSVs. Nothing reads it any more; it is kept only for
  reference and can be deleted. (`Issues.txt` there is obsolete: those files were Windows-1252.)
- **d18O is drawn upside down**, the usual convention, so warmer is at the top, and temperature-like
  series are coloured from blue (cold) to red (warm).
- **Two temperature curves, not one.** The ice-core record is *Antarctic* temperature against the
  last millennium; the thermometer record is *global* temperature against 1951-1980. They are
  labelled separately rather than stitched into one line.
- **Scraped panels can break.** Panels built from Wikipedia articles depend on the article's
  layout. Each parser checks what it reads and fails loudly if the page has changed, and the app
  then keeps showing the last good copy (and says so under Sources & notes).
