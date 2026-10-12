# Turn times into text people can read. Needs constants.R and dplyr.
# Shared by the data layer (tooltips) and the app (readout, hover text).
#
# Times are calendar years `t` (decimal, astronomical numbering). Text is always "years ago",
# counted back from `today`, abbreviated with k = thousand, m = million, b = billion.

format_count <- function(x, digits = 0) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

# Three significant digits: 954 -> "954", 12340 -> "12.3k", 66e6 -> "66m", 4.54e9 -> "4.54b".
format_ya <- function(years_ago) {
  years_ago <- pmax(years_ago, 0)
  divisor <- dplyr::case_when(
    years_ago >= 1e9 ~ 1e9, years_ago >= 1e6 ~ 1e6, years_ago >= 1e3 ~ 1e3, .default = 1
  )
  suffix <- dplyr::case_when(
    years_ago >= 1e9 ~ "b", years_ago >= 1e6 ~ "m", years_ago >= 1e3 ~ "k", .default = ""
  )
  # formatC() pads some values with a leading space; trim it.
  paste0(trimws(formatC(years_ago / divisor, format = "fg", digits = 3)), suffix)
}

# "12.3k years ago", "1 year ago", or "now".
format_years_ago <- function(years_ago) {
  dplyr::case_when(
    years_ago < 0.5 ~ "now",
    round(years_ago) == 1 ~ "1 year ago",
    .default = paste(format_ya(years_ago), "years ago")
  )
}

# A calendar year as "1066 CE" / "44 BCE" (astronomical year 0 is 1 BCE).
format_calendar <- function(t) {
  year <- floor(t)
  ifelse(year >= 1, paste(formatC(year, format = "f", digits = 0), "CE"),
         paste(formatC(1 - year, format = "f", digits = 0), "BCE"))
}

# Calendar years are only worth quoting for recent history.
CALENDAR_HORIZON <- 12000

# One moment: "954 years ago (1066 CE)", or just "66m years ago" for deep time.
format_when <- function(t, today) {
  years_ago <- today - t
  text <- format_years_ago(years_ago)
  ifelse(years_ago <= CALENDAR_HORIZON, paste0(text, " (", format_calendar(t), ")"), text)
}

# A stretch of time: "954 → 580 years ago (1066 CE → 1440 CE)"; collapses to one moment
# when both ends read the same.
format_when_span <- function(t0, t1, today) {
  ya0 <- today - t0
  ya1 <- today - t1
  same <- format_years_ago(ya0) == format_years_ago(ya1)
  from <- format_ya(ya0)
  to <- ifelse(ya1 < 0.5, "now", format_ya(ya1))
  core <- ifelse(ya1 < 0.5, paste0(from, " → now"), paste0(from, " → ", to, " years ago"))
  both_recent <- ya0 <= CALENDAR_HORIZON & ya1 <= CALENDAR_HORIZON
  core <- ifelse(
    both_recent,
    paste0(core, " (", format_calendar(t0), " → ", format_calendar(t1), ")"),
    core
  )
  ifelse(same, format_when(t0, today), core)
}

# Round-number ticks for a window [lo, hi] of the x-axis, labelled as years ago ("12k", "66m",
# "4.5b", "now"). The axis itself counts years elapsed since the Earth formed, so the tick
# POSITIONS are elapsed years while the TEXT is years ago. src/timeline.js repeats this
# algorithm to relabel the axis as you zoom; keep the two in step.
year_ticks <- function(lo, hi, target = 7) {
  years_ago_lo <- max(EARTH_AGE - hi, 0)
  years_ago_hi <- min(EARTH_AGE - lo, EARTH_AGE)
  raw_step <- max(years_ago_hi - years_ago_lo, 1e-6) / target
  magnitude <- 10^floor(log10(raw_step))
  step <- magnitude * dplyr::case_when(
    raw_step / magnitude < 1.5 ~ 1, raw_step / magnitude < 3.5 ~ 2,
    raw_step / magnitude < 7.5 ~ 5, .default = 10
  )
  years_ago <- seq(ceiling(years_ago_lo / step) * step, years_ago_hi, by = step)
  list(
    values = EARTH_AGE - years_ago,
    text = ifelse(years_ago <= 1e-4, "now", format_ya(years_ago))
  )
}

# The whole tooltip: a bold title, a "When" line counted back from `today`, then the metric's own
# lines (`body`, from hover_lines() in the data layer). `t1` equals `t0` for a single moment.
hover_text <- function(title, t0, t1, today, body = "") {
  paste0(
    "<b>", htmltools::htmlEscape(title), "</b>",
    "<br><span style=\"color:#8a8f98\">When</span> ", format_when_span(t0, t1, today),
    body
  )
}

# A length of time (in years) as text, e.g. 3.2e7 -> "32 million years".
format_duration <- function(years) {
  dplyr::case_when(
    years < 1 ~ "less than a year",
    years < 1e6 ~ paste(format_count(years), "years"),
    years < 1e9 ~ paste(format_count(years / 1e6, 1), "million years"),
    .default = paste(format_count(years / 1e9, 2), "billion years")
  )
}
