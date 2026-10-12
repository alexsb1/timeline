# Constants and the one place "today" enters the app. Shared by the data layer and the app.
#
# The x-axis is "years elapsed since the formation of the Earth": 0 is the formation of the
# Earth and EARTH_AGE is today. Data is stored as calendar years (see src/data/helpers.R), which
# do not depend on today's date; only the conversion to the x-axis does, and it happens when a
# plot is built, so "years ago" is always counted back from the day the app is used.

EARTH_AGE <- 4.6e9

# Today as a decimal calendar year, e.g. 2026-07-01 -> 2026.5. (Astronomical numbering: the
# year before 1 CE is 0, then -1, ... so subtracting years always works.)
today_year <- function(date = Sys.Date()) {
  year <- as.integer(format(date, "%Y"))
  day_of_year <- as.integer(format(date, "%j"))
  leap <- year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
  year + (day_of_year - 0.5) / ifelse(leap, 366, 365)
}

# Calendar year -> position on the x-axis (years elapsed since the Earth formed).
year_to_elapsed <- function(year, today = today_year()) EARTH_AGE - (today - year)
