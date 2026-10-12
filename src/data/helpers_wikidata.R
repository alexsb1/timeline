# Shared helpers for reading dates out of Wikidata, used by ds_monarchs.R, ds_pandemics.R and
# ds_historic_events.R. Needs helpers.R.

# Verified against known dates: Wikidata's SPARQL service gives YEAR() in ASTRONOMICAL numbering
# (Julius Caesar's death, 44 BCE, is year -43; Augustus's birth, 63 BCE, is -62). It also
# converts every day-precision date stored in the Julian calendar to the proleptic GREGORIAN
# calendar: the Battle of Hastings (14 October 1066, Julian) comes back as 20 October, William I's
# coronation (25 December 1066) as 31 December. Historians quote Julian dates, and the shift can
# push a late-December date into the next year, so day-precision Julian dates are converted back.
# Year- and month-precision values are not shifted by the service (they are stored as the 1st),
# so they are left alone.

# Gregorian (y, m, d) -> Julian (y, m, d), via the Julian day number. Works for BCE years
# (astronomical numbering) down to about 4800 BCE.
gregorian_to_julian <- function(year, month, day) {
  a <- (14 - month) %/% 12
  y <- year + 4800 - a
  m <- month + 12 * a - 3
  jdn <- day + (153 * m + 2) %/% 5 + 365 * y + y %/% 4 - y %/% 100 + y %/% 400 - 32045
  c <- jdn + 32082
  d <- (4 * c + 3) %/% 1461
  e <- c - (1461 * d) %/% 4
  m2 <- (5 * e + 2) %/% 153
  list(
    year = d - 4800 + m2 %/% 10,
    month = m2 + 3 - 12 * (m2 %/% 10),
    day = e - (153 * m2 + 2) %/% 5 + 1
  )
}

# Calendar year `t` from the columns every Wikidata query here returns for a date:
# y, m, d (YEAR(), MONTH(), DAY() of the time value), the Wikidata precision (9 = year,
# 10 = month, 11 = day) and jul (1 if the value is stored in the Julian calendar).
wd_year <- function(y, m, d, precision, jul) {
  m <- if_else(precision >= 10, m, NA_integer_)
  d <- if_else(precision >= 11, d, NA_integer_)
  shift <- !is.na(jul) & jul == 1 & precision >= 11 & !is.na(d)
  if (any(shift)) {
    j <- gregorian_to_julian(y[shift], m[shift], d[shift])
    y[shift] <- j$year
    m[shift] <- j$month
    d[shift] <- j$day
  }
  calendar_to_year(y, "CE", month = as.integer(m), day = as.integer(d))
}

