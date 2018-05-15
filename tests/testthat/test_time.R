context("Time")

library(lubridate)

start <- ymd_hms("2016-11-14 12:00:00", tz = "US/Eastern")
later <- ymd_hms(
  "2016-11-14 12:00:00", # same time
  "2016-11-14 12:30:00", # 30 min later
  "2016-11-15 12:00:00", # 1 day later
  "2016-11-21 12:00:00", # 1 week later
  "2016-12-21 12:00:00", # 1 month later
  "2017-11-21 12:00:00", # 1 year later
  tz = "US/Eastern"
)
earlier <- ymd_hms(
  "2016-11-14 12:00:00", # same time
  "2016-11-14 11:30:00", # 30 min earlier
  "2016-11-13 12:00:00", # 1 day earlier
  "2016-11-07 12:00:00", # 1 week earlier
  "2016-10-14 12:00:00", # 1 month earlier
  "2015-11-14 12:00:00", # 1 year earlier
  tz = "US/Eastern"
)

test_that("counting starts at correct number", {
  expect_equal(count_periods(start, start, days(1), FALSE), 1)
  expect_equal(count_periods(start, start, days(1), TRUE), 0)
})

test_that("backwards is negative", {
  expect_lt(count_periods(start, earlier[4], days(1)), 0)
})
