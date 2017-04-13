library(burnr)
context('Statistics')

data(lgr2)
REF_MULTI <- lgr2
REF_SINGLE <- get_series(REF_MULTI, "LGR53")

test_that("first_year on single series", {
  expect_equal(first_year(REF_SINGLE), 1772)
})

test_that("first_year on multi-series fhx object", {
  expect_equal(first_year(REF_MULTI), 1366)
})

test_that("last_year on single series", {
  expect_equal(last_year(REF_SINGLE), 1843)
})

test_that("last_year on multi-series fhx object", {
  expect_equal(last_year(REF_MULTI), 2012)
})

test_that("count_year_span on single series", {
  expect_equal(count_year_span(REF_SINGLE), 72)
})

test_that("count_year_span on multi-series fhx object", {
  expect_equal(count_year_span(REF_MULTI), 647)
})

test_that("outer_type on single series", {
  expect_match(as.character(outer_type(REF_SINGLE)), "outer_year")
})

test_that("outer_type on multi-series fhx object", {
  expect_match(as.character(outer_type(REF_MULTI)), "bark_year")
})

test_that("inner_type on single series", {
  expect_match(as.character(inner_type(REF_SINGLE)), "pith_year")
})

test_that("inner_type on multi-series fhx object", {
  expect_match(as.character(inner_type(REF_MULTI)), "inner_year")
})

test_that("count_scar on single series", {
  expect_equal(count_scar(REF_SINGLE), 1)
})

test_that("count_scar on multi-series fhx object", {
  expect_equal(count_scar(REF_MULTI), 9)
})

test_that("count_injury on single series", {
  expect_equal(count_injury(REF_SINGLE), 0)
})

test_that("count_injury on multi-series fhx object", {
  expect_equal(count_injury(REF_MULTI), 6)
})

test_that("count_recording on single series", {
  expect_equal(count_recording(REF_SINGLE), 38)
})

test_that("count_recording on multi-series fhx object", {
  expect_equal(count_recording(REF_MULTI), 514)
})

test_that("count_recording on single series with injury as events", {
  expect_equal(count_recording(REF_SINGLE, injury_event = TRUE), 38)
})

test_that("count_recording on multi-series fhx object with injury as events", {
  expect_equal(count_recording(REF_MULTI, injury_event = TRUE), 514)
})

test_that("series_mean_interval on single series", {
  expect_equal(series_mean_interval(REF_SINGLE), NA)
})

test_that("series_mean_interval on multi-series fhx object", {
  expect_equal(series_mean_interval(REF_MULTI), 47.9)
})

test_that("sample_depth on multi-series fhx object", {
  sdepth <- subset(sample_depth(REF_MULTI), 
                   year %in% c(1366, 1436, 2011,2012))[['samp_depth']]
  expect_equal(sdepth, c(0, 1, 13, 2))
})

