library(burnr)
context('Interval statistics')

data(pgm)
REF <- composite(pgm)

test_that("Check observed interval mean", {
  expect_equal(mean(intervals(REF)), 11.75)
})

test_that("Check observed interval median", {
  expect_equal(median(intervals(REF)), 10)
})

test_that("Check observed interval standard dev", {
  expect_equal(round(sd(intervals(REF)), 3), 7.738)
})

test_that("Check minimum interval", {
  expect_equal(min(intervals(REF)), 2)
})

test_that("Check maximum interval", {
  expect_equal(max(intervals(REF)), 30)
})


