library(burnr)
context('Interval statistics')

data(pgm)
REF <- composite(pgm)
TEST_INTER <- intervals(REF)

test_that("Check observed interval mean", {
  expect_equal(mean(TEST_INTER), 11.75)
})

test_that("Check observed interval median", {
  expect_equal(median(TEST_INTER), 10)
})

test_that("Check observed interval standard dev", {
  expect_equal(round(sd(TEST_INTER), 3), 7.738)
})

test_that("Check minimum interval", {
  expect_equal(min(TEST_INTER), 2)
})

test_that("Check maximum interval", {
  expect_equal(max(TEST_INTER), 30)
})


