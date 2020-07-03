library(burnr)
context("Interval statistics")

data(pgm)
REF <- composite(pgm)
TEST_INTER <- intervals(REF)

test_that("Check observed interval mean", {
  expect_equal(mean(TEST_INTER), 11.75)
})

test_that("Check observed interval median", {
  expect_equal(median(TEST_INTER), 10)
})

test_that("Check minimum interval", {
  expect_equal(min(TEST_INTER), 2)
})

test_that("Check maximum interval", {
  expect_equal(max(TEST_INTER), 30)
})

test_that("quantile.intervals() basic cases", {
  expect_equal(quantile(TEST_INTER),
    c(3.918526, 10.590013, 20.555245),
    tolerance = 1e-3
  )
  expect_equal(quantile(TEST_INTER, q = c(0.25)),
    c(6.227954),
    tolerance = 1e-3
  )
})

test_that("The intervals object prints", {
  prnt_int <- capture_output(print(TEST_INTER))
  expect_equal(nchar(prnt_int), 771)
})

test_that("Plotting intervals works", {
  p <- plot(TEST_INTER)
  expect_is(p, "ggplot")
})

