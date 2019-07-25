library(burnr)
data(lgr2)
context("FHX plotting")

TEST_FHX <- lgr2

test_that("plot_demograph() gets ggplot on fhx object", {
  p <- plot_demograph(TEST_FHX)
  expect_is(p, "ggplot")
})

test_that("plot() method gets ggplot on fhx object", {
  # Note we're directing output to a temp jpg file.
  tmpfile <- tempfile()

  jpeg(tmpfile)
  p <- plot(TEST_FHX)
  dev.off()

  expect_is(p, "ggplot")
  unlink(tmpfile)
})
