library(burnr)
data(lgr2)
data(lgr2_meta)
context("FHX plotting")

TEST_FHX <- lgr2
TEST_META <- lgr2_meta

test_that("plot_demograph() gets ggplot on fhx object", {
  p <- plot_demograph(TEST_FHX)
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot on 50yr fhx object", {
  p <- plot_demograph(get_year(TEST_FHX, seq(1800, 1850)))
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot with composite_rug", {
  p <- plot_demograph(TEST_FHX, composite_rug = TRUE)
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot with facet", {
  p <- plot_demograph(TEST_FHX,
    facet_group = TEST_META$SpeciesID,  # nolint
    facet_id = TEST_META$TreeID  # nolint
  )
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot with grid facet", {
  p <- plot_demograph(TEST_FHX,
    facet_group = TEST_META$SpeciesID,  # nolint
    facet_id = TEST_META$TreeID,  # nolint
    facet_type = "grid"
  )
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot with wrap facet", {
  p <- plot_demograph(TEST_FHX,
    facet_group = TEST_META$SpeciesID,  # nolint
    facet_id = TEST_META$TreeID,  # nolint
    facet_type = "wrap"
  )
  expect_is(p, "ggplot")
})

test_that("plot_demograph() gets ggplot with color", {
  p <- plot_demograph(TEST_FHX,
    color_group = TEST_META$SpeciesID,  # nolint
    color_id = TEST_META$TreeID  # nolint
  )
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
