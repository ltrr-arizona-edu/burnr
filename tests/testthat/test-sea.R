library(burnr)
context("SEA")

data(pgm)
data(pgm_pdsi)
COMP_TEST <- composite(pgm)
suppressWarnings(RNGversion("3.5.0"))
set.seed(123)
SEA_TEST <- sea(pgm_pdsi, COMP_TEST)

test_that("Check sea warning on bad rownames", {
  expect_warning(sea(data.frame(1:2000), COMP_TEST), regexpr = "rownames")
})

test_that("Check sea stop on no matching years", {
  expect_error(sea(data.frame(1:20), COMP_TEST), regexpr = "shared")
})

test_that("Check sea departure data.frame", {
  goal_mean <- c(
    -0.283, 0.608, -0.148, 0.997, 1.234, 0.204,
    -2.156, -0.090, -0.505, 0.299, 0.283
  )
  goal_lower95 <- c(
    -1.006, -1.014, -1.054, -1.049, -1.061,
    -1.040, -0.957, -0.995, -1.025, -1.020, -1.045
  )
  goal_upper95 <- c(
    1.032, 1.014, 0.966, 0.996, 0.972,
    0.977, 0.965, 1.015, 1.015, 0.932, 1.013
  )
  goal_lower99 <- c(
    -1.267, -1.333, -1.337, -1.407, -1.311,
    -1.329, -1.333, -1.332, -1.310, -1.342, -1.364
  )
  goal_upper99 <- c(
    1.333, 1.415, 1.179, 1.331, 1.174,
    1.169, 1.341, 1.427, 1.508, 1.232, 1.386
  )
  expect_equal(SEA_TEST$departure$lag, seq(-6, 4))
  expect_equal(SEA_TEST$departure$mean, goal_mean)
  expect_equal(SEA_TEST$departure$upper_95_perc, goal_upper95)
  expect_equal(SEA_TEST$departure$lower_95_perc, goal_lower95)
  expect_equal(SEA_TEST$departure$upper_99_perc, goal_upper99)
  expect_equal(SEA_TEST$departure$lower_99_perc, goal_lower99)
})

test_that("Plotting sea output works", {
  p <- plot(SEA_TEST)
  expect_is(p, "ggplot")
})

test_that("The sea object prints", {
  prnt_sea <- capture_output(print(SEA_TEST))
  expect_equal(prnt_sea, "\tSuperposed Epoch Analysis\n\t=========================\n lag upper95 lower95 upper99 lower99 departure sig\n  -6   1.032  -1.006   1.333  -1.267    -0.283    \n  -5   1.014  -1.014   1.415  -1.333     0.608    \n  -4   0.966  -1.054   1.179  -1.337    -0.148    \n  -3   0.996  -1.049   1.331  -1.407     0.997   .\n  -2   0.972  -1.061   1.174  -1.311     1.234   *\n  -1   0.977  -1.040   1.169  -1.329     0.204    \n   0   0.965  -0.957   1.341  -1.333    -2.156   *\n   1   1.015  -0.995   1.427  -1.332    -0.090    \n   2   1.015  -1.025   1.508  -1.310    -0.505    \n   3   0.932  -1.020   1.232  -1.342     0.299    \n   4   1.013  -1.045   1.386  -1.364     0.283    \n---\nSignif. codes: 0.01 '*' 0.05 '.'")
})
