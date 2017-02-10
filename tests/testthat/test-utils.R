library(burnr)
context('Utils')

data(lgr2)
TARGET_SERIES <- "LGR53"
REF_MULTI <- lgr2
REF_SINGLE <- get_series(REF_MULTI, TARGET_SERIES)

test_that("spotcheck get_series", {
  test_subj <- get_series(REF_MULTI, TARGET_SERIES) 
  expect_true(all(test_subj$series == TARGET_SERIES))
})

test_that("series_names on single series", {
  expect_match(series_names(REF_SINGLE), TARGET_SERIES)
})

test_that("series_names on multi-series FHX object", {
  a <-  c("LGR54", "LGR44", "LGR47", "LGR48", "LGR46", "LGR41", "LGR52", 
          "LGR51", "LGR45", "LGR49", "LGR53", "LGR43", "LGR55", "LGR56", 
          "LGR36", "LGR33", "LGR31", "LGR32", "LGR27", "LGR29", "LGR25", 
          "LGR35", "LGR30", "LGR26", "LGR42", "LGR34")
  test_subj <- series_names(REF_MULTI)
  expect_true(length(union(a, test_subj)) == length(a))
})

test_that("get_year on single series", {
  test_subj <- get_year(REF_SINGLE, 1825)
  expect_match(as.character(test_subj$series), TARGET_SERIES)
  expect_equal(test_subj$year, 1825)
  expect_match(as.character(test_subj$rec_type), "recorder_year")
})

test_that("delete series on multi-series FHX object", {
  test_subj <- delete(REF_MULTI, s = TARGET_SERIES)
  expect_false(TARGET_SERIES %in% series_names(test_subj))
})

test_that("delete year on multi-series FHX object", {
  target_year <- 1825
  test_subj <- delete(REF_MULTI, yr = target_year)
  expect_false(target_year %in% series_names(test_subj))
})

test_that("delete series and year on multi-series FHX object", {
  target_year <- 1825
  test_subj <- delete(REF_MULTI, s = TARGET_SERIES, yr = target_year)
  expect_false(as.character(TARGET_SERIES) %in% as.character(series_names(get_year(test_subj, target_year))))
  expect_false(target_year %in% get_series(test_subj, TARGET_SERIES)$year)
})

test_that("year_range on multi-series FHX object", {
  test_subj <- year_range(REF_MULTI)
  expect_true(all(test_subj == c(1366, 2012)))
})


