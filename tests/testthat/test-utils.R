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

test_that("make_rec_type handles single character vector", {
  test_subj <- make_rec_type("late_fs")
  expect_true(is.factor(test_subj))
  expect_equal(length(levels(test_subj)), 19)
})

test_that("make_rec_type handles multiple character vector", {
  test_subj <- make_rec_type(c("null_year", "late_fs"))
  expect_equal(length(test_subj), 2)
  expect_true(is.factor(test_subj))
  expect_equal(length(levels(test_subj)), 19)
})

test_that("make_rec_type throws error on bad levels", {
  expect_error(make_rec_type("BACON!"), "not TRUE")
  expect_error(make_rec_type(c("null_year", "BACON!")), "not all TRUE")
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

test_that("count_event_position on FHX object without injuries as events", {
  test_subj <- count_event_position(REF_MULTI)
  expect_equal(subset(test_subj, event == "unknown_fs")$count, 2)
  expect_equal(subset(test_subj, event == "early_fs")$count, 4)
})

test_that("count_event_position on FHX object with injuries as events", {
  test_subj <- count_event_position(REF_MULTI, injury_event = TRUE)
  expect_equal(subset(test_subj, event == "unknown_fs")$count, 2)
  expect_equal(subset(test_subj, event == "unknown_fi")$count, 6)
})

test_that("count_event_position on FHX object with multiple select positions", {
  test_subj <- count_event_position(REF_MULTI, position = c("unknown", "dormant"))
  expect_equal(subset(test_subj, event == "dormant_fs")$count, 3)
  expect_equal(subset(test_subj, event == "unknown_fs")$count, 2)
  expect_false("early_fs" %in% as.character(test_subj$event))
})

test_that("count_event_position on FHX object groupby list", {
  grplist <- list(foo = c("unknown_fs", "early_fs"), bar = c("dormant_fs", "unknown_fi"))
  test_subj <- count_event_position(REF_MULTI, groupby = grplist)
  expect_equal(subset(test_subj, event == "unknown_fs")$count, 2)
  expect_equal(subset(test_subj, event == "early_fs")$count, 4)
  expect_equal(subset(test_subj, event == "foo")$count, 6)
  expect_equal(subset(test_subj, event == "bar")$count, 3)
})


test_that("sort.fhx on FHX object by first_year", {
  goal <- c("a", "b")
  test_fhx <- fhx(year = c(1850, 2010, 1900, 2000),
                  series = factor(c("a", "a", "b", "b")),
                  rec_type = c("pith_year", "bark_year", 
                               "pith_year", "bark_year"))
  sorted <- sort(test_fhx)
  expect_equal(goal, levels(sorted$series))
})

test_that("sort.fhx on FHX object by first_year with decreasing=TRUE", {
  goal <- c("b", "a")
  test_fhx <- fhx(year = c(1850, 2010, 1900, 2000),
                  series = factor(c("a", "a", "b", "b")),
                  rec_type = c("pith_year", "bark_year", 
                               "pith_year", "bark_year"))
  sorted <- sort(test_fhx, sort_by = "first_year", decreasing = TRUE)
  expect_equal(goal, levels(sorted$series))
})


test_that("sort.fhx on FHX object by last_year", {
  goal <- c("b", "a")
  test_fhx <- fhx(year = c(1850, 2010, 1900, 2000),
                  series = factor(c("a", "a", "b", "b")),
                  rec_type = c("pith_year", "bark_year", 
                               "pith_year", "bark_year"))
  sorted <- sort(test_fhx, sort_by = 'last_year')
  expect_equal(goal, levels(sorted$series))
})

test_that("+.fhx on FHX objects", {
  year1 <- c(1850, 2010)
  series1 <- c("a", "a")
  rt1 <- c("pith_year", "bark_year")
  year2 <- c(1900, 2000)
  series2 <- c("b", "b")
  rt2 <- c("pith_year", "bark_year")
  test_fhx1 <- fhx(year = year1,
                  series = factor(series1),
                  rec_type = rt1) 
  test_fhx2 <- fhx(year = year2,
                  series = factor(series2),
                  rec_type = rt2)
  test_fhx3 <- test_fhx1 + test_fhx2
  expect_equal(test_fhx3$year, c(year1, year2))
  expect_equal(test_fhx3$series, factor(c(series1, series2)))
  expect_equal(test_fhx3$rec_type, make_rec_type(c(rt1, rt2)))
})

test_that("check_duplicates returns with OK fhx obj", {
  test_fhx <- fhx(year = c(1850, 2010, 1900, 2000),
                  series = factor(c("a", "a", "b", "b")),
                  rec_type = c("pith_year", "bark_year", 
                               "pith_year", "bark_year"))
  checked <- burnr:::check_duplicates(test_fhx) 
  expect_equal(test_fhx, checked)
})

test_that("check_duplicates throws error when fhx obj has duplicates", {
  test_fhx <- fhx(year = c(1850, 2010, 1900, 2000),
                  series = factor(c("a", "a", "b", "b")),
                  rec_type = c("pith_year", "bark_year", 
                               "pith_year", "bark_year"))
  test_fhx <- rbind(test_fhx, test_fhx)
  expect_error(burnr:::check_duplicates(test_fhx), 
               "*Please resolve duplicate records*")
})


