#' Generate series-level descriptive statistics for `fhx` object
#'
#' @param x An `fhx` object.
#' @param func_list A list of named functions that will be run on each series
#'   in the `fhx` object. The list name for each function is the corresponding
#'   column name in the output data frame.
#'
#' @return A `data.frame` containing series-level statistics.
#'
#' @seealso
#'   * [fhx()] creates an `fhx` object.
#'   * [as_fhx()] casts data frame into an `fhx` object.
#'   * [first_year()] gets earliest year in an `fhx` object.
#'   * [last_year()] gets latest year in an `fhx` object.
#'   * [count_year_span()] counts the year span of an `fhx` object.
#'   * [inner_type()] gets "rec_type" for inner event of an `fhx` object.
#'   * [outer_type()] get "rec_type" for outside event of an `fhx` object.
#'   * [count_scar()] counts scars in an `fhx` object.
#'   * [count_injury()] counts injuries in an `fhx` object.
#'   * [count_recording()] counts recording years in `fhx` object.
#'   * [series_mean_interval()] quickly estimates mean fire-interval of `fhx`
#'     object.
#'   * [sample_depth()] gets sample depth of an `fhx` object.
#'   * [summary.fhx()] brief summary of an `fhx` object.
#'   * [composite()] create a fire `composite` from an `fhx` object.
#'   * [intervals()] get fire `intervals` analysis from `composite`.
#'   * [sea()] superposed epoch analysis.
#'
#' @examples
#' data(lgr2)
#' series_stats(lgr2)
#'
#' # You can create your own list of statistics to output. You can also create
#' # your own functions:
#' flist <- list(
#'   n = count_year_span,
#'   xbar_interval = function(x) mean_interval(x, injury_event = TRUE)
#' )
#' sstats <- series_stats(lgr2)
#' head(sstats)
#' @export
series_stats <- function(x, func_list = list(
                           first = first_year, last = last_year,
                           years = count_year_span,
                           inner_type = inner_type, outer_type = outer_type,
                           number_scars = count_scar,
                           number_injuries = count_injury,
                           recording_years = count_recording,
                           mean_interval = series_mean_interval
                         )) {
  stopifnot(is_fhx(x))
  plyr::ddply(x, c("series"),
    function(df) data.frame(lapply(func_list, function(f) f(df)))
  )
}


#' First (earliest) year of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The minimum or first year of series in `x`.
#'
#' @seealso
#'   * [last_year()] get last year of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
first_year <- function(x) {
  min(x$year)
}


#' Last (most recent) year of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The maximum or last year of series in `x`. `NA` will be returned if
#'   `NA` is in `x$year`.
#'
#' @seealso
#'   * [first_year()] get first year of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
last_year <- function(x) {
  max(x$year)
}


#' Number of years of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The difference between the first and last observations in the `fhx`
#'   object. `NA` will be returned if `NA` is in `x$year`.
#'
#' @seealso
#'   * [first_year()] get first year of `fhx` object.
#'   * [last_year()] get last year of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
count_year_span <- function(x) {
  max(x$year) - min(x$year) + 1
}


#' Type of observation in the last (most recent) year of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The a factor giving the type of observation in the last observation
#'   of `x`.
#'
#' @seealso
#'   * [inner_type()] get observation type in inner-most year of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
outer_type <- function(x) {
  x$rec_type[which.max(x$year)]
}


#' Type of observation in the first (earliest) year of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The a factor giving the type of observation in the first observation
#'   of `x`.
#'
#' @seealso
#'   * [outer_type()] get observation type in outer-most year of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
inner_type <- function(x) {
  x$rec_type[which.min(x$year)]
}


#' Number of scar events in an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The number of fire scar events in `x`
#'
#' @seealso
#'   * [count_injury()] Count the injuries in an `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
count_scar <- function(x) {
  length(grep("_fs", x$rec_type))
}


#' Number of injury events in an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return The number of injury events in `x`
#'
#' @seealso
#'   * [count_scar()] Count the injuries in an `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
count_injury <- function(x) {
  length(grep("_fi", x$rec_type))
}


#' Number of recording years in an `fhx` object
#'
#' @param x An `fhx` object.
#' @param injury_event Boolean indicating whether injuries should be considered
#'   event.
#'
#' @return The number of recording events in `x`.
#'
#' @seealso [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
count_recording <- function(x, injury_event = FALSE) {
  nrow(find_recording(x, injury_event = injury_event))
}


#' Calculate quick mean fire interval of an `fhx` object with single series
#'
#' You really should be using [intervals()].
#'
#' @param x An `fhx` object with a single series.
#' @param injury_event Boolean indicating whether injuries should be considered
#'   event.
#'
#' @return The mean fire interval observed `x`.
#'
#' @seealso
#'   * [intervals()] Proper way to do fire-interval analysis of `fhx` object.
#'   * [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
series_mean_interval <- function(x, injury_event = FALSE) {
  if (length(unique(x$series)) > 1) {
    warning(
      "`series_mean_interval()` run on object with multiple series - ",
      "results may not be correct"
    )
  }
  search_str <- "_fs"
  if (injury_event) {
    search_str <- paste0("_fi|", search_str)
  }
  event_years <- sort(x$year[grepl(search_str, x$rec_type)])
  out <- NA
  if (length(event_years) > 1) {
    intervals <- diff(event_years)
    out <- round(mean(intervals), 1)
  }
  out
}

#' Calculate the sample depth of an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return A data frame containing the years and number of observations.
#'
#' @seealso [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
sample_depth <- function(x) {
  if (!is_fhx(x)) stop("x must be an fhx object")
  x_stats <- series_stats(x)
  n_trees <- nrow(x_stats)
  out <- data.frame(year = min(x_stats$first):max(x_stats$last))
  for (i in 1:n_trees) {
    yrs <- x_stats[i, ]$first:x_stats[i, ]$last
    treespan <- data.frame(year = yrs, z = 1)
    names(treespan)[2] <- paste(x_stats$series[i])
    out <- merge(out, treespan, by = c("year"), all = TRUE)
  }
  if (n_trees > 1) {
    out$samp_depth <- rowSums(out[, -1], na.rm = TRUE)
  }
  else {
    out$samp_depth <- out[, -1]
  }
  out <- subset(out, select = c("year", "samp_depth"))
  return(out)
}


#' Summary of `fhx` object
#'
#' @param object An `fhx` object.
#' @param ... Additional arguments that are tossed out.
#'
#' @return A `summary.fhx` object.
#'
#' @seealso [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @export
summary.fhx <- function(object, ...) {
  out <- list(
    number_series = length(series_names(object)),
    first_year = first_year(object),
    last_year = last_year(object),
    number_scars = count_scar(object),
    number_injuries = count_injury(object)
  )
  class(out) <- "summary.fhx"
  out
}


#' Percent scarred time series for `fhx` object
#'
#' @param x An `fhx` object.
#' @param injury_event Boolean indicating whether years with injury events
#'   should be considered as scars. Default is `FALSE`.
#'
#' @return `data.frame` with four columns:
#'   * "Year": The year.
#'   * "NumRec": The number of recording trees.
#'   * "NumScars": Number of fire scars and possibly fire injuries.
#'   * "PercScarred": The proportion of scars (and possibly injuries) to
#'     non-scar/injury series in the year.
#'
#' @seealso [series_stats()] basic statistics for series in an `fhx` object.
#'
#' @examples
#' data("pgm")
#' percent_scarred(pgm)
#' @export
percent_scarred <- function(x, injury_event = FALSE) {
  series_rec <- plyr::ddply(x, "series", find_recording, injury_event = TRUE)
  rec_count <- plyr::count(series_rec, "recording")
  series_fs <- x[grepl("_fs", x$rec_type), ]
  fs_count <- plyr::count(series_fs, "year")
  if (injury_event) {
    series_fs <- x[grepl("_fs", x$rec_type) | grepl("_fi", x$rec_type), ]
    fs_count <- plyr::count(series_fs, "year")
  }
  out <- merge(rec_count, fs_count,
    by.x = "recording", by.y = "year", all = TRUE
  )
  names(out) <- c("year", "num_rec", "num_scars")
  out[is.na(out$num_scars), "num_scars"] <- 0
  out$percent_scarred <- round(out$num_scars / out$num_rec * 100, 0)
  return(out)
}
