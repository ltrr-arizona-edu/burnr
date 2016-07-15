#' Constructor for S3 fhx class.
#'
#' @param year A numeric vector of observation years for each \code{series} and \code{rec_type} argument.
#' @param series A factor of series names for each \code{year} and \code{rec_type} argument.
#' @param rec_type A factor of ring types for each element in \code{year} and \code{series}.
#' @param metalist An option list of arbitrary metadata to be included in the fhx instance.
#'
#' @return An fhx instance.
#'
#' @export
fhx <- function(year,  series, rec_type, metalist=list()) {
  if (!is.numeric(year)) stop("year must be numeric")
  if (!is.factor(series)) stop("series must be character")
  if (!is.factor(rec_type)) stop("rec_type must be factor")
  if (!is.list(metalist)) stop("metalist must be list")
  ringsdf = data.frame(year = year, series = series, rec_type = rec_type)
  class(ringsdf) <- c('fhx', 'data.frame')
  ringsdf
}

#' Get years with events for an fhx object.
#'
#' @param x An \code{fhx} object.
#' @param scar_event Boolean indicating whether years with scar events should be returned. Default is TRUE.
#' @param injury_event Boolean indicating weather years with injury events should be returned. Default is FALSE.
#' @param custom_grep_str Character string to pass a custom grep search pattern to search rec_type column for. Undefined by default.
#'
#' @return A list. Elements of the list are integer vectors giving the years with events for each fhx series. Each element's name reflects the series name.
#'
#' @examples
#' data(pgm)
#' get_event_years(pgm, scar_event = TRUE, injury_event = TRUE)
#'
#' # Passing a custom string to grep. This one identified recorder years:
#' get_event_years(pgm, custom_grep_str = 'recorder_')
#'
#' # Use with composite to get composite years:
#' comp <- composite(pgm, comp_name = 'pgm')
#' event_yrs <- get_event_years(comp)[['pgm']]
#' print(event_yrs)
#'
#' @export
get_event_years <- function(x, scar_event=TRUE, injury_event=FALSE, custom_grep_str=NULL) {
  stopifnot(is.fhx(x))
  if (!is.null(custom_grep_str)) {
    message('burnr::get_events(): custom_search_str was defined, ignoring scar_event and injury_event arguments')
  }
  # Build our search string.
  search_str <- NA
  if (is.null(custom_grep_str)) {
    search_parts <- c()
    if (scar_event) {
      search_parts <- c(search_parts, '_fs')
    }
    if (injury_event) {
      search_parts <- c(search_parts, '_fi')
    }
    if (length(search_parts) > 1){
      search_str <- paste(search_parts, collapse = '|')
    } else {
      search_str <- search_parts
    }
  } else {
    search_str <- custom_grep_str
  }
  plyr::dlply(x, c('series'), function(a) a$year[grepl(search_str, a$rec_type)])
}

#' Get \code{fhx} series names.
#'
#' @param x An fhx object.
#'
#' @return A character vector or \code{NULL}.
#'
#'
#' @examples
#' data(lgr2)
#' series_names(lgr2)
#'
#' @export
series_names <- function(x) {
  stopifnot(is.fhx(x))
  as.character(unique(x$series))
}

#' Extract fhx observations for given years.
#'
#' @param x An fhx object.
#' @param yr Integer vector of year(s) you would like extracted from x.
#'
#' @return A dataframe with extracted observations.
#'
#' @examples
#' data(lgr2)
#' get_year(lgr2, 1806)
#'
#' get_year(lgr2, 1805:1807)
#'
#' @export
get_year <- function(x, yr) {
  stopifnot(is.fhx(x))
  stopifnot(is.numeric(yr))
  subset(x, x$year %in% yr)
}

#' Extract fhx observations for given series.
#'
#' @param x An fhx object.
#' @param s Character vector of series you would like extracted from x.
#'
#' @return A dataframe with extracted observations.
#'
#' @examples
#' data(lgr2)
#' get_series(lgr2, 'LGR46')
#'
#' get_series(lgr2, c('LGR41', 'LGR46'))
#'
#' @export
get_series <- function(x, s) {
  stopifnot(is.fhx(x))
  stopifnot(is.character(s))
  subset(x, x$series %in% s)
}

#' Remove series or years from an fhx object.
#'
#' @param x An fhx object.
#' @param s Character vector of series to erase from x.
#' @param yr Integer vector of years to erase from x.
#'
#' @return An fhx object with observations erased.
#'
#' @details
#' You can combine s and yr to specify years within select series to remove.
#'
#' @examples
#' data(lgr2)
#' plot(delete(lgr2, s = 'LGR46'))
#'
#' plot(delete(lgr2, yr = 1300:1550))
#'
#' @export
delete <- function(x, s, yr) {
  # Hint: It's just an inverse subset.
  stopifnot(is.fhx(x))
  out <- c()
  # I'm sure there is a more clever way to handle this.
  if (missing(s)) {
    out <- subset(x, !(x$year %in% yr))
  } else if (missing(yr)) {
    out <- subset(x, !(x$series %in% s))
  } else if (!missing(yr) & !missing(s)) {
    out <- subset(x, !((x$series %in% s) & (x$year %in% yr)))
  } else {
    out <- x
  }
  fhx(out$year, out$series, out$rec_type)
}

#' Subset `rings` data.frame to years that are considered recording.
#'
#' @param x A an fhx object dataframe.
#' @param injury_event Boolean indicating whether injuries should be considered event.
#'
#' @examples
#' require(plyr)
#' data(lgr2)
#' ddply(lgr2$rings, 'series', burnr:::find_recording, injury_event = TRUE)
#'
#' @return A dataframe with a column of each year which is 'recording'.
find_recording <- function(x, injury_event) {
  # Use with: ddply(lgr2$rings, 'series', recorder_finder)
  x <- x[order(x$year), ]
  recorder <- list("|" = "recorder_year",
                    "U" = "unknown_fs",
                    "D" = "dormant_fs",
                    "E" = "early_fs",
                    "M" = "middle_fs",
                    "L" = "late_fs",
                    "A" = "latewd_fs")
  injury <- list("u" = "unknown_fi",
                   "d" = "dormant_fi",
                   "e" = "early_fi",
                   "m" = "middle_fi",
                   "l" = "late_fi",
                   "a" = "latewd_fi")
  scar <- list("U" = "unknown_fs",
                   "D" = "dormant_fs",
                   "E" = "early_fs",
                   "M" = "middle_fs",
                   "L" = "late_fs",
                   "A" = "latewd_fs")
  ends <- list("[" = "pith_year",
               "]" = "bark_year",
               "{" = "inner_year",
               "}" = "outer_year")
  if (injury_event) {
    recorder <- c(recorder, injury)
  }
  rec <- subset(x, x$rec_type %in% recorder)$year
  inj <- subset(x, x$rec_type %in% injury)$year
  end <- subset(x, x$rec_type %in% ends)$year
  inj_dif <- diff(inj)
  active <- c(rec, intersect(rec - 1, end), intersect(rec + 1, end))
  active <- c(active, intersect(active - 1, inj), intersect(active + 1, inj)) # Really only need when injury_event = FALSE.
  if (any(inj_dif == 1) & !injury_event) {
    for (i in which(inj_dif == 1)) {
      if (inj_dif[i] %in% active) {
        active <- c(inj_dif[i + 1], active)
      }
    }
  }
  data.frame(recording = union(rec, active))
}


#' Count the number of recording series for each year in an fhx object.
#'
#' @param x An fhx object.
#' @param injury_event Boolean indicating whether injuries should be considered event. Default is FALSE.
#'
#' @return A dataframe with a columns giving the year and corresponding number of recording events for that year.
#'
#' @examples
#' data(lgr2)
#' yearly_recording(lgr2)
#'
#' @export
yearly_recording <- function(x, injury_event=FALSE) {
  as.data.frame(table(year = plyr::ddply(x, 'series',
                                         find_recording,
                                         injury_event = injury_event)$recording))
}

#' Composite fire events in fhx object returning composited object with prominent fires.
#'
#' @param x An fhx instance.
#' @param filter_prop The proportion of fire events to recording series needed in order to be considered. Default is 0.25.
#' @param filter_min The minimum number of recording series needed to be considered a fire event. Default is 2 recording series.
#' @param injury_event Boolean indicating whether injuries should be considered events. Default is FALSE.
#' @param comp_name Character vector of the series name for the returned fhx object composite series. Default is 'COMP'.
#'
#' @return An fhx object representing the composited series.
#'
#' @examples
#' data(lgr2)
#' composite(lgr2)
#'
#' # Use with composite to get composite years:
#' comp <- composite(pgm, comp_name = 'pgm')
#' event_yrs <- get_event_years(comp)[['pgm']]
#' print(event_yrs)
#'
#' @export
composite <- function(x, filter_prop=0.25, filter_min=2, injury_event=FALSE, comp_name='COMP') {
  stopifnot(is.fhx(x))
  injury <- list("u" = "unknown_fi",
                 "d" = "dormant_fi",
                 "e" = "early_fi",
                 "m" = "middle_fi",
                 "l" = "late_fi",
                 "a" = "latewd_fi")
  scar <- list("U" = "unknown_fs",
               "D" = "dormant_fs",
               "E" = "early_fs",
               "M" = "middle_fs",
               "L" = "late_fs",
               "A" = "latewd_fs")
  ends <- list("[" = "pith_year",
               "]" = "bark_year",
               "{" = "inner_year",
               "}" = "outer_year")
  event <- scar
  if (injury_event) {
    event <- c(event, injury)
  }
  event_count <- as.data.frame(table(year = subset(x, x$rec_type %in% event)$year))
  recording_count <- yearly_recording(x, injury_event = injury_event)
  # `Var1` in the _count data.frames is the year, `Freq` is the count.
  counts <- merge(event_count, recording_count,
                  by = 'year', suffixes = c('_event', '_recording'))
  counts$prop <- counts$Freq_event / counts$Freq_recording
  filter_mask <- (counts$prop >= filter_prop) & (counts$Freq_recording >= filter_min)
  out <- subset(counts, filter_mask)$year
  composite_event_years <- as.integer(levels(out)[out])
  # Make composite events unknown firescars.
  out_year <- composite_event_years
  out_rec_type <- rep("unknown_fs", length(composite_event_years))
  # Make first year in x the inner year.
  out_year <- c(out_year, min(x$year))
  out_rec_type <- c(out_rec_type, "inner_year")
  # Make last year in x the outer year.
  out_year <- c(out_year, max(x$year))
  out_rec_type <- c(out_rec_type, "outer_year")
  # Make all years after the first event 'recording'.
  new_recording <- setdiff(seq(min(composite_event_years), max(x$year)),
                           out_year)
  out_year <- c(out_year, new_recording)
  out_rec_type <- c(out_rec_type, rep("recorder_year", length(new_recording)))
  out_series <- factor(rep(comp_name, length(out_year)))
  out_rec_type <- factor(out_rec_type,
                        levels = c("null_year", "recorder_year", "unknown_fs",
                                   "unknown_fi", "dormant_fs", "dormant_fi",
                                   "early_fs", "early_fi", "middle_fs",
                                   "middle_fi", "late_fs", "late_fi",
                                   "latewd_fs", "latewd_fi", "pith_year",
                                   "bark_year", "inner_year", "outer_year",
                                   "estimate"))
  fhx(year = out_year, series = out_series, rec_type = out_rec_type)
}

#' Sort the series names of fhx object by the earliest or latest year.
#'
#' @param x An fhx instance to be sorted.
#' @param sort_by Designate the inner or outer year for sorting. Defaults to "first_year"
#' @param decreasing Logical. Decreasing sorting? Defaults to FALSE.
#' @param ... Additional arguments that fall off the face of the universe.
#'
#' @return A copy of \code{x} with reordered series.
#'
#' @examples
#' data(lgr2)
#' plot(sort(lgr2, decreasing = TRUE))
#' plot(sort(lgr2, sort_by = "last_year"))
#'
#' @export
sort.fhx <- function(x, decreasing=FALSE, sort_by = c('first_year', 'last_year'), ...) {
  stopifnot(is.fhx(x))

  if (is.null(sort_by)) sort.order <- min
  if (sort_by == "first_year") sort.order <- min
  if (sort_by == "last_year") sort.order <- max
  if (length(unique(x$series)) == 1) {
    return(x)
  }
  series_minyears <- stats::aggregate(year ~ series, x, sort.order)
  i <- order(series_minyears$year, decreasing = decreasing)
  x$series <- factor(x$series,
                     levels = series_minyears$series[i],
                     ordered = TRUE)
  x
}

#' Concatenate or combine two fhx objects.
#'
#' @param a An fhx object.
#' @param b The fhx object to be append.
#'
#' @return An fhx object with the series from \code{a} and \code{b}.
#'
#' @examples
#' data(lgr2)
#' data(pgm)
#' plot(lgr2 + pgm)
#'
#' @export
"+.fhx" <- function(a, b) {
  stopifnot('fhx' %in% class(a))
  stopifnot('fhx' %in% class(b))
  f <- rbind(a, b)
  check_duplicates(f)
}

#' Check if object is fhx.
#'
#' @param x Any R object.
#'
#' @return Boolean indicating whether `x` is an fhx object.
#'
#' @examples
#' data(lgr2)
#' is.fhx(lgr2)
#'
#' @export
is.fhx <- function(x) {
  inherits(x, 'fhx')
}

#' Check for duplicate observations in an fhx object.
#'
#' @param x An fhx object.
#'
#' @return A \code{x} or stop() is thrown.
#'
#' @examples
#' data(lgr2)
#' data(pgm)
#' burnr:::check_duplicates(lgr2 + pgm)
#'
check_duplicates <- function(x) {
  stopifnot(is.fhx(x))
  if (!anyDuplicated(x)) {
    return(x)
  } else {
      duplicates <- x[duplicated(x), ]
      print(duplicates)
      stop(c(dim(duplicates)[1],
           " duplicate(s) found. Please resolve duplicate records."))
  }
}
