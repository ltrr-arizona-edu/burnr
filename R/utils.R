#' Constructor for `fhx` objects
#'
#' @param year An n-length numeric vector of observation years.
#' @param series An n-length factor or character vector of observation series
#'   names.
#' @param rec_type An n-length factor or character vector denoting the record
#'   type for each observations. Note that this needs to use a controlled
#'   vocabulary, see `burnr:::rec_type_all` for all possible values.
#'
#' @return An `fhx` object. `fhx` are S3 objects; specialized data frames with 3
#' columns:
#'   * "year": An n-length numeric vector. The year of an observation.
#'   * "series": An n-length factor. Giving the series name for each
#'     observation.
#'   * "rec_type": An n-length factor with controlled vocabulary and levels.
#'     This records the type of ring or record of each observation.
#'
#' @details
#' Note that 'year', 'series', and 'rec_type' are pass through [as.numeric()],
#' [as.factor()], and [make_rec_type()] the `fhx` object is created.
#'
#' @examples
#' x <- fhx(
#'   year = c(1900, 1954, 1996),
#'   series = rep("tree1", 3),
#'   rec_type = c("pith_year", "unknown_fs", "bark_year")
#' )
#' print(x)
#'
#' @seealso
#'   * [as_fhx()] casts data frame-like object into `fhx` object.
#'   * [sort.fhx()] sort an `fhx` object.
#'   * [is_fhx()] test whether object is `fhx`.
#'   * [+.fhx()] concatenate multiple `fhx` objects together.
#'   * [make_rec_type()] helpful to convert `rec_type`-like character vectors to
#'     full facors with proper levels.
#'   * [read_fhx()] Read FHX2 files.
#'   * [write_fhx()] Write FHX2 files.
#'   * [plot_demograph()] makes demography plots of `fhx` objects.
#'   * [series_stats()] basic common statistical summaries of `fhx` objects.
#'   * [composite()] create fire composites from `fhx` objects.
#'   * [intervals()] fire interval analysis.
#'   * [sea()] superposed epoch analysis.
#'
#' @export
fhx <- function(year, series, rec_type) {
  ringsdf <- data.frame(
    year = as.numeric(year),
    series = as.factor(series),
    rec_type = make_rec_type(rec_type)
  )
  class(ringsdf) <- c("fhx", "data.frame")
  ringsdf
}


#' Turn character vector into factor with proper `fhx` levels
#'
#' @param x A character vector or factor containing one or more rec_type-like
#'   strings. This uses a controlled vocabulary, see `burnr:::rec_type_all`
#'   for list of all possible rec_type values.
#'
#' @return A factor with appropriate `fhx` levels.
#'
#' @seealso
#'   * [fhx()] constructs an `fhx` object.
#'   * [as_fhx()] casts data frame-like objects into `fhx` objects.
#'
#' @examples
#' make_rec_type("null_year")
#'
#' make_rec_type(c("null_year", "late_fs"))
#'
#' @export
make_rec_type <- function(x) {
  possible_levels <- rec_type_all  # nolint
   # TODO(brews): This v could be make into a more clear error.
  stopifnot(x %in% possible_levels)
  factor(x, levels = possible_levels)
}


#' Get years with events for an `fhx` object
#'
#' @param x An `fhx` object.
#' @param scar_event Boolean indicating whether years with scar events should be
#'   returned. Default is `TRUE`.
#' @param injury_event Boolean indicating whether years with injury events
#'   should be returned. Default is `FALSE`.
#' @param custom_grep_str Character string to pass a custom grep search pattern
#'   to search `x` "rec_type" column for. `NULL` by default.
#'
#' @return A list. Elements of the list are numeric vectors giving the years
#'   with events for each `fhx` series. Each element's name reflects the series'
#'   name.
#'
#' @seealso
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [year_range()] get earliest and latest year in an `fhx` object.
#'   * [get_year()] subset an `fhx` object to select years.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'   * [count_event_position()] count the number of different events in an `fhx`
#'     object.
#'   * [yearly_recording()] count the number of "recording" events in each year of
#'     an `fhx` object.
#'   * [series_stats()] basic summary stats for an `fhx` object.
#'
#' @examples
#' data(pgm)
#' get_event_years(pgm, scar_event = TRUE, injury_event = TRUE)
#'
#' # Passing a custom string to grep. This one identified recorder years:
#' get_event_years(pgm, custom_grep_str = "recorder_")
#'
#' # Use with composite to get composite years:
#' comp <- composite(pgm, comp_name = "pgm")
#' event_yrs <- get_event_years(comp)[["pgm"]]
#' print(event_yrs)
#'
#' @export
get_event_years <- function(x, scar_event = TRUE, injury_event = FALSE,
                            custom_grep_str = NULL) {
  stopifnot(is_fhx(x))
  if (!is.null(custom_grep_str)) {
    message(
      "burnr::get_events(): custom_search_str was defined, ",
      "ignoring scar_event and injury_event arguments")
  }
  # Build our search string.
  search_str <- NA
  if (is.null(custom_grep_str)) {
    search_parts <- c()
    if (scar_event) {
      search_parts <- c(search_parts, "_fs")
    }
    if (injury_event) {
      search_parts <- c(search_parts, "_fi")
    }
    if (length(search_parts) > 1) {
      search_str <- paste(search_parts, collapse = "|")
    } else {
      search_str <- search_parts
    }
  } else {
    search_str <- custom_grep_str
  }
  plyr::dlply(x, c("series"), function(a) a$year[grepl(search_str, a$rec_type)])
}


#' Range of years in an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return A numeric vector or `NULL`.
#'
#' @seealso
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [get_year()] subset an `fhx` object to select years.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'   * [count_event_position()] count the number of different events in an `fhx`
#'     object.
#'   * [yearly_recording()] count the number of "recording" events in each year of
#'     an `fhx` object.
#'   * [series_stats()] basic summary stats for an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' year_range(lgr2)
#'
#' @export
year_range <- function(x) {
  stopifnot(is_fhx(x))
  range(x$year)
}


#' Get `fhx` series names
#'
#' @param x An `fhx` object.
#'
#' @return A character vector or `NULL`.
#'
#' @seealso
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [get_year()] subset an `fhx` object to select years.
#'   * [year_range()] get earliest and latest year in an `fhx` object.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'   * [count_event_position()] count the number of different events in an `fhx`
#'     object.
#'   * [yearly_recording()] count the number of "recording" events in each year of
#'     an `fhx` object.
#'   * [series_stats()] basic summary stats for an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' series_names(lgr2)
#'
#' @export
series_names <- function(x) {
  stopifnot(is_fhx(x))
  as.character(unique(x$series))
}


#' Extract `fhx` observations for given years
#'
#' @param x An `fhx` object.
#' @param yr Numeric vector of year(s) to extract from `x`.
#'
#' @return An `fhx` object.
#'
#' @seealso
#'   * [year_range()] get earliest and latest year in an `fhx` object.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [delete()] remove observations from an `fhx` object.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' get_year(lgr2, 1806)
#'
#' get_year(lgr2, 1805:1807)
#'
#' @export
get_year <- function(x, yr) {
  stopifnot(is_fhx(x))
  stopifnot(is.numeric(yr))
  subset(x, x$year %in% yr)
}


#' Extract `fhx` observations for given series
#'
#' @param x An `fhx` object.
#' @param s Character vector of series to extract from `x`.
#'
#' @return An `fhx` object.
#'
#' @seealso
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [get_year()] subset an `fhx` object to select years
#'   * [delete()] remove observations from an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' get_series(lgr2, "LGR46")
#'
#' get_series(lgr2, c("LGR41", "LGR46"))
#'
#' @export
get_series <- function(x, s) {
  stopifnot(is_fhx(x))
  stopifnot(is.character(s))
  subset(x, x$series %in% s)
}


#' Remove series or years from an `fhx` object
#'
#' @param x An `fhx` object.
#' @param s Character vector of series to remove from `x`.
#' @param yr Integer vector of years to remove from `x`.
#'
#' @return An fhx `object` with observations removed.
#'
#' @details
#' You can combine `s` and `yr` to specify years within select series to remove.
#'
#' @seealso
#'   * [fhx()] constructs an `fhx` object.
#'   * [as_fhx()] casts data frame-like object into an `fhx` object.
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [year_range()] get earliest and latest year in an `fhx` object.
#'   * [get_year()] subset an `fhx` object to select years.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' plot(delete(lgr2, s = "LGR46"))
#'
#' plot(delete(lgr2, yr = 1300:1550))
#'
#' @export
delete <- function(x, s, yr) {
  # Hint: It's just an inverse subset.
  stopifnot(is_fhx(x))
  out <- c()
  # I'm sure there is a more clever way to handle this.
  if (missing(s)) {
    out <- subset(x, !(x$year %in% yr))
  } else if (missing(yr)) {
    out <- subset(x, !(x$series %in% s))
  } else if (!missing(yr) & !missing(s)) {
    out <- subset(x, !( (x$series %in% s) & (x$year %in% yr) ))
  } else {
    out <- x
  }
  fhx(out$year, out$series, out$rec_type)
}


#' Find years that are considered "recording" in an `fhx` object
#'
#' @param x An `fhx` object. This generally should only contain one series, but
#'    we do not check for this.
#' @param injury_event Boolean indicating whether injuries should be considered
#'   event. Default is `FALSE`.
#'
#' @return A data frame with column "recording" indicating years which are
#' "recording".
#'
#' @examples
#' require(plyr)
#' data(lgr2)
#' ddply(lgr2$rings, "series", burnr:::find_recording, injury_event = TRUE)
#'
#' @noRd
find_recording <- function(x, injury_event=FALSE) {
  # Use with: ddply(lgr2$rings, 'series', recorder_finder)
  x <- x[order(x$year), ]

  recorder <- rec_type_recorder  # nolint
  injury <- rec_type_injury  # nolint
  ends <- rec_type_ends  # nolint

  if (injury_event) {
    recorder <- c(recorder, injury)
  }

  rec <- subset(x, x$rec_type %in% recorder)$year
  inj <- subset(x, x$rec_type %in% injury)$year
  end <- subset(x, x$rec_type %in% ends)$year

  inj_dif <- diff(inj)

  # "ends" and "injuries" only record when there is recording event in adj year
  active <- c(rec, intersect(rec - 1, end), intersect(rec + 1, end))
  # Really only need below when injury_event = FALSE.
  active <- c(active, intersect(active - 1, inj), intersect(active + 1, inj))

  # recording-ness is communicated through injury events
  if (any(inj_dif == 1) & !injury_event) {
    for (i in which(inj_dif == 1)) {
      if (inj_dif[i] %in% active) {
        active <- c(inj_dif[i + 1], active)
      }
    }
  }

  data.frame(recording = union(rec, active))
}


#' Count different events in an `fhx` object
#'
#' @param x An `fhx` object.
#' @param injury_event Optional boolean indicating whether injuries should be
#'   considered an "event". Default is `FALSE`.
#' @param position Optional character vector giving the types of event positions
#'   to include in the count. Can be any combination of the following:
#'   * "unknown"
#'   * "dormant"
#'   * "early"
#'   * "middle"
#'   * "late"
#'   * "latewd"
#'
#'   The default counts all types of event positions.
#' @param groupby Optional named list containing character vectors that are used
#'   to count the total number of different event types. The names given to each
#'   character vector give the group's name in the output data frame.
#'
#' @return A data frame with a columns giving the event or event group and
#' values giving the corresponding count for each event type or group.
#'
#' @seealso
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'   * [yearly_recording()] count the number of "recording" events in each year of
#'     an `fhx` object.
#'   * [series_stats()] basic summary stats for an `fhx` object.
#'
#' @examples
#' data(pgm)
#' count_event_position(pgm)
#'
#' # As above, but considering injuries to be a type of event.
#' count_event_position(pgm, injury_event = TRUE)
#'
#' # Count only events of a certain position, in this case, "unknown", "early",
#' # and "middle".
#' count_event_position(pgm,
#'   injury_event = TRUE,
#'   position = c("unknown", "early", "middle")
#' )
#'
#' # Using custom "groupby" args.
#' grplist <- list(
#'   foo = c("dormant_fs", "early_fs"),
#'   bar = c("middle_fs", "late_fs")
#' )
#' count_event_position(pgm, groupby = grplist)
#'
#' @export
count_event_position <- function(x, injury_event = FALSE, position, groupby) {
  stopifnot(is_fhx(x))

  possible_position <- c(
    "unknown", "dormant", "early", "middle", "late", "latewd"
  )
  if (missing(position)) {
    position <- possible_position
  }
  stopifnot(all(position %in% possible_position))

  target_events <- paste0(position, "_fs")
  if (injury_event == TRUE) {
    target_events <- c(target_events, paste0(position, "_fi"))
  }

  msk <- x$rec_type %in% target_events

  out <- plyr::count(x$rec_type[msk])
  names(out) <- c("event", "count")

  if (!missing(groupby)) {

    outgroup <- plyr::ldply(groupby,
      function(g) sum(subset(out, out$event %in% g)$count)
    )

    names(outgroup) <- c("event", "count")
    out <- rbind(out, outgroup)
  }

  out
}


#' Count the number of recording series for each year in an `fhx` object
#'
#' @param x An `fhx` object.
#' @param injury_event Boolean indicating whether injuries should be considered
#'   events. Default is `FALSE`.
#'
#' @return A data frame with columns giving the year and recording events count.
#'
#' @examples
#' data(lgr2)
#' yearly_recording(lgr2)
#'
#' @export
yearly_recording <- function(x, injury_event = FALSE) {
  out <- as.data.frame(
    table(
      year = plyr::ddply(x, "series", find_recording, 
        injury_event = injury_event
      )$recording
    ),
    stringsAsFactors=FALSE
  )
  out$year <- as.numeric(out$year)
  out
}


#' Composite fire events in fhx object
#'
#' @param x An `fhx` object.
#' @param filter_prop The minimum proportion of fire events in recording series
#'   needed for fire event to be considered for composite. Default is 0.25.
#' @param filter_min_rec The minimum number of recording series needed for a
#'   fire event to be considered for the composite. Default is 2 recording
#'   series.
#' @param filter_min_events The minimum number of fire scars needed for a fire
#'   event to be considered for the composite. Default is 1. Fire injuries are
#'   included in this count if `injury_event`  is `TRUE`.
#' @param injury_event Boolean indicating whether injuries should be considered
#'   events. Default is `FALSE`.
#' @param comp_name Character vector of the series name for the returned `fhx`
#'   object composite series. Default is 'COMP'.
#'
#' @return An `fhx` object representing the composited series. The object will
#'   be empty if there are nocomposite-worthy events.
#'
#' @seealso
#'   * [intervals()] fire interval analysis from an `fhx` composite.
#'   * [sea()] superposed epoch analysis.
#'   * [series_stats()] basic summary stats for an `fhx` object.
#'   * [get_event_years()] gets years for various events in an `fhx` object.
#'   * [count_event_position()] count the number of different events in an `fhx`
#'     object.
#'   * [yearly_recording()] count the number of "recording" events in each year of
#'     an `fhx` object.
#'   * [fhx()] constructs an `fhx` object.
#'   * [as_fhx()] casts data frame-like object into an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' composite(lgr2)
#'
#' # Use with composite to get composite years:
#' comp <- composite(pgm, comp_name = "pgm")
#' event_yrs <- get_event_years(comp)[["pgm"]]
#' print(event_yrs)
#'
#' @export
composite <- function(x, filter_prop = 0.25, filter_min_rec = 2,
                      filter_min_events = 1, injury_event = FALSE,
                      comp_name = "COMP") {
  stopifnot(is_fhx(x))

  injury <- rec_type_injury  # nolint
  scar <- rec_type_scar  # nolint

  event <- scar
  if (injury_event) {
    event <- c(event, injury)
  }

  event_year <- subset(x, x$rec_type %in% event)$year
  if (length(event_year) < 1) {
    return(fhx(as.numeric(c()), as.factor(c()), make_rec_type(c())))
  } else {
    event_count <- as.data.frame(
      table(year = event_year)
    )
  }

  recording_count <- yearly_recording(x, injury_event = injury_event)
  # `Var1` in the _count data.frames is the year, `Freq` is the count.
  counts <- merge(event_count, recording_count,
    by = "year", suffixes = c("_event", "_recording")
  )
  counts$prop <- counts$Freq_event / counts$Freq_recording

  filter_mask <- (
    (counts$prop >= filter_prop)
    & (counts$Freq_recording >= filter_min_rec)
    & (counts$Freq_event >= filter_min_events)
  )

  out <- subset(counts, filter_mask)$year
  composite_event_years <- as.integer(levels(out)[out])

  if (length(composite_event_years) == 0) {
    return(fhx(as.numeric(c()), as.factor(c()), make_rec_type(c())))
  }

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
  new_recording <- setdiff(
    seq(min(composite_event_years), max(x$year)),
    out_year
  )
  out_year <- c(out_year, new_recording)
  out_rec_type <- c(out_rec_type, rep("recorder_year", length(new_recording)))
  out_series <- factor(rep(comp_name, length(out_year)))
  out_rec_type <- make_rec_type(out_rec_type)
  fhx(year = out_year, series = out_series, rec_type = out_rec_type)
}


#' Sort the series names of `fhx` object by the earliest or latest year
#'
#' @param x An `fhx` object to sort.
#' @param sort_by Either "first_year" or "last_year". Designates the inner or
#'   outer year for sorting. Defaults to "first_year"
#' @param decreasing Logical. Decreasing sorting? Defaults to `FALSE`.
#' @param ... Additional arguments that fall off the face of the universe.
#'
#' @return A copy of `x` with reordered series.
#'
#' @seealso
#'   * [fhx()] constructs an `fhx` object.
#'   * [as_fhx()] casts data frame-like object into an `fhx` object.
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [delete()] remove observations from an `fhx` object.
#'   * [+.fhx()] concatenate multiple `fhx` objects together.
#'
#' @examples
#' data(lgr2)
#' plot(sort(lgr2, decreasing = TRUE))
#' plot(sort(lgr2, sort_by = "last_year"))
#'
#' @export
sort.fhx <- function(x, decreasing = FALSE, sort_by = "first_year", ...) {
  stopifnot(is_fhx(x))
  stopifnot(sort_by %in% c("first_year", "last_year"))
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
    ordered = TRUE
  )
  x
}


#' Concatenate or combine two fhx objects
#'
#' @param a An `fhx` object.
#' @param b The `fhx` object to be append.
#'
#' @return An `fhx` object with the observations from `a` and `b`.
#'
#' @note Throws `stop()` if there are duplicate series names in `a` and `b`.
#'
#' @seealso
#'   * [series_names()] get all the series in an `fhx` object.
#'   * [get_series()] subset an `fhx` object to select series.
#'   * [delete()] remove observations from an `fhx` object.
#'   * [sort.fhx()] sort an `fhx` object.
#'
#' @examples
#' data(lgr2)
#' data(pgm)
#' plot(lgr2 + pgm)
#'
#' @export
"+.fhx" <- function(a, b) {
  stopifnot(is_fhx(a))
  stopifnot(is_fhx(b))
  f <- rbind(a, b)
  check_duplicates(f)
}


#' Check if object is `fhx`.
#'
#' @param x An object.
#'
#' @return Boolean indicating whether `x` is an `fhx` object.
#'
#' @seealso
#'   * [fhx()] constructs an `fhx` object.
#'   * [as_fhx()] casts data frame-like object into an `fhx` object.
#'   * [+.fhx()] concatenate multiple `fhx` objects together.
#'
#' @examples
#' data(lgr2)
#' is_fhx(lgr2)
#'
#' @export
is_fhx <- function(x) {
  inherits(x, "fhx")
}


#' Alias to [is_fhx()]
#'
#' @inherit is_fhx
#'
#' @export
is.fhx <- function(x) {
  is_fhx(x)
}


#' Cast data frame or list-like to `fhx` object
#'
#' @param x A data frame or list-like object to cast. Must have named elements
#'   for "year", "series", and "rec_type".
#'
#' @return `x` cast to an `fhx` object.
#'
#' @seealso
#'   * [fhx()] constructs an `fhx` object.
#'   * [is_fhx()] test whether object is `fhx`.
#'   * [make_rec_type()] helpful to convert `rec_type`-like character vectors to
#'     full facors with proper levels.
#'
#' @examples
#' data(lgr2)
#' example_dataframe <- as.data.frame(lgr2)
#' back_to_fhx <- as_fhx(example_dataframe)
#'
#' @export
as_fhx <- function(x) {
  if (!all(c("year", "series", "rec_type") %in% names(x))) {
    stop("`x` must have members 'year', 'series', and 'rec_type'")
  }

  fhx(x$year, x$series, x$rec_type)
}


#' Alias to [as_fhx()]
#'
#' @inherit as_fhx
#'
#' @export
as.fhx <- function(x) {
  as_fhx(x)
}


#' Check for duplicate observations in an `fhx` object
#'
#' @param x An `fhx` object.
#'
#' @return An `x`, otherwise `stop()` is thrown.
#'
#' @examples
#' data(lgr2)
#' data(pgm)
#' burnr:::check_duplicates(lgr2 + pgm)
#'
#' @noRd
check_duplicates <- function(x) {
  stopifnot(is_fhx(x))
  if (!anyDuplicated(x)) {
    return(invisible(x))
  } else {
    duplicates <- x[duplicated(x), ]
    stop(duplicates, "\n", c(
      dim(duplicates)[1],
      " duplicate(s) found. Please resolve duplicate records."
    ))
  }
}


#' Test if `fhx` object respects canon FHX2 format
#'
#' @param x An `fhx` object.
#'
#' @return Boolean. Does `x` violate the canon format?
#'
#' @details
#' Checks `x` "rec_type" to see if it uses experimental or non-canon events
#' that go against the vanilla FHX2 file format.
#'
#' @noRd
violates_canon <- function(x) {
  !all(x$rec_type %in% rec_type_canon)  # nolint
}
