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
fhx <- function(year,  series, rec_type, metalist=list()){
  if (!is.numeric(year)) stop("year must be numeric")
  if (!is.factor(series)) stop("series must be character")
  if (!is.factor(rec_type)) stop("rec_type must be factor")
  if (!is.list(metalist)) stop("metalist must be list")
  ringsdf = data.frame(year = year, series = series, rec_type = rec_type)
  structure(list(meta = metalist, rings = ringsdf), class = "fhx")
}

#' Print an \code{fhx} object.
#'
#' @param x An \code{fhx} object.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @export
print.fhx <- function(x, ...) {
  stopifnot(class(x) == 'fhx')
  print(x$rings)
}

#' Get \code{fhx} series names.
#'
#' @param x An fhx object.
#'
#' @return A character vector or \code{NULL}.
#'
#' @export
series_names <- function(x) {
  stopifnot(class(x) == 'fhx')
  as.character(unique(x$rings$series))
}

#' Extract fhx observations for given years.
#'
#' @param x An fhx object.
#' @param yr Integer vector of year(s) you would like extracted from x.
#'
#' @return A dataframe with extracted observations.
#'
#' @export
get_year <- function(x, yr) {
  stopifnot(class(x) == 'fhx')
  stopifnot(is.numeric(yr))
  subset(x$rings, year %in% yr)
}

#' Extract fhx observations for given series.
#'
#' @param x An fhx object.
#' @param s Character vector of years you would like extracted from x.
#'
#' @return A dataframe with extracted observations.
#'
#' @export
get_series <- function(x, s) {
  stopifnot(class(x) == 'fhx')
  stopifnot(is.character(s))
  subset(x$rings, series %in% s)
}

#' Remove series or year(s) from an fhx object and return.
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
#' @export
delete <- function(x, s, yr) {
  # Hint: It's just an inverse subset.
  stopifnot(class(x) == 'fhx')
  out <- c()
  # I'm sure there is a more clever way to handle this.
  if (missing(s)) {
    out <- subset(x$rings, !(year %in% yr))
  } else if (missing(yr)) {
    out <- subset(x$rings, !(series %in% s))
  } else if (!missing(yr) & !missing(s)) {
    out <- subset(x$rings, !((series %in% s) & (year %in% yr)))
  } else {
    out <- x$rings
  }
  fhx(out$year, out$series, out$rec_type)
}

#' Subset fhx observations for given years or series.
#'
#' @param x An fhx object.
#' @param subset Character vector of series, or integer vector of years to extract from x.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return An fhx object with extracted observations.
#'
#' @export
subset.fhx <- function(x, subset, ...) {
  stopifnot(class(x) == 'fhx')
  out <- c()
  if (is.character(subset)) {
    out <- get_series(x, subset)
  } else if (is.numeric(subset)) {
    out <- get_year(x, subset)
  } else {
    stop('`subset` was not numeric or character vector')
  }
  fhx(out$year, out$series, out$rec_type)
}

#' Composite fire events in x returning years with prominent fires.
#'
#' @param x An fhx instance.
#' @param filter_prop The proportion of fire events to recording series needed in order to be considered. Default is 0.25.
#' @param filter_min The minimum number of recording series needed to be considered a fire event. Default is 2 recording series.
#'
#' @return A vector of years from x.
#'
#' @export
composite <- function(x, filter_prop=0.25, filter_min=2) {
  stopifnot(class(x) == "fhx")
  recording <- list("|" = "recorder_year",
                   "U" = "unknown_fs",
                   "u" = "unknown_fi",
                   "D" = "dormant_fs",
                   "d" = "dormant_fi",
                   "E" = "early_fs",
                   "e" = "early_fi",
                   "M" = "middle_fs",
                   "m" = "middle_fi",
                   "L" = "late_fs",
                   "l" = "late_fi",
                   "A" = "latewd_fs",
                   "a" = "latewd_fi")
  event <- list("U" = "unknown_fs",
               "u" = "unknown_fi",
               "D" = "dormant_fs",
               "d" = "dormant_fi",
               "E" = "early_fs",
               "e" = "early_fi",
               "M" = "middle_fs",
               "m" = "middle_fi",
               "L" = "late_fs",
               "l" = "late_fi",
               "A" = "latewd_fs",
               "a" = "latewd_fi")
  event_count <- as.data.frame(table(subset(x$rings, x$rings$rec_type %in% event)$year))
  recording_count <- as.data.frame(table(subset(x$rings, x$rings$rec_type %in% recording)$year))
  # `Var1` in the _count data.frames is the year, `Freq` is the count.
  counts <- merge(event_count, recording_count, by = "Var1", suffixes = c('_event', '_recording'))
  counts$prop <- counts$Freq_event / counts$Freq_recording
  filter_mask <- (counts$prop >= filter_prop) & (counts$Freq_event >= filter_min)
  out <- subset(counts, filter_mask)$Var1
  as.integer(levels(out)[out])
}

#' Resort the series names of an fhx instance.
#'
#' @param x An fhx instance to be reorder.
#' @param decreasing Logical. Decreasing sorting? Defaults to FALSE.
#' @param ... Additional arguments that fall off the face of the universe.
#'
#' @return A copy of \code{x} with reordered series.
#'
#' @export
sort.fhx <- function(x, decreasing=FALSE, ...) {
  stopifnot(class(x) == "fhx")
  if (length(names(x$rings)) > 1) {
    return(x)
  }
  #test <- subset(x$rings,
                 #x$rings$rec_type == "inner_year" | x$ring$rec_type == "pith_year")
  #i <- order(test$year, decreasing = TRUE)
  series_minyears <- aggregate(year ~ series, x$rings, min) 
  i <- order(series_minyears$year, decreasing = TRUE)
  x$rings$series <- factor(x$rings$series,
                           #levels = series_levels,
                           levels = series_minyears$series[i],
                           ordered = TRUE)
  i <- order(x$rings$series, x$rings$year, decreasing = decreasing)
  x$rings <- x$rings[i, ]
  x
}

#' Combine two fhx objects.
#'
#' @param a An fhx instance.
#' @param b The fhx instance to be appended.
#'
#' @return An fhx instance with the information from \code{a} and \code{b}. Duplicates are resolved with \code{fire::resolve_duplicates()}.
#'
#' @export
combine <- function(a, b) {
  stopifnot(class(a) == "fhx")
  stopifnot(class(b) == "fhx")
  f <- list(meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$rings <- rbind(a$rings, b$rings)
  if (length(a$meta) | length(b$meta) > 0)  # If meta data present...
    f$meta <- c(a$meta, b$meta)
  sort(resolve_duplicates(f))
}

#' Merge/remove duplicate observations in an fhx object.
#'
#' @param x An fhx instance.
#'
#' @return A copy of \code{x} with duplicates removed.
#'
#' @export
resolve_duplicates <- function(x) {
  stopifnot(class(x) == "fhx")
  if (!anyDuplicated(x$rings)) {
    return(x)
  } else {
      duplicates <- x$rings[duplicated(x$rings), ]
      print(duplicates)
      stop(c(dim(duplicates)[1],
           " duplicate(s) found. Please resolve duplicate records."))
  }
}
