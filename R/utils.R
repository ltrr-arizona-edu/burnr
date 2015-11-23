#' Constructor for S3 fhx class.
#'
#' @param year A numeric vector of observation years for each \code{series} and \code{type} argument.
#' @param series A factor of series names for each \code{year} and \code{type} argument.
#' @param type A factor of ring types for each element in \code{year} and \code{series}.
#' @param metalist An option list of arbitrary metadata to be included in the fhx instance.
#' @return An fhx instance.
fhx <- function(year,  series, type, metalist=list()){
  if (!is.numeric(year)) stop("year must be numeric")
  if (!is.factor(series)) stop("series must be character")
  if (!is.factor(type)) stop("type must be factor")
  if (!is.list(metalist)) stop("metalist must be list")
  ringsdf = data.frame(year = year, series = series, type = type)
  structure(list(meta = metalist, rings = ringsdf), class = "fhx")
}

#' Composite fire events in `x` returning years with prominent fires.
#'
#' @param x An fhx instance.
#' @param filter_prop The proportion of fire events to recording series needed in order to be considered. Default is 0.25.
#' @param filter_min The minimum number of recording series needed to be considered a fire event. Default is 2 recording series.
#' @return A vector of years from `x`.
composite <- function(x, filter_prop=0.25, filter_min=2) {
  stopifnot(class(x) == "fhx")
  recording <- list("|" = "recorder.year",
                   "U" = "unknown.fs",
                   "u" = "unknown.fi",
                   "D" = "dormant.fs",
                   "d" = "dormant.fi",
                   "E" = "early.fs",
                   "e" = "early.fi",
                   "M" = "middle.fs",
                   "m" = "middle.fi",
                   "L" = "late.fs",
                   "l" = "late.fi",
                   "A" = "latewd.fs",
                   "a" = "latewd.fi")
  event <- list("U" = "unknown.fs",
               "u" = "unknown.fi",
               "D" = "dormant.fs",
               "d" = "dormant.fi",
               "E" = "early.fs",
               "e" = "early.fi",
               "M" = "middle.fs",
               "m" = "middle.fi",
               "L" = "late.fs",
               "l" = "late.fi",
               "A" = "latewd.fs",
               "a" = "latewd.fi")
  event_count <- as.data.frame(table(subset(x$rings, x$rings$type %in% event)$year))
  recording_count <- as.data.frame(table(subset(x$rings, x$rings$type %in% recording)$year))
  counts <- merge(event_count, recording_count, by = "Var1")
  counts$prop <- counts$Freq.x / counts$Freq.y
  conditions <- (counts$prop >= filter_prop) & (counts$Freq.x >= filter_min)
  out <- subset(counts, conditions)$Var1
  as.integer(levels(out)[out])
}

#' Resort the series names of an fhx instance.
#'
#' @param x An fhx instance to be reorder.
#' @param decreasing Logical. Decreasing sorting? Defaults to FALSE.
#' @param ... Additional arguments that fall off the face of the universe.
#' @return A copy of \code{x} with reordered series.
sort.fhx <- function(x, decreasing=FALSE, ...) {
  stopifnot(class(x) == "fhx")
  if (length(names(x$rings)) > 1) {
    return(x)
  }
  #test <- subset(x$rings,
                 #x$rings$type == "inner.year" | x$ring$type == "pith.year")
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
#' @return An fhx instance with the information from \code{a} and \code{b}. Duplicates are resolved with \code{fire::resolve_duplicates()}.
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
#' @return A copy of \code{x} with duplicates removed.
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
