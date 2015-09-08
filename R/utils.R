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

#' Filter fire events in `x` returning years with prominent fires.
#'
#' @param x An fhx instance.
#' @param filter.prop The proportion of fire events to recording series needed in order to be considered. Default is 0.25.
#' @param filter.min The minimum number of recording series needed to be considered a fire event. Default is 2 recording series.
#' @return A vector of years from `x`.
rug.filter <- function(x, filter.prop=0.25, filter.min=2) {
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
  event.count <- as.data.frame(table(subset(x$rings, x$rings$type %in% event)$year))
  recording.count <- as.data.frame(table(subset(x$rings, x$rings$type %in% recording)$year))
  counts <- merge(event.count, recording.count, by = "Var1")
  counts$prop <- counts$Freq.x / counts$Freq.y
  conditions <- (counts$prop >= filter.prop) & (counts$Freq.x >= filter.min)
  out <- subset(counts, conditions)$Var1
  as.integer(levels(out)[out])
}

#' Reorder the series names of an fhx instance.
#'
#' @param x An fhx instance to be reorder.
#' @return A copy of \code{x} with reordered series.
order.fhx <- function(x) {
  stopifnot(class(x) == "fhx")
  test <- subset(x$rings,
                 x$rings$type == "inner.year" | x$ring$type == "pith.year")
  i <- order(test$year, decreasing = TRUE)
  x$rings$series <- factor(x$rings$series,
                           levels = unique(test$series[i]),
                           ordered = TRUE)
  i <- order(x$rings$series, x$rings$year, decreasing = TRUE)
  x$rings <- x$rings[i, ]
  x
}

#' Concatenate two fhx instance.
#'
#' @param a An fhx instance.
#' @param b The fhx instance to be appended.
#' @return An fhx instance with the information from \code{a} and \code{b}. Duplicates are resolved with \code{fire::resolve_duplicates()}.
"+.fhx" <- function(a, b) {
  stopifnot(class(b) == "fhx")
  f <- list(meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$rings <- rbind(a$rings, b$rings)
  if (length(a$meta) | length(b$meta) > 0)  # If meta data present...
    f$meta <- c(a$meta, b$meta)
  order.fhx(resolve_duplicates(f))
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