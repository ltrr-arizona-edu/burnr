#' Calculate fire intervals from a `composite`
#'
#' @param comp A `composite` instance, usually output from [composite()].
#'    Should contain only one series.
#' @param densfun String giving desired distribution to fit. Either "weibull"
#'   or "lognormal". Default is "weibull".
#'
#' @return An `intervals` object. `intervals` have components:
#'   * "intervals" an integer vector giving the actual fire intervals.
#'   * "fitdistr" a `fitdistr` object from [MASS::fitdistr()] representing the
#'     density function fit.
#'   * "densfun" a string giving the name of the density function used.
#'   * "kstest" an `htest` object from [stats::ks.test()] giving the result of a
#'     one-sample Kolmogorov-Smirnov test.
#'   * "shapirotest" an `htest` object from [stats::shapiro.test()] giving the
#'     result of a Shapiro-Wilk normality test.
#'   * "comp_name" a string giving the name of the interval's input composite.
#'   * "event_range" an integer vector giving the year range (min, max) of
#'     events used to create this intervals.
#'
#' @seealso
#'   * [composite()] to create a `composite` object.
#'   * [mean.intervals()] gets mean fire interval.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [plot_intervals_dist()] plots `intervals`.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @examples
#' data(pgm)
#' interv <- intervals(composite(pgm))
#' print(interv)
#'
#' mean(interv) # Mean interval
#'
#' # Now fit log-normal distribution instead of Weibull.
#' intervals(composite(pgm), densfun = "lognormal")
#' \dontrun{
#' # Boxplot of fire interval distribution.
#' boxplot(intervals(composite(pgm))$intervals)
#' }
#'
#' @export
intervals <- function(comp, densfun = "weibull") {
  stopifnot(is_fhx(comp))
  stopifnot(densfun %in% c("weibull", "lognormal"))
  if (length(series_names(comp)) > 1) {
    stop("Found multiple series in `comp`. There can only be one.")
  }
  dens2cum <- list(weibull = stats::pweibull, lognormal = stats::plnorm)
  x <- list()
  x$intervals <- diff(get_event_years(comp)[[1]])
  if (length(x$intervals) < 2) {
    stop("Too few fire events to compute intervals")
  }
  class(x) <- c("intervals")
  x$fitdistr <- MASS::fitdistr(x$intervals, densfun)
  x$densfun <- densfun
  p_densfun <- dens2cum[[densfun]]
  kstest_args <- c(
    list(x = x$intervals, y = p_densfun),
    x$fitdistr$estimate
  )
  x$kstest <- do.call(stats::ks.test, kstest_args)
  x$shapirotest <- stats::shapiro.test(x$intervals)
  x$comp_name <- series_names(comp)
  x$event_range <- range(unlist(get_event_years(comp)))
  x
}


#' Check if object is fire `intervals`
#'
#' @param x An R object.
#'
#' @return Boolean indicating whether x is an `intervals` object.
#'
#' @seealso [intervals()] creates an `intervals` object.
#' @export
is_intervals <- function(x) inherits(x, "intervals")


#' Alias to [is_intervals()]
#'
#' @inherit is_intervals
#'
#' @export
is.intervals <- function(x) is_intervals(x)


#' Fire `intervals` arithmetic mean
#'
#' @param x An `intervals` object.
#' @param ... Additional arguments passed to [mean()].
#'
#' @return Numeric or NA.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @export
mean.intervals <- function(x, ...) {
  mean(x$intervals, ...)
}


#' Fire `intervals` median
#'
#' @param x An `intervals` object.
#' @param ... Additional arguments passed to [stats::median()].
#'
#' @return Numeric or NA.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [mean.intervals()] gets mean fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @importFrom stats median
#' @export
median.intervals <- function(x, ...) {
  median(x$intervals, ...)
}


#' Minimum interval in fire `intervals`
#'
#' @param x An `intervals` object.
#' @param ... Additional arguments passed to [min()].
#'
#' @return Numeric or NA.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [mean.intervals()] gets median fire interval.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @export
min.intervals <- function(x, ...) {
  min(x$intervals)
}


#' Maximum interval in fire `intervals`
#'
#' @param x An `intervals` object.
#' @param ... Additional arguments passed to [max()].
#'
#' @return Numeric or NA.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [mean.intervals()] gets median fire interval.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @export
max.intervals <- function(x, ...) {
  max(x$intervals)
}


#' Fit distribution quantiles to fire `intervals`
#'
#' @param x An `intervals` object.
#' @param q Vector giving the desired quantiles.
#' @param ... Additional arguments passed to the [quantile()] method for the
#'   fit distribution.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [mean.intervals()] gets median fire interval.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @examples
#' data(pgm)
#' intervs <- intervals(composite(pgm))
#' quantile(intervs)
#'
#' # Or you can pass in your own quantiles:
#' quantile(intervs, q = c(0.25, 0.5, 0.75))
#' @importFrom stats quantile
#' @export
quantile.intervals <- function(x, q = c(0.125, 0.5, 0.875), ...) {
  dens2cum <- list(weibull = stats::qweibull, lognormal = stats::qlnorm)
  q_densfun <- dens2cum[[x$densfun]]
  quant_args <- c(
    list(q, ...),
    x$fitdistr$estimate
  )
  quants <- do.call(q_densfun, quant_args)
  quants
}


#' Plot a fire `intervals` object
#'
#' @param ... Arguments passed to [plot_intervals_dist()].
#'
#' @seealso [plot_intervals_dist()] plot `intervals` distributions.
#'
#' @examples
#' data(pgm)
#' interv <- intervals(composite(pgm))
#'
#' plot(interv, binwidth = 5)
#' @export
plot.intervals <- function(...) {
  print(plot_intervals_dist(...))
}


#' Basic fire `intervals` distribution plot
#'
#' @param x An `intervals` object, from [intervals()].
#' @inheritParams ggplot2::geom_histogram
#'
#' @return A ggplot object.
#'
#' @seealso
#'   * [intervals()] to create a fire `intervals` object.
#'   * [mean.intervals()] gets mean fire interval.
#'   * [median.intervals()] gets median fire interval.
#'   * [quantile.intervals()] get fit distribution quantiles.
#'   * [min.intervals()] gives the minimum fire interval.
#'   * [max.intervals()] gives the maximum fire interval.
#'   * [print.intervals()] prints common fire-interval summary statistics.
#'
#' @export
plot_intervals_dist <- function(x, binwidth = NULL) {
  p <- ggplot2::ggplot(
    data.frame("intervals" = x[["intervals"]]),
    ggplot2::aes_string(x = "intervals")
  )
  if (is.null(binwidth)) {
    p <- (p + ggplot2::geom_histogram())
  } else {
    p <- (p + ggplot2::geom_histogram(binwidth = binwidth))
  }
  p <- (p + ggplot2::geom_rug() + ggplot2::theme_bw())
  p
}


#' Print a fire `intervals` object
#'
#' @param x An `intervals` object.
#' @param ...  Additional arguments that are tossed.
#'
#' @seealso [intervals()] to create a fire `intervals` object.
#'
#' @examples
#' data(pgm)
#' interv <- intervals(composite(pgm))
#' print(interv)
#'
#' # Note, you can also catch the printed table:
#' summary_stats <- print(interv)
#'
#' @export
print.intervals <- function(x, ...) {
  wfit <- MASS::fitdistr(x$intervals, "weibull")

  weib_invshape <- 1 / wfit$estimate["shape"]
  weibull_median <- wfit$estimate["scale"] * log(2)^weib_invshape  # nolint

  quants <- quantile(x, q = c(0.847, 0.5, 0.125))
  cat(strwrap("Interval Analysis", prefix = "\t"), sep = "\n")
  cat(strwrap("=================", prefix = "\t"), sep = "\n")
  cat("\n")
  cat(paste0("Composite name: ", x$comp_name, "\n"))
  cat(paste0(
    "Events range: ", x$event_range[1],
    " to ", x$event_range[2], "\n"
  ))
  cat("\n")
  cat(paste0("\tTotal intervals: ", length(x$intervals), "\n"))
  cat(paste0("\tMean interval: ", round(mean(x), 1), "\n"))
  cat(paste0("\tMedian interval: ", round(median(x), 1), "\n"))
  cat(paste0("\tWeibull median: ", round(weibull_median, 1), "\n"))
  cat(paste0("\tStandard deviation: ", round(stats::sd(x$intervals), 1), "\n"))
  cat(paste0("\tMinimum interval: ", min(x), "\n"))
  cat(paste0("\tMaximum interval: ", max(x), "\n"))

  cat("\n\n")

  cat(strwrap("Theoretical distribution"), sep = "\n")
  cat(strwrap("------------------------"), sep = "\n")
  cat("\n")
  cat(paste0("Fit distribution: ", x$densfun, "\n\n"))
  print(x$fitdistr)

  cat("\n\n")

  cat(strwrap("Percentiles"), sep = "\n")
  cat(strwrap("-----------"), sep = "\n")
  cat("\n")

  cat(strwrap(paste0(
    "lower (12.5%): ", round(quants[1], 1), " | ", x$densfun, " ",
    "median (50.0%): ", round(quants[2], 1), " | ",
    "upper (87.5%): ", round(quants[3], 1), "\n"
  )))

  cat("\n\n")

  cat(strwrap(x$kstest$method), sep = "\n")
  cat(strwrap(strrep("-", nchar(x$kstest$method))), sep = "\n")
  cat("\n")
  cat(paste0(
    "D^- = ", round(x$kstest$statistic, 5),
    ", p = ", round(x$kstest$p.value, 5), "\n"
  ))

  null_msg <- paste0(
    "Null hypothesis: ",
    "The intervals were sampled from the theoretical distribution."
  )
  cat(strwrap(null_msg, exdent = 4), sep = "\n")

  alt_msg <- paste0(
    "Alt. hypothesis: ",
    "The intervals were not sampled from the theoretical distribution."
  )
  cat(strwrap(alt_msg, exdent = 4), sep = "\n")
  cat("\n\n")

  invisible(x)
}
