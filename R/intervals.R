#' Constructor for S3 intervals class.
#'
#' @param comp A composite fhx instance. Should have only one series in it.
#' @param densfun String giving desired distribution to fit. Suggest "weibull" or "lognormal". Default is "weibull".
#'
#' @return An intervals instance.
#'
#' @examples
#' data(pgm)
#' interv <- intervals(composite(pgm))
#' print(interv)
#'
#' mean(interv)  # Mean interval
#' 
#' # Now fit log-normal distribution instead of Weibull.
#' intervals(composite(pgm), densfun = "lognormal")
#'
#' \dontrun{
#' # Boxplot of fire interval distribution.
#' boxplot(intervals(composite(pgm))$intervals)
#' }
#'
#' @export
intervals <- function(comp, densfun="weibull") {
  stopifnot(is.fhx(comp))
  stopifnot(densfun %in% c("weibull", "lognormal"))
  if (length(series_names(comp)) > 1)
    stop("Found multiple series in `comp`. There can only be one.")
  dens2cum <- list(weibull = stats::pweibull, lognormal = stats::plnorm)
  x <- list()
  x$intervals <- diff(get_event_years(comp)[[1]])  # TODO: Check that we need [[1]]
  if (length(x$intervals) < 2)
    stop("Too few fire events to compute intervals")
  class(x) <- c("intervals")
  x$fitdistr <- MASS::fitdistr(x$intervals, densfun)
  x$densfun <- densfun
  p_densfun <- dens2cum[[densfun]]
  kstest_args <-c(list(x = x$intervals, y = p_densfun, alternative = "less"), 
                  x$fitdistr$estimate) 
  x$kstest <- do.call(stats::ks.test, kstest_args)
  x$shapirotest <- stats::shapiro.test(x$intervals)
  x$comp_name <- series_names(comp)
  x$event_range <- range(unlist(get_event_years(comp)))
  x
}

#' Check if object is intervals.
#'
#' @param x An R object.
#'
#' @return Boolean indicating whether `x` is an intervals object.
#'
#' @export
is.intervals <- function(x) inherits(x, "intervals")

#' Interval arithmetic mean.
#'
#' @param x An intervals object.
#' @param ... Additional arguments passed to \code{mean}.
#'
#' @return Numeric or NA.
#'
#' @export
mean.intervals <- function(x, ...) {
  mean(x$intervals, ...)
}

#' Interval median.
#'
#' @param x An intervals object.
#' @param ... Additional arguments passed to \code{median}.
#'
#' @return Numeric or NA.
#'
#' @importFrom stats median
#' @export
median.intervals <- function(x, ...) {
  median(x$intervals, ...)
}


#' Minimum interval.
#'
#' @param x An intervals object.
#' @param ... Additional arguments passed to \code{min}.
#'
#' @return Numeric or NA.
#'
#' @export
min.intervals <- function(x, ...) {
  min(x$intervals)
}

#' Maximum interval.
#'
#' @param x An intervals object.
#' @param ... Additional arguments passed to \code{max}.
#'
#' @return Numeric or NA.
#'
#' @export
max.intervals <- function(x, ...) {
  max(x$intervals)
}

#' Fit distribution quantiles.
#'
#' @param x An intervals object.
#' @param q Vector giving the desired quantiles.
#' @param ... Additional arguments passed to the quantile function of the fit distribution.
#'
#' @examples
#' data(pgm)
#' intervs <- intervals(composite(pgm))
#' quantile(intervs)
#'
#' # Or you can pass in your own quantiles:
#' quantile(intervs, q = c(0.25, 0.5, 0.75))
#'
#' @importFrom stats quantile
#' @export
quantile.intervals <- function(x, q=c(0.125, 0.5, 0.875), ...) {
  dens2cum <- list(weibull = stats::qweibull, lognormal = stats::qlnorm)
  q_densfun <- dens2cum[[x$densfun]]
  quant_args <-c(list(q, ...), 
                  x$fitdistr$estimate)
  quants <- do.call(q_densfun, quant_args)
  quants
}



#' Plot an intervals object.
#'
#' @param ... Arguments passed on to \code{plot_intervals_dist}.
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


#' Basic intervals distribution plot.
#'
#' @param x An intervals object.
#' @param binwidth A sea object.
#'
#'
#' @return A ggplot object.
#'
#' @export
plot_intervals_dist <- function(x, binwidth=NULL) {
  p <- ggplot2::ggplot(data.frame("intervals" = x[["intervals"]]),
                       ggplot2::aes_string(x = "intervals"))
  if (is.null(binwidth)) {
  p <- (p + ggplot2::geom_histogram())
  } else {
  p <- (p + ggplot2::geom_histogram(binwidth = binwidth))
  }
  p <- (p + ggplot2::geom_rug() + ggplot2::theme_bw())
  p
}


#' Print an intervals objects.
#'
#' @param x An intervals object.
#' @param ...  Additional arguments that are tossed.
#'
#' @export
print.intervals <- function(x, ...) {
  #ans_sum <- format(rbind(mean(x), median(x), sd(x)), digits = 2, justify = 'right')
  #dimnames(ans_sum) <- list(c('mean', 'median', 'sd'), "")
  quants <- quantile(x, q = c(0.847, 0.5, 0.125))
  cat(strwrap("Interval Analysis", prefix = "\t"), sep = "\n")
  cat(strwrap("=================", prefix = "\t"), sep = "\n")
  cat("\n")
  cat(paste0("Composite name: ", x$comp_name, "\n"))
  cat(paste0("Events range: ", x$event_range[1], " to ", x$event_range[2], "\n"))
  cat("\n")
  cat(paste0("\tTotal intervals: ", length(x$intervals), "\n"))
  cat(paste0("\tMean interval: ", round(mean(x), 1), "\n"))
  cat(paste0("\tMedian interval: ", round(median(x), 1), "\n"))
  cat(paste0("\tStandard deviation: ", round(stats::sd(x$intervals), 1), "\n"))
  cat(paste0("\tMinimum interval: ", min(x), "\n"))
  cat(paste0("\tMaximum interval: ", max(x), "\n"))

  # cat("\n\n")
  
  # cat(strwrap(x$shapirotest$method, prefix = "\t"), sep = "\n")
  # cat("\n")
  # cat(paste0("W = ", round(x$shapirotest$statistic, 5), ", p = ", round(x$shapirotest$p.value, 5), "\n"))
  # cat(strwrap("Null hypothesis: The intervals were sampled from a normally distributed population.", exdent = 4), sep = "\n")
  # cat(strwrap("Alt. hypothesis: The intervals were not sampled from a normally distributed population.", exdent = 4), sep = "\n")
  
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

  cat(strwrap(paste0('lower (12.5%): ', round(quants[1], 1), ' | ', x$densfun, ' median (50.0%): ', round(quants[2], 1), ' | ', 'upper (87.5%): ', round(quants[3], 1), '\n')))

  # cat(paste0('Lower (12.5%): ', round(quants[1], 1), ' | '))
  # cat(paste0(x$densfun, ' median (50.0%): ', round(quants[2], 1), ' | '))
  # cat(paste0('Upper (87.5%): ', round(quants[3], 1), '\n'))
  
  cat("\n\n")
 
  cat(strwrap(x$kstest$method), sep = "\n")
  cat(strwrap(strrep("-", nchar(x$kstest$method))), sep = "\n")
  cat("\n")
  cat(paste0("D^- = ", round(x$kstest$statistic, 5), ", p = ", round(x$kstest$p.value, 5), "\n"))
  cat(strwrap("Null hypothesis: The intervals were sampled from the fit theoretical distribution.", exdent = 4), sep = "\n")
  cat(strwrap("Alt. hypothesis: The intervals distribution lies below the fit theoretical distribution.", exdent = 4), sep = "\n")
  cat("\n\n")
  
  invisible(x)
}
