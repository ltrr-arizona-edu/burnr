#' Constructor for S3 intervals class.
#'
#' @param comp A composite fhx instance.
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
#' sd(interv)  # Interval standard dev
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
  dens2cum <- list(weibull = stats::pweibull, lognormal = stats::plnorm)
  x <- list()
  x[["intervals"]]<- diff(get_event_years(comp)[[1]])  # TODO: Check that we need [[1]]
  if (length(x$intervals) < 2)
    stop("Too few fire events to compute intervals")
  class(x) <- c("intervals")
  x$fitdistr <- MASS::fitdistr(x$intervals, densfun)
  x$densfun <- densfun
  p_densfun <- dens2cum[[densfun]]
  kstest_args <-c(list(x = x$intervals, y = p_densfun, alternative = "less"), 
                  x$fitdistr$estimate) 
  x$kstest <- do.call(stats::ks.test, kstest_args)
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
#' @export
median.intervals <- function(x, ...) {
  stats::median(x$intervals, ...)
}

#' Interval standard deviation.
#'
#' @param x An intervals object.
#' @param ... Additional arguments passed to \code{sd}.
#'
#' @return Numeric or NA.
#'
#' @export
sd.intervals <- function(x, ...) {
  stats::sd(x$intervals)
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

#' Print an intervals objects.
#'
#' @param x An intervals object.
#' @param ...  Additional arguments that are tossed.
#'
#' @export
print.intervals <- function(x, ...) {
  #ans_sum <- format(rbind(mean(x), median(x), sd(x)), digits = 2, justify = 'right')
  #dimnames(ans_sum) <- list(c('mean', 'median', 'sd'), "")
  dens2cum <- list(weibull = stats::qweibull, lognormal = stats::qlnorm)
  q_densfun <- dens2cum[[x$densfun]]
  quant_args <-c(list(p = c(0.125, 0.5, 0.875)), 
                  x$fitdistr$estimate)
  quants <- do.call(q_densfun, quant_args)
  #quants <- stats::qlnorm(, sdlog = out[["distr_scale"]], meanlog = out[["distr_mean"]])
  cat(strwrap("Summary", prefix = "\t"), sep = "\n")
  cat("\n")
  cat(paste0("mean = ", round(mean(x), 1), ", "))
  cat(paste0("median = ", round(median(x), 1), ", "))
  cat(paste0("sd = ", round(sd(x), 1), ", "))
  cat(paste0("min = ", min(x), ", "))
  cat(paste0("max = ", max(x), "\n"))

  cat("\n\n")
  
  cat(strwrap("Distribution", prefix = "\t"), sep = "\n")
  cat("\n")
  cat(paste0("type: ", x$densfun, "\n\n"))
  print(x$fitdistr)

  cat("\n")
  
  print(x$kstest)
  
  cat("\n\n")
  
  cat(strwrap("Quantiles", prefix = "\t"), sep = "\n")
  cat(paste0('12.5%: ', round(quants[1], 2), ' | '))
  cat(paste0('50.0%: ', round(quants[2], 2), ' | '))
  cat(paste0('87.5%: ', round(quants[3], 2), '\n'))
  
  invisible(x)
}
