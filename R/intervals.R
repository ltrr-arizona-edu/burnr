#' Constructor for S3 intervals class.
#'
#' @param comp A composite fhx instance.
#' @param densfun String giving desired distribution to fit. Can be "weibull" or "lognormal". Default is "weibull".
#' @param ... Arguments passed to \code{fit_distr}.
#'
#' @return An intervals instance.
#'
#' @export
intervals <- function(comp, densfun="weibull", ...) {
  stopifnot(is.fhx(comp))
  x <- list()
  x[["intervals"]]<- diff(get_event_years(comp)[[1]])  # TODO: Check that we need [[1]] or unlist().
  x[["number_intervals"]] <- length(x[["intervals"]])
  if (x[["number_intervals"]] < 2)
    stop("Too few fire intervals to compute a summary")
  x[["mean_interval"]] <- round(mean(x[["intervals"]]), 1)
  x[["median_interval"]] <- round(stats::median(x[["intervals"]]), 1)
  x[["standard_dev"]] <- round(stats::sd(x[["intervals"]]), 2)
  x[["coef_var"]] <- round(stats::sd(x[["intervals"]])/mean(x[["intervals"]]), 2)
  x[["min_interval"]] <- min(x[["intervals"]])
  x[["max_interval"]] <- max(x[["intervals"]])
  x[["dens_func"]] <- densfun
  x[["distr_shape"]] <- NA
  x[["distr_scale"]] <- NA
  x[["distr_mean"]] <- NA
  x[["distr_median"]] <- NA
  x[["distr_mode"]] <- NA
  x[["ks_d"]] <- NA
  x[["pval"]] <- NA
  x[["lower_exceedance"]] <- NA
  x[["upper_exceedance"]] <- NA
  class(x) <- c('intervals')
  x <- fit_distr(x, densfun, ...)
  x
}

#' Check if object is intervals.
#'
#' @param x An R object.
#'
#' @return A boolean indicating whether `x` is an intervals object.
#'
#' @export
is.intervals <- function(x) inherits(x, "intervals")

#' Fit a theoretical distribution to intervals.
#'
#' @param intrv An intervals instance.
#' @param distr String giving desired distribution to fit. Can be "weibull" or "lognormal". Default is "weibull".
#' @param ... Arguments passed to \code{MASS::fitdistr}.
#'
#' @return An intervals instance with fit distribution.
#'
#' @export
fit_distr <- function(intrv, distr, ...) {
  stopifnot(distr %in% c("weibull", "lognormal"))
  stopifnot(is.intervals(intrv))
  out <- intrv
  ft <- MASS::fitdistr(intrv[["intervals"]], distr, ...)
  if (distr == "weibull") {
    out[["distr_shape"]] <- ft[['estimate']][['shape']]
    out[["distr_scale"]] <- ft[['estimate']][['scale']]
    out[["distr_mean"]] <- round(out[["distr_scale"]] * gamma(1 + 1/out[["distr_shape"]]), 2)
    out[["distr_mode"]] <- round(out[["distr_scale"]] * ((out[["distr_shape"]] - 1) / out[["distr_shape"]]) ^ (1 / out[["distr_shape"]]), 2)
    quants <- stats::qweibull(c(0.125, 0.5, 0.875), shape = out[["distr_shape"]], scale = out[["distr_scale"]])
    out[["distr_median"]] <- round(quants[2], 2)
    out[["lower_exceedance"]] <- round(quants[1], 2)
    out[["upper_exceedance"]] <- round(quants[3], 2)
    gf <- stats::ks.test(out[["intervals"]], y = stats::pweibull, shape = out[["distr_shape"]], scale = out[["distr_scale"]], alternative = 'less')
    out[["ks_d"]] <- round(gf$statistic, 2)
    out[["pval"]] <- round(gf$p.value, 2)
  } else if (distr == "lognormal") {
    out[["distr_scale"]] <- ft[['estimate']][['sdlog']]
    out[["distr_mean"]] <- ft[['estimate']][['meanlog']]
    quants <- stats::qlnorm(c(0.125, 0.5, 0.875), sdlog = out[["distr_scale"]], meanlog = out[["distr_mean"]])
    out[["distr_median"]] <- round(quants[2], 2)
    out[["lower_exceedance"]] <- round(quants[1], 2)
    out[["upper_exceedance"]] <- round(quants[3], 2)
    gf <- stats::ks.test(out[["intervals"]], y = stats::plnorm, sdlog = out[["distr_scale"]], meanlog = out[["distr_mean"]], alternative = 'less')
    out[["ks_d"]] <- round(gf$statistic, 2)
    out[["pval"]] <- round(gf$p.value, 2)
  }
  out
}


##' Print an intervals objects.
##'
##' @param x An intervals object.
##'
##' @export
#print.intervals <- function(x, ...) cat(format(x, ...), "\n")
