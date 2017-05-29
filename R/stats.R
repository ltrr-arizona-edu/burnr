#' Generate series-level descriptive statistics.
#'
#' @param x An fhx object.
#' @param func_list A list of named functions that will be run on each series
#'   in the fhx object. The list name for each function is the corresponding
#'   column name in the output data.frame.
#'
#' @return A data.frame containing series-level statistics.
#'
#' @examples
#' data(lgr2)
#' series_stats(lgr2)
#'
#' # You can create your own list of statistics to output. You can also create
#' # your own functions:
#' flist <- list(n = count_year_span,
#'               xbar_interval = function(x) mean_interval(x, injury_event = TRUE))
#' sstats <- series_stats(lgr2)
#' head(sstats)
#'
#' @export
series_stats <- function(x, func_list=list(first=first_year,last=last_year,
  years=count_year_span,inner_type=inner_type,outer_type=outer_type,
  number_scars=count_scar,number_injuries=count_injury,
  recording_years=count_recording,mean_interval=series_mean_interval)) {
  stopifnot(is.fhx(x))
  plyr::ddply(x, c('series'), function(df) data.frame(lapply(func_list, function(f) f(df))))
}

#' First (earliest) year of an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The minimum or first year of series in 'x'.
#'
#' @export
first_year <- function(x) {
  min(x$year)
}

#' Last (most recent) year of an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The maximum or last year of series in 'x'. 'NA' will be returned if 'NA' is in x$year.
#'
#' @export
last_year <- function(x) {
  max(x$year)
}

#' Number of years of an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The difference between the first and last observations in the series. 'NA' will be returned if 'NA' is in 'x$year'.
#'
#' @export
count_year_span <- function(x) {
  max(x$year) - min(x$year) + 1
}

#' Type of observation in the last (most recent) year of an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The a factor giving the type of observation in the last observation of the series.
#'
#' @export
outer_type <- function(x) {
  x$rec_type[which.max(x$year)]
}

#' Type of observation in the first (earliest) year of an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The a factor giving the type of observation in the first observation of the series.
#'
#' @export
inner_type <- function(x) {
  x$rec_type[which.min(x$year)]
}

#' Number of scar events in an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The number of fire events observed in the series.
#'
#' @export
count_scar <- function(x) {
  length(grep('_fs', x$rec_type))
}

#' Number of injury events in an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The number of injury events observed in the series.
#'
#' @export
count_injury <- function(x) {
  length(grep('_fi', x$rec_type))
}

#' Number of recording years in an fhx series.
#'
#' @param x An fhx object.
#' @param injury_event Boolean indicating whether injuries should be considered event.
#'
#' @return The number of recording events observed in the series.
#'
#' @export
count_recording <- function(x, injury_event=FALSE) {
  nrow(find_recording(x, injury_event = injury_event))
}

#' Calculate mean fire interval of a single fhx series.
#'
#' @param x An fhx object with a single series. For proper fire intervals see `intervals()`.
#' @param injury_event Boolean indicating whether injuries should be considered event.
#'
#' @return The mean fire interval observed in the series.
#' @seealso intervals()
#'
#' @export
series_mean_interval <- function(x, injury_event=FALSE) {
  search_str <- '_fs'
  if (injury_event) {
    search_str <- paste0('_fi|', search_str)
  }
  event_years <- sort(x$year[grepl(search_str, x$rec_type)])
  out <- NA
  if (length(event_years) > 1) {
    intervals <- diff(event_years)
    out <- round(mean(intervals), 1)
  }
  out
}

#' Calculate the sample depth of an fhx object
#'
#' @param a An fhx object.
#' @return A data.frame containing the years and number of trees
#'
#' @export
#'
sample_depth <- function(a) {
  stopifnot('fhx' %in% class(a))
  x <- series_stats(a)
  n.trees <- nrow(x)
  aa <- data.frame(year = min(x$first):max(x$last))
  for(i in 1:n.trees){
    yrs <- x[i, ]$first : x[i, ]$last
    bb <- data.frame(year = yrs, z = 1)
    names(bb)[2] <- x$series[i]
    aa <- merge(aa, bb, by=c('year'), all=TRUE)
  }
  aa$samp_depth <- rowSums(aa[, 2:n.trees + 1], na.rm=TRUE)
  out <- subset(aa, select=c('year', 'samp_depth'))
  out
}

#' Summary of `fhx` object
#'
#' @param object An fhx object.
#' @param ... Additional arguments.
#'
#' @return A summary.fhx object.
#'
#' @export
summary.fhx <- function(object, ...) {
  out <-list(number_series = length(series_names(object)),
             first_year = first_year(object),
             last_year = last_year(object),
             number_scars = count_scar(object),
             number_injuries = count_injury(object))
  class(out) <- 'summary.fhx'
  out
}

#' Generate site-level summary statistics
#'
#' @param x An fhx object
#' @param site_name Three character site code, defaults to "XXX"
#' @param year_range Delimits the analysis period. For example, \code{c(1600, 1900)}.
#' @param filter_prop An optional argument if the user chooses to include a composite rug in their plot. This is passed to \code{composite}. See this function for details.
#' @param filter_min_rec An optional argument if the user chooses to include a composite rug in their plot. This is passed to \code{composite}. See this function for details.
#' @param filter_min_events An optional argument if the user chooses to include a composite rug in their plot. This is passed to \code{composite}. See this function for details.
#' @param injury_event Boolean indicating whether injuries should be considered recorders. This is passed to \code{composite}. See this function for details.
#'
#' @details This function produces a summary table for any fhx object. The statistics it includes are shared by other popular fire history software such as FHX2 and FHAES.
#' @return A data.frame of summary statistics
#' @export

site_stats <- function(x, site_name = 'XXX', year_range = NULL, filter_prop = 0.25, filter_min_rec = 2,
                        filter_min_events = 1, injury_event = FALSE) {

  stopifnot(is.fhx(x))
  sumNames <- c('number_series', 'first_year', 'last_year', 'first_event', 'last_event',
                'number_intervals', 'mean_interval', 'median_interval',
                'standard_dev', 'coef_var', 'min_interval', 'max_interval',
                'weibull_shape', 'weibull_scale', 'weibull_mean',
                'weibull_median', 'weibull_mode', 'KS_d', 'pval', 'lower_exceedance',
                'upper_exceedance')
  site.stats <- data.frame(variable = sumNames, site = NA)
  names(site.stats)[2] <- site_name
  # Perform site composite for interval stats
  if (!is.null(year_range)) {
    x <- x[x$year >= min(year_range) & x$year <= max(year_range), ]
  }
  x.comp <- composite(x, filter_prop = filter_prop, filter_min_rec = filter_min_rec,
                          filter_min_events = filter_min_events, injury_event = injury_event)
  intervals <- diff(get_event_years(x.comp)[[1]])
  if(length(intervals) < 2)
    stop("Too few fire intervals to compute a summary")
  # Weibull fit
  ft.r <- MASS::fitdistr(intervals, "weibull")
  shape <- as.numeric(ft.r$estimate[1])
  scale <- as.numeric(ft.r$estimate[2])
  weib.quants <- stats::qweibull(c(.125, .5, .875), shape=shape, scale=scale)
  # gf <- suppressWarnings( stats::ks.test(intervals, y=stats::pweibull, shape=shape, scale=scale, alternative='less'))
  gf <- stats::ks.test(intervals, y=stats::pweibull, shape=shape, scale=scale, alternative='less')
  # Fill out summary table
  site.stats['number_trees', ] <- length(levels(x$series))
  site.stats['first_year', ] <- first_year(x)
  site.stats['last_year', ] <- last_year(x)
  if (injury_event == FALSE) {
    site.stats['first_event', ] <- min(x[grep('fs', x$rec_type), ]$year)
    site.stats['last_event', ] <- max(x[grep('fs', x$rec_type), ]$year)
  }
  else {
    site.stats['first_event', ] <- min(min(x[grep('fs', x$rec_type), ]$year),
                                       min(x[grep('fi', x$rec_type), ]$year))
    site.stats['last_event', ] <- max(max(x[grep('fs', x$rec_type), ]$year),
                                      max(x[grep('fi', x$rec_type), ]$year))
  }
  site.stats['number_intervals', ] <- length(intervals)
  site.stats['mean_interval', ] <- round(mean(intervals), 1)
  site.stats['median_interval', ] <- round(stats::median(intervals), 1)
  site.stats['standard_dev', ] <- round(stats::sd(intervals), 2)
  site.stats['coef_var', ] <- round(stats::sd(intervals)/mean(intervals), 2)
  site.stats['min_interval', ] <- min(intervals)
  site.stats['max_interval', ] <- max(intervals)
  site.stats['weibull_shape', ] <- round(shape, 2)
  site.stats['weibull_scale', ] <- round(scale, 2)
  site.stats['weibull_mean', ] <- round(scale * gamma(1 + 1/shape), 2)
  site.stats['weibull_median', ] <- round(weib.quants[2], 2)
  site.stats['weibull_mode', ] <- round(scale * ((shape-1)/shape)^(1/shape), 2)
  site.stats['KS_d', ] <- round(gf$statistic, 2)
  site.stats['pval', ] <- round(gf$p.value, 2)
  site.stats['lower_exceedance', ] <- round(weib.quants[1], 2)
  site.stats['upper_exceedance', ] <- round(weib.quants[3], 2)
  return(site.stats)
}

