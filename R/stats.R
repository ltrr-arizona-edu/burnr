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
# ' flist <- list(n = count_year_span,
# '               xbar_interval = function(x) mean_interval(x, injury_event = TRUE))
#' sstats <- series_stats(lgr2)
#' head(sstats)
#'
#' @export
series_stats <- function(x, func_list=list(first=first_year,last=last_year,
  years=count_year_span,inner_type=inner_type,outer_type=outer_type,
  number_fires=count_fire,number_injuries=count_injury,
  recording_years=count_recording,mean_interval=mean_interval)) {
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

#' Number of fire events in an fhx series.
#'
#' @param x An fhx object.
#'
#' @return The number of fire events observed in the series.
#'
#' @export
count_fire <- function(x) {
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

#' Calculate mean fire interval of an fhx series.
#'
#' @param x An fhx object.
#' @param injury_event Boolean indicating whether injuries should be considered event.
#'
#' @return The mean fire interval observed in the series.
#'
#' @export
mean_interval <- function(x, injury_event=FALSE) {
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

#' Perform superposed epoch analysis.
#'
#'
#' @param x A data.frame climate reconstruction or tree-ring series with row names as years.
#' @param key A vector of event years for superposed epoch, such as fire years, or an fhx object
#' with a single \code{series} as produced by \code{composite}
#' @param years_before  The number of lag years prior to the event year
#' @param years_after The number of lag years following the event year
#' @param time_span The length of the x time series to use. Defaults to "key_period"
#' which constrains the time series to the time period of key events; "all" will use the entire
#' time series
#' @param n_iter The number of iterations for bootstrap resampling
#'
#' @details Superposed epoch analysis (SEA) helps to evaluate fire-climate
#' relationships in studies of tree-ring fire history. It works by compositing the values of
#' an anual timeseries or climate reconstruction for the fire years provided (\code{key}) and both positive and
#' negative lag years. Bootstrap resampling of the timeseries is performed to evaluate the statistical
#' significance of each year's mean value. Users interpret the departure of the actual event year
#' means from the simulated event year means.
#'
#' The significance of lag-year departures from the average climate condition was first noted by
#' Baisan and Swetnam (1990) and used in an organized SEA by Swetnam (1993). Since then, the procedure
#' has been commonly applied in fire history studies. The FORTRAN program EVENT.exe was written by
#' Richard Holmes and Thomas Swetnam (Holmes and Swetnam 1994) to perform SEA for fire history
#' specifically. EVENT was incorporated in the FHX2 software by Henri Grissino-Mayer.
#'
#' run_sea was designed to replicate EVENT as closely as possible. We have tried to stay true to their implementation of
#' SEA, although multiple versions of the analysis exist in the climate literature and for fire
#' history (e.g., FHAES implements a diferent procedure). The outcome of EVENT and run_sea should
#' only differ slightly in the values of the simulated events and the departures, because random
#' draws are used. The event year and lag significance levels should match, at least in the general
#' pattern.
#'
#' We note that our implementation of run_sea borrows from the \code{dplR:::sea} function in how it performs
#' the bootstrap procedure, but differs in the kind of output provided for the user.
#'
#' @return A list of three data frames, following the output of EVENT.
#' (1) the actual events table, (2) the simulated events table, and (3) departures of actual from simulated
#'
#' @references Baisan and Swetnam 1990, Fire history on desert mountain range: Rincon Mountain Wilderness, Arizona, U.S.A. Canadian Journal of Forest Research 20:1559-1569.
#' @references Bunn 2008, A dendrochronology program library in R (dplR), Dendrochronologia 26:115-124
#' @references Holmes and Swetnam 1994, EVENT program desription
#' @references Swetnam 1993, Fire history and climate change in giant sequoia groves, Science 262:885-889.
#'
#' @examples
#' \dontrun{
#' # Read in the Cook and Krusic (2004; The North American Drought Atlas) reconstruction
#' # of Palmer Drought Severity Index (PDSI) for the Jemez Mountains area (gridpoint 133).
#' target_url <- paste0('http://iridl.ldeo.columbia.edu',
#'                      '/SOURCES/.LDEO/.TRL/.NADA2004'
#'                      '/pdsiatlashtml/pdsiwebdata/1050w_350n_133.txt')
#' pdsi <- read.table(target_url, header = TRUE, row.names = 1)
#' pdsi <- subset(pdsi, select = "RECON")
#'
#' # Run SEA on Peggy Mesa (pgm) data
#' data(pgm)
#' (pgm.comp <- composite(pgm))
#'
#' (pgm.sea <- run_sea(pdsi, pgm.comp))
#'
#' # Make a bargraph with confidence intervals
#' par(mar=c(2, 3, 1, 1), oma=c(3, 3, 1, 1))
#' bp <- barplot(pgm.sea[[3]]$mean_value,
#'               col=c(rep("grey75", 3),"grey45", "grey30",
#'                     "grey75", "grey30", rep("grey75", 4)),
#'               ylab = '', las=1, cex.axis=1.3, cex=1.3, ylim=c(-2, 2))
#' axis(1, at=bp, labels = -6:4, tick=FALSE, cex.axis=1.3)
#' lines(bp, pgm.sea[[3]]$lower_95_perc, lwd=2, lty=2)
#' lines(bp, pgm.sea[[3]]$upper_95_perc, lwd=2, lty=2)
#' lines(bp, pgm.sea[[3]]$lower_99_perc, lwd=2, lty=3)
#' lines(bp, pgm.sea[[3]]$upper_99_perc, lwd=2, lty=3)
#' mtext(expression(bold('PDSI departure')), side=2, line=2.2, cex=1.5)
#' mtext(expression(bold('Lag year')), side=1, line=3.3, cex=1.5)
#' }
#' \dontrun{
#' # For users who want to perform SEA very near to EVENT.exe and/or have reproducable draws from
#' # the bootstrap procedure, consider including the \code{set.seed} function prior to \code{run_sea}.
#' # Convention is to provide a long integer, such as a birthday (e.g. 3191982).
#' # In the EVENT.exe program, Richard Holmes used the number of days since 1 January 1935.
#' days <- as.numeric(Sys.Date() - as.Date("1jan1935", "%d%b%Y"))
#' set.seed(days)
#' }
#'
#' @export
run_sea <- function(x, key, years_before=6, years_after=4,
                    time_span=c('key_period'), n_iter=1000) {

  message('run_sea(): This function is under development and will likely change in the future.')

  if (is.fhx(key)){
   if (length(unique(key$series)) > 1) stop("key must have a single series")
    else key <- get_event_years(key)[[1]]
  }

  # set up
  period <- range(key)
  rnames <- as.numeric(rownames(x))
  rnames.cut <- rnames[period[1] : period[2]]
  n <- length(key)
  seq.n <- seq_len(n)
  m <- years_before + years_after + 1
  yrs.base <- -years_before:years_after
  out_table <- data.frame(matrix(NA_real_, nrow=m, ncol=16,
                                 dimnames=list(1:m, c('lag_year', 'mean_value',
                                                      'n_values', 'St_dev', 'lower_95',
                                                      'upper_95', 'lower_99', 'upper_99',
                                                      'lower_99.9', 'upper_99.9', 'lower_95_perc',
                                                      'upper_95_perc', 'lower_99_perc', 'upper_99_perc',
                                                      'min_value', 'max_value'))))
  out_table[, 1] <- yrs.base

  # key-event matrix
  event.table <- matrix(NA_real_, ncol = m, nrow = n)
  for (i in seq.n) {
    yrs <- as.character(key[i] + yrs.base)
    event.table[i, ] <- x[yrs, ]
  }

  key_event_table <- out_table[, -c(11:14)]
  key_event_table[, 2] <- colMeans(event.table, na.rm=TRUE)
  key_event_table[, 3] <- apply(event.table, 2, function(x) sum(!is.na(x)))
  key_event_table[, 4] <- apply(event.table, 2, stats::sd, na.rm=TRUE)
  key_event_table[, 5] <- apply(event.table, 2, function(x) mean(x) - 1.960*stats::sd(x, na.rm=TRUE))
  key_event_table[, 6] <- apply(event.table, 2, function(x) mean(x) + 1.960*stats::sd(x, na.rm=TRUE))
  key_event_table[, 7] <- apply(event.table, 2, function(x) mean(x) - 2.575*stats::sd(x, na.rm=TRUE))
  key_event_table[, 8] <- apply(event.table, 2, function(x) mean(x) + 2.575*stats::sd(x, na.rm=TRUE))
  key_event_table[, 9] <- apply(event.table, 2, function(x) mean(x) - 3.294*stats::sd(x, na.rm=TRUE))
  key_event_table[, 10] <- apply(event.table, 2, function(x) mean(x) + 3.294*stats::sd(x, na.rm=TRUE))
  key_event_table[, 11] <- apply(event.table, 2, min, na.rm=TRUE)
  key_event_table[, 12] <- apply(event.table, 2, max, na.rm=TRUE)
  key_event_table <- round(key_event_table, 3)

  # random event matrix
  re.table <- matrix(NA_real_, ncol = m, nrow = n_iter)
  re.subtable <- matrix(NA_real_, ncol = m, nrow = n)
  if(time_span == "key_period"){
    rand_yrs <- rnames.cut
  }
  else {
    rand_yrs <- rnames
  }

  rand_pick <- matrix(sample(rand_yrs, n * n_iter, replace=TRUE),
                      ncol=n_iter, nrow=n, byrow=FALSE)

  for (k in 1:ncol(rand_pick)) {
    for(i in 1:nrow(rand_pick)) {
      yrs <- as.character(rand_pick[i, k] + yrs.base)
      re.subtable[i, ] <- x[yrs, ]
    }
    re.table[k, ] <- colMeans(re.subtable, na.rm = TRUE)
  }

  rand_event_table <- out_table
  rand_event_table[, 2] <- colMeans(re.table, na.rm=TRUE)
  rand_event_table[, 3] <- apply(re.table, 2, function(x) sum(!is.na(x)))
  rand_event_table[, 4] <- apply(re.table, 2, stats::sd, na.rm=TRUE)
  rand_event_table[, 5] <- apply(re.table, 2, function(x) mean(x) - 1.960*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 6] <- apply(re.table, 2, function(x) mean(x) + 1.960*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 7] <- apply(re.table, 2, function(x) mean(x) - 2.575*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 8] <- apply(re.table, 2, function(x) mean(x) + 2.575*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 9] <- apply(re.table, 2, function(x) mean(x) - 3.294*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 10] <- apply(re.table, 2, function(x) mean(x) + 3.294*stats::sd(x, na.rm=TRUE))
  rand_event_table[, 11] <- apply(re.table, 2, function(x) stats::quantile(x, .025, na.rm=TRUE))
  rand_event_table[, 12] <- apply(re.table, 2, function(x) stats::quantile(x, .975, na.rm=TRUE))
  rand_event_table[, 13] <- apply(re.table, 2, function(x) stats::quantile(x, .005, na.rm=TRUE))
  rand_event_table[, 14] <- apply(re.table, 2, function(x) stats::quantile(x, .995, na.rm=TRUE))
  rand_event_table[, 15] <- apply(re.table, 2, min, na.rm=TRUE)
  rand_event_table[, 16] <- apply(re.table, 2, max, na.rm=TRUE)
  rand_event_table <- round(rand_event_table, 3)

  # Departure table

  departure_table <- out_table[, -c(3, 4, 15, 16)]
  departure_table[, 2] <- key_event_table[, 2] - rand_event_table[, 2]
  departure_table[, 3] <- apply(re.table, 2, function(x) -1 * 1.960 * stats::sd(x, na.rm = TRUE))
  departure_table[, 4] <- apply(re.table, 2, function(x)      1.960 * stats::sd(x, na.rm = TRUE))
  departure_table[, 5] <- apply(re.table, 2, function(x) -1 * 2.575 * stats::sd(x, na.rm = TRUE))
  departure_table[, 6] <- apply(re.table, 2, function(x)      2.575 * stats::sd(x, na.rm = TRUE))
  departure_table[, 7] <- apply(re.table, 2, function(x) -1 * 3.294 * stats::sd(x, na.rm = TRUE))
  departure_table[, 8] <- apply(re.table, 2, function(x)      3.294 * stats::sd(x, na.rm = TRUE))
  temp <- apply(re.table, 2, function(x) stats::median(x))  #Simulated medians
  departure_table[, 9] <- rand_event_table[, 11] - temp
  departure_table[, 10] <- rand_event_table[, 12] - temp
  departure_table[, 11] <- rand_event_table[, 13] - temp
  departure_table[, 12] <- rand_event_table[, 14] - temp
  rm(temp)
  departure_table <- round(departure_table, 3)

  out_list <- list("Actual events" = key_event_table, "Simulated events" = rand_event_table,
                   "Departures of actual from simulated" = departure_table)

  prnt.tbl <- data.frame(lag = departure_table$lag_year,
                         departure = departure_table$mean_value,
                         sig = paste(ifelse(departure_table$mean_value < departure_table$lower_95_perc |
                                              departure_table$mean_value > departure_table$upper_95_perc, '*', ''),
                                     ifelse(departure_table$mean_value < departure_table$lower_99_perc |
                                              departure_table$mean_value > departure_table$upper_99_perc, '**', ''),
                                     sep=''))
  print(prnt.tbl)

  return(out_list)
}

#' Calculate the sample depth of an fhx object
#'
#' @param a An fhx object.
#' @return A data.frame containing the years and number of trees
#'
#' @export
#'
sample_depth <- function(a){
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
