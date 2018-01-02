#' Perform superposed epoch analysis.
#'
#' @param x A data.frame climate reconstruction or tree-ring series with row names as years.
#' @param key A vector of event years for superposed epoch, such as fire years, or an fhx object
#' with a single \code{series} as produced by \code{composite}
#' @param years_before  The number of lag years prior to the event year
#' @param years_after The number of lag years following the event year
#' @param key_period Logical. Constrains the time series to the time period of key events within the range
#' of the x climate series. False uses the entire climate series, ignoring the period of key events.
#' time series
#' @param n_iter The number of iterations for bootstrap resampling
#'
#' @details Superposed epoch analysis (SEA) helps to evaluate fire-climate
#' relationships in studies of tree-ring fire history. It works by compositing the values of
#' an annual time series or climate reconstruction for the fire years provided (\code{key}) and both positive and
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
#' history (e.g., FHAES implements a different procedure). The outcome of EVENT and run_sea should
#' only differ slightly in the values of the simulated events and the departures, because random
#' draws are used. The event year and lag significance levels should match, at least in the general
#' pattern.
#'
#' We note that our implementation of run_sea borrows from the \code{dplR::sea} function in how it performs
#' the bootstrap procedure, but differs in the kind of output provided for the user.
#'
#' @return A list of three data frames, following the output of EVENT.
#' (1) the actual events table, (2) the simulated events table, and (3) departures of actual from simulated
#'
#' @references Baisan and Swetnam 1990, Fire history on desert mountain range: Rincon Mountain Wilderness, Arizona, U.S.A. Canadian Journal of Forest Research 20:1559-1569.
#' @references Bunn 2008, A dendrochronology program library in R (dplR), Dendrochronologia 26:115-124
#' @references Holmes and Swetnam 1994, EVENT program description
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
                    key_period = TRUE, n_iter=1000) {
  .Deprecated('sea')
  sea(x, key, nbefore=years_before, nafter=years_after,
                    event_range=key_period, n_iter=n_iter)
}


#' Perform superposed epoch analysis.
#'
#' @param x A data.frame climate reconstruction or tree-ring series with row names as years.
#' @param event A vector of event years for superposed epoch, such as fire years, or an fhx object
#' with a single \code{series} as produced by \code{composite}
#' @param nbefore  The number of lag years prior to the event year
#' @param nafter The number of lag years following the event year
#' @param event_range Logical. Constrains the time series to the time period of key events within the range
#' of the x climate series. False uses the entire climate series, ignoring the period of key events.
#' time series
#' @param n_iter The number of iterations for bootstrap resampling
#'
#' @details Superposed epoch analysis (SEA) helps to evaluate fire-climate
#' relationships in studies of tree-ring fire history. It works by compositing the values of
#' an annual timeseries or climate reconstruction for the fire years provided (\code{key}) and both positive and
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
#' sea was designed to replicate EVENT as closely as possible. We have tried to stay true to their implementation of
#' SEA, although multiple versions of the analysis exist in the climate literature and for fire
#' history (e.g., FHAES implements a different procedure). The outcome of EVENT and sea should
#' only differ slightly in the values of the simulated events and the departures, because random
#' draws are used. The event year and lag significance levels should match, at least in the general
#' pattern.
#'
#' We note that our implementation of run_sea borrows from the \code{dplR::sea} function in how it performs
#' the bootstrap procedure, but differs in the kind of output provided for the user.
#'
#' @return A list of three data frames, following the output of EVENT.
#' (1) the actual events table, (2) the simulated events table, and (3) departures of actual from simulated
#'
#' @references Baisan and Swetnam 1990, Fire history on desert mountain range: Rincon Mountain Wilderness, Arizona, U.S.A. Canadian Journal of Forest Research 20:1559-1569.
#' @references Bunn 2008, A dendrochronology program library in R (dplR), Dendrochronologia 26:115-124
#' @references Holmes and Swetnam 1994, EVENT program description
#' @references Swetnam 1993, Fire history and climate change in giant sequoia groves, Science 262:885-889.
#'
#' @examples
#' \dontrun{
#' # Read in the Cook and Krusic (2004; The North American Drought Atlas) reconstruction
#' # of Palmer Drought Severity Index (PDSI) for the Jemez Mountains area (gridpoint 133).
# ' target_url <- paste0('http://iridl.ldeo.columbia.edu',
# '                      '/SOURCES/.LDEO/.TRL/.NADA2004',
# '                      '/pdsiatlashtml/pdsiwebdata/1050w_350n_133.txt')
# ' pdsi <- read.table(target_url, header = TRUE, row.names = 1)
# ' pdsi <- subset(pdsi, select = "RECON")
# '
# ' # Run SEA on Peggy Mesa (pgm) data
# ' data(pgm)
# ' pgm_comp <- composite(pgm)
# '
# ' pgm_sea <- sea(pdsi, pgm_comp)
# ' 
# ' # See basic results:
# ' print(pgm_sea)
# '
# ' # Basic plot: 
# ' plot(pgm_sea)
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
sea <- function(x, event, nbefore=6, nafter=4, event_range=TRUE, n_iter=1000) {
  if (is.fhx(event)){
   if (length(unique(event$series)) > 1) stop("event must have a single series")
    else event <- get_event_years(event)[[1]]
  }

  # set up
  rnames <- as.numeric(rownames(x))
  if (all(as.character(seq(length(rnames))) == rnames)) {
    warning("`x` arg for `sea()` could be missing rownames - be sure that timeseries years are rownames")
  }
  event.cut <- rnames[rnames %in% event]
  if (length(event.cut) <= 0) {
    stop("`x` and `event` have no shared years")
  }
  period <- range(event.cut)
  rnames.cut <- seq(period[1], period[2])
  n <- length(event.cut)
  if (length(event.cut) != length(event)) {
    warning(paste('One or more event years is outside the range of the climate series. Using ',
                  n, ' event years: ', period[1], ' to ', period[2], '.', 
                  sep = ''),
            call. = FALSE)
  }
  seq.n <- seq_len(n)
  m <- nbefore + nafter + 1
  yrs.base <- -nbefore:nafter
  out_table <- data.frame(matrix(NA_real_, nrow=m, ncol=16,
                                 dimnames=list(1:m, c('lag', 'mean',
                                                      'n', 'St_dev', 'lower_95',
                                                      'upper_95', 'lower_99', 'upper_99',
                                                      'lower_99.9', 'upper_99.9', 
                                                      'lower_95_perc',
                                                      'upper_95_perc', 'lower_99_perc', 'upper_99_perc',
                                                      'min', 'max'))))
  out_table[, 1] <- yrs.base

  # event-event matrix
  event.table <- matrix(unlist(lapply(event.cut, function(bb) x[rnames %in% (bb + yrs.base), ])),
                        nrow=n, ncol=m, byrow = TRUE)

  actual_event_table <- out_table[, -c(11:14)]
  actual_event_table[, 2] <- colMeans(event.table, na.rm=TRUE)
  actual_event_table[, 3] <- apply(event.table, 2, function(x) sum(!is.na(x)))
  actual_event_table[, 4] <- apply(event.table, 2, stats::sd, na.rm=TRUE)
  actual_event_table[, 5] <- apply(event.table, 2, function(x) mean(x) - 1.960*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 6] <- apply(event.table, 2, function(x) mean(x) + 1.960*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 7] <- apply(event.table, 2, function(x) mean(x) - 2.575*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 8] <- apply(event.table, 2, function(x) mean(x) + 2.575*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 9] <- apply(event.table, 2, function(x) mean(x) - 3.294*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 10] <- apply(event.table, 2, function(x) mean(x) + 3.294*stats::sd(x, na.rm=TRUE))
  actual_event_table[, 11] <- apply(event.table, 2, min, na.rm=TRUE)
  actual_event_table[, 12] <- apply(event.table, 2, max, na.rm=TRUE)
  actual_event_table <- round(actual_event_table, 3)

  # random event matrix
  if(event_range ==  TRUE){
    rand_yrs <- rnames.cut
  }
  else {
    rand_yrs <- rnames
  }

  rand_pick <- matrix(sample(rand_yrs, n * n_iter, replace=TRUE),
                      ncol=n_iter, nrow=n, byrow=FALSE)

  rand_list <- lapply(seq_len(ncol(rand_pick)), function(aa){
    matrix(unlist(lapply(rand_pick[, aa], function(bb) x[rnames %in% (bb + yrs.base), ])),
           nrow=n, ncol=m, byrow=TRUE)
  })

  re.table <- t(sapply(rand_list, function(x) colMeans(x)))
  rand_event_table <- out_table
  rand_event_table[, 2] <- colMeans(re.table, na.rm=TRUE)
  rand_event_table[, 3] <- apply(re.table, 2, function(x) sum(!is.na(x)))
  rand_event_table[, 4] <- apply(re.table, 2, function(x) stats::sd(x, na.rm=TRUE))
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
  departure_table[, 2] <- actual_event_table[, 2] - rand_event_table[, 2]
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

  out <- list("actual" = actual_event_table,
              "random" = rand_event_table,
              "departure" = departure_table)
  out$simulated <- re.table  # DEBUG
  out$observed <- event.table  # DEBUG
  class(out) <- c("sea")

  out
}


#' Check if object is sea.
#'
#' @param x An R object.
#'
#' @return Boolean indicating whether `x` is an sea object.
#'
#' @export
is.sea <- function(x) inherits(x, "sea")


#' Plot a sea object.
#'
#' @param ... Arguments passed on to \code{plot_sealags}.
#'
#' @examples
#' \dontrun{
#' # Read in the Cook and Krusic (2004; The North American Drought Atlas) reconstruction
#' # of Palmer Drought Severity Index (PDSI) for the Jemez Mountains area (gridpoint 133).
#' 
#' data(pgm_pdsi)
#'
#' # Run SEA on Peggy Mesa (pgm) data
#' data(pgm)
#' pgm_comp <- composite(pgm)
#'
#' pgm_sea <- sea(pgm_pdsi, pgm_comp)
#' 
#' plot(pgm_sea)
#' }
#' @export
plot.sea <- function(...) {
  print(plot_sealags(...))
}


#' Basic SEA lag plot.
#'
#' @param x A sea object.
#'
#' @return A ggplot object.
#'
#' @export
plot_sealags <- function(x) {
  p <- ggplot2::ggplot(x$departure, ggplot2::aes_string(y = "mean", x = "lag"))
  p <- (p + ggplot2::geom_col()
          + ggplot2::geom_line(ggplot2::aes_string(y = "upper_99_perc")) + ggplot2::geom_point(ggplot2::aes_string(y = "upper_99_perc"))
          + ggplot2::geom_line(ggplot2::aes_string(y = "lower_99_perc")) + ggplot2::geom_point(ggplot2::aes_string(y = "lower_99_perc"))
          + ggplot2::geom_line(ggplot2::aes_string(y = "lower_95_perc"), linetype = "dashed") + ggplot2::geom_point(ggplot2::aes_string(y = "upper_95_perc"))
          + ggplot2::geom_line(ggplot2::aes_string(y = "upper_95_perc"), linetype = "dashed") + ggplot2::geom_point(ggplot2::aes_string(y = "lower_95_perc"))
          + ggplot2::ylab("Mean departure")
          + ggplot2::xlab("Lag")
          + ggplot2::theme_bw())
  p
}


#' Print an sea objects.
#'
#' @param x An intervals object.
#' @param ...  Additional arguments that are tossed.
#'
#' @export
print.sea <- function(x, ...) {
  prnt_tbl <- data.frame(lag = x$departure$lag,
                         upper95 = x$departure$upper_95_perc,
                         lower95 = x$departure$lower_95_perc,
                         upper99 = x$departure$upper_99_perc,
                         lower99 = x$departure$lower_99_perc,
                         departure = x$departure$mean)
  # prnt_tbl$sig <- ifelse(x$departure$mean < x$departure$lower_95_perc | x$departure$mean > x$departure$upper_95_perc, ".", " ")
  prnt_tbl$sig <- " "
  prnt_tbl$sig[x$departure$mean < x$departure$lower_95_perc | x$departure$mean > x$departure$upper_95_perc] <- "."
  prnt_tbl$sig[x$departure$mean < x$departure$lower_99_perc | x$departure$mean > x$departure$upper_99_perc] <- "*"
                         # sig = paste(ifelse(x$departure$mean < x$departure$lower_95_perc |
                         #                      x$departure$mean > x$departure$upper_95_perc, '*', ''),
                         #             ifelse(x$departure$mean < x$departure$lower_99_perc |
                         #                    x$departure$mean > x$departure$upper_99_perc, '*', ''),
                         #             sep=''))
  cat(strwrap("Superposed Epoch Analysis", prefix = "\t"), sep = "\n")
  cat(strwrap("=========================", prefix = "\t"), sep = "\n")
  print(prnt_tbl, row.names = FALSE)
  cat(strwrap("---"), sep = "\n")
  cat(paste0("Signif. codes: 0.01 '*' 0.05 '.'\n"))
  invisible(x)
}
