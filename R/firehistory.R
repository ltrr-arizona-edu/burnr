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

#' Read input FHX file body from \code{fname} and use to return an \code{fhx} object.
#'
#' @param fname Name of target FHX file. Needs to be in format version 2.
#' @param encoding Encoding to use when reading the FHX file. The default is to use the system.
#' @return An \code{fhx} object.
read.fhx <- function(fname, encoding=getOption("encoding")) {
  con <- file(fname, encoding = encoding)
  on.exit(close(con))
  # Error checking and basic variables.
  if (length(readLines(con, n = 1)) == 0)
    stop("file appears to be empty")
  fl <- readLines(con, warn = FALSE)
  if (!any(suppressWarnings(grepl("FHX2 FORMAT|FIRE2 FORMAT", fl, ignore.case = TRUE))))
    stop("Cannot find line 'FHX2 FORMAT' or 'FIRE2 FORMAT'.")

  first <- suppressWarnings(grep("FHX2 FORMAT|FIRE2 FORMAT", fl, ignore.case = TRUE))
  describe <- as.numeric(strsplit(fl[[first + 1]], " ")[[1]])
  if (length(describe) != 3) {  # First year; no. sites; length of site id.
    stop(paste("Three-digit descriptive information that should be on line ",
               first + 1,
               " needs to have 3 elements separated by spaces."))
  }
  # TODO: Need error check that row length = describe[2] + year.
  # TODO: Need error check that first year in body is first year in meta.
  
  type.key <- list("?" = "estimate",  # My own creation for estimated years to pith.
                   "." = "null.year",
                   "|" = "recorder.year",
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
                   "a" = "latewd.fi",
                   "[" = "pith.year",
                   "]" = "bark.year",
                   "{" = "inner.year",
                   "}" = "outer.year")
  # Parse series names.
  uncleaned <- as.matrix(unlist(strsplit(fl[(first + 2):(first + 1 + describe[3])], "")))
  if ((describe[2] * describe[3]) != dim(uncleaned)[1])
      stop("The file's three-digit descriptive information on line ", first + 1,
           " does not match the series titles in the file. Please correct this discrepancy.")
  dim(uncleaned) <- c(describe[2], describe[3])
  series.names <- apply(uncleaned, 1, function(x) gsub("^\\s+|\\s+$", "", paste(x, collapse = "")))
  # series.names <- apply(uncleaned, 1, paste, collapse = "")
  databuff <- 2
  while (TRUE) {
    if (gsub("^\\s+|\\s+$", "", fl[first + databuff + describe[3]]) == "") {
      databuff <- databuff + 1
    } else {
      break
    }
  }
  if (fl[first + databuff - 1 + describe[3]] != "")
      Stop("The line before the annual FHX data should be blank.")
  # Filling with info from the fhx file body.
  fl.body <- strsplit(fl[(first + databuff + describe[3]) : length(fl)], split = "")
  first.year <- describe[1]
  fl.body <- as.data.frame(t(sapply(fl.body, function(x) x[1:describe[2]])),
                           stringsAsFactors = FALSE)
  # DEBUG: Should try doing the lines below as part of the above function and see the time dif. Might be a boost.
  names(fl.body) <- series.names
  fl.body$year <- seq(first.year, first.year + dim(fl.body)[1] - 1)
  fl.body.melt <- reshape2::melt(fl.body, id.vars = "year", value.name = "type",
                       variable.name = "series")
  fl.body.melt <- subset(fl.body.melt, type != ".")
  fl.body.melt$type <- vapply(fl.body.melt$type, function(x) type.key[[x]], "a") 
  fl.body.melt$type <- factor(fl.body.melt$type,
                              levels = c("null.year", "recorder.year", "unknown.fs",
                                         "unknown.fi", "dormant.fs", "dormant.fi",
                                         "early.fs", "early.fi", "middle.fs",
                                         "middle.fi", "late.fs", "late.fi",
                                         "latewd.fs", "latewd.fi", "pith.year",
                                         "bark.year", "inner.year", "outer.year",
                                         "estimate"))
  f <- fhx(year = fl.body.melt$year, series = fl.body.melt$series,
           type = fl.body.melt$type)
  order.fhx(f)
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

#' Write an fhx object to a new FHX v2 format file.
#'
#' @param x An fhx instance.
#' @param fname Output filename.
write.fhx <- function(x, fname="") {
  if ( fname == "" ) {
    print("Please specify a character string naming a file or connection open
          for writing.")
    stop()
  }
  type.key <- list("null.year"    = ".", 
                   "recorder.year"= "|", 
                   "unknown.fs"   = "U", 
                   "unknown.fi"   = "u", 
                   "dormant.fs"   = "D", 
                   "dormant.fi"   = "d", 
                   "early.fs"     = "E", 
                   "early.fi"     = "e", 
                   "middle.fs"    = "M", 
                   "middle.fi"    = "m", 
                   "late.fs"      = "L", 
                   "late.fi"      = "l", 
                   "latewd.fs"    = "A", 
                   "latewd.fi"    = "a", 
                   "pith.year"    = "[", 
                   "bark.year"    = "]", 
                   "inner.year"   = "{", 
                   "outer.year"   = "}")
  out <- x$rings
  out$type <- vapply(out$type, function(x) type.key[[x]], "a") 
  year.range <- seq(min(out$year), max(out$year))
  filler <- data.frame(year = year.range,
                       series = rep("hackishSolution", length(year.range)),
                       type = rep(".", length(year.range)))
  out <- rbind(out, filler)
  out <- reshape2::dcast(out, year ~ series, value.var = "type", fill = ".")
  out$hackishSolution <- NULL
  # Weird thing to move year to the last column of the data.frame:
  out$yr <- out$year
  out$year <- NULL
  series.names <- rev(as.character(unique(x$rings$series)))
  no.series <- length(series.names)
  max.series.name.length <- max(sapply(series.names, nchar))
  head.line <- "FHX2 FORMAT"
  subhead.line <- paste(min(x$rings$year), no.series, max.series.name.length)
  # Vertical series name heading.
  series.heading <- matrix(" ", nrow = max.series.name.length, ncol = no.series)
  for ( i in seq(1, no.series) ) {
    ingoing <- strsplit(series.names[i], split = "")[[1]]
    n <- length(ingoing)
    series.heading[1:n, i] <- ingoing
  }
  # Now we quickly open and write to the file.
  fl <- file(fname, open = "wt")
  cat(paste(head.line, "\n", subhead.line, "\n", sep = ""),
      file = fl, sep = "")
  write.table(series.heading, fl,
              append = TRUE, quote = FALSE,
              sep = "", na = "!",
              row.names = FALSE, col.names = FALSE)
  cat("\n", file = fl, sep = "", append = TRUE)
  write.table(out, fl, 
              append = TRUE, quote = FALSE,
              sep = "", na = "!",
              row.names = FALSE, col.names = FALSE)
  close(fl)
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

#' Create an ggplot2 object for plotting.
#'
#' @param x An \code{fhx} instance.
#' @param spp Option to plot series with colors by species. A vector of species which corresponds to the series names given in \code{sppid}. Both \code{spp} and \code{sppid} need to be specified. Default plot gives no species colors.
#' @param sppid Option to plot series with colors by species. A vector of series names corresponding to species names given in \code{spp}. Every unique values in \code{x} series.names needs to have a corresponding species value. Both \code{spp} and \code{sppid} need to be specified. Default plot gives no species colors.
#' @param cluster Option to plot series with facetted by a factor. A vector of factors or characters which corresponds to the series names given in \code{clusterid}. Both \code{cluster} and \code{clusterid} need to be specified. Default plot is not facetted.
#' @param clusterid Option to plot series with facetted by a factor. A vector of series names corresponding to species names given in \code{cluster}. Every unique values in \code{x} series.names needs to have a corresponding cluster value. Both \code{cluster} and \code{clusterid} need to be specified.  Default plot is not facetted.
#' @param ylabels Optional boolean to remove y-axis (series name) labels and tick  marks. Default is TRUE.
#' @param yearlims Option to limit the plot to a range of years. This is a vector with two integers. The first integer gives the lower year for the range while the second integer gives the upper year. The default is to plot the full range of data given by \code{x}.
#' @param plot.rug A boolean option to plot a rug on the bottom of the plot. Default is FALSE.
#' @param filter.prop An optional argument if the user chooses to include a rug in their plot. This is passed to \code{rug.filter}. See this function for details.
#' @param filter.min An optional argument if the user chooses to include a rug in their plot. This is passed to \code{rug.filter}. See this function for details.
#' @param legend A boolean option allowing the user to choose whether a legend is included in the plot or not. Default is FALSE.
#' @param event.size An optional numeric that adjusts the size of fire event symbols on the plot. Default is 4.
#' @param rugbuffer.size An optional integer. If the user plots a rug, thiscontrols the amount of buffer whitespace along the y-axis between the rug and the main plot. Must be >= 2.
#' @param rugdivide.pos Optional integer if plotting a rug. Adjust the placement of the rug divider along the y-axis. Default is 2.
#' @return A ggplot object for plotting or manipulation.
ggplot.fhx <- function(x, spp, sppid, cluster, clusterid, ylabels=TRUE,
                       yearlims=FALSE, plot.rug=FALSE, filter.prop=0.25,
                       filter.min=2, legend=FALSE, event.size=4, 
                       rugbuffer.size=2, rugdivide.pos=2) {
# TODO: Merge ends and events into a single df. with a factor to handle the 
#       different event types... this will allow us to put these "fire events" and
#       "pith/bark" into a legend.
  stopifnot(rugbuffer.size >= 2)
  clean.nonrec <- subset(x$rings, x$rings$type != "recorder.year")
  scar.types <- c("unknown.fs", "dormant.fs", "early.fs",
                  "middle.fs", "late.fs", "latewd.fs")
  injury.types <- c("unknown.fi", "dormant.fi", "early.fi",
                    "middle.fi", "late.fi", "latewd.fi")
  pithbark.types <- c("pith.year", "bark.year")
  events <- subset(clean.nonrec, (type %in% scar.types) | (type %in% injury.types) | (type %in% pithbark.types))
  levels(events$type)[levels(events$type) %in% scar.types] <- "Scar"
  levels(events$type)[levels(events$type) %in% injury.types] <- "Injury"
  levels(events$type)[levels(events$type) %in% pithbark.types] <- "Pith/Bark"
  events$type <- factor(events$type, levels = c("Scar", "Injury", "Pith/Bark"))
  
  live <- aggregate(x$rings$year, by = list(x$rings$series), FUN = range, na.rm = TRUE)
  live <- data.frame(series = live$Group.1,
                     first = live$x[, 1],
                     last = live$x[, 2],
                     type = rep("non-recording", dim(live)[1]))
  recorder <- subset(x$rings, x$rings$type == "recorder.year")
  if ( dim(recorder)[1] > 0 ) {  # If there are recorder years...
    # Get the min and max of the recorder years.
    recorder <- aggregate(recorder$year,  # TODO: rename this var.
                           by = list(recorder$series, recorder$type),
                           FUN = range,
                           na.rm = TRUE)
    recorder <- data.frame(series = recorder$Group.1,
                           first = recorder$x[, 1],
                           last = recorder$x[, 2],
                           type = rep("recording", dim(recorder)[1]))
    segs <- rbind(recorder, live)
  } else {  # If there are no recorder years...
    segs <- live
  }
  levels(segs$type) <- c("Recording", "Non-recording")
  
  p <- NA
  rings <- x$rings
  if (!missing(cluster) & !missing(clusterid)) {
    merged <- merge(rings, data.frame(series = clusterid, cluster = cluster), by = "series")
    segs <- merge(segs, data.frame(series = clusterid, cluster = cluster), by = "series")
    events <- merge(events, data.frame(series = clusterid, cluster = cluster),
                    by = "series")
  }
  if (missing(spp) | missing(sppid)) {
    p <- ggplot2::ggplot(data = rings, ggplot2::aes(y = series, x = year))
    p <- (p + ggplot2::geom_segment(ggplot2::aes(x = first, xend = last, y = series, yend = series, linetype = type),
                           data = segs)
            + ggplot2::scale_linetype_manual(values = c("solid", "dashed", "solid"))
            + ggplot2::scale_size_manual(values = c(0.5, 0.5, 0.3)))
    p <- (p + ggplot2::geom_point(data = events, ggplot2::aes(shape = type), size = event.size)
            + ggplot2::scale_shape_manual(guide = "legend",
                                 values = c("Scar" = 124, "Injury" = 6, "Pith/Bark" = 20))) # `shape` 25 is empty triangles
  } else {
    merged <- merge(rings, data.frame(series = sppid, species = spp), by = "series")
    p <- ggplot2::ggplot(merged, ggplot2::aes(y = series, x = year, color = species))
    segs <- merge(segs, data.frame(series = sppid, species = spp), by = "series")
    p <- (p + ggplot2::geom_segment(ggplot2::aes(x = first, xend = last, y = series, yend = series, linetype = type),
                           data = segs)
            + ggplot2::scale_linetype_manual(values = c("solid", "dashed", "solid"))
            + ggplot2::scale_size_manual(values = c(0.5, 0.5, 0.3)))
    events <- merge(events, data.frame(series = sppid, species = spp),
                    by = "series")
    p <- (p + ggplot2::geom_point(data = events, ggplot2::aes(shape = type),
                         size = event.size, color = "black")
            + ggplot2::scale_shape_manual(guide = "legend",
                                 values = c("Scar" = 124, "Injury" = 6, "Pith/Bark" = 20))) # `shape` 25 is empty triangles
  }
  if (!missing(cluster) & !missing(clusterid)) {
    p <- p + ggplot2::facet_wrap(~ cluster, scales = "free_y")
  }
  if (plot.rug) {
    p <- (p + ggplot2::geom_rug(data = subset(rings,
                                     rings$year %in% rug.filter(d, 
                                                                filter.prop = filter.prop,
                                                                filter.min = filter.min)),
                       sides = "b", color = "black")
            + ggplot2::scale_y_discrete(limits = c(rep("", rugbuffer.size), levels(rings$series)))
            + ggplot2::geom_hline(yintercept = rugdivide.pos, color = "grey50"))
  }
  brks.major <- NA
  brks.minor <- NA
  yr.range <- diff(range(rings$year))
  if (yr.range < 100) {
      brks.major = seq(round(min(rings$year), -1),
                       round(max(rings$year), -1),
                       10)
      brks.minor = seq(round(min(rings$year), -1),
                       round(max(rings$year), -1),
                       5)
  } else if (yr.range >= 100) {
      brks.major = seq(round(min(rings$year), -2),
                       round(max(rings$year), -2),
                       100)
      brks.minor = seq(round(min(rings$year), -2),
                       round(max(rings$year), -2),
                       50)
  }
  p <- (p + ggplot2::scale_x_continuous(breaks = brks.major, minor_breaks = brks.minor)
          + ggplot2::theme_bw()
          + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                  panel.grid.minor.y = ggplot2::element_blank(),
                  axis.title.x = ggplot2::element_blank(),
                  axis.title.y = ggplot2::element_blank(),
                  legend.title = ggplot2::element_blank(),
                  legend.position = "bottom"))
  if (!legend) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  if (!missing(yearlims)) {
    p <- p + ggplot2::coord_cartesian(xlim = yearlims)
  }
  if (!ylabels) {
   p <- p + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())
  }
  p
}

#' Plot an fhx object.
#'
#' @param ... Arguments passed on to \code{ggplot.fhx}.
plot.fhx <- function(...) {
  print(ggplot.fhx(...))
}
