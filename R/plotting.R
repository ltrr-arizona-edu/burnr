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
                                 values = c("Scar" = 124, "Injury" = 6, "Pith/Bark" = 1))) # `shape` 25 is empty triangles
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
                                 values = c("Scar" = 124, "Injury" = 6, "Pith/Bark" = 1))) # `shape` 25 is empty triangles
  }
  if (!missing(cluster) & !missing(clusterid)) {
    p <- p + ggplot2::facet_wrap(~ cluster, scales = "free_y")
  }
  if (plot.rug) {
    p <- (p + ggplot2::geom_rug(data = subset(rings,
                                     rings$year %in% rug.filter(x, 
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
