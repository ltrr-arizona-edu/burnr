# Little tools specifically for Emily Heyerdahl (USFS) and Don Falk.

#   Copyright 2011, 2012 S. Brewster Malevich <malevich@email.arizona.edu>
#
#   This is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>

source("firehistory.R")

ParseSite <- function(s, scar.est.file, est.file, scar.fhx.file) {
# Parse establishment and FHX files with output to a specific file.
#
# Args:
#   s : The target site.
#   scar.est.file : The establishment file for scarred trees.
#   est.file : The big establishment file.
#   scar.fhx.file: 
#
# Returns:
#   Nothing. An FHX file will be written in the curren working directory named
#   <site>FDD.fhx.
scar <- read.csv(scar.est.file, na.strings = c(-999), row.names = NULL)
est <- read.csv(est.file, na.strings = c(-999), row.names = NULL)

fl <- paste(s, "FDD.fhx", sep = "")
scar.fhx <- read.fhx(scar.fhx.file)  # This may take a long time.
scar.fhx <- PlaceMeta(scar.fhx, scar)
est.site <- subset(est, est$site == s)
scar.fhx.site <- GetSite(scar.fhx, site = s)
est.site.fhx <- fhx(est.site)  # Also, very very slow.
all.fhx <- scar.fhx.site + est.site.fhx
all.fhx <- AddSpp(all.fhx)
all.fhx <- SpecialSort(all.fhx)
write.fhx(all.fhx, fl)
}

ParsePlot <- function(s, p, scar.est.file, est.file, scar.fhx.file) {
# Parse establishment and FHX files with output to a specific file.
#
# Args:
#   s : The target site.
#   p : The target plot.
#   scar.est.file : The establishment file for scarred trees.
#   est.file : The big establishment file.
#   scar.fhx.file: 
#
# Returns:
#   Nothing. An FHX file will be written in the curren working directory named
#   <site><plot>FDD.fhx.
scar <- read.csv(scar.est.file, na.strings = c(-999), row.names = NULL)
est <- read.csv(est.file, na.strings = c(-999), row.names = NULL)

fl <- paste(s, p, "FDD.fhx", sep = "")

scar.fhx <- read.fhx(scar.fhx.file)  # This may take a long time.
scar.fhx <- PlaceMeta(scar.fhx, scar)

# Subset to target site.
est.site <- subset(est, est$site == s)
scar.fhx.site <- GetSite(scar.fhx, site = s)

# Subset to target plot.
est.site.plot <- subset(est.site, est$plot == p)
scar.fhx.site.plot <- GetPlot(scar.fhx.site, plt = p)
est.site.plot.fhx <- fhx(est.site.plot)  # Also, very very slow.

# Combine, parse and output.
all.fhx <- scar.fhx.site.plot + est.site.plot.fhx
all.fhx <- AddSpp(all.fhx)
all.fhx <- SpecialSort(all.fhx)
write.fhx(all.fhx, fl)
}

SpecialSort <- function(x) {
  # Sort an fhx object, first by the first three-letters (related to species)
  # of the series name and then by the earliest inner.year or pith.year.
  # Note that this is a project specific ordering..
  stopifnot(class(x) == "fhx")
  test <- subset(x$rings,
                 x$rings$type == "inner.year" | x$ring$type == "pith.year")
  test$spp <- substr(test$series, 1, 3)  # Create spp column.
  # Ordered levels for spp factor.
  # TODO: Keep in mind that this might need to be reversed.
  spp.level <- c("TSM", "PIA", "PIC", "ABL", "PIM",
                 "LAO", "ABG", "CAD", "PSM", "PIP",
                 "JUO")
  test$spp <- factor(test$spp, levels = rev(spp.level), ordered = TRUE)
  i <- order(test$spp, -test$year, decreasing = TRUE)
  x$rings$series <- factor(x$rings$series,
                           levels = unique(test$series[i]),
                           ordered = TRUE)
  x$series.names <- levels(x$rings$series)  # Push the ordering to series.names
  i <- order(x$rings$series, x$rings$year, decreasing = TRUE)
  x$rings <- x$rings[i, ]
  x
}
#SpecialSort <- function(x) {
#  # Sort an fhx object first by species and then by earliest year.
#  stopifnot(class(x) == "fhx")
#  y <- x
#  SPP <- c("TSME", "PIAL", "PICO", "ABLA", "PIMO", "LAOC",
#           "ABGR", "CADE", "PSME", "PIPO", "JUOC")
#  x <- MergeMeta(x)
#  test <- subset(x$rings,
#                 x$rings$type == "inner.year" | x$ring$type == "pith.year")
#  test <- merge(test, x$meta, join = series)
#  stopifnot(test$species %in% SPP)
#  test$species <- factor(test$species, levels = SPP, ordered = TRUE)
#  i <- order(test$species, test$year, decreasing = TRUE)
#  x$rings$series <- factor(x$rings$series,
#                           levels = unique(test$series[i]),
#                           ordered = TRUE)
#  x$series.names <- levels(x$rings$series)  # Push the ordering to series.names
#  i <- order(x$rings$series, x$rings$year, decreasing = TRUE)
#  y$rings <- x$rings[i, ]
#  x
#}

PlaceMeta <- function(x, y) {
  # Place meta data data.frame y in fhx object x.
  stopifnot(class(x) == "fhx")
  stopifnot(class(y) == "data.frame")
  # We're only adding meta data for the sites which are in the fhx obj.
  y.sub <- subset(y, y$unique %in% x$series.names)
  y.sub <- data.frame("series" = y.sub$unique,
                      "site" = y.sub$site,
                      "plot" = y.sub$plot,
                      "tree" = y.sub$tree,
                      "species" = y.sub$species)
  x$meta[["scar"]] <- y.sub
  x
}


GetSite <- function(x, site="") {
  # Subset an fhx object to get obs from a specific site.
  stopifnot(class(x) == "fhx")
  stopifnot(site != "")
  interest <- data.frame(series = x$meta$scar$series,
                         site = x$meta$scar$site)
  out <- merge(x$rings, interest, by = "series")

  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$rings <- subset(out, out$site == site)
  f$rings$site <- NULL
  f$meta$scar <- subset(x$meta$scar, x$meta$scar$site == site)
  f$first.year <- min(f$rings$year, na.rm = TRUE)
  f$last.year <- max(f$rings$year, na.rm = TRUE)
  order.fhx(f)
}

GetPlot <- function(x, plt="") {
  # Subset an fhx object to get obs from a specific plot.
  stopifnot(class(x) == "fhx")
  stopifnot(plt != "")
  interest <- data.frame(series = x$meta$scar$series,
                         plot = x$meta$scar$plot)
  out <- merge(x$rings, interest, by = "series")

  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$rings <- subset(out, out$plot == plt)
  f$rings$plot <- NULL
  f$meta$scar <- subset(x$meta$scar, x$meta$scar$plot == plt)
  f$first.year <- min(f$rings$year, na.rm = TRUE)
  f$last.year <- max(f$rings$year, na.rm = TRUE)
  order.fhx(f)
}


AddSpp <- function(x) {
  #Replace 3-letter site code at the beginning of the series.names with 3-letters
  #of project-specific species codes.
  stopifnot(class(x) == "fhx")
  stopifnot( all(c("scar", "est") %in% names(x$meta)) )
  # Build a list-key to translate the series names.
  name.key <- list()
  for ( i in seq(1, length(x$series.names)) ) {
    target.spp <- NA
    if (x$series.names[i] %in% x$meta$est$series) {
      target.spp <- x$meta$est$species[x$meta$est$series == x$series.names[i]]
    } else if (x$series.names[i] %in% x$meta$scar$series) {
      target.spp <- x$meta$scar$species[x$meta$scar$series == x$series.names[i]]
    } else {
      # If all else fails, print a crappy error message.
      cat(x$series.names[i], "does not appear to be in the meta data.")
      stop()
    }
    name.key[[ x$series.names[i] ]] <- paste(substr(target.spp[1], 1, 3), substr(x$series.names[i], 4, nchar(x$series.names[i])), sep = "")
  }

  # Now the actual translation.
  x$rings$series <- unlist(name.key[x$rings$series])
  order.fhx(x)  # This may be mucking up our special sort.
}

MergeMeta <- function(x) {
  # Were needed, will merge scar and est meta data in an fhx object.
  stopifnot(class(x) == "fhx")
  stopifnot(("scar" %in% names(x$meta)) | ("est" %in% names(x$meta)))
  if ( all(c("scar", "est") %in% names(x$meta)) ) {
    stopifnot(c("series", "site", "plot", "tree", "species") %in% names(x$meta$scar))
    stopifnot(c("series", "site", "plot", "tree", "species") %in% names(x$meta$est))
  }
  # TODO: test if x$meta is just scar or est, then just return x.
  x$meta <- rbind(x$meta$scar, x$meta$est)
  x
}

BFP <- function(x, ylabs=TRUE, vline=FALSE, nrow=NULL, ncol=NULL) {
  print(ggplot.BFP(x, ylabs=ylabs, vline=vline, nrow=nrow, ncol=ncol))
}

ggplot.BFP <- function(x, ylabs=TRUE, vline=FALSE, nrow=NULL, ncol=NULL) {
  stopifnot(is.logical(ylabs))
  stopifnot(is.logical(vline))
  stopifnot(class(x) == "fhx")
  stopifnot(("scar" %in% names(x$meta)) | ("est" %in% names(x$meta)))
  y <- x$rings
  x <- MergeMeta(x)
  clean <- subset(y, y$type != "null.year")
  clean.nonrec <- subset(clean, clean$type != "recorder.year")
  events <- subset(clean.nonrec, clean.nonrec$type %in% c("unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi"))
  ends <- subset(clean.nonrec, clean.nonrec$type %in% c("pith.year", "bark.year") &
                 !(clean.nonrec$type %in% c("estimate")))

  estimate <- subset(clean, clean$type == "estimate")
  live <- NA
  if ( dim(estimate)[1] > 0 ) {  # If we have estimate years.
    # Get the min and max of the estimate years.
    estimate <- aggregate(estimate$year,
                          by = list(estimate$series),
                          FUN = range,
                          na.rm = TRUE)
    estimate <- data.frame(series = estimate$Group.1,
                           first = estimate$x[, 1],
                           last = estimate$x[, 2],
                           type = rep("estimate", dim(estimate)[1]))
    live <- subset(clean, clean$type != "estimate")
    # Get min and max of living years.
    live <- aggregate(live$year,
                      by = list(live$series),
                      FUN = range,
                      na.rm = TRUE)
  } else {  # If we don't have estimate years.
    live <- aggregate(clean$year,
                      by = list(clean$series),
                      FUN = range,
                      na.rm = TRUE)
  }
  live <- data.frame(series = live$Group.1,
                     first = live$x[, 1],
                     last = live$x[, 2],
                     type = rep("non-recording", dim(live)[1]))

  recorder <- subset(y, y$type == "recorder.year")
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
  if ( dim(estimate)[1] > 0 ) { # If we have estimate years.
    segs <- rbind(segs, estimate)
  }
  levels(segs$type) <- c("recording", "non-recording", "estimate")
  #levels(events$type) <- c()
  #levels(ends$type) <- c()
  
  y <- merge(y, x$meta, join = series)
  segs <- merge(segs, x$meta, join = series)
  ends <- merge(ends, x$meta, join = series)
  events <- merge(events, x$meta, join = series)
  
  p <- ggplot(data = y, aes(y = series, x = year, color = species))
  p <- p +
       geom_segment(aes(x = first, xend = last,
                        y = series, yend = series, linetype = type, size = type),
                        data = segs) +
       scale_linetype_manual(values = c(1, 3, 1)) +
       scale_size_manual(values = c(0.5, 0.5, 0.3)) + 
       geom_point(data = ends, shape = 16)
  if (dim(events)[1] > 0) { # If we actually have events...
    p <- p + geom_point(data = events, shape = 25)
    if (vline == TRUE)
        p <- p + geom_vline(xintercept = events$year, alpha = 0.05, size = 1.5) 
  }
  p <- p + facet_wrap(~ plot, ncol = ncol, nrow = nrow, scales = "free_y")
  if (ylabs == FALSE)
    p <- p + scale_y_discrete(breaks = NULL)
  p
}
