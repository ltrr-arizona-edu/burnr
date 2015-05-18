#   Copyright 2011-2014 S. Brewster Malevich <malevich@email.arizona.edu>
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

require("ggplot2")
require("reshape2")

read.fhx <- function(fname, encoding=getOption("encoding")) {
  # Read input FHX file body from 'fname' and use to return an fhx object.
  #
  # Input:
  #   fname - Name of target FHX file. Needs to be in format version 2.
  #   encoding - Encoding to use when reading the FHX file. The default is to
  #              use whatever the system default is.
  # Output:
  #   An fhx object.
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
  
  # Define fhx class.
  f <- list(meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)     # Data frame that actually contains the ring data.
  class(f) <- "fhx"
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
      stop("The file's three-digit descriptive information on line ", first + 1, " does not match the series titles in the file. Please correct this discrepancy.")
  dim(uncleaned) <- c(describe[2], describe[3])
  #series.names <- apply(uncleaned, 1, function(x) gsub("^\\s+|\\s+$", "", paste(x, collapse = "")))
  series.names <- apply(uncleaned, 1, paste, collapse = "")
  # Filling the class with info from the fhx file body.
  fl.body <- strsplit(fl[(first + 3 + describe[3]) : length(fl)], split = "")
  first.year <- describe[1]
  fl.body <- as.data.frame(t(sapply(fl.body, function(x) x[1:describe[2]])), stringsAsFactors = FALSE)
  # DEBUG: Should try doing the lines below as part of the above function and see the time dif. Might be a boost.
  names(fl.body) <- series.names
  fl.body$year <- seq(first.year, first.year + dim(fl.body)[1] - 1)
  fl.body.melt <- melt(fl.body, id.vars = "year", value.name = "type", variable.name = "series")
  fl.body.melt <- subset(fl.body.melt, type != ".")
  fl.body.melt$type <- vapply(fl.body.melt$type, function(x) type.key[[x]], "a") 
  fl.body.melt$type <- factor(fl.body.melt$type, levels = c("null.year", "recorder.year", "unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi", "pith.year",
                                "bark.year", "inner.year", "outer.year",
                                "estimate"))
  f$rings <- fl.body.melt
  order.fhx(f)
}

rug.filter <- function(x, filter.prop=0.25, filter.min=2) {
  # Filter fire events in `x` returning years with prominent fires.
  #
  # Args:
  #   filter.prop: The proportion of fire events to recording series needed
  #     in order to be considered. Default is 0.25.
  #   filter.min: The minimum number of recording series needed to be
  #     considered a fire event. Default is 2 recording series.
  #
  # Returns:
  #   A vector of years from `x`.
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

write.fhx <- function(x, fname="") {
  # Write an fhx object to a new FHX v2 format file.
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
  # TODO: This is creating output about using type as value column. Get rid of this.
  # TODO: This also fails if there are multiple measurements for a single series year.
  out <- x$rings
  out$type <- vapply(out$type, function(x) type.key[[x]], "a") 
  year.range <- seq(min(out$year), max(out$year))
  filler <- data.frame(year = year.range,
                       series = rep("hackishSolution", length(year.range)),
                       type = rep(".", length(year.range)))
  out <- rbind(out, filler)
  out <- dcast(out, year ~ series, value.var = "type", fill = ".")
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

"+.fhx" <- function(a, b) {
  # Concatenate two fhx objects and return the combination.
  stopifnot(class(b) == "fhx")
  f <- list(meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$rings <- rbind(a$rings, b$rings)
  if (length(a$meta) | length(b$meta) > 0)  # If meta data present...
    f$meta <- c(a$meta, b$meta)
  order.fhx(resolve_duplicates(f))
}

resolve_duplicates <- function(x) {
  # Merge/remove duplicate observations in an fhx object.
  stopifnot(class(x) == "fhx")
  if (!anyDuplicated(x$rings)) {
    return(x)
  } else {
      duplicates <- x$rings[duplicated(x$rings), ]
      print(duplicates)
      stop(c(dim(duplicates)[1], " duplicate(s) found. Please resolve duplicate records."))
      # Not very elegant, and very slow. Should be written in C.
      # TODO: If I can come up with a quick check, this could work with
      # 3-replications via recursion.
      # Define constants for use in parsing.
#       SCARS <- c("unknown.fs", "dormant.fs", "early.fs",
#                  "middle.fs", "late.fs", "latewd.fs")
#       INJURIES <- c("unknown.fi", "dormant.fi", "early.fi",
#                     "middle.fi", "late.fi", "latewd.fi")
#       SOLID <- c("bark.year", "pith.year")
#       SOFT <- c("inner.year", "outer.year")
#       y <- x$rings
#       # No way to predetermine the size of these without making assumptions...?
#       type <- c()
#       year <- c()
#       series <- c()
#       # Now parse each series for each year.
#       for ( i in unique(x$rings$series) ) {
#         for ( j in seq(range(x$rings$year)) ) {
#           victim <- na.omit(x$rings$type[x$rings$series == i & x$rings$year == j])
#           victim.len <- length(victim)
#           if ( victim.len == 1 ) {
#             # If no extra observations.
#             next
#     #      } else if ( is.na(victim) ) {
#     #        # Catch errors.  # This is creating trouble. DEBUG.
#     #        stop()  # DEBUG.
#           } else if ( victim.len == 2) { # If we have two copies.
#             message(paste("Duplicate found in series", i, "year", j))
#             # Do our parsing here.
#             type.tmp <- NA
#             if ( all(victim[1] == victim) )  # See if all have the same value.
#               type.tmp <- as.character(victim[1])
#             else if ("estimate" %in% victim)
#               type.tmp <- "estimate"
#             else if ( victim[1] %in% SOFT )
#               type.tmp <- as.character(victim[1])
#             else if ( victim[1] %in% SOLID & victim[2] %in% SOFT )
#               type.tmp <- as.character(victim[2])
#             else if ( victim[1] %in% SOLID )
#               type.tmp <- as.character(victim[1])
#             else if ( victim[1] == "null.year")
#               type.tmp <- as.character(victim[2])
#             else if ( victim[1] == "recorder.year" )
#               type.tmp <- as.character(victim[2])
#             else if ( victim[1] %in% SCARS & victim[2] %in% SCARS )
#               type.tmp <- "unknown.fs"
#             else if ( victim[1] %in% SCARS & victim[2] %in% INJURIES )
#               type.tmp <- "unknown.fi"
#             else if ( victim[1] %in% INJURIES )
#               type.tmp <- "unknown.fi"
#           # TODO: Really need this to be recursive. We're only handling two
#           # instances properly.
#           } else if ( victim.len == 3 ) {  # If have three copies...
#             if ( all(victim[1] == victim) ) { 
#               type.tmp <- as.character(victim[1])
#             } else {
#             cat("There are more than two values is a series.", "\n")
#             stop()
#             }
#           } else if ( victim.len == 4 ) {
#             if ( all(victim[1] == victim) ) {
#                 type.tmp <- as.character(victim[1])
#             } else {
#               cat("There are more than two values in a series.", "\n")
#               stop()
#             }
#           } else {
#             print(i)  # DEBUG.
#             print(j)  # DEBUG.
#             cat("There are more than two values in this series:", victim, "\n")
#             stop()
#           }
#           # This is harsh because we're rewriting x$rings with each iteration.
#           y <- y[!(y$year == j & y$series == i),]
#           type <- c(type, type.tmp)
#           year <- c(year, j)
#           series <- c(series, i)
#         }
#       }
#       x$rings <- rbind(y, data.frame(series = factor(series),
#                                      year = year, 
#                                      type = factor(type, 
#                                           levels = c("null.year", "recorder.year", 
#                                                      "unknown.fs", "unknown.fi", 
#                                                      "dormant.fs", "dormant.fi", 
#                                                      "early.fs", "early.fi", 
#                                                      "middle.fs", "middle.fi", 
#                                                      "late.fs", "late.fi", 
#                                                      "latewd.fs", "latewd.fi", 
#                                                      "pith.year", "bark.year", 
#                                                      "inner.year", "outer.year", 
#                                                      "estimate")) ))
#       return(x)
  }
}

ggplot.fhx <- function(x, spp, sppid, ylabels=TRUE, yearlims=FALSE, plot.rug=FALSE, filter.prop=0.25, filter.min=2, legend=FALSE, event.size=4, rugbuffer.size=2, rugdivide.pos=1.5) {
  # Return a ggplot2 object for plotting.
  #
  # Args:
  #   x: An `fhx` instance.
  #   spp: Option to plot series with colors by species. A vector of species 
  #     which corresponds to the series names given in `sppid`. Both `spp` and 
  #     `sppid` need to be specified. Default plot gives no species colors.
  #   sppid: Option to plot series with colors by species. A vector of series 
  #     names corresponding to species names given in `spp`. Every unique 
  #     values in `x`'s series.names needs to have a corresponding species 
  #     value. Both `spp` and `sppid` need to be specified. Default plot gives
  #     no species colors.
  #   ylabels: Optional boolean to remove y-axis (series name) labels and tick 
  #     marks. Default is TRUE.
  #   yearlims: Option to limit the plot to a range of years. This is a vector 
  #     with two integers. The first integer gives the lower year for the range 
  #     while the second integer gives the upper year. The default is to plot 
  #     the full range of data given by `x`.
  #   plot.rug: A boolean option to plot a rug on the bottom of the plot.
  #     Default is FALSE.
  #   filter.prop: An optional argument if the user chooses to include a rug in 
  #     their plot. This is passed to `rug.filter()'. See this function for 
  #     details.
  #   filter.min: An optional argument if the user chooses to include a rug in 
  #     their plot. This is passed to `rug.filter()'. See this function for 
  #     details.
  #   legend: A boolean option allowing the user to choose whether a legend is 
  #     included in the plot or not. Default is FALSE.
  #   event.size: An optional numeric that adjusts the size of fire event 
  #     symbols on the plot. Default is 4.
  #   rugbuffer.size: An optional integer. If the user plots a rug, this
  #     controls the amount of buffer whitespace along the y-axis between 
  #     the rug and the main plot. Must be >= 2.
  #   rugdivide.pos: Optional integer if plotting a rug. Adjust the 
  #     placement of the rug divider along the y-axis. Default is 1.5.
  #
  # Returns:
  # A ggplot object for plotting or manipulation.
  #
  # TODO: Merge ends and events into a single df. with a factor to handle the 
  #       different event types... this will allow us to put these "fire events" and
  #       "pith/bark" into a legend.
  stopifnot(rugbuffer.size >= 2)
  clean.nonrec <- subset(x$rings, x$rings$type != "recorder.year")
  events <- subset(clean.nonrec, clean.nonrec$type %in% c("unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi"))
  
  ends <- subset(clean.nonrec, clean.nonrec$type %in% c("pith.year", "bark.year") &
                 !(clean.nonrec$type %in% c("estimate")))
#  scar.types <- c("unknown.fs", "dormant.fs", "early.fs","middle.fs", "late.fs", "latewd.fs")
#  injury.types <- c("unknown.fi", "dormant.fi","early.fi", "middle.fi", "late.fi", "latewd.fi")
#  pithbark.types <- c("pith.year", "bark.year")
#  events <- subset(clean.nonrec, (type %in% scar.types) | (type %in% injury.types) | (type %in% pithbark.types))
#  levels(events$type)[levels(events$type) %in% scar.types] <- "Scar"
#  levels(events$type)[levels(events$type) %in% injury.types] <- "Injury"
#  levels(events$type)[levels(events$type) %in% pithbark.types] <- "Pith/Bark"
#  events$type <- factor(events$type, levels = c("Scar", "Injury", "Pith/Bark"))
  
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
  levels(segs$type) <- c("recording", "non-recording", "estimate")
  
  p <- NULL
  rings <- x$rings
  if (missing(spp) | missing(sppid)) {
    p <- ggplot(data = rings, aes(y = series, x = year))
    p <- p +
         geom_segment(aes(x = first, xend = last,
                          y = series, yend = series, linetype = type), data = segs) +
         scale_linetype_manual(values = c("solid", "dashed", "solid"))
         scale_size_manual(values = c(0.5, 0.5, 0.3))
    if (dim(ends)[1] > 0)  # If we have bark and pith years.
      p <- p + geom_point(data = ends, shape = 16)  # size = 4
    if (dim(events)[1] > 0) { # If we actually have events...
      #p <- p + geom_point(data = events, shape = "|", size = event.size) # `shape` 25 is empty triangles
      injuries <- subset(events, type %in% c("unknown.fi", "dormant.fi","early.fi", "middle.fi", "late.fi", "latewd.fi"))
      scars <- subset(events, type %in% c("unknown.fs", "dormant.fs", "early.fs","middle.fs", "late.fs", "latewd.fs"))
      p <- p + geom_point(data = scars, shape = "|", size = event.size) # `shape` 25 is empty triangles
      p <- p + geom_point(data = injuries, shape = 6) # `shape` 25 is empty triangles
    }
  } else {
    merged <- merge(rings, data.frame(series = sppid, species = spp), by = "series")
    p <- ggplot(merged, aes(y = series, x = year, color = species))
    segs <- merge(segs, data.frame(series = sppid, species = spp), by = "series")
    p <- p +
         geom_segment(aes(x = first, xend = last,
                          y = series, yend = series, linetype = type), data = segs) +
         scale_linetype_manual(values = c("solid", "dashed", "solid"))
         scale_size_manual(values = c(0.5, 0.5, 0.3))
    if (dim(ends)[1] > 0)  { # If we have bark and pith years.
      ends <- merge(ends, data.frame(series = sppid, species = spp), by = "series")
      p <- p + geom_point(data = ends, shape = 16)
    }
    if (dim(events)[1] > 0) { # If we actually have events...
      events <- merge(events, data.frame(series = sppid, species = spp), by = "series")
      injuries <- subset(events, type %in% c("unknown.fi", "dormant.fi","early.fi", "middle.fi", "late.fi", "latewd.fi"))
      scars <- subset(events, type %in% c("unknown.fs", "dormant.fs", "early.fs","middle.fs", "late.fs", "latewd.fs"))
      p <- p + geom_point(data = scars, shape = "|", size = event.size, color = "black") # `shape` 25 is empty triangles
      p <- p + geom_point(data = injuries, shape = 6, color = "black") # `shape` 25 is empty triangles
    }
  }
  if (plot.rug) {
    p <- (p + geom_rug(data = subset(rings,
                                     rings$year %in% rug.filter(d, 
                                                                filter.prop = filter.prop,
                                                                filter.min = filter.min)),
                       sides = "b", color = "black")
            + scale_y_discrete(limits = c(rep("", rugbuffer.size), levels(rings$series)))
            + geom_hline(yintercept = rugdivide.pos, color = "grey50")
         )
  }
  brks.major <- NA
  brks.minor <- NA
  yr.range <- diff(range(rings$year))
  if (yr.range < 100) {
      brks.major = seq(round(min(rings$year), -1), round(max(rings$year), -1), 10)
      brks.minor = seq(round(min(rings$year), -1), round(max(rings$year), -1), 5)
  } else if (yr.range >= 100) {
      brks.major = seq(round(min(rings$year), -2), round(max(rings$year), -2), 100)
      brks.minor = seq(round(min(rings$year), -2), round(max(rings$year), -2), 25)
  }
  p <- (p + scale_x_continuous(breaks = brks.major, minor_breaks = brks.minor)
          + theme_bw()
          + theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  legend.title = element_blank(),
                  legend.position = "bottom") )
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  if (!missing(yearlims)) {
    p <- p + coord_cartesian(xlim = yearlims)
  }
  if (!ylabels) {
   p <- p + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
  }
  p
}

plot.fhx <- function(...) {
  # Plot an fhx object.
  print(ggplot.fhx(...))
}
