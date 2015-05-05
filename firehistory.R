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
  if (length(readLines(con, n = 1)) == 0) {
    stop("file appears to be empty")
  }
  fl <- readLines(con)
  if (!any(suppressWarnings(grepl("FHX2 FORMAT", fl)))) {
    stop("cannot find line 'FHX2 FORMAT' ")
  }
  first <- suppressWarnings(grep("FHX2 FORMAT", fl))
  meta <- as.numeric(strsplit(fl[[first + 1]], " ")[[1]])
  if (length(meta) != 3) {  # First year; no. sites; length of site id.
    stop(paste("The 'meta' information that should be on line ",
               first + 1,
               " needs to have 3 elements separated by spaces."))
  }
  # TODO: Need error check that row length = meta[2] + year.
  # TODO: Need error check that first year in body is first year in meta.
  
  # Define fhx class. Oh, brave new world!
  # Trying to stay true to the vocab used in the FHX2 manual...
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
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
  # Defining the series names.
  f$series.names <- rep("", meta[2])
  series.names.bad <- strsplit(fl[(first + 2):(first + 2 + meta[3])], "")
  # TODO: These loops will be slow and need to be redone. Maybe C?
  for (i in seq(1, meta[3])) {
    for (j in seq(1, meta[2])) {
      uncleaned <- paste(f$series.names[j], 
                                 series.names.bad[[i]][j], 
                                 sep = "")
      f$series.names[j] <- gsub("^\\s+|\\s+$", "", uncleaned) # Remove leading or trailing whitespace.
    }
  }
  # Filling the class with info from the fhx file body.
  meat.bad <- strsplit(fl[(first + 3 + meta[3]) : length(fl)], split = "")
  f$first.year = meta[1]
  f$last.year = meta[1] + length(meat.bad) - 1
  tmp.year <- rep(NA, length(meat.bad) * length(f$series))
  tmp.series <- rep(NA, length(meat.bad) * length(f$series))
  tmp.type <- factor(rep(NA, length(meat.bad) * length(f$series)),
                     levels = c("null.year", "recorder.year", "unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi", "pith.year",
                                "bark.year", "inner.year", "outer.year",
                                "estimate"))
  k <- 0
  for (i in seq(1, length(meat.bad))) {
    yr <- as.numeric(paste(meat.bad[[i]][(meta[2] + 1):length(meat.bad[[i]])],
                           sep = "", collapse = ""))
    for (j in seq(1, meta[2])) {
      k <- k + 1
      target <- meat.bad[[i]][j]
      tmp.year[k] <- yr
      tmp.type[k] <- type.key[[target]]
      tmp.series[k] <- f$series.name[j]
    }
  }
  # Need f$rings$year as type int to get around casting problems with writing
  # the fhx file out from R.
  f$rings <- data.frame(year = as.integer(tmp.year),
                        type = tmp.type, 
                        series = tmp.series)
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
  event_count <- as.data.frame(table(subset(x$rings, x$rings$type %in% event)$year))
  recording_count <- as.data.frame(table(subset(x$rings, x$rings$type %in% recording)$year))
  counts <- merge(event_count, recording_count, by = "Var1")
  counts$prop <- counts$Freq.x / counts$Freq.y
  conditions <- (counts$prop >= filter.prop) & (counts$Freq.x >= filter.min)
  out <- subset(counts, conditions)$Var1
  as.integer(levels(out)[out])
}

fhx <- function(est) {
  # Read input "establishment" data.frame return an fhx object.
  #
  # Input:
  #   est - "establishment" data.frame.
  #
  # Output:
  #   An fhx object.
  stopifnot(class(est) == "data.frame")
  stopifnot(c("unique", "site", "plot", "tree", "species",
              "pith.date", "inner.ring.date", "outer.ring.date",
              "bark.date") %in% names(est))
  # Draw out the columns we need from est:
  est <- data.frame("unique" = est$unique,
                    "site" = est$site,
                    "plot" = est$plot,
                    "tree" = est$tree,
                    "species" = est$species,
                    "pith.date" = est$pith.date,
                    "inner.ring.date" = est$inner.ring.date,
                    "outer.ring.date" = est$outer.ring.date,
                    "bark.date" = est$bark.date)
  #Cleaning
  est <- subset(est, 
                (!(is.na(est$pith.date) & is.na(est$inner.ring.date)) |
                 !(is.na(est$bark.date) & is.na(est$outer.ring.date))) )
  est$unique <- factor(est$unique)  # To reset the levels.

  # Trying to stay true to the vocab used in the FHX2 manual...
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$series.names <- unique(est$unique)
  f$meta$est <- data.frame("series" = est$unique,
                           "site" = est$site,
                           "plot" = est$plot,
                           "tree" = est$tree,
                           "species" = est$species)
 
  est <- est[, c("unique", "pith.date", "inner.ring.date",
                 "outer.ring.date", "bark.date")]
  f$first.year <- with(est, min(c(min(pith.date, na.rm = TRUE),
                                  min(inner.ring.date, na.rm = TRUE))))
  f$last.year <- with(est, max(c(max(bark.date, na.rm = TRUE),
                                 max(outer.ring.date, na.rm = TRUE))))
  est.melt <- melt(est, id = "unique")
  tmp.year <- rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)) 
  tmp.type <- factor(rep(NA, length(f$series.names) * (f$last.year - f$first.year + 1)),
                     levels = c("null.year", "recorder.year", "unknown.fs",
                                "unknown.fi", "dormant.fs", "dormant.fi",
                                "early.fs", "early.fi", "middle.fs",
                                "middle.fi", "late.fs", "late.fi",
                                "latewd.fs", "latewd.fi", "pith.year",
                                "bark.year", "inner.year", "outer.year",
                                "estimate"))
  tmp.series <- rep(NA, length(f$series.names) * (f$last.year - f$first.year))
  yr.seq <- seq(f$first.year, f$last.year)
  k <- 0
  for ( i in seq(1, length(f$series.names)) ) {
    s <- f$series.names[i]
    tmp.pith.date <- est$pith.date[est$unique == s]
    tmp.inner.ring.date <- est$inner.ring.date[est$unique == s]
    tmp.outer.ring.date <- est$outer.ring.date[est$unique == s]
    tmp.bark.date <- est$bark.date[est$unique == s]
    for ( j in seq(1, length(yr.seq)) ) {
      k <- k + 1
      #print(k)  # DEBUG
      foo <- NA
      y <- yr.seq[j]
      # Now to try to catch some errors:
      if ( is.na(tmp.pith.date) & is.na(tmp.inner.ring.date) )
        next
      if ( is.na(tmp.outer.ring.date) & is.na(tmp.bark.date) )
        next
      # This is a mess, especially in dealing with NAs. Very sorry.
      if ( y < min(c(tmp.pith.date, tmp.inner.ring.date), na.rm = TRUE) )  {
        foo <- "null.year"
      } else if ( (!is.na(tmp.pith.date) & y == tmp.pith.date) & 
                 (is.na(tmp.inner.ring.date) | 
                  tmp.pith.date <= tmp.inner.ring.date) ) {
        foo <- "pith.year"
      } else if ( (!is.na(tmp.pith.date) & y > tmp.pith.date) & 
                 ( !is.na(tmp.inner.ring.date) & y < tmp.inner.ring.date) &
                 (!is.na(tmp.pith.date) & !is.na(tmp.inner.ring.date)) ) {
        foo <- "estimate"
      } else if ( (!is.na(tmp.inner.ring.date) & y == tmp.inner.ring.date) & 
                 (is.na(tmp.pith.date) | 
                  tmp.pith.date != tmp.inner.ring.date) ) {
        foo <- "inner.year"
      } else if ( ((!is.na(tmp.inner.ring.date) & y > tmp.inner.ring.date) | 
                   (is.na(tmp.inner.ring.date) & y > tmp.pith.date)) & 
                 y < min(c(tmp.outer.ring.date, tmp.bark.date), na.rm = TRUE) ) {
        foo <- "null.year"
      } else if ( !is.na(tmp.outer.ring.date) & 
                 y == tmp.outer.ring.date & 
                 is.na(tmp.bark.date) ) {
        foo <- "outer.year"
      } else if ( !is.na(tmp.bark.date) & y == tmp.bark.date ) {
        foo <- "bark.year"
      } else if ((!is.na(tmp.bark.date) & y > tmp.bark.date ) |
                 (is.na(tmp.bark.date) & y > tmp.outer.ring.date) |
                 (tmp.bark.date == tmp.outer.ring.date & y > tmp.bark.date) ) {
        foo <- "null.year"
      } else {
        # If all else fails, a weak debug message:
        cat("\n\nSomething went wrong! Here is some debugging data:\n")
        cat(paste("series name: ", s, "\n", "iter year: ", y, "\n", sep = ""))
        cat(paste("pith.date: ", tmp.pith.date, "\n",
                  "inner.ring.date: ", tmp.inner.ring.date, "\n",
                  "outer.ring.date: ", tmp.outer.ring.date, "\n",
                  "bark.date: ", tmp.bark.date, "\n", sep = ""))
        stop()
      }
      tmp.year[k] <- y
      tmp.series[k] <- as.character(s)  # Gets around inserting numbers bug.
      tmp.type[k] <- foo
    }
  }
  f$rings <- data.frame(year = tmp.year, series = factor(tmp.series),
                        type = factor(tmp.type, 
                                      levels = c("null.year", "recorder.year", 
                                                 "unknown.fs", "unknown.fi", 
                                                 "dormant.fs", "dormant.fi", 
                                                 "early.fs", "early.fi", 
                                                 "middle.fs", "middle.fi", 
                                                 "late.fs", "late.fi", 
                                                 "latewd.fs", "latewd.fi", 
                                                 "pith.year", "bark.year", 
                                                 "inner.year", "outer.year", 
                                                 "estimate")))
  order.fhx(f)
}


read.establish <- function(fname, encoding=getOption("encoding")) {
  # Read input FHX file body from 'fname' and use to return an fhx object.
  #
  # Input:
  #   fname - Name of target establishment file.
  #   encoding - Encoding to use when reading the file. The default is to
  #              use whatever the system default is.
  # Output:
  #   An fhx object.
  est <- read.csv(fname,
                  encoding = encoding,
                  na.strings = c(-999, "NA"),
                  row.names = NULL)
  fhx(est)
}


write.fhx <- function(x, fname="") {
  # Write an fhx object to a new FHX v2 format file.
  if ( fname == "" ) {
    print("Please specify a character string naming a file or connection open
          for writing.")
    stop()
  }
  type.key <- list("estimate"     = "?", 
                   "null.year"    = ".", 
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
  x <- estimate.omit(x)
  victims <- x$rings
  victims <- data.frame(year = x$rings$year,
                        series = x$rings$series,
                        type = unlist(type.key[as.character(x$rings$type)]))
  # TODO: This is creating output about using type as value column. Get rid of this.
  # TODO: This also fails if there are multiple measurements for a single series year.
  out <- dcast(victims, year ~ series)

  # Weird thing to move year to the last column of the data.frame.
  out$yr <- out$year
  out$year <- NULL
  
  no.series <- length(x$series.names)
  max.series.name.length <- max(sapply(x$series.names, nchar))
  head.line <- "FHX2 FORMAT"
  subhead.line <- paste(x$first.year, no.series, max.series.name.length)

  # Now for the vertical series name heading. Ugg...
  series.heading <- matrix(" ", ncol = max.series.name.length, nrow = no.series)
  for ( i in seq(1, no.series) ) {
    ingoing <- strsplit(x$series.names[i], split = "")[[1]]
    n <- length(ingoing)
    series.heading[i, 1:n] <- ingoing
  }
  series.heading <- t(series.heading)

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


estimate.omit <- function(x) {
  # Remove estimated pith and ring values from an fhx object, x.
  # This is needed because the FHX2 format has no specs for estimated pith.
  # TODO: This needs testing.
  stopifnot(class(x) == "fhx")
  # This gets messy:
  for (i in x$series.names) {  # Remove estimate pith.years.
    if ( !"estimate" %in% x$rings$type[x$rings$series == i] ) {  # Has no estimate year?
      next
    } else if ( "pith.year" %in% x$rings$type[x$rings$series == i] ) {  # Has a pith year?
      pith.yr <- x$rings$year[x$rings$type == "pith.year" & x$rings$series == i]
      if ( x$rings$type[x$rings$year == pith.yr + 1 & x$rings$series == i] == "estimate" ) {  # If next year is estimate...
        x$rings$type[x$rings$year == pith.yr & x$rings$series == i] <- "null.year"
      }
    }
  }
  # Turn all "estimate" to "null.year".
  x$rings$type[x$rings$type == "estimate"] <- "null.year"
  x
}

order.fhx <- function(x) {
  stopifnot(class(x) == "fhx")
  test <- subset(x$rings,
                 x$rings$type == "inner.year" | x$ring$type == "pith.year")
  i <- order(test$year, decreasing = TRUE)
  x$rings$series <- factor(x$rings$series,
                           levels = unique(test$series[i]),
                           ordered = TRUE)
  x$series.names <- levels(x$rings$series)  # Push the ordering to series.names
  i <- order(x$rings$series, x$rings$year, decreasing = TRUE)
  x$rings <- x$rings[i, ]
  x
}


"+.fhx" <- function(a, b) {
  # Concatenate two fhx objects and return the combination.
  stopifnot(class(b) == "fhx")
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$first.year <- min(a$first.year, b$first.year)
  f$last.year <- max(a$last.year, b$last.year)
  f$series.names <- c(a$series.names, b$series.names)
  f$rings <- rbind(a$rings, b$rings)
  if ( length(a$meta) | length(b$meta) > 0 ) {  # If meta data present...
    f$meta <- c(a$meta, b$meta)
  }
  tmp.year <- c()
  tmp.series <- c()
  tmp.type <- c()
  # Stuffing data so that it matches the combined year-range of a and b.
  for ( i in f$series.names ) {
    i.min <- with(f$rings, min(year[series == i], na.rm = TRUE))
    i.max <- with(f$rings, max(year[series == i], na.rm = TRUE))
    low.seq <- c()
    high.seq <- c()
    if (i.min > f$first.year)
      low.seq <- c(seq(i.min, f$first.year), low.seq)
    if (i.max < f$last.year)
      high.seq <- c(high.seq, seq(i.max, f$last.year))
    tmp.year <- c(tmp.year, low.seq, high.seq)
    tmp.series <- c( tmp.series, rep(i, length(c(low.seq, high.seq))) )
  }
  tmp.type <- rep("null.year", length(tmp.year))
  tmp.rings <- data.frame(year = tmp.year,
                          series = tmp.series,
                          type = tmp.type)
  f$rings <- rbind(f$rings, tmp.rings)
  f <- remove_duplicates(f)  # DEBUG
  order.fhx(f)
}

remove_duplicates <- function(x) {
  # Merge/remove duplicate observations in an fhx object.
  stopifnot(class(x) == "fhx")
  # Not very elegant, and very slow. Should be written in C.
  # TODO: If I can come up with a quick check, this could work with
  # 3-replications via recursion.
  
  # Define constants for use in parsing.
  SCARS <- c("unknown.fs", "dormant.fs", "early.fs",
             "middle.fs", "late.fs", "latewd.fs")
  INJURIES <- c("unknown.fi", "dormant.fi", "early.fi",
                "middle.fi", "late.fi", "latewd.fi")
  SOLID <- c("bark.year", "pith.year")
  SOFT <- c("inner.year", "outer.year")
  y <- x$rings
  # No way to predetermine the size of these without making assumptions...?
  type <- c()
  year <- c()
  series <- c()

#                     levels = c("null.year", "recorder.year", "unknown.fs",
#                                "unknown.fi", "dormant.fs", "dormant.fi",
#                                "early.fs", "early.fi", "middle.fs",
#                                "middle.fi", "late.fs", "late.fi",
#                                "latewd.fs", "latewd.fi", "pith.year",
#                                "bark.year", "inner.year", "outer.year",
#                                "estimate"))

  # Now parse each series for each year.
  for ( i in x$series.names ) {
    for ( j in seq(x$first.year, x$last.year) ) {
      # Having troubles with large no. of NAs in 1313 of LYT18D45...???
      victim <- na.omit(x$rings$type[x$rings$series == i & x$rings$year == j])
      victim.len <- length(victim)
      if ( victim.len == 1 ) {
        # If no extra observations.
        next
#      } else if ( is.na(victim) ) {
#        # Catch errors.  # This is creating trouble. DEBUG.
#        stop()  # DEBUG.
      } else if ( victim.len == 2) { # If we have two copies.
        message(paste("Duplicate found in series", i, "year", j))
        # Do our parsing here.
        type.tmp <- NA
        if ( all(victim[1] == victim) )  # See if all have the same value.
          type.tmp <- as.character(victim[1])
        else if ("estimate" %in% victim)
          type.tmp <- "estimate"
        else if ( victim[1] %in% SOFT )
          type.tmp <- as.character(victim[1])
        else if ( victim[1] %in% SOLID & victim[2] %in% SOFT )
          type.tmp <- as.character(victim[2])
        else if ( victim[1] %in% SOLID )
          type.tmp <- as.character(victim[1])
        else if ( victim[1] == "null.year")
          type.tmp <- as.character(victim[2])
        else if ( victim[1] == "recorder.year" )
          type.tmp <- as.character(victim[2])
        else if ( victim[1] %in% SCARS & victim[2] %in% SCARS )
          type.tmp <- "unknown.fs"
        else if ( victim[1] %in% SCARS & victim[2] %in% INJURIES )
          type.tmp <- "unknown.fi"
        else if ( victim[1] %in% INJURIES )
          type.tmp <- "unknown.fi"
      # TODO: Really need this to be recursive. We're only handling two
      # instances properly.
      } else if ( victim.len == 3 ) {  # If have three copies...
        if ( all(victim[1] == victim) ) { 
          type.tmp <- as.character(victim[1])
        } else {
        cat("There are more than two values is a series.", "\n")
        stop()
        }
      } else if ( victim.len == 4 ) {
        if ( all(victim[1] == victim) ) {
            type.tmp <- as.character(victim[1])
        } else {
          cat("There are more than two values in a series.", "\n")
          stop()
        }
      } else {
        print(i)  # DEBUG.
        print(j)  # DEBUG.
        cat("There are more than two values in this series:", victim, "\n")
        stop()
      }
      # This is harsh because we're rewriting x$rings with each iteration.
      y <- y[!(y$year == j & y$series == i),]
      type <- c(type, type.tmp)
      year <- c(year, j)
      series <- c(series, i)
    }
  }
  x$rings <- rbind(y, data.frame(series = factor(series),
                                 year = year, 
                                 type = factor(type, 
                                      levels = c("null.year", "recorder.year", 
                                                 "unknown.fs", "unknown.fi", 
                                                 "dormant.fs", "dormant.fi", 
                                                 "early.fs", "early.fi", 
                                                 "middle.fs", "middle.fi", 
                                                 "late.fs", "late.fi", 
                                                 "latewd.fs", "latewd.fi", 
                                                 "pith.year", "bark.year", 
                                                 "inner.year", "outer.year", 
                                                 "estimate")) ))
  x$series.names <- factor(unique(x$rings$series))
  x
}

ggplot.fhx <- function(x, spp, sppid, yearlims=FALSE, rug=FALSE, filter.prop=0.25, filter.min=2, legend=FALSE, event.size=4, rugbuffer.size=2) {
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
  #   yearlims: Option to limit the plot to a range of years. This is a vector 
  #     with two integers. The first integer gives the lower year for the range 
  #     while the second integer gives the upper year. The default is to plot 
  #     the full range of data given by `x`.
  #   rug: A boolean option to plot a rug on the bottom of the plot. Default is 
  #     FALSE.
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
  #
  # Returns:
  # A ggplot object for plotting or manipulation.
  #
  # TODO: Merge ends and events into a single df. with a factor to handle the 
  #       different event types... this will allow us to put these "fire events" and
  #       "pith/bark" into a legend.
  stopifnot(rugbuffer.size >= 2)
  clean <- subset(x$rings, x$rings$type != "null.year")
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
    # TODO: This is a problem because it will plot on top of "estimate" years.
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
  if ( dim(estimate)[1] > 0 ) { # If we have estimate years.
    segs <- rbind(segs, estimate)
  }
  levels(segs$type) <- c("recording", "non-recording", "estimate")
  #levels(events$type) <- c()
  #levels(ends$type) <- c()
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
      p <- p + geom_point(data = events, shape = "|", size = event.size) # `shape` 25 is empty triangles
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
    if (dim(ends)[1] > 0)  # If we have bark and pith years.
      ends <- merge(ends, data.frame(series = sppid, species = spp), by = "series")
      p <- p + geom_point(data = ends, shape = 16)
    if (dim(events)[1] > 0) { # If we actually have events...
      events <- merge(events, data.frame(series = sppid, species = spp), by = "series")
      p <- p + geom_point(data = events, shape = "|", size = event.size, color = "black")
    }
  }
  if (rug) {
    p <- (p + geom_rug(data = subset(rings,
                                     rings$year %in% rug.filter(d, 
                                                                filter.prop = filter.prop,
                                                                filter.min = filter.min)),
                       sides = "b", color = "black")
            + scale_y_discrete(limits = c(rep("", rugbuffer.size), levels(rings$series)))
            + geom_hline(yintercept = 2, color = "grey50")
         )
  }
  p <- (p + scale_x_continuous(breaks = seq(round(min(rings$year), -2), round(max(rings$year), -2), 100),
                               minor_breaks = seq(round(min(rings$year), -2), round(max(rings$year), -2), 25))
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
  p
}

plot.fhx <- function(...) {
  # Plot an fhx object.
  print(ggplot.fhx(...))
}

compress <- function(x, series.name, compress.p = 0.2) {
  # Create a compressed or composite FHX object from a larger FHX object.
  #
  # Input:
  #   x - The target FHX object.
  #   series.name - A string giving the desired name of the composite.
  #       Must be no greater than 8 characters.
  #   compress.p - For each year, if the proportion of total series is >= 
  #       compress.p, an unknown fire injury will be noted for that year.
  # 
  # Output:
  #   An FHX object representing a composite or compression of the original.
  stopifnot(class(x) == "fhx")
  stopifnot(compress.p > 0 & compress.p < 1)
  stopifnot(nchar(series.name) < 9)
  year.seq <- seq(x$first.year, x$last.year)
  year.n <- length(year.seq)
  series.n <- length(x$series.names)
  values <- factor(rep("null.year", year.n),
                   levels = c("null.year", "recorder.year", "unknown.fs",
                              "unknown.fi", "dormant.fs", "dormant.fi",
                              "early.fs", "early.fi", "middle.fs",
                              "middle.fi", "late.fs", "late.fi",
                              "latewd.fs", "latewd.fi", "pith.year",
                              "bark.year", "inner.year", "outer.year",
                              "estimate"))
  targets = c("unknown.fs", "unknown.fi", 
              "dormant.fs", "dormant.fi", 
              "early.fs", "early.fi", 
              "middle.fs", "middle.fi", 
              "late.fs", "late.fi", 
              "latewd.fs", "latewd.fi") 
  for ( i in seq(1, year.n) ) {
    count <- dim(subset(x$rings, x$rings$year == year.seq[i] & x$rings$type %in% targets))[1]
    if ( (count / series.n) >= compress.p )
      values[i] <- "unknown.fs"
  }
  values[1] <- "inner.year"
  values[year.n] <- "outer.year"
  f <- list(first.year = NA,  # First year of all the series.
            last.year = NA,  # Last year of all the series.
            series.names = NA,  # Ordered factor of the series.names.
            meta = list(),  # Odd list for collecting various bits of metadata.
            rings = NA)  # Data frame that actually contains the ring data.
  class(f) <- "fhx"
  f$first.year <- x$first.year
  f$last.year <- x$last.year
  f$series.names <- c(series.name)
  f$rings <- data.frame(year = year.seq,
                        type = values, 
                        series = rep(series.name, year.n))
  f
}