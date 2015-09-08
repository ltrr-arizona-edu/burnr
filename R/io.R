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
