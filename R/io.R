#' Read input FHX file body from \code{fname} and use to return an \code{fhx} object.
#'
#' @param fname Name of target FHX file. Needs to be in format version 2.
#' @param encoding Encoding to use when reading the FHX file. The default is to use the system.
#' @return An \code{fhx} object.
read_fhx <- function(fname, encoding=getOption("encoding")) {
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
  
  type_key <- list("?" = "estimate",  # My own creation for estimated years to pith.
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
  series_names <- apply(uncleaned, 1, function(x) gsub("^\\s+|\\s+$", "", paste(x, collapse = "")))
  # series_names <- apply(uncleaned, 1, paste, collapse = "")
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
  fl_body <- strsplit(fl[(first + databuff + describe[3]) : length(fl)], split = "")
  first_year <- describe[1]
  if (length(series_names) == 1) {
    # For whatever reason R wants to flip our dims when we have a single series.
    fl_body <- as.data.frame(sapply(fl_body, function(x) x[1:describe[2]]),
                             stringsAsFactors = FALSE)
  } else {
    fl_body <- as.data.frame(t(sapply(fl_body, function(x) x[1:describe[2]])),
                             stringsAsFactors = FALSE)
  }
  # DEBUG: Should try doing the lines below as part of the above function and see the time dif. Might be a boost.
  names(fl_body) <- series_names
  fl_body$year <- seq(first_year, first_year + dim(fl_body)[1] - 1)
  fl_body_melt <- reshape2::melt(fl_body, id.vars = "year", value.name = "type",
                       variable.name = "series")
  fl_body_melt <- subset(fl_body_melt, type != ".")
  fl_body_melt$type <- vapply(fl_body_melt$type, function(x) type_key[[x]], "a") 
  fl_body_melt$type <- factor(fl_body_melt$type,
                              levels = c("null.year", "recorder.year", "unknown.fs",
                                         "unknown.fi", "dormant.fs", "dormant.fi",
                                         "early.fs", "early.fi", "middle.fs",
                                         "middle.fi", "late.fs", "late.fi",
                                         "latewd.fs", "latewd.fi", "pith.year",
                                         "bark.year", "inner.year", "outer.year",
                                         "estimate"))
  f <- fhx(year = fl_body_melt$year, series = fl_body_melt$series,
           type = fl_body_melt$type)
  sort(f, decreasing = TRUE)
}

#' Write an fhx object to a new FHX v2 format file.
#'
#' @param x An fhx instance.
#' @param fname Output filename.
write_fhx <- function(x, fname="") {
  if ( fname == "" ) {
    print("Please specify a character string naming a file or connection open
          for writing.")
    stop()
  }
  type_key <- list("null.year"    = ".", 
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
  out$type <- vapply(out$type, function(x) type_key[[x]], "a") 
  year_range <- seq(min(out$year), max(out$year))
  filler <- data.frame(year = year_range,
                       series = rep("hackishSolution", length(year_range)),
                       type = rep(".", length(year_range)))
  out <- rbind(out, filler)
  out <- reshape2::dcast(out, year ~ series, value.var = "type", fill = ".")
  out$hackishSolution <- NULL
  # Weird thing to move year to the last column of the data.frame:
  out$yr <- out$year
  out$year <- NULL
  series_names <- rev(as.character(unique(x$rings$series)))
  no_series <- length(series_names)
  max_series_name_length <- max(sapply(series_names, nchar))
  head_line <- "FHX2 FORMAT"
  subhead_line <- paste(min(x$rings$year), no_series, max_series_name_length)
  # Vertical series name heading.
  series_heading <- matrix(" ", nrow = max_series_name_length, ncol = no_series)
  for ( i in seq(1, no_series) ) {
    ingoing <- strsplit(series_names[i], split = "")[[1]]
    n <- length(ingoing)
    series_heading[1:n, i] <- ingoing
  }
  # Now we quickly open and write to the file.
  fl <- file(fname, open = "wt")
  cat(paste(head_line, "\n", subhead_line, "\n", sep = ""),
      file = fl, sep = "")
  write.table(series_heading, fl,
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
