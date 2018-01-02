#' Read FHX2 file and return an \code{fhx} object.
#'
#' @param fname Name of target FHX file. Needs to be in format version 2.
#' @param encoding Encoding to use when reading the FHX file. The default is to use the system.
#' @param text Character string. If \code{fname} is not provided and 
#'   \code{text} is, then data is read from \code{text} using a text connection.
#'
#' @return An \code{fhx} object.
#'
#' @examples
#' \dontrun{
#' d <- read_fhx('afile.fhx')
#' }
#'
#' @export
read_fhx <- function(fname, encoding, text) {
  if (missing(encoding))
    encoding <- getOption('encoding')
  if (missing(fname) && !missing(text)) {
    con <- textConnection(text)
    on.exit(close(con))
  }
  if (!missing(fname)) {
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
  }
  # Error checking and basic variables.
  if (length(readLines(con, n = 1)) == 0)
    stop("file appears to be empty")
  fl <- readLines(con, warn = FALSE)
  if (!any(suppressWarnings(grepl("^FHX2 FORMAT|^FIRE2 FORMAT", fl, ignore.case = TRUE))))
    stop("Cannot find line 'FHX2 FORMAT' or 'FIRE2 FORMAT'.")
  first <- suppressWarnings(grep("^FHX2 FORMAT|^FIRE2 FORMAT", fl, ignore.case = TRUE))
  describe <- as.numeric(strsplit(fl[[first + 1]], " ")[[1]])
  if (length(describe) != 3) {  # First year; no. sites; length of site id.
    stop(paste("Three-digit descriptive information that should be on line ",
               first + 1,
               " needs to have 3 elements separated by spaces."))
  }
  # TODO: Need error check that row length = describe[2] + year.
  # TODO: Need error check that first year in body is first year in meta.

  type_key <- list("?" = "estimate",  # My own creation for estimated years to pith.
                   "." = "null_year",
                   "|" = "recorder_year",
                   "U" = "unknown_fs",
                   "u" = "unknown_fi",
                   "D" = "dormant_fs",
                   "d" = "dormant_fi",
                   "E" = "early_fs",
                   "e" = "early_fi",
                   "M" = "middle_fs",
                   "m" = "middle_fi",
                   "L" = "late_fs",
                   "l" = "late_fi",
                   "A" = "latewd_fs",
                   "a" = "latewd_fi",
                   "[" = "pith_year",
                   "]" = "bark_year",
                   "{" = "inner_year",
                   "}" = "outer_year")
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
      stop("The line before the annual FHX data should be blank.")
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
  fl_body_melt <- reshape2::melt(fl_body, id.vars = "year", value.name = "rec_type",
                       variable.name = "series", na.rm = TRUE)
  fl_body_melt <- fl_body_melt[fl_body_melt$rec_type != '.', ]
  fl_body_melt$rec_type <- vapply(fl_body_melt$rec_type, function(x) type_key[[x]], "a")
  fl_body_melt$rec_type <- factor(fl_body_melt$rec_type,
                              levels = c("null_year", "recorder_year", "unknown_fs",
                                         "unknown_fi", "dormant_fs", "dormant_fi",
                                         "early_fs", "early_fi", "middle_fs",
                                         "middle_fi", "late_fs", "late_fi",
                                         "latewd_fs", "latewd_fi", "pith_year",
                                         "bark_year", "inner_year", "outer_year",
                                         "estimate"))
  f <- fhx(year = fl_body_melt$year, series = fl_body_melt$series,
           rec_type = fl_body_melt$rec_type)
}

#' List of character strings to write to FHX file.
#'
#' @param x An fhx object.
#'
#' @return A list with four members containing vectors: "head_line", 
#'     "subhead_line", "series_heading", and "body". Each referring 
#'     to a portion of an FHX file that the strings are dumped into.
#'
#' @seealso write_fhx
list_filestrings <- function(x) {
  stopifnot(is.fhx(x))
  type_key <- list("null_year"    = ".",
                   "recorder_year"= "|",
                   "unknown_fs"   = "U",
                   "unknown_fi"   = "u",
                   "dormant_fs"   = "D",
                   "dormant_fi"   = "d",
                   "early_fs"     = "E",
                   "early_fi"     = "e",
                   "middle_fs"    = "M",
                   "middle_fi"    = "m",
                   "late_fs"      = "L",
                   "late_fi"      = "l",
                   "latewd_fs"    = "A",
                   "latewd_fi"    = "a",
                   "pith_year"    = "[",
                   "bark_year"    = "]",
                   "inner_year"   = "{",
                   "outer_year"   = "}")
  out <- x
  out$rec_type <- vapply(out$rec_type, function(x) type_key[[x]], "a")
  year_range <- seq(min(out$year), max(out$year))
  filler <- data.frame(year = year_range,
                       series = rep("hackishSolution", length(year_range)),
                       rec_type = rep(".", length(year_range)))
  out <- rbind(out, filler)
  out <- reshape2::dcast(out, year ~ series, value.var = "rec_type", fill = ".")
  out$hackishSolution <- NULL
  # Weird thing to move year to the last column of the data.frame:
  out$yr <- out$year
  out$year <- NULL
  series_names <- as.character(unique(x$series))
  no_series <- length(series_names)
  max_series_name_length <- max(sapply(series_names, nchar))
  head_line <- "FHX2 FORMAT"
  subhead_line <- paste(min(x$year), no_series, max_series_name_length)
  # Vertical series name heading.
  series_heading <- matrix(" ", nrow = max_series_name_length, ncol = no_series)
  for ( i in seq(1, no_series) ) {
    ingoing <- strsplit(series_names[i], split = "")[[1]]
    n <- length(ingoing)
    series_heading[1:n, i] <- ingoing
  }
  list('head_line' = head_line,
       'subhead_line' = subhead_line,
       'series_heading' = series_heading,
       'body' = out)
}

#' Write an fhx object to a new FHX2 file.
#'
#' @param x An fhx object.
#' @param fname Output filename.
#'
#' @examples
#' \dontrun{
#' data(lgr2)
#' write_fhx(lgr2, 'afile.fhx')
#' }
#'
#' @export
write_fhx <- function(x, fname="") {
  if ( fname == "" ) {
    stop('Please specify a character string naming a file or connection open
          for writing.')
  }
  d <- list_filestrings(x)
  fl <- file(fname, open = "wt")
  cat(paste(d[['head_line']], "\n", d[['subhead_line']], "\n", sep = ""),
      file = fl, sep = "")
  utils::write.table(d[['series_heading']], fl,
                     append = TRUE, quote = FALSE,
                     sep = "", na = "!",
                     row.names = FALSE, col.names = FALSE)
  cat("\n", file = fl, sep = "", append = TRUE)
  utils::write.table(d[['body']], fl,
                     append = TRUE, quote = FALSE,
                     sep = "", na = "!",
                     row.names = FALSE, col.names = FALSE)
  close(fl)
}
