#' Read FHX2 file and return an `fhx`` object
#'
#' @param fname Name of target FHX file. Needs to be in format version 2.
#' @param encoding Encoding to use when reading the FHX file. The default is to
#'   use the system default in R.
#' @param text Character string. If `fname` is not provided and text is, then
#'   data is read from text using a text connection.
#'
#' @return An `fhx` object, as returned by [fhx()].
#'
#' @seealso
#'   * [write_fhx()] write an `fhx` object to a file.
#'   * [fhx()] create an `fhx` object.
#'   * [as_fhx()] cast data frame or similar object to an `fhx` object.
#'
#' @examples
#' \dontrun{
#' d <- read_fhx("afile.fhx")
#' }
#'
#' @export
read_fhx <- function(fname, encoding, text) {
  if (missing(encoding)) {
    encoding <- getOption("encoding")
  }
  if (missing(fname) && !missing(text)) {
    con <- textConnection(text)
    on.exit(close(con))
  }
  if (!missing(fname)) {
    con <- file(fname, encoding = encoding)
    on.exit(close(con))
  }
  # Error checking and basic variables.
  if (length(readLines(con, n = 1)) == 0) {
    stop("file appears to be empty")
  }
  fl <- readLines(con, warn = FALSE)

  no_head_line <- !any(suppressWarnings(grepl(
    "^FHX2 FORMAT|^FIRE2 FORMAT", fl,
    ignore.case = TRUE
  )))

  if (no_head_line) {
    stop("Cannot find line 'FHX2 FORMAT' or 'FIRE2 FORMAT'.")
  }

  first <- suppressWarnings(grep(
    "^FHX2 FORMAT|^FIRE2 FORMAT", fl,
    ignore.case = TRUE
  ))

  describe <- as.numeric(strsplit(fl[[first + 1]], "[ ]+")[[1]])
  # First year; no. sites; length of site id.
  if (length(describe) != 3) {
    stop(paste(
      "Three-digit descriptive information that should be on line ",
      first + 1,
      " needs to have 3 elements separated by spaces."
    ))
  }
  # TODO: Need error check that row length = describe[2] + year.
  # TODO: Need error check that first year in body is first year in meta.
  # Parse series names.
  ## use lapply function to eliminate extra speces at end of id blocks
  id_block <- fl[(first + 2):(first + 1 + describe[3])]
  uncleaned <- as.matrix(unlist(lapply(seq_along(id_block), function(x) {
    splt <- unlist(strsplit(id_block[x], ""))
    splt <- splt[1:describe[2]]
    return(splt)
  })))

  if ( (describe[2] * describe[3]) != dim(uncleaned)[1] ) {
    stop(
      "The file's three-digit descriptive information on line ", first + 1,
      " does not match the series titles in the file. Please correct this ",
      "discrepancy."
    )
  }
  dim(uncleaned) <- c(describe[2], describe[3])
  series_names <- apply(
    uncleaned, 1,
    function(x) gsub("^\\s+|\\s+$", "", paste(x, collapse = ""))
  )

  databuff <- 2
  while (TRUE) {
    if (gsub("^\\s+|\\s+$", "", fl[first + databuff + describe[3]]) == "") {
      databuff <- databuff + 1
    } else {
      break
    }
  }

  no_blank_line <- !any(
    fl[first + databuff - 1 + describe[3]] ==
      c("", " ", strrep(" ", describe[2]))
  )
  if (no_blank_line) {
    stop("The line before the annual FHX data should be blank.")
  }

  # Filling with info from the fhx file body.
  fl_body <- strsplit(
    fl[(first + databuff + describe[3]):length(fl)],
    split = ""
  )
  first_year <- describe[1]
  if (length(series_names) == 1) {
    # For whatever reason R wants to flip our dims when we have a single series.
    fl_body <- as.data.frame(sapply(fl_body, function(x) x[1:describe[2]]),
                             stringsAsFactors = FALSE
    )
  } else {
    fl_body <- as.data.frame(t(sapply(fl_body, function(x) x[1:describe[2]])),
                             stringsAsFactors = FALSE
    )
  }
  names(fl_body) <- series_names
  fl_body$year <- seq(first_year, first_year + dim(fl_body)[1] - 1)
  fl_body_melt <- reshape2::melt(fl_body,
                                 id.vars = "year", value.name = "rec_type",
                                 variable.name = "series", na.rm = TRUE
  )
  fl_body_melt <- fl_body_melt[! fl_body_melt$rec_type %in% c(".", "\032"), ]
  fl_body_melt$rec_type <- vapply(fl_body_melt$rec_type, abrv2rec_type, "") # nolint
  fl_body_melt$rec_type <- make_rec_type(fl_body_melt$rec_type)
  f <- fhx(
    year = fl_body_melt$year, series = fl_body_melt$series,
    rec_type = fl_body_melt$rec_type
  )
  f
}


#' List of character strings to write to FHX file
#'
#' @param x An `fhx` object.
#'
#' @return A list with four members containing vectors:
#'   * "head_line"
#'   * "subhead_line"
#'   * "series_heading"
#'   * "body".
#' Each referring to a portion of an FHX file that the strings are dumped into.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
#' @noRd
list_filestrings <- function(x) {
  stopifnot(is_fhx(x))
  out <- x
  out$rec_type <- vapply(out$rec_type, rec_type2abrv, "") # nolint
  year_range <- seq(min(out$year), max(out$year))
  filler <- data.frame(
    year = year_range,
    series = rep("hackishsolution", length(year_range)),
    rec_type = rep(".", length(year_range))
  )
  out <- rbind(out, filler)
  out <- pivot_wider(out,
                     names_from = .data$series,
                     values_from = .data$rec_type,
                     values_fill = list(rec_type = "."))
  out <- out[order(out$year), ]
  out$hackishsolution <- NULL
  # Weird thing to move year to the last column of the data.frame:
  out$yr <- paste0(" ", out$year)
  out$year <- NULL
  series_names <- as.character(unique(x$series))
  no_series <- length(series_names)
  max_series_name_length <- max(sapply(series_names, nchar))
  head_line <- "FHX2 FORMAT"
  subhead_line <- paste(min(x$year), no_series, max_series_name_length)
  # Vertical series name heading.
  series_heading <- matrix(" ", nrow = max_series_name_length, ncol = no_series)
  for (i in seq(1, no_series)) {
    ingoing <- strsplit(series_names[i], split = "")[[1]]
    n <- length(ingoing)
    series_heading[1:n, i] <- ingoing
  }
  list(
    "head_line" = head_line,
    "subhead_line" = subhead_line,
    "series_heading" = series_heading,
    "body" = out
  )
}


#' Write an `fhx` object to a new FHX2 file
#'
#' @param x An `fhx` object.
#' @param fname Output filename.
#'
#' @seealso
#'   * [write.csv()] to write a CSV file. Also works on `fhx` objects.
#'   * [read_fhx()] to read an FHX2 file.
#'
#' @examples
#' \dontrun{
#' data(lgr2)
#' write_fhx(lgr2, "afile.fhx")
#' }
#'
#' @export
write_fhx <- function(x, fname = "") {
  if (fname == "") {
    stop("Please specify a character string naming a file or connection open
          for writing.")
  }
  if (violates_canon(x)) {
    warning(
      "`write_fhx()` run on `fhx` object with rec_types that violate FHX2",
      " canon - other software may not be able to read the output FHX file"
    )
  }
  d <- list_filestrings(x)
  fl <- file(fname, open = "wt")
  cat(paste(d[["head_line"]], "\n", d[["subhead_line"]], "\n", sep = ""),
      file = fl, sep = ""
  )
  utils::write.table(d[["series_heading"]], fl,
                     append = TRUE, quote = FALSE,
                     sep = "", na = "!",
                     row.names = FALSE, col.names = FALSE
  )
  cat("\n", file = fl, sep = "", append = TRUE)
  utils::write.table(d[["body"]], fl,
                     append = TRUE, quote = FALSE,
                     sep = "", na = "!",
                     row.names = FALSE, col.names = FALSE
  )
  close(fl)
}


#' Convert abreviated `fhx` file event char to rec_type char
#'
#' @param x A character string.
#'
#' @return A character string.
#'
#' @seealso [rec_type2abrc] inverse function, converting an `fhx` rec_type
#'   string to abbreviated character used in FHX files.
#'
#' @noRd
abrv2rec_type <- function(x) {
  rec_type_all[[as.character(x)]] # nolint
}


#' Convert rec_type char to abreviated FHX file event char
#'
#' @param x A character string.
#'
#' @return A character string.
#'
#' @seealso [abrv2rec_type] inverse function, convert abbreviated character used
#'   in FHX files to an `fhx` rec_type string.
#'
#' @noRd
rec_type2abrv <- function(x) {
  rec_type_abrv[[as.character(x)]] # nolint
}
