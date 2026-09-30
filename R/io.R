#' Read FHX2 file and return an `fhx`` object
#'
#' @param fname Name of target FHX file. Needs to be in format version 2.
#' @param encoding Encoding to use when reading the FHX file. The default is to
#'   use the system default in R.
#' @param text Character string. If `fname` is not provided and text is, then
#'   data is read from text using a text connection.
#' @param verbose When TRUE (default) provides feedback regarding the file
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
read_fhx <- function(fname, encoding, text, verbose = TRUE) {
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
  if (verbose) {
    check_series(f)
  }

  f
}

#' Diagnostic checks of common data issues in FHX objects
#'
#' @param x An `fhx` object.
#' @param keep_checks Boolean indicating whether the check data should be
#'   provided for further diagnostics. Defaults to FALSE, and only warnings or
#'   information are provided.
#'
#' @return If necessary, a set of warnings and informational text to indicate
#'   where and when issues are found. If `keep_checks = TRUE`, a list object
#'   with the set of check data.
#'
#' @importFrom dplyr  %>% mutate filter group_by reframe n case_when
#' @importFrom rlang .data
#' @importFrom forcats fct_collapse fct_count
#' @importFrom cli cli_alert_info cli_alert_danger cli_alert_success style_bold
#' @importFrom stringr str_c
#'
#' @examples
#' # Nothing is returned unless an issue is found
#' check_series(lgr2)
#'
#' # Informative messages flag potential issues
#' check_series(pgm)
#'
#' # Danger flags warn of data quality and usability issues
#' \dontrun{
#' d <- read_fhx(paste0("https://github.com/ltrr-arizona-edu/burnr/",
#'                       "files/818119/Dumb_broke_fhx.txt"))
#' check_series(d)
#' }
#'
#' @export

check_series <- function(x, keep_checks = FALSE) {
  stopifnot(is_fhx(x))

  ## Read in file and collapse rec_types
  check_file <- x %>%
    mutate(gen_type = suppressWarnings(
      fct_collapse(.data$rec_type,
                   recorder = c(rec_type_recorder,
                                rec_type_injury),
                   inner = c("pith_year", "inner_year"),
                   outer = c("bark_year", "outer_year"))
    )
    )

  ## Count end types
  end_code_counts <- check_file %>%
    group_by(.data$series) %>%
    reframe(fct_count(factor(.data$gen_type))) %>%
    tidyr::complete(.data$series, .data$f, fill = list(n = 0)) %>%
    filter(.data$f %in% c("inner", "outer"))


  ## Excluded start/end indicators
  no_ends <- end_code_counts %>%
    reframe(n = n(), .by = .data$series) %>%
    filter(.data$n < 2)

  if (nrow(no_ends) > 0) {
    for (i in 1:nrow(no_ends)) {
      bad_series <- no_ends$series[i] %>% as.character()
      bad_type <- ifelse(filter(end_code_counts,
                                .data$series == bad_series)$f %>%
                           as.character() == "inner", "outer", "inner")
      cli_alert_info("Series {style_bold({bad_series})} is missing a specific {bad_type} year code.")
    }
  }

  ## Duplicate start/end indicators
  bad_ends <- end_code_counts %>%
    filter(.data$n > 1)

  if (nrow(bad_ends) > 0){
    for (i in 1:nrow(bad_ends)) {
      bad_series <- bad_ends$series[i] %>% as.character()
      bad_type <- bad_ends$f[i] %>% as.character()
      years <- str_c(filter(check_file,
                            .data$series == bad_series,
                            .data$gen_type == bad_type)$year,
                     collapse = " and ")
      cli_alert_danger("Series {style_bold({bad_series})} includes duplicate {bad_type} year codes in {years}.")
    }
  }

  ## ID series lacking scar/injury codes
  empty_series <- check_file %>%
    group_by(.data$series) %>%
    reframe(n_rec = sum(.data$gen_type == "recorder")) %>%
    filter(.data$n_rec == 0)

  if (nrow(empty_series) > 0) {
    for (i in 1:nrow(empty_series)) {
      cli_alert_info("Series {style_bold({empty_series$series[i]})} does not include any scar or injury features.")
    }
  }

  ## If all of the series are caught with no end terms or no scars, the next set
  ## of tests are meaningless, so this if() statement bypasses them
  if (length(series_names(x)) != sum(nrow(no_ends), nrow(empty_series))) {

  ## Check whether scars/injuries exist beyond start/end years
  rec_diffs <- check_file %>%
    filter(! .data$series %in% no_ends$series,
           ! .data$series %in% empty_series$series,
           ! .data$series %in% bad_ends$series) %>%
    group_by(.data$series) %>%
    reframe(inner_diff = .data$year[.data$gen_type == "inner"] -
                min(.data$year[.data$gen_type == "recorder"]),
              outer_diff = max(.data$year[.data$gen_type == "recorder"]) -
                                 .data$year[.data$gen_type == "outer"])

  ## Recorder years the same as inner or outer year
  dup_years <- rec_diffs %>%
    filter(.data$inner_diff == 0 | .data$outer_diff == 0)

  if (nrow(dup_years) > 0) {
    for (i in 1:nrow(dup_years)) {
      bad_series <- dup_years$series[i] %>% as.character()
      position <- dplyr::case_when(
        dup_years$inner_diff[i] == 0 & dup_years$outer_diff[i] == 0
        ~ "both inner and outer year codes",
        dup_years$inner_diff[i] == 0 ~ "the inner-year code",
        dup_years$outer_diff[i] == 0 ~ "the outer-year code"
      )
      cli_alert_danger(
        c("Series {style_bold({bad_series})} includes a scar or injury code",
          " in the same year as {position}."),
        wrap = TRUE
      )
    }
  }

  ## Recorder years before inner year
  inner_diffs <- rec_diffs %>%
    filter(.data$inner_diff > 0)

  if (nrow(inner_diffs) > 0) {
    for (i in 1:nrow(inner_diffs)) {
      bad_series <- inner_diffs$series[i] %>% as.character()
      cli_alert_danger(
        c("Series {style_bold({bad_series})} includes a scar or injury code",
        " {inner_diffs$inner_diff[i]} years before the inner-year code."),
        wrap = TRUE
      )
    }
  }

  ## Recorder years after outer year
  outer_diffs <- rec_diffs %>%
    filter(.data$outer_diff  > 0)

  if (nrow(outer_diffs) > 0){
    for (i in 1:nrow(outer_diffs)) {
      bad_series <- outer_diffs$series[i] %>% as.character()
      cli_alert_danger(
        c("Series {style_bold({bad_series})} includes a scar or injury code",
        " {outer_diffs$outer_diff[i]} years after the outer-year code."),
        wrap = TRUE
      )
    }
  }
  } else {
    bad_series <-  data.frame()
    outer_diffs <- data.frame()
    inner_diffs <- data.frame()
    dup_years <- data.frame()
  }

  ## Duplicate event years
  dup_events <- check_file %>%
    filter(.data$gen_type == "recorder") %>%
    group_by(.data$series, .data$year) %>%
    reframe(n = n()) %>%
    filter(n > 1)

  if (nrow(dup_events) > 0){
    for (i in 1:nrow(dup_events)) {
      bad_series <- dup_events$series[i] %>% as.character()
      cli_alert_danger(
        c("Series {style_bold({bad_series})} includes multiple scar or injury",
          " codes in {dup_events$year[i]}."),
        wrap = TRUE
      )
    }
  }

  ## Successful checks
  if (! any(
    c(
      nrow(bad_ends) > 0,
      nrow(dup_years) > 0,
      nrow(inner_diffs) > 0,
      nrow(outer_diffs) > 0,
      nrow(dup_events) > 0
    )
  )
  ) {
    cli_alert_success(
      c("Key checks successful for all {length(series_names(x))} series")
    )
  }

  if (keep_checks) {
    return(list(
      "no_ends" = no_ends,
      "bad_ends" = bad_ends,
      "dup_ends" = dup_years,
      "emtpy_series" = empty_series,
      "inner_diffs" = inner_diffs,
      "outer_diffs" = outer_diffs,
      "dup_events" = dup_events)
    )
  }
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
#' @importFrom rlang abort
#' @importFrom utils write.table
#' @importFrom cli cli_alert_info
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

  chk_list <- check_series(x, keep_checks = TRUE)
  if (any(
    c(nrow(chk_list$dup_ends) > 0,
      nrow(chk_list$inner_diffs) > 0,
      nrow(chk_list$outer_diffs) > 0,
      nrow(chk_list$dup_events) > 0
      )
    )
  ) abort("Data errors listed above prevent the creation of an FHX file")

  if (violates_canon(x)) {
    cli_alert_info(
      c("The output file includes codes that violate FHX2",
        " canon. Other software may not be able to read the file")
    )
  }
  d <- list_filestrings(x)
  fl <- file(fname, open = "wt")
  cat(paste(d[["head_line"]], "\n", d[["subhead_line"]], "\n", sep = ""),
      file = fl, sep = ""
  )
  write.table(d[["series_heading"]], fl,
              append = TRUE, quote = FALSE,
              sep = "", na = "!",
              row.names = FALSE, col.names = FALSE
  )
  cat("\n", file = fl, sep = "", append = TRUE)
  write.table(d[["body"]], fl,
              append = TRUE, quote = FALSE,
              sep = "", na = "!",
              row.names = FALSE, col.names = FALSE
  )

  close(fl)
}


#' Convert abbreviated `fhx` file event char to rec_type char
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
