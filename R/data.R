#' Los Griegos Peak plot2 fire-history data
#'
#' An `fhx` object with fire-history data from Los Griegos Peak, New Mexico.
#'
#' @format An fhx object with 26 series from 1366 to 2012 CE.
#'
#' @seealso [lgr2_meta] Los Griegos Peak metadata.
"lgr2"


#' Metadata for the Los Griegos Peak fire-history dataset
#'
#' @description
#' A data frame with species information for the Los Griegos Peak plot2
#' fire-history dataset ([lgr2]).
#'
#' @format A `data.frame` with 26 rows and 2 variables:
#'   * "TreeID": Name of tree series.
#'   * "SpeciesID": Abbreviated tree species
#'
#' @seealso [lgr2] Log Griegos Peak fire-history data.
"lgr2_meta"


#' Peggy Mesa fire-history data
#'
#' @description
#' An `fhx` object with fire-history data from Peggy Mesa.
#'
#' @format An `fhx` object with 41 series from 1555 to 2013 CE.
#'
#' @source Guiterman, Christopher H., Ellis Q. Margolis, and
#' Thomas W. Swetnam. 2015. "Dendroecological Methods For Reconstructing
#' High-Severity Fire In Pine-Oak Forests." Tree-Ring Research 71 (2): 67-77.
#' doi:10.3959/1536-1098-71.2.67.
#'
#' @seealso
#'   * [pgm_meta] Peggy Mesa metadata.
#'   * [pgm_pdsi] PDSI time-series for Peggy Mesa site.
"pgm"


#' Metadata for the Peggy Mesa fire-history dataset
#'
#' @description
#' A data frame with species and location information for the Peggy Mesa
#' fire-history dataset ([pgm]).
#'
#' @format A `data.frame` with 41 rows and 5 variables:
#'   * "TreeID": Name of tree series.
#'   * "SpeciesID": Abbreviated tree species.
#'   * "Latitude": latitude of tree in decimal degrees.
#'   * "Longitude": longitude of tree in decimal degrees.
#'   * "Elevation": tree elevation in meters.
#'
#' @source Guiterman, Christopher H., Ellis Q. Margolis, and
#' Thomas W. Swetnam. 2015. "Dendroecological Methods For Reconstructing
#' High-Severity Fire In Pine-Oak Forests." Tree-Ring Research 71 (2): 67-77.
#' doi:10.3959/1536-1098-71.2.67.
#'
#' @seealso
#'   * [pgm] Peggy Mesa fire-history data.
#'   * [pgm_pdsi] PDSI time-series for Peggy Mesa site.
"pgm_meta"


#' Reconstructed PDSI time series for the Peggy Mesa fire-history dataset
#'
#' @description
#' A tree-ring reconstructed Palmer Drought-Severity Index time series
#' corresponding to the Peggy Mesa fire-history dataset ([pgm]) -- specifically,
#' the Jemez Mountains area (gridpoint 133). The reconstruction is from The
#' North American Drought Atlas (Cook and Krusic 2004).
#'
#' @format A `data.frame` with 2004 rows and 1 variables. Row names give the
#' year for the reconstructed value:
#'   * "RECON": The reconstructed PDSI series.
#'
#' @source Cook, E. R., and Krusic, P. J. (2004). The North American Drought
#' Atlas. Retrieved September 13, 2017, from
#' http://iridl.ldeo.columbia.edu/SOURCES/.LDEO/.TRL/.NADA2004/.pdsi-atlas.html
#'
#' @seealso
#'   * [pgm] Peggy Mesa fire-history data.
#'   * [pgm_meta] Peggy Mesa metadata.
"pgm_pdsi"

#' Pajarito Mountain East fire-history data
#'
#' @description
#' An `fhx` object with fire-history data.
#'
#' @format An `fhx` object with 17 series from 1702 to 1993 CE.
#'
#' @source https://www1.ncdc.noaa.gov/pub/data/paleo/firehistory/firescar/northamerica/uspme001.fhx
#'
"pme"

#' Pajarito Mountain West fire-history data
#'
#' @description
#' An `fhx` object with fire-history data.
#'
#' @format An `fhx` object with 11 series from 1617 to 1993 CE.
#'
#' @source https://www1.ncdc.noaa.gov/pub/data/paleo/firehistory/firescar/northamerica/uspmw001.fhx
#'
"pmw"

#' Pajarito Mountain Ridge fire-history data
#'
#' @description
#' An `fhx` object with fire-history data.
#'
#' @format An `fhx` object with 23 series from 1626 to 1993 CE.
#'
#' @source https://www1.ncdc.noaa.gov/pub/data/paleo/firehistory/firescar/northamerica/uspmr001.fhx
#'
"pmr"
