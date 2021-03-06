% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sea.R
\name{plot.sea}
\alias{plot.sea}
\title{Plot a \code{sea} object}
\usage{
\method{plot}{sea}(...)
}
\arguments{
\item{...}{Arguments passed on to \code{\link[=plot_sealags]{plot_sealags()}}.}
}
\description{
Plot a \code{sea} object
}
\examples{
\dontrun{
# Read in the Cook and Krusic (2004; The North American Drought Atlas)
# reconstruction of Palmer Drought Severity Index (PDSI) for the Jemez
# Mountains area (gridpoint 133).
data(pgm_pdsi)

# Run SEA on Peggy Mesa (pgm) data
data(pgm)
pgm_comp <- composite(pgm)

pgm_sea <- sea(pgm_pdsi, pgm_comp)

plot(pgm_sea)
}
}
\seealso{
\code{\link[=plot_sealags]{plot_sealags()}} handles the plotting for this function.
}
