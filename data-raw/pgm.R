library(burnr)

pgm <- read_fhx('data-raw/pgm.fhx')

devtools::use_data(pgm, overwrite = TRUE)
