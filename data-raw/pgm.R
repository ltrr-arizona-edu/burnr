library(burnr)
library(usethis)

pgm <- read_fhx('data-raw/pgm.fhx')

usethis::use_data(pgm, overwrite = TRUE)
