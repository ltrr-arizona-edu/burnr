library(burnr)

lgr2 <- read_fhx('data-raw/lgr2.fhx')

devtools::use_data(lgr2, overwrite = TRUE)
