library(burnr)
library(usethis)

lgr2 <- read_fhx('data-raw/lgr2.fhx')

usethis::use_data(lgr2, overwrite = TRUE)
