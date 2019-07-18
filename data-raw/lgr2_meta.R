library(usethis)

lgr2_meta <- read.csv('data-raw/lgr2_meta.csv')

usethis::use_data(lgr2_meta, overwrite = TRUE)
