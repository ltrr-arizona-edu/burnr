# Encode Pajarito mountains data

library(burnr)
library(usethis)

# Data obtained from NOAA IMPD
## https://www1.ncdc.noaa.gov/pub/data/paleo/firehistory/firescar/northamerica/

pmr <- read_fhx('data-raw/uspmr001.fhx')
pme <- read_fhx('data-raw/uspme001.fhx')
pmw <- read_fhx('data-raw/uspmw001.fhx')

use_data(pmr, overwrite = TRUE)
use_data(pme, overwrite = TRUE)
use_data(pmw, overwrite = TRUE)
