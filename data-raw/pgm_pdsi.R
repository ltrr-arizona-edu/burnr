library(usethis)

# TXT file downloaded from http://iridl.ldeo.columbia.edu/SOURCES/.LDEO/.TRL/.NADA2004/pdsiatlashtml/pdsiwebdata/1050w_350n_133.txt
pdsi_raw <- read.table('data-raw/pgm_pdsi.txt', header = TRUE, row.names = 1)
pgm_pdsi <- subset(pdsi_raw, select = "RECON")

usethis::use_data(pgm_pdsi, overwrite = TRUE)