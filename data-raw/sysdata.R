library(usethis)
library(burnr)


type_key <- list("?" = "estimate",  # My own creation for estimated years to pith.
                "." = "null_year",
                "|" = "recorder_year",
                "U" = "unknown_fs",
                "u" = "unknown_fi",
                "D" = "dormant_fs",
                "d" = "dormant_fi",
                "E" = "early_fs",
                "e" = "early_fi",
                "M" = "middle_fs",
                "m" = "middle_fi",
                "L" = "late_fs",
                "l" = "late_fi",
                "A" = "latewd_fs",
                "a" = "latewd_fi",
                "[" = "pith_year",
                "]" = "bark_year",
                "{" = "inner_year",
                "}" = "outer_year")

rec_type_all <- unlist(type_key)
# Has names() of the original, abbreviated FHX file event codes.

rec_type_abrv <- names(rec_type_all)
# Has names() of the rec_type levels we use for this package.
names(rec_type_abrv) <- rec_type_all
usethis::use_data(rec_type_all, rec_type_abrv, internal = TRUE, overwrite = TRUE)
