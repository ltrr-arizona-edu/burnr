library(usethis)
library(burnr)


type_key <- list(
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
  "}" = "outer_year",
  "?" = "estimate",  # non-canon for estimated years to pith.
  "T" = "transition_fs",  # noncanon
  "t" = "transition_fi",  # noncanon
  "F" = "falldormant_fs",  # noncanon
  "f" = "falldormant_fi",  # noncanon
  "S" = "springdormant_fs", # noncanon
  "s" = "springdormant_fi", # noncanon
  "B" = "earlylw_fs",  # noncanon
  "b" = "earlylw_fi",  # noncanon
  "C" = "latelw_fs",  # noncanon
  "c" = "latelw_fi"  # noncanon
)

rec_type_all <- unlist(type_key)
# Has names() of the original, abbreviated FHX file event codes.

rec_type_abrv <- names(rec_type_all)
# Has names() of the rec_type levels we use for this package.
names(rec_type_abrv) <- rec_type_all

rec_type_recorder <- c(
  "recorder_year",
  "unknown_fs",
  "dormant_fs",
  "early_fs",
  "middle_fs",
  "late_fs",
  "latewd_fs",
  "transition_fs",  # noncanon
  "falldormant_fs",  # noncanon
  "springdormant_fs", #noncanon
  "earlylw_fs",  # noncanon
  "latelw_fs"  # noncanon
)
rec_type_injury <- c(
  "unknown_fi",
  "dormant_fi",
  "early_fi",
  "middle_fi",
  "late_fi",
  "latewd_fi",
  "transition_fi",  # noncanon
  "falldormant_fi",  # noncanon
  "springdormant_fi", # noncanon
  "earlylw_fi",  # noncanon
  "latelw_fi"  # noncanon
)
rec_type_scar <- c(
  "unknown_fs",
  "dormant_fs",
  "early_fs",
  "middle_fs",
  "late_fs",
  "latewd_fs",
  "transition_fs",  # noncanon
  "falldormant_fs",  # noncanon
  "springdormant_fs", # noncanon
  "earlylw_fs",  # noncanon
  "latelw_fs"  # noncanon
)
rec_type_ends <- c(
  "pith_year",
  "bark_year",
  "inner_year",
  "outer_year"
)

# Only "official" canon rec_types go here:
rec_type_canon <- c(
  "null_year",
  "recorder_year",
  "unknown_fs",
  "unknown_fi",
  "dormant_fs",
  "dormant_fi",
  "early_fs",
  "early_fi",
  "middle_fs",
  "middle_fi",
  "late_fs",
  "late_fi",
  "latewd_fs",
  "latewd_fi",
  "pith_year",
  "bark_year",
  "inner_year",
  "outer_year"
)

usethis::use_data(rec_type_all, rec_type_abrv,
  rec_type_recorder, rec_type_injury, rec_type_scar, rec_type_ends,
  rec_type_canon,
  internal = TRUE, overwrite = TRUE
)
