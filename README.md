# burnr

[![Travis-CI Build Status](https://travis-ci.org/ltrr-arizona-edu/burnr.svg?branch=master)](https://travis-ci.org/ltrr-arizona-edu/burnr)
[![Coverage Status](https://coveralls.io/repos/ltrr-arizona-edu/burnr/badge.svg)](https://coveralls.io/r/ltrr-arizona-edu/burnr)

A tool set to analyze forest fire history data (e.g. FHX2) in R. This is designed for power users and projects with special needs.

The project is under heavy development. Hic sunt dracones.

## A quick example

```R
library(burnr)

data(lgr2)

plot(lgr2)
```

This gives you a basic plot. There are more advanced options. For example, we can color our plot by sample species.

```R
data(lgr2_meta)

plot(lgr2, color_group = lgr2_meta$SpeciesID, color_id = lgr2_meta$TreeID,
     plot_legend = TRUE)
```

Cool, eh?

See `help(get_ggplot2)` for more plot options. You can read and write your own FHX files with `read_fhx()` and `write_fhx()`.

## Installation

This package is not yet in CRAN. If you'd like to install it, first be sure you have the `devtools` package installed in R. Install `burnr` with:

```R
devtools::install_github("ltrr-arizona-edu/burnr")
```

## Support

Documentation is included in the code. More information, including a cookbook, can be found on the [project's wiki](https://github.com/ltrr-arizona-edu/burnr/wiki). Note, this is still under construction.

## Development

Want to contribute? Great! We're following [Hadley's packaging workflow](http://r-pkgs.had.co.nz/) and [code style](http://adv-r.had.co.nz/Style.html). Fork away.

Please file bugs in the [bug tracker](https://github.com/ltrr-arizona-edu/burnr/issues).
