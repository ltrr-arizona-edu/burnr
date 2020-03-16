# burnr

[![Travis-CI Build Status](https://travis-ci.org/ltrr-arizona-edu/burnr.svg?branch=master)](https://travis-ci.org/ltrr-arizona-edu/burnr)
[![Coverage Status](https://coveralls.io/repos/github/ltrr-arizona-edu/burnr/badge.svg?branch=master)](https://coveralls.io/github/ltrr-arizona-edu/burnr?branch=master)


Basic tools to analyze forest fire history data (e.g. FHX) in R. This is designed for power users and projects with special needs.


## A quick example

```R
library(burnr)

data(lgr2)

plot(lgr2)
```

This gives you a basic plot. There are more advanced options. For example, we can color our plot by sample species.

```R
data(lgr2_meta)

plot(lgr2,
  color_group = lgr2_meta$SpeciesID,
  color_id = lgr2_meta$TreeID,
  plot_legend = TRUE
)
```

Cool, eh?

See `help(plot_demograph)` for more plot options. You can read and write your own FHX files with `read_fhx()` and `write_fhx()`.


## Installation

You can install a relatively stable version of this package from CRAN by opening an R session and running:

```R
install.packages('burnr')
```

You can also install the development version of the package from Github. First, be sure you have the `devtools` package installed in R. Install `burnr` with:

```R
devtools::install_github("ltrr-arizona-edu/burnr")
```

## Support

Documentation is included in the code. If you're new to `burnr`, our [2018 paper in Dendrochronologia](https://doi.org/10.1016/j.dendro.2018.02.005) is a nice survey of the package with many examples. We have also an [introduction](https://cran.r-project.org/package=burnr/vignettes/introduction.html). More information can be found on the [project's wiki](https://github.com/ltrr-arizona-edu/burnr/wiki). Note, this is still under construction.


## Citation

Please cite the original `burnr` paper if you use it in your research: 

    Malevich, Steven B., Christopher H. Guiterman, and Ellis Q. Margolis. 2018. 
    “Burnr: Fire History Analysis and Graphics in R.” Dendrochronologia 49 
    (June): 9–15. https://doi.org/10.1016/j.dendro.2018.02.005.


Citations help us to identify user needs and justify additional time developing and maintaining `burnr`.


## Development

Please file bugs in the [bug tracker](https://github.com/ltrr-arizona-edu/burnr/issues).

Want to contribute? Great! We're following [Hadley's packaging workflow](http://r-pkgs.had.co.nz/) and [style guide](http://style.tidyverse.org). Fork away.

If you're not a developer, don't worry! We also welcome help with documentation and tutorials.
