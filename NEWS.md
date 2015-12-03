# burnr v0.1.0.9999

Changes in this minor release:

* Added several small utility functions: `get_year()`, `get_series()`, `series_names()`, `delete()`.

* `subset()` for `fhx` objects.

* Pretty printing for `fhx` objects.

* Improved documentation!

# burnr v0.0.2

Changes in this patch:

* `plot()` and `get_ggplot()` should no longer allow the user to plot facets and composite rugs in the same plot. This closes bug #25.

* Ring types (the `rec_type` column in `x$rings`) has "." in all levels replaced with a "_". For example, 'dormant.fs' is now 'dormant_fs.' Closes #45.

* In `get_ggplot()` the `legend` argument is now `plot_legend` to avoid clash with common functions.

* In `fhx$rings` and all functions, `type` arugment is now `rec_type` to avoid clash with `type` function. `rec_type` is short for "record_type".

* Can now read FHX files with a single series (closes bug #43).

* `concatenate()` has been renamed `combine()` at the request of beloved users.

* `lgr2` and `pgm` are available fire-history datasets when you load this package. The metadata for each of these sets is `lgr2_meta` and `pgm_meta`. Load the data with `data()`.


# burnr v0.0.1

* First github release

* Added `NEWS.md` file. Existing users should check back here for changes.

* `rug.filter` function is renamed `composite`.

* `ggplot.fhx` is now `get_ggplot`. This is more descriptive. The following arguments for this function have been changed as a part of this fix: `plot.rug` is now `composite_rug`, `filter.min` is now `filter_min`, `filter.prop` is now `filter_prop`, `event.size` is now `event_size`, `rugbuffer.size` is now `rugbuffer_size`, `rugdivide.pos` is now `rugdivide_pos`. `spp` argument is renamed to `color_group`. `sppid` is now `color_id`. `cluster` is now `facet_group`. `clusterid` is now `cluster_id`. 

* `read.fhx` function is renamed `read_fhx`.

* `write.fhx` function is renamed `write_fhx`.

* `order.fhx` is now `sort` (see method `sort.fhx`). Also, added a boolean argument `decreasing`, which defaults to FALSE. Previously this has been TRUE..

* `+` with `fhx` objects, (`+.fhx`) is now `concatenate`. So, `a + b` should now be `concatenate(a, b)`.

* Fixed bug that disappeared a series which started or ended with fire scars.
