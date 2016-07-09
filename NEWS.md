# burnr v0.1.0

Changes in this minor release:

* Added a vignette!

* `get_ggplot()` is being depreciated. Please use `plot_demograph()`.

* Changed design of fhx objects. They are now data.frame and not lists (nobody used the 'meta', list member anyways). This means that if x is your fhx object and you previously used x$rings, you can now just use x.

* Added function `is.fhx()`.

* Updated `pgm` example data.

* composite() now returns an fhx object.

* Added function get_event_years() to find fire and fire injury years in fhx objects. This is especially helpful for composites.

* `concatenate()` and `combine()` were reverted back to '+' at the request of beloved users. This means code like `lgr2 + pgm` will combine these objects together into a single fhx object!

* Added examples to all function documentation.

* `resolve_duplicates()` is now hidden (i.e. `burnr:::check_duplicates()`).

* Added function sample_depth() to get sample depth.

* Added run_sea function to begin to address issue #58. Has pretty table output. This is still very experimental.

* Bug fix in series_stats to maintain series IDs for those with no features

* Changed composite() behavior. Now more similar to FHAES. Can choose to count injuries as "fire events" with the injury_event argument.

* Added count_recording() function.

* Fixed minor bugs in series_stats.

* Removed sorting by default on read_fhx() and '+.fhx'

* Fixed sort()s non-responsive decreasing argument.

* Fixed poor sorting function for FHX objects.

* Added several small utility functions: `get_year()`, `get_series()`, `series_names()`, `delete()`.

* Improved documentation!

* Adding series_stats function, closes issue #44.

# burnr v0.0.2

Changes in this patch:

* `plot()` and `get_ggplot()` should no longer allow the user to plot facets and composite rugs in the same plot. This closes bug #25.

* Ring types (the `rec_type` column in `x$rings`) has "." in all levels replaced with a "_". For example, 'dormant.fs' is now 'dormant_fs.' Closes #45.

* In `get_ggplot()` the `legend` argument is now `plot_legend` to avoid clash with common functions.

* In `fhx$rings` and all functions, `type` arugment is now `rec_type` to avoid clash with `type` function. `rec_type` is short for "record_type".

* Can now read FHX files with a single series (closes bug #43).

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
