# burnr v0.2.2

Changes in this patch:

* Added warnings and errors to catch bad input on `seas()` (#112).

* Added warning if using `series_mean_interval()` on fhx object with multiple series (#113).

* Fixed bad numbers in `print()` for SEAs objects (#111).

* Fixed typos in warning messages (#114).


# burnr v0.2.1

Changes in this patch:

* `intervals` objects' KS test for distribution test is now two sided (Issue #108).

* Printed `intervals` objects now report the Weibull median, regardless of fit chosen (Issue #109).


# burnr v0.2.0

Changes in this minor release:

* Renamed `run_sea()` to `sea()` and enhanced speed, and changed the `time_span` parameter to `event_range`, `key` argument is now `event`, and `years_before` and `years_after` are now `nbefore` and `nafter`, respectively. The function now returns a proper sea object with special `plot()` and `print()` methods. The `plot()` generic method uses the new `plot_sealags()`. You can still use the old `run_sea()`, but it is deprecated and will be removed in future releases. We're happy with the general interfaces to `sea()` but its internal structure may change over the next few minor releases, so beware.

* Added `intervals()` to handle interval analysis. These are a whole new object. `mean_interval()` has been renamed `series_mean_interval()` to avoid confusion. We also have a generic `plot()` function that uses `plot_intervals_dist()`. It plots a little histogram with a rug along the x-axis.

* Added `pgm_pdsi` data to run with sea. Load it with `data("pgm_pdsi")`.

* Added `make_rec_type()`. I hope this will make it easier to create `rec_type` factors used by `fhx()`.

* Added `event_count_position()`, which gives the number of events found in different positions or seasons.

* Added `year_range()` to give (min, max) years for an `fhx` object.

* Updated `composite()` to include a filter for number of events. This necesitates fixes in other functions (e.g. `plot_demograph()`).

* Added `summary()` function for fhx and intervals objects.

* `count_fire()` renamed `count_scar()`

* Added `quantile()` function for intervals.


# burnr v0.1.2

Changes in this patch:

* Fixed additional bug in `read_fhx()` caused by "FHX2 format" in FHX file header.

* Fixed bug #71, causing `read_fhx()` to fail when FHX has empty lastline.

* Added `text` option to `read_fhx()`.

* Corrected error message for `series` arg in `fhx()`. Fixes #73.

* Fixed write_fhx() reversing order of series names in output file.

# burnr v0.1.1

Changes in this patch:

* Added option to sort series by their last (or most recent) year with `sort()`.

* Updated `run_sea()` to better replicate EVENT Fortran program. Addresses #58. Added detailed description and an example page with graphics. Also fixed issue with percentage CIs in the departure table. Mind you, `run_sea()` is still super experimental.

* Several minor changes to documentation and meta data.

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
