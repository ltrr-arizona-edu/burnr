# burnr v0.0.1.9000

* Added `NEWS.md` file. Existing users should check back here for changes.

* `rug.filter` function is renamed `composite`.

* `ggplot.fhx` is now `get_ggplot`. This is more descriptive. The following arguments for this function have been changed as a part of this fix: `plot.rug` is now `composite_rug`, `filter.min` is now `filter_min`, `filter.prop` is now `filter_prop`, `event.size` is now `event_size`, `rugbuffer.size` is now `rugbuffer_size`, `rugdivide.pos` is now `rugdivide_pos`. `spp` argument is renamed to `color_group`. `sppid` is now `color_id`. `cluster` is now `facet_group`. `clusterid` is now `cluster_id`. 

* `read.fhx` function is renamed `read_fhx`.

* `write.fhx` function is renamed `write_fhx`.

* `order.fhx` is now `sort` (see method `sort.fhx`). Also, added a boolean argument `decreasing`, which defaults to FALSE. Previously this has been TRUE..

* `+` with `fhx` objects, (`+.fhx`) is now `concatenate`. So, `a + b` should now be `concatenate(a, b)`.

* Fixed bug that disappeared a series which started or ended with fire scars.
