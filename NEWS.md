# snapprep 0.2.0 (Release date: 2017-10-06)

* Rearranged arguments in `clim_dist_seasonal`.
* Added `region_group` argument to `clim_stats_ar5` for file filtering to allow smaller batch processing jobs.

# snapprep 0.1.9 (Release date: 2017-10-05)

* Fixed issue with output directory paths for seasonal climate distributions.
* Updated example code.
* Removed unneeded, unused function argument from `clim_dist_seasonal`. The function only takes `files` for input. Use absolute paths.
* Added `vairable` and `rcp` arguments to `clim_dist_seasonal` for splitting processing into smaller file batches.
* Refactored `clim_stats_ar5`. This function uses monthly or seasonal outputs from `clim_dist_monthly` and `clim_dist_seasonal` to compute statistics.

# snapprep 0.1.0

* Added initial climate variable distribution estimation content to vignette.
* Updated functions and documentation for CMIP5 data extraction and density estimation functions.
* Updated defaults in `snapdef`.
* Add defaults unit tests.
* Added better handling of near-zero precipitation.

# snapprep 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
