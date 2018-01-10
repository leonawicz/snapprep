
<!-- README.md is generated from README.Rmd. Please edit that file -->
snapprep
========

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/snapprep.svg?branch=master)](https://travis-ci.org/leonawicz/snapprep) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/snapprep?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/snapprep) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/snapprep/master.svg)](https://codecov.io/github/leonawicz/snapprep?branch=master)

The `snapprep` package contains R functions used to support a wide range of SNAP projects by preparing and curating useful data sets from upstream raw SNAP data. The data sets compiled with the aid of `snapprep` are then made avaible to other projects. This includes compiling data sets that are contained in [SNAPverse](https://leonawicz.github.io/snapverse/) data packages.

`snapprep` is a developer package used by the SNAPverse author and maintainer. For user packages catering to analysis and graphing of the curated data sets available in SNAPverse data packages, see the `snapstat` package instead.

Use case examples
-----------------

Examples of some of the processing chains that `snapprep` is involved in include:

-   Preparing climate distributions data frames for the `climdist` Shiny app.
-   Compiling data frames of cell indices of the intersection of commonly used SNAP polygon shapefiles with common rasterized map layers for subsequent efficient processing of large data extraction tasks.
-   Uploading curated data sets to Amazon Web Services S3 buckets for rapid access remote use by various SNAP applications and projects.

Installation
------------

You can install snapprep from github with:

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/snapprep")
```

Reference
---------

[Complete package reference and function documentation](https://leonawicz.github.io/snapprep/)
