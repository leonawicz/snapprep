
<!-- README.md is generated from README.Rmd. Please edit that file -->
snapprep
========

[![Travis-CI Build Status](https://travis-ci.org/leonawicz/snapprep.svg?branch=master)](https://travis-ci.org/leonawicz/snapprep) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/leonawicz/snapprep?branch=master&svg=true)](https://ci.appveyor.com/project/leonawicz/snapprep) [![Coverage Status](https://img.shields.io/codecov/c/github/leonawicz/snapprep/master.svg)](https://codecov.io/github/leonawicz/snapprep?branch=master)

The `snapprep` package contains R functions used to support a wide range of SNAP projects by preparing and curating useful data sets from upstream raw SNAP data. The data sets compiled with the aid of `snapprep` are then made avaible to other projects. This includes compiling data sets that are contained in SNAPverse data packages.

`snapprep` is a developer package used by the SNAPverse author and maintainer. For user packages catering to analysis and graphing of the curated data sets available in SNAPverse data packages, see the `snapfuns` package instead.

Installation
------------

You can install snapprep from github with:

``` r
# install.packages('devtools')
devtools::install_github("leonawicz/snapprep")
```
