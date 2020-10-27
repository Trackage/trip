
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/Trackage/trip/workflows/R-CMD-check/badge.svg)](https://github.com/Trackage/trip/actions)
<!-- badges: end -->

[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/Trackage/trip?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/Trackage/trip)
[![OSX Build
Status](http://badges.herokuapp.com/travis/Trackage/trip?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/Trackage/trip)
[![Build
status](https://ci.appveyor.com/api/projects/status/e2r8t15huw9yopbq?svg=true)](https://ci.appveyor.com/project/mdsumner/trip)
[![Coverage\_Status](https://img.shields.io/codecov/c/github/Trackage/trip/master.svg)](https://codecov.io/github/Trackage/trip?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/trip)](https://cran.r-project.org/package=trip)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/trip)](https://cran.r-project.org/package=trip)

# Tools for animal track data

The trip package provides facilities to access and manipulate
spatial-temporal data organized for tracking movements. Data with
locations, date-times, and grouping IDs for trips are formally declared
using the `trip()` function and enables easy calculation of step
distance and angle and grouped summaries of trip duration and length.

Conversion from and to many disparate formats is a key focus, and data
may be treated as points-in-time or as line segments with a start/end
time property. There are functions for simple speed-distance-angle
filtering determining time-spent in area and creating line or point
plots with standard R spatial tools and graphics.

## Reading data

Track data may be read from various Argos formats with `readArgos()` or
`readDiag()`, from generic data frame or spatial- data frames, and many
types of objects from other tracking packages are supported.

## Converting data

Trip objects can be imported directly from types defined in packages
`adehabitatLT`, `amt`, `ctmm`, `eyetrackeRdata`, `mousetrap`, `sf`, and
`sp`. When these formats do not have time or trip-grouping values they
may be declared by name. A generic data frame may be converted to trip
by organizing the x, y, time, ID columns in order, or by a combination
of row grouping with x, y, time columns. All other data are retained in
the obvious way.

Trip objects can be used to create other data types defined by
`adehabitatLT`, `amt`, `sf`, `sp`, `spatstat`, and this is possible for
conversion to point types or line types. For example, `as(trip,
"SpatialLinesDataFrame")` creates a single multi-line for each trip,
with start and end time properties. The `explode(trip)` function breaks
a trip into individual line segments for every pair of coordinates, and
`as(trip, "SpatialPointsDataFrame")` will create a standard sp points
data frame. There are similar facilities for `sf` types, and for the
`spatstat` types point pattern `as.ppp(trip)` or line segments
`as.psp(trip)`.

Trips can be exported to Google Earth and explored using the continuous
time slider with the `write_track_kml()` function.

## Validating data

The `trip()` function performs a significant series of data validation,
to ensure that basic sense prevails. The function will always proceed to
give a valid trip object, and simply warn about which problems had to be
overcome. These validation checks can be used to discover what is wrong
with a particular set of data, and in time we hope this to become much
more powerful and transparent.

## Transforming and merging data

Trip objects can be reproject directly with the `spTransform` function
in the usual way, and functions that are coordinate system aware such as
`raster::extract` will automatically reproject the coordinates behind
the scenes. We are developing more sophisticated [point-in-time data
extraction](https://github.com/AustralianAntarcticDivision/raadtools)
routines and [auto-reprojecting map
plotting](https://github.com/AustralianAntarcticDivision/SOmap)
functions in related projects.

## Simple data filtering and time-spent gridding

Some simple data summary methods have gone out of favour or been
ignored, and trip provides direct access to the `speedfilter()`, `sda()`
and `rasterize()` functions for straightforward data filtering on
maximum speed, minimum angle/distance, and time-spent-in-area gridding.

There is a `homedist()` function to calculate the maximum distance from
each trip’s starting point, and various old-school functions. Some of
these will be improved and better exposed in time.

See `vignette("trip")` for more.

Let us know what you think via
[Issues](https://github.com/Trackage/trip/issues).

## Installing

The package is easily installed from CRAN in R.

``` r
install.packages("trip")
```

## Install dev version

To install the development package from Github:

``` r
remotes::install_github("Trackage/trip")
```

# Development

  - trip should work with sf or sp, there’s really no need to
    distinguish these
  - ability to treat all line segments as continuous, as `cut.trip` can
    for rasterizing, possibly needs a second-level ID grouping
  - separate out reading functionality to
    [rgos](https://github.com/Trackage/rgos)
  - separate out algorithm functions into a generic package that other
    tracking packages could use (distance, speed, angle filtering,
    rasterizing, time spent, interpolation etc.)
  - Better summary tools, the SGAT (and tripEstimation) packages have
    functions for dealing with spatial track summaries that are atomized
    to the level of each time step.
  - More conversion tools between formats
  - Validation must include a detailed report object of where the
    problems are, and how to filter/fix/flush them.

Trip would be better implemented on top of the ideas in [the silicate
package](https://github.com/hypertidy/silicate/), rather than the
twilight zone between MULTIPOINTs and LINESTRINGs as it is now.

Please me know what you think via
[Issues](https://github.com/Trackage/trip/issues).

-----

\#\# Code of Conduct

Please note that the trip project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
