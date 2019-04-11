
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](http://badges.herokuapp.com/travis/Trackage/trip?branch=master&env=BUILD_NAME=trusty_release&label=linux)](https://travis-ci.org/Trackage/trip)
[![OSX Build
Status](http://badges.herokuapp.com/travis/Trackage/trip?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/Trackage/trip)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Trackage/trip?branch=master&svg=true)](https://ci.appveyor.com/project/Trackage/trip)[![Coverage\_Status](https://img.shields.io/codecov/c/github/Trackage/trip/master.svg)](https://codecov.io/github/Trackage/trip?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/trip)](https://cran.r-project.org/package=trip)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/trip)](https://cran.r-project.org/package=trip)

# Tools for animal track data

The trip package provides functions for accessing and manipulating
spatial data for animal tracking. Filter for speed and create time spent
plots from animal track data.

Let me know what you think via
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

# Current concerns

Currently working on tidying up old daggy ends, see [next release
milestones](https://github.com/Trackage/trip/milestone/2).

I want it to be very easy to create trip objects from other forms,
either by reading known formats (Argos), coercion from other packages
(sp, sf, adehabitatLT, move, etc.), or from raw data with a dplyr/gg
aesthetic e.g. this kind of construct from a data
frame:

``` r
d <- read.csv(system.file("extdata/MI_albatross_sub10.csv", package = "trip"), stringsAsFactors = FALSE)
d$gmt <- as.POSIXct(d$gmt, tz = "UTC")
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(trip)
d %>% arrange(tag_ID, gmt) %>% group_by(tag_ID) %>%  select(lon, lat, gmt, everything()) %>% trip()
#> Warning in assume_if_longlat(out): input looks like longitude/latitude
#> data, assuming +proj=longlat +datum=WGS84
#> 
#> Object of class trip
#>    tripID ("tag_ID") No.Records   startTime ("gmt")     endTime ("gmt")
#> 1               1186         90 2005-12-14 15:27:39 2007-03-03 19:53:49
#> 2               1187         71 2005-12-22 10:27:43 2006-12-06 10:16:23
#> 3               1190        109 2006-01-11 12:00:00 2009-06-01 12:00:00
#> 4               1191         25 2005-12-05 12:00:00 2006-08-01 12:00:00
#> 5               1192         74 2005-12-09 17:20:09 2006-12-07 16:53:54
#> 6               1194         72 2006-01-09 10:34:58 2006-12-27 10:36:06
#> 7               1195         22 2005-12-15 16:22:30 2006-03-30 19:29:15
#> 8               1198         76 2005-12-06 12:00:00 2007-12-23 12:00:00
#> 9               1200        149 2005-12-02 12:00:00 2009-12-06 12:00:00
#> 10              1203         35 2005-12-02 12:00:00 2006-12-10 12:00:00
#> 11              2123          6 1998-12-16 12:00:00 1999-02-02 12:00:00
#> 12             12604         28 1992-11-13 18:23:00 1992-12-07 11:16:00
#> 13             12606         25 1992-11-13 18:22:00 1992-12-10 04:04:00
#> 14             12609         17 1992-11-14 04:11:00 1992-12-04 00:17:00
#> 15             14257        169 2001-12-06 06:29:19 2006-04-22 15:35:34
#> 16             14403         48 2001-12-02 09:00:48 2001-12-18 19:23:05
#> 17             14418        242 2001-12-02 13:58:52 2006-06-26 01:38:46
#> 18             20874         43 1999-12-20 20:48:45 2000-12-24 20:46:29
#> 19             20875        103 1999-12-20 17:32:46 2001-12-21 15:43:17
#> 20             20876         68 1999-12-20 09:17:04 2000-12-15 10:58:48
#> 21             20877        345 1999-12-20 10:57:51 2006-07-04 18:19:19
#> 22             37388        120 2006-01-03 01:36:57 2006-03-14 04:54:22
#> 23             37389        100 2006-01-05 19:43:22 2006-03-11 11:28:12
#> 24             37467        225 2002-12-09 07:31:00 2006-07-06 18:00:50
#> 25             37468         87 2002-12-09 11:36:47 2003-01-13 06:52:43
#> 26             37629        100 2006-02-15 08:24:31 2006-06-01 12:52:08
#> 27             37631        127 2006-02-21 08:03:41 2006-07-05 21:47:57
#> 28             43921        108 2003-12-29 01:20:42 2004-02-07 20:01:02
#> 29             43922        170 2004-01-01 06:00:23 2004-02-25 19:40:44
#> 30             55166         22 2004-12-15 23:15:17 2005-03-17 22:50:00
#> 31             55167         15 2004-12-14 00:00:03 2005-02-08 22:13:35
#>     tripDuration
#> 1  444.1848 days
#> 2  348.9921 days
#> 3      1237 days
#> 4       239 days
#> 5  362.9818 days
#> 6  352.0008 days
#> 7  105.1297 days
#> 8       747 days
#> 9      1465 days
#> 10      373 days
#> 11       48 days
#> 12 23.70347 days
#> 13 26.40417 days
#> 14  19.8375 days
#> 15 1598.379 days
#> 16 16.43214 days
#> 17 1666.486 days
#> 18 369.9984 days
#> 19  731.924 days
#> 20 361.0706 days
#> 21 2388.307 days
#> 22 70.13709 days
#> 23 64.65613 days
#> 24 1305.437 days
#> 25 34.80273 days
#> 26 106.1858 days
#> 27 134.5724 days
#> 28 40.77801 days
#> 29 55.56969 days
#> 30 91.98244 days
#> 31 56.92606 days
#> 
#>        data.columns data.class                  
#> 1               gmt    POSIXct **trip DateTime**
#> 2              band     factor                  
#> 3   breeding_status     factor                  
#> 4           species     factor                  
#> 5               sex     factor                  
#> 6 deployment_status     factor                  
#> 7            device     factor                  
#> 8            tag_ID    integer **trip ID**

## OR this for the same outcome
tr <- trip(dplyr::select(d, lon, lat, gmt, tag_ID, everything()))
#> Warning in assume_if_longlat(out): input looks like longitude/latitude
#> data, assuming +proj=longlat +datum=WGS84
```

Longitude latitude is assumed if the values have sane ranges.

There is some ongoing integration with the `sf` package, mostly to
provide outputs in its formats.

We can already convert to POINT:

``` r
library(trip)
sf::st_as_sf(walrus818)
#> Simple feature collection with 10558 features and 4 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: -117277 ymin: -412557 xmax: 307789 ymax: 84896
#> epsg (SRID):    NA
#> proj4string:    +proj=aeqd +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs
#> First 10 features:
#>    Deployment              DataDT Wet Forage             geometry
#> 1         353 2009-09-15 04:00:00   1      0 POINT (281017 22532)
#> 2         353 2009-09-15 05:00:00   0      0 POINT (281399 22392)
#> 3         353 2009-09-15 06:00:00   0      0 POINT (281209 22218)
#> 4         353 2009-09-15 07:00:00   0      0 POINT (281376 22175)
#> 5         353 2009-09-15 08:00:00   0      0 POINT (281543 22132)
#> 6         353 2009-09-15 09:00:00   0      0 POINT (281710 22089)
#> 7         353 2009-09-15 10:00:00   0      0 POINT (281877 22046)
#> 8         353 2009-09-15 11:00:00   0      0 POINT (282044 22003)
#> 9         353 2009-09-15 12:00:00   0      0 POINT (282211 21961)
#> 10        353 2009-09-15 13:00:00   0      0 POINT (282378 21918)
```

or atomic LINESTRING with time on the MEASURE:

``` r
as(walrus818, "sf")
#> Simple feature collection with 14 features and 4 fields
#> geometry type:  LINESTRING
#> dimension:      XYM
#> bbox:           : -117277 : -412557 : 307789 : 84896
#> epsg (SRID):    NA
#> proj4string:    +proj=aeqd +ellps=WGS84 +lon_0=-170 +lat_0=70
#> First 10 features:
#>     tripID           tripStart             tripEnd tripDur
#> 353    353 2009-09-15 04:00:00 2009-09-30 19:00:00 1350000
#> 354    354 2009-09-16 04:00:00 2009-10-20 00:00:00 2923200
#> 355    355 2009-09-16 04:00:00 2009-11-04 19:00:00 4287600
#> 356    356 2009-09-16 23:00:00 2009-11-21 21:00:00 5695200
#> 357    357 2009-09-16 23:00:00 2009-11-09 22:00:00 4662000
#> 358    358 2009-09-16 23:00:00 2009-10-18 19:00:00 2750400
#> 359    359 2009-09-16 23:00:00 2009-10-30 19:00:00 3787200
#> 361    361 2009-09-17 01:00:00 2009-11-06 04:00:00 4330800
#> 362    362 2009-09-17 01:00:00 2009-10-03 17:00:00 1440000
#> 366    366 2009-09-19 15:00:00 2009-10-12 20:00:00 2005200
#>                           geometry
#> 353 LINESTRING M (281017 22532 ...
#> 354 LINESTRING M (267900 -9078 ...
#> 355 LINESTRING M (267657 -9668 ...
#> 356 LINESTRING M (268080 -9030 ...
#> 357 LINESTRING M (263622 -8147 ...
#> 358 LINESTRING M (267884 -9155 ...
#> 359 LINESTRING M (268330 -8509 ...
#> 361 LINESTRING M (305229 55449 ...
#> 362 LINESTRING M (305740 56129 ...
#> 366 LINESTRING M (208779 -53699...
```

But, trip itself would be better implemented on top of the ideas in [the
silicate package](https://github.com/hypertidy/silicate/), rather than
the twilight zone between MULTIPOINTs and LINESTRINGs as it is now.

# Development

  - trip should work with sf or sp, there’s really no need to
    distinguish these
  - ability to treat all line segments as continuous, as `cut.trip` can
    for rasterizing, possibly needs a second-level ID grouping
  - separate out reading functionality to
    [rgos](https://github.com/mdsumner/rgos)
  - separate out algorithm functions into a generic package that other
    tracking packages could use (distance, speed, angle filtering,
    rasterizing, time spent, interpolation etc.)

Let me know what you think via
[Issues](https://github.com/Trackage/trip/issues).

## TODO

  - **Probability image**. The SGAT (and tripEstimation) packages have
    functions for dealing with spatial track summaries that are atomized
    to the level of each time step. There are methods for combining
    summaries from multiple tracks and for casting arbitrary durations
    (by sum) to standard image structures. This would be a good feature
    to replace the existing tripGrid function by storing the individual
    grid summaries for each implicit line segment.

  - **Coercion to/from other classes** See
    [spbabel](https://github.com/mdsumner/spbabel).

  - **Validation** Must include a detailed report object of where the
    problems are, and how to filter/fix/flush them.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.
