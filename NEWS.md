# trip 1.8.0

* Updated and fixed some tests thanks to CRAN. 

* New function `interp_equal()` for intermediate points by distance or duration. 

* Fixed spurious warnings in tests, thanks to Roger Bivand #45. 

# trip 1.7.1

* Fixed documentation cross-references to sp topic, thanks to CRAN. 

* Migrate to reproj to avoid rgdal package. Ancient function tripTransform has been removed completely, and 
spTransform methods are now  defunct and suggest use of `reproj()`. 

* Removed problematic PROJ strings, thanks to Roger Bivand. 

* `readDiag()` and `readArgos()` now read all records from a single text vector after reading
all raw lines. This means they can be used to read data from mixed text streams.  

* `readDiag()`  no longer converts columns id and iq to factor, and gains arguments `return_trip` and 
 `read_alt` to control output format. 


# trip 1.6.0

## MAJOR CHANGES

* BREAKING change: the sp package is now imported rather than full Depends. 
 This means that calls to its functions like `coordinates<-`, `CRS<-`, 
 `proj4string<-`, `spTransform` will need to be prefixed with  `sp::`, or 
  with the package explicitly attached with `library(sp)`. This may cause some 
  pain but should be offset by improvements in helpful ways to create trip objects. 
 
* The `trip()` function now accepts raw data frames or grouped data frames, with 
 the first four columns assumed to be the x, y, time coordinates and the ID 
 column. If the data frame is grouped the first grouping
 column is used as the ID. 
 
* When trip objects are created they are by default forced into basic data sense, 
 and so will usually succeed on creation as long as the nominated coordinates, 
 trip group ID and date-times are sensible. The data will be automatically cleaned 
 up for 1) duplicated records 2) duplicated date-times 3) records out of order. 

* `summary(trip)` now does not calculate the rolling RMS speed for performance 
 reasons, this might be reviewed in future. 
 
* New function `write_track_kml` to produce a compressed Keyhole Markup Language 
 ('KML') file in 'KMZ' form, for use in Google Earth. 

## MINOR CHANGES

* `readArgos` gains a new argument `read_alt` which is set to NULL by default and 
 maintains existing behaviour. If set to  1 or 2 the corresponding "alternative" 
 location is returned. 1 is a standardized location corresponding 
 to the original PRV message, and 2 is a "dummy" location. Added by request of 
 Chris Ward. 
 
* Now using geodist package rather than internal distance function. There's no 
 speed benefit for moderately small data sets (1000s of points) but there is for 
 much larger data. Some small numeric changes in distances when using longitude, latitude 
 data - in `trackDistance` and `homedist` but at the level of metres in terms of precision. 

*  New support to coerce to sf with `as(x, "sf")` which creates every segment 
 as a LINESTRING. If `sf::st_as_sf()` is  used standard coercion from sp -> sf is used and 
 results in POINT. 

* Add `trip()` support including coercion from and to the `amt` package type `track_xyt`. 

* Add `trip()` support for `trackeRdata` objects from the `trackeR` package, `mousetrap` 
 objects from the `mousetrap` package. Conversion to these types from trip is not practical. 

* Add `trip())` support for `sf` objects with POINT geometry. The column names for date-time 
 and id must be supplied. Z or M values from XYZ, and XYZM, and XYM are currently ignored. 

* Add partial `trip()` support for `telemetry` objects from the `ctmm` package, 
 see #33. 

# trip 1.5.0 and prior

* See file [ONEWS](https://github.com/Trackage/trip/blob/master/inst/ONEWS). 
