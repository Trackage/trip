# trip dev

MAJOR CHANGES

* When trip objects are created they are by default forced into basic data sense, and so will 
 usually succeed on creation as long as the nominated coordinates, trip group ID and date-times 
 are sensible. The data will be automatically cleaned up for 1) duplicated records 2) duplicated 
 date-times 3) records out of order. 

* `summary(trip)` now does not calcuate the rolling RMS speed for performance reasons, this might be reviewed in future. 

MINOR CHANGES

* `readArgos` gains a new argument `read_alt` which is set to NULL by default and maintains existing behaviour. If set to 
 1 or 2 the corresponding "alternative" location is returned. 1 is a standardized location corresponding to the original 
 PRV message, and 2 is a "dummy" location. Added by request of Chris Ward. 
 
* Now using geodist package rather than internal distance function. There's no speed benefit for moderately small
 data sets (1000s of points) but there is for much larger data. Some small numeric changes in distances when using
 longitude, latitude data - in `trackDistance` and `homedist` but at the level of metres in terms of precision. 

 
*  New support to coerce to sf with `as(x, "sf")` which creates every segment as a LINESTRING. If `sf::st_as_sf()` is 
 used standard coercion from sp -> sf is used and results in POINT. 


# trip 1.5.0 and prior

* See file ONEWS. 
