# trip dev

MAJOR CHANGES

* When trip objects are created they are by default forced into basic data sense, and so will 
 usually succeed on creation as long as the nominated coordinates, trip group ID and date-times 
 are sensible. The data will be automatically cleaned up for 1) duplicated records 2) duplicated 
 date-times 3) records out of order. 
 
MINOR CHANGES

* Now using geodist package rather than internal distance function. There's no speed benefit for moderately small
 data sets (1000s of points) but there is for much larger data. 
 
*  New support to coerce to sf with `as(x, "sf")` which creates every segment as a LINESTRING. If `sf::st_as_sf()` is 
 used standard coercion from sp -> sf is used and results in POINT. 


# trip 1.5.0 and prior

* See file ONEWS. 
