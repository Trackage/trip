# trip dev

* Now using geodist package rather than internal distance function. There's no speed benefit for moderately small
 data sets (1000s of points) but there is for much larger data. 
 
*  New support to coerce to sf with `as(x, "sf")` which creates every segment as a LINESTRING. If `sf::st_as_sf()` is 
 used standard coercion from sp -> sf is used and results in POINT. 


# trip 1.5.0 and prior

* See file ONEWS. 
