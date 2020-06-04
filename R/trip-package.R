#' trip.
#'
#' Functions for accessing and manipulating spatial data for animal
#' tracking, with straightforward coercion from and to other formats. Filter
#' for speed and create time spent maps from animal track data. There are
#' coercion methods to convert between 'trip' and 'ltraj' from 'adehabitatLT', 
#' and between 'trip' and 'psp' and 'ppp' from 'spatstat'. Trip objects
#' can be created from raw or grouped data frames, and from types in the 'sp', 
#' 'sf', 'amt', 'trackeR', and other packages. 
#' @name trip-package
#' @docType package
#' @importFrom grDevices hsv 
#' @importFrom stats approxfun density dnorm nlm 
#' @importFrom utils tail 
NULL



#' Walrus tracking data set. 
#' 
#' Behavior of Pacific Walruses Tracked from the Alaska Coast of the Chukchi Sea. 
#' 
#' Data set is provided as a 'trip' object. This is the abstract for the work: 
#' 
#' "We tracked movements and haulout foraging behavior of walruses instrumented with 
#' satellite-linked data loggers from the Alaskan shores of the Chukchi Sea during the 
#' autumn of 2009 (n=13) and 2010 (n=2)."
#'  Jay, C. V. and Fischbach, A.S.
#'  
#' @examples
#' data(walrus818)
#' plot(walrus818)
#' lines(walrus818)
#' 
#' ##dontdoanything
#' ## library(mapview)
#' ##mapview(as(walrus818, "SpatialLinesDataFrame"), burst = TRUE)
#' @name walrus818
#' @docType data
#' @rdname walrus818
NULL

#' World north polygons
#' 
#' A spatial polygons object with coastlines of the northern hemisphere. 
#' 
#' This data set exists purely to avoid requiring reprojection in the
#' vignette, the data uses the same projection as [walrus818]. 
#' @name world_north
#' @docType data
"world_north"
