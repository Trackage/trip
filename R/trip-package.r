#' trip.
#'
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
#' "We tracked movments and haulout foraging behavior of walruses instrumented with 
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