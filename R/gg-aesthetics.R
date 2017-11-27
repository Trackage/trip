#' specify trip by standard grammar 
#' 
#' Construct 'trip' objects from user-nominated variables, grouping and arrangement. 
#' Currently hardcoded to require 'x', 'y', 'time' and 'id'. 
#'
#' longitude latitude is assumed if data is in reasonable range of that domain. 
#' @param .data data frame
#' @param ... named expressions in the form 'x = longitude'
#' @param crs projection in PROJ.4 form
#'
#' @return trip object
#' @export
#' @importFrom rlang quos 
#' @importFrom dplyr select
#' @importFrom raster couldBeLonLat
#' @importFrom sp CRS SpatialPoints SpatialPointsDataFrame
#' @examples
#' d <- tibble::as_tibble(as.data.frame(walrus818[1:100, ]))
#' g <- trip2(d, x = X_AED170_70, y = Y_AED170_70, time = DataDT, id = Deployment)
trip2 <- function(.data, ..., crs = NULL) {

  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  if (!all(names(quo_named) %in% c("x", "y", "time", "id"))) {
    stop("we must have x = ,  y = , time = , id =  ")
    ## future versions will allow x, y by position, and leave id as optional
  }
   xytg <- dplyr::select(.data, !!!quo_named)
   if (is.null(crs)) {
     if (raster::couldBeLonLat(as.matrix(xytg[,1:2]))) {
       crs <- "+init=epsg:4326"
       warning("assuming data is long-lat, on WGS84 - use 'crs' to assign explicitly")
     } else {
       crs <- NA_character_
       warning("no known projection, crs set to NA -  use 'crs' to assign explicitly")
     }
   }
      if (ncol(.data) == 4) {
     .data <- tibble::tibble(nominal = seq_len(nrow(.data)))
   } else {
     .data <- tibble::as_tibble(.data[, base::setdiff(names(.data), names(xytg))])
   }
   new("trip", SpatialPointsDataFrame(SpatialPoints(as.matrix(xytg[,1:2]), 
                                      proj4string = CRS(crs)), 
       cbind(xytg[, 3:4], .data)), TimeOrderedRecords(c("time", "id")))
}


#' @importFrom dplyr filter
filter.trip <- function(.data, ...)  {
  ## how do we achiever this?
  ## filter(speedfilter(x, y, time))
}
