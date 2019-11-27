#' Reprojection
#' 
#' A reproj method for trip objects. 
#'
#' @param x trip object
#' @param target target projection
#' @param ... ignored
#' @param source projection of source data, usually ignore this for trips
#'
#' @return a trip reprojected to 'target'
#' @importFrom reproj reproj
#' @export reproj
#' @name reproj
#' @export
reproj.trip <- function(x, target, ..., source = NULL) {
  xy <- x@coords
  x@coords <- suppressWarnings(reproj::reproj(xy, target = target, 
                           source = x@proj4string@projargs)[, 1:2])
  x@proj4string <- sp::CRS(target, doCheckCRSArgs = FALSE)
  xr <- range(x@coords[,1])
  yr <- range(x@coords[,2])
  x@bbox <- rbind(x = c(min = xr[1L], max = xr[2L]), 
                y = c(min = yr[1L], max = yr[2L]))
  x
}

