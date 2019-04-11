
#' Coercion from other classes to \code{trip} objects
#'
#' Coercing objects to \code{trip} class
#'
#' @name as.trip
#' @aliases as.trip-methods as.trip as.trip,ltraj-method ltraj2trip
#' coerce,trip,ltraj-method
#' @docType methods
#' @param x, ltr ltraj object
#' @param \dots Arguments passed to other methods. Ignored for \code{ltraj}
#' method.
#' @section Methods:
#'
#' \describe{
#'
#' \item{coerce}{\code{signature(from="ltraj", to="trip")}}
#'
#' \item{as.trip}{\code{signature(x="ltraj")}}
#'
#' }
#' @examples
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' sp::coordinates(d) <- ~x+y
#' ## this avoids complaints later, but these are not real track data (!)
#' sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere")
#' tr <- trip(d, c("tms", "id"))
#' 
#' if (require(adehabitatLT)) {
#'     ##l <- as.ltraj.trip(tr)
#'     ##ltraj2trip(l)
#'     ##as.trip(l)
#' }
##' @rdname as.trip-methods
##' @export
setGeneric("as.trip",
           function(x, ...) standardGeneric("as.trip"))

##' @export
ltraj2trip <- function (ltr)
{
  requireNamespace("adehabitatLT") ||
    stop("adehabitatLT package is required, but unavailable")
  if (!inherits(ltr, "ltraj"))
    stop("ltr should be of class \"ltraj\"")
  ltr <-  lapply(ltr, function(x) {
    x$id=attr(x,  "id")
    x$burst=attr(x,  "burst")
    x})
  tr <- do.call("rbind", ltr)
  class(tr) <- "data.frame"
  xy <- tr[!is.na(tr$x), c("x", "y")]
  tr <- tr[!is.na(tr$x), ]
  tr$y <- tr$x <- NULL
  res <- SpatialPointsDataFrame(xy, tr)
  trip(res, c("date", "id"))
}

setMethod("as.trip", signature(x="ltraj"),
          function(x, ...) ltraj2trip(x))

setAs("ltraj", "trip", function(from) as.trip(from))

