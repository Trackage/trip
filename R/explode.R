## explode a trip into line segments

##' Break a trip into its component line segments
##'
##' Function to create a SpatialLinesDataFrame from a trip object, resulting in a line segment for each implicit segment along the tracks. The object stores the start and end times, duration and the ID of the segment. 
##' @title .explode
##' @param x trip object
##' @param ... reserved for future methods
##' @return SpatialLinesDataFrame
##' @examples
##' ## Continuing the example from '?trip-methods:
##' utils::example("trip-methods", package="trip",
##'            ask=FALSE, echo=FALSE)
##' spldf <- trip:::.explode(tr)
##' summary(tr)
.explode <- function(x, ...) {
  tor <- getTORnames(x)
  id <- x[[tor[2]]]
  xs <- split(x, id)
  df <- do.call("rbind",
                lapply(xs, function(x) {
                  n <- nrow(x)
                  tms <- x[[tor[1]]]
                  data.frame(starttime = tms[-n], endtime = tms[-1], timedur = diff(unclass(tms)), id = x[[tor[2]]][-1])
                }
                       )
                )
  Linelist <- vector("list", nrow(df))
  cnt <- 0
  for (i in seq_along(xs)) {
    this.x <- xs[[i]]
    this.coords <- coordinates(this.x)
    for (j in seq_len(nrow(this.x)-1)) {
      cnt <- cnt + 1
      Linelist[[cnt]] <- Lines(list(Line(this.coords[j:(j+1), ])), rownames(df)[cnt])
    }

  }
  splines <- SpatialLines(Linelist, proj4string = CRS(proj4string(x)))
  SpatialLinesDataFrame(splines, df)
}


# setMethod("lines", signature(x="trip"),
#           function(x,
#                    col=hsv(seq(0, 0.9, length = length(unique(x[[getTORnames(x)[2]]]))),
#                            0.8, 0.95),
#                    ...) {
#             x <- .explode(x)
#             col <- heat_hcl(25, h = c(0, -100), l = c(55, 40), c = c(40, 80), power = 3)
#             times <- x$time
#             val <- scl(unclass(times)) * (length(col)-1) + 1
#
#             plot(x,  col=col[val], add=TRUE, ...)
#
#           })
