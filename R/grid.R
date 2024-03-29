
## TODO:
## a version of tripGrid that takes Lines, so
## as.SpatialLinesDataFrame.trip, and then to grid

## allow sigmas argument for density version, for each line segment

## replaces tripGrid, old version is now called tripGrid.interp



trip_raster <- function(x, grid = NULL, method = "pixellate",  ...) {
  if (!is.null(grid) & !is(grid, "GridTopology")) grid <- as(grid, "GridTopology")

  if (method == "pixellate") {
    out <- traipse_time_spent(x, grid)
  } else {
   out <-     raster(tripGrid(x, grid = grid, method = method, ...))
  }
  out
}

#' Rasterize trip objects based on line-segment attributes.
#'
#' Trip rasterize.
#' @name rasterize
#' @aliases rasterize,trip,RasterLayer-method rasterize,trip,missing-method
#' @export
#' @importFrom raster raster rasterize
#' @exportMethod rasterize trip
#' @param x \code{trip} object
#' @param y Raster* object
#' @param field attribute from which differences will be calculated, defaults to the time-stamp between trip locations
#' @examples
#'  d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
#' tr <- trip(d, c("tms", "id"))
#'
#' tr$temp <- sort(runif(nrow(tr)))
#' r <- rasterize(tr)
#'
#' rasterize(tr, grid = r)
#' \donttest{
#' rasterize(tr, r, field = "temp")
#' rasterize(tr, method = "density")
#' rasterize(tr, method = "density", grid = r)
#'
#' rasterize(tr, r, field = "tms")
#' rasterize(tr, r)
#'
#' }
#'
#' @return RasterLayer
NULL

setMethod("rasterize", signature(x = "trip", y = "missing"), trip_raster)
setMethod("rasterize", signature(x = "trip", y = "RasterLayer"),
          function(x, y, field, fun = 'pixellate', ...)
          {
            if (!is.na(raster::projection(x)) && !is.na(raster::projection(y))) {
              x <- reproj(x, raster::projection(y))
            }
            if (missing(field)) {
              if (!is.null(y)) y <- as(y, "GridTopology")

              if (fun == "density") {
               out <- raster(tripGrid(x, grid = y,  method = fun, ...))
              } else {
               out <-   traipse_time_spent(x, grid = y)
              }
            } else {
              ll <- explode(x)
              diffvals <- unlist(lapply(split(x[[field]], x[[getTORnames(x)[2]]]), diff))
              ##ll[[field]] <- diffvals
              out <- rasterize(ll, y, field = diffvals, fun = 'sum', ..., na.rm = TRUE)
            }
            out
          }
            )


#' @importFrom crsmeta crs_proj
#' @importFrom traipse track_time
traipse_time_spent <- function(xx, grid = NULL) {
  if (is.null(grid)) {
    grid <- makeGridTopology(xx)
  }
  x.psp <- spatstat.geom::as.psp(xx)
  xy <- coordinates(xx)
  dd <- data.frame(x = xy[,1], y = xy[,2],
                   time = xx[[getTORnames(xx)[1]]],
                   id = xx[[getTORnames(xx)[2]]], stringsAsFactors = FALSE)

#  dt <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(dd, .data$id),
#                                         dt = traipse::track_time(.data$time)))[["dt"]]
  dd[["dt"]] <- rep(NA_real_, dim(dd)[1L])
  split_df <- split(dd, dd[["id"]])[as.character(unique(dd[["id"]]))]
#  browser()
  for (i in seq_along(split_df)) {
    split_df[[i]][["dt"]] <- traipse::track_time(split_df[[i]][["time"]])
  }
  dt <- do.call(rbind, split_df)[["dt"]]
  dt_na <- dt[!is.na(dt)]
  lngths <- spatstat.geom::lengths_psp(x.psp)
  zeros <- which(!lngths > 0)
  ow <- .g2ow(grid)
  if (length(zeros) > 1) {
    grd <- spatstat.geom::pixellate(x.psp[-zeros], W= ow,
                               weights=(dt_na/lngths)[-zeros])
  } else {
    grd <- spatstat.geom::pixellate(x.psp, W=ow,
                               weights=dt_na/lngths)
  }

  r0 <- raster(grd,
               crs = crsmeta::crs_proj(xx))
  ## zero points
  if (length(zeros) > 0) {
    zp <- data.frame(x = x.psp$ends$x1[zeros], y = x.psp$ends$y1[zeros])
    zp$cell <- raster::cellFromXY(r0, cbind(zp$x, zp$y))
    zp$val <- dt_na[zeros]
    #z <- dplyr::summarise(dplyr::group_by(zp, .data$cell), s = sum(.data$val))
    z <- tapply(zp$val, zp$cell, sum)
    #browser()
    r1 <- raster::setValues(r0, 0)
    r1[as.integer(names(z))] <- z
    r0 <- r0 + r1
  }
  r0
}
#' Generate a grid of time spent by line-to-cell gridding
#'
#'
#' Create a grid of time spent from an object of class \code{trip} by exact
#' cell crossing methods, weighted by the time between locations for separate
#' trip events.
#'
#'
#' Zero-length lines cannot be summed directly, their time value is summed by
#' assuming the line is a point. A warning used to be given, but as it achieved
#' nothing but create confusion it has been removed. The density method returns
#' proportionate values, not summed time durations.
#'
#' See \code{pixellate.psp} and \code{pixellate.ppp} for the details on the
#' method used. See \code{density.psp} for method="density".
#'
#' Trip events are assumed to start and end as per the object passed in. To
#' work with inferred "cutoff" positions see \code{split.trip.exact}.
#'
#' @param x object of class \code{trip}
#' @param grid GridTopology - will be generated automatically if NULL
#' @param method pixellate or density
#' @param \dots pass arguments to density.psp if that method is chosen (and
#' temporary mechanism to direct users of legacy methods to
#' \code{\link{tripGrid.interp}})
#' @return
#'
#' \code{tripGrid} returns an object of class \code{SpatialGridDataFrame}, with
#' one column "z" containing the time spent in each cell in seconds.
#' @keywords manip
#' @export tripGrid
#' @importFrom spatstat.explore density.ppp density.psp
#' @importFrom spatstat.geom psp ppp [.psp pixellate pixellate.psp lengths_psp  owin
tripGrid <- function (x, grid=NULL, method="pixellate", ...)
{
    if (method %in% c("kde", "count")) {
        msg <- paste("'kde' and 'count' methods no longer supported, using 'pixellate'",
                     "from trip_1.1-6 and will be ignored,",
                     "see ?tripGrid.interp for legacy function")
        warning(msg)
        method <- "pixellate"
    }
    if (!is.null(list(...)$dur)) {
        msg <- paste("dur(ation) not necessary for this function from",
                     "trip_1.1-6 and will be ignored - time sum is now",
                     "exact\n see ?tripGrid.interp for legacy function")
        stop(msg)
    }
    if (is.null(grid))
    grid <- makeGridTopology(x)
    spgdf <- SpatialGridDataFrame(grid,
                                  data.frame(z=rep(0, prod(grid@cells.dim))))
    res <- as.image.SpatialGridDataFrame(spgdf)
    tor <- x@TOR.columns

   trip.list <- split.data.frame(x[, tor], x[[tor[2]]])
    ow <- .g2ow(grid)
    sm <- 0
    sz <- 0

    for (this in trip.list) {
        xs <- coordinates(this)[, 1]
        ys <- coordinates(this)[, 2]
        dt <- diff(unclass(this[[tor[1]]]))
        sm <- sm + sum(dt)
        x.psp <- psp(xs[-length(xs)], ys[-length(ys)], xs[-1],
                               ys[-1], window=ow)

        lngths <- lengths_psp(x.psp)
        if (any(!lngths > 0)) {
            ## trim psp objects (0-lines give NaNs)
            zeros <- which(!lngths > 0)
            cc <- coordinates(this)[zeros, , drop=FALSE]
            suppressWarnings(x.ppp <- ppp(cc[, 1], cc[, 2], window=ow))
            if (method == "pixellate") {
              ## this should never happen
                v <- pixellate(x.ppp, W=ow, weights=dt[zeros])$v
            }
            if (method == "density") {
                v <- density(x.ppp, ...)$v
            }
            res$z <- res$z + t(v)
            sz <- sz + sum(dt[zeros])
        }
        x.psp <- x.psp[lngths > 0]
        weights <- dt/ifelse(lngths > 0, lngths, .Machine$double.eps)
        weights <- weights[lngths > 0]
        if (method == "pixellate") {

            v <- pixellate.psp(x.psp, W=ow, weights=weights)$v
        }
        if (method == "density") {
            ## v <- density(x.psp, ...)$v
            for (li in 1:x.psp$n) {
                dens <- density(x.psp[li], ...)$v
                if (li == 1) {
                    v <- dens
                } else {
                    v <- v + dens * dt[li]
                }
            }
        }
        res$z <- res$z + t(v)
    }
    image2Grid(res, p4=proj4string(x))
}

##' @rdname trip-internal
.g2ow <- function(x) {
  mn <- x@cellcentre.offset - x@cellsize / 2
  mx <- mn + x@cells.dim * x@cellsize
  owin(c(mn[1], mx[1]), c(mn[2], mx[2]),
       mask=matrix(TRUE, x@cells.dim[2], x@cells.dim[1]),
       xy=list(x=seq(mn[1], mx[1], length=x@cells.dim[1]),
               y=seq(mn[2], mx[2], length=x@cells.dim[2])))
}



#' Generate a grid of time spent using approximate methods
#'
#'
#' Create a grid of time spent from an object of class \code{trip} by
#' approximating the time between locations for separate trip events.
#'
#'
#' This set of functions was the the original tripGrid from prior to version
#' 1.1-6. \code{tripGrid} should be used for more exact and fast calculations
#' assuming linear motion between fixes.
#'
#' The intention is for \code{tripGrid.interp} to be used for exploring
#' approximate methods of line-to-cell gridding.
#'
#' Trip locations are first interpolated, based on an equal-time spacing
#' between records. These interpolated points are then "binned" to a grid of
#' cells.  The time spacing is specified by the \code{dur} (duration) argument to
#' \code{interpequal} in seconds (i.e. \code{dur=3600} is used for 1 hour).
#' Shorter time periods will require longer computation with a closer
#' approximation to the total time spent in the gridded result.
#'
#' Currently there are methods "count" and "kde" for quantifying time spent,
#' corresponding to the functions "countPoints" and "kdePoints". "kde" uses
#' kernel density to smooth the locations, "count" simply counts the points
#' falling in a grid cell.
#'
#' @aliases tripGrid.interp interpequal countPoints kdePoints
#' @param x object of class trip
#' @param grid GridTopology - will be generated automatically if NULL
#' @param method name of method for quantifying time spent, see Details
#' @param dur The \"dur\"ation of time used to interpolate between available
#' locations (see Details)
#' @param \dots other arguments passed to \code{interpequal} or \code{kdePoints}
#' @return
#'
#' \code{tripGrid} returns an object of class \code{SpatialGridDataFrame}, with
#' one column "z" containing the time spent in each cell in seconds. If
#' kdePoints is used the units are not related to the time values and must be
#' scaled for further use.
#' @keywords manip
#' @export tripGrid.interp
tripGrid.interp <- function(x, grid=NULL, method="count", dur=NULL, ...) {
  method <- paste(method, "Points", sep="")
  if (!exists(method)) stop("no such method: ", method)
  cat("Using method ", method, "\n\n")
  if (is.null(grid)) grid <- makeGridTopology(x)
  res <- SpatialGridDataFrame(grid,
                              data.frame(z=rep(0, prod(grid@cells.dim))),
                              CRS(proj4string(x), doCheckCRSArgs = FALSE))
  tor <- getTORnames(x)
  trip.list <- split(x[, tor], x[[tor[2]]])
  cnt <- 0
  for (this in trip.list) {
    this <- interpequal(this, dur=dur, quiet=TRUE)
    cnt <- cnt + nrow(this)
    res$z <- res$z + do.call(method,
                             list(x=trip(this, tor), grid=grid, ...))$z
  }
  if (method == "countPoints") res$z <- res$z * dur
  res
}

#' @param h kernel bandwidth
#' @param resetTime rescale result back to the total duration of the input
#' @rdname tripGrid.interp
#' @seealso \code{\link[MASS]{bandwidth.nrd}} for the calculation of bandwidth values used internally when not supplied by the user
##' @importFrom MASS bandwidth.nrd
kdePoints <- function (x, h=NULL, grid=NULL, resetTime=TRUE, ...) {
  coords <- coordinates(x)
  xx <- coords[ , 1]
  yy <- coords[ , 2]
  tids <- getTimeID(x)
  time <- tids[, 1]
  id <- tids[, 2]
  timesum <- sum(tapply(time, id, function(x) {
    diff(range(unclass(x)))
  }))
  ## must acknowledge MASS for this
  if (missing(h)) {
    h <- c(bandwidth.nrd(xx), bandwidth.nrd(yy))/10
  }
  if (is.null(grid))  grid <- makeGridTopology(coords, ...)
  ## use bbox here
  dimXY <- grid@cells.dim
  nx <- nrow(x)
  gcs <- coordinatevalues(grid)
  gx <- gcs$s1 + grid@cellsize[1]
  gy <- gcs$s2 + grid@cellsize[2]
  ax <- outer(gx, xx, "-")/h[1]
  ay <- outer(gy, yy, "-")/h[2]
  z <- (matrix(dnorm(ax), dimXY[1], nx) %*%
          t(matrix(dnorm(ay), dimXY[2], nx))) / (nx * h[1] * h[2])
  if (resetTime) z <- (z * timesum/sum(z)) / 3600
  SpatialGridDataFrame(grid, data.frame(z=as.vector(z)), CRS(proj4string(x), doCheckCRSArgs = FALSE))
}

#' @rdname tripGrid.interp
countPoints <- function (x, dur=1, grid=NULL)
{
  coords <- coordinates(x)
  xx <- coords[, 1]
  yy <- coords[, 2]
  if (is.null(grid))  grid <- makeGridTopology(coords)
  orig <- grid@cellcentre.offset - grid@cellsize / 2
  ## scl <- c(diff(grd$x)[1], diff(grd$y)[1])
  scl <- grid@cellsize
  xdim <- grid@cells.dim[1]
  ydim <- grid@cells.dim[2]
  xmin <- orig[1]
  xmax <- orig[1] + (xdim + 1) * scl[1]
  ymin <- orig[2]
  ymax <- orig[2] + (ydim + 1) * scl[2]
  xlim <- c(xmin, xmax)
  ylim <- c(ymin, ymax)
  if (xlim[1] < xmin || xlim[2] > (xmax) ||
        ylim[1] < ymin || ylim[2] > (ymax)) {
    stop("Data are out of bounds")
  }
  cps <- ceiling(cbind((xx - orig[1]) / scl[1], (yy - orig[2]) / scl[2]))
  tps <- tabulate((cps[, 1] - 1) * ydim + cps[, 2], xdim * ydim)
  mps <- matrix(tps, ydim, xdim)
  z <- t(mps)
  SpatialGridDataFrame(grid, data.frame(z=as.vector(z[, ncol(z):1])),
                       CRS(proj4string(x), doCheckCRSArgs = FALSE))
}




#' Generate a GridTopology from a Spatial object
#'
#'
#' Sensible defaults are assumed, to match the extents of data to a manageable
#' grid.
#'
#' Approximations for kilometres in longlat can be made using \code{cellsize}
#' and \code{adjust2longlat}.
#'
#'
#' @param obj any Spatial object, or other object for which \code{bbox} will
#' work
#' @param cells.dim the number of cells of the grid, x then y
#' @param xlim x limits of the grid
#' @param ylim y limits of the grid
#' @param buffer proportional size of the buffer to add to the grid limits
#' @param cellsize pixel cell size
#' @param adjust2longlat assume cell size is in kilometres and provide simple
#' adjustment for earth-radius cells at the north-south centre of the grid
#' @keywords manip
#' @return S4 class GridTopology with properties set variously from input parameters
#' @export makeGridTopology
makeGridTopology <- function (obj, cells.dim=c(100, 100),
                              xlim=NULL, ylim=NULL, buffer=0, cellsize=NULL,
                              adjust2longlat=FALSE) {
  if ((is.null(xlim) | is.null(ylim)) & missing(obj))
    stop("require at least a Spatial object, matrix object, or xlim and ylim")
  if (!missing(obj)) bb <- bbox(obj)
  if (!is.null(xlim) & !is.null(ylim)) buffer <- 0
  if (is.null(xlim)) xlim <- bb[1,]
  if (is.null(ylim)) ylim <- bb[2,]
  ## PROBLEMS
  ## determination is boundary based, but grid is cell based
  ## break down into simpler functions, then recombine including longlat adjust
  ## gridFromNothing - world1 ?
  ## gridFromLimits
  ## gridFromLimits/dims
  ## gridFromLimits/cellsize
  ##
  ## gridFromDims?
  ## gridFromCellsize?
  ## gridFromDims/Cellsize?
  ## proj <- NA
  ## if (!missing(obj)) proj <- is.projected(obj)
  ## if (is.na(proj)) {
  ##   warning("coordinate system unknown, assuming longlat")
  ## 	proj <- FALSE
  ## }
  if (is.null(cellsize) & adjust2longlat)
    warning("cellsize not provided with adjust2longlat, ignoring")
  if (!is.null(cellsize)) {
    if (!length(cellsize) == 2)
      stop("cellsize must be of length 2")
    if (adjust2longlat) {
      cellsize <- c(cellsize[1] /
                      (cos((pi / 180) * mean(ylim)) * 1.852 * 60),
                    cellsize[2] / (1.852 * 60))
      if (any(!cellsize > 0)) {
        msg <- paste("longlat adjustment resulted in invalid",
                     "cellsize. Does it really make sense for",
                     "these latitude limits? \n")
        stop(msg, paste(format(ylim), collapse=","))
      }
    }
    xvalues <- seq(xlim[1], xlim[2] + cellsize[1], by=cellsize[1])
    yvalues <- seq(ylim[1], ylim[2] + cellsize[2], by=cellsize[2])
    xlim <- range(xvalues)
    ylim <- range(yvalues)
    cells.dim <- c(length(xvalues), length(yvalues))
  } else cellsize <- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
  if (buffer > 0) {
    addXY <- ceiling(cellsize * buffer)
    xlim <- xlim + c(-addXY[1], addXY[1])
    ylim <- ylim + c(-addXY[2], addXY[2])
    cellsize <- c(diff(xlim), diff(ylim)) / (cells.dim - 1)
  }
  new("GridTopology", cellcentre.offset=c(min(xlim), min(ylim)),
      cellsize=cellsize, cells.dim=as.integer(cells.dim))
}

