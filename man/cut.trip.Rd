%% $Id: cut.trip.Rd 120 2013-05-02 13:04:27Z mdsumner $
\name{cut.trip}
\alias{cut.trip}

\title{

  Split trip events into exact time-based boundaries.
  
}


\description{
  
  Split trip events within a single object into exact time boundaries,
  adding interpolated coordinats as required.

}


\usage{
\method{cut}{trip}(x, dates, \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{x}{A trip object.}

  \item{dates}{A vector of date-time boundaries. These must encompass
    all the time range of the entire trip object.  }
  \item{\ldots}{Unused arguments.}

}


\details{

  Motion between boundaries is assumed linear and extra coordinates are
  added at the cut points.

}


\value{
  
  A list of trip objects, named by the time boundary in which they lie.

}


\author{Michael D. Sumner}


\seealso{See also \code{\link{tripGrid}}.}


\examples{
\dontrun{
set.seed(66)
d <- data.frame(x=1:100, y=rnorm(100, 1, 10),
                tms=Sys.time() + c(seq(10, 1000, length=50),
                seq(100, 1500, length=50)), id=gl(2, 50))
coordinates(d) <- ~x+y
tr <- trip(d, c("tms", "id"))

bound.dates <- seq(min(tr$tms) - 1, max(tr$tms) + 1, length=5)
trip.list <- cut(tr, bound.dates)
bb <- bbox(tr)
cn <- c(20, 8)
g <- GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)

tg <- tripGrid(tr, grid=g)
tg <- as.image.SpatialGridDataFrame(tg)
tg$x <- tg$x - diff(tg$x[1:2]) / 2
tg$y <- tg$y - diff(tg$y[1:2]) / 2

op <- par(mfcol=c(4, 1))
for (i in 1:length(trip.list)) {
  plot(coordinates(tr), pch=16, cex=0.7)
  title(names(trip.list)[i], cex.main=0.9)
  lines(trip.list[[i]])
  abline(h=tg$y, v=tg$x, col="grey")
  image(tripGrid(trip.list[[i]], grid=g), interpolate=FALSE,
  col=c("white", grey(seq(0.2, 0.7,  length=256))),add=TRUE)
  abline(h=tg$y, v=tg$x,  col="grey")
  lines(trip.list[[i]])
  points(trip.list[[i]], pch=16, cex=0.7)
}

par(op)
print("you may need to resize the window to see the grid data")

cn <- c(200, 80)
g <- GridTopology(bb[, 1], apply(bb, 1, diff) / (cn - 1), cn)

tg <- tripGrid(tr, grid=g)
tg <- as.image.SpatialGridDataFrame(tg)
tg$x <- tg$x - diff(tg$x[1:2]) / 2
tg$y <- tg$y - diff(tg$y[1:2]) / 2

op <- par(mfcol=c(4, 1))
for (i in 1:length(trip.list)) {
  plot(coordinates(tr), pch=16, cex=0.7)
  title(names(trip.list)[i], cex.main=0.9)
  image(tripGrid(trip.list[[i]], grid=g, method="density", sigma=1),
        interpolate=FALSE,
        col=c("white", grey(seq(0.2, 0.7, length=256))),
        add=TRUE)
  lines(trip.list[[i]])
  points(trip.list[[i]], pch=16, cex=0.7)
}

par(op)
print("you may need to resize the window to see the grid data")

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{chron}
