%% $Id: trip-deprecated.Rd 122 2013-05-04 03:48:23Z mdsumner $
\name{trip-deprecated}
\alias{trip-deprecated}

\alias{trip.split.exact}
\alias{as.trip.SpatialLinesDataFrame}
\alias{tripTransform}

% -------------------------------------------------------------------------

\title{Deprecated functions in trip}


\description{
  These functions will be declared defunct in a future release.
}


\usage{
trip.split.exact(x, dates)
as.trip.SpatialLinesDataFrame(from)
tripTransform(x, crs, \ldots)
}


\arguments{

  \item{x}{see \code{\link{cut.trip}}}

  \item{dates}{see \code{\link{cut.trip}}}

  \item{from}{see \code{\link{as.SpatialLinesDataFrame.trip}}}

  
  \item{crs}{CRS object, or PROJ.4 string accepted by
    \code{\link[sp]{CRS}} }
  
  \item{\dots}{Further arguments to \code{\link[rgdal]{spTransform}} }

}


\seealso{

  \code{\link{cut.trip}}, \code{\link{as.SpatialLinesDataFrame.trip}}

}

% -------------------------------------------------------------------------

\keyword{manip}
