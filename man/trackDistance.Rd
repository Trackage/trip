%% $Id: trackDistance.Rd 96 2013-03-26 13:33:36Z sluque $
\name{trackDistance}
\alias{trackDistance}
\alias{trackDistance.default}
\alias{trackDistance.trip}

%\alias{gcdist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Determine distances along a track}


\description{

  Calculate the distances  between subsequent 2-D coordinates using
  Euclidean or Great Circle distance (WGS84 ellipsoid) methods.

}


\usage{

trackDistance(x1, y1, x2, y2, longlat=TRUE, prev = FALSE) 

}



\arguments{
  
  \item{x1}{trip object, matrix of 2-columns, with x/y coordinates OR a vector of x
    start coordinates}

  \item{x2}{vector of x end coordinates, if x1 is not a matrix}

  \item{y1}{vector of y start coordinates, if x1 is not a matrix}

  \item{y2}{vector of y end coordinates, if x1 is not a matrix}

  \item{longlat}{if FALSE, Euclidean distance, if TRUE Great Circle
    distance}
  \item{prev}{if TRUE and x1 is a trip, the return value has a padded
  end value (\"prev\"ious), rather than start (\"next\")}


 % \item{lon1, lon2}{vectors of paired longitude coordinates}
 % \item{lat1, lat2}{vectors of paired latitude coordinates}

  %\item{push}{an offset for measuring distance between this and the next (push)
 %location}
}


\details{

  If \code{x1} is a trip object, arguments \code{x2}, \code{x3},
  \code{y2} are ignored and the return result has an extra element for
  the start point of each individual trip, with value 0.0.  

  The \code{prev} argument is ignore unless x1 is a trip.

  Distance values are in the units of the input coordinate system when
  longlat is FALSE, and in kilometres when longlat is TRUE.
  
  This originally used \code{\link[sp]{spDistsN1}} but now implements
  the sp \code{gcdist} source directly in R.

}


\references{Original source taken from sp package.}


\value{Vector of distances between coordinates.}


\keyword{manip}% at least one, from doc/KEYWORDS
