
\name{trackAngle}
\alias{trackAngle}
\alias{trackAngle.default}
\alias{trackAngle.trip}

%\alias{gcdist}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Determine distances or angles along a track}


\description{

  Calculate the angles  between subsequent 2-D coordinates using
  Great Circle distance (spherical) methods.

}


\usage{
trackAngle(x)
}

\arguments{
  
  \item{x}{trip object, or matrix of 2-columns, with x/y coordinates}

}


\details{
  This is an optimized hybrid of "raster::bearing" and \code{\link[maptools]{gzAzimuth}}. 

}


\value{Vector of angles (degrees) between coordinates.}


\keyword{manip}% at least one, from doc/KEYWORDS
