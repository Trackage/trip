%% $Id: as.Other.Rd 106 2013-03-27 23:30:03Z sluque $
\name{as.SpatialLinesDataFrame.trip}
\alias{as.SpatialLinesDataFrame.trip}

\alias{coerce,trip,SpatialLinesDataFrame-method}
\alias{as.ltraj.trip}
\alias{coerce,ltraj,trip-method}
\alias{as.ppp.trip}
\alias{as.psp.trip}
\alias{coerce,trip,psp-method}
\alias{coerce,trip,ppp-method}


\title{Coercion from \code{trip} objects to other classes}


\description{Coercing \code{trip} objects to other classes.}


\section{Methods}{
  
  \describe{
    
    \item{coerce}{\code{signature(from="trip",
	to="SpatialLinesDataFrame")}}

    \item{coerce}{\code{signature(from="trip", to="ltraj")}}

    \item{coerce}{\code{signature(from="trip", to="ppp")}}
    
    \item{coerce}{\code{signature(from="trip", to="psp")}}
    
  }
}

 
\usage{
as.SpatialLinesDataFrame.trip(from)
%% as(from, "SpatialLinesDataFrame")

as.ltraj.trip(xy, typeII=TRUE, slsp="remove")

\S3method{as.ppp}{trip}(X, \ldots, fatal)
\S3method{as.psp}{trip}(x, \ldots, from, to)

}


\arguments{
  
  \item{from}{\code{trip} object for
    \code{as.SpatialLinesDataFrame.trip}, but see
    \code{\link[spatstat]{as.psp}} for that method.}

  \item{X, x, xy}{\code{trip} object.}

  \item{typeII}{see \code{\link[adehabitatLT]{as.ltraj}}.}
  
  \item{slsp}{details for the \code{\link[adehabitatLT]{ltraj}} turning
    angles.}

  \item{\ldots}{Ignored.}

  \item{fatal}{Logical value, see Details of
    \code{\link[spatstat]{as.ppp}}.}

  \item{to}{See \code{\link[spatstat]{as.psp}}.}
  
}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

as.SpatialLinesDataFrame.trip(tr)
as(tr, "SpatialLinesDataFrame")

if (require(adehabitatLT)) {
    as.ltraj.trip(tr)
}

as.ppp(tr)
as.psp(tr)

}


\keyword{spatial}
\keyword{manip}
