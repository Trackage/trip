%% $Id: as.trip.Rd 105 2013-03-27 22:58:20Z sluque $
\name{as.trip-methods}
\docType{methods}
\alias{as.trip-methods}
\alias{as.trip}

\alias{as.trip,ltraj-method}
\alias{ltraj2trip}
\alias{coerce,trip,ltraj-method}

\title{Coercion from other classes to \code{trip} objects}

\description{Coercing objects to \code{trip} class}

\section{Methods}{
  
  \describe{
    
    \item{coerce}{\code{signature(from="ltraj", to="trip")}}

    \item{as.trip}{\code{signature(x="ltraj")}}

  }
}

 
\usage{

ltraj2trip(ltr)

\S4method{as.trip}{ltraj}(x, \ldots)

}


\arguments{
  
  \item{x, ltr}{ltraj object}

  \item{\ldots}{Arguments passed to other methods. Ignored for
  \code{ltraj} method.}
  
}


\author{Michael D. Sumner}


\examples{

## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)

if (require(adehabitatLT)) {
    l <- as.ltraj.trip(tr)
    ltraj2trip(l)
    as.trip(l)
}

}


\keyword{methods}
\keyword{spatial}
\keyword{manip}
