%% $Id: tripGrid.Rd 96 2013-03-26 13:33:36Z sluque $
\name{tripGrid}
\alias{tripGrid}


\title{Generate a grid of time spent by line-to-cell gridding}


\description{
  
  Create a grid of time spent from an object of class \code{trip} by
  exact cell crossing methods, weighted by the time between locations
  for separate trip events.
  
}


\usage{
tripGrid(x, grid=NULL, method="pixellate", \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{object of class \code{trip}}

  \item{grid}{GridTopology - will be generated automatically if NULL}

  \item{method}{pixellate or density}

  \item{\ldots}{pass arguments to density.psp if that method is chosen
    (and temporary mechanism to direct users of legacy methods to
    \code{\link{tripGrid.interp}})}
  
}


\details{
      
  Zero-length lines cannot be summed directly, their time value is
  summed by assuming the line is a point. A warning is given. The
  density method returns proportionate values, not summed time
  durations.
 
  See \code{pixellate.psp} and \code{pixellate.ppp} for the details on
  the method used. See \code{density.psp} for method="density".
 
  Trip events are assumed to start and end as per the object passed
  in. To work with inferred "cutoff" positions see
  \code{split.trip.exact}.
      
}


\value{
  
  \code{tripGrid} returns an object of class
  \code{SpatialGridDataFrame}, with one column "z" containing the time
  spent in each cell in seconds.
 
}


\keyword{manip}% at least one, from doc/KEYWORDS
