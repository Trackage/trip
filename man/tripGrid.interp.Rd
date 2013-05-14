%% $Id: tripGrid.interp.Rd 96 2013-03-26 13:33:36Z sluque $
\name{tripGrid.interp}
\alias{tripGrid.interp}

\alias{interpequal}
\alias{countPoints}
\alias{kdePoints}
%- Also NEED an '\alias' for EACH other topic documented here.


\title{Generate a grid of time spent using approximate methods}


\description{
  
  Create a grid of time spent from an object of class \code{trip} by
  approximating the time between locations for separate trip events.
  
}


\usage{

tripGrid.interp(x, grid=NULL, method="count", dur=NULL,  \ldots)
interpequal(x, dur=NULL, quiet=FALSE)
countPoints(x, dur=1, grid=NULL) 
kdePoints(x, h=NULL, grid=NULL, resetTime=TRUE, \ldots)

}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{object of class trip}

  \item{grid}{GridTopology - will be generated automatically if NULL}

  \item{method}{name of method for quantifying time spent, see Details }

  \item{\dots}{other arguments passed to \code{interpequal} or
    \code{kdePoints}}
  
  %% interpequal

  \item{dur}{The \"dur\"ation of time used to interpolate between
    available locations (see Details)}

  \item{quiet}{logical - report on difference between time summed and
    time in trip?}
  
  %% count/kdePoints

  \item{h}{numeric vector of two elements specifying bandwidth for
    kernel density}

  \item{resetTime}{logical - reset the values of the kde grid to match
    the sum of the total time?}

}


\details{

  This set of functions was the the original tripGrid from prior to
  version 1.1-6. \code{tripGrid} should be used for more exact and fast
  calculations assuming linear motion between fixes.
  
  The intention is for \code{tripGrid.interp} to be used for exploring
  approximate methods of line-to-cell gridding.

  Trip locations are first interpolated, based on an equal-time spacing
  between records. These interpolated points are then "binned" to a grid
  of cells.  The time spacing is specified by the "dur"ation argument to
  \code{interpequal} in seconds (i.e. \code{dur=3600} is used for 1
  hour). Shorter time periods will require longer computation with a
  closer approximation to the total time spent in the gridded result.

  Currently there are methods "count" and "kde" for quantifying time
  spent, corresponding to the functions "countPoints" and
  "kdePoints". "kde" uses kernel density to smooth the locations,
  "count" simply counts the points falling in a grid cell.

}


\value{
  
  \code{tripGrid} returns an object of class
  \code{SpatialGridDataFrame}, with one column "z" containing the time
  spent in each cell in seconds. If kdePoints is used the units are not
  related to the time values and must be scaled for further use.
 
}


\keyword{manip}% at least one, from doc/KEYWORDS
