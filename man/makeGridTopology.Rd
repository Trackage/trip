%% $Id: makeGridTopology.Rd 88 2013-03-23 17:54:40Z sluque $
\name{makeGridTopology}
\alias{makeGridTopology}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Generate a GridTopology from a Spatial object}


\description{
  
  Sensible defaults are assumed, to match the extents of data to a
  manageable grid.

  Approximations for kilometres in longlat can be made using
  \code{cellsize} and \code{adjust2longlat}.
  
}


\usage{
makeGridTopology(obj, cells.dim=c(100, 100), xlim=NULL, ylim=NULL,
  buffer=0, cellsize=NULL, adjust2longlat=FALSE)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{obj}{any Spatial object, or other object for which \code{bbox}
    will work}

  \item{cells.dim}{the number of cells of the grid, x then y}

  \item{xlim}{x limits of the grid}

  \item{ylim}{y limits of the grid}

  \item{buffer}{proportional size of the buffer to add to the grid
    limits}

  \item{cellsize}{pixel cell size}

  \item{adjust2longlat}{assume cell size is in kilometres and provide
  simple adjustment for earth-radius cells at the north-south centre of
  the grid}

}


\keyword{manip}% at least one, from doc/KEYWORDS
