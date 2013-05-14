%% $Id: forceCompliance.Rd 96 2013-03-26 13:33:36Z sluque $
\name{forceCompliance}
\alias{forceCompliance}

\title{Function to ensure dates and times are in order with trip ID}


\description{
  
  A convenience function, that removes duplicate rows, sorts by the
  date-times within ID, and removes duplicates from a data frame or
  SpatialPointsDataFrame.

}


\usage{
forceCompliance(x, tor)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{\code{\link{data.frame}} or
    \code{\link[sp]{SpatialPointsDataFrame}}}

  \item{tor}{character vector of names of date-times and trip ID
    columns}

}


\value{\code{\link{data.frame}} or
  \code{\link[sp]{SpatialPointsDataFrame}}.}


\author{Michael D. Sumner}


\note{

 It's really important that data used are of a given quality, but this
 function makes the most common trip problems easy to apply.

}


\seealso{\code{\link{trip}}}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
