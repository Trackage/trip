%% $Id: trip-accessors.Rd 96 2013-03-26 13:33:36Z sluque $
\name{trip-accessors}
\alias{trip-accessors}

\alias{getTORnames}
\alias{getTimeID}


\title{

  Functions to retrieve DateTime and ID data from within (Spatial) data
  frames.
  
}


\description{
  
  Functions for retrieving the names of the columns used for DateTime
  and ID, as well as the data.
  
}


\usage{

getTORnames(obj)
getTimeID(obj)

}


\arguments{
  
  \item{obj}{\code{trip} object.}
  
}


\value{

  \code{getTORnames} retrieves the column names from an object extending
  the class \code{TimeOrderedRecords}, and \code{getTimeID} returns the
  data as a data frame from an object extending the class
  \code{TimeOrderedRecords}.
  
}


\seealso{

  \code{\link{trip-class}}, for the use of this class with
  \code{\link[sp]{SpatialPointsDataFrame}}.

  \code{\link{trip}}

}


\examples{

tor <- TimeOrderedRecords(c("time", "id"))
getTORnames(tor)

}


\keyword{manip}% at least one, from doc/KEYWORDS
