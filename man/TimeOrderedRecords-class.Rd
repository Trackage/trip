%% $Id: TimeOrderedRecords-class.Rd 91 2013-03-25 21:14:53Z sluque $
\name{TimeOrderedRecords-class}
\docType{class}
\alias{TimeOrderedRecords-class}

\alias{TimeOrderedRecords}


\title{Class \code{"TimeOrderedRecords"}}


\description{

  A simple class to act as a place-holder for DateTime and ID records in
  spatial data.

}


\section{Objects from the Class}{

  Objects can be created by calls of the form
  \code{new("TimeOrderedRecords", TOR.columns)}, or
  \code{TimeOrderedRecords(x)}.  \code{x} and \code{TOR.columns} are a
  2-element character vector specifying the DateTime and ID columns in
  an object of class \code{trip}.
  
}


\section{Slots}{

  \describe{

    \item{\code{TOR.columns}:}{2-element vector of class
      \code{"character"}}
    
  }
}


\usage{

TimeOrderedRecords(x)

}


\arguments{
  
  \item{x}{Character vector of 2-elements, specifying the data columns
    of DateTimes and IDs.}
  
}


\details{
  
  The main use of this class and creator function is for
  \code{\link[sp]{SpatialPointsDataFrame}}s which are used with
  TimeOrderedRecords for the class \code{trip}.
  
}


\value{

  \code{TimeOrderedRecords} returns an object with a 2-element character
  vector, specifying the columns names.

}


\author{Michael D. Sumner}


\note{

  Future versions may change significantly, this class is very basic
  and could probably be implemented in a better way. Specifying TOR
  columns by formula would be a useful addition.

}


\seealso{

  See Also \code{\link{trip}} for creating trip objects, and
  \code{\link{trip-class}} for the class.

}


\examples{

tor <- new("TimeOrderedRecords", TOR.columns=c("datetime", "ID"))
tor <- TimeOrderedRecords(c("datetime", "ID"))

}


\keyword{classes}
\keyword{methods}
