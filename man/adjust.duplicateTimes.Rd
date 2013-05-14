%% $Id: adjust.duplicateTimes.Rd 88 2013-03-23 17:54:40Z sluque $
\name{adjust.duplicateTimes}
\alias{adjust.duplicateTimes}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Adjust duplicate DateTime values}

\description{

  Duplicated DateTime values within ID are adjusted forward
  (recursively) by one second until no duplicates are present. This is
  considered reasonable way of avoiding the nonsensical problem of
  duplicate times.
  
}


\usage{
adjust.duplicateTimes(time, id)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{time}{vector of DateTime values}
  
  \item{id}{vector of ID values, matching DateTimes that are assumed
    sorted within ID }
  
}


\details{
  
 This function is used to remove duplicate time records in animal track
 data, rather than removing the record completely.
 
}


\value{
  
  The adjusted DateTime vector is returned.
  
}


\references{http://staff.acecrc.org.au/~mdsumner/}


\author{ Michael D. Sumner }


\section{Warning}{

  I have no idea what goes on at CLS when they output data that are
  either not ordered by time or have duplicates. If this problem exists
  in your data it's probably worth finding out why.

}


\seealso{ \code{\link{readArgos}} }

\examples{

## DateTimes with a duplicate within ID
tms <- Sys.time() + c(1:6, 6, 7:10) *10 
id <- rep("a", length(tms))
range(diff(tms))
    
## duplicate record is now moved one second forward
tms.adj <- adjust.duplicateTimes(tms, id)
range(diff(tms.adj))

}

\keyword{manip}% at least one, from doc/KEYWORDS
