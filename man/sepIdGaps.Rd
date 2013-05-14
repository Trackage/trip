%% $Id: sepIdGaps.Rd 96 2013-03-26 13:33:36Z sluque $
\name{sepIdGaps}
\alias{sepIdGaps}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Separate a set of IDs based on gaps}


\description{
  
  A new set of ID levels can be created by separating those given based
  on a minimum gap in another set of data. This is useful for separating
  instruments identified only by their ID into separate events in time.
 
}


\usage{
sepIdGaps(id, gapdata, minGap=3600 * 24 * 7)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{id}{existing ID levels}

  \item{gapdata}{data matching \code{id} with gaps to use as separators}

  \item{minGap}{the minimum "gap" to use in gapdata to create a new ID
    level}
  
}


\details{
  
  The assumption is that a week is a long time for a tag not to record
  anything.
  
}


\value{
  
  A new set of ID levels, named following the pattern that "ID" split
  into 3 would provided "ID", "ID\_2" and "ID\_3".

}


\author{Michael D. Sumner}


\section{Warning}{

  It is assumed that each vector provides is sorted by \code{gapdata}
  within \code{id}. No checking is done, and so it is suggested that
  this only be used on ID columns within existing, validated \code{trip}
  objects.

}


\seealso{\code{\link{trip}}}


\examples{

id <- gl(2, 8)
gd <- Sys.time() + 1:16
gd[c(4:6, 12:16)] <- gd[c(4:6, 12:16)] + 10000
sepIdGaps(id, gd, 1000)
  
}


\keyword{manip}% at least one, from doc/KEYWORDS
