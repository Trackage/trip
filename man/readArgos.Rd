%% $Id: readArgos.Rd 106 2013-03-27 23:30:03Z sluque $
\name{readArgos}
\alias{readArgos}
%- Also NEED an '\alias' for EACH other topic documented here.

\alias{readDiag}

\title{Read Argos "DAT" or "DIAG" files}


\description{
  
  Return a (Spatial) data frame of location records from raw Argos
  files.  Multiple files may be read, and each set of records is
  appended to the data frame in turn.  Basic validation of the data is
  enforced by default.

}


\usage{

readArgos(x, correct.all=TRUE, dtFormat="\%Y-\%m-\%d \%H:\%M:\%S",
          tz="GMT", duplicateTimes.eps=0.01,
          p4="+proj=longlat +ellps=WGS84", verbose=FALSE)

readDiag(x)

}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{x}{vector of file names of Argos "DAT" or "DIAG" files.}

  \item{correct.all}{logical - enforce validity of data as much as possible? (see Details)}

  \item{dtFormat}{the DateTime format used by the Argos data "date" and
    "time" pasted together}

  \item{tz}{timezone - GMT/UTC is assumed}
  
  \item{duplicateTimes.eps}{what is the tolerance for times being
    duplicate?}

  \item{p4}{PROJ.4 projection string, "+proj=longlat +ellps=WGS84" is
    assumed}

  \item{verbose}{if TRUE, details on date-time adjustment is reported}
  
}


\details{
  
  \code{readArgos} performs basic validation checks for class
  \code{trip} are made, and enforced based on \code{correct.all}:
  
  No duplicate records in the data, these are simply removed.  Records
  are ordered by DateTime ("date", "time", "gmt") within ID ("ptt").  No
  duplicate DateTime values within ID are allowed: to enforce this the
  time values are moved forward by one second - this is done recursively
  and is not robust.
  
  If validation fails the function will return a
  \code{\link[sp]{SpatialPointsDataFrame}}.  Files that are not
  obviously of the required format are skipped.
  
  Argos location quality data "class" are ordered, assuming that the
  available levels is \code{levels=c("Z", "B", "A", "0", "1", "2",
  "3")}.
  
  A projection string is added to the data, assuming the PROJ.4 longlat
  - if any longitudes are greater than 360 the PROJ.4 argument "+over"
  is added.

  \code{readDiag} simply builds a \code{data.frame}.
}


\value{
  
  \code{readArgos} returns a \code{trip} object, if all goes well, or
  simply a \code{\link[sp]{SpatialPointsDataFrame}}.

  \code{readDiag} returns a \code{data.frame} with 8 columns:

  \item{lon1, lat1 }{first pair of coordinates}

  \item{lon2, lat2}{second pair of coordinates}

  \item{gmt}{DateTimes as POSIXct}

  \item{id}{Platform Transmitting Terminal (PTT) ID}

  \item{lq}{Argos location quality class}

  \item{iq}{some other thing}

  
}


\references{

  The Argos data documentation is at
  \url{http://www.argos-system.org/manual/}.  Specific details on the
  PRV ("provide data") format were found here
  \url{http://www.cls.fr/manuel/html/chap4/chap4_4_8.htm}.
  
}


\author{Michael D. Sumner}


\section{Warning }{

  This works on some Argos files I have seen, it is not a guaranteed
  method and is in no way linked officially to Argos.

}


\seealso{

  \code{\link{trip}}, \code{\link[sp]{SpatialPointsDataFrame}},
  \code{\link{adjust.duplicateTimes}}, for manipulating these data, and
  \code{\link{argos.sigma}} for relating a numeric value to Argos
  quality "classes".

  \code{\link{sepIdGaps}} for splitting the IDs in these data on some
  minimum gap.

  \code{\link{order}}, \code{\link{duplicated}}, , \code{\link{ordered}}
  for general \R manipulation of this type.
  
}


\keyword{IO}% at least one, from doc/KEYWORDS
\keyword{manip}
