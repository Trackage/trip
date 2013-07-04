%% $Id: trip-methods.Rd 109 2013-03-28 03:09:51Z sluque $
\name{trip-methods}
\docType{methods}
\alias{trip-methods}
\alias{trip}

\alias{trip,SpatialPointsDataFrame,ANY-method}
\alias{trip,ANY,TimeOrderedRecords-method}
\alias{trip,trip,ANY-method}
\alias{trip,trip,TimeOrderedRecords-method}


\title{

  Function to handle animal track data, organized as \code{"trip"}s

}


\description{
  
  Create an object of class \code{"trip"}, extending the basic
  functionality of \code{\link[sp]{SpatialPointsDataFrame}} by
  specifying the data columns that define the "TimeOrdered" quality of
  the records.
  
}


\usage{

\S4method{trip}{SpatialPointsDataFrame,ANY}(obj, TORnames)
\S4method{trip}{ANY,TimeOrderedRecords}(obj, TORnames)
\S4method{trip}{trip,ANY}(obj, TORnames)
\S4method{trip}{trip,TimeOrderedRecords}(obj, TORnames)

}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{obj}{A \code{\link[sp]{SpatialPointsDataFrame}}, or an object
    that can be coerced to one, containing at least two columns with the
    DateTime and ID data as per \code{TORnames}.  It can also be a
    \code{trip} object for redefining \code{TORnames}.}

  \item{TORnames}{Either a \code{TimeOrderedRecords} object, or a
    2-element character vector specifying the DateTime and ID column of
    \code{obj}}

}


\section{Methods}{

  Most of the methods available are by virtue of the sp package.  Some,
  such as \code{split.data.frame} have been added to SPDF so that trip
  has the same functionality.
  
  \describe{
    
    \item{trip}{\code{signature(obj="SpatialPointsDataFrame",
	TORnames="ANY")}}{The main construction.}

    \item{trip}{\code{signature(obj="ANY",
	TORnames="TimeOrderedRecords")}: create a \code{trip} object
      from a data frame.}
    
    \item{trip}{\code{signature(obj="trip", TORnames="ANY")}:
      (Re)-create a \code{trip} object using a character vector for
      \code{TORnames}.}
    
    \item{trip}{\code{signature(obj="trip",
	TORnames="TimeOrderedRecords")}: (re)-create a trip object using
      a \code{TimeOrderedRecords} object.}
      
  }
}


\value{
  
  A trip object, with the usual slots of a
  \code{\link[sp]{SpatialPointsDataFrame}} and the added
  \code{TimeOrderedRecords}. For the most part this can be treated as a
  \code{data.frame} with \code{Spatial} coordinates.
 
}


\section{Warning}{

  There are some kludges to allow \code{trip} to do things, such as
  replace \code{\link{POSIXt}} column data using \code{"$<-.trip"} and
  \code{"[[<-.trip"} which should not be necessary once sp implements
  the new \code{data.frame} class of R >=2.4.0.

}


\author{Michael D. Sumner}


\seealso{

  \code{\link{speedfilter}}, and \code{\link{tripGrid}} for
  simple(istic) speed filtering and spatial time spent gridding.
  
}


\examples{

d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
coordinates(d) <- ~x+y
## this avoids complaints later, but these are not real track data (!)
proj4string(d) <- CRS("+proj=laea")
(tr <- trip(d, c("tms", "id")))

%% I really don't want adehabitatMA to be loaded as a requirement here
\dontrun{
## a simple example with the common fixes required for basic track data

dat <- read.csv("trackfile.csv")
names(dat)  ## e.g. [1] "long" "lat" "seal" "date" "local" "lq"
library(sp)
coordinates(dat) <- c("long", "lat")

## date/times may be in a particular time zone, please check
dat$gmt <- as.POSIXct(strptime(paste(dat$date, dat$local),
                      "\%d-\%b-\%y \%H:\%M:\%S"), tz="GMT")

## if there are problems in the data, this will error
tr <- trip(dat, c("gmt", "seal"))

## the following code tries to fix common problems

## remove completely-duplicated rows
dat <- dat[!duplicated(dat), ]
## order the rows by seal, then by time
dat <- dat[order(dat$seal, dat$gmt), ]
## fudge duplicated times
dat$gmt <- adjust.duplicateTimes(dat$gmt, dat$seal)

## finally, convert to Spatial and create trip object
coordinates(dat) <- c("long", "lat")
tr <- trip(dat, c("gmt", "seal"))
}


\dontrun{
if (require(adehabitatLT)) {
    data(porpoise)
    porpoise <- as.trip(porpoise)
    proj4string(porpoise) <- CRS("+proj=utm +zone=21 +ellps=WGS84 +units=m +no_defs")
    summary(porpoise)

}


## extended example to check that our projection metadata is correct
library(maptools)
data(wrld_simpl)
library(rgeos)
library(raster)

## 3 degrees either side (for half a zone . . .)
ext <- as(extent(spTransform(porpoise, CRS(proj4string(wrld_simpl)))) + 3, "SpatialPolygons")
proj4string(ext) <- CRS(proj4string(wrld_simpl))
## crop to the buffered tracks, and project to its native CRS
w <- spTransform(gIntersection(wrld_simpl[grep("United States", wrld_simpl$NAME), ], ext), CRS(proj4string(porpoise)))

plot(w)
lines(porpoise)

}

}


\keyword{methods}
\keyword{manip}
