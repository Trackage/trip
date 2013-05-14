%% $Id: trip-class.Rd 100 2013-03-27 14:29:56Z sluque $
\name{trip-class}
\docType{class}
\alias{trip-class}

\alias{dim.trip}
\alias{names.trip}
\alias{names<-.trip}
\alias{[,trip,ANY,ANY,ANY-method}
\alias{[[<-,trip,ANY,missing-method}
\alias{lines,trip-method}
\alias{plot,trip,missing-method}
\alias{points,trip-method}
\alias{recenter,trip-method}
\alias{show,trip-method}
\alias{show,summary.TORdata-method}
\alias{print.trip}
\alias{print.summary.TORdata}
\alias{summary,trip-method}
\alias{text,trip-method}
\alias{subset,trip-method}
\alias{as.data.frame.trip}


\title{Class \code{"trip"}}


\description{

  An extension of \code{\link[sp]{SpatialPointsDataFrame}} by including
  \code{"TimeOrderedRecords"}.  The records within the data frame are
  explicitly ordered by DateTime data within IDs.

}


%% Placing \arguments here to avoid ESS indentation bug (Rd mode is so
%% broken...)
\arguments{

  \item{x, object, obj}{A \code{trip} object.}

  \item{i, j}{For "[", rows and columns, respectively, of
    SpatialDataFrame to subset.  For "[[<-", integer corresponding to
    the Datetime or ID vector, or the string naming it.}

  \item{\ldots, drop, y, col}{Arguments passed to other methods.}

  \item{value}{Object to replace time or ID column with.}

}


\usage{

\S3method{print}{trip}(x, \ldots)
\S4method{summary}{trip}(object, \ldots)
\S3method{print}{summary.TORdata}(x, \ldots)

\S3method{as.data.frame}{trip}(x, \ldots)

\S3method{dim}{trip}(x)

\S3method{names}{trip}(x)
\S3method{names}{trip}(x) <- value

\S4method{[}{trip,ANY,ANY,ANY}(x, i, j, \ldots, drop=TRUE)
\S4method{[[}{trip,ANY,missing}(x, i) <- value

\S4method{plot}{trip,missing}(x, y, \ldots)
\S4method{lines}{trip}(x,
      col=hsv(seq(0, 0.9, length=length(summary(x)$tripID)),
                  0.8, 0.95), \ldots)
\S4method{points}{trip}(x, \ldots)
\S4method{text}{trip}(x, \ldots)

\S4method{recenter}{trip}(obj)

\S4method{subset}{trip}(x, \ldots)

}


\section{Objects from the Class}{
  
  Objects can be created by calls of the form
  \code{trip(obj="SpatialPointsDataFrame",
  TORnames="TimeOrderedRecords")}.  The object contains all the slots
  present within a \code{\link[sp]{SpatialPointsDataFrame}},
  particularly \code{data} which contains columns of at least those
  specified by \code{TOR.columns}.

}


\section{Slots}{

  \describe{
    
    \item{\code{TOR.columns}:}{Object of class \code{"character"}
      specifiying the DateTime and ID columns (in that order) in
      \code{data}.}
    
    \item{\code{data}:}{Object of class \code{"data.frame"} the native
      data object for a Spatial data frame.}
    
  }

  Also, other slots usual to a \code{\link[sp]{SpatialPointsDataFrame}}.

}

\section{Extends}{

  Class \code{"TimeOrderedRecords"}, directly.
  Class \code{"SpatialPointsDataFrame"}, directly.
  Class \code{"SpatialPoints"}, by class \code{"SpatialPointsDataFrame"}.
  Class \code{"Spatial"}, by class \code{"SpatialPointsDataFrame"}.
  
}


\section{General Methods}{

  Most of the methods available are by virtue of the sp package.  Some,
  such as \code{split.data.frame} have been added to SPDF so that trip
  has the same functionality.  See \code{\link{trip-methods}} for
  methods to create \code{trip} objects.
  
  \describe{
    
    \item{[}{\code{signature(x="trip", i="ANY", j="ANY", drop="ANY")}:
      subset rows or columns as per
      \code{\link[sp]{SpatialPointsDataFrame}}.  In the case that
      \code{TimeOrderedRecords} columns are dropped, the object reverts
      to the straight Spatial version.}

    \item{[[<-}{\code{signature(x="trip", i="ANY", j="missing")}:
      Replace the DateTime or ID vectors.}

    \item{dim}{\code{signature(x="trip")}: S3 method to retrieve the
      dimensions of the underlying
      \code{\link[sp]{SpatialPointsDataFrame}} object.}

    \item{names, names<-}{\code{signature(x="trip")}: S3 methods to
      retrieve or replace the names of the columns of the
      \code{\link[sp]{SpatialPointsDataFrame}}.}

    \item{lines}{\code{signature(x="trip")}: add lines to a plot with
      separate colours for each trip.}
    
    % \item{names}{\code{signature(x="trip")}: return the column names
    % of the data}
  
    \item{plot}{\code{signature(x="trip", y="missing")}: plot as
      \code{\link[sp]{SpatialPoints}}}
    
    \item{points}{\code{signature(x="trip")}: add points to a plot using
      the Spatial coordinates.}
    
    \item{recenter}{\code{signature(obj="trip")}: perform coordinate
      recentering, from the [-180,180] convention to [0, 360]}
    
    \item{print}{\code{signature(object="trip")}: print a short summary
      of the trip data.}

    \item{show}{\code{signature(object="trip")}: Like \code{print}, but
      only takes one argument.}

    % \item{spTransform}{\code{signature(x="trip", "CRS")}: transform
    % coordinates via rgal PROJ.4}

    \item{subset}{\code{signature(x="trip")}: subset a trip in the
      expected manner.}

    \item{summary}{\code{signature(object="trip")}: print a summary as
      per \code{\link[sp]{SpatialPointsDataFrame}} including a summary
      of the trip data}
    
    \item{text}{\code{signature(x="trip", y="missing")}: add text to a
      plot using Spatial coordinates.}

    %% \item{split}{\code{signature(x="trip")}: split a trip like a data frame}

  }
}


\author{Michael D. Sumner }


\seealso{
  
  \code{\link{trip}} for examples of directly using the class.

  \code{\link{trip-accessors}} describes methods for accessing
  information on \code{trip} objects.
  
}


\examples{

showClass("trip")

## Examples of general methods
## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
               ask=FALSE, echo=FALSE)
summary(tr)
plot(tr)
lines(tr)

dim(tr)
names(tr)
subset(tr, id == "2")
as.data.frame(tr)

tr[1:3, ]
tr[, 1]
tr[[1]]

if (exists("porpoise")) {
    dim(porpoise)
    names(porpoise)
    porpoise[porpoise[["id"]] == "GUS", ]
}

}



\keyword{classes}
