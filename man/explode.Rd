\name{explode}
\alias{explode}
\title{explode}
\usage{
  explode(x, ...)
}
\arguments{
  \item{x}{trip object}

  \item{...}{reserved for future methods}
}
\value{
  SpatialLinesDataFrame
}
\description{
  Break a trip into its component line segments
}
\details{
  Function to create a SpatialLinesDataFrame from a trip
  object, resulting in a line segment for each implicit
  segment along the tracks. The object stores the start and
  end times, duration and the ID of the segment.
}
\examples{
## Continuing the example from '?trip-methods:
utils::example("trip-methods", package="trip",
           ask=FALSE, echo=FALSE)
spldf <- trip:::explode(tr)
summary(tr)
}

