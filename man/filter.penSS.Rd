%% $Id: filter.penSS.Rd 96 2013-03-26 13:33:36Z sluque $
\name{filter.penSS}
\alias{filter.penSS}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Non-destructive smoothing filter}


\description{
  
  Non-destructuve filter for track data using penalty smoothing on
  velocity.
  
}


\usage{

filter.penSS(tr, lambda, first=TRUE, last=TRUE, \ldots)

}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{tr}{A \code{trip} object.}

  \item{lambda}{Smoothing parameter, see Details.}

  \item{first}{Fix the first location and prevent it from being updated
    by the filter.}
  
  \item{last}{Fix the last location and prevent it from being updated by
    the filter.}
  
  \item{\ldots}{%% ~~Describe \code{\dots} here~~
  }

}


\details{

  Destructive filters such as \code{\link{speedfilter}} can be recast
  using a penalty smoothing approach in the style of Green and Silverman
  (1994).

  This filter works by penalizing the fit of the smoothed track to the
  observed locations by the sum of squared velocities.  That is, we
  trade off goodness of fit against increasing the total sum of squared
  velocities.

  When lambda=0 the smoothed track reproduces the raw track
  exactly. Increasing lambda favours tracks requiring less extreme
  velocities, at the expense of reproducing the original locations.

}


\value{

  A trip object with updated coordinate values based on the filter - all
  the data, including original coordinates which are maintained in the
  trip data frame.

}


\references{

  Green, P. J. and Silverman, B. W. (1994). Nonparametric regression and
  generalized linear models: a roughness penalty approach. CRC Press.

}


\author{Simon Wotherspoon and Michael Sumner}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\seealso{\code{\link{speedfilter}}}


\examples{

\dontrun{## Example takes a few minutes

## Fake some data

## Brownian motion tethered at each end
brownian.bridge <- function(n, r) {
  x <- cumsum(rnorm(n, 0, 1))
  x <- x - (x[1] + seq(0, 1, length=n) * (x[n] - x[1]))
  r * x
}

## Number of days and number of obs
days <- 50
n <- 200

## Make separation between obs gamma distributed
x <- rgamma(n, 3)
x <- cumsum(x)
x <- x/x[n]

## Track is lissajous + brownian bridge
b.scale <- 0.6
r.scale <- sample(c(0.1, 2, 10.2), n, replace=TRUE,
                  prob=c(0.8, 0.18, 0.02))
set.seed(44)

tms <- ISOdate(2001, 1, 1) + trunc(days * 24 * 60 * 60 *x)
lon <- 120 + 20 * sin(2 * pi * x) +
    brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)
lat <- -40 + 10 *(sin(3 * 2 * pi * x) + cos(2 * pi * x) - 1) +
    brownian.bridge(n, b.scale) + rnorm(n, 0, r.scale)

tr <- new("trip",
          SpatialPointsDataFrame(cbind(lon, lat),
                                 data.frame(gmt=tms, id="lbb")),
                                 TimeOrderedRecords(c("gmt", "id")))
plot(tr)

## the filtered version
trf <- filter.penSS(tr, lambda=1, iterlim=400, print.level=1)

lines(trf)

}

}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
\keyword{misc}% __ONLY ONE__ keyword per line
