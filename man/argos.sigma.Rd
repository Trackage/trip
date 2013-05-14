%% $Id: argos.sigma.Rd 88 2013-03-23 17:54:40Z sluque $
\name{argos.sigma}
\alias{argos.sigma}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Assign numeric values for Argos "class" }


\description{
  
  Assign numeric values for Argos "class" by matching the levels
  available to given numbers. An adjustment is made to allow sigma to be
  specified in kilometeres, and the values returned are the approximate
  values for longlat degrees.  It is assumed that the levels are part of
  an "ordered" factor from least precise to most precise.
  
}


\usage{
argos.sigma(x, sigma=c(100, 80, 50, 20, 10, 4, 2), adjust=111.12)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  
  \item{x}{factor of Argos location quality "classes" }
  
  \item{sigma}{numeric values (by default in kilometres) }
  
  \item{adjust}{a numeric adjustment to convert from kms to degrees }
  
}


\details{
  
  The available levels in Argos are \code{levels=c("Z", "B", "A", "0",
    "1", "2", "3")}.

  The actual sigma values given by default are (as far as can be
  determined) a reasonable stab at what Argos believes.

}


\value{
  
  Numeric values for given levels.
  
}


\examples{

cls <- ordered(sample(c("Z", "B", "A", "0", "1", "2", "3"), 30,
                      replace=TRUE),
               levels=c("Z", "B", "A", "0", "1", "2", "3"))
argos.sigma(cls)

}


\keyword{manip}% at least one, from doc/KEYWORDS
