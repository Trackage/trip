%% $Id: oc.theme.Rd 96 2013-03-26 13:33:36Z sluque $
\name{oc.theme}
\alias{oc.theme}

\alias{oc.colors}


\title{SeaWiFS ocean colour colours}


\description{
  
  Generate ocean colour colours, using the SeaWiFS scheme
  
}


\usage{
oc.theme(x=50)
oc.colors(n)
}


\arguments{

  \item{x}{Number of colours to generate as part of a theme}

  \item{n}{Number of colours to generate  }

}


\details{
  
  This is a high-contrast palette, log-scaled originally for ocean
  chlorophyll.

}


\value{A set of colours or a theme object.}


\author{Michael D. Sumner}


\seealso{

  Similar functions in sp \code{\link[sp]{sp.theme}},
  \code{\link[sp]{bpy.colors}}

}


\examples{

oc.colors(10)
library(lattice)
trellis.par.set(oc.theme)

}


\keyword{color}
