library(trip)

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

d <- data.frame(x = lon,  y = lat, gmt=tms, id="lbb", stringsAsFactors = FALSE)
d1 <- d
d1[,c("x", "y")] <- d1[,c("x", "y")]  + 5
d1$id <- "laa"
d <- rbind(d1, d)


tr <- new("trip",
          SpatialPointsDataFrame(as.matrix(d[,1:2]), d),
          TimeOrderedRecords(c("gmt", "id")))



## tests
