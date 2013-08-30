library(trip)
library(sp)
library(testthat)
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

## test cases

## [,trip-method
## i,j,...,drop=TRUE
##  "n" is the length(unique(ID))

##  all missing
## tr[]  identical object

##  single numeric i
## tr[1]
##   i must be within 1:n
##  multiple numeric i
## tr[1:2]
##   i must be within 1:n
##  logical i
## tr[c(TRUE, FALSE)]

## single or multiple character i
## tr["laa"]
##  i must all be in IDs

## i,j indexing works as before

## tests
test_that("indexing a trip does expected job", {
    expect_that(identical(tr, tr[]), is_true())
    expect_that(tr[1], is_a("trip"))
    expect_that(tr[1:2], is_a("trip"))

     expect_that(tr["lbb"], is_a("trip"))
    expect_that(tr[c("lbb", "laa")], is_a("trip"))

    expect_that(tr[1:10, 1], shows_message())
    expect_that(tr[1:10, 1], is_a("SpatialPointsDataFrame"))
})

test_that("bad indexes fail nicely", {
    expect_that(tr[c(1, 3)], throws_error())
    expect_that(tr[c(FALSE, FALSE)], gives_warning())
    expect_that(tr[NA], throws_error())
    expect_that(tr[c(NA, 1)], throws_error())

    expect_that(tr[""], throws_error())
    expect_that(tr["la"], throws_error())


    expect_that(tr[c("laa", "lbb", "1")], throws_error())

    expect_that(tr[1:2, 1], shows_message())

})

coerceto <- c("SpatialLinesDataFrame", "ltraj", "ppp", "psp")
test_that("coercions to other classes work", {
    for (i in seq_along(coerceto)) {
        expect_that(as(tr, coerceto[i]), is_a(coerceto[i]))

    }

    expect_that(explode(tr), is_a("SpatialLinesDataFrame"))

    expect_that(nrow(tr) - length(trip:::.getUID(tr)), equals(length(explode(tr)$id)))

})


