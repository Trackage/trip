context("trip")

## TODO: Rename context
## TODO: Add more tests
library(trip)
d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
coordinates(d) <- ~x+y
## a projection should always be set, is it WGS84 or NAD83 . . .
proj4string(d) <- CRS("+proj=laea +ellps=sphere")


test_that("trip works", {
  expect_that(tr <- trip(d, c("tms", "id")), is_a("trip"))
  expect_that(raster(tr), is_a("RasterLayer"))
})
