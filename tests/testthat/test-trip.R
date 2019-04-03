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
  
  filt <- speedfilter(tr, max.speed = 1) %>% 
    expect_type("logical")
  expect_equal(sum(filt), 8)
  ## metres per hour
  xx <- walrus818[1:1000, ]
  filt2 <- sda(xx, smax = 16000)  %>% expect_type("logical")
  expect_true(sum(filt2) > 900)
  expect_that(raster::raster(tr), is_a("RasterLayer"))
  library(raster)
  expect_that(timed <- rasterize(tr), is_a("RasterLayer"))
  expect_true(raster::cellStats(timed, "sum") > 7)
})


test_that("plot works", {
  expect_silent({plot(walrus818); lines(walrus818)})
})

test_that("compliance works", {
  expect_that(tr <- trip(d, c("tms", "id"), correct_all = TRUE), is_a("trip"))
})
