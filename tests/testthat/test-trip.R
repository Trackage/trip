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
  tr$d <- 1:nrow(tr)
  expect_silent(subset(tr, d < 4))
  expect_warning(subset(tr, d < 3))
  
  tr$id <- as.integer(tr$id)
  expect_silent(subset(tr, d < 4))
  expect_warning(subset(tr, d < 3))
  
  expect_silent(spTransform(walrus818[1:100, ], "+proj=laea +datum=WGS84"))
})


test_that("plot works", {
  expect_silent({plot(walrus818); lines(walrus818)})
})

test_that("compliance works", {
  expect_that(tr <- trip(d, c("tms", "id"), correct_all = TRUE), is_a("trip"))
  
  dd <- as.data.frame(walrus818)[sample(1:500), ]
  expect_warning(d1 <- force_internal(dd, c("DataDT", "Deployment")) %>% expect_s3_class("data.frame"), 
                 "ordering input records by trip ID, then time")
  coordinates(dd) <- c("X_AED170_70", "Y_AED170_70")
  expect_warning(d2 <- force_internal(dd, c("DataDT", "Deployment")) %>% expect_s4_class("SpatialPointsDataFrame"), 
                 "ordering input records by trip ID, then time")
  
  dd <- as.data.frame(walrus818)[1:162, ]
  expect_warning(aa <- force_internal(dd, c("DataDT", "Deployment")) %>% expect_s3_class('data.frame'), 
                 "removing trip IDs that have too few elements")
  
  
  dd <- rbind(dd, dd[1, ])
  expect_warning(aa <- force_internal(dd, c("DataDT", "Deployment")) %>% expect_s3_class('data.frame'), 
                 "removing records that are complete duplicates at rows")
  
  
  dd <- as.data.frame(walrus818)[1:100, ]
  
  dd$DataDT[10] <- dd$DataDT[9]
  
  expect_warning(aa <- force_internal(dd, c("DataDT", "Deployment")) %>% expect_s3_class('data.frame'), 
                 "updating .* duplicated time records by a small adjustment")
  
  d3 <- as.data.frame(walrus818)[sample(1:500), ]
  expect_warning(forceCompliance(d3, c("DataDT", "Deployment")))
  coordinates(d3) <- c("X_AED170_70", "Y_AED170_70")
  expect_warning(forceCompliance(d3, c("DataDT", "Deployment")))
  
  
  tms <- Sys.time() + c(1, 10, 10, 20, 30)
  expect_equal(adjust.duplicateTimes(tms, rep(1, length(tms))), tms[1] + c(0, 9, 10, 19, 29))
  
  expect_equal(argos.sigma(factor(c("Z", "B", "A", "0", "1", "2", "3"), levels=c("Z", "B", "A", "0", "1", "2", "3")), adjust  = 1), 
               c(Z = 100, B = 80, A = 50, "0"= 20, "1" = 10, "2" =  4, "3" =  2))
})



