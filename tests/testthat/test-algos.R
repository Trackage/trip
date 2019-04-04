context("test-algos")

test_that("separate IDs works", { 
  
  tms <- (Sys.time() + 1:100)[-(40:60)]
  expect_equal(sepIdGaps(rep_len(1, length(tms)), tms, minGap = 2), 
               rep(c("1", "1_2"), c(39, 40)))
  
  
  expect_equal(sepIdGaps(rep_len(2, length(tms)), tms, minGap = 2), 
               rep(c("2", "2_2"), c(39, 40)))
  
  
})
tr <- walrus818[1:600, ]
test_that("speedfilter and sdafilter works", {
  expect_s4_class(tr, "trip")
  expect_true(!is.na(tr@proj4string@projargs))
  expect_silent(filt1 <- speedfilter(tr, max.speed = 2000))
  filt2 <- speedfilter(tr, max.speed = 100)
  expect_true(sum(filt1) > sum(filt2))
  
  expect_silent(sda(tr, smax = 1000, pre = filt1))
  expect_error(speedfilter(data.frame(1)), "only trip objects supported")
  tr@proj4string@projargs <- NA_character_
  expect_warning(speedfilter(tr, 20))
  
  expect_warning(expect_equal(expect_output(speedfilter(tr)), tr))
})


test_that("non destructive filter works", {
  trll <- spTransform(walrus818[c(1, 2, 3, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 35), ], 
                      "+proj=longlat +datum=WGS84")
  expect_s4_class(aa <- filter.penSS(trll, 0.1), "trip")
  expect_true(sum(trackDistance(trll)) > sum(trackDistance(aa)))
})
test_that("angle calculation works", {
   tra <- walrus818[1:12, ]
  expect_equal(trackAngle(tra), 
               c(`3531` = NA, `3532` = 62.6340505667122, `3533` = 56.944063667873, 
                 `3534` = 179.999988340919, `3535` = 179.999988340449, `3536` = 179.999988341847, 
                 `3537` = 179.999988340532, `3538` = 179.677666430905, `3539` = 179.677689616978, 
                 `35310` = 179.677666700317, `35311` = 179.6776898878, `35312` = NA
               ))
  
  tra@proj4string@projargs <- NA_character_
  expect_warning(trackAngle(tra))
  
  tra1 <- tra[1:5, ]
  tra1@coords <- cbind(1:5, 0)
  ## we trigger the "all 180"
 expect_warning( expect_equal(unname(trackAngle(tra1)), c(NA, 180, 180, 180, NA)))
})
d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
coordinates(d) <- ~x+y
## this avoids complaints later, but these are not real track data (!)
proj4string(d) <- CRS("+proj=laea +ellps=sphere")
(tr <- trip(d, c("tms", "id")))

test_that("equal-time interpolation works", {
  expect_output(tr2 <- interpequal(tr, dur = 0.1), "lost seconds")
  
  expect_true(nrow(tr2) > nrow(tr) * 8)
})


test_that("home distance works", {
  expect_equal(homedist(walrus818), c(`353` = 194119.892690059, `354` = 515915.874666597, `355` = 513435.909676174, 
                                      `356` = 516913.614400898, `357` = 519705.713481197, `358` = 515747.520001793, 
                                      `359` = 514025.366592934, `361` = 512403.000210772, `362` = 199095.87065532, 
                                      `366` = 516366.163835316, `367` = 63337.130468628, `368` = 510725.681066656, 
                                      `443` = 518192.713176285, `444` = 454476.593414886))
  
  expect_equal(homedist(spTransform(walrus818, "+proj=longlat +datum=WGS84")),
               c(`353` = 194.088733710391, `354` = 515.838473516953, `355` = 513.370932023196, 
                 `356` = 516.830659523451, `357` = 519.620809802099, `358` = 515.664147669753, 
                 `359` = 513.94959955415, `361` = 512.327561703805, `362` = 199.062060291758, 
                 `366` = 516.290786361838, `367` = 63.3177196800778, `368` = 510.650324690835, 
                 `443` = 518.109431801804, `444` = 454.418599207949))
  
  expect_equal(homedist(walrus818, home = cbind(0, 0)), 
           c(`353` = 284117.827876042, `354` = 359920.112524988, `355` = 354514.47593152, 
             `356` = 412693.049935421, `357` = 374615.873889508, `358` = 369769.362496408, 
             `359` = 356057.134572529, `361` = 354525.035826809, `362` = 310849.500950219, 
             `366` = 356838.886705191, `367` = 313049.639389666, `368` = 353935.729935535, 
             `443` = 371292.155059867, `444` = 289451.351879724))
  
  
})


test_that("time spent calc works", {
  expect_s4_class(trip_raster(walrus818), "BasicRaster")
  expect_s4_class(trip_raster(walrus818, grid = raster(walrus818)), "BasicRaster")
  
  expect_warning(rasterize(walrus818[1:10, ], method = "kde"))
  expect_error(rasterize(walrus818[1:10, ], method = "pixellate", dur = 1000))
  expect_s4_class(rasterize(walrus818[1:100, ]), "BasicRaster")

  expect_s4_class(tripGrid(walrus818[1:100, ]), "SpatialGridDataFrame")  
  expect_s4_class(tripGrid(walrus818[1:100, ], grid = as(raster(walrus818), "GridTopology")), "SpatialGridDataFrame")
  
  context("check when locations don't change")
  ww <- walrus818[1:100, ]
  ww@coords[10, ] <- ww@coords[9, ]
  expect_s4_class(rasterize(ww), "BasicRaster")
  
})
