
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
  ## no longer needed #45
  #tr@proj4string@projargs <- NA_character_
  #expect_warning(speedfilter(tr, 20))
 #  expect_warning(expect_equal(expect_output(speedfilter(tr)), tr))
})



test_that("angle calculation works", {
   tra <- walrus818[1:12, ]
  expect_equal(sum(is.na(trackAngle(tra))), 2)



  tra1 <- tra[1:5, ]
  tra1@coords <- cbind(1:5, 0)

})
d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
sp::coordinates(d) <- ~x+y
## this avoids complaints later, but these are not real track data (!)
sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere")
(tr <- trip(d, c("tms", "id")))

test_that("equal-time interpolation works", {
  expect_output(tr2 <- interpequal(tr, dur = 0.1), "lost seconds")

  expect_true(nrow(tr2) > nrow(tr) * 8)
})


test_that("home distance works", {
  expect_length(homedist(walrus818), length(unique(walrus818$Deployment)))

  expect_length(homedist(reproj(walrus818, "+proj=longlat +datum=WGS84")),length(unique(walrus818$Deployment)))

  expect_length(homedist(walrus818, home = cbind(0, 0)),
                    length(unique(walrus818$Deployment)))


})


test_that("time spent calc works", {
  expect_s4_class(trip_raster(walrus818), "BasicRaster")
  expect_s4_class(trip_raster(walrus818, grid = raster(walrus818)), "BasicRaster")
  w10 <- walrus818[1:10, ]
  #expect_error(rasterize(w10, method = "pixellate", dur = 1000))
  expect_s4_class(rasterize(w10), "BasicRaster")


  expect_s4_class(tripGrid(w10), "SpatialGridDataFrame")
  expect_s4_class(tripGrid(w10, grid = as(raster(w10), "GridTopology")), "SpatialGridDataFrame")
  expect_error(tripGrid(w10, grid =  raster(w10), field = w10$Wet), "GridTopology")
  expect_s4_class(rasterize(w10, grid = raster(w10), field = "Wet"), "BasicRaster")
  expect_s4_class(rasterize(w10, grid = raster(w10), method = "density"), "BasicRaster")


  expect_output(expect_s4_class(tripGrid.interp(w10, grid = as(raster(w10), "GridTopology"), method = "count", dur = 1300), "SpatialGridDataFrame"))

  expect_s4_class(makeGridTopology(w10), "GridTopology")
  expect_s4_class(makeGridTopology(xlim = c(0, 10), ylim = c(10, 100)), "GridTopology")
  expect_s4_class(makeGridTopology(xlim = c(0, 10), ylim = c(10, 100), buffer = 50), "GridTopology")
  expect_s4_class(makeGridTopology(xlim = c(0, 10), ylim = c(10, 100), cellsize = c(20, 30), adjust2longlat = TRUE), "GridTopology")
  expect_s4_class(makeGridTopology(xlim = c(0, 10), ylim = c(10, 100), buffer = 19, cellsize = c(20, 30), adjust2longlat = TRUE), "GridTopology")

  context("check when locations don't change")
  ww <- walrus818[1:100, ]
  ww@coords[10, ] <- ww@coords[9, ]
  expect_s4_class(rasterize(ww), "BasicRaster")

})



test_that("exact cut works", {
  tr1 <- walrus818[1:20, ]

  cutter <- c(tr1$DataDT[1] - 1, tr1$DataDT[5] - 360, max(tr1$DataDT) + 1)

  expect_equal(length(cut(tr1, cutter)), 2L)

expect_error(  cut(tr1, c("arble", "2 min")), "if breaks is character, length\\(breaks\\) should be 1L")
}
)
