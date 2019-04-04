context("test-read")

test_that("read works", {
  expect_output(expect_warning(readArgos(system.file("extdata/JM-2009-11-04-000.PRV", package = "trip")), "trip"), 
                "Data fully validated: returning object of class  trip")
  
  expect_output(expect_warning(d <- readArgos(system.file("extdata/JM-2009-11-04-000.PRV", package = "trip"), correct.all = FALSE), 
                 "incomplete final line found on .*JM-2009-11-04-000.PRV"))
  
  
  expect_output(expect_warning(d1 <- readArgos(system.file("extdata/JM-2009-11-04-000.PRV", package = "trip"), 
                                              p4 = " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                               "incomplete final line found on .*JM-2009-11-04-000.PRV"))
  
  expect_equal(d1@proj4string@projargs, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
})
