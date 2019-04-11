context("test-read")

test_that("read works", {
  expect_output(readArgos(system.file("extdata/argos/98feb.dat", package = "trip")), 
                "Data fully validated: returning object of class  trip")
  
  expect_output(d <- readArgos(system.file("extdata/argos/98feb.dat", package = "trip"), correct.all = FALSE))
  
  
  expect_output(d1 <- readArgos(system.file("extdata/argos/98feb.dat", package = "trip"), 
                                              p4 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"), 
                          )
  
  expect_equal(d1@proj4string@projargs, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 +over")
})
