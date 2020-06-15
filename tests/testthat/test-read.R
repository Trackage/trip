context("test-read")

test_that("read works", {
  expect_output(readArgos(system.file("extdata/argos/98feb.dat", package = "trip")),
                "Data fully validated: returning object of class  trip")
  expect_output(d <- readArgos(system.file("extdata/argos/98feb.dat", package = "trip"), correct.all = FALSE))


  expect_output(d1 <- readArgos(system.file("extdata/argos/98feb.dat", package = "trip"),
                                              p4 = "+proj=longlat +datum=WGS84 +no_defs"),
                          )


  expect_s3_class(readDiag(system.file("extdata/argos/98feb_a.dia", package = "trip")), "data.frame") %>%
    expect_length(8L) %>% expect_named(c("lon1", "lat1", "lon2", "lat2", "gmt", "id", "lq", "iq"))


  expect_error(expect_warning(readDiag(system.file("DESCRIPTION", package = "trip"))), "no valid Diag records found")
})

test_that("readArgos has sensible bounds", {
  argosfile <-
     system.file("extdata/argos/98feb.dat", package = "trip", mustWork = TRUE)
  argos <- readArgos(argosfile)
  expect_true(all(bbox(argos)[1L, ] > 300))
  expect_true(all(bbox(argos)[2L, ] < 0))
  obj1 <- as(walrus818[1:10, ], "SpatialPointsDataFrame")
  obj2 <- force_internal(obj1, c("DataDT", "Deployment"))
  expect_equivalent(bbox(obj1), bbox(obj2))

})
