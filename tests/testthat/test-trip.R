context("trip")

## TODO: Rename context
## TODO: Add more tests
library(trip)
d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
sp::coordinates(d) <- ~x+y
## a projection should always be set, is it WGS84 or NAD83 . . .
sp::proj4string(d) <- sp::CRS(NA_character_, doCheckCRSArgs = FALSE)
sp::proj4string(d) <- sp::CRS("+proj=laea +datum=WGS84", doCheckCRSArgs = FALSE)


test_that("trip works", {
  expect_that(tr <- trip(d, c("tms", "id")), is_a("trip"))

  filt <- speedfilter(tr, max.speed = 1) %>%
    expect_type("logical")
  expect_equal(sum(filt), 8)
  ## metres per hour
  xx <- walrus818[1:1000, ]
  expect_error(sp::recenter(xx), "cannot recenter projected coordinate reference system")

  xxx <- reproj(xx, "+proj=longlat +datum=WGS84")
  
  expect_that(dim(xx), equals(c(1000, 4)))
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
  
  expect_silent(reproj(walrus818[1:100, ], "+proj=laea +datum=WGS84"))
  
  
  expect_s4_class(trip(tr, c("tms", "id")), "trip")
  expect_s4_class(trip(as(tr, "SpatialPointsDataFrame"), TimeOrderedRecords(c("tms", "id"))), "trip")
  expect_s4_class(trip(as(tr, "SpatialPointsDataFrame"), c("tms", "id")), "trip")


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



test_that("raw data input works", {
  rawd <- as.data.frame(walrus818)
  expect_error(trip(rawd), "first two columns must be numeric, x,y or longitude,latitude")
  expect_that(trip(dplyr::select(rawd, X_AED170_70, Y_AED170_70, DataDT, Deployment, dplyr::everything()), correct_all = TRUE), is_a("trip"))

  expect_that(trip(dplyr::select(rawd, X_AED170_70, Y_AED170_70, DataDT, dplyr::everything()) %>% dplyr::group_by(Deployment), correct_all = TRUE), is_a("trip"))
})


sfx <- structure(list(tms = structure(c(1555279658.29249, 1555279659.29249,
                                        1555279660.29249, 1555279661.29249, 1555279662.29249, 1555279663.29249,
                                        1555279664.29249, 1555279665.29249, 1555279666.29249, 1555279667.29249
), class = c("POSIXct", "POSIXt")), id = structure(c(1L, 1L,
                                                     1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"),
geometry = structure(list(structure(c(1, 2.39803794844702
), class = c("XY", "POINT", "sfg")), structure(c(2, 2.13970155680187
), class = c("XY", "POINT", "sfg")), structure(c(3, -0.38861880116111
), class = c("XY", "POINT", "sfg")), structure(c(4, -0.63980443199236
), class = c("XY", "POINT", "sfg")), structure(c(5, -0.639444783007308
), class = c("XY", "POINT", "sfg")), structure(c(6, -0.395647695993382
), class = c("XY", "POINT", "sfg")), structure(c(7, -0.817986381992888
), class = c("XY", "POINT", "sfg")), structure(c(8, 0.140944877113193
), class = c("XY", "POINT", "sfg")), structure(c(9, -0.0919535429316362
), class = c("XY", "POINT", "sfg")), structure(c(10, 1.81208376125709
), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT",
                                                "sfc"), precision = 0, bbox = structure(c(xmin = 1, ymin = -0.817986381992888,
                                                                                          xmax = 10, ymax = 2.39803794844702), class = "bbox"), crs = structure(list(
                                                                                            epsg = NA_integer_, proj4string = "+proj=laea +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), class = "crs"), n_empty = 0L)), row.names = c(NA,
                                                                                                                                                                                                                                                                   -10L), class = c("sf", "data.frame"), sf_column = "geometry", agr = structure(c(tms = NA_integer_,
                                                                                                                                                                                                                                                                                                                                                   id = NA_integer_), class = "factor", .Label = c("constant", "aggregate",
                                                                                                                                                                                                                                                                                                                                                                                                   "identity")))


# test_that("sf input works", {
#   expect_that(trip(sfx, c("tms", "id")), is_a("trip"))
#   expect_that(trip(sfx), is_a("trip"))
#   expect_error(trip(sfx["id"]))
# })

#ms <- mousetrap::KH2017
#ms$data <- ms$data[1:3, ]
#ms$trajectories <- ms$trajectories[1:3, 1:5, ]
ms <- structure(list(data = structure(list(subject_nr = c(1L, 1L, 1L
), count_trial = 2:4, Condition = structure(c(2L, 2L, 1L), .Label = c("Atypical",
                                                                      "Typical"), class = "factor"), Exemplar = structure(c(13L, 9L,
                                                                                                                            17L), .Label = c("Aal", "Alligator", "Chamaeleon", "Falke", "Fledermaus",
                                                                                                                                             "Goldfisch", "Hai", "Hund", "Kaninchen", "Katze", "Klapperschlange",
                                                                                                                                             "Lachs", "Loewe", "Pferd", "Pinguin", "Schmetterling", "Seeloewe",
                                                                                                                                             "Spatz", "Wal"), class = "factor"), CategoryLeft = structure(c(2L,
                                                                                                                                                                                                            4L, 5L), .Label = c("Amphibie", "Fisch", "Insekt", "Reptil",
                                                                                                                                                                                                                                "Saeugetier", "Vogel"), class = "factor"), CategoryRight = structure(c(5L,
                                                                                                                                                                                                                                                                                                       5L, 2L), .Label = c("Amphibie", "Fisch", "Insekt", "Reptil",
                                                                                                                                                                                                                                                                                                                           "Saeugetier", "Vogel"), class = "factor"), CategoryCorrect = structure(c(4L,
                                                                                                                                                                                                                                                                                                                                                                                                    4L, 4L), .Label = c("Fisch", "Insekt", "Reptil", "Saeugetier",
                                                                                                                                                                                                                                                                                                                                                                                                                        "Vogel"), class = "factor"), response = structure(c(5L, 5L, 5L
                                                                                                                                                                                                                                                                                                                                                                                                                        ), .Label = c("Amphibie", "Fisch", "Insekt", "Reptil", "Saeugetier",
                                                                                                                                                                                                                                                                                                                                                                                                                                      "Vogel"), class = "factor"), response_time = c(1000L, 1387L,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     2211L), correct = c(1L, 1L, 1L), mt_id = c("id0001", "id0002",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                "id0003")), row.names = c("id0001", "id0002", "id0003"), class = "data.frame"),
trajectories = structure(c(0, 0, 0, 11, 11, 10, 21, 21, 20,
                           31, 31, 30, 41, 41, 40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(3L,
                                                                                              5L, 3L), .Dimnames = list(c("id0001", "id0002", "id0003"),
                                                                                                                        NULL, c("timestamps", "xpos", "ypos")))), class = "mousetrap")
test_that("mousetrap input works", {
  expect_that(expect_warning(trip(ms)), is_a("trip"))
})

