context("coerce-sf")

w6 <- walrus818[1:600, ]
test_that("sf works", {
  sfx <- as(w6, "sf") %>% expect_s3_class("sf")
  sfx$geometry %>% expect_s3_class("sfc_LINESTRING")
})

context("coerce-sp")

test_that("coercion to sp works", {
  expect_s4_class(explode(w6), "SpatialLinesDataFrame")
})

context("coerce-ltraj")

test_that("coercion to adehabitLT works", 
          expect_s3_class(as(w6, "ltraj"), "ltraj")          
)

test_that("coercion from adehabitatLT works", 
          expect_s4_class(as.trip(as(w6, "ltraj")), "trip"))

context("coerce-spatstat")

test_that("coercion to spatstat works", {
  expect_s3_class(as(w6, "ppp"), "ppp")          
  expect_s3_class(as(w6, "psp"), "psp") 
})

test_that("coercion from amt works", {
  amt <- as(walrus818, "track_xyt") %>% expect_s3_class("track_xyt")
  as.trip(amt) %>% expect_s4_class("trip")
  trip(amt) %>% expect_s4_class("trip")
})