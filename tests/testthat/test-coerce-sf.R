context("coerce-sf")

test_that("sf works", {
  sfx <- as(walrus818, "sf") %>% expect_s3_class("sf")
  sfx$geometry %>% expect_s3_class("sfc_LINESTRING")
})
