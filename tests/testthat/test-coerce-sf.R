context("coerce-sf")

test_that("sf works", {
  expect_that(st_as_sf(walrus818), is_a("sf"))
})
