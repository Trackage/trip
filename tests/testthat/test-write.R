context("test-write")

test_that("write KML works", {
  outfile <- tempfile(fileext = ".kml")
  expect_true(!file.exists(outfile))
  expect_that(write_track_kml(walrus818[1:100, ], kml_file = outfile), 
              is_a("character")) %>% expect_length(1L)
  expect_true(file.exists(outfile))
  unlink(outfile)
  expect_true(!file.exists(outfile))
  })
