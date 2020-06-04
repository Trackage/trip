context("test-methods")
tr <- walrus818[1:1000, ]
test_that("split works", {
  expect_s4_class(split(tr, tr$Deployment)[[1]], "trip")
})

test_that("coordinates works", {
  expect_that(coordinates(tr), is_a("matrix")) %>% expect_length(2000L)
})

test_that("print and show", {
  expect_output(show(tr))
  expect_output(print(tr))
  expect_silent(summary(tr))
  
  expect_silent({plot(tr[seq(1, 1000, by = 5), ]); lines(tr); text(tr, lab = tr$Deployment)})
})
