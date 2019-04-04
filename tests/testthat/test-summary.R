context("test-summary")

test_that("summary of trip works", {
  snames <- c("spdf", "maxSpeed", "meanSpeed", "tripDistance", "tripDurationSeconds", 
              "tripDuration", "TORnames", "nRecords", "tripID", "tmaxs", "tmins", 
              "class")
  ss <- summary(walrus818) %>% expect_named( snames)
  expect_equal(lengths(ss[snames[-1]]), 
               c(maxSpeed = 14L, meanSpeed = 14L, tripDistance = 14L, tripDurationSeconds = 14L, 
                 tripDuration = 14L, TORnames = 2L, nRecords = 14L, tripID = 14L, 
                 tmaxs = 14L, tmins = 14L, class = 1L))
  
  expect_named(as.data.frame(ss), c("tripID", "No.Records", "startTime", "endTime", "tripDuration", 
                                    "tripDistance", "meanSpeed", "maxSpeed"))
  expect_output(print(ss))
  
  
  expect_output(show(ss))
 expect_output(show(walrus818))  
 
 expect_output(print(walrus818))
})
