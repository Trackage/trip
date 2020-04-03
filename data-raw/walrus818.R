## Walrus tracking data provided by Anthony Fischbach
##  http://project.nprb.org/view.jsp?id=07d2ebd6-93ac-462a-b907-ae4085c5bed5
walrus818 <- read.csv("data-raw/NPRB_818/NPRB_818_Behavior.csv", stringsAsFactors = FALSE)
walrus818$DataDT <- as.POSIXct(strptime(walrus818$DataDT, "%Y/%m/%d %H:%M:%S"), tz = "GMT")
walrus818 <- subset(walrus818, !Deployment == 360)
library(trip)
trip(walrus818) <- c("X_AED170_70", "Y_AED170_70", "DataDT", "Deployment")
raster::projection(walrus818) <- "+proj=aeqd +datum=WGS84 +lon_0=-170 +lat_0=70"

usethis::use_data(walrus818, compress = "xz")

library(rworldxtra)
data(countriesHigh)
plot(walrus818)
plot(sp::spTransform(countriesHigh, proj4string(walrus818)), add = TRUE, col = "grey")
lines(walrus818)
