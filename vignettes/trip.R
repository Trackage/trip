## -----------------------------------------------------------------------------
library(trip)
d <- data.frame(x=1:10,y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
tr <- trip(d)
summary(tr)

## -----------------------------------------------------------------------------
plot(tr)
lines(tr)

## -----------------------------------------------------------------------------
plot(tr,pch = ".", col = rainbow(nrow(tr)))
lines(tr, col = c("dodgerblue", "firebrick"))

## -----------------------------------------------------------------------------
tg <- rasterize(tr)
plot(tg, col = c("transparent", heat.colors(25)))

## ----read-argos---------------------------------------------------------------
argosfile <- 
 system.file("extdata/argos/98feb.dat", package = "trip", mustWork = TRUE)
argos <- readArgos(argosfile) 

summary(argos)

## ----plot-argos-anti-meridian-------------------------------------------------
plot(argos, pch = ".")
lines(argos)
maps::map("world2", add = TRUE)
axis(1)
sp::degAxis(2)

## ----destructive-filters------------------------------------------------------
argos$spd <- speedfilter(argos, max.speed = 4)  ## km/h
mean(argos$spd)  ## more than 5% are too fast

plot(argos)
lines(argos[argos$spd & argos$class > "A", ])


argos$sda <- sda(argos, smax = 12)  ## defaults based on argosfilter, Freitas et al. (2008) 
mean(argos$sda)
plot(argos)
lines(argos[argos$sda, ])


## -----------------------------------------------------------------------------
raster::projection(walrus818)
data("walrus818")

plot(walrus818, pch = ".")
lines(walrus818)

axis(1)
axis(2)

## -----------------------------------------------------------------------------
data("world_north", package= "trip")
p <- par(mar = rep(0.5, 4))
plot(raster::extent(walrus818) + 600000)
plot(walrus818, pch = ".", add = TRUE)
plot(world_north, add = TRUE, col = "grey")
lines(walrus818)
par(p)

## ----conversions-points-------------------------------------------------------
## as points
as(walrus818, "SpatialPointsDataFrame")
as(walrus818, "ppp")


## ----conversions-lines--------------------------------------------------------
## as lines
as(walrus818, "SpatialLinesDataFrame")
class(as(walrus818, "sf"))
class(as(walrus818, "sf")$geom)
as(walrus818, "ltraj")

## ----conversions-segments-----------------------------------------------------
## as segments
explode(walrus818)
as(walrus818, "psp")


