.llproj <- function() {
  "+proj=longlat +datum=WGS84"
}

## replace for sp::is.projected to avoid sf/rgdal
sp_is_projected <- function(x) {
  ## wild I know, but if it's in 0,360 just go longlat
  ex <- c(as.numeric(x@bbox))[c(1, 3, 2, 4)]
  !all(abs(ex[1:2]) <= 360 & abs(ex[3:4]) <= 90)
}
