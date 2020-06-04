library(trip)

d <- data.frame(x=1:10, y=rnorm(10), tms=Sys.time() + 1:10, id=gl(2, 5))
sp::coordinates(d) <- ~x+y
## this avoids complaints later, but these are not real track data (!)
sp::proj4string(d) <- sp::CRS("+proj=laea +ellps=sphere", doCheckCRSArgs = FALSE)
tr <- trip(d, c("tms", "id"))[d$id == 1, ]

pairs0 <- function (x) {
  data_frame(s0 = head(x, -1), s1 = tail(x, -1))
}
library(spbabel)
library(dplyr)
triptopology <- function(x) {
  cn <- sp::coordnames(x)
  tor <- trip:::getTORnames(x)
  d <- as.data.frame(x)
  #d$angle_ <- trackAngle(x)
  #d$distance_ <- trackDistance(x)
  ## all vertices in source tab
  d$id_ <- seq(nrow(d))
  names(d)[match(c(cn, tor), names(d))] <- c("x_", "y_", "t_", "object_")
  
  sptab <- data_frame(vertex_ = as.vector(t(as.matrix(pairs0(d$id_)))), 
                      branch_ = rep(seq(nrow(d) - 1), each = 2), object_ = branch_)
  sptab$order_ <- seq(nrow(sptab))
  
  sp(sptab %>% inner_join(d %>% select(x_, y_, id_), c("vertex_" = "id_"))) 
  %>% plot()
  
spt <- data_frame()
}