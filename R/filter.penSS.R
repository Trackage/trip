# $Id: filter.penSS.R 68 2013-03-20 03:11:06Z sluque $

filter.penSS <- function(tr, lambda, first=TRUE, last=TRUE,...) {

    penalized <- function(x) {
        ## Form smoothed track
        p <- p.obs
        p[sub, ] <- x
        ## Velocities between smoothed points
        ##v <- gc.dist(p[-n,],p[-1,])/dt
        v <- trackDistance(p[, 2:1]) / dt
        ## Distances from smoothed points to observations
        ##d <- gc.dist(p,p.obs)
        d <- trackDistance(p[, 2], p[, 1], p.obs[, 2], p.obs[, 1])
        ## This is the penalized sum of squares
        (sum(d ^ 2) + lambda * sum(v ^ 2)) / n ^ 2
    }

    if (length(summary(tr)$tripID) > 1) {
        msg <- paste("trip object contains multiple events,",
                     "only the first trip used")
        warning(msg)
        tr <- tr[tr[[getTORnames(tr)[2]]] == summary(tr)$tripID[1], ]
    }
    ## Number of points and subset
    n <- nrow(tr)
    sub <- (1 + first):(n - last)
    ## Observed points
    ##  p.obs <- as.matrix(tr[,c("Lat","Lon")])
    p.obs <- coordinates(tr)[, 2:1]
    ## Time intervals (in days) between obs
    ##dt <- diff(unclass(tr$Time)/(24*60*60))
    dt <- diff(unclass(tr[[getTORnames(tr)[1]]]) / (24 * 60 * 60))
    mn <- nlm(penalized, as.matrix(p.obs[sub, ]), ...)
    m <- n - (first + last)
    res <- coordinates(tr)
    ##  tr$Lat[sub] <- mn$estimate[1:m]
    ##  tr$Lon[sub] <- mn$estimate[m+1:m]
    res[sub, 2] <- mn$estimate[1:m]
    res[sub, 1] <- mn$estimate[m + 1:m]
    res <- SpatialPointsDataFrame(res, as.data.frame(tr),
                                  proj4string=CRS(proj4string(tr)))
    trip(res, getTORnames(tr))
}



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
