# $Id: readDiag.R 68 2013-03-20 03:11:06Z sluque $

##' @rdname readArgos
##' @export
readDiag <- function (x) {
    data <- NULL
    for (fl in x) {
        d <- readLines(fl)
        locs <- d[grep("LON1", d, ignore.case=TRUE)]
        tms <- d[grep("DATE", d, ignore.case=TRUE)]
        bad <- (grep("\\?", locs))
        if (length(bad) > 0) {
            if (length(locs[-bad]) == 0) {
                warning(paste("no valid locations in:", fl, "\n ...ignoring"))
                next
            }
            locs <- locs[-bad]
            tms <- tms[-(bad)]
        }
        dlines <- paste(locs, tms)
        dlines <- strsplit(dlines, "\\s+", perl=TRUE)
        reclen <- length(dlines[[1]])
        dfm <- matrix(unlist(dlines[sapply(dlines, length) ==
                                    reclen]), ncol=reclen, byrow=TRUE)
        lonlat <- dfm[, c(4, 7, 10, 13)]
        dic <- dfm[, c(14, 17, 18, 21, 24), drop=FALSE]
        id <- dic[, 1]
        gmt <- as.POSIXct(strptime(paste(dic[, 2], dic[, 3]),
                                   "%d.%m.%y %H:%M:%S"), tz="GMT")
        lq <- dic[, 4]
        iq <- dic[, 5]
        ll <- as.vector(lonlat)
        ll[grep("S", ll)] <- paste("-", ll[grep("S", ll)], sep="")
        ll <- gsub("S", "", ll)
        ll[grep("N", ll)] <- paste("", ll[grep("N", ll)], sep="")
        ll <- gsub("N", "", ll)
        ll[grep("E", ll)] <- paste("", ll[grep("E", ll)], sep="")
        ll <- gsub("E", "", ll)
        ll[grep("W", ll)] <- paste("-", ll[grep("W", ll)], sep="")
        ll <- gsub("W", "", ll)
        ll <- matrix(as.numeric(ll), ncol=4)
        lon <- ll[, 2]
        lon2 <- ll[, 4]
        lq <- factor(lq, ordered=TRUE,
                     levels=c("Z", "B", "A", "0", "1", "2", "3"))
        data <- rbind(data,
                      data.frame(lon1=lon, lat1=ll[, 1],
                                 lon2=lon2, lat2=ll[, 3],
                                 gmt=gmt, id=id, lq=lq, iq=iq))
    }
    data
}



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
