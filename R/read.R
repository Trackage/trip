
#' Read Argos "DAT" or "DIAG" files
#'
#'
#' Return a (Spatial) data frame of location records from raw Argos files.
#' Multiple files may be read, and each set of records is appended to the data
#' frame in turn.  Basic validation of the data is enforced by default.
#'
#'
#' \code{readArgos} performs basic validation checks for class \code{trip} are
#' made, and enforced based on \code{correct.all}:
#'
#' No duplicate records in the data, these are simply removed.  Records are
#' ordered by DateTime ("date", "time", "gmt") within ID ("ptt").  No duplicate
#' DateTime values within ID are allowed: to enforce this the time values are
#' moved forward by one second - this is done recursively and is not robust.
#'
#' If validation fails the function will return a
#' \code{\link[sp]{SpatialPointsDataFrame}}.  Files that are not obviously of
#' the required format are skipped.
#'
#' Argos location quality data "class" are ordered, assuming that the available
#' levels is \code{levels=c("Z", "B", "A", "0", "1", "2", "3")}.
#'
#' A projection string is added to the data, assuming the PROJ.4 longlat - if
#' any longitudes are greater than 360 the PROJ.4 argument "+over" is added.
#'
#' \code{readDiag} simply builds a \code{data.frame}.
#'
#' @aliases readArgos readDiag
#' @param x vector of file names of Argos "DAT" or "DIAG" files.
#' @param correct.all logical - enforce validity of data as much as possible?
#' (see Details)
#' @param dtFormat the DateTime format used by the Argos data "date" and "time"
#' pasted together
#' @param tz timezone - GMT/UTC is assumed
#' @param duplicateTimes.eps what is the tolerance for times being duplicate?
#' @param p4 PROJ.4 projection string, "+proj=longlat +ellps=WGS84" is assumed
#' @param verbose if TRUE, details on date-time adjustment is reported
#' @return
#'
#' \code{readArgos} returns a \code{trip} object, if all goes well, or simply a
#' \code{\link[sp]{SpatialPointsDataFrame}}.
#'
#' \code{readDiag} returns a \code{data.frame} with 8 columns:
#' \itemize{
#' \item {\code{lon1},\code{lat1} first pair of coordinates}
#' \item {\code{lon1},\code{lat1} second pair of coordinates}
#' \item {gmt DateTimes as POSIXct}
#' \item {id Platform Transmitting Terminal (PTT) ID}
#' \item {lq Argos location quality class}
#' \item {iq some other thing}
#' }
#' @section Warning :
#'
#' This works on some Argos files I have seen, it is not a guaranteed method
#' and is in no way linked officially to Argos.
#' @seealso
#'
#' \code{\link{trip}}, \code{\link[sp]{SpatialPointsDataFrame}},
#' \code{\link{adjust.duplicateTimes}}, for manipulating these data, and
#' \code{\link{argos.sigma}} for relating a numeric value to Argos quality
#' "classes".
#'
#' \code{\link{sepIdGaps}} for splitting the IDs in these data on some minimum
#' gap.
#'
#' \code{\link{order}}, \code{\link{duplicated}}, , \code{\link{ordered}} for
#' general manipulation of this type.
#' @references
#'
#' The Argos data documentation was (ca. 2003) at
#' http://www.argos-system.org/manual.  Specific details on the PRV
#' ("provide data") format were found in Chapter 4_4_8, originally at 
#' 'http://www.cls.fr/manuel/html/chap4/chap4_4_8.htm'.
#' @keywords IO manip
#' @export readArgos
readArgos <- function (x, correct.all=TRUE, dtFormat="%Y-%m-%d %H:%M:%S",
                       tz="GMT", duplicateTimes.eps=1e-2,
                       p4="+proj=longlat +ellps=WGS84", verbose=FALSE) {
  ## add "correct.all" argument - just return data frame if it fails, with
  ## suggestions of how to sort/fix it
  
  
  ## this should be heaps faster
  dout <- vector("list", length(x))
  for (icon in seq_along(x)) {
    old.opt <- options(warn=-1)
    dlines <- strsplit(readLines(x[icon]), "\\s+", perl=TRUE)
    options(old.opt)
    loclines <- sapply(dlines, length) == 12
    if (any(loclines)) {
      dfm <- matrix(unlist(dlines[sapply(dlines, length) == 12]),
                    ncol=12, byrow=TRUE)
      if (dfm[1,7] == "LC") {
        msg <- paste(" appears to be a diag file, skipping.",
                     "Use readDiag to obtain a dataframe. \n\n")
        cat("file ", icon, msg)
        next
      }
      df <- vector("list", 12)
      names(df) <- c("prognum", "ptt", "nlines", "nsensor",
                     "satname", "class", "date", "time", "latitude",
                     "longitude", "altitude", "transfreq")
      for (i in c(1:4, 9:12)) df[[i]] <- as.numeric(dfm[, i])
      for (i in 5:6) df[[i]] <- factor(dfm[, i])
      for (i in 7:8) df[[i]] <- dfm[, i]
      df <- as.data.frame(df)
      df$gmt <- as.POSIXct(strptime(paste(df$date, df$time),
                                    dtFormat), tz)
      dout[[icon]] <- df
    } else {
      cat("Problem with file: ", x[icon], " skipping\n")
      
    }
  }
  if (all(sapply(dout, is.null)))
    stop("No data to return: check the files")
  
  dout <- do.call(rbind, dout)
  if (correct.all) {
    ## should add a reporting mechanism for these as well
    ##  and return a data.frame if any of the tests fail
    ## sort them
    dout <- dout[order(dout$ptt, dout$gmt), ]
    ## remove duplicate rows
    dout <- dout[!duplicated(dout), ]
    ## adjust duplicate times (now that they are sorted properly)
    dt.by.id <- unlist(tapply(dout$gmt, dout$ptt,
                              function(x) c(-1, diff(x))))
    dup.by.eps <- which(abs(dt.by.id) < duplicateTimes.eps)
    if (length(dup.by.eps) >= 1) {
      if (verbose) {
        cat("Adjusting duplicate times\n.....\n")
        for (i in  dup.by.eps) {
          ind <- i + (-2:1)
          print(cbind(dout[ind,c("ptt", "gmt", "class")],
                      row.number=ind))
        }
      }
      dout$gmt <- adjust.duplicateTimes(dout$gmt, dout$ptt)
      if (verbose) {
        cat("\n  Adjusted records now: \n\n")
        for (i in  dup.by.eps) {
          ind <- i + (-2:1)
          print(cbind(dout[ind,c("ptt", "gmt", "class")],
                      row.number=ind))
        }
      }
    }
    if(any(dout$longitude > 180)) {
      msg <- paste("\nLongitudes contain values greater than 180,",
                   "assuming proj.4 +over\n\n")
      cat(msg)
      p4 <- "+proj=longlat +ellps=WGS84 +over"
    }
    dout$class <- ordered(dout$class,
                          levels=c("Z", "B", "A", "0", "1", "2", "3"))
    coordinates(dout) <- c("longitude", "latitude")
    proj4string(dout) <- CRS(p4)
    ##tor <- TimeOrderedRecords(c("gmt", "ptt"))
    test <- try(dout <- trip(dout, c("gmt", "ptt")))
    if (!is(test, "trip")) {
      cat("\n\n\n Data not validated: returning object of class ",
          class(dout), "\n")
      return(dout)
    }
    ## for now, only return spdftor if correct.all is TRUE
    cat("\n\n\n Data fully validated: returning object of class ",
        class(dout), "\n")
    return(dout)
  }
  cat("\n\n\n Data not validated: returning object of class ",
      class(dout), "\n")
  dout
}

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
