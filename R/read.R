
readPRVa <- function(x, altloc = 1) {
  prv <- readPRV0(x)
  alt <- readAlt(x)
  if (!nrow(prv) == length(alt)) {
    warning('mismatch between PRV data and coordinate data, assuming they are aligned from start')
    prv <- prv[1:length(alt), , drop = FALSE]
  }
 if (length(alt) < 1) stop("no alternative locations found")
  tab <- utils::read.table(text = gsub("\\s+", " ", alt), sep = " ", stringsAsFactors = FALSE)
  cols <- c(5, 3)
  if (altloc == 2) cols <- c(9, 7)
  negatory <- function(x) {
    fac <- c(1, -1)[grepl("W|S", x) + 1]
    fac * as.numeric(gsub("[[:alpha:]]", "", x))
  }
  prv[,10] <- as.character(negatory(tab[[cols[1]]]))
  prv[,9] <- as.character(negatory(tab[[cols[2]]]))
  prv
}

readAlt <- function(x) {
  old.opt <- options(warn=-1)
  on.exit(options(old.opt), add = TRUE)

  altlines <- grep("      Lat1:", x, value = TRUE)
  altlines
}


readPRV0 <- function(x) {
             #"01155 18862  73 32 P 3 2009-11-04 16:43:32 -68.575   77.952  0.000 401650567"
  widths<- c(5, 1, 5, 1, 3, 1, 2, 1, 1, 1,1,1, 10, 1, 8, 1, 7, 2, 7,2, 5, 1, 9)
  x <- gsub("^\\s+", "", x)
  x <- gsub("\\s+$", "", x)
  ok <- nchar(x)== sum(widths)
  if (!any(ok)) return(NULL)
  do.call(rbind, strsplit(x[ok], "\\s+", perl = TRUE))

}
.readPRV0_old <- function(x) {
  old.opt <- options(warn=-1)
  on.exit(options(old.opt), add = TRUE)
  ## x is the text
  dlines <- strsplit(x, "\\s+", perl=TRUE)
  loclines <- unlist(lapply(dlines, length)) == 12
  print(sum(loclines))

  if (any(loclines)) {
    dfm <- matrix(unlist(dlines[loclines]),
                  ncol=12, byrow=TRUE)
    if (dfm[1,7] == "LC") {
      msg <- paste(" appears to be a diag file, skipping.",
                   "Use readDiag to obtain a dataframe. \n\n")

    }
  } else {
    cat("Problem with file: ", x, " skipping\n")
    dfm <- NULL
  }
  dfm
}

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
#' \code{\link[sp]{SpatialPointsDataFrame-class}}.  Files that are not obviously of
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
#' With \code{read_alt} the default value \code{NULL} returns the PRV location as-is. Some files may have
#' a standardized location, and a dummy. If \code{read_alt} is set to 1 or 2 the corresponding "alternative"
#' location is returned. 1 is a standardized location corresponding to the original PRV message, and 2 is a
#' "dummy" location.
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
#' @param read_alt is `NULL` by default, with longitude and latitude read from the PRV message, if `1` or `2`
#' then attempt is made to read the alternative locations (but these are not always present)
#' @param return_trip for [readDiag()] if `TRUE` will return a trip object, use `read_alt` to control the location
#' @param ... reserved for future use
#' @return
#'
#' \code{readArgos} returns a \code{trip} object, if all goes well, or simply a
#' \code{\link[sp]{SpatialPointsDataFrame-class}}.
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
#' This works on some Argos files I have seen.
#' @seealso
#'
#' \code{\link{trip}}, \code{\link[sp]{SpatialPointsDataFrame-class}},
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
#' @examples
#' argosfile <-
#'   system.file("extdata/argos/98feb.dat", package = "trip", mustWork = TRUE)
#' argos <- readArgos(argosfile)
readArgos <- function (x, correct.all=TRUE, dtFormat="%Y-%m-%d %H:%M:%S",
                       tz="GMT", duplicateTimes.eps=1e-2,
                       p4="+proj=longlat +ellps=WGS84", verbose=FALSE, read_alt = NULL, ...) {
  ## add "correct.all" argument - just return data frame if it fails, with
  ## suggestions of how to sort/fix it
  li <- length(x)
  if (any(!file.exists(x))) {
    x <- x[file.exists(x)]
    if (length(x) < 1) stop("no input files exist")
    if (length(x) < li) warning("some input files don't exist")
  }
  dout <- vector("list", length(x))


  if (is.null(read_alt)) {
    dfm <- readPRV0(unlist(lapply(x, readLines)))

  } else {
    stopifnot(read_alt == 1 || read_alt == 2)
    dfm <- readPRVa(unlist(lapply(x, readLines)), altloc = read_alt)
  }


  if (is.null(dfm) || nrow(dfm) < 1)
    stop("No data to return: check the files")

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


  dout <- df
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
                   "\n\n")
      cat(msg)
      p4 <- "+proj=longlat +datum=WGS84"
    }
    dout$class <- ordered(dout$class,
                          levels=c("Z", "B", "A", "0", "1", "2", "3"))

    coordinates(dout) <- c("longitude", "latitude")

    dout@proj4string <- CRS(p4)
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
readDiag <- function (x, return_trip = FALSE, read_alt = 1L, ...) {

    d <- unlist(lapply(x, readLines))
    locs <- d[grep("LON1", d, ignore.case=TRUE)]
    if (length(locs) < 1L) stop("no valid Diag records found")
    tms <- d[grep("DATE", d, ignore.case=TRUE)]
    bad <- (grep("\\?", locs))
    if (length(bad) > 0) {
      if (length(locs[-bad]) == 0) {
        stop(paste("no valid locations in:", paste(x, collapse = "\n"), "\n ...ignoring"))
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
    data <- data.frame(lon1=lon, lat1=ll[, 1],
                             lon2=lon2, lat2=ll[, 3],
                             gmt=gmt, id=id, lq=lq, iq=iq, stringsAsFactors = FALSE)

    if (return_trip) {
      if (!read_alt %in% c(1, 2)) stop("read_alt must be either 1 or 2")
      if (read_alt == 1L) {
        data <- trip(data[c("lon1", "lat1", "gmt", "id", "lq", "iq", "lon2", "lat2")])
      }
      if (read_alt == 2L) {
        data <- trip(data[c("lon2", "lat2", "gmt", "id", "lq", "iq", "lon1", "lat1")])
      }
    }

  data
}
