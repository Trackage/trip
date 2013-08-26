.intpFun <- function(x) {
    len <- round(x[3] + 1)
    new <- seq(x[1], x[2], length=len)
    if (len > 1)
        new[-len]
    else new
}

iequal <- function(x, dur=NULL, quiet=FALSE, intfun = NULL) {
    if (is.null(intfun)) intfun <- gcIntermediate
  if (!is(x, "trip"))
    stop("only trip objects supported")
  if (is.null(dur))
    stop("equal time duration must be specified \"dur=?\"")
  ## x must be a single trip
  tor <- getTORnames(x)
  tids <- getTimeID(x)
  time <- tids[, 1]
  id <- factor(tids[, 2])
  coords <- coordinates(x)
  x <- coords[,1]
  y <- coords[,2]
  levs <- levels(id)
  newPts <- vector("list", length(levs))
  ##if (is.null(dur))
  ##   dur <- as.numeric(min(unlist(tapply(as.integer(time),
  ##            id, diff))))
  for (isub in seq_along(levs)) {
    ind <- id == levs[isub]
    xx <- x[ind]
    yy <- y[ind]
    tms <- time[ind]
    dt <- diff(as.numeric(tms))
    dtn <- floor(dt/dur)
    ##ax <- cbind(xx, c(xx[-1], xx[length(xx)]), c(dtn, 0))
    ##ay <- cbind(yy, c(yy[-1], yy[length(yy)]), c(dtn, 0))
    intime <- as.numeric(tms) - min(as.numeric(tms))
    at <- cbind(intime, c(intime[-1], intime[length(intime)]),
                c(dtn, 0))

nxy <- matrix(0, nrow = sum(dtn) + 2 * length(dtn), ncol = 2)
    cnt <- 2
    for (i in seq_along(dtn)) {
        nxy[(cnt-1):(cnt + dtn[i]), ] <- rbind(c(xx[i], yy[i]),
                                                   intfun(c(xx[i], yy[i]), c(xx[(i + 1)], yy[i+1]), n = dtn[i]),
                                                   c(xx[(i + 1)], yy[i+1]))
        cnt <- cnt + dtn[i] + 2
    }
    ##nx <- unlist(apply(ax, 1, .intpFun))
    ##ny <- unlist(apply(ay, 1, .intpFun))
    nt <- unlist(apply(at, 1, .intpFun)) + min(tms)
    ni <- factor(rep(levs[isub], length=length(nt)))
##    newPts <- rbind(newPts,
    newPts[[isub]] <- data.frame(x=nxy[,1], y=nxy[,2], time=nt, id=ni)
  }
  newPts <- do.call("rbind", newPts)

  origTotal <- sum(tapply(time, id, function(x) {
    diff(range(as.numeric(x)))
  }))
  newTotal <- nrow(newPts) * dur
  uType <- "hours"
  hTotal <- sum(tapply(time, id, function(x) {
    difftime(range(x)[2], range(x)[1], units=uType)
  }))
  if (!quiet) {
    cat("lost seconds=", as.integer(origTotal - newTotal),
        " out of a total ", hTotal, " ", uType, "\n")
  }
  coordinates(newPts) <- c("x", "y")
  names(newPts) <- tor
  newPts
}
