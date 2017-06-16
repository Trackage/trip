trip2 <- function(.data, ...) {
  aesth <- ggplot2::aes(...)
  if (is.null(names(aesth))) {
    mess <- sprintf("need at least x, y, time, id nominated from\n\n%s", 
                    paste(sort(names(.data)), collapse = "\n"))

    stop(mess)
  }
  aesth
}

# examples
#d <- as.data.frame(walrus818[1:100, ])
#trip2(d, X_AED170_70, Y_AED170_70, time = DataDT, id = Deployment)

