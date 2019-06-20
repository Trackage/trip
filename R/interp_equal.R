interp_equal <- function(x, dur = NULL) {
  tor <- getTORnames(x)
  cds <- sp::coordinates(x)
  date <- x[[tor[1]]]
  id <- x[[tor[2]]]
  d <- tibble::tibble(x = cds[,1], y = cds[,2], 
                      date = date, id = id)
  
  if (is.null(dur)) {
    dur <- dplyr::group_by(d, id) %>% 
      dplyr::summarize(dur = unclass(max(date)) - unclass(min(date))) %>% 
      dplyr::summarize(dur = mean(dur)/100) %>% dplyr::pull(dur)
  }
  out <- d %>% dplyr::group_by(id) %>% 
    dplyr::mutate(duration = dplyr::lead(unclass(date), 1L) - unclass(date)) %>% 
    dplyr::mutate(nn = pmax(3L, round(duration / dur))) %>% 
    dplyr::mutate(x1 = dplyr::lead(x, 1), y1 = dplyr::lead(y, 1), t1 = dplyr::lead(date, 1)) %>% 
    dplyr::slice(-dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    purrr::transpose() %>% 
    furrr::future_map_dfr(~{ tibble::tibble(x = approx(c(.x$x, .x$x1), n = .x$nn)$y, 
                                            y = approx(c(.x$y, .x$y1), n = .x$nn)$y, 
                                            date = approx(c(.x$date, .x$t1), n = .x$nn)$y, 
                                            id = .x$id)})
   out$date <- ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC") + out$date
  out <- trip(dplyr::distinct(out))
  raster::projection(out) <- raster::projection(x)
  out
}

