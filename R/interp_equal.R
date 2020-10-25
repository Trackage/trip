#' Track intermediate points
#'
#' Calculate great circle intermediate points on longitude, latitude input vectors. A spherical model is used, from the geosphere package.
#'
#' For the result to be sensible, the input must either be in longitude/latitude, or be in a projection and have a valid CRS. Great
#' circle movement is assumed, there's no way to use this to interpolate equal-distance in the native projection.
#'
#' If no input `distance` or `duration` is provided a default is used of 15 points between each input point.
#'
#' if both `distance` AND `duration` is provided, `distance` is ignored.
#'
#' Note, the original implementation of this function was called 'interpequal()', and was used for time spent
#' calculations. The functionality is now provided by the traipse package.
#' @param x trip object
#'
#' @param distance 	optional minimum distance (metres) between interpolated points
#' @param duration 	optional minimum duration (seconds) between interpolated points
#'
#' @importFrom rlang .data
#' @importFrom dplyr group_by mutate
interp_equal <- function(x, distance = NULL, duration = NULL) {
  if (!is.null(distance) && !is.null(duration)) {
    distance <- NULL
    message("ignoring 'distance', only one of 'distance' or 'duration' may be set in 'interp_equal()'")
  }
  xy <- sp::coordinates(x)
  crs <- crsmeta::crs_proj(x)
  if (sp::is.projected(x)) {
    if (!is.na(crs)) {
      xy <- reproj::reproj(xy, target = 4326, source = crs)[, c(1L, 2L)]
    }
  }
  cl <- getTORnames(x)
  xx <- data.frame(x = xy[,1L], y = xy[,2L],
                   time = x[[cl[1L]]], id = x[[cl[2L]]])
  xx1 <- dplyr::mutate(dplyr::group_by(xx, rlang::.data$id),
                       inter = traipse::track_intermediate(.data$x, .data$y, .data$time,
                                                           distance = distance, duration = duration))

  outid <- rep(xx1[["id"]],  unlist(lapply(xx1[["inter"]], function(df) dim(df)[1L])))
  out <- do.call(rbind, xx1[["inter"]])
  out[["id"]] <- outid
 out <-  suppressWarnings(trip(out, correct_all = TRUE))
 out@proj4string <- sp::CRS(crs)
 out
}
