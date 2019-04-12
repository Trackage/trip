
#' Create a time-continuous KML file 
#' 
#' Export track data to a KML file, for use in Google Earth the continuous
#' time slider. 
#' 
#'
#' @param id vector of grouping IDs (or a trip object)
#' @param lon vector of longitude (ignored if id is a trip)
#' @param lat vector of latitude (ignored if id is a trip)
#' @param utc vector of POSIXct date-times (ignored if id is a trip)
#' @param z vector of elevations (currently ignored)
#' @param kml_file filename for KMZ (must end in .kmz)
#' @param name internal name of dat (derived from kml_file if not specified)
#'
#' @return
#' @export
#'
#' @examples
#' library(trip)
#' write_track_kml(walrus818)
write_track_kml <- function(id, lon, lat, utc, z = NULL, 
                            kml_file =  tempfile(fileext = ".kmz"), 
                            name = NULL) {
  
   stopifnot(grepl("\\.kmz$", kml_file))
  
  if (is.null(name)) {
    name <- basename(gsub("\\.kmz$", "", kml_file))
  }
  if (inherits(id, "trip")) {
    obj <- id 
  } else {
    ## from here on is standard, as long as "obj" is a trip object
    d <- data.frame(lon = lon, lat = lat, utc = utc, id = id)
    if (!is.null(z)) d[["z"]] <- z
    obj <- trip(d)
  }
  if (!raster::isLonLat(obj)) obj <- sp::spTransform(obj, "+init=epsg:4326")
  
  xsegs <- trip::explode(obj)
  coords_segs <-   unlist(lapply(xsegs@lines, function(a) lapply(a@Lines, slot, "coords")), recursive = FALSE)
  coordinates <- unlist(lapply(coords_segs, function(x) paste(apply(cbind(x, 0), 1, paste, collapse = ","), collapse = "\n")))  ## will need the get Z and/or M for sf
  tripID <- xsegs[["id"]]
  linesegID <- unlist(lapply(split(tripID, tripID), seq_along))
  time_begin <- format(xsegs[["starttime"]], "%Y-%m-%dT%H:%M:%S+00:00")
  time_end <- format(xsegs[["endtime"]], "%Y-%m-%dT%H:%M:%S+00:00")
  aaggbbrr <- function(x) {
    unlist(lapply(strsplit(x, ""), function(x) paste(x[7], x[8], x[5], x[6], x[3], x[4], x[1], x[2], collapse = "", sep = "")))
  }


  line_colour <- aaggbbrr(tolower(viridis::viridis(length(unique(tripID)))[factor(tripID)]))


  template_document <- '<kml xmlns:xsd="http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" xmlns:xmlns="http://www.opengis.net/kml/2.2/" version="1.0">
   <Document>
   <name>{name}</name>
   <visibility>1</visibility>
   <open>1</open>
   <Folder>

   %s
   </Folder>
   </Document>
   </kml>
  '


 template_line <- '
  <Placemark> 
  <TimeSpan><begin>{time_begin}</begin><end>{time_end}</end></TimeSpan>
  <LineString>
  <altitudeMode>clampToGround</altitudeMode>
  <coordinates>
  {coordinates}
  </coordinates>
  </LineString> 

  <Style> 
  <LineStyle>  
  <color>{line_colour}</color>
  </LineStyle> 
  </Style>
  </Placemark>
  '


 line <- glue(template_line)
 doc <- sprintf(template_document, paste(line, collapse = "\n"))

 tfile <- tempfile(fileext = ".kml")
 writeLines(doc, tfile)
 zip(kml_file, tfile)
 unlink(tfile)
 kml_file
}
