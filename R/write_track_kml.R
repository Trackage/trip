
#' Create a time-continuous KML file 
#' 
#' Export track data to a KML file, for use in Google Earth the continuous
#' time slider. 
#' 
#' To include altitude set every argument explicitly, by input of separate 
#' 'id', 'lon', 'lat', 'utc' and 'z' arguments. If the first argument 'id' is 
#' a trip object there is no facility to include the 'z' altitude values. 
#' 
#' If 'z' is included it is applied as a third coordinate, with 'altitude_mode' 
#' controlling the interpretation, see 
#' \url{https://developers.google.com/kml/documentation/altitudemode}.
#' If the 'kml_file' ends with ".kmz" the file is compressed, otherwise it must 
#' end with ".kml" and the compression archive step is not applied. 
#' 
#' Sadly the interactive time slider is only available with the desktop version of
#' Google Earth, the data loads into the browser version but can't be interactive. 
#' @param id vector of grouping IDs (or a trip object)
#' @param lon vector of longitude (ignored if id is a trip)
#' @param lat vector of latitude (ignored if id is a trip)
#' @param utc vector of POSIXct date-times (ignored if id is a trip)
#' @param z vector of elevations, this cannot be set if 'id' is a trip
#' @param kml_file filename for KML (KML or KMZ) (must end in .kml or .kmz)
#' @param name internal name of dat (derived from kml_file if not specified)
#' @param altitude_mode the altitude mode, 'absolute', 'clampToGround', 'clampToSeaFloor', 
#' 'relativeToGround', or 'relativeToSeaFloor', see Details
#' @return character vector, file name location of file produced
#' @export
#' @author Original implementation by Tomislav Hengl in the 'plotKML' package for 'SpatialLinesDataFrame', 
#' adapted by M. Sumner for use in continuous-time form. 
#' @examples
#' \dontrun{
#' kfile <- write_track_kml(walrus818[seq(1, 1000, by = 5), ])
#' print(kfile)
#' unlink(kfile)
#' }
write_track_kml <- function(id, lon, lat, utc, z = NULL, 
                            kml_file =  tempfile(fileext = ".kmz"), 
                            name = NULL, 
                            altitude_mode = c("absolute", "clampToGround", "clampToSeaFloor", 
                            "relativeToGround", "relativeToSeaFloor")) {
  
  altitude_mode <- match.arg(altitude_mode)
  stopifnot(grepl("\\.kmz$", kml_file) || grepl("\\.kml$", kml_file))
  if (!inherits(id, "trip")) {
    len <- length(id)
    stopifnot(length(lon) == length(id))
    stopifnot(length(lat) == length(id))
    stopifnot(length(utc) == length(id))
    if (!is.null(z)) stopifnot(length(z) == length(id))
  }
  altitudeMode <- "clampToGround"
  if (is.null(name)) {
    name <- basename(gsub("\\.kmz$", "", kml_file))
  }
  if (inherits(id, "trip")) {
    obj <- id 
  } else {
    ## from here on is standard, as long as "obj" is a trip object
    d <- data.frame(lon = lon, lat = lat, utc = utc, id = id)
    if (!is.null(z)) {
      d[["z"]] <- z
     altitudeMode <- "absolute" 
    }
    obj <- trip(d)
  }
  
  if (!raster::isLonLat(obj)) obj <- reproj(obj, target = .llproj())
  
  xsegs <- trip::explode(obj)
  
  coords_segs <-   unlist(lapply(xsegs@lines, function(a) lapply(a@Lines, slot, "coords")), recursive = FALSE)
  if (is.null(z)) z <- rep(0, length(id))
  for (i in seq_along(coords_segs)) {
      coords_segs[[i]] <- cbind(coords_segs[[i]], c(z[i], altitude = z[i+1]))
  }
  coordinates <- unlist(lapply(coords_segs, function(x) paste(apply(x, 1, paste, collapse = ","), collapse = "\n")))  ## will need the get Z and/or M for sf
  tripID <- xsegs[["id"]]
  linesegID <- unlist(lapply(split(tripID, tripID), seq_along))
  time_begin <- format(xsegs[["starttime"]], "%Y-%m-%dT%H:%M:%S+00:00")
  time_end <- format(xsegs[["endtime"]], "%Y-%m-%dT%H:%M:%S+00:00")
  aaggbbrr <- function(x) {
    unlist(lapply(strsplit(x, ""), function(x) paste(x[1L], 
                                                     x[8L], x[9L], 
                                                     x[6L], x[7L], 
                                                     x[4L], x[5L], 
                                                     x[2L], x[3L], collapse = "", sep = "")))
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
altitude <- glue::glue('<altitudeMode>{altitudeMode}</altitudeMode>')
# not needed ##if (!is.null(z)) altitude <- sprintf('<altitude>{z}</altitude>\n%s', altitude)
template_line <- '
  <Placemark> 
  <TimeSpan><begin>{time_begin}</begin><end>{time_end}</end></TimeSpan>
  <LineString>
  {altitude}
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
 line <- glue::glue(template_line)
 template_document <- glue::glue(template_document)
 doc <- sprintf(template_document, paste(line, collapse = "\n"))

 if (grepl("kmz$", kml_file)) {
   tfile <- tempfile(fileext = ".kml")
   writeLines(doc, tfile)
   utils::zip(kml_file, tfile)
   unlink(tfile)
 } else {

   writeLines(doc, kml_file)
 }
 kml_file
}
