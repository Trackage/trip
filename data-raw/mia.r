library(readr)
library(dplyr)
cl <- readr::cols(band = "c", breeding_status = "c", gmt = "T",  
           lon = "d", lat = "d", 
           species  = "c", sex = "c", deployment_status = "c", device = "c", 
           tag_ID =  "i")

 mia <- readr::read_csv("data-raw/MI_albatross_final.csv", 
                        col_types = cl) %>%  distinct() %>% 
   mutate(day = as.integer(format(gmt, "%d")), month = as.integer(format(gmt, "%m")), year = as.integer(format(gmt, "%Y")))
# 
# 
# library(RSQLite)
 db <- src_sqlite(file.path("inst/extdata", "mia_db.sqlite3"), create = TRUE)
 mia <- copy_to(db, mia, temporary = FALSE, indexes = list(c("year", "month", "day"), "breeding_status", "band", "tag_ID"))

 #' buffer an extent e1 to whole number e2
 bufext <- function(e1, e2) {
   if (e2 == 0) return(e1)
   x <- e1     ## input object that has extent()
   chunk <- e2 ## rounding chunk
   num0 <- c(xmin(x), xmax(x), ymin(x), ymax(x))
   
   extent(    c((num0[c(1L, 3L)] %/% chunk) * chunk, 
                (num0[c(2L, 4L)] %/% chunk) * chunk + chunk)[c(1, 3, 2, 4)])
   
 }
 
buftime <- function(e1, e2) {
  out <- (as.numeric(e1) %/% e2) * e2 + c(0, e2)
  class(out) <- class(e1)
  out
}

mia <- tbl(src_sqlite(file.path("inst/extdata", "mia_db.sqlite3")), "mia") %>% 
             dplyr::select(band, breeding_status, gmt, lon, lat, species, tag_ID) %>% 
             filter(breeding_status!="island") %>%  collect() %>% 
             mutate(sp_id = sprintf("%s%s_%s_%s", species, substr(breeding_status, 1, 1), band, tag_ID), 
                    gmt = gmt + ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")) %>% distinct(gmt, sp_id)

 library(spdplyr)
library(trip)
library(rgdal)
trip(mia) <- c("lon", "lat", "gmt", "sp_id")
proj4string(mia) <- CRS("+proj=longlat +ellps=WGS84")
mia <- spTransform(mia, "+proj=laea +lon_0=52 +lat_0=-55 +ellps=WGS84")
spdplyr:::.print_Spatial(mia)

gr <- raster(bufext(extent(mia), 5e4) , res = 5e4, crs = projection(mia))

library(purrr)
## time-spent per individual
indivmaps <- stack(mia %>% split(.$sp_id) %>% map(rasterize, y = gr))
indivmaps

## time-spent per time interval
bt <- buftime(range(mia$gmt), 3600 * 24 * 6 * 30)
sq <- seq(bt[1], bt[2], by = "6 months")

month6 <- stack(cut(mia, sq) %>% 
      map(rasterize, y = gr)
 )
 

               