library(dplyr)

mia <- tbl(src_sqlite(file.path("inst/extdata", "mia_db.sqlite3")), "mia") %>% 
  dplyr::select(band, breeding_status, gmt, lon, lat, species, tag_ID) %>% 
  filter(breeding_status!="island") %>%  collect() %>% 
  mutate(sp_id = sprintf("%s%s_%s_%s", species, substr(breeding_status, 1, 1), band, tag_ID), 
         gmt = gmt + ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "GMT")) %>% distinct(gmt, sp_id)

library(spdplyr)
library(trip)
library(rgdal)

nform <- function (data, ..., branch_cols = character()) {
  #key_col <- tidyr:::col_name(substitute(.key))
  vertex_cols <- unname(dplyr::select_vars(colnames(data), ...))
  nform_(data, vertex_cols, branch_cols)
}

nform_ <- function(x, ...) UseMethod("nform_")


nform_.data.frame <- function(data, vertex_cols = character(), branch_cols) {
  group_cols <- setdiff(names(data), vertex_cols)
  #list(group_cols, vertex_cols)
  vertices = select_(data, .dots = vertex_cols)
  
  objects = select_(data, .dots = group_cols)
  
}
nform(mia, gmt, lon, lat) %>% distinct(sp_id)
