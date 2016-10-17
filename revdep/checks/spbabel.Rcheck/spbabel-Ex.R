pkgname <- "spbabel"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "spbabel-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('spbabel')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("map_table")
### * map_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: map_table
### Title: A decomposition of 'vector' map data structures to tables.
### Aliases: map_table

### ** Examples

data(holey)
spholey <- sp(holey)
map_table(spholey)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("map_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("semap")
### * semap

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: semap
### Title: "South-east" map data.
### Aliases: seatt semap

### ** Examples

# recreate as sp object
mp <- sp(semap, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("semap", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sp")
### * sp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sp
### Title: Convert from dplyr tbl form to Spatial*DataFrame.
### Aliases: sp sp.data.frame

### ** Examples

library(dplyr)
semap1 <- semap  %>% dplyr::filter(y_ > -89.9999)
sp_obj <- sp(semap1, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
## look, seamless Antarctica!
## library(rgdal); plot(spTransform(sp_obj, "+proj=laea +lat_0=-70"))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sptable")
### * sptable

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sptable
### Title: Convert from sp package 'Spatial' classes to a table.
### Aliases: map_table.trip sptable sptable.SpatialLines
###   sptable.SpatialMultiPointsDataFrame sptable.SpatialPointsDataFrame
###   sptable.SpatialPolygons sptable<-

### ** Examples

## holey is a decomposed SpatialPolygonsDataFrame
spdata <- sp(holey)
library(sp)
plot(spdata, col = rainbow(nrow(spdata), alpha = 0.4))
points(holey$x_, holey$y_, cex = 4)
holes <- subset(holey, !island_)
## add the points that only belong to holes
points(holes$x_, holes$y_, pch = "+", cex = 2)

## manipulate based on topology
## convert to not-holes
notahole <- holes
notahole$island_ <- TRUE
#also convert to singular objects - note that this now means we have an overlapping pair of polys
#because the door had a hole filled by another object
notahole$object_ <- notahole$branch_
plot(sp(notahole), add = TRUE, col = "red")

## example using in-place modification with sptable<-
library(maptools)
data(wrld_simpl)
spdata2 <- spdata
library(dplyr)
## modify the geometry on this object without separating the vertices from the objects
sptable(spdata2) <- sptable(spdata2) %>% mutate(x_ = x_ + 10, y_ = y_ + 5)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sptable", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
