install.packages("sf") #som sp fast man kan använda pipes
library(raster)
library(maptools)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(sf)

buffer<-readOGR(dsn = "Geodata/buffer runt lyor/rödrävsbuffer2000_2016.shp")
buffer <-as.data.frame(buffer)
buffer$bufferID <- as.character(1:length(buffer$Date))
red_fox <- readOGR(dsn = "Lyor, kullar, gps-punkter, yta och avstånd/red_fox_2000_2016_sweref/red_fox_2000_2016.shp")
helagsRaster <- raster("Gnagardata/Lämmelprediktion uppgångsår.tif")
helagsRaster[!is.na(helagsRaster)] <- 0 # nu har jag ett tomt bakgrundsraster. Pixlarna kanske är för små men kan iofs ta ett medelvärde.

helagsRaster@crs # sweref
foxBuffer <- gBuffer(red_fox, byid=TRUE, width=1500)
#' En skriven funktion från https://amywhiteheadresearch.wordpress.com/2014/05/01/shp2raster/
#' för att konvertera shapefiler till raster

shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
                       proj.to = NA, map = TRUE) {
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
            overwrite = T)
  
  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }
  
  names(r) <- label
  return(r)
}
 
plot(helagsRaster, axes = F, box = F, legend = F, main = "Helags")
plot(foxBuffer, add = T)


#A first cut at an example:
  
  library(sp)
library(rgeos)

pols <- gBuffer(red_fox, byid=TRUE, width=1500)
plot(pols, add=TRUE)
STR <- gBinarySTRtreeQuery(pols, pols)
length(STR)
summary(sapply(STR, length))
res <- vector(mode="list", length=length(STR))

for(i in seq(along=STR)) res[[i]] <- gOverlaps(pols[i], pols[STR[[i]]],
                                               byid=TRUE)


summary(sapply(res, sum)-1) # to remove self-counting, i overlaps i

#So then we have # overlap counts per input polygon - but what are the 
#output reporting units? Should we actually be counting the number of 
#polygons that a fine grid of points over box belong to? If we focus on 
#this, then maybe:
  
  GT <- GridTopology(c(0.5, 0.5), c(1, 1), c(1000, 1000))
SG <- SpatialGrid(GT)
o <- over(SG, pols, returnList=TRUE)
ct <- sapply(o, length)
summary(ct)
SGDF <- SpatialGridDataFrame(SG, data=data.frame(ct=ct))
spplot(SGDF, "ct", col.regions=bpy.colors(20))

#is closer? I haven't checked why the overlaps and the grid over counts 
#differ - homework for someone? How does the output resolution affect the 
#detected number of hits?

#A first cut at an example:

  library(sp)
library(rgeos)
box <- readWKT("POLYGON((0 0, 0 1000, 1000 1000, 1000 0, 0 0))")
plot(box)
set.seed(1)
pts <- spsample(box, n=2000, type="random")
pols <- gBuffer(pts, byid=TRUE, width=50)
plot(pols, add=TRUE)
STR <- gBinarySTRtreeQuery(pols, pols)
length(STR)
summary(sapply(STR, length))
res <- vector(mode="list", length=length(STR))
for(i in seq(along=STR)) res[[i]] <- gOverlaps(pols[i], pols[STR[[i]]],
                                               byid=TRUE)
summary(sapply(res, sum)-1) # to remove self-counting, i overlaps i

#So then we have # overlap counts per input polygon - but what are the 
#output reporting units? Should we actually be counting the number of 
#polygons that a fine grid of points over box belong to? If we focus on 
#this, then maybe:
  
  GT <- GridTopology(c(0.5, 0.5), c(1, 1), c(1000, 1000))
SG <- SpatialGrid(GT)
o <- over(SG, pols, returnList=TRUE)
ct <- sapply(o, length)
summary(ct)
SGDF <- SpatialGridDataFrame(SG, data=data.frame(ct=ct))
spplot(SGDF, "ct", col.regions=bpy.colors(20))

#is closer? I haven't checked why the overlaps and the grid over counts 
#differ - homework for someone? How does the output resolution affect the 
#detected number of hits?


