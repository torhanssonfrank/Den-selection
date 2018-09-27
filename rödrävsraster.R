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
foxBuffer$fox_ID <- as.character(1:length(foxBuffer$Year))
foxBuffer$fox_ID
summary(foxBuffer)
#' Skriver ut filen igen så att jag har ID för varje obs. Blir kanske enklare att 
#' hantera den i qgis
writeOGR(foxBuffer, dsn = "Geodata/buffer runt lyor/rödrävsbuffer2000_2016.shp", layer = "foxBuffer", driver = "ESRI Shapefile")

#' Gör en enkel uträkning på hur många skjutna rödrävar mellan 2000-2016
#'  som hamnar inom varje lybuffer. Sedan mäter jag avstånd till alla rödrävar
#'  från alla lyor och tar en subset på alla som är närmare än 2500 meter

lyor <- readOGR(dsn = "Lyor, kullar, gps-punkter, yta och avstånd/lyor helags alla.shp", stringsAsFactors = FALSE)
summary(lyor) # sweref och projected

#Lägger på en buffer med radie 2500 meter
lybufferstor <- gBuffer(lyor, byid=TRUE, width=2500)
summary(lybufferstor)# sweref och projected

overlap<-over(red_fox, lybufferstor)

table(overlap$Namn) #Här är de ihopräknade rävarna.

# Avstånd till alla skjutna rödrävar per lya. Tar bara de som är närmre än 2500 meter
k<-gDistance(lyor, red_fox, byid = TRUE) # Ger alla avstånd i en matris.
colnames(k)<- c(lyor$Namn) #Namnen försvinner eftersom outputen är en matris. Namnger igen

k <- as.data.frame(k)
ks<-stack(k) # stack är en king basfunktion. Den gör om kolumnnamnen till rader. Bra när man jobbar med matriser. De nya kolumnnamnen blir value och ind.

View(ks)
colnames(ks)<- c("distans", "Namn")
ks.sub <- ks %>% 
  group_by(Namn) %>% 
  filter(distans < 2500)

View(ks.sub)

ks.count <- ks.sub %>%
  count(Namn)

?count 
View(ks.count)

ks.sum <- ks.sub %>% 
  group_by(Namn) %>% 
  summarise(tot.dist = sum(distans))

(View(ks.sum))
ks.rel<-ks.sum %>% 
  left_join(ks.count, by = "Namn")

ks.rel <- ks.rel %>% 
  mutate(rel.dist = n/tot.dist)

View(ks.rel)

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
summary(fox)
#' Lämmelrastern är väldigt stor. Den går hela vägen till treriksröset.
#' Av någon anledning gick den inte beskära i qgis så jag gör det här istället.
#' Genom att klicka på kartan i qgis med coordinate capture plugin får jag ut koordinaterna
#' för den västligaste, östligaste, sydligaste och nordligaste punkten som jag vill ha. Sen kan jag
#' beskära.
#' 
west <- 351409.447
east <-445337.351
north <-7016939.202
south <-6955416.424
e <- as(extent(west, east, south, north), 'SpatialPolygons')
crs(e) <- "+init=EPSG:3006"
helagsRaster <- crop(helagsRaster, e)


redRaster <- shp2raster(shp = foxBuffer,
                        mask.raster = helagsRaster, label = "Red fox density in Helags", 
                        transform = FALSE, value = 1)





#' Förslag från https://stat.ethz.ch/pipermail/r-sig-geo/2014-March/020595.html
#' för att göra en raster av buffer
#A first cut at an example:
  
  library(sp)
library(rgeos)
plot(helagsRaster)
pts<-SpatialPoints(red_fox)
pols <- gBuffer(pts, byid=TRUE, width=1500)
proj4string(pols) <- CRS("+init=EPSG:3006")
plot(pols)
STR <- gBinarySTRtreeQuery(pols, pols)
length(STR)
summary(sapply(STR, length))
res <- list()


for(i in seq(along=STR)){ res[[i]] <- gOverlaps(pols[i], pols[STR[[i]]],
                                               byid=TRUE)
}

res

summary(sapply(res, sum)-1) # to remove self-counting, i overlaps i

res<-SpatialPolygonsDataFrame(res)
proj4string(res) <- CRS("+init=EPSG:3006")
plot(res)
#So then we have # overlap counts per input polygon - but what are the 
#output reporting units? Should we actually be counting the number of 
#polygons that a fine grid of points over box belong to? If we focus on 
#this, then maybe:
?GridTopology
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


