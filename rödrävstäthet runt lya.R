
# RÄKNAR UT RÖDRÄVSTÄTHET RUNT VARJE LYA MED MIN RÖDRÄVSRASTER
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

heatmap <- raster("Geodata/Rödrävsraster/rödrävheatmap.tif")
lyor  <- readOGR(dsn = "./Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.shp", layer = "Lyor helags alla", stringsAsFactors = FALSE)

heatmap@crs #Sweref
summary(lyor) # är också sweref. 

mean_density <- raster::extract(heatmap,             # raster layer. tidyr som också är laddat har en extract-funktion också. Därför måste jag specificera att det är extract från raster jag vill ha.
                           lyor,   # SPDF with centroids for buffer
                           buffer = 2500,     # buffer size, units depend on CRS. Väljer 2500 eftersom rödrävars territorium är stora, typ 19.5 kvm (2,5 km radie)
                           fun=mean,         # what value to extract. 
                           df=TRUE)         # return a dataframe? 

head(mean_density)
colnames(mean_density) <- c("Namn", "rödräv_densitet")

mean_density$Namn <- lyor$Namn
View(mean_density)

#Printar filen

write_xlsx(mean_density, path = "Den and territory selection/Data/rödrävstäthet runt lyor.xlsx")

