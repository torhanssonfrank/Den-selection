
#Räkna ut längden på transekterna

install.packages("geosphere")
library(geosphere)
library(rgeos)
library(maptools)
library(sp)
library(rgdal)
library(dplyr)
library(tidyr)
library(raster)
library(writexl)
library(readxl)
library(lubridate)

ripa_vår<-read_xlsx(file.choose())# läser in tor_modifierade riptransekter vår 2018
View(ripa_vår)
class(ripa_vår)
ripa_vår<-as.data.frame(ripa_vår)

variabler <- c("CORNER", "START", "NYRIKT", "SLUT")

transektpunkter <- ripa_vår %>% 
  filter(observation %in% variabler) %>%
  unite(col = "obsID", lya, `gps-punkt`) %>% 
  dplyr::select(obsID, N, E, observation) #jävla rasterpaketet har också en funktion som heter select som fuckar med dplyr
  

head(transektpunkter)
class(transektpunkter$N)
class(transektpunkter$E)

transektpunkter$N <- as.numeric(transektpunkter$N)
transektpunkter$E <- as.numeric(transektpunkter$E)

coordinates(transektpunkter) <- c("E", "N")

summary(transektpunkter)

proj4string(transektpunkter) <-CRS("+init=EPSG:3006")

summary(transektpunkter)
par(mfrow=c(1,1))
plot(transektpunkter)

transektpunkter$obsID

writeOGR(transektpunkter, dsn = "./GIS-data/transektpunkter", layer = "transektpunkter", driver = "ESRI Shapefile")
dist(transektpunkter$obsID)
