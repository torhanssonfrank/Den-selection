library(rgeos)
library("sp")
library("rgdal")
library(dplyr)
library(tidyr)
library(raster)
library(writexl)
library(readxl)




lybuffer <- readOGR(dsn = file.choose(), layer = "lybuffer", stringsAsFactors = FALSE) #läser in filen lybuffer.shp
CRS.new <- CRS("+init=EPSG:3006")
lybuffer <- spTransform(lybuffer, CRS.new) 

summary(lybuffer)
plot(lybuffer, col = "red")
writeOGR(lybuffer, dsn ="./Geodata/buffer runt lyor/lybuffer_sweref.shp", layer = "lybuffer", driver = "ESRI Shapefile") #enklast att göra om den till sweref i R.


myrar <- readOGR(dsn = "./Geodata/myr runt lyor/myr_runt_lyor_alla.shp", layer = "myr_runt_lyor_alla", stringsAsFactors = FALSE) #viktigt med stringsAsFactors = FALSE. Annars blir det fel när man ändrar siffror till numeric.
summary(myrar)
myrar <- as.data.frame(myrar) #gör om från spatial till data frame

summary(myrar)

myr_area <- myrar %>% 
  select(Namn, Kommentar, N, E, Nyarea) #plockar ut de kolumner jag vill ha

head(myr_area)

is.character(myr_area$Nyarea) #sparas som character vid inläsning från shapefil
myr_area$Nyarea<-as.numeric(myr_area$Nyarea) #sparar om till numeric

head(myr_area) #siffrorna stämmer fortfarande. Det gör de INTE om man ändrar från faktor till numeric

is.factor(myr_area$Namn)
myr_area$Namn<-as.factor(myr_area$Namn) #här måste det vara faktor för att dplyr ska fungera


myr_area<-myr_area %>% 
  group_by(Namn,Kommentar, N, E) %>% 
  summarise(area_myr = sum(Nyarea))

nrow(myr_area) #alla lyor är inte med här eftersom de som inte har någon area följer med. Endast 74 är med
View(myr_area)

alla_lyor<- readOGR(dsn = "./Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.shp", layer = "Lyor helags alla", stringsAsFactors = FALSE)

nrow(alla_lyor) #här är alla lyor. 80 stycken

alla_lyor <- as.data.frame(alla_lyor)

head(alla_lyor)

alla_lyor<-alla_lyor %>% 
  select(Namn,Kommentar, N, E)
head(alla_lyor)
alla_lyor<-cbind(alla_lyor, `area_myr` = c(0)) #bättre att använda c här istället för "". med c blir det automatiskt numeric.
head(alla_lyor)


överblivna_lyor <- alla_lyor[!alla_lyor$Namn %in% myr_area$Namn, ] #plockar ut de lyor som finns i alla_lyor men inte i my_area

överblivna_lyor

alla_myrlyor <- bind_rows(överblivna_lyor, myr_area)

nrow(alla_myrlyor) # nu är det 80 lyor varav 6 inte har någon myrarea.
View(alla_myrlyor)

write_xlsx(alla_myrlyor, path = "Lyor, kullar, gps-punkter, yta och avstånd/Area med myrmark med 1,5 km radie runt varje lya.xlsx")

###### Samma sak fast med vatten######

vatten <- readOGR(dsn = file.choose(), layer = "vatten_runt_lyor", stringsAsFactors = FALSE) #viktigt med stringsAsFactors = FALSE. Annars blir det fel när man ändrar siffror till numeric.
summary(vatten)
vatten <- as.data.frame(vatten) #gör om från spatial till data frame

summary(vatten)

vatten_area <- vatten %>% 
  select(Namn, Kommentar, N, E, Nyarea) #plockar ut de kolumner jag vill ha

head(vatten_area)

is.character(vatten_area$Nyarea) #sparas som character vid inläsning från shapefil
vatten_area$Nyarea<-as.numeric(vatten_area$Nyarea) #sparar om till numeric

head(vatten_area)  #siffrorna stämmer fortfarande. Det gör de INTE om man ändrar från faktor till numeric

is.factor(vatten_area$Namn)
vatten_area$Namn<-as.factor(vatten_area$Namn) #här måste det vara faktor för att dplyr ska fungera


vatten_area<-vatten_area %>% 
  group_by(Namn,Kommentar, N, E) %>% 
  summarise(area_vatten = sum(Nyarea))

nrow(vatten_area) #alla lyor är med. Alla har alltså vatten inom en radie på 1.5 km
View(vatten_area)

write_xlsx(vatten_area, path = "Lyor, kullar, gps-punkter, yta och avstånd/Area som är vatten med 1,5 km radie runt varje lya.xlsx")
