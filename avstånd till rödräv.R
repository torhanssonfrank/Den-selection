# avstånd till närmsta rödräv

library(maptools)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

rödräv <- read_xlsx(path = "Skript och rödrävsdata från Rasmus/red_fox_2000_2016_sweref.xlsx")
class(rödräv) # blir inläst som table data frame, table och data frame på samma gång

rödräv <- as.data.frame(rödräv)
View(rödräv)
class(rödräv$Year) #numeric
#plockar ut några separata år för att se om rävarna skjuts på ungefär samma ställe

x2001 <- rödräv %>% 
  filter(Year == 2001)
length(x2001$Year)

coordinates(x2001) <- c("E", "N")
proj4string(x2001) <- CRS("+init=EPSG:3006")
summary(x2001) #sweref och projected
plot(x2001)

x2005 <- rödräv %>% 
  filter(Year == 2005)

coordinates(x2005) <- c("E", "N")
proj4string(x2005) <- CRS("+init=EPSG:3006")
summary(x2005) #sweref och projected
plot(x2005)

x2008 <- rödräv %>% 
  filter(Year == 2008)

coordinates(x2008) <- c("E", "N")
proj4string(x2008) <- CRS("+init=EPSG:3006")
summary(x2008) #sweref och projected
plot(x2008)

writeOGR(x2001, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2001", layer = "red.fox.2001", driver = "ESRI Shapefile")
           
writeOGR(x2005, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2005", layer = "red.fox.2005", driver = "ESRI Shapefile")

writeOGR(x2008, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2008", layer = "red.fox.2008", driver = "ESRI Shapefile")

# kollar 2014 och 2016 också. Vill se vilken lya som är den närmaste
x2014 <- rödräv %>% 
  filter(Year == 2014)

coordinates(x2014) <- c("E", "N")
proj4string(x2014) <- CRS("+init=EPSG:3006")
summary(x2014) #sweref och projected
plot(x2014)

writeOGR(x2014, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2014", layer = "red.fox.2014", driver = "ESRI Shapefile")

x2016 <- rödräv %>% 
  filter(Year == 2016)

coordinates(x2016) <- c("E", "N")
proj4string(x2016) <- CRS("+init=EPSG:3006")
summary(x2016) #sweref och projected
plot(x2016)

writeOGR(x2016, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2016", layer = "red.fox.2016", driver = "ESRI Shapefile")


#' rödrävarna är skjutna överallt. Inget mönster i datat.
#' Istället för att välja avstånd till ett rödrävsområde gör
#' jag ett antagande att rödräven befann sig på samma område sommaren innan
#' vintern den blev skjuten. Därefter lägger jag på en buffer runt punkten där
#' räven blev skjuten som motsvarar rödrävens revirstorlek (som jag hittar i litteraturen)
