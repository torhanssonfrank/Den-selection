# avstånd till närmsta rödräv

library(SearchTrees) #använde aldrig det här paketet. Kan användas för att beräkna minsta avstånd.
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
lyor <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/lyor helags alla.xlsx")
lyor<- as.data.frame(lyor)
View(lyor)


rödräv <- as.data.frame(rödräv)
View(rödräv)
class(rödräv$Year) #numeric

# Plockar ut de enskilda åren så att jag kan mäta och undersöka dem separat

# År 2000
x2000 <- rödräv %>%
  filter(Year == 2000)
length(x2000$Year) # tre rävar skjutna

# År 2001
x2001 <- rödräv %>% 
  filter(Year == 2001)
length(x2001$Year)

coordinates(x2001) <- c("E", "N")
proj4string(x2001) <- CRS("+init=EPSG:3006")
summary(x2001) #sweref och projected
plot(x2001)


# År 2002

x2002 <- rödräv %>% 
  filter(Year == 2002)
length(x2002$Year)

coordinates(x2002) <- c("E", "N")
proj4string(x2002) <- CRS("+init=EPSG:3006")
summary(x2002) #sweref och projected
plot(x2002)

# År 2003
x2003 <- rödräv %>% 
  filter(Year == 2003)
length(x2003$Year)

coordinates(x2003) <- c("E", "N")
proj4string(x2003) <- CRS("+init=EPSG:3006")
summary(x2003) #sweref och projected
plot(x2003)

# År 2004

x2004 <- rödräv %>% 
  filter(Year == 2003)
length(x2004$Year)

coordinates(x2004) <- c("E", "N")
proj4string(x2004) <- CRS("+init=EPSG:3006")
summary(x2004) #sweref och projected
plot(x2004)

# År 2005
x2005 <- rödräv %>% 
  filter(Year == 2005)

coordinates(x2005) <- c("E", "N")
proj4string(x2005) <- CRS("+init=EPSG:3006")
summary(x2005) #sweref och projected
plot(x2005)

# År 2006
x2006 <- rödräv %>% 
  filter(Year == 2006)

coordinates(x2006) <- c("E", "N")
proj4string(x2006) <- CRS("+init=EPSG:3006")
summary(x2006) #sweref och projected
plot(x2006)

# År 2007
x2007 <- rödräv %>% 
  filter(Year == 2007)

coordinates(x2007) <- c("E", "N")
proj4string(x2007) <- CRS("+init=EPSG:3006")
summary(x2007) #sweref och projected
plot(x2007)


# År 2008
x2008 <- rödräv %>% 
  filter(Year == 2008)

coordinates(x2008) <- c("E", "N")
proj4string(x2008) <- CRS("+init=EPSG:3006")
summary(x2008) #sweref och projected
plot(x2008)


# År 2009
x2009 <- rödräv %>% 
  filter(Year == 2009)

coordinates(x2009) <- c("E", "N")
proj4string(x2009) <- CRS("+init=EPSG:3006")
summary(x2009) #sweref och projected
plot(x2009)

# År 2010

x2010 <- rödräv %>% 
  filter(Year == 2010)

coordinates(x2010) <- c("E", "N")
proj4string(x2010) <- CRS("+init=EPSG:3006")
summary(x2010) #sweref och projected
plot(x2010)

# År 2011
x2011 <- rödräv %>% 
  filter(Year == 2011)

coordinates(x2011) <- c("E", "N")
proj4string(x2011) <- CRS("+init=EPSG:3006")
summary(x2011) #sweref och projected
plot(x2011)

# År 2012
x2012 <- rödräv %>% 
  filter(Year == 2012)

coordinates(x2012) <- c("E", "N")
proj4string(x2012) <- CRS("+init=EPSG:3006")
summary(x2012) #sweref och projected
plot(x2012)

# År 2013
x2013 <- rödräv %>% 
  filter(Year == 2013)

coordinates(x2013) <- c("E", "N")
proj4string(x2013) <- CRS("+init=EPSG:3006")
summary(x2013) #sweref och projected
plot(x2013)

# År 2014 
x2014 <- rödräv %>% 
  filter(Year == 2014)

coordinates(x2014) <- c("E", "N")
proj4string(x2014) <- CRS("+init=EPSG:3006")
summary(x2014) #sweref och projected
plot(x2014)

# År 2015
x2015 <- rödräv %>% 
  filter(Year == 2015)

coordinates(x2015) <- c("E", "N")
proj4string(x2015) <- CRS("+init=EPSG:3006")
summary(x2015) #sweref och projected
plot(x2015)

# År 2016
x2016 <- rödräv %>% 
  filter(Year == 2016)

coordinates(x2016) <- c("E", "N")
proj4string(x2016) <- CRS("+init=EPSG:3006")
summary(x2016) #sweref och projected
plot(x2016)



#plockar ut några separata år för att se i QGIS om rävarna skjuts på ungefär samma ställe

writeOGR(x2001, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2001", layer = "red.fox.2001", driver = "ESRI Shapefile")

writeOGR(x2002, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2002", layer = "red.fox.2002", driver = "ESRI Shapefile")

writeOGR(x2005, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2005", layer = "red.fox.2005", driver = "ESRI Shapefile")

writeOGR(x2008, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2008", layer = "red.fox.2008", driver = "ESRI Shapefile")

writeOGR(x2014, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2014", layer = "red.fox.2014", driver = "ESRI Shapefile")

writeOGR(x2016, dsn = "Skript och rödrävsdata från Rasmus/2001, 2005 och 2008 att titta på i qgis/red.fox.2016", layer = "red.fox.2016", driver = "ESRI Shapefile")


#' rödrävarna är skjutna överallt. Inget mönster i datat.
#' Istället för att välja avstånd till ett rödrävsområde gör
#' jag ett antagande att rödräven befann sig på samma område sommaren innan
#' vintern den blev skjuten. Alternativ: lägga på en buffer runt punkten där
#' räven blev skjuten som motsvarar rödrävens revirstorlek (vilket är 19.5 km^2 enligt 
#' Walton så det blir nog ett för stort område, dvs cirkel med 2.5 km radie vilket
#' också blir ett antagande eftersom räven måste ha befunnit sig i mitten av sitt revir
#' när den blev skjuten för att det ska bli ett vettigt antagande.)

# börjar med att spara om alla lyor som spatial
#Gör en kopia av lyor att göra om till spatial
lyor$N <- as.numeric(lyor$N)
lyor$E <- as.numeric(lyor$E)

lyorSP <- cbind(lyor)

coordinates(lyorSP) <- c("E", "N")
lyorSP<- SpatialPoints(lyorSP) #funkar med SpatialPointsDataFrame också så den här raden behövs egentligen inte.
proj4string(lyorSP) <- CRS("+init=EPSG:3006")
summary(lyorSP) #sweref och projected
plot(lyorSP)



#' skapar dataramar för att hålla distanserna
#' jag kan bara göra åren som har rödrävar skjutna nästkommande vinter. Därför
#' blir det bara mellan 2001 och 2015
år2001 <- data.frame(Namn = lyor$Namn, År = (rep(2001,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2002 <- data.frame(Namn = lyor$Namn, År = (rep(2002,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2003 <- data.frame(Namn = lyor$Namn, År = (rep(2003,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2004 <- data.frame(Namn = lyor$Namn, År = (rep(2004,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2005 <- data.frame(Namn = lyor$Namn, År = (rep(2005,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2006 <- data.frame(Namn = lyor$Namn, År = (rep(2006,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2007 <- data.frame(Namn = lyor$Namn, År = (rep(2007,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2008 <- data.frame(Namn = lyor$Namn, År = (rep(2008,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2009 <- data.frame(Namn = lyor$Namn, År = (rep(2009,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2010 <- data.frame(Namn = lyor$Namn, År = (rep(2010,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2011 <- data.frame(Namn = lyor$Namn, År = (rep(2011,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2012 <- data.frame(Namn = lyor$Namn, År = (rep(2012,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2013 <- data.frame(Namn = lyor$Namn, År = (rep(2013,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2014 <- data.frame(Namn = lyor$Namn, År = (rep(2014,length(lyor$Namn))), närmaste_rödräv = numeric(80))
år2015 <- data.frame(Namn = lyor$Namn, År = (rep(2015,length(lyor$Namn))), närmaste_rödräv = numeric(80))


#Räknar ut avstånd till närmsta rödräv från varje lya. "min" ger kortaste avståndet
år2001$närmaste_rödräv <- apply(gDistance(x2002, lyorSP, byid=TRUE), 1, min)
år2002$närmaste_rödräv <- apply(gDistance(x2003, lyorSP, byid=TRUE), 1, min)
år2003$närmaste_rödräv <- apply(gDistance(x2004, lyorSP, byid=TRUE), 1, min)
år2004$närmaste_rödräv <- apply(gDistance(x2005, lyorSP, byid=TRUE), 1, min)
år2005$närmaste_rödräv <- apply(gDistance(x2006, lyorSP, byid=TRUE), 1, min)
år2006$närmaste_rödräv <- apply(gDistance(x2007, lyorSP, byid=TRUE), 1, min)
år2007$närmaste_rödräv <- apply(gDistance(x2008, lyorSP, byid=TRUE), 1, min)
år2008$närmaste_rödräv <- apply(gDistance(x2009, lyorSP, byid=TRUE), 1, min)
år2009$närmaste_rödräv <- apply(gDistance(x2010, lyorSP, byid=TRUE), 1, min)
år2010$närmaste_rödräv <- apply(gDistance(x2011, lyorSP, byid=TRUE), 1, min)
år2011$närmaste_rödräv <- apply(gDistance(x2012, lyorSP, byid=TRUE), 1, min)
år2012$närmaste_rödräv <- apply(gDistance(x2013, lyorSP, byid=TRUE), 1, min)
år2013$närmaste_rödräv <- apply(gDistance(x2014, lyorSP, byid=TRUE), 1, min)
år2014$närmaste_rödräv <- apply(gDistance(x2015, lyorSP, byid=TRUE), 1, min)
år2015$närmaste_rödräv <- apply(gDistance(x2016, lyorSP, byid=TRUE), 1, min)

# Smackar ihop alla i en dataram
distans_rödräv <-rbind(år2001, år2002, år2003,
                            år2004, år2005, år2006,
                            år2007, år2008, år2009,
                            år2010, år2011, år2012,
                            år2013, år2014, år2015)
View(distans_rödräv)                            

# Och printar filen
write_xlsx(distans_rödräv, path = "Den and territory selection/Rawdata/avstånd till rödräv 2001-2015.xlsx")



