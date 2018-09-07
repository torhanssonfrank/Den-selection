install.packages("maptools")
library(maptools)
library(sp)
library(rgeos)
library(rgdal)
library(readxl)
library(writexl)

# Har använt mitt försök till sammanställning. Nu har jag avstånd för de lyorna. Borde vara alla avstånd jag behöver.



# # Vi vill beräkna avstånd till närmsta föryngring.

# Vi behöver sålunda följande
# Ingång: Lypositioner
# Urval: år (från kull-id)
# Funktion: Närmsta avstånd

# Förslag på hur göra finns på Stackoverflow:
# https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad

# gör en shapefil av Rasmus kulldata men först måste gps-positioner läggas in för varje kull.
# Filen "Lypositioner kullar 2000-2017 SWEREF99 per kull.csv" innehåller inte omatade lyor så 
# jag måste göra en fil som innehåller alla kullar
rasmuskulldata <- read_xlsx(path ="Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx") # dataramen får fortsätta heta rasmuskulldata
class(rasmuskulldata)
rasmuskulldata <- as.data.frame(rasmuskulldata)
View(rasmuskulldata)
colnames(rasmuskulldata) <- c("denNr", "Kommentar", "year", "fas")
unique(rasmuskulldata$year)


lypositionerfull <- readOGR(dsn = "Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.shp", stringsAsFactors = FALSE) #den inlästa filen är Lypositioner Rovbasen Helags SWEREF99.csv 
lypositionerfull<- as.data.frame(lypositionerfull)
View(lypositionerfull)

#readOGR lägger till två extra koordinatkullar. Tar bort dem
lypositionerfull<-lypositionerfull %>% 
  select(-c(coords.x1, coords.x2))
#har ingen excel-fil på det så skriver ut en och sparar

write_xlsx(lypositionerfull, path =  "Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.xlsx")



lypositioner<- subset(lypositionerfull, select = c(Namn, N, E)) #tar ut de kolumner jag behöver
View(lypositioner)
lypositionerKull<- merge(lypositioner, rasmuskulldata, by="Namn") #funkade bra! Trots att "lypositioner" bara har en rad per år parar merge ihop alla kullar med rätt north och east.####
View(lypositionerKull)

#' skapar en ny kolumn som heter litterID och byter namn på kolumnerna så att de
#' är samma som i Rasmus skript

lypositionerKull <-lypositionerKull %>% 
  unite(litterID, År, Namn, sep="-", remove = FALSE )
colnames(lypositionerKull)
colnames(lypositionerKull) <- c("litterID", "denNr", "north", "east", "kommentar", "year", "fas")

lypositionerKull<-subset(lypositionerKull, select = c("denNr","north","east","year","litterID")) #plockar ut de kolumner jag behöver
View(lypositionerKull)
class(lypositionerKull$east)
class(lypositionerKull$north)
class(lypositionerKull$year)

lypositionerKull$east<- as.numeric(lypositionerKull$east)
lypositionerKull$north<-as.numeric(lypositionerKull$north)

coordinates(lypositionerKull) <- c("east", "north") #öst är alltid före norr i sp-klasser. Funktionen coordinates talar om för R i vilka kolumner koordinaterna finns####
summary(lypositionerKull) # dataramen är inte projected (inte i meter), och har inget koordinatsystem.

# lägger in SWEREF 99 som koordinatsystem
proj4string(lypositionerKull) <- CRS("+init=EPSG:3006")
summary(lypositionerKull) # nu är den projected (i meter) och har koordinatsystem (SWEREF 99). Då 
# går det att mäta avstånd
plot(lypositionerKull)
# ändrade om working directory till mappen "Masterarbete" så att 
# jag inte sparar gps-punkterna i mappen som ska upp på github

# nu när jag har talat om för R vilka kolumner som innehåller koordinater och vilket
# koordinatsystem som gäller kan jag printa ut en shapefil.
writeOGR(lypositionerKull, dsn ="./Lyor, kullar, gps-punkter, yta och avstånd/lypositionerKullTor.shp", layer = "lypositionerKull", driver = "ESRI Shapefile")

#nu kan jag läsa in shapefilen igen så att jag kan mäta####

lypositionerSHP<-readOGR(dsn ="./Lyor, kullar, gps-punkter, yta och avstånd/lypositionerKullTor.shp") # läser alltså in lypositionerKullTor.shp. Märk att R sparar om east som coords.x1 och north som coords.x2
summary(lypositionerSHP)


# Vi behöver en dataframe som har variablerna vi vill använda oss av, 
# bara för att det är enklare så. Vi vill ha åren med kull. Använder Rasmus namn på kolumner och
# dataramar för att enklare kunna hålla koll på var jag är i processen.
yearFrame <- as.data.frame(unique(rasmuskulldata$year))
colnames(yearFrame) <- "yearlist"
yearFrame$denDistance <- paste("denDistance",yearFrame$yearlist, sep="")
yearFrame$minimumdenDistance <- paste("minimumdenDistance",yearFrame$yearlist, sep="")
yearFrame

# Nu kan jag använda en loop för att beräkna avstånd per år.
# assign lägger ett värde till det angivna namnet (denDistance)
# jag använder mig av gDistance (som i avstånd till skog och vatten)
# och shapefilen så att R fattar att det är koordinater

for(i in 1:length(yearFrame$yearlist)) {
  assign(yearFrame$denDistance[i],
         as.data.frame(gDistance(lypositionerSHP[lypositionerSHP$year==yearFrame$yearlist[i],], byid=T)))
}

#' Utifrån dessa dejtafrejmar (separerade per år) vill vi nu läsa ut det näst längsta 
#' avstånet (eftersom det första avståndet är
#'  till punkten själv, och det är naturligtvis noll.) Rasmus försökte automatisera, 
#'  men det var som förgjort.
#' Här lånar jag Rasmus skript rakt av. Antar att [2] anger andra värdet. Att decreasing är 
#' satt till FALSE antar jag betyder att andra värdet i stigande led väljs. I filen 
#' "ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx" finns
#' alla år förutom 2012, eftersom det inte var någon kull då. 
#' År 2000, 2003 och 2006 var det bara en kull så därför finns inget avstånd till någon 
#' annan kull





minimumdenDistance2001 <- apply(denDistance2001, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2002 <- apply(denDistance2002, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2004 <- apply(denDistance2004, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2005 <- apply(denDistance2005, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2007 <- apply(denDistance2007, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2008 <- apply(denDistance2008, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2009 <- apply(denDistance2009, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2010 <- apply(denDistance2010, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2011 <- apply(denDistance2011, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2013 <- apply(denDistance2013, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2014 <- apply(denDistance2014, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2015 <- apply(denDistance2015, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2016 <- apply(denDistance2016, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2017 <- apply(denDistance2017, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2018 <- apply(denDistance2018, 1, function(x) order(x, decreasing=F)[2])


# Härifrån stjäl jag Rasmus skript rakt av.

# Använde find-funktionen (cmd + F) för att ersätta 
# Rasmus shapefil-namn LypositionerHEFKullshp med lypositionerSHP (som jag sparade shapefilen som)
# @data$structure.c. anger de tre första avstånden (och annat skräp, typ "names" i dataramar där det bara finns två avstånd). Det är så att R ska veta vilka värden som ska
# bli en vektor. Det räcker med att skriva SminimumdenDistance2000@data$structure.c så visar R resten. År 2000 måste naturligtvis ändras till 2001 för år 2001 dataramen osv.

# Och så vill vi slå ihop datat. Vi fyller på en dejtafrejm.
litterDistance <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(litterDistance) <- c("litterID", "distance", "year")

# År 2000
# bara en kull detta år, därför blir det inte något avstånd till någon annan kull

#År 2001
SminimumdenDistance2001 <- cbind(lypositionerSHP[lypositionerSHP$year==2001,],
                                 lypositionerSHP[lypositionerSHP$year==2001,][minimumdenDistance2001,],
                                 apply(denDistance2001, 1, function(x) sort(x, decreasing=F)[2]))


# Stoppa in i dataram för 2001
litterDistance2001 <- as.data.frame(SminimumdenDistance2001@data$litterID)
colnames(litterDistance2001) <- "litterID"
litterDistance2001$distance <-  as.vector(SminimumdenDistance2001@data$structure.c.8667.09714956513..8667.09714956513....Names...c..55... )
litterDistance2001$year <-  SminimumdenDistance2001@data$year

#År 2002
SminimumdenDistance2002 <- cbind(lypositionerSHP[lypositionerSHP$year==2002,],
                                 lypositionerSHP[lypositionerSHP$year==2002,][minimumdenDistance2002,],
                                 apply(denDistance2002, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2002
litterDistance2002 <- as.data.frame(SminimumdenDistance2002@data$litterID)
colnames(litterDistance2002) <- "litterID"
litterDistance2002$distance <-  as.vector(SminimumdenDistance2002@data$structure.c.4974.73798304996..4295.23573276252..4974.73798304996.. )
litterDistance2002$year <-  SminimumdenDistance2002@data$year

#År 2003

#Bara en kull detta år. Därför blir det inget avstånd till någon annan kull.


#År 2004

SminimumdenDistance2004 <- cbind(lypositionerSHP[lypositionerSHP$year==2004,],
                                 lypositionerSHP[lypositionerSHP$year==2004,][minimumdenDistance2004,],
                                 apply(denDistance2004, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2004
litterDistance2004 <- as.data.frame(SminimumdenDistance2004@data$litterID)
colnames(litterDistance2004) <- "litterID"
litterDistance2004$distance <-  as.vector(SminimumdenDistance2004@data$structure.c.8152.76425514684..8152.76425514684..4295.23573276252..)
litterDistance2004$year <-  SminimumdenDistance2004@data$year

#År 2005

SminimumdenDistance2005 <- cbind(lypositionerSHP[lypositionerSHP$year==2005,],
                                 lypositionerSHP[lypositionerSHP$year==2005,][minimumdenDistance2005,],
                                 apply(denDistance2005, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2005
litterDistance2005 <- as.data.frame(SminimumdenDistance2005@data$litterID)
colnames(litterDistance2005) <- "litterID"
litterDistance2005$distance <-  as.vector(SminimumdenDistance2005@data$structure.c.6221.83067914902..8152.76425514684..4997.35780187891..)
litterDistance2005$year <-  SminimumdenDistance2005@data$year

#År 2006

#Bara en kull

#År 2007

SminimumdenDistance2007 <- cbind(lypositionerSHP[lypositionerSHP$year==2007,],
                                 lypositionerSHP[lypositionerSHP$year==2007,][minimumdenDistance2007,],
                                 apply(denDistance2007, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2007
litterDistance2007 <- as.data.frame(SminimumdenDistance2007@data$litterID)
colnames(litterDistance2007) <- "litterID"
litterDistance2007$distance <-  as.vector(SminimumdenDistance2007@data$structure.c.17742.8858982974..13502.2961010341..8043.57066233647..)
litterDistance2007$year <-  SminimumdenDistance2007@data$year


# År 2008
SminimumdenDistance2008 <- cbind(lypositionerSHP[lypositionerSHP$year==2008,],
                                 lypositionerSHP[lypositionerSHP$year==2008,][minimumdenDistance2008,],
                                 apply(denDistance2008, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2008
litterDistance2008 <- as.data.frame(SminimumdenDistance2008@data$litterID)
colnames(litterDistance2008) <- "litterID"
litterDistance2008$distance <-  as.vector(SminimumdenDistance2008@data$structure.c.15291.6309790683..2021.19222242715..4429.6732385132.. )
litterDistance2008$year <-  SminimumdenDistance2008@data$year

#År 2009

SminimumdenDistance2009 <- cbind(lypositionerSHP[lypositionerSHP$year==2009,],
                                 lypositionerSHP[lypositionerSHP$year==2009,][minimumdenDistance2009,],
                                 apply(denDistance2009, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2009
litterDistance2009 <- as.data.frame(SminimumdenDistance2009@data$litterID)
colnames(litterDistance2009) <- "litterID"
litterDistance2009$distance <-  as.vector(SminimumdenDistance2009@data$structure.c.11608.0242935652..11608.0242935652....Names...c..43... )
litterDistance2009$year <-  SminimumdenDistance2009@data$year


# År 2010
SminimumdenDistance2010 <- cbind(lypositionerSHP[lypositionerSHP$year==2010,],
                                 lypositionerSHP[lypositionerSHP$year==2010,][minimumdenDistance2010,],
                                 apply(denDistance2010, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2010
litterDistance2010 <- as.data.frame(SminimumdenDistance2010@data$litterID)
colnames(litterDistance2010) <- "litterID"
litterDistance2010$distance <-  as.vector(SminimumdenDistance2010@data$structure.c.15291.6309790683..6788.74399281634..7564.3324226266.. )
litterDistance2010$year <-  SminimumdenDistance2010@data$year

# År 2011
SminimumdenDistance2011 <- cbind(lypositionerSHP[lypositionerSHP$year==2011,],
                                 lypositionerSHP[lypositionerSHP$year==2011,][minimumdenDistance2011,],
                                 apply(denDistance2011, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2011
litterDistance2011 <- as.data.frame(SminimumdenDistance2011@data$litterID)
colnames(litterDistance2011) <- "litterID"
litterDistance2011$distance <-  as.vector(SminimumdenDistance2011@data$structure.c.3876.4808783225..15291.6309790683..3443.10673665514.. )
litterDistance2011$year <-  SminimumdenDistance2011@data$year

# År 2012

#Inga kullar

# År 2013

SminimumdenDistance2013 <- cbind(lypositionerSHP[lypositionerSHP$year==2013,],
                                 lypositionerSHP[lypositionerSHP$year==2013,][minimumdenDistance2013,],
                                 apply(denDistance2013, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2013
litterDistance2013 <- as.data.frame(SminimumdenDistance2013@data$litterID)
colnames(litterDistance2013) <- "litterID"
litterDistance2013$distance <-  as.vector(SminimumdenDistance2013@data$structure.c.3876.4808783225..3443.10673665514..2790.89412196163.. )
litterDistance2013$year <-  SminimumdenDistance2013@data$year

# År 2014
SminimumdenDistance2014 <- cbind(lypositionerSHP[lypositionerSHP$year==2014,],
                                 lypositionerSHP[lypositionerSHP$year==2014,][minimumdenDistance2014,],
                                 apply(denDistance2014, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2014
litterDistance2014 <- as.data.frame(SminimumdenDistance2014@data$litterID)
colnames(litterDistance2014) <- "litterID"
litterDistance2014$distance <-  as.vector(SminimumdenDistance2014@data$structure.c.5601.3849180359..3999.90112377794..3999.90112377794.. )
litterDistance2014$year <-  SminimumdenDistance2014@data$year


# År 2015
SminimumdenDistance2015 <- cbind(lypositionerSHP[lypositionerSHP$year==2015,],
                                 lypositionerSHP[lypositionerSHP$year==2015,][minimumdenDistance2015,],
                                 apply(denDistance2015, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2015
litterDistance2015 <- as.data.frame(SminimumdenDistance2015@data$litterID)
colnames(litterDistance2015) <- "litterID"
litterDistance2015$distance <-  as.vector(SminimumdenDistance2015@data$structure.c.5601.3849180359..2021.19222242715..5838.50468870241.. )
litterDistance2015$year <-  SminimumdenDistance2015@data$year

# År 2016

SminimumdenDistance2016 <- cbind(lypositionerSHP[lypositionerSHP$year==2016,],
                                 lypositionerSHP[lypositionerSHP$year==2016,][minimumdenDistance2016,],
                                 apply(denDistance2016, 1, function(x) sort(x, decreasing=F)[2]))


# Stoppa in i dataram för 2016
litterDistance2016 <- as.data.frame(SminimumdenDistance2016@data$litterID)
colnames(litterDistance2016) <- "litterID"
litterDistance2016$distance <-  as.vector(SminimumdenDistance2016@data$structure.c.8667.09714956513..8667.09714956513....Names...c..57... )
litterDistance2016$year <-  SminimumdenDistance2016@data$year

# År 2017
SminimumdenDistance2017 <- cbind(lypositionerSHP[lypositionerSHP$year==2017,],
                                 lypositionerSHP[lypositionerSHP$year==2017,][minimumdenDistance2017,],
                                 apply(denDistance2017, 1, function(x) sort(x, decreasing=F)[2]))


# Stoppa in i dataram för 2017
litterDistance2017 <- as.data.frame(SminimumdenDistance2017@data$litterID)
colnames(litterDistance2017) <- "litterID"
litterDistance2017$distance <-  as.vector(SminimumdenDistance2017@data$structure.c.3335.99835131854..12687.656521202..8826.28421250981.. )
litterDistance2017$year <-  SminimumdenDistance2017@data$year

# År 2018
SminimumdenDistance2018 <- cbind(lypositionerSHP[lypositionerSHP$year==2018,],
                                 lypositionerSHP[lypositionerSHP$year==2018,][minimumdenDistance2018,],
                                 apply(denDistance2018, 1, function(x) sort(x, decreasing=F)[2]))


# Stoppa in i dataram för 2017
litterDistance2018 <- as.data.frame(SminimumdenDistance2018@data$litterID)
colnames(litterDistance2018) <- "litterID"
litterDistance2018$distance <-  as.vector(SminimumdenDistance2018@data$structure.c.2879.34523807063..12687.656521202..2790.89412196163.. )
litterDistance2018$year <-  SminimumdenDistance2018@data$year




yearFrame$litterdistance <- paste("litterDistance", yearFrame$yearlist, sep="")

litterDistanceTotal <- rbind(litterDistance2001, litterDistance2002, litterDistance2004, litterDistance2005, litterDistance2007,
                             litterDistance2008, litterDistance2009, litterDistance2010, litterDistance2011, litterDistance2013, litterDistance2014, litterDistance2015, 
                             litterDistance2016, litterDistance2017, litterDistance2018)


litterDistanceTotal
# Och så spottar vi ur oss filen. Tjohoo!
write_xlsx(litterDistanceTotal, "Den and territory selection/Rawdata/distans närmsta förynging.xlsx")



