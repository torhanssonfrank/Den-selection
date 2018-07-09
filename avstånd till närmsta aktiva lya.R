install.packages("maptools")
library(maptools)
library(sp)
library(rgeos)
library(rgdal)

# DET STÄMMER INTE! DET ÄR FÄRRE KULLAR I RASMUS.KULLDATA TILL TOR ÄN I 
# Lypositioner kullar 2000-2017 SWEREF99 per kull.csv FAST ALLA OMATADE KULLAR SKA VARA 
# BORTTAGNA UR DEN FILEN. DET ÄR DÄRFÖR DET BLIR FEL I SLUTET.
lyorperkull<-read.csv(file.choose(), stringsAsFactors = FALSE)

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
rasmuskulldata <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE) #den inlästa filen är Rasmus.Kulldata till Tor.csv
rasmuskulldata
View(rasmuskulldata)


lypositionerfull <- read.csv(file.choose(), header = TRUE, sep = ";", stringsAsFactors = FALSE) #den inlästa filen är Lypositioner Rovbasen Helags SWEREF99.csv 
View(lypositionerfull)
lypositioner<- subset(lypositionerfull, select = c(denNr, north, east)) #tar ut de kolumner jag behöver
View(lypositioner)
lypositionerKull<- merge(lypositioner, rasmuskulldata, by="denNr") #funkade bra! Trots att "lypositioner" bara har en rad per år parar merge ihop alla kullar med rätt north och east.####
View(lypositionerKull)

lypositionerKull2<- merge(lypositioner, rasmuskulldata, by="denNr") #försöker felsöka, behövs inte i slutgiltiga versionen

lypositionerKull<-subset(lypositionerKull, select = c("denNr","north","east","year","litterID")) #plockar ut de kolumner jag behöver
View(lypositionerKull)


coordinates(lypositionerKull) <- c("east", "north") #öst är alltid före norr i sp-klasser. Funktionen coordinates talar om för R i vilka kolumner koordinaterna finns####
summary(lypositionerKull) # dataramen är inte projected (inte i meter), och har inget koordinatsystem.

# lägger in SWEREF 99 som koordinatsystem
proj4string(lypositionerKull) <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
summary(lypositionerKull) # nu är den projected (i meter) och har koordinatsystem (SWEREF 99). Då 
# går det att mäta avstånd

# ändrade om working directory till mappen "Masterarbete" så att 
# jag inte sparar gps-punkterna i mappen som ska upp på github

# nu när jag har talat om för R vilka kolumner som innehåller koordinater och vilket
# koordinatsystem som gäller kan jag printa ut en shapefil.
writeOGR(lypositionerKull, dsn ="./Lyor, gps-punkter och avstånd/lypositionerKullTor.shp", layer = "lypositionerKull", driver = "ESRI Shapefile")

#nu kan jag läsa in shapefilen igen så att jag kan mäta####

lypositionerSHP<-readOGR(file.choose()) # läser alltså in lypositionerKullTor.shp. Märk att R sparar om east som coords.x1 och north som coords.x2
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

# Utifrån dessa dejtafrejmar (separerade per år) vill vi nu läsa ut det näst längsta avstånet (eftersom det första avståndet är
#  till punkten själv, och det är naturligtvis noll.) Rasmus försökte automatisera, men det var som förgjort.
# Här lånar jag Rasmus skript rakt av. Antar att [2] anger andra värdet. Att decreasing är 
# satt till FALSE antar jag betyder att andra värdet i stigande led väljs. I filen Rasmus.kulldata till Tor
# saknas år 2000, därför fungerar inte första raden. Det saknas även ett par andra år. Lägg till
# dem i csv-filen när skriptet är klart och kör det igen. Jag kan även lägga till 2018 när sommarens
# inventering är klar om det hinns med.



minimumdenDistance2000 <- apply(denDistance2000, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2001 <- apply(denDistance2001, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2002 <- apply(denDistance2002, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2004 <- apply(denDistance2004, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2005 <- apply(denDistance2005, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2007 <- apply(denDistance2007, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2008 <- apply(denDistance2008, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2010 <- apply(denDistance2010, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2011 <- apply(denDistance2011, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2013 <- apply(denDistance2013, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2014 <- apply(denDistance2014, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2015 <- apply(denDistance2015, 1, function(x) order(x, decreasing=F)[2])
minimumdenDistance2017 <- apply(denDistance2017, 1, function(x) order(x, decreasing=F)[2])

# Härifrån stjäl jag Rasmus skript rakt av.

# Använde find-funktionen (cmd + F) för att ersätta 
# Rasmus shapefil-namn LypositionerHEFKullshp med lypositionerSHP (som jag sparade shapefilen som)
# vad gör kommandot @data$structure.c.8667....?

# Och så vill vi slå ihop datat. Vi fyller på en dejtafrejm.
litterDistance <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(litterDistance) <- c("litterID", "distance", "year")


SminimumdenDistance2001 <- cbind(lypositionerSHP[lypositionerSHP$year==2001,],
                                 lypositionerSHP[lypositionerSHP$year==2001,][minimumdenDistance2001,],
                                 apply(denDistance2001, 1, function(x) sort(x, decreasing=F)[2]))

SminimumdenDistance2001
litterDistance2001
# Stoppa in i dataram för 2001
litterDistance2001 <- as.data.frame(SminimumdenDistance2001@data$litterID)
colnames(litterDistance2001) <- "litterID"
litterDistance2001$distance <-  as.vector(SminimumdenDistance2001@data$structure.c.8667.09714956513..8667.09714956513.)
litterDistance2001$year <-  SminimumdenDistance2001@data$year


SminimumdenDistance2002 <- cbind(lypositionerSHP[lypositionerSHP$year==2002,],
                                 lypositionerSHP[lypositionerSHP$year==2002,][minimumdenDistance2002,],
                                 apply(denDistance2002, 1, function(x) sort(x, decreasing=F)[2]))
SminimumdenDistance2002
# Stoppa in i dataram för 2002
litterDistance2002 <- as.data.frame(SminimumdenDistance2002@data$litterID)
colnames(litterDistance2002) <- "litterID"
litterDistance2002$distance <-  as.vector(SminimumdenDistance2002@data$structure.c.4974.73798304996..4974.73798304996..5948.64060101129/1000)
litterDistance2002$year <-  SminimumdenDistance2002@data$year


SminimumdenDistance2004 <- cbind(lypositionerSHP[lypositionerSHP$year==2004,],
                                 lypositionerSHP[lypositionerSHP$year==2004,][minimumdenDistance2004,],
                                 apply(denDistance2004, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2004
litterDistance2004 <- as.data.frame(SminimumdenDistance2004@data$litterID)
colnames(litterDistance2004) <- "litterID"
litterDistance2004$distance <-  as.vector(SminimumdenDistance2004@data$structure.c.8152.76425514684..8152.76425514684..4295.23573276252../1000)
litterDistance2004$year <-  SminimumdenDistance2004@data$year


SminimumdenDistance2005 <- cbind(lypositionerSHP[lypositionerSHP$year==2005,],
                                 lypositionerSHP[lypositionerSHP$year==2005,][minimumdenDistance2005,],
                                 apply(denDistance2005, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2005
litterDistance2005 <- as.data.frame(SminimumdenDistance2005@data$litterID)
colnames(litterDistance2005) <- "litterID"
litterDistance2005$distance <-  as.vector(SminimumdenDistance2005@data$structure.c.8152.76425514684..8152.76425514684..8187.89014337637../1000)
litterDistance2005$year <-  SminimumdenDistance2005@data$year

SminimumdenDistance2007 <- cbind(lypositionerSHP[lypositionerSHP$year==2007,],
                                 lypositionerSHP[lypositionerSHP$year==2007,][minimumdenDistance2007,],
                                 apply(denDistance2007, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2007
litterDistance2007 <- as.data.frame(SminimumdenDistance2007@data$litterID)
colnames(litterDistance2007) <- "litterID"
litterDistance2007$distance <-  as.vector(SminimumdenDistance2007@data$structure.c.13503.1344879624..8043.57066233647..8152.76425514684../1000)
litterDistance2007$year <-  SminimumdenDistance2007@data$year

SminimumdenDistance2008 <- cbind(lypositionerSHP[lypositionerSHP$year==2008,],
                                 lypositionerSHP[lypositionerSHP$year==2008,][minimumdenDistance2008,],
                                 apply(denDistance2008, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2008
litterDistance2008 <- as.data.frame(SminimumdenDistance2008@data$litterID)
colnames(litterDistance2008) <- "litterID"
litterDistance2008$distance <-  as.vector(SminimumdenDistance2008@data$structure.c.2021.19222242715..4429.6732385132..4429.6732385132../1000)
litterDistance2008$year <-  SminimumdenDistance2008@data$year

SminimumdenDistance2010 <- cbind(lypositionerSHP[lypositionerSHP$year==2010,],
                                 lypositionerSHP[lypositionerSHP$year==2010,][minimumdenDistance2010,],
                                 apply(denDistance2010, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2010
litterDistance2010 <- as.data.frame(SminimumdenDistance2010@data$litterID)
colnames(litterDistance2010) <- "litterID"
litterDistance2010$distance <-  as.vector(SminimumdenDistance2010@data$structure.c.17742.8858982974..6788.74399281634..7564.95128867331../1000)
litterDistance2010$year <-  SminimumdenDistance2010@data$year

SminimumdenDistance2011 <- cbind(lypositionerSHP[lypositionerSHP$year==2011,],
                                 lypositionerSHP[lypositionerSHP$year==2011,][minimumdenDistance2011,],
                                 apply(denDistance2011, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2011
litterDistance2011 <- as.data.frame(SminimumdenDistance2011@data$litterID)
colnames(litterDistance2011) <- "litterID"
litterDistance2011$distance <-  as.vector(SminimumdenDistance2011@data$structure.c.3876.4808783225..16446.3916103199..3443.10673665514../1000)
litterDistance2011$year <-  SminimumdenDistance2011@data$year

SminimumdenDistance2013 <- cbind(lypositionerSHP[lypositionerSHP$year==2013,],
                                 lypositionerSHP[lypositionerSHP$year==2013,][minimumdenDistance2013,],
                                 apply(denDistance2013, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2013
litterDistance2013 <- as.data.frame(SminimumdenDistance2013@data$litterID)
colnames(litterDistance2013) <- "litterID"
litterDistance2013$distance <-  as.vector(SminimumdenDistance2013@data$structure.c.3876.4808783225..3443.10673665514..2790.89412196163../1000)
litterDistance2013$year <-  SminimumdenDistance2013@data$year

SminimumdenDistance2014 <- cbind(lypositionerSHP[lypositionerSHP$year==2014,],
                                 lypositionerSHP[lypositionerSHP$year==2014,][minimumdenDistance2014,],
                                 apply(denDistance2014, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2014
litterDistance2014 <- as.data.frame(SminimumdenDistance2014@data$litterID)
colnames(litterDistance2014) <- "litterID"
litterDistance2014$distance <-  as.vector(SminimumdenDistance2014@data$structure.c.5601.3849180359..3999.90112377794..3999.90112377794../1000)
litterDistance2014$year <-  SminimumdenDistance2014@data$year

SminimumdenDistance2015 <- cbind(lypositionerSHP[lypositionerSHP$year==2015,],
                                 lypositionerSHP[lypositionerSHP$year==2015,][minimumdenDistance2015,],
                                 apply(denDistance2015, 1, function(x) sort(x, decreasing=F)[2]))

# Stoppa in i dataram för 2015
litterDistance2015 <- as.data.frame(SminimumdenDistance2015@data$litterID)
colnames(litterDistance2015) <- "litterID"
litterDistance2015$distance <-  as.vector(SminimumdenDistance2015@data$structure.c.5601.3849180359..2021.19222242715..5838.99186503972../1000)
litterDistance2015$year <-  SminimumdenDistance2015@data$year

SminimumdenDistance2017 <- cbind(lypositionerSHP[lypositionerSHP$year==2017,],
                                 lypositionerSHP[lypositionerSHP$year==2017,][minimumdenDistance2017,],
                                 apply(denDistance2017, 1, function(x) sort(x, decreasing=F)[2]))


# Stoppa in i dataram för 2017
litterDistance2017 <- as.data.frame(SminimumdenDistance2017@data$litterID)
colnames(litterDistance2017) <- "litterID"
litterDistance2017$distance <-  as.vector(SminimumdenDistance2017@data$structure.c.3326.4865849722..12687.656521202..8826.57011528261../1000)
litterDistance2017$year <-  SminimumdenDistance2017@data$year

yearFrame$litterdistance <- paste("litterDistance", yearFrame$yearlist, sep="")

litterDistanceTotal <- rbind(litterDistance2001, litterDistance2002, litterDistance2004, litterDistance2005, litterDistance2007,
                             litterDistance2008, litterDistance2010, litterDistance2011, litterDistance2013, litterDistance2014, litterDistance2015,
                             litterDistance2017)
#vissa lyor har inte fått något avstånd, därför funkar det inte att slå ihop dem.
# det blir problem med raderna! Mina är inte exakt samma som Rasmus eftersom jag använder omatade
# kullar också. se till att siffrorna efter structure.c. är samma som i dataramen 
# SminimumdenDistance20XX. De siffrorna är avståndet i meter och antalet avstånd måste stämma med
# antalet kullar varje år.

# Och så spottar vi ur oss filen. Tjohoo!
write.csv(litterDistanceTotal, ":/Den and territory selection/Rawdata/distansnärmstaförynging.csv", row.names = F)



