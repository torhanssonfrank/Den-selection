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

red_fox_2000_2012 <- read_xlsx("Lyor, kullar, gps-punkter, yta och avstånd/Helags_Red_Fox_Feeding.xlsx", sheet = 11)
red_fox_2000_2012_copy <- read_xlsx("Lyor, kullar, gps-punkter, yta och avstånd/Helags_Red_Fox_Feeding.xlsx", sheet = 11)
View(red_fox_2000_2012_copy)

head(red_fox_2000_2012)
names(red_fox_2000_2012)
View(red_fox_2000_2012)

red_fox_2000_2012 <- red_fox_2000_2012 %>% 
  select(Year, Date, Easting90, Northing90) #tar 90 här istället för sweref som också är inlagt så allt blir samma
head(red_fox_2000_2012)

red_fox_2013 <- read_xls("Skript och rödrävsdata från Rasmus/Rödrävsjakt Helags 2012-2013.xls") #går inte öppna de här filerna i exel av någon anledning
View(red_fox_2013)
head(red_fox_2013) #koordinatsystemet är 90. Inte sweref 99. Måste ändra i qgis.
names(red_fox_2013)
#tar bort översta raden eftersom den inte innehåller data
red_fox_2013 <- red_fox_2013 %>% 
  slice(-1)
View(red_fox_2013$Datum) # finns två från 2012 här och nio utan år eller datum. Kan dubbelkolla koordinaterna med Peters fil

#' finns två likadana ostkoordinater i båda filerna men inga likadana nordkoordinater.
#' Det är alltså inte samma skjutna rödrävar i 2000_2012 som red_fox_2013. Kollade i listan och 
#' de i 2000_2012 är från 2008 och 2006.
dubletter_rödräv <- red_fox_2013[red_fox_2013$`O koord` %in% red_fox_2000_2012$Easting90,]
dubletter_rödräv #två överlapp
dubletter_rödräv2 <- red_fox_2013[red_fox_2013$`N koord` %in% red_fox_2000_2012$Northing90,]
dubletter_rödräv2 #tom
View(red_fox_2013)
View(red_fox_2000_2012)
rm(dubletter_rödräv, dubletter_rödräv2)

# jag antar att de som inte har datum i red_fox_2013 är från 2013. Dubbelkolla med Lars
red_fox_2013$Datum[17:25] <- 2013



#läser in efterföljande år
red_fox_2014_2015 <- read_xls("Skript och rödrävsdata från Rasmus/Rödrävsjakt Helags 2014-2015 (2).xls")
View(red_fox_2014_2015)
names(red_fox_2014_2015) #samma kolumner som 2013

View(red_fox_2014_2015$Datum) #alla har år inlagt i alla fall. Vissa saknar månad och dag
#tar bort översta raden eftersom den inte innehåller data
red_fox_2014_2015 <- red_fox_2014_2015 %>% 
  slice(-1)

#' eftersom kolumnnamnen är samma kan jag lägga ihop dem direkt
#' och plocka ut kolumnerna jag behöver efteråt så behöver jag bara
#' göra det en gång

red_fox_2013_2015 <- bind_rows(red_fox_2013, red_fox_2014_2015)
head(red_fox_2013_2015)

red_fox_2013_2015 <- red_fox_2013_2015 %>% 
  select (Datum, `O koord`, `N koord`)



#' kom inte på ett sätt att bara ta ut en kolumn med år ur datum så jag gör två och slänger en.
#' remove = FALSE gör att Datum-kolumnen behålls

red_fox_2013_2015 <- red_fox_2013_2015 %>% 
  separate(Datum, c("Year", "ta bort"), sep = 4, remove = FALSE)

head(red_fox_2013_2015)
red_fox_2013_2015<-red_fox_2013_2015 %>% 
  select(- `ta bort`)
View(red_fox_2013_2015)

#'några rader har inget datum, bara år. Därför
#'ligger det bara ett årtal där istället för datum. Sätter NA istället
red_fox_2013_2015$Datum[17:27] <- NA

# nu ska jag lägga ihop red_fox_2013_2015 med red_fox_2000_2012. De är alla i koordinatformat 90.
names(red_fox_2013_2015)
names(red_fox_2000_2012)
red_fox_2013_2015 <- red_fox_2013_2015[c("Year", "Datum", "O koord", "N koord")] #byter plats på kolumner
class(red_fox_2013_2015$Year)
class(red_fox_2000_2012$Year)

red_fox_2013_2015$Year <- as.numeric(red_fox_2013_2015$Year)

class(red_fox_2013_2015$Datum)
class(red_fox_2000_2012$Date)
red_fox_2013_2015$Datum
View(red_fox_2000_2012$Date)

red_fox_2013_2015<- red_fox_2013_2015 %>% 
  mutate(Datum = ymd(Datum))
View(red_fox_2013_2015)
class(red_fox_2013_2015$Datum)

red_fox_2000_2012 <- red_fox_2000_2012 %>% 
  mutate(Date = ymd(Date))

View(red_fox_2000_2012)
class(red_fox_2000_2012$Date)


colnames(red_fox_2013_2015) <- c("Year","Date","Easting90","Northing90") #byter namn så att det är samma som red_fox_2000_2012


red_fox_2000_2015 <- bind_rows(red_fox_2000_2012, red_fox_2013_2015) 

View(red_fox_2000_2015)

#' gör om till en spatial - fil. Gör en kopia av dataramen först så att jag kan leka runt utan att
#' paja orginalet

red_fox_copy <- cbind(red_fox_2000_2015)
View(red_fox_copy)

coordinates(red_fox_copy) <- c("Easting90", "Northing90")

summary(red_fox_copy) #inte projected än

#' Koordinaterna är angedda i det gamla svenska koordinatsystemet
#' RT90. Jag hittade koden i QGIS genom att högerklicka på ett lager och välja
#' set CRS. Där kan man söka upp olika koordinatsystem. Där finner man också koden.
proj4string(red_fox_copy) <- CRS("+init=EPSG:3021")



summary(red_fox_copy) #RT90 och projected

CRS.new <- CRS("+init=EPSG:3006") #sparar SWEREFF99 i ett objekt. Den långa raddan jag använde innan var fel! Den här förkortningen funkar.

red_fox_copy_sweref <- spTransform(red_fox_copy, CRS.new) #nu är koordinaterna i SWEREF! KING!
plot(red_fox_copy_sweref, col = "red")


summary(red_fox_copy_sweref)

red_fox_2000_2015_sweref<-as.data.frame(red_fox_copy_sweref)

View(red_fox_2000_2015_sweref)
head(red_fox_2000_2015_sweref)

#' följande år
# red_fox_2016_2017 <- read_xls("Skript och rödrävsdata från Rasmus/Rödrävsjakt 2016-2017.xls")
# View(red_fox_2016_2017) #den här filen är tom så tar bort den!
# rm(red_fox_2016_2017)


# koordinaterna i de nedre filerna är inlagda som sweref99
red_fox_2016 <- read_xlsx("Skript och rödrävsdata från Rasmus/Rödräv 2016.xlsx")
View(red_fox_2016)

#tar bort översta raden eftersom den inte innehåller data
red_fox_2016 <- red_fox_2016 %>% 
  slice(-1)

names(red_fox_2016)
colnames(red_fox_2016)[4] <- "O koord" #ost-koordinaterna var döpta till E koord i den här. Heter O koord i de andra


red_fox_2016_Alf <- read_xls("Skript och rödrävsdata från Rasmus/Rödräv_rapportAlf16.xls")
View(red_fox_2016_Alf)
#tar bort översta raden eftersom den inte innehåller data
red_fox_2016_Alf <- red_fox_2016_Alf %>% 
  slice(-1)

#' red_fox_2016 och red_fox_2016_Alf har samma kolumner så slänger
#' ihop dem här. De har koordinater i sweref99

red_fox_2016_sweref<- bind_rows(red_fox_2016, red_fox_2016_Alf )
head(red_fox_2016_sweref)

#'plockar ut de kolumner
#' jag behöver

red_fox_2016_sweref <- red_fox_2016_sweref %>% 
  select(Datum, `O koord`, `N koord`)

red_fox_2016_2017_Mathias <- read_xls("Skript och rödrävsdata från Rasmus/Rödrävsjakt Mathias 2016-2017.xls")
View(red_fox_2016_2017_Mathias)
#tar bort översta raden eftersom den inte innehåller data
red_fox_2016_2017_Mathias <- red_fox_2016_2017_Mathias %>% 
  slice(-1)

#' red_fox_2016_2017_Mathias har inte samma kolumner som red_fox_2016_Alf och
#'  red_fox_2016 så jag plockar ut dem separat och lägger sen in dem
#'  i red_fox_2016_sweref

red_fox_2016_2017_Mathias <- red_fox_2016_2017_Mathias %>% 
  select(Datum, `O koord`, `N koord`)
head(red_fox_2016_2017_Mathias)

#' lägger ihop Mathias med sweref

red_fox_2016_sweref <- bind_rows(red_fox_2016_sweref, red_fox_2016_2017_Mathias)
View(red_fox_2016_sweref)

red_fox_2016_sweref$Datum[5] <- 20161214 #den radens datum stod som 161214

red_fox_2016_sweref$Datum[5]



#' kom inte på ett sätt att bara ta ut en kolumn med år ur datum så jag gör två och slänger en.
#' remove = FALSE gör att Datum-kolumnen behålls

red_fox_2016_sweref <- red_fox_2016_sweref %>% 
  separate(Datum, c("Year", "ta bort"), sep = 4, remove = FALSE)

head(red_fox_2016_sweref)
red_fox_2016_sweref<-red_fox_2016_sweref %>% 
  select(- `ta bort`)
head(red_fox_2016_sweref)

#Och sen lägger vi ihop red_fox_2000_2015_sweref med red_fox_2016_sweref

names(red_fox_2000_2015_sweref)

names(red_fox_2016_sweref)

red_fox_2016_sweref <- red_fox_2016_sweref[c("Year", "Datum", "O koord", "N koord")] #byter plats på kolumner
head(red_fox_2016_sweref)
class(red_fox_2016_sweref$Year)
red_fox_2016_sweref$Year<-as.numeric(red_fox_2016_sweref$Year)
class(red_fox_2016_sweref$Datum)

red_fox_2016_sweref<-red_fox_2016_sweref %>% 
  mutate(Datum = ymd(Datum))


#byter namn så att de har samma
colnames(red_fox_2016_sweref) <- c("Year","Date","E","N")
colnames(red_fox_2000_2015_sweref) <- c("Year","Date","E","N")

View(red_fox_2016_sweref)# fattas koordinater på en rad. Tar bort den
red_fox_2016_sweref<-red_fox_2016_sweref %>% 
  slice(-3)
#och lägger ihop dem

red_fox_2000_2016 <- bind_rows(red_fox_2000_2015_sweref,red_fox_2016_sweref)
View(red_fox_2000_2016)
warnings() # "Datum"- kolumnen spökar. Kollade upp det och det är en bugg
red_fox_2000_2016$Year[466] <-2013 #Hittade ett fel. Ett år låg inne som 2113.
red_fox_2000_2016$Date[466] <- ymd("2013-04-26") #måste vara en string för att den ska kunna omvandlas till date.
red_fox_2000_2016$Date[466]
class(red_fox_2000_2016$Date[466])

#printar ut en excelfil först
write_xlsx(red_fox_2000_2016, path = "Skript och rödrävsdata från Rasmus/red_fox_2000_2016_sweref.xlsx")

#Gör en spatialfil
coordinates(red_fox_2000_2016) <- c("E", "N") #vissa saknar koordinater. Måste ta bort de raderna.


summary(red_fox_2000_2016) #inte projected än

proj4string(red_fox_2000_2016) <- CRS("+init=EPSG:3006")
summary(red_fox_2000_2016) #sweref och projected
plot(red_fox_2000_2016)

writeOGR(red_fox_2000_2016, dsn ="./Lyor, kullar, gps-punkter, yta och avstånd/red_fox_2000_2016_sweref", layer = "red_fox_2000_2016", driver = "ESRI Shapefile")

