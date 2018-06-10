library("sp")
library("rgdal")
library("rgeos")
library(dplyr)
install.packages("xlsx")
library(writexl)

lyor  <- readOGR(dsn = file.choose(), layer = "lyor", stringsAsFactors = FALSE)

proj4string(lyor) <- CRS("+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs")
summary(lyor) #den är projected redan!####
plot(lyor, col ="red", pch = ".", cex = 3)


#I grönt är en alternativ metod om man inte lägger ihop shapefilerna i qgis innan man importerar dem
# i R. Subset gör att man kan plocka ut de features man är intresserad av (förutsatt att det finns
# features). Av någon anledning hängde sig R hela tiden när jag försökte lägga ihop skog_sverige och 
# skog_norge. Jag sket i det och la ihop allt i qgis istället och importerade en shapefil för all skog####
#vegfjall <- readOGR(dsn = file.choose(), layer = "vegfjall", stringsAsFactors = FALSE) 
#summary(vegfjall) #också projected och  har samma proj4string (koordinatsystem/CRS) som lyor####
#View(vegfjall)
#jag är bara intresserad av skogen, inte av myrar och annat. Därför måste jag plocka ut skogsfeatures####
#svensk_skog <- subset(vegfjall, VEGETATION == "Barrskog, lavristyp" | VEGETATION == "Barrskog, lavtyp" | VEGETATION == "Fuktig-våt barrskog" | VEGETATION == "Lavmarksbarrskog" | VEGETATION == "Lavmarkslövskog" | VEGETATION == "Mossmarksbarrskog" | VEGETATION == "Mossmarkslövskog" | VEGETATION == "Sumplövskog" | VEGETATION == "Torr-frisk barrskog")
#summary(svensk_skog)
#norsk_skog<- readOGR(dsn = file.choose(), layer = "skog_norge", stringsAsFactors = FALSE)
#summary(norsk_skog)
#skog <-gUnion(svensk_skog, norsk_skog)
#summary(skog)

skog <- readOGR(dsn = file.choose(), layer = "skog_sverigenorge", stringsAsFactors = FALSE)
summary(skog) #samma proj4strings som lyor. Den är projected####

vatten <- readOGR(dsn = file.choose(), layer = "vatten_sverigenorge", stringsAsFactors = FALSE)
summary(vatten) #samma proj4strings som lyor. Den är projected####



plot(skog, col = "green")
plot(alla_vatten, col = "royalblue1")
points(lyor, col = "red", pch =".", cex = 3)

dist_vatten<-apply(gDistance(lyor, vatten,byid=TRUE),2,min)
View(dist_vatten) #lynamnen kommer inte med men lyorna verkar ligga i samma ordning som i "lyor"-data frame#####

dist_vatten<-as.data.frame(dist_vatten) #gör om från Spatial till vanlig data frame####
colnames(dist_vatten) <- ("distans_till_vatten") #lägger till kolumnnamn

dist_skog<-apply(gDistance(lyor, skog,byid=TRUE),2,min) #små trädplättar på kalfjället borttagna
View(dist_skog)
dist_skog<-as.data.frame(dist_skog)
colnames(dist_skog) <-("distans_till_skog")




lyor_data <- as.data.frame(lyor) #gör om från Spatial till vanlig data frame
summary(lyor_data)
class(lyor_data)
lyor_data


lyor_data<-lyor_data %>% 
  bind_cols(dist_vatten, dist_skog)  #lägger till distans till datasetet

lyor_data

View(lyor_data)

write_xlsx(lyor_data, path = "GIS-data/lyor_distans_vatten_skog.xlsx") #sparar den som excel-fil istället för csv så blir den enklare att manipulera i excel. Skrev ut den till GIS-data-mappen men flyttade den sen till lyor, gpspunkter och avstånd####


