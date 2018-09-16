
# Printar ut ett år med kullar för att kolla i QGIS om kullarna är nära rödrävar
library(readxl)


kullar <- read_xlsx(path ="Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx") 
lypositionerfull <- readOGR(dsn = "Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.shp", stringsAsFactors = FALSE)
summary(lypositionerfull)

lypositionerDF <- as.data.frame(lypositionerfull)
lypositionerDF

lypositionerDF<-lypositionerDF %>% 
  select(Namn, E, N)

kullar<- kullar %>%
  left_join(lypositionerDF, by = "Namn")
View(kullar)

kullar$E<-as.numeric(kullar$E)
kullar$N<-as.numeric(kullar$N)

kull2007 <- kullar %>% 
  filter(År == 2007)

coordinates(kull2007) <- c("E", "N")
proj4string(kull2007) <- CRS("+init=EPSG:3006")
summary(kull2007)
plot(kull2007)

writeOGR(kull2007, dsn ="./Lyor, kullar, gps-punkter, yta och avstånd/kullar2007.shp", layer = "kull2007", driver = "ESRI Shapefile")
