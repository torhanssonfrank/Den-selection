library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(tibble)


#jag vill ha ut sannolikheten för lämmel i varje pixel inom en 1500 meter radie runt lyan.
#förklaring här: https://www.neonscience.org/extract-values-rasters-r

lemmel_topp <- raster("Gnagardata/Lämmelprediktion toppår.tif")
lemmel_topp@crs #kollar koordinatsystem. Den är sweref

lemmel_uppgång <- raster("Gnagardata/Lämmelprediktion uppgångsår.tif")
lemmel_uppgång@crs #kollar koordinatsystem. Den är sweref

summary(lyor) # är också sweref. Läste in den här dataramen i annat skript.
  
medelvärde_topp <- extract(lemmel_topp,             # raster layer
                      lyor,   # SPDF with centroids for buffer
                      buffer = 1500,     # buffer size, units depend on CRS
                      fun=mean,         # what value to extract. 
                      df=TRUE)         # return a dataframe? 
medelvärde_uppgång <- extract(lemmel_uppgång,             # raster layer
                  lyor,   # SPDF with centroids for buffer
                  buffer = 1500,     # buffer size, units depend on CRS
                  fun=mean,         # what value to extract. 
                  df=TRUE)         # return a dataframe? 

#sparar om dem som vektorer med bättre namn. Sen skapar jag en dataram och lägger in dem.
medelvärde_lämmelprediktion_toppår <- medelvärde_topp$La.mmelprediktion_toppa.r  
medelvärde_lämmelprediktion_uppgångsår <- medelvärde_uppgång$La.mmelprediktion_uppga.ngsa.r

lemmel <- data.frame(medelvärde_lämmelprediktion_toppår = medelvärde_lämmelprediktion_toppår,
                     medelvärde_lämmelprediktion_uppgångsår = medelvärde_lämmelprediktion_uppgångsår, stringsAsFactors = FALSE)
lemmel

#Lägger in lynamnen så att varje värde har en rad med rätt lya
lemmel$Namn <- lyor$Namn

head(lemmel)

#byter plats på kolumnerna
lemmel <- lemmel[c("Namn","medelvärde_lämmelprediktion_uppgångsår", "medelvärde_lämmelprediktion_toppår")] 


#nu vill jag ha värdena för lämmelsannolikhet för varje pixel inom en 1500 m buffer runt lyan
lemmel_lista_topp <- extract(lemmel_topp,             # raster layer
                  lyor,   # SPDF with centroids for buffer
                  buffer = 1500,     # buffer size, units depend on CRS
                  df=TRUE)         # return a dataframe? 
head(lemmel_lista_topp)

lemmel_lista_uppgång <- extract(lemmel_uppgång,             # raster layer
                             lyor,   # SPDF with centroids for buffer
                             buffer = 1500,     # buffer size, units depend on CRS
                             df=TRUE)            # return a dataframe?
head(lemmel_lista_uppgång)



class(lemmel_lista_uppgång) #blir en matrix av någon anledning
lemmel_lista_uppgång <- as.data.frame(lemmel_lista_uppgång)
class(lemmel_lista_uppgång$ID)

# tog svinlång tid att hitta en lösning på att lägga in rätt lynamn på alla rader. När lemmel_lista_uppgång
# skapas så behålls inte lyornas namn. Det läggs in default-namn istället. Den kolumnen heter ID och är bara nummer
# 1 till 78 (78 lyor). Eftersom det är svinmånga rader som ska ha samma lynamn funkar inte cbind. Man måste tala 
# om för r vilka siffror som ska ha vilket namn. Det gör man genom att föklara att ID-numren är factor levels.
# man behöver inte skriva ut alla (1:78 räcker). För att slippa skriva ut alla lynamn med citationstecken och komma
# kan man använda paste så gör r det automatiskt. Det funkar inte att bara skriva lyor$Namn. Lynamnen blir faktorer.
# Hoppas det inte blir problem.
lemmel_lista_uppgång$Namn<-factor(lemmel_lista_uppgång$ID, levels = c(1:78),
       labels = c(paste(lyor$Namn)))

lemmel_lista_uppgång
class(lemmel_lista_uppgång$Namn)

names(lemmel_lista_uppgång) <- c("ID","lämmelprediktion_uppgångsår", "Namn")

head(lemmel_lista_uppgång)

lemmel_lista_uppgång %>% 
  select(-ID) %>%
  head()

#byter plats på kolumnerna
lemmel_lista_uppgång <- lemmel_lista_uppgång[c("Namn","lämmelprediktion_uppgångsår")] 
head(lemmel_lista_uppgång)

#Gör samma sak för toppår
class(lemmel_lista_topp)
lemmel_lista_topp<- as.data.frame(lemmel_lista_topp)

lemmel_lista_topp$Namn<-factor(lemmel_lista_topp$ID, levels = c(1:78),
                                  labels = c(paste(lyor$Namn)))
lemmel_lista_topp

names(lemmel_lista_topp) <- c("ID","lämmelprediktion_toppår", "Namn")

head(lemmel_lista_topp)

lemmel_lista_topp %>% 
  select(-ID) %>%
  head()

#byter plats på kolumnerna
lemmel_lista_topp <- lemmel_lista_topp[c("Namn","lämmelprediktion_toppår")] 
head(lemmel_lista_topp)

max(lemmel_lista_uppgång$lämmelprediktion_uppgångsår)
which(lemmel_lista_topp$Namn == "FSZZ008")
max(lemmel_lista_topp$lämmelprediktion_toppår)

