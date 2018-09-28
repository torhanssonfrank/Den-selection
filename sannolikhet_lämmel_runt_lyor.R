library(raster)
library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(tibble)
library(writexl)
library(stringr)



#jag vill ha ut sannolikheten för lämmel i varje pixel inom en 1500 meter radie runt lyan.
#förklaring här: https://www.neonscience.org/extract-values-rasters-r

lemmel_topp <- raster("Gnagardata/Lämmelprediktion toppår.tif")
lemmel_topp@crs #kollar koordinatsystem. Den är sweref

lemmel_uppgång <- raster("Gnagardata/Lämmelprediktion uppgångsår.tif")
lemmel_uppgång@crs #kollar koordinatsystem. Den är sweref

lyor  <- readOGR(dsn = "./Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.shp", layer = "Lyor helags alla", stringsAsFactors = FALSE)
summary(lyor) # är också sweref. 
  
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

head(lemmel)
View(lemmel)

median(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE)
mean(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE)
max(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE)


which(is.na(lemmel$medelvärde_lämmelprediktion_uppgångsår))
which(is.na(lemmel$medelvärde_lämmelprediktion_toppår))

write_xlsx(lemmel, path = "Den and territory selection/Rawdata/lämmelprediktion_medelvärde_topp_uppgång.xlsx")

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
max(lemmel_lista_uppgång$La.mmelprediktion_uppga.ngsa.r)


class(lemmel_lista_uppgång) #blir en matrix av någon anledning
lemmel_lista_uppgång <- as.data.frame(lemmel_lista_uppgång)
class(lemmel_lista_uppgång$ID)
length(unique(lemmel_lista_uppgång$ID))
# tog svinlång tid att hitta en lösning på att lägga in rätt lynamn på alla rader. När lemmel_lista_uppgång
# skapas så behålls inte lyornas namn. Det läggs in default-namn istället. Den kolumnen heter ID och är bara nummer
# 1 till 80 (80 lyor). Eftersom det är svinmånga rader som ska ha samma lynamn funkar inte cbind. Man måste tala 
# om för r vilka siffror som ska ha vilket namn. Det gör man genom att föklara att ID-numren är factor levels.
# man behöver inte skriva ut alla (1:80 räcker). För att slippa skriva ut alla lynamn med citationstecken och komma
# kan man använda paste så gör r det automatiskt. Det funkar inte att bara skriva lyor$Namn. Lynamnen blir faktorer.
# Hoppas det inte blir problem. 
lemmel_lista_uppgång$Namn<-factor(lemmel_lista_uppgång$ID, levels = c(1:80),
       labels = c(paste(lyor$Namn)))

head(lemmel_lista_uppgång)
class(lemmel_lista_uppgång$Namn)

names(lemmel_lista_uppgång) <- c("ID","lämmelprediktion_uppgångsår", "Namn")

head(lemmel_lista_uppgång)

lemmel_lista_uppgång<-lemmel_lista_uppgång %>% 
  dplyr::select(-ID)
  


#byter plats på kolumnerna
lemmel_lista_uppgång <- lemmel_lista_uppgång[c("Namn","lämmelprediktion_uppgångsår")] 
head(lemmel_lista_uppgång)

#' Karin tyckte att jag skulle ta variansen som ett mått istället för andel bra lämmelhabitat för
#' uppgångsår.
#' Jag behåller dock koden för andel bra habitat ifall den behövs.

t <- lemmel_lista_uppgång %>% 
  group_by(Namn) %>% 
  summarise(lemmel_var = var(lämmelprediktion_uppgångsår))
max(t$lemmel_var, na.rm = TRUE)
min(t$lemmel_var, na.rm = TRUE)

write_xlsx(t, path = "Den and territory selection/Data/lämmelvarians per lya.xlsx")

#Gör samma sak för toppår
class(lemmel_lista_topp)
lemmel_lista_topp<- as.data.frame(lemmel_lista_topp)

lemmel_lista_topp$Namn<-factor(lemmel_lista_topp$ID, levels = c(1:80),
                                  labels = c(paste(lyor$Namn)))
lemmel_lista_topp

names(lemmel_lista_topp) <- c("ID","lämmelprediktion_toppår", "Namn")

head(lemmel_lista_topp)

lemmel_lista_topp<-lemmel_lista_topp %>% 
  dplyr::select(-ID)

#byter plats på kolumnerna
lemmel_lista_topp <- lemmel_lista_topp[c("Namn","lämmelprediktion_toppår")] 
head(lemmel_lista_topp)

#Nu måste jag få fram andel bra lämmelhabitat per lybuffer
max(lemmel_lista_uppgång$lämmelprediktion_uppgångsår)
length(which(lemmel_lista_topp$Namn == "FSZZ008")) #Själva pixlarna är inte på 500 x 500 meter. Det är bara upplösningen på NDVI. Pixlarna är typ 48 x 43 meter. Därför finns det fett många värden per lya..

head(lemmel_lista_uppgång)

max(lemmel_lista_uppgång$lämmelprediktion_uppgångsår, na.rm = TRUE) #maximala sannolikheten är 1 för en pixel. Låter skumt för ett uppgångsår tycker Rasmus.
min(lemmel_lista_uppgång$lämmelprediktion_uppgångsår, na.rm = TRUE)
View(lemmel_lista_uppgång)


#' Först plockar jag ut de pixlar som är bra habitat för lämmel under uppgångsår.
#' Jag sätter 0,265 som gräns för bra lämmelhabitat. Det är det maximala medelvärdet för en enskild lya delat på
#' två. Borde kanske ta median eller medelvärdet av medelvärdet istället.
#' 
uppgång_bra <- lemmel_lista_uppgång %>%
  group_by(Namn) %>%
  filter(lämmelprediktion_uppgångsår > ((max(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE))/2)) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(bra_lämmelhabitat = sum(n))
  
head(uppgång_bra)

#' Karin tyckte jag skulle ha tre nivåer av lämmelhabitat. Gör en medelbra. Tar spannet
#' mellan 0,256 och medianen så länge.

uppgång_medel <- lemmel_lista_uppgång %>%
  group_by(Namn) %>%
  dplyr::filter(lämmelprediktion_uppgångsår < ((max(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE))/2),
         lämmelprediktion_uppgångsår > median(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE)) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(medelbra_lämmelhabitat = sum(n))



head(uppgång_medel)

#' plockar även ut dåliga habitat. Tar de som är lägre än medianen så länge

uppgång_dålig <- lemmel_lista_uppgång %>%
  group_by(Namn) %>%
  dplyr::filter(lämmelprediktion_uppgångsår < median(lemmel$medelvärde_lämmelprediktion_uppgångsår, na.rm = TRUE)) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(dåliga_lämmelhabitat = sum(n))

head(uppgång_dålig)


alla_habitat <- lemmel_lista_uppgång %>%
  group_by(Namn) %>%
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(alla_lämmelhabitat = sum(n))



View(alla_habitat)
length(uppgång_bra$Namn)
length(uppgång_medel$Namn)
length(uppgång_dålig$Namn)
length(alla_habitat$Namn) #den innehåller fler lyor än uppgång_bra, uppgång_medel och uppgång_dålig


#' alla_habitat innehåller de lyor som har NA på 
#' lämmelsannolikhet eftersom de inte täcks in av rastern. Måste lokalisera vilka
#' det är och ta bort dem så att kolumnerna är lika långa. Koden nedan gör detta.
#' ! betyder alla som finns i alla_habitat som inte finns i uppång_bra. Uppgång_bra har lika många 
#' lyor som uppgång_medel och uppgång_dålig
#' 
att_ta_bort<-alla_habitat[!alla_habitat$Namn %in% uppgång_bra$Namn, ] 

att_ta_bort$Namn


alla_habitat <- subset(alla_habitat, !Namn %in% c(paste(att_ta_bort$Namn))) #tar bort lyorna "FSZZ041", "FSZZ047", "FSZZ049", "FSZZ086", "FSZZ093".

length(alla_habitat$Namn) #nu är de borta

#' Nu kan vi lägga in dem i samma dataram. spara som vektorer först
Namn<- uppgång_bra$Namn
bra_lämmelhabitat<- uppgång_bra$bra_lämmelhabitat
medelbra_lämmelhabitat<- uppgång_medel$medelbra_lämmelhabitat
dåliga_lämmelhabitat <- uppgång_dålig$dåliga_lämmelhabitat
alla_lämmelhabitat<-alla_habitat$alla_lämmelhabitat

kombinerad <- data.frame(Namn, bra_lämmelhabitat, medelbra_lämmelhabitat, dåliga_lämmelhabitat, alla_lämmelhabitat)

head(kombinerad)

proportioner <- kombinerad %>% 
  mutate(andel_bra_lämmelhabitat_uppgångsår = bra_lämmelhabitat/alla_lämmelhabitat) %>% 
  mutate(andel_medelbra_lämmelhabitat_uppgångsår = medelbra_lämmelhabitat/alla_lämmelhabitat) %>% 
  mutate(andel_dåliga_lämmelhabitat_uppgångsår = dåliga_lämmelhabitat/alla_lämmelhabitat)

head(proportioner)

View(proportioner)

#kollar så att andelarna summerar till 1. Det gör de
stopifnot((proportioner$andel_bra_lämmelhabitat_uppgångsår + 
             proportioner$andel_medelbra_lämmelhabitat_uppgångsår + 
             proportioner$andel_dåliga_lämmelhabitat_uppgångsår) == 1)


View(proportioner)



#' kommer säkert behöva skriva över filen eftersom gränsvärdena för bra, medelbra och dåligt lämmelhabitat inte
#' är speciellt smart satta
write_xlsx(proportioner, path = "Den and territory selection/Rawdata/andel_lämmelhabitattyper_per_lya.xlsx")

#' Jag gör en till uppdelning i tre, men den här gången
#' tar jag bra lyor som den översta tredjedelen av alla sannolikheter. Medelbra som
#' den mittersta delen och dåliga som den
#' lägsta tredjedelen.

#' först sorterar jag sannolikhetsvärdena efter storlek med arrange()
#' kommandot desc() gör att värdena sorteras descending, dvs från stor
#' till liten

?arrange

sorterade_lämlar<-lemmel_lista_uppgång %>% 
  arrange(desc(lämmelprediktion_uppgångsår))
  

View(sorterade_lämlar)

length(sorterade_lämlar$lämmelprediktion_uppgångsår)

#' Det finns 241820 rader med sannolikheter. Funktionen slice i
#' dplyr plockar ut rader baserat på position. Jag plockar ut den översta tredjedelen
#' först. Dessa är bra lämmelhabitat

#'Räknar ut den översta tredjedelen av värdena.
#'Round avrundar. Man kan inte specificera kolumn i slice,
#'den tar bara alla rader. Det gör ingenting för mig eftersom jag
#'vill ha med båda kolumnerna
round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3))
class(sorterade_lämlar$lämmelprediktion_uppgångsår)

bra_lämmelhabitat_2<- sorterade_lämlar %>%
  slice(1:round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3))) %>% 
  group_by(Namn) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(bra_lämmelhabitat = sum(n))

round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3))             
1+(round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3)))     

head(bra_lämmelhabitat_2)
length(bra_lämmelhabitat_2$bra_lämmelhabitat)

medelbra_lämmelhabitat_2 <-sorterade_lämlar %>% 
  slice(82821:
          (2*round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3)))) %>% 
  group_by(Namn) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(medelbra_lämmelhabitat = sum(n))

head(medelbra_lämmelhabitat_2)

dåliga_lämmelhabitat_2 <-sorterade_lämlar %>% 
  slice(1+(2*round((length(sorterade_lämlar$lämmelprediktion_uppgångsår)/3))):
          max(length(sorterade_lämlar$lämmelprediktion_uppgångsår))) %>% 
  group_by(Namn) %>% 
  count(lämmelprediktion_uppgångsår) %>% 
  summarise(dåliga_lämmelhabitat = sum(n))

head(dåliga_lämmelhabitat_2)
length(dåliga_lämmelhabitat_2$dåliga_lämmelhabitat)#innehåller NA-raderna

#tar bort NA raderna som inte täcks av rastern
dåliga_lämmelhabitat_2 <- subset(dåliga_lämmelhabitat_2, !Namn %in% c(paste(att_ta_bort$Namn))) #återanvänder den här. Nu är NAs borta 

#samma sak med alla_habitat
att_ta_bort<-alla_habitat[!alla_habitat$Namn %in% uppgång_bra$Namn, ] 

att_ta_bort$Namn

alla_habitat <- subset(alla_habitat, !Namn %in% c(paste(att_ta_bort$Namn))) #tar bort lyorna "FSZZ041", "FSZZ047", "FSZZ049", "FSZZ086", "FSZZ093".

kombinerad_2<- bra_lämmelhabitat_2 %>% 
  left_join(medelbra_lämmelhabitat_2, by = "Namn" ) %>% 
  left_join(dåliga_lämmelhabitat_2, by = "Namn") %>% 
  left_join(alla_habitat, by = "Namn")



View(kombinerad_2)


proportioner_2 <- kombinerad_2 %>% 
  group_by(Namn) %>% 
  mutate(andel_bra_lämmelhabitat_uppgångsår = bra_lämmelhabitat/alla_lämmelhabitat) %>% 
  mutate(andel_medelbra_lämmelhabitat_uppgångsår = medelbra_lämmelhabitat/alla_lämmelhabitat) %>% 
  mutate(andel_dåliga_lämmelhabitat_uppgångsår = dåliga_lämmelhabitat/alla_lämmelhabitat)

View(proportioner_2)

#kollar så att andelarna summerar till 1. Det gör de
stopifnot((proportioner_2$andel_bra_lämmelhabitat_uppgångsår + 
             proportioner_2$andel_medelbra_lämmelhabitat_uppgångsår + 
             proportioner_2$andel_dåliga_lämmelhabitat_uppgångsår) == 1)

#vet inte varför stopifnot inte fungerar. Alla blir ju 1:

proportioner_2$andel_bra_lämmelhabitat_uppgångsår + 
  proportioner_2$andel_medelbra_lämmelhabitat_uppgångsår + 
  proportioner_2$andel_dåliga_lämmelhabitat_uppgångsår

write_xlsx(proportioner_2, path = "Den and territory selection/Rawdata/andel_lämmelhabitattyper_per_lya_tredjedelsuppdelning.xlsx")


