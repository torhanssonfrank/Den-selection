#relativt mått på kullar

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(lubridate)

kullar <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx")
kullar<- as.data.frame(kullar)
head(kullar)

tot_kullar <- kullar %>%
  group_by(Namn) %>% 
  count(År) %>% 
  summarise(kullar_totalt = sum(n))
View(tot_kullar)

inventeringar2000_2010<- read_xlsx(path = "Våraktivitet fjällräv/BEBODDA_LYOR_HEF 00_10.xlsx")

head(inventeringar2000_2010)  

inventeringar2000_2010 <-as.data.frame(inventeringar2000_2010)

lyor_alla <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.xlsx")
lyor_alla<- as.data.frame(lyor_alla)
head(lyor_alla)

lyor_alla<-lyor_alla %>% 
  select("Namn")


lyor_alla <- cbind(lyor_alla,`antal kullar` = 0 )

#' Räknar ihop antal somrar som varje lya inventerats
#' mellan 2000 och 2010
sommar_inv<- inventeringar2000_2010 %>%
  group_by(DenCode) %>%
  filter(`INV. SUM` %in% "Y") %>% 
  count(`INV. SUM`) %>% 
  summarise(inv_totalt = sum(n))

View(sommar_inv)

colnames(sommar_inv) <- c("Namn", "inv_totalt")
#Läser in inventeringar 2015-2018 rovbasen

inventeringar2015_2018<- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Lyinventeringar 2015_2018.xlsx")

class(inventeringar2015_2018$Kontrolldato) #datumen blev inlästa som datum

# Plockar ut sommarinventeringarna. 6,7,8 står för juni, juli augusti
sommar_inv_15_18 <- inventeringar2015_2018[month(inventeringar2015_2018$Kontrolldato) %in% c(6,7,8), ]
head(sommar_inv_15_18)
View(sommar_inv_15_18)

#' Tar bort kolumner jag inte behöver. Tar även
#' bort datumkolumnen. Då har jag flera obsar som är dubletter. Då kan
#' jag använda "distinct" för att ta bort dubletterna så att jag får en 
#' inventering per sommar.

sommar_inv_15_18 <- sommar_inv_15_18 %>% 
  select(-c(X__1, Kontrolldato)) %>% 
  distinct()

View(sommar_inv_15_18)

#' Räknar ihop antal somrar som varje lya inventerats
#' mellan 2015 och 2018
sommar_inv_15_18<- sommar_inv_15_18 %>%
  group_by(Namn) %>%
  count(År) %>% 
  summarise(inv_totalt2 = sum(n))

View(sommar_inv_15_18)

inventeringar_alla <- sommar_inv_15_18 %>% 
  left_join(sommar_inv, by = "Namn")

#ändrar NA till 0 i den kortare kolumnen
inventeringar_alla <- inventeringar_alla %>% 
  mutate_all(funs(replace(., is.na(.),0)))

View(inventeringar_alla)
inventeringar_alla<-inventeringar_alla %>% 
  mutate(inventeringar = inv_totalt2 + inv_totalt) %>% 
  select(-c(inv_totalt2, inv_totalt))

lyor_antal_kullar$inventeringar[79] <- 1

lyor_antal_kullar<- lyor_alla %>% 
  left_join(tot_kullar, by = "Namn") %>%
  left_join(inventeringar_alla, by = "Namn")

lyor_antal_kullar 

#ersättar NA med 0
lyor_antal_kullar <- lyor_antal_kullar %>% 
  mutate_all(funs(replace(., is.na(.),0)))

lyor_antal_kullar$inventeringar[79] <- 1 #den här raden hade en kull men 0 inventeringar.

lyor_antal_kullar<- lyor_antal_kullar %>% 
  mutate(relativa_kullar = kullar_totalt/inventeringar)
View(lyor_antal_kullar)

lyor_antal_kullar <- lyor_antal_kullar %>% 
  mutate_all(funs(replace(., is.na(.),0)))

#'den här filen stämmer inte med avseende på relativa kullar.
#'Det saknas nog antal inventeringar. zz108 har 1, vilket är bäst relativa kullar.
#'Den har bara 1 inventering och 1 kull inlagd, alltså 100% hitrate.

write_xlsx(lyor_antal_kullar, path = "Den and territory selection/Rawdata/antal kullar per lya 2000_2018.xlsx")

