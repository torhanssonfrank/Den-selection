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

#' Räknar ihop antal somrar som varje lya inventerats
#' mellan 2000 och 2010
sommar_inv<- inventeringar2000_2010 %>%
  group_by(DenCode) %>%
  filter(`INV. SUM` %in% "Y") %>% 
  count(`INV. SUM`) %>% 
  summarise(inv_totalt = sum(n))

View(sommar_inv)

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
  summarise(inv_totalt = sum(n))

View(sommar_inv_15_18)
  
