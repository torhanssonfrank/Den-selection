#limmar in lyornas trivialnamn för att skicka till Lars
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

lyor_till_lars <- read_excel(path = "Lyor, kullar, gps-punkter, yta och avstånd/Slutgiltiga valplyor delvis från Lars.xlsx")
alla_lyor <- read_excel(path = "lyor.xlsx") #inläst sen innan när jag gjorde myrar. Innehåller alla lyor med lynummer(Namn) och trivialnamn.

head(lyor_till_lars)
head(alla_lyor)

names(lyor_till_lars) <- c("Namn", "År", "fas")
names(alla_lyor) <- c("Namn","vanliga_namn", "N","E")

bara_namn<- alla_lyor %>% 
  select(Namn, vanliga_namn)

head(bara_namn)

lyor_till_lars<-lyor_till_lars %>% 
  left_join(bara_namn, by = "Namn" ) #left_join lägger in vanliga_namn vid matchande lynummer(Namn)

head(lyor_till_lars)

lyor_till_lars <- lyor_till_lars[c("Namn", "vanliga_namn", "År", "fas")] #byter plats på kolumner

head(lyor_till_lars)

write_xlsx(lyor_till_lars, path = "Lyor, kullar, gps-punkter, yta och avstånd/Fler lyor att dubbelkolla med lynamn.xlsx")

saknas<-lyor_till_lars[!lyor_till_lars$Namn %in% alla_lyor$Namn, ] 
saknas #det var bara FSZZ106 Norr Vaktklumpen som fanns i de rapporterade reproduktiva lyorna som inte fanns
# i min fil med alla lyor i Jämtland (viktigt att den har alla eftersom den läses in i QGIS). Den lyan är
# tillagd i lyor.xlsx och lyor.csv nu, precis som Blankan.
