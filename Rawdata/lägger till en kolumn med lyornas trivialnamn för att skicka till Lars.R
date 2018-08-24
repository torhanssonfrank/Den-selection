#limmar in lyornas trivialnamn för att skicka till Lars
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

lyor_till_lars <- read_excel(path = "Lyor, kullar, gps-punkter, yta och avstånd/Slutgiltiga valplyor delvis från Lars.xlsx")
alla_lyor #inläst sen innan när jag gjorde myrar. Innehåller alla lyor med lynummer(Namn) och trivialnamn.

names(lyor_till_lars) <- c("Namn", "År", "Kommentar")
names(alla_lyor) <- c("Namn","vanliga_namn", "N","E","area_myr")

bara_namn<- alla_lyor %>% 
  select(Namn, vanliga_namn)

head(bara_namn)

lyor_till_lars<-lyor_till_lars %>% 
  left_join(bara_namn, by = "Namn" ) #left_join lägger in vanliga_namn vid matchande lynummer(Namn)

head(lyor_till_lars)

lyor_till_lars <- lyor_till_lars[c("Namn", "vanliga_namn", "År", "Kommentar")] #byter plats på kolumner

head(lyor_till_lars)

write_xlsx(lyor_till_lars, path = "Lyor, kullar, gps-punkter, yta och avstånd/Lyor att dubbelkolla med lynamn.xlsx")
