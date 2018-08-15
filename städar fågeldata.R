library(dplyr)
library(tidyr)
library(writexl)
library(stringr) #man måste ha stringr för att kunna plocka ut raderna med ett en viss text (string)

#Läser in csv-filen som jag sparade ur Peters Microsoft access database (accdb - fil)
fågel <- read.csv(file.choose(), stringsAsFactors = FALSE) # filen heter FågelObservation_från_Peter.csv

head(fågel)

fågel_zz <- fågel %>%
  filter(str_detect(LineLabel, "FSZZ")) #str_detect från stringr anger att det är text dplyr ska leta efter i raderna. För kolumner (select) behövs inte stringr

fågel_zz <- fågel_zz %>% 
  separate(LineLabel, c("DenID", "Year", "No.obs"), sep =":")

fågel_zz
unique(fågel_zz$Year)
unique(fågel_zz$DenID)

write_xlsx(fågel_zz, path = "gamla fågeltransekter/fågeltransekter_Helags_05-08.xlsx") #ändrade wd till masterarbete

