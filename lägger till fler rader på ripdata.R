library(writexl)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

riptransekter <- read_xlsx(path = "Rawdata/tor.riptransekter helags 2018.xlsx")

variabler <- c("F", "D", "R") #vet inte varför man måste spara en vektor med namnen. Funkar att ta många strings
# på en gång i alla fall. Får bara till att ta en string i taget med str_detect.

dubbla_ripor <- riptransekter %>%
  filter(antal > 1) %>% 
  filter(observation %in% variabler)

dubbla_ripor$antal  #bara obsar med fler än en ripa.

fler_ripor <- riptransekter %>%
  filter(antal > 2) %>% 
  filter(observation %in% variabler)

fler_ripor$antal #max antal ripor per rad är 3.



#' lägger till 4 grader på vinkeln på de tredje riporna
fler_ripor$vinkel <- (fler_ripor$vinkel + 4)

fler_ripor$vinkel

#' lägger till 2 grader på de dubbla riporna. De innehåller även de tredje riporna. det är bra, eftersom 
#' de ska ha tre rader.

dubbla_ripor$vinkel
dubbla_ripor$vinkel <- (dubbla_ripor$vinkel + 2)

dubbla_ripor<-cbind(dubbla_ripor, `kommentar` = ("lagt till 2 grader extra till vinkeln i efterhand") )
fler_ripor<-cbind(fler_ripor, `kommentar` = ("lagt till 4 grader extra till vinkeln i efterhand") )

head(dubbla_ripor)
head(fler_ripor)

length(dubbla_ripor$lya)
length(fler_ripor$lya)
length(riptransekter$lya)

length(dubbla_ripor$lya) + 
  length(fler_ripor$lya) + 
  length(riptransekter$lya) # om raderna läggs ihop rätt i nästa steg ska det bli 426 rader


extra_rader<-bind_rows(riptransekter, dubbla_ripor, fler_ripor)

length(extra_rader$lya) #stämmer!


#' vill titta på raderna för att se hur det blev. Jag plockar ut fågel-obsarna igen.
nya_rader <- extra_rader %>%
  filter(antal > 1) %>% 
  filter(observation %in% variabler)

View(nya_rader) # ser ut att stämma!

#' nu vill jag dela upp positionskolumnen i två olika kolumner med east och north-koordinater
extra_rader<-extra_rader %>% 
  separate(Position, c("N", "E"), sep = 7) # sep anger vid vilken position (vilken siffra) som kolumnen ska delas.
# vid den sjunde siffran splittas kolumnen alltså

View(extra_rader)

write_xlsx(extra_rader, path = "Rawdata/tor_modifierade_riptransekter_vår.xlsx")
