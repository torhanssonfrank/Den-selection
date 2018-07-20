

# testar med en fil jag fick från Karin. Kan jämföra den med Rasmus filer och min egen sammanställning. Finns dock bara reproduktionsdata
# fram till 2010.


library(readxl)
library(dplyr)
library(tidyr)
library(assertr)
library(stringr)
library(writexl)
alla.lyor <- read_xlsx(file.choose()) #läser in filen BEBODDA_LYOR_HEF 00_10.xlsx

variable.names(alla.lyor)

karins.lyor<- alla.lyor %>% #plockar ut lyorna med reproduktion
  filter(Litter == "1")

head(karins.lyor)

karins.lyor <- karins.lyor %>% 
  unite(col = "litterID", sep = "-", Year, DenCode, remove = FALSE) %>% #sätter remove = FALSE för att spara kolumnerna Year och DenCode, annars plockas de bort
  select(DenCode, litterID, Year)

head(karins.lyor)

tors.summary <- read_xlsx(file.choose()) #läser in filen "försök till sammanställning av alla kullar.xlsx"

head(tors.summary)

tors.lyor <- tors.summary %>% #plockar ut litterID, behöver inte spara de andra variablerna i den här dataramen. Tar 2010 och äldre för det finns inte nyare i Karins fil
  filter(year <= 2010) %>% 
  select(litterID)

head(tors.lyor)
tail(tors.lyor)



all.equal(tors.lyor$litterID, karins.lyor$litterID) #filerna innehåller inte lika många lyor

length(tors.lyor$litterID)
length(karins.lyor$litterID) #karin har 61 kullar, jag har 56. Detta framgår även av all.equal

tors.lyor %>% 
  filter(str_detect(litterID, "2000")) #min data har en kull registrerad år 2000. funktionen str_detect finns i stringr-paketet

karins.lyor %>% 
  filter(str_detect(litterID, "2000")) #karins data har två kullar registrerade år 2000

karins.lyor %>% 
  filter(str_detect(litterID, "2007-FSZZ059")) #den här finns i Rasmus.kulldata till Tor men inte här eller i Helags_Red_Fox_Feeding

#jag vill dock automatisera detta så att jag slipper titta igenom varje år manuellt.

commonID <- intersect(tors.lyor$litterID, karins.lyor$litterID) #här sparar jag alla litterID som finns i både min och karins fil

karins.lyor[!karins.lyor$litterID %in% commonID, ] #här säger jag åt R att spotta ut de lyor som finns i Karins lyor som inte finns i commonID,
# det vill säga som inte finns i både hennes och min fil. "!" betyder "inte".

#alternativ metod som är lite snabbare. Jag vet ju att Karins fil har mer lyor än min fil, eftersom jag använde "length()" innan för
#att se hur många rader som fanns i respektive fil. Då kan jag bara fråga R vilka filer som finns i karins.lyor som inte finns i tors.lyor

udda.lyor <- karins.lyor[!karins.lyor$litterID %in% tors.lyor$litterID, ]
udda.lyor


head(tors.summary)

  
names(udda.lyor) <-c("denNr", "litterID", "year") 


udda.lyor <- cbind(udda.lyor,`tillagd av Tor från fil` = ("BEBODDA_LYOR_HEF 00_10.xlsx") )
head(udda.lyor)

kombinerade.lyor <- bind_rows(tors.summary, udda.lyor)


View(kombinerade.lyor)

write_xlsx(kombinerade.lyor, path = "Rawdata/min sammanställning plus BEBODDA_LYOR_HEF 00_10.xlsx")


