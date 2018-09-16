library(MuMIn)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)


#' Skapar en AIC-fil med alla år separat.
#' 
kullar <- read_xlsx(path ="Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx") 
röd_dist<-read_xlsx(path = "Den and territory selection/Rawdata/avstånd till rödräv 2001-2015.xlsx")
kull_dist<-read_xlsx(path = "Den and territory selection/Rawdata/distans närmsta förynging.xlsx")
höjd <- read_xlsx(path = "Den and territory selection/Rawdata/lyor_hojd_helags.xlsx")
lemmel_medel <- read_xlsx(path = "Den and territory selection/Rawdata/lämmelprediktion_medelvärde_topp_uppgång.xlsx")
lemmel_andel<- read_xlsx(path = "Den and territory selection/Rawdata/andel_lämmelhabitattyper_per_lya.xlsx")
andel_myr <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area med myrmark med 1,5 km radie runt varje lya.xlsx")
andel_vatten <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area som är vatten med 1,5 km radie runt varje lya.xlsx")
vatten_skog <-read.csv("Den and territory selection/Rawdata/lyor_distans_vatten_skog_utan_gps.csv", stringsAsFactors = FALSE, sep =";", dec = ",")

kullar<-as.data.frame(kullar)
röd_dist<-as.data.frame(röd_dist)
kull_dist<-as.data.frame(kull_dist)
höjd<-as.data.frame(höjd)
lemmel_medel<-as.data.frame(lemmel_medel)
lemmel_andel<-as.data.frame(lemmel_andel)
andel_myr<-as.data.frame(andel_myr)
andel_vatten<-as.data.frame(andel_vatten)
vatten_skog<-as.data.frame(vatten_skog)

View(kullar) # ta bort kommentar. Fas kan vara kvar
head(röd_dist)
head(kull_dist) # måste skapa en kolumn med Namn och byta namn från year till År
head(höjd)
head(lemmel_medel) # ska bara ha uppgångsår här
head(lemmel_andel) # börjar med andel bra lämmelhabitat här
head(andel_myr) # ta bort koordinater och kommentar
head(andel_vatten) # ta bort koordinater och kommentar
head(vatten_skog)

# Städar upp de dataramar där det behövs

# Plockar ut de kolumner jag behöver ur kullar
kullar <- kullar %>%
  select(Namn, År, Fas)



#' det var lågår 2009 (fas = 1), så lägger in det.
#' 2009 är position 43 och 44 i dataramen

kullar$Fas[43:44] <- 1

# lägger in en obsID kolumn så att det blir enklare att lägga ihop data sen

kullar <- kullar %>%
  unite(obsID, År, Namn, sep = "-", remove = FALSE)
head(kullar)
#' fixar en kolumn som heter Namn och byter namn från year till År i 
#' filen med avstånd till närmsta kull. Eftersom År skapas av litterID kan jag bara 
#' droppa kolumnen year.

head(kull_dist)
View(kull_dist)

kull_avs <- kull_dist%>%
  separate(litterID, c("År", "Namn"), remove = FALSE) %>%
  rename(obsID = litterID) %>%
  rename(avs_kull = distance) %>% 
  select(-year)

head(kull_avs)
?separate
kull_avs <- kull_avs[c("obsID", "Namn", "År", "avs_kull")] #ändrar ordning
View(kull_avs)

# ta ut uppgångsåren för lämmelmedelvärdena

lemmelM_uppg <- lemmel_medel %>% 
  select(Namn, medelvärde_lämmelprediktion_uppgångsår)
head(lemmelM_uppg)

# tar ut andel bra för uppgångsåren för lämmelandelarna
head(lemmel_andel)
lemmelA_uppg <- lemmel_andel %>%
  select(Namn, andel_bra_lämmelhabitat_uppgångsår)
head(lemmelA_uppg)  

# ta bort koordinater och kommentarer
head(andel_myr)

myrA <- andel_myr %>% 
  select(Namn, area_myr)
head(andel_myr)

# ta bort koordinater och kommentarer
head(andel_vatten)
vattenA <- andel_vatten %>%
  select(Namn, area_vatten)

#' röd_dist innehåller alla lyor alla år
#' lägger in kolumnerna i en ny dataram

alla_år_var <- cbind(röd_dist)
head(alla_år_var)

#Lägger till obsID så att det blir enklare att lägga ihop dataramar sen
alla_år_var <- alla_år_var %>% 
  unite(obsID, År, Namn, sep = "-", remove = FALSE)

#' och lägger på åren med kull. En kolumn skapas och där det finns en match med 
#' kullar-dataramen blir det en 1. Där det inte är match blir det 0.

alla_år_var$kull <- as.numeric(alla_år_var$obsID %in% kullar$obsID)
View(alla_år_var)

#lägger på distans till närmsta föryngring

View(kull_avs)

(1.5^2) * pi
