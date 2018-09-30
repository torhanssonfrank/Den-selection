

library(MuMIn)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(sp)
library(rgdal)
library(rgeos)

#' Skapar en AIC-fil med alla år separat.
#' 

#' Först en fil med alla lyor med kullar i långt format.
#' Alla lyor har en separat rad för varje år.
lyor_long <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/lyor helags med fas och kull lång.xlsx")

# behöver nog inte den här
kullar <- read_xlsx(path ="Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx") 

# De lyor som jag ska använda, dvs kärnlyorna i Helags som inventerats varje år.

kärnlyor <- readOGR(dsn ="Lyor, kullar, gps-punkter, yta och avstånd/lyor helags kärnområde.shp")

#' avstånd till rödräv. kommer nog inte använda den
röd_dist<-read_xlsx(path = "Den and territory selection/Rawdata/avstånd till rödräv 2001-2015.xlsx")

#' rödrävstäthet runt varje lya
rödräv_densitet <- read_xlsx(path = "Den and territory selection/Data/rödrävstäthet runt lyor.xlsx")

#' avstånd till närmsta kull från lya med kull
kull_dist<-read_xlsx(path = "Den and territory selection/Rawdata/distans närmsta förynging.xlsx")

#'avstånd från tom lya till kull
avs_tom_till_kull <- read_xlsx(path = "Den and territory selection/Data/avstånd tomma lyor till kull.xlsx")

#' varje lyas höjd över havet
höjd <- read_xlsx(path = "Den and territory selection/Rawdata/lyor_hojd_helags.xlsx")

#' medelvärde av lämmeltäthet med 1.5 km radie per lya
lemmel_medel <- read_xlsx(path = "Den and territory selection/Rawdata/lämmelprediktion_medelvärde_topp_uppgång.xlsx")

#' andel bra lämmelhabitat med 1.5 km radie per lya. Byta till varians?
lemmel_andel<- read_xlsx(path = "Den and territory selection/Rawdata/andel_lämmelhabitattyper_per_lya.xlsx")

#' variansen av lämmelhabitat runt varje lya
lemmel_varians<- read_xlsx(path = "Den and territory selection/Data/lämmelvarians per lya.xlsx")

#' andel area med myr med 1.5 km radie runt varje lya
andel_myr <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area med myrmark med 1,5 km radie runt varje lya.xlsx")

#' andel area med vatten med 1.5 km radie runt varje lya
andel_vatten <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area som är vatten med 1,5 km radie runt varje lya.xlsx")

# avstånd till närmsta vattenkälla och skog
vatten_skog <-read.csv("Den and territory selection/Rawdata/lyor_distans_vatten_skog_utan_gps.csv", stringsAsFactors = FALSE, sep =";", dec = ",")

lyor_long<-as.data.frame(lyor_long)
kullar<-as.data.frame(kullar)
kärnlyor <- as.data.frame(kärnlyor)
röd_dist<-as.data.frame(röd_dist)
rödräv_densitet<-as.data.frame(rödräv_densitet)
kull_dist<-as.data.frame(kull_dist)
avs_tom_till_kull <- as.data.frame(avs_tom_till_kull)
höjd<-as.data.frame(höjd)
lemmel_medel<-as.data.frame(lemmel_medel)
lemmel_andel<-as.data.frame(lemmel_andel)
lemmel_varians<-as.data.frame(lemmel_varians)
andel_myr<-as.data.frame(andel_myr)
andel_vatten<-as.data.frame(andel_vatten)
vatten_skog<-as.data.frame(vatten_skog)

View(lyor_long)
View(kullar) # ta bort kommentar. Fas kan vara kvar
head(kärnlyor) # behöver bara Namn
head(röd_dist)
head(rödräv_densitet)
head(kull_dist) # måste skapa en kolumn med Namn och byta namn från year till År
head(höjd)
head(lemmel_medel) # ska bara ha uppgångsår här
head(lemmel_andel) # börjar med andel bra lämmelhabitat här
head(lemmel_varians)
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

# Tar bort allt utom namn ur kärnlyor

kärnlyor <- kärnlyor %>% 
  select(Namn)

head(kärnlyor)

#' fixar en kolumn som heter Namn och byter namn från year till År i 
#' filen med avstånd till närmsta kull. Eftersom År skapas av litterID kan jag bara 
#' droppa kolumnen year.

head(kull_dist)
View(kull_dist)

kull_avs <- kull_dist%>%
  separate(litterID, c("År", "Namn"), remove = FALSE) %>%
  rename(obsID = litterID) %>%
  rename(avs_kull = distance) %>% 
  select(obsID, avs_kull)

head(kull_avs)


# lägger ihop kull_avs med avstånd från tomma lyor till kull
View(avs_tom_till_kull)

tom_avs <- avs_tom_till_kull %>% 
  select(obsID, avs_kull)

head(tom_avs)

kullar_avs <- bind_rows(kull_avs, tom_avs)

length(kullar_avs$obsID)

#distans till rödräv
dist_rödräv <- röd_dist %>%
  unite(obsID, År, Namn, sep = "-")

View(dist_rödräv)
length(dist_rödräv$obsID)

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
head(myrA)

# ta bort koordinater och kommentarer
head(andel_vatten)
vattenA <- andel_vatten %>%
  select(Namn, area_vatten)

head(vattenA)

#den här är i rätt format
head(vatten_skog)

#' Dags att plocka fram ly- och kullramen och lägga på variabler.
#' Börjar med avstånd till kull
head(lyor_long)
length(lyor_long$obsID)
length(kullar_avs$obsID) # den här är tre obsar kortare än lyor_long
udda<-lyor_long[!lyor_long$obsID %in% kullar_avs$obsID, ]

udda # de obsar som fattas i kullar_avs är kullarna 2000, 2003 och 2006. Det var endast en kull då. Därför blir det inget avstånd från lyan med kull till en annan kull.

k <- lyor_long %>%
  left_join(kullar_avs, by = "obsID") %>% 
  left_join(dist_rödräv, by = "obsID")


View(k)
# lägger på resten
t <- k %>%
  left_join(lemmelM_uppg, by = "Namn") %>% 
  left_join(lemmelA_uppg, by = "Namn") %>%
  left_join(lemmel_varians, by = "Namn") %>% 
  left_join(rödräv_densitet, by = "Namn") %>% 
  left_join(höjd, by = "Namn") %>%
  left_join(myrA, by = "Namn") %>%
  left_join(vattenA, by = "Namn") %>%
  left_join(vatten_skog, by = "Namn")
  
View(t)

#printar en fil så att jag har en sparad med alla variabler

write_xlsx(t, path = "fjällrävslyor AIC.xlsx")

# Plockar ut kärnlyorna. Det är bara dem jag ska använda

t_sub<-t[t$Namn %in% kärnlyor$Namn, ]
View(t_sub)
length(t_sub$Namn)
length(unique(t_sub$Namn)) # 60 lyor är med. Det stämmer

# printar en fil med bara kärnlyor

write_xlsx(t_sub, path = "kärnlyor Helags AIC.xlsx")
