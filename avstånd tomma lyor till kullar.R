
# Räknar ut avstånd från tomma lyor till närmsta kull
library(maptools)
library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

#' Behöver två typer dataramar, en för varje tom lya per år och en
#' för varje kull per år
lyor <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Lyor helags alla.xlsx")
kullar <- read_xlsx(path ="Lyor, kullar, gps-punkter, yta och avstånd/ALLA VALPLYOR HELAGS  KORREKT 2000-2018.xlsx") 

lyor <- as.data.frame(lyor)
kullar <- as.data.frame(kullar)

#' Gör en lång dataram som innehåller alla lyor för alla år med en kolumn för
#' kull. 1 = kull, 0 = ingen kull

År_ram <- as.data.frame(unique(kullar$År))
colnames(År_ram) <- "År"
length(År_ram$År)

lyor



# replikerar lyorna 18 gånger, det vill säga alla år med kull

lyor_long <-  bind_rows(replicate(length(År_ram$År), lyor, simplify = FALSE))
View(lyor_long)

#' Replikerar år och fas
#' det var lågår 2009 (fas = 1), så lägger in det.
#' 2009 är position 43 och 44 i dataramen

kullar$Fas[43:44] <- 1
View(kullar)
# sparar faserna i en dataram
År_fas_ram <- kullar %>%
  group_by(År) %>% 
  summarise(Fas = mean(Fas))

# replikerar alla år och faser 80 gånger, det vill säga antalet lyor
År_multi <- as.data.frame(År_fas_ram[rep(seq_len(nrow(År_fas_ram)), each= length(lyor$Namn)), ])

colnames(År_multi) <- c("År", "Fas")
View(År_multi)

  
År_fas_ram #tror inte att de här faserna stämmer. 2011 står som 3. Borde vara 4. 4 på 2002, 2005 och 2008

#' lägger ihop lyorna med år och fas. bind_cols går på position så viktigt att
#' båda dataramarna har rätt längd
lyor_long <- bind_cols(lyor_long, År_multi)
View(lyor_long)

# skapar en kolumn som heter obsID och tar bort kommentar

lyor_long <- lyor_long %>%
  select(-Kommentar) %>% 
  unite(obsID, År,Namn, sep = "-", remove = FALSE)

# skapar en obsID-kolumn för kullar och tar bort kommentar

kullar <- kullar %>% 
  select(Namn, År, Fas) %>% 
  unite(obsID, År, Namn, sep = "-", remove = FALSE)


# lägger till kolumnen kull med 1 för kull och 0 för ingen kull
head(kullar)
lyor_long$kull <- as.numeric(lyor_long$obsID %in% kullar$obsID)
View(lyor_long)
#' kommer kunna använda dataramen lyor_long senare när jag gör AIC, så printar den nu.
#' Om faserna är fel kommer jag behöva göra om den biten och printa igen.

write_xlsx(lyor_long, path = "Lyor, kullar, gps-punkter, yta och avstånd/lyor helags med fas och kull lång.xlsx")

# nu kan jag plocka ut åren separat och börja mäta. t för tom, k för kull

lyor_long$N <- as.numeric(lyor_long$N)
lyor_long$E <- as.numeric(lyor_long$E)

coordinates(lyor_long) <- c("E", "N")
proj4string(lyor_long) <- CRS("+init=EPSG:3006")
summary(lyor_long) #sweref och projected
plot(lyor_long)

#' Enklare att strunta i dplyr. Då slipper man göra om till dataram 
#' och sen lägga på koordinaterna varenda gång för varje år som jag gjorde i distans till
#' rödräv

# År 2000
t2000 <- subset(lyor_long, År == 2000 & kull == 0)
k2000 <- subset(lyor_long, År == 2000 & kull == 1)

# År 2001
t2001 <- subset(lyor_long, År == 2001 & kull == 0)
k2001 <- subset(lyor_long, År == 2001 & kull == 1)

# År 2002
t2002 <- subset(lyor_long, År == 2002 & kull == 0)
k2002 <- subset(lyor_long, År == 2002 & kull == 1)

# År 2003
t2003 <- subset(lyor_long, År == 2003 & kull == 0)
k2003 <- subset(lyor_long, År == 2003 & kull == 1)

# År 2004
t2004 <- subset(lyor_long, År == 2004 & kull == 0)
k2004 <- subset(lyor_long, År == 2004 & kull == 1)

# År 2005
t2005 <- subset(lyor_long, År == 2005 & kull == 0)
k2005 <- subset(lyor_long, År == 2005 & kull == 1)

# År 2006
t2006 <- subset(lyor_long, År == 2006 & kull == 0)
k2006 <- subset(lyor_long, År == 2006 & kull == 1)

# År 2007
t2007 <- subset(lyor_long, År == 2007 & kull == 0)
k2007<- subset(lyor_long, År == 2007 & kull == 1)

# År 2008
t2008 <- subset(lyor_long, År == 2008 & kull == 0)
k2008<- subset(lyor_long, År == 2008 & kull == 1)

# År 2009
t2009 <- subset(lyor_long, År == 2009 & kull == 0)
k2009<- subset(lyor_long, År == 2009 & kull == 1)

# År 2010
t2010 <- subset(lyor_long, År == 2010 & kull == 0)
k2010<- subset(lyor_long, År == 2010 & kull == 1)

# År 2011
t2011 <- subset(lyor_long, År == 2011 & kull == 0)
k2011<- subset(lyor_long, År == 2011 & kull == 1)

# År 2012
# ingen kull

# År 2013
t2013 <- subset(lyor_long, År == 2013 & kull == 0)
k2013<- subset(lyor_long, År == 2013 & kull == 1)

# År 2014
t2014 <- subset(lyor_long, År == 2014 & kull == 0)
k2014<- subset(lyor_long, År == 2014 & kull == 1)

# År 2015
t2015 <- subset(lyor_long, År == 2015 & kull == 0)
k2015<- subset(lyor_long, År == 2015 & kull == 1)

# År 2016
t2016 <- subset(lyor_long, År == 2016 & kull == 0)
k2016<- subset(lyor_long, År == 2016 & kull == 1)

# År 2017
t2017 <- subset(lyor_long, År == 2017 & kull == 0)
k2017<- subset(lyor_long, År == 2017 & kull == 1)

# År 2018
t2018 <- subset(lyor_long, År == 2018 & kull == 0)
k2018<- subset(lyor_long, År == 2018 & kull == 1)


# skapar dataramar för att hålla avstånden
år2000 <- as.data.frame(cbind(t2000))
år2000$avs_kull <- as.numeric(0)

år2001 <- as.data.frame(cbind(t2001))
år2001$avs_kull <- as.numeric(0)

år2002 <- as.data.frame(cbind(t2002))
år2002$avs_kull <- as.numeric(0)

år2003 <- as.data.frame(cbind(t2003))
år2003$avs_kull <- as.numeric(0)

år2004 <- as.data.frame(cbind(t2004))
år2004$avs_kull <- as.numeric(0)

år2005 <- as.data.frame(cbind(t2005))
år2005$avs_kull <- as.numeric(0)

år2006 <- as.data.frame(cbind(t2006))
år2006$avs_kull <- as.numeric(0)

år2007 <- as.data.frame(cbind(t2007))
år2007$avs_kull <- as.numeric(0)

år2008 <- as.data.frame(cbind(t2008))
år2008$avs_kull <- as.numeric(0)

år2009 <- as.data.frame(cbind(t2009))
år2009$avs_kull <- as.numeric(0)

år2010 <- as.data.frame(cbind(t2010))
år2010$avs_kull <- as.numeric(0)

år2011 <- as.data.frame(cbind(t2011))
år2011$avs_kull <- as.numeric(0)

år2013 <- as.data.frame(cbind(t2013))
år2013$avs_kull <- as.numeric(0)

år2014 <- as.data.frame(cbind(t2014))
år2014$avs_kull <- as.numeric(0)

år2015 <- as.data.frame(cbind(t2015))
år2015$avs_kull <- as.numeric(0)

år2016 <- as.data.frame(cbind(t2016))
år2016$avs_kull <- as.numeric(0)

år2017 <- as.data.frame(cbind(t2017))
år2017$avs_kull <- as.numeric(0)

år2018 <- as.data.frame(cbind(t2018))
år2018$avs_kull <- as.numeric(0)

# Beräknar närmsta avstånd från tom lya till kull
år2000$avs_kull <- apply(gDistance(k2000, t2000, byid=TRUE), 1, min)
år2001$avs_kull <- apply(gDistance(k2001, t2001, byid=TRUE), 1, min)
år2002$avs_kull <- apply(gDistance(k2002, t2002, byid=TRUE), 1, min)
år2003$avs_kull <- apply(gDistance(k2003, t2003, byid=TRUE), 1, min)
år2004$avs_kull <- apply(gDistance(k2004, t2004, byid=TRUE), 1, min)
år2005$avs_kull <- apply(gDistance(k2005, t2005, byid=TRUE), 1, min)
år2006$avs_kull <- apply(gDistance(k2006, t2006, byid=TRUE), 1, min)
år2007$avs_kull <- apply(gDistance(k2007, t2007, byid=TRUE), 1, min)
år2008$avs_kull <- apply(gDistance(k2008, t2008, byid=TRUE), 1, min)
år2009$avs_kull <- apply(gDistance(k2009, t2009, byid=TRUE), 1, min)
år2010$avs_kull <- apply(gDistance(k2010, t2010, byid=TRUE), 1, min)
år2010$avs_kull <- apply(gDistance(k2010, t2010, byid=TRUE), 1, min)
år2011$avs_kull <- apply(gDistance(k2011, t2011, byid=TRUE), 1, min)
år2013$avs_kull <- apply(gDistance(k2013, t2013, byid=TRUE), 1, min)
år2014$avs_kull <- apply(gDistance(k2014, t2014, byid=TRUE), 1, min)
år2015$avs_kull <- apply(gDistance(k2015, t2015, byid=TRUE), 1, min)
år2016$avs_kull <- apply(gDistance(k2016, t2016, byid=TRUE), 1, min)
år2017$avs_kull <- apply(gDistance(k2017, t2017, byid=TRUE), 1, min)
år2018$avs_kull <- apply(gDistance(k2018, t2018, byid=TRUE), 1, min)

# lägger ihop i en dataram
avs_tom_till_kull <-rbind(år2000, år2001, år2002, år2003,
                           år2004, år2005, år2006,
                           år2007, år2008, år2009,
                           år2010, år2011,
                           år2013, år2014, år2015,
                           år2016, år2017, år2018)


View(avs_tom_till_kull)
# tar bort koordinaterna så kan jag spara på github

avs_tom_till_kull <- avs_tom_till_kull %>% 
  select(-c(E, N))
length(avs_tom_till_kull$År)
# Printar filen. OBS! OSÄKER PÅ OM FASERNA ÄR RÄTT!!!!

write_xlsx(avs_tom_till_kull, path = "Den and territory selection/Data/avstånd tomma lyor till kull.xlsx")





år2001$avs_kull <- apply(gDistance(k2000, t2000, byid=TRUE), 1, min)