# AIC -analys matade lyor

library(readxl)
library(writexl)
library(tidyverse)
library(MuMIn)
library(lme4)
library(MASS)
library(car)
library(arm)

fedDenYear<- read_xlsx(path= "Lyor, kullar, gps-punkter, yta och avstånd/fedDenYears.xlsx", sheet = 3)
dens <- read_xlsx(path = "kärnlyor Helags AIC 2000 - 2018.xlsx")
all.dens <- read_xlsx(path = "fjällrävslyor AIC 2000 - 2018.xlsx") # tar med den här också så kan jag printa en fil med matning för alla lyor och skicka till Rasmus
all.dens <- as.data.frame(all.dens)
dens <- as.data.frame(dens)
str(dens)

View(fedDenYear)

length(unique(fedDenYear$denNr)) #35.lika många som lyor som haft kullar.

#plockar ut lyorna från 2008 och framåt
colnames(fedDenYear)

fed.sub<- fedDenYear %>%
  dplyr::select(denNr, X__9:X__18)
#plockar bort de lyor som saknar matstation under något av åren

fed.sub <- fed.sub[complete.cases(fed.sub), ]

View(fed.sub)

#sparar namnen i en vektor

fed.names<-fed.sub %>% 
  dplyr::select(denNr) %>% 
  unique()

length(fed.names$denNr)# 23 matade lyor från 2008 och framåt

# måste vara en character vector för att select i dplyr ska funka
fed.names <- print(fed.names$denNr )


#ändrar till tre faser
dens$Fas[dens$Fas==4] <- 1
View(dens)
max(dens$Fas) # max är nu 3

#Ändrar om faserna till stringnamn så att R inte råkar läsa dem som siffror (då kan det bli helfel)
dens$Fas[dens$Fas==1] <- "low"
dens$Fas[dens$Fas==2] <- "increase"
dens$Fas[dens$Fas==3] <- "peak"
class(dens$Fas)

dens.sub <- dens %>%
  dplyr::select(-N, -E,-avs_kull, -närmaste_rödräv,  -andel_bra_lämmelhabitat_uppgångsår, -lemmel_var, -hojd_over_havet)

dens.sub <- dens.sub[complete.cases(dens.sub), ]


class(fed.names$Namn)
# plockar ut de matade lyorna ur variabelramen
fedDens<- dens.sub %>% 
  filter(Namn %in% fed.names)

#plockar ut från 2008 och framåt
yearframe <- as.numeric(2008:2018)

fedDens <- fedDens %>% 
  filter(År %in% yearframe)
length(fedDens$Namn) # 253 obsar
length(fedDens$Namn[fedDens$kull == 1]) #129 kullar
length(fedDens$Namn[fedDens$kull == 0]) # 124 ly-år utan kull

fedDens$Namn <- as.factor(fedDens$Namn) #måste vara factor för att kunna analyseras
fedDens$År <- as.factor(fedDens$År)
fedDens$Fas <- as.factor(fedDens$Fas)


## generalised linear models, multimodel averaging, endast matade lyor från 2008 och framåt ####

pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.fed <- fedDens
datsc.fed[pvars] <- lapply(datsc.fed[pvars],scale)

View(datsc)



global.modell.fed <- glmer(kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
                       + rödräv_densitet
                       + area_myr + area_vatten + distans_till_vatten
                       + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                       data = datsc.fed) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model.fed <- standardize(global.modell.fed, standardize.y = FALSE)
summary(stdz.model.fed)
car::vif(stdz.model.fed)
model.set.fed <- dredge(stdz.model.fed)
m.ave.fed<-model.avg(model.set.fed, subset = delta < 2)
summary(m.ave.fed)
confint(m.ave.fed, full = TRUE)

## fas 1 ####
fas.1.fed <- fedDens %>% 
  filter(Fas == "low")
View(fas.1.fed)
length(fas.1.fed$kull[fas.1.fed$kull==1]) # 18 kullar under lågår
length(fas.1.fed$obsID)

datsc.1.fed <- fas.1.fed
datsc.1.fed[pvars] <- lapply(datsc.1.fed[pvars],scale)
View(datsc.1.fed)

fas.1.modell.fed <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.1.fed)

stdz.model.1.fed <- standardize(fas.1.modell.fed, standardize.y = FALSE)
summary(stdz.model.1.fed)
car::vif(stdz.model.1.fed)
fas.1.setsc.fed <- dredge(stdz.model.1.fed)

nrow(datsc.1.fed) ## 92
ncol(getME(stdz.model.1.fed,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 92/7 = 13

ave.1sc.fed<-model.avg(fas.1.setsc.fed, subset = delta < 2) 
summary(ave.1sc.fed) 
confint(ave.1sc.fed, full = TRUE)

## fas 2 ####
fas.2.fed <- fedDens %>% 
  filter(Fas == "increase")

length(fas.2.fed$kull[fas.2.fed$kull==1]) # 39 kullar under uppgångsår
length(fas.2.fed$obsID)

datsc.2.fed <- fas.2.fed
datsc.2.fed[pvars] <- lapply(datsc.2.fed[pvars],scale)


fas.2.modell.fed <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                          + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc.2.fed)

stdz.model.2.fed <- standardize(fas.2.modell.fed, standardize.y = FALSE)
summary(stdz.model.2.fed)
car::vif(stdz.model.2.fed)
fas.2.setsc.fed <- dredge(stdz.model.2.fed)

nrow(datsc.2.fed) ## 69
ncol(getME(stdz.model.2.fed,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 69/7 = 10

ave.2sc.fed<-model.avg(fas.2.setsc.fed, subset = delta < 2) 
summary(ave.2sc.fed) 
confint(ave.2sc.fed, full = TRUE)

## fas 3 ####
fas.3.fed <- fedDens %>% 
  filter(Fas == "peak")

length(fas.3.fed$kull[fas.3.fed$kull==1]) # 72 kullar under toppår
length(fas.3.fed$obsID)

datsc.3.fed <- fas.3.fed
datsc.3.fed[pvars] <- lapply(datsc.3.fed[pvars],scale)


fas.3.modell.fed <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                          + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc.3.fed)

stdz.model.3.fed <- standardize(fas.3.modell.fed, standardize.y = FALSE)
summary(stdz.model.3.fed)
car::vif(stdz.model.3.fed)
fas.3.setsc.fed <- dredge(stdz.model.3.fed)

nrow(datsc.3.fed) ## 92
ncol(getME(stdz.model.3.fed,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 92/7 = 13

ave.3sc.fed<-model.avg(fas.3.setsc.fed, subset = delta < 2) 
summary(ave.3sc.fed) 
confint(ave.3sc.fed, full = TRUE)

## prövar med att lägga till matning som en variabel ####

head(fedDenYear)
fedDenYear<-as.data.frame(fedDenYear)

colnames(fedDenYear)
fedDen.comp <- fedDenYear %>%
  gather(släng,obsID,X__1:X__18)
View(fedDen.comp)

fedDen.long<-fedDen.comp %>%
  dplyr::select(-släng) %>%
  filter(str_detect(obsID, 'FSZZ'))

View(fedDen.long)

# antar att samma lyor var matade 2018

x2018<-fedDen.comp %>%
  filter(str_detect(obsID, '2017')) %>% 
  dplyr::select(denNr) %>% 
  dplyr::rename(obsID = denNr)


x2018$obsID <- paste0('2018-', x2018$obsID)
x2018

fedDen.long<-fedDen.long %>% 
  bind_rows(x2018)
View(fedDen.long)


dens.sub$fed <- as.numeric(dens.sub$obsID %in% fedDen.long$obsID) #lägger till en kolumn som heter fed i dens.sub. Där det finns en obsID-match med fedDen.long blir det en etta, där det inte är en match blir det en nolla. Det anger om lyan var matad eller inte under det året.
View(dens.sub)

length(unique(fedDen.long$denNr)) #den här blir en längre än fedDenYear eftersom denNr inte är inlagt för 2018. Det unika namnet för den raden blir ett NA. 
length(unique(fedDenYear$denNr))

str(dens.sub)
dens.sub$fed[dens.sub$fed==1] <- "fed"
dens.sub$fed[dens.sub$fed==0] <- "unfed"
str(dens.sub)
dens.sub$fed <- as.factor(dens.sub$fed)

# gör en dataram med alla lyor, kull, gps-punkter, fas, år och matning och skickar till Rasmus och Karin också
all.dens$matad<-as.numeric(all.dens$obsID %in% fedDen.long$obsID)
View(all.dens)
colnames(all.dens)
all.dens.print <- all.dens %>% 
  dplyr::select(obsID, Namn, N, E, År, Fas, kull, matad)
str(all.dens.print)
write_xlsx(all.dens.print, path = "Lyor, kullar, gps-punkter, yta och avstånd/lyor.helags.alla.koordinat.år.fas.kull.matad.lång.xlsx")
## AIC-analys med matning som variabel ####
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.fedvar <- dens.sub
datsc.fedvar[pvars] <- lapply(datsc.fedvar[pvars],scale)

View(datsc.fedvar)



global.modell.fedvar <- glmer(kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
                           + rödräv_densitet
                           + area_myr + area_vatten + distans_till_vatten
                           + distans_till_skog + fed + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                           data = datsc.fedvar) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model.fedvar <- standardize(global.modell.fedvar, standardize.y = FALSE)
summary(stdz.model.fedvar)
?standardize
model.set.fedvar <- dredge(stdz.model.fedvar)
m.ave.fedvar<-model.avg(model.set.fedvar, subset = delta < 2)
summary(m.ave.fedvar)
confint(m.ave.fedvar, full = TRUE)

## fas 1 ####
fas.1.fedvar <- dens.sub %>% 
  filter(Fas == "low")
View(fas.1.fedvar)
length(fas.1.fedvar$kull[fas.1.fedvar$kull==1]) # 29 kullar under lågår
length(fas.1.fedvar$obsID)

datsc.1.fedvar <- fas.1.fedvar
datsc.1.fedvar[pvars] <- lapply(datsc.1.fedvar[pvars],scale)
View(datsc.1.fedvar)

fas.1.modell.fedvar <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                          + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + fed + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc.1.fedvar)

stdz.model.1.fedvar <- standardize(fas.1.modell.fedvar, standardize.y = FALSE)
summary(stdz.model.1.fedvar)
car::vif(stdz.model.1.fedvar)
fas.1.setsc.fedvar <- dredge(stdz.model.1.fedvar)

ave.1sc.fedvar<-model.avg(fas.1.setsc.fedvar, subset = delta < 2) 
summary(ave.1sc.fedvar) 
confint(ave.1sc.fedvar, full = TRUE)

## fas 2 ####
fas.2.fedvar <- dens.sub %>% 
  filter(Fas == "increase")

length(fas.2.fedvar$kull[fas.2.fedvar$kull==1]) # 61 kullar under uppgångsår
length(fas.2.fedvar$obsID)

datsc.2.fedvar <- fas.2.fedvar
datsc.2.fedvar[pvars] <- lapply(datsc.2.fedvar[pvars],scale)


fas.2.modell.fedvar <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                          + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + fed + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc.2.fedvar)

stdz.model.2.fedvar <- standardize(fas.2.modell.fedvar, standardize.y = FALSE)
summary(stdz.model.2.fedvar)
car::vif(stdz.model.2.fedvar)
fas.2.setsc.fedvar <- dredge(stdz.model.2.fedvar)

nrow(datsc.2.fedvar) ## 69
ncol(getME(stdz.model.2.fedvar,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 69/7 = 10

ave.2sc.fedvar<-model.avg(fas.2.setsc.fedvar, subset = delta < 2) 
summary(ave.2sc.fedvar) 
confint(ave.2sc.fedvar, full = TRUE)

## fas 3 ####
fas.3.fedvar <- dens.sub %>% 
  filter(Fas == "peak")

length(fas.3.fedvar$kull[fas.3.fedvar$kull==1]) # 96 kullar under toppår
length(fas.3.fedvar$obsID)

datsc.3.fedvar <- fas.3.fedvar
datsc.3.fedvar[pvars] <- lapply(datsc.3.fedvar[pvars],scale)


fas.3.modell.fedvar <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                          + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + fed + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc.3.fedvar)

stdz.model.3.fedvar <- standardize(fas.3.modell.fedvar, standardize.y = FALSE)
summary(stdz.model.3.fedvar)
car::vif(stdz.model.3.fedvar)
fas.3.setsc.fedvar <- dredge(stdz.model.3.fedvar)


ave.3sc.fedvar<-model.avg(fas.3.setsc.fedvar, subset = delta < 2) 
summary(ave.3sc.fedvar) 
confint(ave.3sc.fedvar, full = TRUE)

length(dens.sub$Namn) #1140 ly-år
length(dens.sub$Namn[dens.sub$fed == "fed"]) #377 ly-år med matning

length(dens.sub$Namn[dens.sub$fed == "fed" & dens.sub$Fas == "peak"]) # 125 ly-år med topp och matning

## Hur många kullar har varit matade? ####

length(dens.sub$Namn[dens.sub$kull==1]) #186 kullar totalt
length(dens.sub$Namn[dens.sub$fed == "fed" & dens.sub$kull==1]) #178 av de kullarna var matade
length(dens.sub$Namn[dens.sub$kull==1 & dens.sub$Fas == "peak" ]) #96 kullar under toppår
length(dens.sub$Namn[dens.sub$fed == "fed" & dens.sub$kull==1 & dens.sub$Fas == "peak" ]) # 91 kullar med matning under toppår. Majoriteten av kullar under toppår är alltså matade
length(dens.sub$Namn[dens.sub$kull==1 & dens.sub$Fas == "increase" ]) # 61 kullar under uppgångsår
length(dens.sub$Namn[dens.sub$fed == "fed" & dens.sub$kull==1 & dens.sub$Fas == "increase" ]) #60 kullar har varit matade under uppgångsår
length(dens.sub$Namn[dens.sub$kull==1 & dens.sub$Fas == "low" ]) # 29 kullar totalt under lågår
length(dens.sub$Namn[dens.sub$fed == "fed" & dens.sub$kull==1 & dens.sub$Fas == "low" ]) # 27 kullar har varit matade
