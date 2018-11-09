
# AIC-analys av lyvariabler

library(data.table)
library(lme4)
library(lubridate)
library(readxl)
library(writexl)
library(MASS)
library(car)
library(tidyverse)
library(MuMIn) # model averaging
library(glmulti) # model averaging med GLM
library(rJava) # om det inte funkar att installera eller starta, installera Java Development kit. Se den här tråden https://github.com/rstudio/rstudio/issues/2254
library(unmarked) # Används i instruktionerna från Rasmus . gör också model averaging och ger en hel del statistik i outputen. 
library('devtools')
library(visreg)
library(sjPlot) # diagnostic plots for linear models
library(DHARMa) # diagnostic plots for generalised mixed models

lydata.long <- read_xlsx(path = "Den and territory selection/Data/lyvariabler.lång.aic.xlsx")

lydata.long<- as.data.frame(lydata.long)
str(lydata.long)
lydata.long$area <- as.numeric(lydata.long$area)
lydata.long$snöfri.area <- as.numeric(lydata.long$snöfri.area)
lydata.long$riktning <- as.factor(lydata.long$riktning)
lydata.long$Namn <- as.factor(lydata.long$Namn)

names(lydata.long)

lydata.long <- lydata.long[!lydata.long$Namn == "FSZZ099", ] # tar bort 99:an. Kommer inte använda den.
View(lydata.long)

#gör om till tre faser
lydata.long$Fas[lydata.long$Fas==4] <- 1
View(lydata.long)
max(lydata.long$Fas) # max är nu 3

#Ändrar om faserna till stringnamn så att R inte råkar läsa dem som siffror (då kan det bli helfel)
lydata.long$Fas[lydata.long$Fas==1] <- "low"
lydata.long$Fas[lydata.long$Fas==2] <- "increase"
lydata.long$Fas[lydata.long$Fas==3] <- "peak"
class(lydata.long$Fas)

lydata.long$Fas <- as.factor(lydata.long$Fas)
head(lydata.long)
#  mutate(area = log(area)) %>% 
#mutate(snöfri.area = log(snöfri.area)) %>% 
#  mutate(ripspillning = log(ripspillning)) %>%
#  mutate(ripor = log(ripspillning)) %>% 
#  mutate(vinkel = log(vinkel))

lydata.sub<- lydata.long %>% 
  dplyr::select(Namn, kull, Fas, uppskattat_antal_ripor,
                uppskattat_antal_ripspillningshögar, area, snöfri.area) %>% 
  dplyr::rename(ripor = uppskattat_antal_ripor) %>% 
  dplyr::rename(ripspillning = uppskattat_antal_ripspillningshögar) %>% 
  mutate(andel.snöfri = snöfri.area/area) %>% 
  dplyr::select(-snöfri.area)
  
View(lydata.sub)
  ?transmute
  #mutate(ripspillning = log(uppskattat_antal_ripspillningshögar)) %>% 
  #dplyr::select(-uppskattat_antal_ripspillningshögar) %>% 
  #mutate(ripor = log(ripor)) %>% 
  #mutate(area = log(area)) %>% 
  #mutate(snöfri.area = log(snöfri.area)) %>% 
   

head(lydata.sub)


str(lydata.sub)

##AIC analys av lydata, alla faser ####


#' Complete separation occurs in a binary-response model when there is some linear combination of the parameters 
#' that perfectly separates failures from successes - for example, when all of the observations are zero for some 
#' particular combination of categories. The symptoms of this problem are unrealistically large parameter estimates; 
#' ridiculously large Wald standard errors (the Hauck-Donner effect); and various warnings

# Felsöker
install.packages("brglm2")
library(brglm2)

#' det blir separation. Jag gissar att jag har för lite data. Jag testar att plocka bort några variabler
glm(kull ~ vinkel+ Fas +riktning + öppning.v + ripor + 
                         ripspillning + area + snöfri.area, na.action = "na.fail", family = binomial(link = 'logit'), 
                            data = lydata.sub, method = "detect_separation")

#' skalar om med sample standard deviations på continuous variables.
#' Börjar med att lägga på en linjär modell med bara de kontinuerliga med

pvars.lya <- c("ripor","ripspillning", "area", "andel.snöfri" )
lysc <- lydata.sub
lysc[pvars.lya] <- lapply(lysc[pvars.lya],scale)
head(lysc)


lydata.modell <- glmer(kull ~ Fas + ripor  + andel.snöfri +
                         ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                       data = lysc)
?standardize

stdz.lydata <- standardize(lydata.modell, standardize.y = FALSE)
car::vif(stdz.lydata)# maxvif är 2.02
summary(stdz.lydata)
sim.output.ly<-simulateResiduals(stdz.lydata)
sim.group.ly<-recalculateResiduals(sim.output.ly, group = lysc$Namn)
testDispersion(sim.group.ly)
testUniformity(sim.group.ly)

lydata.full.set <- dredge(stdz.lydata)

lydata.ave <- model.avg(lydata.full.set, subset = delta < 2)

sumtable.ly<-summary(lydata.ave)

sumtable.ly
nrow(sumtable.ly$msTable)
round(sumtable.ly$coefficients, digits = 3)

round(sumtable.ly$coefmat.full, digits = 3)

## tabell alla faser ####

sumtable.ly$importance

imp.ly<-as.data.frame(sumtable.ly$importance)
imp.ly$`sumtable.ly$importance` # ska kanske ha med det här med i plotten.
imp.ly<-stack(imp.ly) # ger error men det fungerar
imp.ly
imp.ly<-round(imp.ly, digits = 2) #avrundar
imp.ly$Parameters <- as.character(NA)
imp.ly$Parameters<-rownames(imp.ly) # lägger radnamn som kolumn
rownames(imp.ly) <-NULL #tar bort radnamn
class(imp.ly$`sumtable.ly$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp.ly$`sumtable.ly$importance` <- as.numeric(imp.ly$`sumtable.ly$importance`)

full.coefs.ly<-as.data.frame(round(sumtable.ly$coefmat.full, digits = 3))
full.coefs.ly$Parameters <- as.character(NA)
full.coefs.ly$Parameters<-rownames(full.coefs.ly)
rownames(full.coefs.ly)<- NULL

coefs.table.ly <- full.coefs.ly %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table.ly
conf.int.ly<-as.data.frame(round(confint(sumtable.ly, full = TRUE), digits = 3)) # ger konfidensintervall för full average
conf.int.ly$Parameters <- rownames(conf.int.ly)
rownames(conf.int.ly) <- NULL

conf.int.ly <- conf.int.ly %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int.ly

coefs.table.ly <-coefs.table.ly %>% 
  left_join(conf.int.ly, by = "Parameters")

coefs.table.ly

colnames(imp.ly)[1] <- "Relative importance"
imp.ly$Parameters[1] <- "Faslow"

coefs.table.ly <- coefs.table.ly %>% 
  left_join(imp.ly, by = "Parameters")

coefs.table.ly$`Relative importance`[coefs.table.ly$Parameters == "Faspeak"] <- 1.00


coefs.table.ly$Parameters <- c("Intercept", "low phase", "peak phase", "den area", "ptarmigan density", "percent bare ground",
                            "ptarmigan droppings density")
coefs.table.ly <- coefs.table.ly %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table.ly) #7
coefs.table.ly
coefs.table.ly <- coefs.table.ly[c(7,1:6),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table.ly
sumtable.ly$coefmat.full
sumtable.ly$importance
confint(sumtable.ly, full = TRUE)


write_xlsx(coefs.table.ly, path = "Den and territory selection/Plottar/tabell_alla_faser_lydata.xlsx")

## fas 1 separat ####

ly.fas.1 <- lydata.sub %>% 
  filter(Fas == "low")

pvars.lya <- c("ripor","ripspillning", "area", "andel.snöfri" )
lysc.1 <- ly.fas.1
lysc.1[pvars.lya] <- lapply(lysc.1[pvars.lya],scale)
head(lysc.1)




lydata.modell.1 <- glmer(kull ~ ripor +andel.snöfri+
                         ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                       data = lysc.1)

summary(lydata.modell.1) 
?convergence
stdz.lydata.1 <- standardize(lydata.modell.1, standardize.y = FALSE)
summary(stdz.lydata.1)
car::vif(stdz.lydata.1) # max är 3.1

nrow(lysc.1) ## 70
ncol(getME(lydata.modell.1,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 70/4 = 

summary(stdz.lydata.1)

lydata.set.1 <- dredge(stdz.lydata.1)

lydata.ave.1 <- model.avg(lydata.set.1, subset = delta <3)

sumtable.ly.1<-summary(lydata.ave.1)

sumtable.ly.1
nrow(sumtable.ly.1$msTable)

round(confint(sumtable.ly.1), digits = 3)

## tabell fas 1 ####
sumtable.ly.1$importance

imp.ly.1<-as.data.frame(sumtable.ly.1$importance)
imp.ly.1$`sumtable.ly.1$importance` # ska kanske ha med det här med i plotten.
imp.ly.1<-stack(imp.ly.1) # ger error men det fungerar
imp.ly.1
imp.ly.1<-round(imp.ly.1, digits = 2) #avrundar
imp.ly.1$Parameters <- as.character(NA)
imp.ly.1$Parameters<-rownames(imp.ly.1) # lägger radnamn som kolumn
rownames(imp.ly.1) <-NULL #tar bort radnamn
class(imp.ly.1$`sumtable.ly.1$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp.ly.1$`sumtable.ly.1$importance` <- as.numeric(imp.ly.1$`sumtable.ly.1$importance`)

full.coefs.ly.1<-as.data.frame(round(sumtable.ly.1$coefmat.full, digits = 3))
full.coefs.ly.1$Parameters <- as.character(NA)
full.coefs.ly.1$Parameters<-rownames(full.coefs.ly.1)
rownames(full.coefs.ly.1)<- NULL

coefs.table.ly.1 <- full.coefs.ly.1 %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table.ly.1
conf.int.ly.1<-as.data.frame(round(confint(sumtable.ly.1, full = TRUE), digits = 3)) # ger konfidensintervall för full average
conf.int.ly.1$Parameters <- rownames(conf.int.ly.1)
rownames(conf.int.ly.1) <- NULL

conf.int.ly.1 <- conf.int.ly.1 %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int.ly.1

coefs.table.ly.1 <-coefs.table.ly.1 %>% 
  left_join(conf.int.ly.1, by = "Parameters")

coefs.table.ly.1

colnames(imp.ly.1)[1] <- "Relative importance"


coefs.table.ly.1 <- coefs.table.ly.1 %>% 
  left_join(imp.ly.1, by = "Parameters")

coefs.table.ly.1



coefs.table.ly.1$Parameters <- c("Intercept", "den area", "ptarmigan density", "percent bare ground",
                               "ptarmigan droppings density")
coefs.table.ly.1 <- coefs.table.ly.1 %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table.ly.1) #5
coefs.table.ly.1

coefs.table.ly.1 <- coefs.table.ly.1[c(5,1:4),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table.ly.1
sumtable.ly.1$coefmat.full # stämmer
sumtable.ly.1$importance # stämmer
confint(sumtable.ly.1, full = TRUE) #stämmer

write_xlsx(coefs.table.ly.1, path = "Den and territory selection/Plottar/tabell_lågfas_lydata.xlsx")

## fas 2 separat ####

ly.fas.2 <- lydata.sub %>% 
  filter(Fas == "increase")

pvars.lya <- c("ripor","ripspillning", "area", "andel.snöfri" )
lysc.2 <- ly.fas.2
lysc.2[pvars.lya] <- lapply(lysc.2[pvars.lya],scale)
head(lysc.2)



lydata.modell.2 <- glmer(kull ~ ripor + andel.snöfri +
                           ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                         data = lysc.2)
car::vif(lydata.modell.2)
nrow(ly.fas.2) ## 70
ncol(getME(lydata.modell.2,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 70/5 = 
summary(lydata.modell.2)
stdz.lydata.2 <- standardize(lydata.modell.2, standardize.y = FALSE)
summary(stdz.lydata.2)
lydata.set.2 <- dredge(stdz.lydata.2)


lydata.ave.2 <- model.avg(lydata.set.2, subset = delta <3)

sumtable.ly.2<-summary(lydata.ave.2)
sumtable.ly.2
nrow(sumtable.ly.2$msTable)
round(confint(sumtable.ly.2), digits = 3)

## tabell fas 2 ####

sumtable.ly.2$importance

imp.ly.2<-as.data.frame(sumtable.ly.2$importance)
imp.ly.2$`sumtable.ly.2$importance` # ska kanske ha med det här med i plotten.
imp.ly.2<-stack(imp.ly.2) # ger error men det fungerar
imp.ly.2
imp.ly.2<-round(imp.ly.2, digits = 2) #avrundar
imp.ly.2$Parameters <- as.character(NA)
imp.ly.2$Parameters<-rownames(imp.ly.2) # lägger radnamn som kolumn
rownames(imp.ly.2) <-NULL #tar bort radnamn
class(imp.ly.2$`sumtable.ly.2$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp.ly.2$`sumtable.ly.2$importance` <- as.numeric(imp.ly.2$`sumtable.ly.2$importance`)

full.coefs.ly.2<-as.data.frame(round(sumtable.ly.2$coefmat.full, digits = 3))
full.coefs.ly.2$Parameters <- as.character(NA)
full.coefs.ly.2$Parameters<-rownames(full.coefs.ly.2)
rownames(full.coefs.ly.2)<- NULL

coefs.table.ly.2 <- full.coefs.ly.2 %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table.ly.2
conf.int.ly.2<-as.data.frame(round(confint(sumtable.ly.2, full = TRUE), digits = 3)) # ger konfidensintervall för full average
conf.int.ly.2$Parameters <- rownames(conf.int.ly.2)
rownames(conf.int.ly.2) <- NULL

conf.int.ly.2 <- conf.int.ly.2 %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int.ly.2

coefs.table.ly.2 <-coefs.table.ly.2 %>% 
  left_join(conf.int.ly.2, by = "Parameters")

coefs.table.ly.2

colnames(imp.ly.2)[1] <- "Relative importance"


coefs.table.ly.2 <- coefs.table.ly.2 %>% 
  left_join(imp.ly.2, by = "Parameters")

coefs.table.ly.2



coefs.table.ly.2$Parameters <- c("Intercept", "den area", "ptarmigan density","ptarmigan droppings density", "percent bare ground"
                                 )
coefs.table.ly.2 <- coefs.table.ly.2 %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table.ly.2) #5
coefs.table.ly.2
sumtable.ly.2$coefmat.full
coefs.table.ly.2 <- coefs.table.ly.2[c(5,1:4),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table.ly.2
sumtable.ly.2$coefmat.full #stämmer
sumtable.ly.2$importance #stämmer
confint(sumtable.ly.2, full = TRUE) # stämmer


write_xlsx(coefs.table.ly.2, path = "Den and territory selection/Plottar/tabell_uppgångsfas_lydata.xlsx")

## fas 3 separat ####

ly.fas.3 <- lydata.sub %>% 
  filter(Fas == "peak")
pvars.lya <- c("ripor","ripspillning", "area", "andel.snöfri" )
lysc.3 <- ly.fas.3
lysc.3[pvars.lya] <- lapply(lysc.3[pvars.lya],scale)
head(lysc.3)

View(ly.fas.3)


lydata.modell.3 <- glmer(kull ~  ripor   + andel.snöfri+
                           ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                         data = lysc.3)
car::vif(lydata.modell.3)
stdz.lydata.3 <- standardize(lydata.modell.3, standardize.y = FALSE)

nrow(ly.fas.3) ## 50
ncol(getME(lydata.modell.3,"X")) # 5, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 50/5 = 10


lydata.set.3<- dredge(stdz.lydata.3)
lydata.ave.3 <-model.avg(lydata.set.3, subset = delta < 3)


sumtable.ly.3<-summary(lydata.ave.3)
sumtable.ly.3

nrow(sumtable.ly.3$msTable)

round(sumtable.ly.3$coefficients, digits = 3)
round(confint(sumtable.ly.3), digits = 3)

## tabell fas 3 ####
sumtable.ly.3$importance

imp.ly.3<-as.data.frame(sumtable.ly.3$importance)
imp.ly.3$`sumtable.ly.3$importance` # ska kanske ha med det här med i plotten.
imp.ly.3<-stack(imp.ly.3) # ger error men det fungerar
imp.ly.3
imp.ly.3<-round(imp.ly.3, digits = 2) #avrundar
imp.ly.3$Parameters <- as.character(NA)
imp.ly.3$Parameters<-rownames(imp.ly.3) # lägger radnamn som kolumn
rownames(imp.ly.3) <-NULL #tar bort radnamn
class(imp.ly.3$`sumtable.ly.3$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp.ly.3$`sumtable.ly.3$importance` <- as.numeric(imp.ly.3$`sumtable.ly.3$importance`)

full.coefs.ly.3<-as.data.frame(round(sumtable.ly.3$coefmat.full, digits = 3))
full.coefs.ly.3$Parameters <- as.character(NA)
full.coefs.ly.3$Parameters<-rownames(full.coefs.ly.3)
rownames(full.coefs.ly.3)<- NULL

coefs.table.ly.3 <- full.coefs.ly.3 %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table.ly.3
conf.int.ly.3<-as.data.frame(round(confint(sumtable.ly.3, full = TRUE), digits = 3)) # ger konfidensintervall för full average
conf.int.ly.3$Parameters <- rownames(conf.int.ly.3)
rownames(conf.int.ly.3) <- NULL

conf.int.ly.3 <- conf.int.ly.3 %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int.ly.3

coefs.table.ly.3 <-coefs.table.ly.3 %>% 
  left_join(conf.int.ly.3, by = "Parameters")

coefs.table.ly.3

colnames(imp.ly.3)[1] <- "Relative importance"


coefs.table.ly.3 <- coefs.table.ly.3 %>% 
  left_join(imp.ly.3, by = "Parameters")

coefs.table.ly.3



coefs.table.ly.3$Parameters <- c("Intercept", "den area","percent bare ground", "ptarmigan density","ptarmigan droppings density")

coefs.table.ly.3 <- coefs.table.ly.3 %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table.ly.3) #5
coefs.table.ly.3
sumtable.ly.3$coefmat.full
coefs.table.ly.3 <- coefs.table.ly.3[c(5,1:4),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table.ly.3
sumtable.ly.3$coefmat.full # stämmer
sumtable.ly.3$importance #stämmer
confint(sumtable.ly.3, full = TRUE) # stämmer

write_xlsx(coefs.table.ly.3, path = "Den and territory selection/Plottar/tabell_toppfas_lydata.xlsx")

##plottar alla faser tillsammans ####
plot.ly1<-read_xlsx(path = "Den and territory selection/Plottar/tabell_lågfas_lydata.xlsx")
plot.ly2<-read_xlsx(path = "Den and territory selection/Plottar/tabell_uppgångsfas_lydata.xlsx")
plot.ly3<-read_xlsx(path = "Den and territory selection/Plottar/tabell_toppfas_lydata.xlsx")


plot.ly1<-plot.ly1 %>% 
  slice(-1) # tar bort interceptet
plot.ly1
plot.ly1$Estimate <- paste0('Est. = ', plot.ly1$Estimate) # lägger till "Est. =" i början på alla rader
plot.ly1$`Unconditional SE` <- paste0('U.SE = ', plot.ly1$`Unconditional SE`)
plot.ly1$`Confidence interval` <- paste0('C.I = ', plot.ly1$`Confidence interval`)

plot.ly2<-plot.ly2 %>% 
  slice(-1) # tar bort interceptet
plot.ly2
plot.ly2$Estimate <- paste0('Est. = ', plot.ly2$Estimate) # lägger till "Est. =" i början på alla rader
plot.ly2$`Unconditional SE` <- paste0('U.SE = ', plot.ly2$`Unconditional SE`)
plot.ly2$`Confidence interval` <- paste0('C.I = ', plot.ly2$`Confidence interval`)

plot.ly3<-plot.ly3 %>% 
  slice(-1) # tar bort interceptet
plot.ly3
plot.ly3$Estimate <- paste0('Est. = ', plot.ly3$Estimate) # lägger till "Est. =" i början på alla rader
plot.ly3$`Unconditional SE` <- paste0('U.SE = ', plot.ly3$`Unconditional SE`)
plot.ly3$`Confidence interval` <- paste0('C.I = ', plot.ly3$`Confidence interval`)
plot.ly3
#fas1
g1.ly<-ggplot(data=plot.ly1, aes(x=Parameters, y=`Relative importance`, fill=`Relative importance`)) +
  scale_x_discrete(limits = plot.ly1$Parameters) + #säger år ggplot att hålla samma ordning på kolumnerna som i dataramen
  geom_bar(stat="identity", width=0.9)+
  geom_text(aes(label= plot.ly1$Estimate), vjust=-4, color="black", size = 6)+ #vjust är position i höjdled
  geom_text(aes(label= plot.ly1$`Unconditional SE`), vjust=-2.5, color="black", size = 6)+
  geom_text(aes(label= plot.ly1$`Confidence interval`), vjust=-1, color="black", size = 6)+
  theme_minimal()


g1.ly<-g1.ly+theme(axis.text=element_text(size=16, color = "black"), # ändrar stapeltextens storlek
               axis.title.x=element_blank(), axis.title.y=element_blank())+ # tar bort axeltitlarna
  annotate(geom = "label", x = 3, y = 0.85,
           label = "Intercept\nEst. =   -3.52\nU.SE = 1.05\nC.I = -5.612, -1.433", # geom = "label" gör en ruta runt texten. geom = "text" ger bara text utan ruta. \n betyder ny rad.
           hjust = 0, size = 6) + # vänsterjusterar texten i boxen
  coord_cartesian(ylim=c(0, 1.35))+ # sätter plottens zoom. Jag vill ha utrymme ovanför staplarna så att texten får plats
  scale_y_continuous(breaks=seq(0, 1, 0.2)) + # sätter min och maxvärdena för vad som ska visas på y-axeln (inte datat, bara axeln). 0.2 är intervallet. 
  guides(fill=FALSE) # tar bort färgstapeln till höger
g1.ly

#fas2
plot.ly2
g2.ly<-ggplot(data=plot.ly2, aes(x=Parameters, y=`Relative importance`, fill=`Relative importance`)) +
  scale_x_discrete(limits = plot.ly2$Parameters) + #säger år ggplot att hålla samma ordning på kolumnerna som i dataramen
  geom_bar(stat="identity", width=0.9)+
  geom_text(aes(label= plot.ly2$Estimate), vjust=-4, color="black", size = 6)+
  geom_text(aes(label= plot.ly2$`Unconditional SE`), vjust=-2.5, color="black", size = 6)+
  geom_text(aes(label= plot.ly2$`Confidence interval`), vjust=-1, color="black", size = 6)+
  theme_minimal()

g2.ly<-g2.ly+theme(axis.text=element_text(size=16, color = "black"), # ändrar stapeltextens storlek
               axis.title.x=element_blank(), axis.title.y=element_blank())+ # tar bort axeltitlarna
  annotate(geom = "label", x = 3, y = 0.85,
           label = "Intercept\nEst. = --2.87\nU.SE =  1.19\nC.I =  -5.239, -0.498", # geom = "label" gör en ruta runt texten. geom = "text" ger bara text utan ruta. \n betyder ny rad.
           hjust = 0, size = 6) + # vänsterjusterar texten
  coord_cartesian(ylim=c(0, 1.35))+ # sätter plottens zoom. Jag vill ha utrymme ovanför staplarna så att texten får plats
  scale_y_continuous(breaks=seq(0, 1, 0.2))+ # sätter min och maxvärdena för vad som ska visas på y-axeln (inte datat, bara axeln). 0.2 är intervallet. 
  guides(fill=FALSE) # tar bort färgstapeln till höger

g2.ly

#fas 3
plot.ly3
g3.ly<-ggplot(data=plot.ly3, aes(x=Parameters, y=`Relative importance`, fill=`Relative importance`)) +
  scale_x_discrete(limits = plot.ly3$Parameters) + #säger år ggplot att hålla samma ordning på kolumnerna som i dataramen
  geom_bar(stat="identity", width=0.9)+
  geom_text(aes(label= plot.ly3$Estimate), vjust=-4, color="black", size = 6)+
  geom_text(aes(label= plot.ly3$`Unconditional SE`), vjust=-2.5, color="black", size = 6)+
  geom_text(aes(label= plot.ly3$`Confidence interval`), vjust=-1, color="black", size = 6)+
  theme_minimal()

g3.ly<-g3.ly+theme(axis.text=element_text(size=16, color = "black"), # ändrar stapeltextens storlek
               axis.title.x=element_blank(), axis.title.y=element_blank())+ # tar bort axeltitlarna
  annotate(geom = "label", x = 3, y = 0.85,
           label = "Intercept\nEst. = -0.81\nU.SE =  0.495 \nC.I =  -1.806, 0.185 ", # geom = "label" gör en ruta runt texten. geom = "text" ger bara text utan ruta. \n betyder ny rad.
           hjust = 0, size = 6) + # vänsterjusterar texten
  coord_cartesian(ylim=c(0, 1.30))+ # sätter plottens zoom. Jag vill ha utrymme ovanför staplarna så att texten får plats
  scale_y_continuous(breaks=seq(0, 1, 0.2)) +  # sätter min och maxvärdena för vad som ska visas på y-axeln (inte datat, bara axeln). 0.2 är intervallet. 
  guides(fill=FALSE) # tar bort färgstapeln till höger
g3.ly


# lägg ihop
theme_set(theme_pubr())
comboplot.lya <- ggarrange(g1.ly, g2.ly, g3.ly,
                       labels = c("low phase", "increase phase", "peak phase"),
                       font.label = list(size = 17, color = "black", face = "bold"),
                       ncol = 1, nrow = 3)
comboplot.lya
comboplot.lya<-annotate_figure(comboplot.lya,
                                bottom = text_grob("Parameters in selected models",
                                                   face = "bold", size = 20),
                                left = text_grob("Relative importance", 
                                                 face = "bold", rot = 90, size = 20))
?ggarrange              
comboplot.lya             
ggexport(comboplot.lya, filename = "comboplot.lya.jpeg", width = 6100, height = 7500, res = 400)
?ggexport