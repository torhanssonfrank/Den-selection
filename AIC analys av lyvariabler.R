
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
                uppskattat_antal_ripspillningshögar, area, snöfri.area, öppning.v) %>% 
  dplyr::rename(ripor = uppskattat_antal_ripor) %>% 
  dplyr::rename(ripspillning = uppskattat_antal_ripspillningshögar) %>% 
  mutate(andel.snöfri = snöfri.area/area)
  
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

pvars.lya <- c("ripor","ripspillning", "area", "snöfri.area", "öppning.v","andel.snö" )
lysc <- lydata.sub
lysc[pvars.lya] <- lapply(lysc[pvars.lya],scale)
head(lysc)


lydata.modell <- glmer(kull ~ Fas + ripor  + andel.snö +
                         ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                       data = lysc)
?standardize
stdz.lydata
stdz.lydata <- standardize(lydata.modell, standardize.y = FALSE)
car::vif(stdz.lydata)# maxvif är 1.54
summary(stdz.lydata)

lydata.full.set <- dredge(stdz.lydata)

lydata.ave <- model.avg(lydata.full.set, subset = delta < 2)

sumtable.ly<-summary(lydata.ave)

sumtable.ly
sumtable.ly$coefficients



## fas 1 separat ####

ly.fas.1 <- lydata.sub %>% 
  filter(Fas == "low")

pvars.lya <- c("ripor","ripspillning", "area", "snöfri.area", "öppning.v", "andel.snö")
lysc.1 <- ly.fas.1
lysc.1[pvars.lya] <- lapply(lysc.1[pvars.lya],scale)
head(lysc.1)



library(optimx)
lydata.modell.1 <- glmer(kull ~ ripor +andel.snö+
                         ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                       data = lysc.1)
#' får en varning bär jag lägger på modellen, även när jag har skalat om. Kanske är för få kullar. Testade med flera optimisiers
#' men det fungerade inte. Tar bort ripor ur analysen. Det är enklast
summary(lydata.modell.1) # det är inte singularity. INgen variance på 0 och ingen collinearity på 1.
?convergence
stdz.lydata.1 <- standardize(lydata.modell.1, standardize.y = FALSE)
summary(stdz.lydata.1)
car::vif(stdz.lydata.1)
ss$which.OK

## 2. check singularity
diag.vals <- getME(stdz.lydata.1,"theta")[getME(lydata.modell.1,"lower") == 0]
any(diag.vals < 1e-6) # TRUE


## 3. recompute gradient and Hessian with Richardson extrapolation. Det fungerade inte
devfun <- update(lydata.modell.1, devFunOnly=TRUE)
if (isLMM(lydata.modell.1)) {
  pars <- getME(lydata.modell.1,"theta")
} else {
  ## GLMM: requires both random and fixed parameters
  pars <- getME(lydata.modell.1, c("theta","fixef"))
}
if (require("numDeriv")) {
  cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
  cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
  cat("scaled gradient:\n")
  print(scgrad <- solve(chol(hess), grad))
}
## compare with internal calculations:
lydata.modell.1@optinfo$derivs

## 4. restart the fit from the original value (or
## a slightly perturbed value):
fm1.restart <- update(lysc.1, start=pars)




source(system.file("utils", "allFit.R", package="lme4"))
fm1.all <- allFit(lydata.modell.1)
ss<-summary(fm1.all)
ss$which.OK # funkade inte med några optimisers heller.

nrow(lysc.1) ## 70
ncol(getME(lydata.modell.1,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 70/4 = 

summary(stdz.lydata.1)

lydata.set.1 <- dredge(stdz.lydata.1)

lydata.ave.1 <- model.avg(lydata.set.1, subset = delta < 2)

sumtable.ly.1<-summary(lydata.ave.1)

sumtable.ly.1


## fas 2 separat ####

ly.fas.2 <- lydata.sub %>% 
  filter(Fas == "increase")

pvars.lya <- c("ripor","ripspillning", "area", "snöfri.area", "öppning.v", "andel.snö")
lysc.2 <- ly.fas.2
lysc.2[pvars.lya] <- lapply(lysc.2[pvars.lya],scale)
head(lysc.2)



lydata.modell.2 <- glmer(kull ~ ripor + andel.snö +
                           ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                         data = lysc.2)
car::vif(lydata.modell.2)
nrow(ly.fas.2) ## 70
ncol(getME(lydata.modell.2,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 70/5 = 
summary(lydata.modell.2)
stdz.lydata.2 <- standardize(lydata.modell.2, standardize.y = FALSE)
summary(stdz.lydata.2)
lydata.set.2 <- dredge(lydata.modell.2)


lydata.ave.2 <- model.avg(lydata.set.2, subset = delta < 6)
sumtable.ly.2psd<-summary(lydata.ave.2.psd)
sumtable.ly.2<-summary(lydata.ave.2)

sumtable.ly.2
sumtable.ly.2psd

## fas 3 separat ####

ly.fas.3 <- lydata.sub %>% 
  filter(Fas == "peak")
pvars.lya <- c("ripor","ripspillning", "area", "snöfri.area", "öppning.v", "andel.snö")
lysc.3 <- ly.fas.3
lysc.3[pvars.lya] <- lapply(lysc.3[pvars.lya],scale)
head(lysc.3)

View(ly.fas.3)


lydata.modell.3 <- glmer(kull ~  ripor   + andel.snö+
                           ripspillning + area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                         data = lysc.3)
car::vif(lydata.modell.3)
stdz.lydata.3 <- standardize(lydata.modell.3, standardize.y = FALSE)

nrow(ly.fas.3) ## 50
ncol(getME(lydata.modell.3,"X")) # 5, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 50/5 = 10

ly.psd.3 <- partial.sd(lydata.modell.3)

ly.psd.3
ly.psd.3[-1]
View(lydata.sub)
z.lydata.3 <- stdize(lydata.sub, scale = c(NA,NA,NA, ly.psd.3[-1]), center =  c(FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE)) # centrerar till mean 0. NA, NA, NA och NA tar bort responsvariabeln, Namn, År och Fas. Samma för FALSE, FALSE, FALSE, FALSE. Kull och År ska inte ändras och Namn kan inte ändras. binary = "omit" fungerade inte.
z.lydata.3

z.lydata.modell.3 <- glmer(kull ~  z.ripor + z.öppning.v +
                             z.ripspillning + z.area + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'),
                           data = z.lydata.3)
summary(z.lydata.modell.3)
lydata.set.3 <- dredge(z.lydata.modell.3)
lydata.set.3
lydata.set.3.PSD <- dredge(z.lydata.modell.3, beta = "partial.sd")
lydata.ave.3.psd <-model.avg(lydata.set.3.PSD, subset = delta < 6)
lydata.ave.3 <- model.avg(lydata.set.3, subset = delta < 6)
sumtable.ly.3psd<-summary(lydata.ave.3.psd)
sumtable.ly.3<-summary(lydata.ave.3)

round(sumtable.ly.3$coefficients)
sumtable.ly.3psd
sumtable.ly.3X
sumtable.ly.3psdX
