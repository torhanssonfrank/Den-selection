install.packages("lme4")
install.packages("openxlsx") # det här paketet ingår i car. Det installerades inte när jag installerade car så då funkar inte car. Det funkar när jag installerar det separat
install.packages("car")
install.packages("glmulti")
install.packages("rJava") # om det inte funkar att installera eller starta, installera Java Development kit. Se den här tråden https://github.com/rstudio/rstudio/issues/2254
install.packages("tidyverse")
install.packages("rgeos")
install.packages("sp")
install.packages("sf")
install.packages("rgdal")
install.packages("readxl")
install.packages("writexl")
install.packages("MuMIn")
install.packages("visreg")
install.packages("nloptr") #behövs för lme4 men funkar inte
install_github("jyypma/nloptr") # funkar om man laddar ner från github direkt
install.packages("devtools")
install.packages("unmarked")
install.packages ("arm")
install.packages("sjPlot")
install.packages("DHARMa")

sessionInfo()

dir.exists(file.path(".R", "Makevars"))

cat(readLines("/.R/Makevars"), sep = '\n')
install.packages("data.table")

.libPaths() # ger pathway till paketen

sessionInfo()  ## see if you have a *clean* session (your session above isn't clean, it has non-default
## packages loaded)
packageVersion("nlme")
sapply(.libPaths(),packageVersion,pkg="nlme")  ## see if you have multiple versions installed
require("lme4")

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

update.packages(ask = FALSE) #flyttade över paketen från mappen för den äldre R-versionen till den nya, sen uppdatera. Se här https://stackoverflow.com/questions/13656699/update-r-using-rstudio



# AIC-analys av kärnlyor i Helagspopulationen

dens <- read_xlsx(path = "kärnlyor Helags AIC 2000 - 2018.xlsx")

dens <- as.data.frame(dens)

str(dens) # hojd_over_havet blev sparat som character


hist(dens$närmaste_rödräv, breaks = 100) # flera parametrar verkar inte vara normalfördelade

par(mfrow=c(1,2))
qqPlot(dens$avs_kull)
qqPlot(dens$avs_kull, "lnorm")
which(is.na(dens$avs_kull))
ak.1 <- na.exclude(dens$avs_kull)  # tar bort NA's, annars funkar inte nästa steg
nbinom <- fitdistr(ak.1, "Negative Binomial") # funkar inte för att värdena är långt från 1. Negative bionomial är inte en passande fit. Poission och gamma funkade inte heller
qqp(ak.1, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # funkar alltså inte heller
avs_kull.log <- log(dens$avs_kull)
qqPlot(avs_kull.log) # blev inte mycket bättre
qqPlot(dens$närmaste_rödräv)
qqPlot(dens$närmaste_rödräv, "lnorm")
nr.log <- log(dens$närmaste_rödräv)
qqPlot(nr.log) # aningen bättre men inte perfekt


qqPlot(dens$medelvärde_lämmelprediktion_uppgångsår)
qqPlot(dens$medelvärde_lämmelprediktion_uppgångsår, "lnorm")# inte så mycket bättre
mlu.log <- log(dens$medelvärde_lämmelprediktion_uppgångsår)
qqPlot(mlu.log) # inte så mycket bättre


qqPlot(dens$andel_bra_lämmelhabitat_uppgångsår)
qqPlot(dens$andel_bra_lämmelhabitat_uppgångsår, "lnorm")
qqline(dens$andel_bra_lämmelhabitat_uppgångsår)
ablu.log <- log(dens$andel_bra_lämmelhabitat_uppgångsår)
qqPlot(ablu.log)  #sämre


qqPlot(dens$lemmel_var)
qqPlot(dens$lemmel_var, "lnorm") # inte mycket bättre
lv.log <- log(dens$lemmel_var)
qqPlot(lv.log) # inte mycket bättre

rd.1 <- dens$rödräv_densitet+1
qqPlot(rd.1)
qqPlot(rd.1, "lnorm")
rd.log <- log((dens$rödräv_densitet+1)) # måste lägga till 1 på alla värden eftersom det finns 0-värden. logaritmen av 0 är inf
qqPlot(rd.log) # lite bättre


qqPlot(dens$hojd_over_havet) #den är ok men dålig på de lägre och högre kvantilerna
qqPlot(dens$hojd_over_havet, "lnorm") # sämre
hoh.log <- log(dens$hojd_over_havet)
qqPlot(hoh.log)# ingen förbättring


qqPlot(dens$area_myr)
qqPlot(dens$area_myr, "lnorm") # ingen förbättring
am.log <- log((dens$area_myr+1))
qqPlot(am.log)#ingen förbättring


qqPlot(dens$area_vatten)
qqPlot(dens$area_vatten, "lnorm") # aningen bättre men dålig
av.log <- log(dens$area_vatten)
qqPlot(av.log)# aningen bättre


qqPlot(dens$distans_till_vatten)
qqPlot(dens$distans_till_vatten, "lnorm") #dålig
dtv.log <- log(dens$distans_till_vatten)
qqPlot(dtv.log)  # aningen bättre


qqPlot(dens$distans_till_skog) # hyfsad men dålig fit i lägre och högre kvantiler
qqPlot(dens$distans_till_skog, "lnorm") # sämre
dts.log <- log(dens$distans_till_skog)
qqPlot(dts.log)# sämre

#' Jag ska ha 3 faser, inte 4. 4 och 1 är låg. 2 är uppgång och 3 är topp

dens$Fas[dens$Fas==4] <- 1
View(dens)
max(dens$Fas) # max är 3

#Ändrar om faserna till stringnamn så att R inte råkar läsa dem som siffror (då kan det bli helfel)
dens$Fas[dens$Fas==1] <- "low"
dens$Fas[dens$Fas==2] <- "increase"
dens$Fas[dens$Fas==3] <- "peak"
class(dens$Fas)

# testar några anovor. Verkar dock bara bli chi square för glmer-objekt.
hojd <- glmer(kull ~ hojd_over_havet + (1 | Namn), data = dens, family = binomial(link = 'logit'))
summary(hojd)
Anova(hojd)

lem <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår + (1 | Namn), data = dens, family = binomial(link = 'logit'))
Anova(lem)


#' Min responsvariabel är  binär (antingen kull eller ingen kull). Då kan man inte använda
#' linear model eller penalized quasi likelihood (PQL)
#' mina förklarande variabler är inte normalfördelade. 
#' Mina residualer är alltså inte normalfördelade.
#'  Därför är det bäst att använda en generalised linear model med mixed effects. Om
#'  man har 5 eller fler random variabler ska man använda Monte Carlo algorithms (MCMC).
#'  Jag kommer bara köra på lya (Namn) som random. Kommer även testa år.
#'  Det verkar inte som att man anger random variabler på samma sätt i AIC-analyser som i övriga linjära analyser.
#'  Jag tror i alla fall att jag klarar mig med glmer() istället för MCMC (den är baserad på Bayesian likelihood 
#'  så då kanske det inte funkar med AIC? BIC kanske funkar i så fall.)
#'  En bra guide finns här: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#' Tar först bort N, E, År, distans till rödräv, lemmel_var och andel bra lämmelhabitat
#' eftersom jag inte ska ha med dem i analysen. distans till rödräv innehåller dessutom 
#' många NA's.


View(dens)
dens.sub <- dens %>%
  dplyr::select(-N, -E, -avs_kull, -närmaste_rödräv, -lemmel_var, -andel_bra_lämmelhabitat_uppgångsår)


#tar bort rader med NA's. Tror dock inte det finns några.
dens.sub <- dens.sub[complete.cases(dens.sub), ]
View(dens.sub)
class(dens$Namn)
dens.sub$Namn <- as.factor(dens.sub$Namn) #måste vara factor för att kunna analyseras
dens.sub$År <- as.factor(dens.sub$År)
dens.sub$Fas <- as.factor(dens.sub$Fas)
names(dens.sub)
rownames(dens.sub) <- NULL





## *************GENERALISED LINEAR MODELS - MULTIMODEL AVERAGING***************** ####


#' får varning att variablerna är på för olika skala. Nä jag kör glmer 1: Some predictor variables are on very different scales: consider rescaling
#' Förklaring till problemet finns här: https://stackoverflow.com/questions/26904580/error-messages-when-running-glmer-in-r
#' Skalar om
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "hojd_over_havet","area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc <- dens.sub
datsc[pvars] <- lapply(datsc[pvars],scale)

?scale
## ***************** ALLA FASER *********************####

#' testar glmer. ska vara 3.Trots att Namn är random variable blir det för många frihetsgrader.
#' boxcox() kan ändra responsvariabeln om min globala modell inte 
#' uppfyller kraven för linjär regression. Instruktioner: https://www.r-bloggers.com/on-box-cox-transform-in-regression-models/
#' Zuur et al 2010 rekommenderar dock inte att man ändrar responsvariabeln.

#' tar 
global.modell <- glmer(kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
            + rödräv_densitet
            + area_myr + area_vatten + distans_till_vatten
            + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
          data = datsc) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper
library(DHARMa)
library(sjPlot)
plot_model(global.modell, type = "slope")
plot_model(global.modell, type = "diag") # qq-plot for kvantilerna för random effekt mot standard normalkvantiler
plot_model(global.modell, type = "re")
plot_model(global.modell, type = "resid" )
simulateResiduals(global.modell, plot = TRUE)
sim.output<-simulateResiduals(global.modell)
plot(sim.output, quantreg = FALSE)
sim.group<-recalculateResiduals(sim.output, datsc$Namn)
plot(sim.group, quantreg = FALSE)
vif(global.modell)
class(global.modell)

summary(global.modell)
#' Jag fick samma varning, men det funkar: This works, 
#' although we get a warning message about a too-large gradient -- I think 
#' this is actually ignorable (we're still working on getting these error 
#' sensitivity thresholds right)

library(arm)
stdz.model <- standardize(global.modell, standardize.y = FALSE)
## increases max gradient -- larger warning
library(MuMIn)
model.set <- dredge(stdz.model)  ## slow, but running ...


summary(model.set)
summary(model.avg(model.set, subset = delta < 4)) # skillnaden mellan modellen med lägst AIC och den modellen med högst AIC som väljs
Weights(model.set)

par(mfrow=c(1,1))
par(mar = c(3,5,6,4))
plot(model.set, labAsExpr = TRUE)
m.ave<-model.avg(model.set, subset = delta < 2) 
summary(m.ave)

#Den här modellen var bäst, men inte klart bäst
best_model <- glmer(kull~ factor(Fas)+ area_myr + distans_till_skog + (1 | Namn), 
                  na.action = "na.fail", family = binomial(link = 'logit'), data = datsc)
plot(best_model) # plottar ej. Varför?
confset.95p <- get.models(model.set, cumsum(weight) <= .95) # alla vikters summa som tas med ska vara 0.95. Alltså 95% sannolikhet för 

avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)
warnings() # Alla varningar kan igoreras. Det är bara  too-large gradient (max grad)

#Vet inte riktigt vad outputen nedan betyder
# Force re-fitting the component models
model.avg(d.modell, cumsum(weight) <= .95, fit = TRUE)
# Models are also fitted if additional arguments are given
model.avg(d.modell, cumsum(weight) <= .95, rank = "AIC")



## Testar att lägga in År som en random effect eftersom jag mäter per år ####
global.modell.år <- glmer(kull ~ Fas +  medelvärde_lämmelprediktion_uppgångsår
                         + rödräv_densitet
                          + area_myr + area_vatten + distans_till_vatten
                          + distans_till_skog + (1 | Namn) + (1 | År), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = datsc)

stdz.model.år <- standardize(global.modell.år, standardize.y = FALSE)
## increases max gradient -- larger warning
library(MuMIn)
model.set.år <- dredge(stdz.model.år)  ## slow, but running ...


summary(model.set.år)
summary(model.avg(model.set.år, subset = delta < 4)) # skillnaden mellan modellen med lägst AIC och den modellen med högst AIC som väljs
Weights(model.set.år)

par(mfrow=c(1,1))
par(mar = c(3,5,6,4))
plot(model.set.år, labAsExpr = TRUE)
m.ave.år<-model.avg(model.set.år, subset = delta < 2) 
summary(m.ave.år) # ingen större skillnad. Bara att lågår inte är signifikant längre. annars är bara distans till skog och peak signifikant, precis som innan.


#' Fas är väldigt signifikant.
#' Testar därför att analyser de tre faserna separat


## Fas 1 ####

fas.1 <- dens.sub %>% 
  filter(Fas == "low")

length(fas.1$kull[fas.1$kull==1]) # 29 kullar under lågår
length(fas.1$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 


# Testar att skala om också så att det blir samma resultat
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "hojd_over_havet","area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.1 <- fas.1
datsc.1[pvars] <- lapply(datsc.1[pvars],scale)

fas.1.modellsc <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.1)

stdz.model.1 <- standardize(fas.1.modellsc, standardize.y = FALSE)
fas.1.setsc <- dredge(stdz.model.1)



par(mar = c(3,5,6,4))
plot(fas.1.setsc, labAsExpr = TRUE)
ave.1sc<-model.avg(fas.1.setsc, subset = delta < 2) 
summary(ave.1sc) # blir samma sak som den oskalade datan



## Fas 2 ####
fas.2 <- dens.sub %>% 
  filter(Fas == "increase")
length(fas.2$kull[fas.1$kull==1])
length(fas.2$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 

#skalar om
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "hojd_over_havet","area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.2 <- fas.2
datsc.2[pvars] <- lapply(datsc.2[pvars],scale)
View(datsc.2)

fas.2.modell <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.2) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model.2 <- standardize(fas.2.modell, standardize.y = FALSE)
fas.2.set <- dredge(stdz.model.2)

par(mar = c(3,5,6,4))
plot(fas.2.set, labAsExpr = TRUE)
ave.2sc<-model.avg(fas.2.set, subset = delta < 2) 
summary(ave.2sc)
plot(datsc.2$area_myr, datsc.2$kull)

# Myr blev signifikant här. Kollar om det finns outliers.
hist(dens.sub$area_myr, breaks = 100)
# Det gör det. ZZ084 är en stor oulier med enormt mycket myr.
outlier<-subset(dens.sub, area_myr>1000000)
outlier
unique(outlier$Namn)
mindre.myr <- dens.sub[ !(dens.sub$area_myr > 1000000), ]
hist(mindre.myr$area_myr, breaks = 100)

# Testar igen utan myr-outliern
fas.2.mm <- mindre.myr %>% 
  filter(Fas == "increase")
length(fas.2.mm$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 

#skalar om
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "hojd_over_havet","area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.2.mm <- fas.2.mm
datsc.2.mm[pvars] <- lapply(fas.2.mm[pvars],scale)
View(datsc.2.mm)

fas.2.modell.mm <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.2.mm) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

stdz.model.2.mm <- standardize(fas.2.modell.mm, standardize.y = FALSE)
fas.2.set.mm <- dredge(stdz.model.2.mm)

par(mar = c(3,5,6,4))
plot(fas.2.set.mm, labAsExpr = TRUE)
ave.2sc.mm<-model.avg(fas.2.set.mm, subset = delta < 2) 
summary(ave.2sc.mm)

## Fas 3 ####

fas.3 <- dens.sub %>% 
  filter(Fas == "peak")
length(fas.3$kull[fas.3$kull == 1])
View(fas.3)
length(fas.3$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 
#skalar om
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "hojd_over_havet","area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
datsc.3 <- fas.3
datsc.3[pvars] <- lapply(datsc.3[pvars],scale)

fas.3.modell <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = datsc.3) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper


stdz.model.3 <- standardize(fas.3.modell, standardize.y = FALSE)
fas.3.set <- dredge(stdz.model.3)

par(mar = c(3,5,6,4))
plot(fas.3.set, labAsExpr = TRUE)
ave.3sc<-model.avg(fas.3.set, subset = delta < 2) 
sumtable.3<-summary(ave.3sc)
par(mar = c(3,6,6,1))
?margin
ggdf<-as.data.frame(sumtable.3$coef.nmod)

ggdf<-ggdf %>% 
  dplyr::select(-`(Intercept)`)
barplot(ggdf, col = "red", cex.names = 0.6 )



ggplot(data=ggdf, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
##' ****** Kod jag testat runt lite med **********#########
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 
##' 


# Med glmulti
summary(global.modell)
kull.modell <- glmulti(global.modell, level = 1, crit="aicc")
d.modell <- dredge(global.modell)

summary(kull.modell)

?glmer()
x<-weightable(kull.modell)
x


#plockar ut modellerna med delta-aicc under 2 för att göra model averaging
x$delta <- x$aicc - x$aicc[1]

top.models <- subset(x, x$delta < 2)

length(top.models$model) # 12 modeller
#måste spara dem manuellt
top.models$model[1]

f1 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + 
            lemmel_var + area_vatten + distans_till_skog, family = binomial(), data = dens.sub)

top.models$model[2]
f2 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
            hojd_over_havet + area_myr + area_vatten + distans_till_skog, family = binomial(),data = dens.sub)

top.models$model[3]
f3 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + 
            area_vatten + distans_till_skog,family = binomial(), data = dens.sub)
top.models$model[4]

f4 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
            hojd_over_havet + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)
top.models$model[5]
f5<- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + hojd_over_havet + 
  area_myr + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[6]
f6<-glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
  area_myr + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[7]
f7<- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + 
           hojd_over_havet + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[8]
f8 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + area_myr + 
  area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[9]
f9 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
  rödräv_densitet + hojd_over_havet + area_myr + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[10]
f10 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
  area_vatten + distans_till_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[11]
f11 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + lemmel_var + 
  rödräv_densitet + area_vatten + distans_till_skog,family = binomial(), data = dens.sub)

top.models$model[12]
f12 <- glm(kull ~ 1 + Fas + avs_kull + medelvärde_lämmelprediktion_uppgångsår + area_vatten + 
  distans_till_vatten + distans_till_skog,family = binomial(), data = dens.sub)

# Kollar på en av modellerna
visreg(f1)
#Gör en model averaging på de bästa modellerna
k.ave <- model.avg(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
summary(k.ave)

#' Testar med dredgeobjektet. Där går det automatisera bättre. Dock verkar det
#' bli fel när man kör en glm i den.

par(mar = c(3,5,6,4))
plot(d.modell, labAsExpr = TRUE)
d.ave<-model.avg(d.modell, subset = delta < 2) 
summary(d.ave)
confset.95p <- get.models(d.modell, cumsum(weight) <= .95)

avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


#Vet inte riktigt vad outputen nedan betyder
# Force re-fitting the component models
model.avg(d.modell, cumsum(weight) <= .95, fit = TRUE)
# Models are also fitted if additional arguments are given
model.avg(d.modell, cumsum(weight) <= .95, rank = "AIC")

#Jämför manuella average model från glmulti med dredge
summary(k.ave)
summary(d.ave) # dredge. Här blir det 13 modeller med delta aicc < 2 av någon anledning och helt andra variabler som får högst vikt. dredge verkar bara funka med linjära modeller
