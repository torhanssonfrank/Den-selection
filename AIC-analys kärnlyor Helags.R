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
dens.short <- read_xlsx(path = "Den and territory selection/Rawdata/antal kullar per lya 2000_2018.xlsx")
dens.core.short <- readOGR(dsn = "Lyor, kullar, gps-punkter, yta och avstånd/lyor helags kärnområde.shp")
dens <- as.data.frame(dens)
dens.short <- as.data.frame(dens.short)
dens.core.short <- as.data.frame(dens.core.short)
str(dens)

# kollar outliers
hist(dens$medelvärde_lämmelprediktion_uppgångsår, breaks = 100) # flera parametrar verkar inte vara normalfördelade
hist(dens$rödräv_densitet, breaks = 100)
hist(dens$area_myr, breaks = 100) # outlier
hist(dens$area_vatten, breaks = 100) # outlier
hist(dens$distans_till_vatten, breaks = 100) #outliers
hist(dens$distans_till_skog, breaks = 100)

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
max(dens$Fas) # max är nu 3

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

names(dens)
View(dens)
dens.sub <- dens %>%
  dplyr::select(-N, -E,-avs_kull, -närmaste_rödräv,  -andel_bra_lämmelhabitat_uppgångsår, -lemmel_var, -hojd_over_havet)

# ändrar kvadratmeter till kvadratkm och m till km, det gör att det går att lägga på en GLMM
dens.sub <- dens.sub %>%
  mutate(area_myr = area_myr/1000000) %>% 
  mutate(area_vatten = area_vatten/1000000) %>%
  mutate(distans_till_vatten = distans_till_vatten/1000) %>% 
  mutate(distans_till_skog = distans_till_skog/1000)

View(dens.sub)
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
#' En lösning är att ändra från kvadratmeter till kvadratkm och meter till km. Det gjorde jag i slutändan. Då kan man 
#' få ut partial standard deviations från en global modell med ostandardisera värden. Sedan kan man lägga på en ny modell
#' med värden som är standardiserade baserat på partial standard deviations.

## ***************** ALLA FASER *********************####

# Plockar först ut de variabler jag behöver.
pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
           "rödräv_densitet",
           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")

full.fas.km <- dens.sub %>% 
  dplyr::select(kull, Namn, År, Fas, pvars)


full.fas.km.modell <- glmer(kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = full.fas.km)

#' Skalar om baserat på sample standard deviation. Ej rekommenderat av Cade 2015.
# pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
#           "rödräv_densitet",
#           "area_myr","area_vatten", "distans_till_vatten", "distans_till_skog")
#datsc <- dens.sub
#datsc[pvars] <- lapply(datsc[pvars],scale)

# View(datsc)



# global.modell <- glmer(kull ~ Fas + medelvärde_lämmelprediktion_uppgångsår
#            + rödräv_densitet
#            + area_myr + area_vatten + distans_till_vatten
#            + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
#          data = datsc) # det ska vara logit eftersom kull är binär data. När jag lägger till Namn som en random variabel fattar R att Namn är grupper

library(sjPlot)
plot_model(full.fas.modell.PSD, type = "slope")
plot_model(full.fas.modell.PSD, type = "diag") # qq-plot for kvantilerna för random effekt mot standard normalkvantiler
plot_model(full.fas.modell.PSD, type = "re")
plot_model(full.fas.modell.PSD, type = "resid" )

library(DHARMa)
# # vignette för DHARMa https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
simulateResiduals(full.fas.modell.PSD, plot = TRUE)
sim.output<-simulateResiduals(stdz.model)
plot(sim.output, quantreg = FALSE)
resid.fitted.plot<-plot(sim.output) #tar längre tid
plotResiduals(datsc$rödräv_densitet , sim.output$scaledResiduals, quantreg = T) # några variabler har enskilt lite dålig fit. Rödräv och lämmel är bra
resid.fitted.plot # kanske behöver presentera den här
testOverdispersion(sim.output) # med för mycket overdispersion är det mer varians i datat än i modellen. Min overdispersion är inte för hög.
testZeroInflation(sim.output) # tror inte det här är ett problem i binomial data. Kanske i negative binomial.
testUniformity(simulationOutput = sim.output) #Heteroscedasticity: när variansen i en variabel är olika runt responsvariabeln. Till exempel om variansen är lägre vid låga värden av responsvariabeln än vid höga värden. Det ger ofta en konformad scatterplot.
testDispersion(sim.output)# testar både over och underdispersion

#det är bäst att använda de här där man grupperar per lya
sim.group<-recalculateResiduals(sim.output, group = full.fas.km$Namn)
testDispersion(sim.group)
testUniformity(sim.group)
plot(sim.group, quantreg = FALSE)


# Fortsatt analys med omskalning baserat på sample standard deviations
summary(global.modell)
#' Jag fick samma varning, men det funkar: This works, 
#' although we get a warning message about a too-large gradient -- I think 
#' this is actually ignorable (we're still working on getting these error 
#' sensitivity thresholds right)

library(arm)
stdz.model <- standardize(full.fas.km.modell, standardize.y = FALSE)
summary(stdz.model)
## increases max gradient -- larger warning
library(MuMIn)
model.set <- dredge(stdz.model)  ## slow, but running ...


par(mfrow=c(1,1))
par(mar = c(3,5,6,4))
plot(model.set, labAsExpr = TRUE)
m.ave<-model.avg(model.set, subset = delta < 2) # skillnaden mellan modellen med lägst AIC och den modellen med högst AIC som väljs
Weights(m.ave)
summary(m.ave)





## skalar om med partial standard deviations, alla faser #### 

#psd <- partial.sd(full.fas.km.modell)
#psd
#z.full.fas.km <- stdize(full.fas.km, scale = c(NA,NA,NA,NA, psd[-1]), center =  c(FALSE, FALSE,FALSE,FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) # centrerar till mean 0. NA, NA, NA och NA tar bort responsvariabeln, Namn, År och Fas. Samma för FALSE, FALSE, FALSE, FALSE. Kull och År ska inte ändras och Namn kan inte ändras. binary = "omit" fungerade inte.


#full.fas.modell.PSD <- glmer(kull ~Fas + z.medelvärde_lämmelprediktion_uppgångsår
#                             + z.rödräv_densitet
#                             + z.area_myr + z.area_vatten + z.distans_till_vatten
#                             + z.distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
#                             data = z.full.fas.km)

#class(z.full.fas.km$Fas)#fortfarande faktor

#full.fas.set.unt.z <- dredge(full.fas.modell.PSD, beta = "none")
#m.ave.km <- model.avg(full.fas.set.unt.z, subset = delta < 2)
#summary(m.ave.km)
#nrow(m.ave.km$msTable)

## plottar alla faser ####

# Tabell
sumtable<-summary(m.ave)
sumtable
sumtable$importance

imp<-as.data.frame(sumtable$importance)
imp$`sumtable$importance` # ska kanske ha med det här med i plotten.
imp<-stack(imp) # ger error men det fungerar
imp<-round(imp, digits = 2) #avrundar
imp$Parameters <- as.character(NA)
imp$Parameters<-rownames(imp) # lägger radnamn som kolumn
rownames(imp) <-NULL #tar bort radnamn
class(imp$`sumtable$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp$`sumtable$importance` <- as.numeric(imp$`sumtable$importance`)

full.coefs<-as.data.frame(sumtable$coefmat.full)
full.coefs$Parameters <- as.character(NA)
full.coefs$Parameters<-rownames(full.coefs)
rownames(full.coefs)<- NULL

coefs.table <- full.coefs %>% 
  dplyr::select(Parameters, Estimate, `Std. Error`) %>% 
  dplyr::rename(`Unconditional SE` = `Std. Error`)

coefs.table
conf.int<-as.data.frame(confint(sumtable))
conf.int$Parameters <- rownames(conf.int)
rownames(conf.int) <- NULL
conf.int$`2.5 %` <- round(conf.int$`2.5 %`, digits = 3)
conf.int$`97.5 %` <- round(conf.int$`97.5 %`, digits = 3)
conf.int <- conf.int %>% 
  unite(`Confidence interval`,`2.5 %`, `97.5 %`, sep = ", ")
conf.int

coefs.table <-coefs.table %>% 
  left_join(conf.int, by = "Parameters")

coefs.table

colnames(imp)[1] <- "Relative importance"
imp$Parameters[1] <- "Faslow"

coefs.table <- coefs.table %>% 
  left_join(imp, by = "Parameters")

coefs.table$`Relative importance`[coefs.table$Parameters == "Faspeak"] <- 1.00


coefs.table$Parameters <- c("Intercept", "low phase", "peak phase", "area bogs", "distance to forest", "distance to water",
                            "mean lemming probability", "area water", "red fox density")
coefs.table <- coefs.table %>% 
  arrange(desc(`Relative importance`))
nrow(coefs.table) #9
coefs.table <- coefs.table[c(9,1:8),] #flyttar interceptet till toppen. Hamnade i botten
coefs.table$Estimate <- round(coefs.table$Estimate, digits = 3)
coefs.table$`Unconditional SE` <- round(coefs.table$`Unconditional SE`, digits = 3)

write_xlsx(coefs.table, path = "Den and territory selection/Plottar/tabell_alla_faser.xlsx")

# figur
imp <- paste0('r.i = ', imp$`sumtable$importance` ) # lägger till r.i =  framför alla relative importance värden. Outputen blir en vektor
imp
imp <- c("r.i = 1", "r.i = 1", "r.i = 1", "r.i = 0.63", "r.i = 0.54", "r.i = 0.47", "r.i = 0.33", "r.i = 0.28") # faslow och faspeak får en gemensam relative importance. Måste lägga till en extra.
imp
coef<-as.data.frame(sumtable$coefmat.subset)
ggdf<-as.data.frame(sumtable$coef.nmod)
ggdf<-ggdf %>% 
  dplyr::select(-`(Intercept)`)

coefP <- coef %>%
  dplyr::select(`Pr(>|z|)`) %>% 
  slice(-1) # tar bort interceptet


coefP
p <- round(coefP$`Pr(>|z|)`, digits =5)#avrundar

p <- paste0('p = ', p) # lägger till p = framför alla p-värden
p[2]<-"p = < 2e-16" # för liten, blev 0. lägger in den manuellt
p
names(ggdf)

colnames(ggdf) <- c("low phase", "peak phase","area bogs", "distance to forest", "distance to water",
                     "mean lemming density", "area water", "red fox density")
                      


ggdf<-stack(ggdf) # gör bred data lång
colnames(ggdf)<-c("Times_selected", "Variables")
ggdf<-ggdf %>%
  arrange(desc(Times_selected))


#' blir ändå fel ordning på staplarna även fast jag sorterat med dplyr.
#' VARNING! P-värdena är i fel ordning
g <- ggplot(data=ggdf, aes(x=Variables, y=Times_selected, fill=Times_selected)) +
  geom_bar(stat="identity", width=0.7)+
  geom_text(aes(label= p), vjust=4, color="white", size = 7)+
  geom_text(aes(label= imp), vjust=2, color="white", size = 7)+
  labs(x = "Variables in selected models")+
  labs(y = "Number of times selected in best models")+
  theme_minimal()

g+theme(axis.text=element_text(size=15, color = "black"), # ändrar stapeltextens storlek
        axis.title=element_text(size=17,face="bold")) # ändrar axeltitlarnas storlek

ggsave("ggdf.png", width = 35, height = 20, units = "cm") # sparar plotten i working directory

#gör en tabell istället
imp
# blir konstig ordning så gör om
impt <- c(1.00, 1.00, 1.00, 0.63, 0.54, 0.47, 0.33, 0.28)
sumtable
pt <- c("5.5e-05", "<2e-16","0.0001","0.1020", "0.2031", "0.1324", "0.4274", "0.9117")
Variables
est.coef <- c("-1.1214", "1.6292", "1.2460", "-0.6301", "-0.4566", "0.4016", "-0.2360", "-0.0349")
Variables <- c("low phase", "peak phase", "distance to forest","area bogs", "distance to water", "mean lemming probability",
               "area water", "red fox density")
fas.tabell <-cbind(as.character(Variables), est.coef, impt, pt)
fas.tabell
colnames(fas.tabell) <- c("variables", "estimate coefficient", "relative importance", "p-value")
fas.tabell

## Testar att lägga in År som en random effect eftersom jag mäter per år ####
global.modell.år <- glmer(kull ~ Fas +  z.medelvärde_lämmelprediktion_uppgångsår
                         + z.rödräv_densitet
                          + z.area_myr + z.area_vatten + z.distans_till_vatten
                          + z.distans_till_skog + (1 | Namn) + (1 | År), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = z.full.fas.km)


library(MuMIn)
model.set.år <- dredge(global.modell.år)  ## slow, but running ...

car::vif(global.modell.år)
par(mfrow=c(1,1))
par(mar = c(3,5,6,4))
plot(model.set.år, labAsExpr = TRUE)
m.ave.år<-model.avg(model.set.år, subset = delta < 6)
summary(m.ave.år) # ingen större skillnad. Bara att lågår inte är signifikant längre. annars är bara distans till skog och peak signifikant, precis som innan.
Weights(model.set.år)

#' Fas är väldigt signifikant.
#' Testar därför att analyser de tre faserna separat


## Fas 1 ####

fas.1 <- dens.sub %>% 
  filter(Fas == "low")
View(fas.1)
length(fas.1$kull[fas.1$kull==1]) # 29 kullar under lågår
length(fas.1$obsID)

View(fas.1)
# Jag tar bara med de variabler jag behöver
fas.1.km <- fas.1 %>%
  dplyr::select(kull, Namn, pvars)
View(fas.1.km)


# Area skalad till kvadratkm och distans till km
fas.1.modell <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                        + rödräv_densitet
                        + area_myr + area_vatten + distans_till_vatten
                        + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                        data = fas.1.km)

car::vif(fas.1.modell)

# Skalar om med sample standard deviations. Ej rekommenderat av Cade.

#Eftersom jag har kvadratkm och km i datat så behövs inte det här stycket. 
#pvars <- c("medelvärde_lämmelprediktion_uppgångsår",
#           "rödräv_densitet","area_myr",
#           "area_vatten", "distans_till_vatten", "distans_till_skog")
#datsc.1 <- fas.1
#datsc.1[pvars] <- lapply(datsc.1[pvars],scale)
#View(datsc.1)
#fas.1.modellsc <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
#                      + rödräv_densitet
#                      + area_myr + area_vatten + distans_till_vatten
#                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
#                      data = datsc.1)

stdz.model.1 <- standardize(fas.1.modell, standardize.y = FALSE)
summary(stdz.model.1)
?scale
?`standardize,merMod-method`
fas.1.setsc <- dredge(stdz.model.1)

nrow(fas.1.km) ## 420
ncol(getME(fas.1.setsc,"X")) # 7, om det är mindre än 10 obs per variabel i AIC analysen är det för få. Jag har 420/7 = 60



par(mar = c(3,5,6,4))
plot(fas.1.setsc, labAsExpr = TRUE)
ave.1sc<-model.avg(fas.1.setsc, subset = delta < 2) 
summary(ave.1sc) 

## Skalar om med partial SD fas 1 ####
psd.1 <- partial.sd(fas.1.modell)

z.fas.1.km <- stdize(fas.1.km, scale = c(NA,NA, psd.1[-1]), center = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) # centrerar till mean 0. NA och NA tar bort responsvariabeln och Namn. Samma för FALSE, FALSE. Kull ska inte ändras och Namn kan inte ändras) # centrerar till mean 0. NA och NA tar bort responsvariabeln och Namn. Samma för FALSE, FALSE. Kull ska inte ändras och Namn kan inte ändras
head(z.fas.1.km)
hist(z.fas.1.km$z.medelvärde_lämmelprediktion_uppgångsår) # de är inte helt unimodala. Outliers ställer till problem. Skiter nog i det.
hist(z.fas.1.km$z.rödräv_densitet)
hist(z.fas.1.km$z.area_myr)
hist(z.fas.1.km$z.area_vatten)
hist(z.fas.1.km$z.distans_till_vatten)
hist(z.fas.1.km$z.distans_till_skog)

fas.1.modell.PSD <- glmer(kull ~ z.medelvärde_lämmelprediktion_uppgångsår
                          + z.rödräv_densitet
                          + z.area_myr + z.area_vatten + z.distans_till_vatten
                          + z.distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = z.fas.1.km)



fas.1.set.unt.z <- dredge(fas.1.modell.PSD, beta = "none") #koefficienterna ej transformerade. Den här är nog bäst.
fas.1.set.PSD <- dredge(fas.1.modell.PSD, beta = "partial.sd") # koefficienterna transformeras baserat på partial standard deviations
fas.1.set.unt <- dredge(fas.1.modell, beta = "none") #koefficienterna ej transformerade
fas.1.set <- dredge(fas.1.modell, beta = "partial.sd") # koefficienterna transformeras baserat på partial standard deviations

koef.norm<-summary(model.avg(fas.1.set.unt.z, subset = delta < 2)) #koefficienterna ej transformerade
koef.psd<-summary(model.avg(fas.1.set.PSD, subset = delta < 2))
summary(model.avg(fas.1.set.unt.z, subset = delta < 2)) #koefficienterna ej transformerade
summary(model.avg(fas.1.set, subset = delta < 2))
fas.1.set.unt.z
# det blir ingen större skillnad på koefficienterna förutom på interceptet om man kör partial.sd med dredge.
nrow(koef.norm$msTable)
koef.norm$coefficients
koef.psd$coefficients
koef.norm
koef.psd

## fas 1 plott ####

ave.1sc<-model.avg(ave.1sc, subset = delta < 2)

sumtable.1<-summary(ave.1sc)
sumtable.1$importance
sumtable.1
imp1<-as.data.frame(sumtable.1$importance)
imp1$`sumtable.1$importance` # ska kanske ha med det här med i plotten.
imp1<-stack(imp1) # ger error men det fungerar
imp1<-round(imp1, digits = 2) #avrundar
rownames(imp1) <- NULL #tar bort radnamnen
class(imp1$`sumtable.1$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp1$`sumtable.1$importance` <- as.numeric(imp1$`sumtable.1$importance`)

imp1 <- paste0('r.i = ', imp1$`sumtable.1$importance` ) # lägger till r.i =  framför alla relative importance värden. Outputen blir en vektor
imp1
coef1<-as.data.frame(sumtable.1$coefmat.subset)
ggdf1<-as.data.frame(sumtable.1$coef.nmod)
ggdf1<-ggdf1 %>% 
  dplyr::select(-`(Intercept)`)

coefP1 <- coef1 %>%
  dplyr::select(`Pr(>|z|)`) %>% 
  slice(-1) # tar bort interceptet

sumtable.1$coefmat.subset
coefP1
p1 <- round(coefP1$`Pr(>|z|)`, digits =3)#avrundar
p1 <- paste0('p = ', p1) # lägger till p = framför alla p-värden
p1

names(ggdf1)
colnames(ggdf1) <- c("distance to forest","red fox density", "area bogs",
                     "distance to water", "area water", "mean lemming probability")
                     

ggdf1<-stack(ggdf1) # gör bred data lång
colnames(ggdf1)<-c("Times_selected", "Variables")
ggdf1

sumtable.1
# importance blev egentligen fel här med men de som var på olika plats hade samma värde (0.26)
g<-ggplot(data=ggdf1, aes(x=Variables, y=Times_selected, fill=Times_selected)) +
  geom_bar(stat="identity", width=0.7)+
  geom_text(aes(label= p1), vjust=4, color="white", size = 7)+
  geom_text(aes(label= imp1), vjust=2, color="white", size = 7)+
  labs(x = "Variables in selected models, lemming low phase")+
  labs(y = "Number of times selected in best models")+
  theme_minimal()

g+theme(axis.text=element_text(size=15, color = "black"), # ändrar stapeltextens storlek
        axis.title=element_text(size=17,face="bold")) # ändrar axeltitlarnas storlek

ggsave("ggdf1.png", width = 35, height = 20, units = "cm") # sparar plotten i working directory

## Fas 2 ####
fas.2 <- dens.sub %>% 
  filter(Fas == "increase")
  
length(fas.2$kull[fas.2$kull==1])
length(fas.2$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 


# skalar om med partial standard deviations
fas.2.km <- fas.2 %>% 
  dplyr::select(kull, Namn, pvars)

View(fas.2.km)
fas.2.modell <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = fas.2.km)
car::vif(fas.2.modell)

psd.2 <- partial.sd(fas.2.modell)

z.fas.2.km <- stdize(fas.2.km, scale = c(NA,NA, psd.2[-1]), center =  c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) # centrerar till mean 0. NA och NA tar bort responsvariabeln och Namn. Samma för FALSE, FALSE. Kull ska inte ändras och Namn kan inte ändras.


fas.2.modell.PSD <- glmer(kull ~ z.medelvärde_lämmelprediktion_uppgångsår
                          + z.rödräv_densitet
                          + z.area_myr + z.area_vatten + z.distans_till_vatten
                          + z.distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = z.fas.2.km)



fas.2.set.unt.z <- dredge(fas.2.modell.PSD, beta = "none")
summary(model.avg(fas.2.set.unt.z, subset = delta < 2))

#skalar om med sample standard deviations. Ej rekommenderat av Cade
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
summary(model.avg(fas.2.set, subset = delta < 6))
ave.2sc<-model.avg(fas.2.set, subset = delta < 2) 
summary(ave.2sc)
plot(datsc.2$area_myr, datsc.2$kull)

## test utan myr-outlier####
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

## Fas 2 plott####
ave.2psd<-model.avg(fas.2.set.unt.z, subset = delta < 6)

sumtable.2<-summary(ave.2psd)

sumtable.2

imp2<-as.data.frame(sumtable.2$importance)
imp2$`sumtable.2$importance` # ska kanske ha med det här med i plotten.
imp2<-stack(imp2) # ger error men det fungerar
imp2
imp2<-round(imp2, digits = 2) #avrundar
rownames(imp2) <- NULL
class(imp2$`sumtable.2$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass

imp2$`sumtable.2$importance` <- as.numeric(imp2$`sumtable.2$importance`)

imp2 <- paste0('r.i = ', imp2$`sumtable.2$importance` ) # lägger till r.i =  framför alla relative importance värden. Outputen blir en vektor
sumtable.2$importance
imp2
coef2<-as.data.frame(sumtable.2$coefmat.subset)
ggdf2<-as.data.frame(sumtable.2$coef.nmod)
ggdf2<-ggdf2 %>% 
  dplyr::select(-`(Intercept)`)

coefP2 <- coef2 %>%
  dplyr::select(`Pr(>|z|)`) %>% 
  slice(-1) # tar bort interceptet

sumtable.2$coefmat.subset


coefP2
p2 <- round(coefP2$`Pr(>|z|)`, digits =3)#avrundar

p2 <- paste0('p = ', p2) # lägger till p = framför alla p-värden
p2

sumtable.2$coef.nmod

ggdf2

colnames(ggdf2) <- c("area bogs", "distance to forest","distance to water", "area water", "mean lemming probability",
                     "red fox density")

ggdf2<-stack(ggdf2) # gör bred data lång
colnames(ggdf2)<-c("Times_selected", "Variables")
ggdf2
p2
# blev fel ordning på relative importance så matade in manuellt imp2 <-c("r.i = 0.83", "r.i = 1.00", "r.i = 0.32", "r.i = 0.32", "r.i = 0.37", "r.i = 0.28")
sumtable.2
g<-ggplot(data=ggdf2, aes(x=Variables, y=Times_selected, fill=Times_selected)) +
  geom_bar(stat="identity", width=0.7)+
  geom_text(aes(label= p2), vjust=5, color="white", size = 7)+
  geom_text(aes(label= imp2), vjust=3, color="white", size = 7)+
  labs(x = "Variables in selected models, lemming increase phase")+
  labs(y = "Number of times selected in best models")+
  theme_minimal()

g+theme(axis.text=element_text(size=15, color = "black"), # ändrar stapeltextens storlek
        axis.title=element_text(size=17,face="bold")) # ändrar axeltitlarnas storlek

ggsave("ggdf2.png", width = 35, height = 20, units = "cm") # sparar plotten i working directory


## Fas 3 ####

fas.3 <- dens.sub %>% 
  filter(Fas == "peak")
length(fas.3$kull[fas.3$kull == 1])

length(fas.3$obsID) # om det är mindre än 10 obs per variabel i AIC analysen är det för få. 

# skalar om med partial standard deviations
fas.3.km <- fas.3 %>% 
  dplyr::select(kull, Namn, pvars)

View(fas.3.km)
fas.3.modell <- glmer(kull ~ medelvärde_lämmelprediktion_uppgångsår
                      + rödräv_densitet
                      + area_myr + area_vatten + distans_till_vatten
                      + distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                      data = fas.3.km)
car::vif(fas.3.modell)

psd.3 <- partial.sd(fas.3.modell)

z.fas.3.km <- stdize(fas.3.km, scale = c(NA,NA, psd.3[-1]), center = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)) # centrerar inte. Överväg dock att göra det. NA och NA tar bort responsvariabeln och Namn. Kull ska inte ändras och Namn kan inte ändras.
round((mean(z.fas.3.km$z.medelvärde_lämmelprediktion_uppgångsår) + mean(z.fas.3.km$z.rödräv_densitet) + mean(z.fas.3.km$z.area_myr) + mean(z.fas.3.km$z.area_vatten) + mean(z.fas.3.km$z.distans_till_vatten) + mean(z.fas.3.km$z.distans_till_skog))/6)
View(z.fas.3.km)
fas.3.modell.PSD <- glmer(kull ~ z.medelvärde_lämmelprediktion_uppgångsår
                          + z.rödräv_densitet
                          + z.area_myr + z.area_vatten + z.distans_till_vatten
                          + z.distans_till_skog + (1 | Namn), na.action = "na.fail", family = binomial(link = 'logit'), 
                          data = z.fas.3.km)



fas.3.set.unt.z <- dredge(fas.3.modell.PSD, beta = "none")
ave.3psd<-model.avg(fas.3.set.unt.z, subset = delta < 2)
summary(ave.3psd)
nrow(ave.3psd$msTable) #20 modeller
?get.models
confset.d4 <- get.models(fas.3.set.unt.z, subset = delta < 2)
summary(model.avg(confset.d4)) #samma sak som summary(ave.3psd)
summary(ave.3psd)
confint(ave.3psd)
confint(ave.3psd, full = TRUE) # conditional är default på alla de här funktionerna. Full= TRUE ger full average. 

logLik(ave.3psd, full = TRUE)
coefTable(ave.3psd, full = TRUE)
vcov(ave.3psd, full = TRUE)
ave.3psd$importance # ger importance baserat på Akaikevikter, inte på antal gånger den är med i de översta modellerna.
lista <- rownames(coefTable(ave.3psd, full = TRUE)) # kom på ett sätt att plocka ut radnamnen så kan man lägga in dem i en vektor och sen kolumn.
lista
?model.avg
#skalar om med sample standard deviations. Ej rekommenderat av Cade.
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
?standardize
summary(stdz.model.3)
fas.3.set <- dredge(stdz.model.3) # stora standard errors betyder att en eller flera modeller inte gick ihop (failed to converge)

# plottar
par(mar = c(3,5,6,4))
plot(fas.3.set, labAsExpr = TRUE)
su<-summary(model.avg(fas.3.set, subset = delta < 2))
summary(su)                           # Std. Error är unconditional standard error eftersom revised.var = true är default i model.avg.
ave.3psd<-model.avg(fas.3.set.unt.z, subset = delta < 2) 
sumtable.3<-summary(ave.3psd)
sumtable.3
coefTable(sumtable.3, full = TRUE) #partial standardisering
coefTable(su, full = TRUE) # vanlig standardisering
imp3<-as.data.frame(sumtable.3$importance)
imp3$`sumtable.3$importance` # ska kanske ha med det här med i plotten.
imp3<-stack(imp3) # ger error men det fungerar
imp3<-round(imp3, digits = 2) #avrundar
rownames(imp3) <- NULL
class(imp3$`sumtable.3$importance` ) #sparas som "importance" och "numeric". Vet inte vad importance är för klass
imp3$`sumtable.3$importance` <- as.numeric(imp3$`sumtable.3$importance`)

imp3 <- paste0('r.i = ', imp3$`sumtable.3$importance` ) # lägger till r.i =  framför alla relative importance värden. Outputen blir en vektor
sumtable.3$importance
imp3
coef3<-as.data.frame(sumtable.3$coefmat.subset)
ggdf3<-as.data.frame(sumtable.3$coef.nmod)
ggdf3<-ggdf3 %>% 
  dplyr::select(-`(Intercept)`)

coefP3 <- coef3 %>%
  dplyr::select(`Pr(>|z|)`) %>% 
  slice(-1)


coefP3
p3 <- round(coefP3$`Pr(>|z|)`, digits =4)#avrundar
p3 <- paste0('p = ', p3) # lägger till p = framför alla p-värden
p3
sumtable.3$coefmat.subset
names(ggdf3)
colnames(ggdf3) <- c("distance to forest", "mean lemming probability", "distance to water", 
                    "area water","area bogs", "red fox density")

ggdf3<-stack(ggdf3) # gör bred data lång
colnames(ggdf3)<-c("Times_selected", "Variables")
ggdf3

sumtable.3$importance
sumtable.3
# importance blev fel ordning här med. Matade in manuellt. imp3 <- c("r.i = 1", "r.i = 0.76", "r.i = 0.46 ", "r.i = 0.38", "r.i = 0.46", "r.i = 0.34") 
g<-ggplot(data=ggdf3, aes(x=Variables, y=Times_selected, fill=Times_selected)) +
  geom_bar(stat="identity", width=0.7)+
  geom_text(aes(label= p3), vjust=4, color="white", size = 7)+
  geom_text(aes(label= imp3), vjust=2, color="white", size = 7)+
  labs(x = "Variables in selected models, lemming peak years")+
  labs(y = "Number of times selected in best models")+
  theme_minimal()

g+theme(axis.text=element_text(size=15, color = "black"), # ändrar stapeltextens storlek
        axis.title=element_text(size=17,face="bold")) # ändrar axeltitlarnas storlek

ggsave("ggdf3.png", width = 40, height = 20, units = "cm") # sparar plotten i working directory

## TABELLER PÅ ANTAL KULLAR OCH VALPLYOR####
# tittar lite på antal reproduktioner per fas och antal lyor med kull

length(fas.1$obsID)/60
length(fas.2$obsID)/60
length(fas.3$obsID)/60
# Antal lyor med kull mellan 2000 - 2018
length(unique(dens.sub$Namn[dens.sub$kull == 1]))
unique(dens.sub$Namn[dens.sub$kull == 1])
length(dens.sub$Namn[dens.sub$kull == 1])

#Kollar några lyor separat
#Under vilka faser har Hulke och Snusestöten haft kull
dens.sub$Fas[dens.sub$kull == 1 & dens.sub$Namn == "FSZZ033"]
dens.sub$Fas[dens.sub$kull == 1 & dens.sub$Namn == "FSZZ020"]

length(dens.sub$kull[dens.sub$kull == 1 & dens.sub$Namn == "FSZZ033"])

#Vilka lyor har kull per fas
unique(dens.sub$Namn[dens.sub$kull == 1 & dens.sub$Fas == "low"])
unique(dens.sub$Namn[dens.sub$kull == 1 & dens.sub$Fas == "increase"])
unique(dens.sub$Namn[dens.sub$kull == 1 & dens.sub$Fas == "peak"])

# plockar ut lyorna med kull för varje fas ur dens -dataramen. Den har koordinater
# ändrar om koordinaterna till numeric först så slipper jag göra det för varje fas
dens$N <- as.numeric(dens$N)
dens$E <- as.numeric(dens$E)
#low
low.lit.dens <- dens %>%
  dplyr::select(Namn,Fas, N, E, kull) %>% 
  filter(Fas == "low") %>%
  filter(kull>0) %>% 
  group_by(Namn, N, E) %>% 
  summarise(`Total litters`= sum(kull))

View(low.lit.dens)

# increase
inc.lit.dens <- dens %>%
  dplyr::select(Namn,Fas, N, E, kull) %>% 
  filter(Fas == "increase") %>%
  filter(kull>0) %>% 
  group_by(Namn, N, E) %>% 
  summarise(`Total litters`= sum(kull))

View(inc.lit.dens)

# peak
peak.lit.dens <- dens %>%
  dplyr::select(Namn,Fas, N, E, kull) %>% 
  filter(Fas == "peak") %>%
  filter(kull>0) %>% 
  group_by(Namn, N, E) %>% 
  summarise(`Total litters`= sum(kull))

View(peak.lit.dens)


coordinates(low.lit.dens) <- c("E", "N")
proj4string(low.lit.dens) <- CRS("+init=EPSG:3006")
summary(low.lit.dens) #sweref och projected

coordinates(inc.lit.dens) <- c("E", "N")
proj4string(inc.lit.dens) <- CRS("+init=EPSG:3006")
summary(inc.lit.dens) #sweref och projected

coordinates(peak.lit.dens) <- c("E", "N")
proj4string(peak.lit.dens) <- CRS("+init=EPSG:3006")
summary(peak.lit.dens) #sweref och projected

writeOGR(low.lit.dens, dsn = "Lyor, kullar, gps-punkter, yta och avstånd/Kullar per lya och fas", layer ="kullar.lågår",  driver = "ESRI Shapefile")
writeOGR(inc.lit.dens, dsn = "Lyor, kullar, gps-punkter, yta och avstånd/Kullar per lya och fas", layer ="kullar.uppgång",  driver = "ESRI Shapefile")
writeOGR(peak.lit.dens, dsn = "Lyor, kullar, gps-punkter, yta och avstånd/Kullar per lya och fas", layer ="kullar.toppår",  driver = "ESRI Shapefile")

?writeOGR

#' Totala kullar per lya för tabell. Onödig kod eftersom jag har den ovan, men jag skrev den först.
#' Den har dock alla kullar totalt för alla valplyor
head(dens.short)
length(dens.short$Namn)
head(dens.core.short)
short.sub<-dens.short[dens.short$Namn %in% dens.core.short$Namn, ]
length(short.sub$Namn)
head(short.sub)
class(short.sub$kullar_totalt)
litter.dens<-subset(short.sub, short.sub$kullar_totalt>=1)
length(litter.dens$Namn)
View(litter.dens)
litter.sorted <- litter.dens %>% 
  arrange(desc(kullar_totalt))


litter.sorted <- litter.sorted %>% 
  dplyr::select(Namn, kullar_totalt) %>% 
  dplyr::rename(`Den code` = Namn) %>% 
  dplyr::rename(`Total litters` = kullar_totalt)

View(litter.sorted)
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
