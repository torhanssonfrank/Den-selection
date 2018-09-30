install.packages("lme4")
install.packages("openxlsx") # det här paketet ingår i car. Det installerades inte när jag installerade car så då funkar inte car. Det funkar när jag installerar det separat
install.packages("car")
install.packages("glmulti")
install.packages("rJava")

library(lme4)
library(ggplot2)
library(readxl)
library(writexl)
library(MASS)
library(car)
library(tidyverse)
library(glmulti)

# AIC-analys av kärnlyor i Helagspopulationen

dens <- read_xlsx(path = "kärnlyor Helags AIC.xlsx")

dens <- as.data.frame(dens)

str(dens) # hojd_over_havet blev sparat som character
dens$hojd_over_havet <- as.numeric(dens$hojd_over_havet)

hist(dens$närmaste_rödräv, breaks = 100) # flera parametrar verkar inte vara normalfördelade

par(mfrow=c(1,2))
qqPlot(ak.1)
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


#' Min responsvariabel är  binär (antingen kull eller ingen kull). Då kan man inte använda
#' linear model eller penalized quasi likelihood (PQL)
#' mina förklarande variabler är inte normalfördelade. 
#' Mina residualer är alltså inte normalfördelade.
#'  Därför är det bäst att använda en generalised linear model med mixed effects. Om
#'  man har fler än 3 random variabler ska man använda Monte Carlo algorithms (MCMC),
#'  vilket jag har.
#'  En bra guide finns här: https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html

#' Tar först bort N, E, År, distans till rödräv och andel bra lämmelhabitat
#' eftersom jag inte ska ha med dem i analysen. distans till rödräv innehåller dessutom 
#' många NA's.

dens.sub <- dens %>%
  dplyr::select(-N, -E, -År, -närmaste_rödräv, -andel_bra_lämmelhabitat_uppgångsår)

#tar bort rader med NA's
dens.sub <- dens.sub[complete.cases(dens.sub), ]

class(dens$Namn)
dens.sub$Namn <- as.factor(dens.sub$Namn) #måste vara factor för att kunna analyseras
names(dens.sub)
rownames(dens.sub) <- NULL
fm <- glm(kull ~ Fas + Namn + avs_kull + medelvärde_lämmelprediktion_uppgångsår
            + lemmel_var + rödräv_densitet + hojd_over_havet
            + area_myr + area_vatten + distans_till_vatten
            + distans_till_skog, na.action = "na.fail", family = binomial(link = 'logit'), 
          data = dens.sub) # det ska vara logit eftersom det är binär data

summary(fm)
mst <- dredge(fm)


