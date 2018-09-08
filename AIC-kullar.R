
#' AIC för hela tidsperioden, inte separat för varje år. Whittingham et al tycker att varje
#' år är bättre.


install.packages("MuMIn")

library(MuMIn) # paket som gör IT - methodology!
library(readxl)
library(writexl)
library(visreg)

lyor_kull <- read_xlsx(path = "Den and territory selection/Rawdata/antal kullar per lya 2000_2018.xlsx")

lyor_kull <- as.data.frame(lyor_kull)
lyor_kull

vatten_skog <-read.csv("Den and territory selection/Rawdata/lyor_distans_vatten_skog_utan_gps.csv", stringsAsFactors = FALSE, sep =";", dec = ",")
head(vatten_skog)

höjd <- read_xlsx(path = "Den and territory selection/Rawdata/lyor_hojd_helags.xlsx")
höjd<-as.data.frame(höjd)
head(höjd)

lemmel_medel <- read_xlsx(path = "Den and territory selection/Rawdata/lämmelprediktion_medelvärde_topp_uppgång.xlsx")
lemmel_medel<-as.data.frame(lemmel_medel)
head(lemmel_medel)

lemmel_andel<- read_xlsx(path = "Den and territory selection/Rawdata/andel_lämmelhabitattyper_per_lya.xlsx")
lemmel_andel<-as.data.frame(lemmel_andel)
head(lemmel_andel)

kullar <- lyor_kull %>% 
  select(Namn, kullar_totalt)

medel_uppgång <- lemmel_medel %>% 
  select(Namn, medelvärde_lämmelprediktion_uppgångsår)

andel_bra_uppgång <- lemmel_andel %>% 
  select(Namn,andel_bra_lämmelhabitat_uppgångsår)

andel_myr <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area med myrmark med 1,5 km radie runt varje lya.xlsx")
andel_myr<- as.data.frame(andel_myr)
head(andel_myr)
myr<- andel_myr %>% 
  select(Namn, area_myr)

andel_vatten <- read_xlsx(path = "Lyor, kullar, gps-punkter, yta och avstånd/Area som är vatten med 1,5 km radie runt varje lya.xlsx")
andel_vatten<- as.data.frame(andel_vatten)
head(andel_vatten)

vatten <- andel_vatten %>% 
  select(Namn, area_vatten)


kullar_var <- kullar %>%
  left_join(medel_uppgång, by = "Namn") %>% 
  left_join(andel_bra_uppgång, by = "Namn") %>%
  left_join(höjd, by = "Namn") %>% 
  left_join(vatten_skog, by = "Namn") %>% 
  left_join(vatten, by = "Namn") %>% 
  left_join(myr, by = "Namn" )

View(kullar_var)  
colnames(kullar_var)
colnames(kullar_var)<- c("Namn", "kull", "lemM", "lemA", "hojd", "vattD", "skogD", "vattA", "myrA")

head(kullar_var)
class(kullar_var$hojd)
kullar_var$hojd<- as.numeric(kullar_var$hojd)
class(kullar_var)
kullar_var<-as.data.frame(kullar_var)



#Testar lite AIC först
fit1 <- lm(kull ~ lemM, data = kullar_var)  
fit2 <- lm(kull ~ lemM + lemA, data = kullar_var)
fit3 <- lm(kull ~ lemM + lemA + hojd, data = kullar_var )
fit4 <- lm(kull ~ lemM + lemA + hojd + vattD, data = kullar_var )
fit5 <- lm(kull ~ lemM + lemA + hojd + vattD + skogD, data = kullar_var )
fit6 <- lm(kull ~ lemM + lemA + hojd + vattD + skogD + vattA, data = kullar_var )
fit7 <- lm(kull ~ lemM + lemA + hojd + vattD + skogD + vattA + myrA, data = kullar_var )

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)

#' Testar dredge i mumin. Kör igenom alla kombinationer av parametrarna och tar AIC-värden på alla.
#' Dredge funkar inte när det finns NAs i datat. Man måste dessutom sätta en inställning
#' själv så att dredge stoppas om det finns NA. Antingen ställer man om HELA R-systemet
#' med options(na.action = "na.fail"). Default är options(na.action = "na.omit"), dvs att NA
#' är ok). Om man använder options ställer man alltså om hela R. Detta skrivs
#' på en separat kod-rad. Annars kan man bara ställa om detta innanför parenteserna
#' på koden för den linjära modellen.

#' Först måste jag dock dumpa lyorna som har NAs. Det är de fem lyorna som inte täcktes
#' in av lämmelrastern. complete.cases tar bort rader som innehåller NA

kullar_sub <- kullar_var[complete.cases(kullar_var), ]

View(kullar_sub)

#' Ska det vara fixed effects(lm) eller random effects?.
#' Avstånd till fasta punkter, andel myrar osv borde ju vara fixed effects.
#' Däremot borde rödrävar kunna vara en random effect. I en fixed 
#' effect får man varians av treatment och sampling error, alltså är det
#' bara sampling error som står för slumpmässighet i designen. I en random effect får man
#' varians på treatment, interaktion och sampling error. Det är alltså mer slumpmässighet
#' i variabeln. Rödrävar borde alltså vara en random effekt eftersom det är slumpmässigt
#' var de råkade befinna sig när de blev skjutna. Sampling error i det fallet skulle vara
#' att man tog gps-positionen fel. Det finns inget slumpmässigt med avstånd till trädgränsen,
#' bara sampling error, tex att trädgränsen ligger fel i GIS-programmet.
#' Schluter:
#' In R, lm assumes that all effects are fixed. Do not use if you have random effects.
#' Use lme (in the nlme package) or lmer (in the lme4 package) to analyze models
#' containing random effects. They model the variance structure explicitly, and use
#' restricted maximum likelihood (REML) to obtain unbiased estimates of effects and
#' test hypotheses.
#' • families made up of siblings
#' • subjects measured repeatedly (“repeated measures”)
#' • transects of quadrats in a sampling survey
#' • field plots of plants
#' • environment chambers containing aquaria

fm1 <- lm(kull ~ lemM + lemA + hojd + vattD + skogD + vattA + myrA, data = kullar_sub, na.action = "na.fail" )


ms1 <- dredge(fm1) #för att den här ska funka får man inte ha NA på någon rad. Då kommer den passa ihop massa subsets av datat med andra subsets och då blir det kaos.

summary(ms1)
summary(model.avg(ms1, subset = delta < 4)) # skillnaden mellan modellen med lägst AIC och den modellen med högst AIC som väljs
Weights(ms1)



par(mar = c(3,5,6,4))
plot(ms1, labAsExpr = TRUE)
model.avg(ms1, subset = delta < 4) 
confset.95p <- get.models(ms1, cumsum(weight) <= .95)

?model.avg
?get.models
?cums

avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)


#Vet inte riktigt vad outputen nedan betyder
# Force re-fitting the component models
model.avg(ms1, cumsum(weight) <= .95, fit = TRUE)
# Models are also fitted if additional arguments are given
model.avg(ms1, cumsum(weight) <= .95, rank = "AIC")
