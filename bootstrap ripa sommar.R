library(dplyr)
library(tidyr)
library(readxl)
library(boot)

ripa_sommar<-read_xlsx(file.choose()) #läste in filen tor.riptransekter helags sommar 2018.xlsx#
head(ripa_sommar)


ripskit<- ripa_sommar %>% 
  select(lya, observation)

ripskit<-ripskit %>% 
  filter(observation == "RS")

head(ripskit)
unique(ripskit$observation)#bara RS kvar####

aktiv<- ripskit %>% 
  filter(lya %in% c("zz014","zz020", "zz033", "zz042"))

head(aktiv)
unique(aktiv$lya)
View(aktiv)  

aktivsumma<-aktiv %>% 
  group_by(lya, observation) %>% 
  count()
  
aktivsumma

inaktiv<- ripskit %>% 
  filter(lya %in% c("zz104","zz062", "zz096", "zz061", "zz075", "zz076"))

head(inaktiv)
unique(inaktiv$lya)
  

inaktivsumma<-inaktiv %>% 
  group_by(lya, observation) %>% 
  count()

inaktivsumma


boot.mean <- function(x, i){ mean(x[i]) } #en funktion för att räkna ut medelvärdet efter varje körning
# av loopen i bootpaketet.

#testar lite med fejkdata först
test<-c(42,39,22,18,34,27,25,28,31,40)
boot.test<-boot(test, boot.mean, R= 10000)
hist(boot.test$t)
boot.ci(boot.test, type = "bca") 

#de aktiva lyorna först
bootaktiv <- boot(aktivsumma$n, boot.mean, R =10000)
hist(bootaktiv$t)
boot.ci(bootaktiv, type = "bca")

#sen de inaktiva
bootinaktiv <- boot(inaktivsumma$n, boot.mean, R = 10000)
hist(bootinaktiv$t)
boot.ci(bootinaktiv, type = "bca")

z<-data.frame(bootaktiv$t, bootinaktiv$t)
colnames(z) <-c("aktiva lyor", "inaktiva lyor")
head(z)

z<-z %>% 
  gather("lya","boot.mean")
head(z)
tail(z)
boxplot(z$boot.mean ~ z$lya, main = "Ripspillning på aktiva och inaktiva lyor (n = 4 och 6). Bootstrap 10 000 gånger. P < 2e-16", ylab = "Medelvärde ripspillning")
?boxplot
lya.aov<-aov(z$boot.mean ~ z$lya)
summary(lya.aov)


