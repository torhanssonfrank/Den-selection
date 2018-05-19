library(dplyr)
library(tidyr)
install.packages("gapminder")
library(gapminder)
head(gapminder)
ripa<-read.csv(file.choose(), header = TRUE, sep = ";")
head(ripa)

sedda<-ripa %>% 
  select(lya, observation, antal) %>% 
  filter(observation == "F")
  

View(sedda)

which(is.na(sedda$antal)) #det fattades 3 värden. Jag la till de i rådatan eftersom de fanns nedskrivna på pappren####

sedda %>%
  spread(lya)
 

sedda_med_lysumma<-sedda %>%
  group_by(lya) %>% 
  summarise(ripor_per_lya = sum(antal))
  
sedda_med_lysumma
levels(sedda_med_lysumma$lya)


x<-sedda_med_lysumma$ripor_per_lya #ett  sätt att sätta ihop lynamnen med "ripor_per_lya"####
names(x)<-sedda_med_lysumma$lya
barplot(x, cex.names = 0.9, ylim = c(0,25), col = c("darkblue", "red"), ylab = "Antal observerade fjällripor", xlab = "Fjällrävslyor", main = "Antal observerade fjällripor per fjällrävslya")

sedda
sedda_med_lysumma %>% 
 spread(ripor_per_lya,lya, c("zz014", "zz020", "zz033", "zz042"))

sedda_med_lysumma
aktiv<-sedda_med_lysumma %>% 
  filter(lya %in% c("zz014", "zz020", "zz033", "zz042"))
summa_aktiv<-aktiv %>% 
  summarise(aktiv_ripor = sum(ripor_per_lya))


inaktiv<-sedda_med_lysumma %>% 
  filter(lya %in% c("zz061", "zz062", "zz076", "zz096", "zz104"))

summa_inaktiv<-inaktiv %>% 
  summarise(inaktiv_ripor = sum(ripor_per_lya))

barplot(summa_aktiv, summa_inaktiv)

skillnad$aktiv <-summa_aktiv$aktiv_ripor
skillnad$inaktiv <-  summa_inaktiv$inaktiv_ripor



summa_aktiv
barplot(table(skillnad$aktiv, summa_inaktiv$inaktiv_ripor), beside = TRUE)
skillnad
?filter
sedda_med_lysumma
test
