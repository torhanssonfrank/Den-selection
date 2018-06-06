library(dplyr)
library(tidyr)
install.packages("tidyverse")

fjallrav<-read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE)
View(fjallrav)


minalyor<- fjallrav %>% 
  filter(denNr %in% c("FSZZ020", "FSZZ104", "FSZZ042", "FSZZ033", "FSZZ099", "FSZZ062", "FSZZ014", "FSZZ096", "FSZZ076", "FSZZ075", "FSZZ061"))

minalyor %>% 
  filter(denNr %in% "FSZZ020")
View(minalyor)

minalyor %>% 
  count(denNr) #ger antal år med kullar per lya. Dock kan det finnas år med två kullar per lya####

årmedkullar<-fjallrav %>% 
  count(denNr)  #samma för alla lyor####

View(årmedkullar)

levels(fjallrav$denNr)
fjallrav %>% 
  filter(year %in% c("2003", "2006", "2009", "2012", "2016")) #dessa år fattas i filen! Det var åtminstone kullar 2016. Finns i rovbase####
table(årmedkullar)
which(table(årmedkullar$denNr)) == max(table(årmedkullar$n)) #Funkar inte även när jag gör om denNr till logical. Vad är fel?####
class(årmedkullar$denNr)

