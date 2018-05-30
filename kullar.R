library(dplyr)
library(tidyr)
install.packages("tidyverse")

fjallrav<-read.csv(file.choose(), header = TRUE)
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

which(årmedkullar$n = )
