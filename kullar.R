library(dplyr)
library(tidyr)

fjallrav<-read.csv(file.choose(), header = TRUE)
View(fjallrav)


minalyor<- fjallrav %>% 
  filter(denNr %in% c("FSZZ020", "FSZZ104", "FSZZ042", "FSZZ033", "FSZZ099", "FSZZ062", "FSZZ014", "FSZZ096", "FSZZ076", "FSZZ075", "FSZZ061"))

minalyor %>% 
  filter(denNr %in% "FSZZ014")
View(minalyor)

