library(dplyr)
library(tidyr)

ripa<-read.csv(file.choose(), header = TRUE, sep = ";")
head(ripa)

sedda<-ripa %>% 
  select(lya, observation, antal) %>% 
  filter(observation == "F") %>% 
  group_by(lya)

View(sedda)

which(is.na(sedda$antal)) #det fattades 3 värden. Jag la till de i rådatan eftersom de fanns nedskrivna på pappren####


sedda
?filter
?logical
?complex

sedda  
  
  