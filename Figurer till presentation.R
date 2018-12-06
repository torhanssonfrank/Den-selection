# figurer till presentation

library(readxl)
library(tidyverse)
library(ggplot2)
library(magrittr)

kullar.lyor <- read_xlsx(path = "Den and territory selection/plottar/kullar.totalt.per.lya.kärnlyor.xlsx")
kullar.lyor<-as.data.frame(kullar.lyor)
str(kullar.lyor)

#Gör om alla kolumner som innehåller ordet "itters" från character till numeric
kullar.lyor <- kullar.lyor %>% 
  mutate_at(vars(contains("litters")), funs(as.numeric(as.character(.))))

# tar bort summeringsraderna längst ner
bara.kullar<-kullar.lyor %>% 
  filter(str_detect(`Den code`, 'FSZZ'))



str(bara.kullar)
g.kull.alla<-ggplot(data=bara.kullar, aes(x=`Den code`, y=`Total litters`, fill=`Total litters`)) +
  scale_x_discrete(limits = bara.kullar$`Den code`) + #säger år ggplot att hålla samma ordning på kolumnerna som i dataramen
  geom_bar(stat="identity")+
  labs(x = "Reproductive dens")+
  labs(y = "Years with litters")+
  theme(axis.text.x = element_blank())
  
g.kull.alla<- g.kull.alla+
  theme(axis.title=element_text(size=17,face="bold"))+ # ändrar axeltitlarnas storlek
  coord_cartesian(ylim=c(0, 19))+ # sätter plottens zoom. Jag vill ha utrymme ovanför staplarna så att texten får plats
  scale_y_continuous(breaks=seq(0, 19))+ # sätter y-axelns interval
  scale_fill_continuous(limits = c(0,14), breaks = c(0, 5, 10, 14), #ändrar färgguidens intervall
                        guide = guide_colourbar(nbin = 14, draw.ulim = TRUE, draw.llim = TRUE)) #ändrar färgguidens boxar

g.kull.alla

ggsave("barplot.kullar.per.lya.png")
