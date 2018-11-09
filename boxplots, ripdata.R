library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

ripor_sommar <- read_xlsx(path ="Den and territory selection/Rawdata/tor.riptransekter helags sommar 2018 modifierade RS_avstånd för dubbla högar.xlsx")

ripdata<-read_xlsx(path = file.choose()) # läser in tor.modifierade riptransekter vår 2018
View(ripdata)
rip_uppskattat <- read_xlsx(path ="Den and territory selection/Rawdata/Uppskattat antal ripor vår distance_sampling.xlsx")
skit_uppskattat <- read_xlsx(path ="Den and territory selection/Data/Uppskattat antal ripspillningshögar Helags sommar 2018 distance_sampling.xlsx")
kullar.tot <- read_xlsx(path = "Den and territory selection/Rawdata/antal kullar per lya 2000_2018.xlsx")
lyvars <- read_xlsx(path = "kärnlyor Helags AIC 2000 - 2018.xlsx") # har höjddata
riplyor.vars<-read_xlsx(path ="Den and territory selection/Data/lyvariabler.aic.xlsx")
  
rip_uppskattat <- as.data.frame(rip_uppskattat)
skit_uppskattat<- as.data.frame(skit_uppskattat)
kullar.tot<- as.data.frame(kullar.tot)
lyvars<- as.data.frame(lyvars)
riplyor.vars<- as.data.frame(riplyor.vars)
#Såg bara 15 ripor under sommaren
length(which(ripor_sommar$observation %in% c("F", "D", "R")))

#Börjar med distance-sampling-datat
# Delar upp bra och dåliga lyor
aktiv_upps<- rip_uppskattat %>% 
  filter(lya %in% c("zz014","zz020", "zz033", "zz042"))

aktiv_upps


inaktiv_upps<- rip_uppskattat %>% 
  filter(lya %in% c("zz104","zz062", "zz096", "zz061", "zz075", "zz076"))

inaktiv_upps

#aktiv_upps
fox <- as.data.frame(c(1,1,1,1))
colnames(fox)<-"fox"
fox
x<-bind_cols((aktiv_upps), fox)

#inaktiv_upps
fox <- as.data.frame(c(0,0,0,0,0,0))
colnames(fox)<-"fox"
y <- bind_cols(inaktiv_upps, fox)
y
fox.dens <- bind_rows(x,y)
fox.dens

wilcox.test(fox.dens$uppskattat_antal_ripor ~ fox.dens$fox) #funkar inte
?wilcox.test

boxplot(fox.dens$uppskattat_antal_ripor ~ fox.dens$fox, names = c("sällan aktiva lyor (n = 6)", "ofta aktiva lyor (n = 4)"), main = ("Uppskattat antal ripor med 1500 m radie runt fjällrävslyor, Helags"), 
        ylab = ("Uppskattat antal ripor"), ylim = c(0,100))


#Plockar ut obsarna
alla_obs <-ripdata %>% 
  filter(observation %in% c("F", "D", "R", "RS", "SP"))
  
unique(alla_obs$observation)

#histogram för alla obsar
obs_vinkel <- alla_obs %>% 
  filter(vinkel > 180) %>% 
  mutate(vinkel =  360 - vinkel)

obs_vinkel$vinkel

# Gör histogram för alla obsars rätvinkliga avstånd från linjen på vårdatat
#tar ut vinklarna under 180 grader i den första dataramen och stoppar in i en ny

obs_vinkel_under <- alla_obs %>%
  filter(vinkel <= 180)

alla_obs_vinkel <- bind_rows(obs_vinkel_under, obs_vinkel)

View(alla_obs_vinkel)

#' räknar om radial-distansen till perpendicular. Sinus, cosinus och tangens är i radianer
#' i R så man måste multiplicera vinkeln med pi och dela med 180 och ta sinus av produkten för
#' att man ska få grader
alla_obs_vinkel<-alla_obs_vinkel %>% 
  mutate(distance = distans * sin(vinkel*pi/180))


View(alla_obs_vinkel)

hist(alla_obs_vinkel$distance, xlab = "perpendicular distance from transect (m)", main = "Observed ptarmigan, points with droppings and tracks", breaks = 100)


# Delar upp bra och dåliga lyor
aktiv<- alla_obs %>% 
  filter(lya %in% c("zz014","zz020", "zz033", "zz042"))

head(aktiv)
unique(aktiv$lya)
View(aktiv)  

aktivsumma<-aktiv %>% 
  group_by(lya) %>% 
  count()
  

View(aktivsumma)

inaktiv<- alla_obs %>% 
  filter(lya %in% c("zz104","zz062", "zz096", "zz061", "zz075", "zz076"))

head(inaktiv)
unique(inaktiv$lya)


inaktivsumma<-inaktiv %>% 
  group_by(lya) %>% 
  count()
  

View(inaktivsumma)



#aktivsumma
fox <- as.data.frame(c(1,1,1,1))
colnames(fox)<-"fox"
fox
x<-bind_cols((aktivsumma), fox)

#inaktivsumma
fox <- as.data.frame(c(0,0,0,0,0,0))
colnames(fox)<-"fox"
y <- bind_cols(inaktivsumma, fox)
y
fox.dens <- bind_rows(x,y)
fox.dens
par(mfrow=c(1,1))
boxplot(fox.dens$n ~ fox.dens$fox, names = c("sällan aktiva lyor (n = 6)", "ofta aktiva lyor (n = 4)"), main = ("Observationer av ripor, positioner med ripspillning samt positioner med spår på 12 km transekter runt fjällrävslyor, Helags"), 
        ylab = ("observationer"), ylim = c(0,50))

# Boxplot utan spår
aktiv
aktivsumma

aktivsumma_utanspår<-aktiv %>% 
  filter(observation %in% c("F", "R", "D", "RS")) %>% 
  group_by(lya) %>% 
  count()
  
  inaktivsumma_utanspår<-inaktiv %>% 
    filter(observation %in% c("F", "R", "D", "RS")) %>% 
    group_by(lya) %>% 
    count()
  
  

  
  #aktivsumma utan spår
  fox <- as.data.frame(c(1,1,1,1))
  colnames(fox)<-"fox"
  fox
  x<-bind_cols((aktivsumma_utanspår), fox)
  
  #inaktivsumma utan spår
  fox <- as.data.frame(c(0,0,0,0,0,0))
  colnames(fox)<-"fox"
  y <- bind_cols(inaktivsumma_utanspår, fox)
  y
  fox.dens <- bind_rows(x,y)
  fox.dens
  
  boxplot(fox.dens$n ~ fox.dens$fox, names = c("sällan aktiva lyor (n = 6)", "ofta aktiva lyor (n = 4)"), main = ("Observationer av ripor och positioner med ripspillning på 12 km transekter runt fjällrävslyor, Helags"), 
          ylab = ("observationer"), ylim = c(0,50))
  
## gör plot på ripor mot antal kullar och höjd ####
rip_uppskattat<-rip_uppskattat %>% 
    dplyr::rename(Namn = lya) %>% 
    mutate(Namn = toupper(Namn))

rip_uppskattat$Namn<-paste0('FS', rip_uppskattat$Namn)

skit_uppskattat <- skit_uppskattat %>% 
  dplyr::rename(Namn = lya) %>% 
  mutate(Namn = toupper(Namn))

skit_uppskattat$Namn<-paste0('FS', skit_uppskattat$Namn)

rip_uppskattat <- rip_uppskattat %>% 
  left_join(kullar.tot, by = "Namn") %>% 
  left_join(lyvars, by = "Namn") %>% 
  dplyr::select(Namn, uppskattat_antal_ripor, kullar_totalt, hojd_over_havet) %>% 
  distinct()

skit_uppskattat <- skit_uppskattat %>% 
  left_join(kullar.tot, by = "Namn") %>% 
  left_join(lyvars, by = "Namn") %>% 
  dplyr::select(Namn, uppskattat_antal_ripspillningshögar, kullar_totalt, hojd_over_havet) %>% 
  distinct()

rip_uppskattat
skit_uppskattat
?ggplot

## plot antal ripor mot antal kullar ####
ggplot(data=rip_uppskattat, aes(x=kullar_totalt, y=uppskattat_antal_ripor))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x = "Litters per den")+
  labs(y = "Relative ptarmigan abundance around dens")+
  scale_x_continuous(breaks=seq(0, 14, 2))

#' Gör några statistiska test. Kategoriserar i bra lyor (fler än 6 reproduktioner)
#' och dålig lyor (färre än 6)

rip_uppskattat$bra.dålig <- as.numeric(NA)
rip_uppskattat$bra.dålig[rip_uppskattat$kullar_totalt>=6]<-1
rip_uppskattat$bra.dålig[rip_uppskattat$kullar_totalt<6] <- 0

wilcox.test(rip_uppskattat$uppskattat_antal_ripor  ~  rip_uppskattat$bra.dålig) # Det blir ett Wilcoxon rank som test det är equivalent to Mann-Whitney U-test

kruskal.test(rip_uppskattat$uppskattat_antal_ripor  ~  rip_uppskattat$bra.dålig)

?wilcox.test
## plot antal ripskitar mot antal kullar ####
ggplot(data=skit_uppskattat, aes(x=kullar_totalt, y=uppskattat_antal_ripspillningshögar))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x = "Litters per den")+
  labs(y = "Relative ptarmigan droppings abundance around dens")+
  scale_x_continuous(breaks=seq(0, 14, 2))



#' Gör några statistiska test. Kategoriserar i bra lyor (fler än 6 reproduktioner)
#' och dålig lyor (färre än 6)

skit_uppskattat$bra.dålig <- as.numeric(NA)
skit_uppskattat$bra.dålig[skit_uppskattat$kullar_totalt>=6]<-1
skit_uppskattat$bra.dålig[skit_uppskattat$kullar_totalt<6] <- 0

wilcox.test(skit_uppskattat$uppskattat_antal_ripspillningshögar  ~  skit_uppskattat$bra.dålig) # Det blir ett Wilcoxon rank som test det är equivalent to Mann-Whitney U-test

kruskal.test(skit_uppskattat$uppskattat_antal_ripspillningshögar  ~  skit_uppskattat$bra.dålig)
## plot antal ripor mot höjd över havet ####
ggplot(data=rip_uppskattat, aes(x=hojd_over_havet, y=uppskattat_antal_ripor))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x = "Den altitude (m)")+
  labs(y = "Relative ptarmigan abundance around dens")

## plot antal ripskitar mot höjd över havet ####
ggplot(data=skit_uppskattat, aes(x=hojd_over_havet, y=uppskattat_antal_ripspillningshögar))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x = "Den altitude (m)")+
  labs(y = "Relative ptarmigan droppings abundance around dens")

# lägger på lyarea och de andra lyvariablerna
ripor.lyarea<-rip_uppskattat %>% 
  left_join(riplyor.vars, by = "Namn")

str(ripor.lyarea)
ripor.lyarea$area<-as.numeric(ripor.lyarea$area)
ripor.lyarea$snöfri.area<-as.numeric(ripor.lyarea$snöfri.area)
ripor.lyarea<-ripor.lyarea %>% 
  mutate(andel.snöfri = snöfri.area/area)

## plottar lyarea mot antal kull
ggplot(data=ripor.lyarea, aes(x=kullar_totalt, y=area))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(x = "Litters per den")+
  labs(y = "Den area (m^2)")+
  scale_x_continuous(breaks=seq(0, 14, 2))  


## Gör en kombinerad figur med ripor och ripskit mot kull ####
library(ggpubr)

rip<-ggplot(data=rip_uppskattat, aes(x=kullar_totalt, y=uppskattat_antal_ripor))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=seq(0, 14, 2))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())

skit<-ggplot(data=skit_uppskattat, aes(x=kullar_totalt, y=uppskattat_antal_ripspillningshögar))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_x_continuous(breaks=seq(0, 14, 2))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())



  library(gridExtra)
  
  theme_set(theme_pubr())
  skit.rip.plot <- ggarrange(rip,skit,
                         labels = c("  Ptarmigan", "Ptarmigan droppings"),
                         font.label = list(size = 15, color = "black", face = "bold"),
                         ncol = 2, nrow = 1)


  skit.rip.plot
  skit.rip.plot<-annotate_figure(skit.rip.plot,
                                  bottom = text_grob("Litters per den",
                                                     face = "bold", size = 20),
                                 left = text_grob("Relative abundance", 
                                                  face = "bold", rot = 90, size = 20))
                                 
               
  skit.rip.plot          
  ggexport(skit.rip.plot, filename = "skit.rip,comboplot.jpeg", width = 1600, height = 600)
?ggexport  
  