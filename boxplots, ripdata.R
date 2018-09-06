library(dplyr)
library(tidyr)
library(readxl)

ripor_sommar <- read_xlsx(path ="Rawdata/tor.riptransekter helags sommar 2018 modifierade RS_avstånd för dubbla högar.xlsx")

ripdata<-read_xlsx(path = file.choose()) # läser in tor.modifierade riptransekter vår 2018
View(ripdata)
rip_uppskattat <- read_xlsx(path ="Rawdata/Uppskattat antal ripor vår distance_sampling.xlsx")

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
  
