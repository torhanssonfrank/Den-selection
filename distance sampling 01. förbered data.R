#' Distance sampling.
#' 
#' Först måste man räkna ut en detection function
#' Vi behöver
#' truncation (w) = bredden på transekten. Funkar inte alltid att ta observationen som ligger längst bort från linjen
#' som gräns. Bäst att ta bort outliers.
#' counts = observationerna
#' effort = längden på transekten. Den är satt från början
#' perpendicular distance (x) = observationernas vinkelräta avstånd från transekten. Kan räknas ut
#' med radial distance (r) och azimut-vinkeln (θ) (som jag har mätt) med formeln x = r sin θ (Buckland 2015)
#' scale parameter (σ) = 
#' Vi måste ha rätt namn på kolumnerna. Distance-paketet känner av namnen i dataramen.
#' Vi behöver:
#' distance = perpendicular distance. Om man inte såg några djur på en transekt måste den läggas till med NA.
#' Sample.Label = namnen på transekterna
#' Effort = längden på transekterna
#' Region.Label = Innehåller olika stratum, det vill säga områden. För mig kanske det skulle kunna vara
#' när trädgräns eller längre ifrån trädgräns, alternativ hög höjd och låg höjd (kan vara bra för produktivitet)
#' Area = arean på strata, det vill säga area nära trädgräns och area långt från trädgräns.

install.packages("rmarkdown")
install.packages("dsm")
install.packages("gdata")
library(Distance)
library(gdata)
library(rmarkdown)
library(dsm)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)


ripdata<-read.xls(file.choose(), stringsAsFactors = FALSE) # läser in tor.modifierade riptransekter vår 2018
View(ripdata)

#Plockar ut riporna
ripor <- c("F", "D", "R")

#' plockar ut de kolumner jag behöver nu. Man kan lägga in fler som variabler sen, typ marktyp, väder, 
#' tid på dagen osv.
rip_dist <- ripdata %>%
  filter(observation %in% ripor) %>% 
  select(lya, observation, distans, vinkel, väder, datum) %>% 
  separate(datum, c("date", "time"), sep = 10 ) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(time = hms::as.hms(time)) #plockar ut en funktion inne i hms. Kör man bara hms blir det formatet 10h 5m 0s istället för 10:05:00


View(rip_dist)
class(rip_dist$date)
class(rip_dist$time)
length(unique(rip_dist$lya)) # alla lyor är med. Ingen saknar observationer.

#' Gör en ny kolumn med period på dagen så att
#' jag kan använda det som en parameter i detection function
#' Börjar med att skapa en tom kolumn
namevector <- "period"
rip_dist[ ,namevector] <- NA

attach(rip_dist)
rip_dist$period[time <= hms::as.hms('8:00:00')] <- "Morning"
rip_dist$period[time >= hms::as.hms('08:00:00') &
                         time <= hms::as.hms('15:00:00')] <- "Day"
rip_dist$period[time > hms::as.hms('15:00:00')] <- "Afternoon"
detach(rip_dist)

View(rip_dist)


#' Alla vinklar som är över 180 grader måste göras om, annars blir avstånden negativa. Jag behöver bara 
#' vinkeln från linjen i det här steget. Höger eller vänster spelar ingen roll.

ny_vinkel <- rip_dist %>% 
  filter(vinkel > 180) %>% 
  mutate(vinkel =  360 - vinkel)
  
ny_vinkel


#tar ut vinklarna under 180 grader i den första dataramen och stoppar in i en ny

vinkel_under <- rip_dist %>%
  filter(vinkel <= 180)

rip_dist_vinkel <- bind_rows(vinkel_under, ny_vinkel)

View(rip_dist_vinkel)

#' räknar om radial-distansen till perpendicular. Sinus, cosinus och tangens är i radianer
#' i R så man måste multiplicera vinkeln med pi och dela med 180 och ta sinus av produkten för
#' att man ska få grader
rip_dist_vinkel<-rip_dist_vinkel %>% 
  mutate(distance = distans * sin(vinkel*pi/180))


View(rip_dist_vinkel)

hist(rip_dist_vinkel$distance, xlab = "perpendicular distance from transect (m)", main = "Observed ptarmigan")

#byter namn på kolumnerna så att Distance känner igen dem
names(rip_dist_vinkel)
colnames(rip_dist_vinkel) <- c("Sample.Label", "observation", "radialdistans", "vinkel", "väder", "date", "time", "period", "distance")

#' skriver ut en fil här och kategoriserar väder manuellt. Det är för mycket olika beskrivningar för att
#' det ska gå att programmera på ett vettigt sätt.

write_xlsx(rip_dist_vinkel, path = "Rawdata/ripor_distansanalys.xlsx")
ripor_analys<-read_xlsx("Rawdata/ripor_distansanalys.xlsx") #la till tre weather-nivåer, "fair", "precip/fog" och "windy"

head(ripor_analys)
rip_perp <-ripor_analys %>% 
  select(Sample.Label, distance, period, weather)

#kanske måste ha alla kolumner för att det ska funka
namevector1 <-c("Area")
rip_perp[ ,namevector1] <- 0

namevector2 <- "Region.Label"
rip_perp[ ,namevector2] <-"Helags"

namevector3 <-"Effort"
rip_perp[ ,namevector3] <-12000 #lägger transektlängden som 12km så länge. Detta måste delas upp.
View(rip_perp)

class(rip_perp$Area)
class(rip_perp$Effort)
class(rip_perp) # DEN VAR BÅDE DATAFRAME, TBL och TBL_DF på samma gång! Därför funkade det inte att lägga på en detection function. Det måste vara en ren data frame
rip_perp <- as.data.frame(rip_perp) #ändrar om till data frame

max(rip_perp$distance) 
#'observationen längst från linjen var 314,6 meter. Sätter truncation till 300
#'så länge. Kommer nog få justera ned det senare.

#sätter en half-normal detection funktion först.

hn.df <- ds(rip_perp, truncation = 300) # default är half-normal detection function. transect = line är också default.
summary(hn.df)
plot(hn.df, main="Half-normal detection function for ptarmigan transects") #half-normal detection function for ptarmigan transects
gof_ds(hn.df, chisq = TRUE) #goodness of fit
#'mer om goodness of fit (måste kolla i chrome): https://workshops.distancesampling.org/online-course/syllabus/Chapter2/
#'p-värde på 0.95. Ett högt p-värde är bra. Betyder att det är liten skillnad mellan datan 
#'och den förväntade linjen. Med chi square kan man se hur bra min half normal detection function förutspår 
#'datan mellan olika interval i meter. Vi ser att mellan 0 och 25 meter förutspår half-normal
#' att det bör vara 37,49 ripor. Jag såg 35. Chi square är då lågt (0.166) eftersom detection function
#' är nära det faktiska observerade antalet. mellan 125 - 150 meter och 150 - 175 meter är det sämre.
#' Jag såg 14 ripor mellan 125 och 150 meter men borde ha sett 9,75. Därför får jag ett högt chi square (1.85).
#' Det är ännu sämre mellan 225 och 250 meter. Jag såg 4 men borde ha sett 1,57 enligt modellen. 
#' Här kan jag alltså överväga att sätta truncation till 225 meter istället för 300 meter.

# Hazard-rate detection function
hr.df <- ds(rip_perp, truncation=300, key= "hr")
summary(hr.df)
plot(hr.df, main="Hazard-rate detection function for ptarmigan transects")
gof_ds(hr.df, chisq = TRUE )
#den här är något sämre


#' Jämför AIC-scores för de två modellerna. Om skillnaden är mindre än 2 är det bäst att ta den enklare
#' modellen, vilket är half-normal. Hazard rate lägger till en extra parameter. 
#' Half normal är dock 6 mindre i det här fallet så då blev valet lätt. 
AIC(hn.df) # 1476.233 (df=3)
AIC(hr.df) # 1482.958 (df=2) hazard rate lägger till en extra parameter.

#' vi kan lägga till fler parametrar till detection function, till exempel väder och tid på dagen.
#' väder och period måste vara sparad som class factor. Vi har redan bestämt oss för half-normal
#' så vi försöker bara förbättra den. Vi skiter alltså i hazard rate.



hn.df.weather <- ds(rip_perp, truncation=300, formula = ~as.factor(weather))
hn.df.period <- ds(rip_perp, truncation=300, formula = ~as.factor(period))
#båda på samma gång
hn.df.weather.period <- ds(rip_perp, truncation=300, formula = ~as.factor(weather)+as.factor(period))

AIC(hn.df) #den här är fortfarande bäst (lägst)
AIC(hr.df)
AIC(hn.df.weather)
AIC(hn.df.period)
AIC(hn.df.weather.period)

