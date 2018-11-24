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
install.packages("Distance")
install.packages("mrds")

library(Distance)
library(knitr)
library(gdata)
library(rmarkdown)
library(dsm)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)


ripdata<-read_xlsx(path = "Den and territory selection/Rawdata/tor_modifierade_riptransekter_vår.xlsx") # läser in tor.modifierade riptransekter vår

View(ripdata)
class(ripdata)
ripdata<-as.data.frame(ripdata)
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
ripor_analys<-read_xlsx("Den and territory selection/Rawdata/ripor_distansanalys_vår_2018.xlsx") #la till tre weather-nivåer, "fair", "precip/fog" och "windy"

head(ripor_analys)
rip_perp <-ripor_analys %>% 
  dplyr::select(Sample.Label,observation, distance, period, weather)

# måste ha alla kolumner som Distance kräver för att det ska funka
namevector1 <-"Area"
rip_perp[ ,namevector1] <- (1500^2) * pi #kör en area som är samma som lybuffer

namevector2 <- "Region.Label"
rip_perp[ ,namevector2] <- paste(rip_perp$Sample.Label) #Jag gör Region.Label samma som Sample.Label. Det blir mycket lättare att räkna ut ripor per lya då eftersom jag får det direkt med "summary"

#' Lägger till rätt längd på alla transekter under en ny kolumn; "Effort". Parar
#' ihop med rätt lynamn


transects <- data.frame(Effort = c(12700, 12100, 11900, 12000, 11900, 12100, 12100, 12100, 11700, 12400),
                       Sample.Label = c("zz020", "zz104", "zz042", "zz033", "zz062", "zz014", "zz096", "zz075", "zz076", "zz061"))
class(transects$Effort)#numeric
transects


rip_perp <- rip_perp %>% 
  left_join(transects, by = "Sample.Label")

transects
View(rip_perp)

class(rip_perp$Area)
class(rip_perp$Effort)

class(rip_perp) # DEN VAR BÅDE DATAFRAME, TBL och TBL_DF på samma gång! Read_xlsx läser in datat så. Därför funkade det inte att lägga på en detection function. Det måste vara en ren data frame
rip_perp <- as.data.frame(rip_perp) #ändrar om till data frame



max(rip_perp$distance) 
#'observationen längst från linjen var 314,6 meter. Sätter truncation till 300
#'så länge. Kommer nog få justera ned det senare.

#Bara 18 R och 19 D
length(which(rip_perp$observation=="R"))
length(which(rip_perp$observation=="D"))
length(rip_perp$observation)

#sätter en half-normal detection funktion först.
hist(rip_perp$distance)
hn.df <- ds(rip_perp, truncation = 300, adjustment = NULL) # default är half-normal detection function. 
#' transect = line är också default. cosinus-adjustment är default så det måste man ta bort om man vill köra
#' utan
summary(hn.df)

par(mfrow=c(1,2))
plot(hn.df, main = "half-normal detection function for ptarmigan transects")
gof_ds(hn.df, chisq = TRUE) #goodness of fit. Inte så bra fit. p-värde lågt (dåligt)

## ÖKA model fit med adjustments ##

#' man kan lägga på adjustments cosine, Hermite polynomial och Simple polynomial.
#' Så här står det på den här hemsidan: http://converged.yt/RDistanceBook/distance-moredf.html
#' It is not recommended to add adjustments (cos, Hermite polynomial etc) 
#' to models when covariates (weather, period etc)
#' are included, as they can lead to detection probabilities that are 
#' greater than 1 for some distances and make it difficult to 
#' diagnose monotonicity problems. 
#' Det är lite debatt om det här. Det kan bli för många parametrar men kan funka ändå.

#' cosinus adjustment är default i ds. Behöver alltså inte specificera. Nya distance gör order manuellt.
#' Den testar upp til 5 och väljer den bästa med AIC. I den här beskrivningen: 
#' (https://synergy.st-andrews.ac.uk/ds-manda/files/2016/11/montrave.pdf)
#' används gamla distance och då måste man specificera själv.
hn.df.cos <- ds(rip_perp, truncation = 300)
summary(hn.df.cos)
plot(hn.df.cos, main="Half-normal detection function with cosine adjustment for ptarmigan transects")
gof_ds(hn.df.cos, chisq = TRUE) #goodness of fit
#'mer om goodness of fit (måste kolla i chrome): https://workshops.distancesampling.org/online-course/syllabus/Chapter2/
#'p-värde på 0.95. Ett högt p-värde är bra. Betyder att det är liten skillnad mellan datan 
#'och den förväntade linjen. Med chi square kan man se hur bra min half normal detection function förutspår 
#'datan mellan olika interval i meter. Vi ser att mellan 0 och 25 meter förutspår half-normal
#' att det bör vara 37,49 ripor. Jag såg 35. Chi square är då lågt (0.166) eftersom detection function
#' är nära det faktiska observerade antalet. mellan 125 - 150 meter och 150 - 175 meter är det sämre.
#' Jag såg 14 ripor mellan 125 och 150 meter men borde ha sett 9,75. Därför får jag ett högt chi square (1.85).
#' Det är ännu sämre mellan 225 och 250 meter. Jag såg 4 men borde ha sett 1,57 enligt modellen. 
#' Här kan jag alltså överväga att sätta truncation till 225 meter istället för 300 meter.

# Half-normal med Hermite polynomial adjustment
hn.df.hermite <- ds(rip_perp, truncation = 300, adjustment = "herm")
summary(hn.df.hermite)
gof_ds(hn.df.hermite, chisq = TRUE ) #dålig fit

# Hazard-rate detection function
hr.df <- ds(rip_perp, truncation=300, key= "hr", adjustment = NULL)
summary(hr.df)
plot(hr.df, main="Hazard-rate detection function for ptarmigan transects")
gof_ds(hr.df, chisq = TRUE )

# Hazard-rate detection function med cosine adjustment
hr.df.cos <- ds(rip_perp, truncation=300, key= "hr")
summary(hr.df.cos)
gof_ds(hr.df.cos, chisq = TRUE )
plot(hr.df, main="Hazard-rate detection with cosine adjustment function for ptarmigan transects")

# Hazard-rate detection function med simple polynomial adjustment
hr.df.poly <- ds(rip_perp, truncation = 300, key = "hr", adjustment = "poly")
summary(hr.df.poly)
gof_ds(hr.df.poly, chisq = TRUE ) #bättre fit. P = 0.90 med bra qq-plot
plot(hr.df.poly)

#' Börjar med en ny df fourier/uniform med cosine adjustment. Den måste ha cosine annars funkar den inte. 
#' Behöver inte specificera order
fourier.df.cos <- ds(rip_perp, truncation=300, key = "unif") # cos är default
summary(fourier.df.cos)
gof_ds(fourier.df.cos, chisq = TRUE ) # inte så bra qq-plot. P = 0.37
plot(fourier.df.cos)
#' Jämför AIC-scores för alla modellerna. Om skillnaden är mindre än 2 är det bäst att ta den enklare
#' modellen, vilket är half-normal eftersom hazard rate lägger till en extra parameter. 
#' Half normalmed cosine adjustment är dock  mindre än hazard rate i det här fallet så då blev valet lätt. 
summarize_ds_models(hn.df, hn.df.cos, hn.df.hermite, hr.df, hr.df.cos, hr.df.poly, fourier.df.cos)
#' det som är lite störigt med summarize_ds_models är att namnen på detection
#' functions inte syns så tydligt. Det står siffror längst till vänster som anger
#' positionen som detection function blev inläst. hn.df är alltså 1 eftersom jag
#' skrev in den först i raddan över. hn.df.cos är nummer 2. Vi kan se att två har 0
#' i deltaAIC. den har alltså lägst AIC.

#Vi kan printa ut alla AIC-scores för att göra det tydligare.

AIC(hn.df) # 1481.226 (df=1)
AIC(hn.df.cos)# 1476.233 (df=3)
AIC(hn.df.hermite) # 1481.226 (df=1)
AIC(hr.df) # 1482.958 (df=2) hazard rate lägger till en extra parameter.
AIC(hr.df.cos) # 1477.127 (df=4) hazard rate lägger till en extra parameter.
AIC(hr.df.poly)# 1478.574 (df=3)
AIC(fourier.df.cos) #1480.189 (df=3)

#' vi kan lägga till fler parametrar till detection function, till exempel väder och tid på dagen.
#' väder och period måste vara sparad som class factor. Vi har redan bestämt oss för half-normal
#' så vi försöker bara förbättra den. Vi skiter alltså i hazard rate.


# först utan adjustments. Det är säkrast
hn.df.weather <- ds(rip_perp, truncation=300, formula = ~as.factor(weather), adjustment = NULL)
hn.df.period <- ds(rip_perp, truncation=300, formula = ~as.factor(period), adjustment = NULL)
#båda på samma gång
hn.df.weather.period <- ds(rip_perp, truncation=300, formula = ~as.factor(weather)+as.factor(period), adjustment = NULL)

#' testar med cosinus adjustment. För att det ska funka med covariates måste man 
#' slå av monotonicity, det vill säga att funktionen antar att upptäckta djur
#' minskar med avståndet från linjen. Det här kan bli helknäppt, så man måste kolla
#' grafen noga efteråt. Order måste också anges
hn.df.weather.cos <- ds(rip_perp, truncation=300, formula = ~as.factor(weather), adjustment = "cos", order = 2, monotonicity = FALSE)
plot(hn.df.weather.cos)
check.mono(hn.df.weather.cos$ddf,n.pts=100) #sannolikheten att se ripor stiger aldrig med ökat avstånd. Om plot är true här plottar den svinmånga kurvor.

gof_ds(hn.df.weather.cos) # sämre qq-plot och p = 0.386

hn.df.period.cos <- ds(rip_perp, truncation=300, formula = ~as.factor(period), adjustment = "cos", order = 2, monotonicity = FALSE)
plot(hn.df.period.cos) 
gof_ds(hn.df.period.cos) #dålig qq-plot och p = 0.396
check.mono(hn.df.period.cos$ddf,n.pts=100)#minskar hela vägen med ökat avstånd

hn.df.weather.period.cos <- ds(rip_perp, truncation=300, formula = ~as.factor(weather)+as.factor(period), adjustment = "cos", order = 2, monotonicity = FALSE)
plot(hn.df.weather.period.cos) #minskar med ökat avstånd hela vägen
gof_ds(hn.df.weather.period.cos) #sämre qq-plot, p = 0.384
check.mono(hn.df.weather.period.cos$ddf,n.pts=100)#minskar hela vägen med ökat avstånd


summarize_ds_models(hn.df.cos, hn.df.weather, hn.df.period, hn.df.weather.period, hn.df.weather.cos, hn.df.period.cos, hn.df.weather.period.cos)
AIC(hn.df.cos) #den här är fortfarande bäst (lägst). I summeringen är den numrerad 1.

#' Model gives a short description of fitted model (this may be ambiguous so the row numbers may be helpful
#'  in working out which model is which).
#' Formula describes the covariate model (just ~1 when there are no covariates).
#' pars gives the number of parameters in the model.
#' P_a lists the average probability of detection.
#' CV(P_a) gives the coefficient of variation of average probability of detection giving an indication of 
#' uncertainty in the model (more on this in How certain are we in our estimates?).
#' AIC finally lists Akaike’s information criterion for the model


plot(hn.df.weather.period)
covar.fit <- ddf.gof(hn.df.weather.period$ddf, main = "Weather and period of day as covariates") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(covar.fit$dsgof$CvM$W,3), 
                 "\nP=", round(covar.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8) #första delen anger positionen i grafen där texten "message" ska klistras in
?plot
par(mfrow=c(1,2))
plot(hn.df.cos, main = "Ptarmigan spring 2018, Half-normal detection function with cosine adjustment, no covar.")

no.covar.fit <- ddf.gof(hn.df.cos$ddf, main = "QQ-plot") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(no.covar.fit$dsgof$CvM$W,3), 
                 "\nP=", round(no.covar.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8)

#' Abundance estimation finns i summary. Eftersom transekterna ligger 
#' under region label får jag abundance per lya direkt
summary(hn.df.cos)

#testar att få en subset av arter

regions <- data.frame(Region.Label = rip_perp$Region.Label, Area = rip_perp$Area)
samples <-data.frame(Region.Label = rip_perp$Region.Label, Sample.Label =rip_perp$Sample.Label,
                     Effort = rip_perp$Effort )
obs.table <- data.frame(Region.Label = rip_perp$Region.Label, 
                        Sample.Label =rip_perp$Sample.Label, 
                        object = rip_perp$observation)

region.drop <- subset(regions, Region.Label!= "zz075") 
length(unique(region.drop$Region.Label))
sample.drop <- subset(samples, Region.Label!= "zz075")
length(unique(sample.drop$Sample.Label))
updated_myData <- subset(myData, id!= 6)

View(samples)

dht(hn.df.cos, subset = object == "F" ) #måste få in region.table och sample.table

ds(rip_perp, formula=~1, region.table=regions, sample.table=samples)

regions
dht
?mrds

#' Gör en tabel med kable från paketet knitr. 
#' Står beskrivet i Miller et al. Distance sampling in R. (har på PDF)
#' här förklaras uncertainty: http://converged.yt/RDistanceBook/distance-uncertainty.html#fn6
rip_table <- summary(hn.df.cos)$dht$individuals$N
rip_table$lcl <- rip_table$ucl <- rip_table$df <- NULL
colnames(rip_table) <- c("Den code", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)",
                           "$\\text{CV}(\\hat{N}$)")

rip_table$`Den code` <- paste0('fs', rip_table$`Den code`)
rip_table<- rip_table%>% 
  mutate(`Den code` = toupper(`Den code`)) %>% 
  mutate_if(is.numeric, round, 2) #avrundar till två värdesiffror

rip_table$`Den code`[11] <- "Total"
rip_table
#printar en fil för markdowntabell
write_xlsx(rip_table, path = "Den and territory selection/plottar/riptabell.estimat.stats.xlsx")
?kable
kable(rip_table, format = "markdown")

#printar en excelfil

ripa_estimated <- summary(hn.df.cos)$dht$individuals$N


ripa_estimated<-ripa_estimated %>% 
  dplyr::select(Label, Estimate, se) %>% 
  slice(-11) #nedersta raden innehåller totala antalet ripor. Tar bort den

colnames(ripa_estimated) <- c("lya", "uppskattat_antal_ripor", "standard_error")

ripa_estimated
write_xlsx(ripa_estimated, path = "Rawdata/Uppskattat antal ripor vår distance_sampling.xlsx")

#' testar detection function för dalripa separat och gör histogram
#' för distans från linjen för fjällripa och dalripa för att jämföra


dal_perp <- rip_perp %>% 
  filter(observation == "D")
length(dal_perp$observation)

fjell_perp <- rip_perp %>% 
  filter(observation == "F")
  
fjell_perp
length(fjell_perp$observation)

hist(fjell_perp$distance, main = "Rock ptarmigan observations (n = 105)", xlab = "distance from transect line (m)")
hist(dal_perp$distance, main = "Willow ptarmigan observations (n = 19)", xlab = "distance from transect line (m)")

g.fjell<-ggplot(data=fjell_perp, aes(fjell_perp$distance)) + 
  geom_histogram(breaks=seq(0, 350, by = 50),
col="black", 
fill="grey", 
alpha = .5)+ #färgtransparens 
theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  
g.dal<-ggplot(data=dal_perp, aes(dal_perp$distance)) + 
  geom_histogram(breaks=seq(0, 350, by = 50),
                 col="black", 
                 fill="grey", 
                 alpha = .5)+ #färgtransparens 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  scale_y_continuous(breaks=seq(0, 12, 3))  # sätter min och maxvärdena för vad som ska visas på y-axeln (inte datat, bara axeln). 0.2 är intervallet. 
g.fjell
g.dal

# lägger ihop plottarna

theme_set(theme_pubr())
ripcomboplot <- ggarrange(g.fjell, g.dal,
                       labels = c("a) Rock ptarmigan (n = 105)", "b) Willow ptarmigan (n = 19)"),
                       font.label = list(size = 9, color = "black", face = "bold"),
                       ncol = 2, nrow = 1)
ripcomboplot
ripcomboplot<-annotate_figure(ripcomboplot,
                                bottom = text_grob("Distance from transect line (m)",
                                                   face = "bold", size = 15),
                                left = text_grob("Frequency", 
                                                 face = "bold", rot = 90, size = 15))
             
ripcomboplot             
ggexport(ripcomboplot, filename = "ripcomboplot.jpeg",width = 2000, height = 1000, res = 300)
?ggexport

#' detection function för dalripa
d.hn.df <- ds(dal_perp, truncation = 300, adjustment = NULL)
summary(d.hn.df)
gof_ds(d.hn.df)
plot(d.hn.df)

d.hn.df.cos <- ds(dal_perp, truncation = 300)  
summary(d.hn.df.cos)
gof_ds(d.hn.df.cos)
plot(d.hn.df.cos)

d.hn.df.hermite <- ds(dal_perp, truncation = 300, adjustment = "herm")
summary(d.hn.df.hermite)
gof_ds(d.hn.df.hermite, chisq = TRUE )
plot(d.hn.df.hermite)

d.hr.df <- ds(dal_perp, truncation = 300, adjustment = NULL)
summary(d.hr.df)
gof_ds(d.hr.df)
plot(d.hr.df)

d.hr.df.cos <- ds(dal_perp, truncation = 300)
summary(d.hr.df.cos)
gof_ds(d.hr.df.cos)
plot(d.hr.df)

d.hr.df.poly <- d.hr.df.cos <- ds(dal_perp, truncation = 300, adjustment = "poly" )
summary(d.hr.df.poly)
gof_ds(d.hr.df.poly)
plot(d.hr.df.poly)

d.fourier.df.cos <- ds(dal_perp, truncation=300, key = "unif") # cos är default
summary(d.fourier.df.cos)
gof_ds(d.fourier.df.cos, chisq = TRUE ) # inte så bra qq-plot. P = 0.37
plot(d.fourier.df.cos)

# half normal detection function med cosine blev bäst här också
summarize_ds_models(d.hn.df, d.hn.df.cos, d.hn.df.hermite, d.hr.df, d.hr.df.cos, d.hr.df.poly, d.fourier.df.cos) 
