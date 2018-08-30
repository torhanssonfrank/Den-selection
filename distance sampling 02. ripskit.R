#Distance sampling för ripbajs, sommar.

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

#' läser in tor.riptransekter helags sommar 2018 modifierade RS_avstånd för dubbla högar. 
#' I den filen ligger alla ripskitshögarna på en ensam rad. Jag har gjort dubletter
#' av de rader som hade dubbla högar (var bara 3. Jag nästan alla högar som separata
#' obsar under datainsamlingen) och lagt till 10 cm på den ena.
rip.orginal<-read_xlsx("Rawdata/tor.riptransekter helags sommar 2018 modifierade RS_avstånd för dubbla högar.xlsx") 
View(rip.orginal)

#Plockar ut ripskit
ripskit <- "RS"

#' Jag skiter i covariate-variabler. Väder
#' och tid på dygnet kommer inte påverka hur många skiter jag ser.
#' Det var bara tre rader med dubbla högar så ingen mening att köra högar
#' som size. Jag behöver inte vinkel eftersom jag bara tog 90 och 270 grader
#' på ripskiten. Distansen jag har är alltså redan perpendicular-distansen.
skit_dist <- rip.orginal %>%
  filter(observation %in% ripskit) %>% 
  select(lya, observation, distans)
  

View(skit_dist)

length(unique(rip_dist$lya)) # alla lyor är med. Ingen saknar observationer.


hist(skit_dist$distans, xlab = "perpendicular distance from transect (m)", main = "Observed ptarmigan dropping piles")

#byter namn på kolumnerna så att Distance känner igen dem
names(skit_dist)
colnames(skit_dist) <- c("Sample.Label", "observation", "distance")

head(skit_dist)

#plockar bort observation
skit_dist <-skit_dist %>% 
  select(-observation)

#kanske måste ha alla kolumner för att det ska funka
namevector1 <-"Area"
skit_dist[ ,namevector1] <- (1500^2) * pi #kör en area som är samma som lybuffer

namevector2 <- "Region.Label"
skit_dist[ ,namevector2] <- paste(skit_dist$Sample.Label) #Jag gör Region.Label samma som Sample.Label. Det blir mycket lättare att räkna ut ripor per lya då eftersom jag får det direkt med "summary"

#' Lägger till rätt längd på alla transekter under en ny kolumn; "Effort". Parar
#' ihop med rätt lynamn
zz014 12100
zz096 12100
zz042 11800
zz104 12100
zz020 12700
zz062 11900
zz033 12000
zz076 11700
zz075 12100
zz061 12400

transects2 <- data.frame(Effort = c(12100,12100,11800, 12100, 12700,11900, 12000, 11700, 12100, 12400),
                        Sample.Label = c("zz014", "zz096", "zz042", "zz104", "zz020", "zz062", "zz033", "zz076", "zz075", "zz061"))
class(transects2$Effort)#numeric



skit_dist <- skit_dist %>% 
  left_join(transects2, by = "Sample.Label")

transects
View(skit_dist)

class(skit_dist$Area)
class(skit_dist$Effort)

class(skit_dist) # DEN VAR BÅDE DATAFRAME, TBL och TBL_DF på samma gång! Därför funkade det inte att lägga på en detection function. Det måste vara en ren data frame
skit_dist <- as.data.frame(skit_dist) #ändrar om till data frame

max(skit_dist$distance) 
#'observationen längst från linjen var 3,42 meter. Sätter truncation till 3 m
#'så länge. Kommer nog få justera ned det senare.

#sätter en half-normal detection funktion först.

rs.hn.df <- ds(skit_dist, truncation = 3, adjustment = NULL) # funkar inte! Passar
# datat för dåligt!
summary(rs.hn.df)

## ÖKA model fit med adjustments ##

rs.hn.df.cos <- ds(skit_dist, truncation = 3) #funkar inte!
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
check.mono(hn.df.weather.cos$ddf,plot=TRUE,n.pts=100) #sannolikheten att se ripor stiger aldrig med ökat avstånd
gof_ds(hn.df.weather.cos) # sämre qq-plot och p = 0.386

hn.df.period.cos <- ds(rip_perp, truncation=300, formula = ~as.factor(period), adjustment = "cos", order = 2, monotonicity = FALSE)
plot(hn.df.period.cos) #minskar hela vägen med ökat avstånd
gof_ds(hn.df.period.cos) #dålig qq-plot och p = 0.396


hn.df.weather.period.cos <- ds(rip_perp, truncation=300, formula = ~as.factor(weather)+as.factor(period), adjustment = "cos", order = 2, monotonicity = FALSE)
plot(hn.df.weather.period.cos) #minskar med ökat avstånd hela vägen
gof_ds(hn.df.weather.period.cos) #sämre qq-plot, p = 0.384



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

plot(hn.df.cos)
covar.fit2 <- ddf.gof(hn.df.cos$ddf, main = "Half-normal detection function, no covariates") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(covar.fit2$dsgof$CvM$W,3), 
                 "\nP=", round(covar.fit2$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8)

# Abundance estimation finns i summary. Eftersom transekterna ligger under region label får jag abundance per lya direkt
summary(hn.df.cos)

#' Gör en tabel med kable från paketet knitr. 
#' Står beskrivet i Miller et al. Distance sampling in R. (har på PDF)
#' här förklaras uncertainty: http://converged.yt/RDistanceBook/distance-uncertainty.html#fn6
rip_table <- summary(hn.df.cos)$dht$individuals$N
rip_table$lcl <- rip_table$ucl <- rip_table$df <- NULL
colnames(rip_table) <- c("Den Nr", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)",
                         "$\\text{CV}(\\hat{N}$)")


?kable
kable(rip_table, format = "markdown")

#printar en excelfil

ripa_estimated <- summary(hn.df.cos)$dht$individuals$N

ripa_estimated<-ripa_estimated %>% 
  dplyr::select(Label, Estimate) %>% 
  slice(-11) #nedersta raden innehåller totala antalet ripor. Tar bort den

colnames(ripa_estimated) <- c("lya", "uppskattat_antal_ripor")

ripa_estimated
write_xlsx(ripa_estimated, path = "Rawdata/Uppskattat antal ripor vår distance_sampling.xlsx")
