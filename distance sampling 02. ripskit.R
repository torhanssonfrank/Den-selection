#Distance sampling för ripbajs, sommar.
install.packages("mmds") # mixture models distance sampling
??mmds

library(mmds)
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
#' obsar under datainsamlingen) och lagt till 10 cm på den ena.Tagit bort "m" ur meterkolumnen
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
  select(lya, observation, distans, Höjd)
  

View(skit_dist)

length(unique(rip_dist$lya)) # alla lyor är med. Ingen saknar observationer.


hist(skit_dist$distans, xlab = "perpendicular distance from transect (m)", main = "Observed ptarmigan dropping piles")

#byter namn på kolumnerna så att Distance känner igen dem
names(skit_dist)
colnames(skit_dist) <- c("Sample.Label", "observation", "distance", "elevation")
class(skit_dist$elevation)#numeric
head(skit_dist)

#plockar bort observation
skit_dist <-skit_dist %>% 
  select(-observation)

#' Gör en covariate med fyra olika höjdklasser som proxy för 
#' växtlighet. Den rikare växtligheten kan ha gjort det 
#' svårare för mig att upptäcka ripskitar på längre avstånd
min(skit_dist$elevation)
max(skit_dist$elevation)
attach(skit_dist)
skit_dist$elevation.bin[elevation <= (900)] <- "<900"
skit_dist$elevation.bin[elevation > (900) &
                  elevation <= (1000)] <- "<1000"
skit_dist$elevation.bin[elevation > (1000) &
                          elevation <= (1100)] <- "<1000"
skit_dist$elevation.bin[elevation > (1100)] <- "<1169"
detach(skit_dist)
View(skit_dist)
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
                        Sample.Label = c("zz014", "zz096", "zz042", "zz104", "zz020", "zz062", "zz033", "zz076", "zz075", "zz061"), stringsAsFactors = FALSE)
class(transects2$Effort)#numeric
class(transects2$Sample.Label)#character


skit_dist <- skit_dist %>% 
  left_join(transects2, by = "Sample.Label")


View(skit_dist)

class(skit_dist$distance)
class(skit_dist$Area)
class(skit_dist$Effort)
class(skit_dist$elevation)

class(skit_dist) # DEN VAR BÅDE DATAFRAME, TBL och TBL_DF på samma gång! Därför funkade det inte att lägga på en detection function. Det måste vara en ren data frame
skit_dist <- as.data.frame(skit_dist) #ändrar om till data frame

max(skit_dist$distance) 
#'observationen längst från linjen var 3,42 meter. Sätter truncation till 3 m
#'så länge. Kommer nog få justera ned det senare.

length(skit_dist$distance) # 214 skitar. Alltså mycket data.
hist(skit_dist$distance, breaks = 100, xlim = c(0, 1.5)) #en "bump" i datat vid 0,3 - 0,4. Inte bra. Det blir svårt att sätta en detection function.


View(skit_dist)
par(mfrow=c(1,2))
#sätter en half-normal detection funktion först.

rs.hn.df <- ds(skit_dist, truncation = 1.5, adjustment = NULL) # funkar inte! Funkar inte att ändra method i optim heller.
# datat för dåligt!
summary(rs.hn.df)


## ÖKA model fit med adjustments ##

rs.hn.df.cos <- ds(skit_dist, truncation = 1.5) #cosine adjustment. funkar inte!


# Half-normal med Hermite polynomial adjustment
rs.hn.df.hermite <- ds(skit_dist, truncation = 1.5, adjustment = "herm") #funkar inte!


# Hazard-rate detection function
rs.hr.df <- ds(skit_dist, truncation=1.5, key= "hr", adjustment = NULL) #funkar inte


# Hazard-rate detection function med cosine adjustment
rs.hr.df.cos <- ds(skit_dist, truncation=1.5, key= "hr") #funkar inte



# Hazard-rate detection function med simple polynomial adjustment
rs.hr.df.poly <- ds(skit_dist, truncation = 1.5, key = "hr", adjustment = "poly")


#' Börjar med en ny df fourier/uniform med cosine adjustment. Den måste ha cosine annars funkar den inte. 
#' Behöver inte specificera order
rs.fourier.df.cos <- ds(skit_dist, truncation=1.5, key = "unif") # cos är default
summary(rs.fourier.df.cos, chisq = TRUE)
gof_ds(rs.fourier.df.cos) #mindre standard error
plot(rs.fourier.df.cos, main = "Uniform")


"Uniform med polynomial också"
rs.fourier.df.poly <- ds(skit_dist, truncation=1.5, key = "unif", adjustment = "poly") #
summary(rs.fourier.df.poly)
gof_ds(rs.fourier.df.poly, chisq = TRUE) #mindre standard error
plot(rs.fourier.df.poly, main = "Uniform med simple polynomial")

#testar Sample.Label som covariate med half-normal
rs.hn.df.transect <- ds(skit_dist, truncation = 1.5, adjustment = NULL, formula = ~as.factor(Sample.Label))
summary(rs.hn.df.transect) #ganska stort standard error
gof_ds(rs.hn.df.transect, chisq = TRUE)
plot(rs.hn.df.transect)


#testar Sample.Label som covariate med hazard rate. Är nog inte en så bra idé att använda Sample.Label som covariate. Finns ingen smart biologisk förklaring. Höjd över havet är bättre.
rs.hr.df.transect <- ds(skit_dist, truncation = 1.5, adjustment = NULL, key = "hr", formula = ~as.factor(Sample.Label))
summary(rs.hr.df.transect)
gof_ds(rs.hr.df.transect, chisq = TRUE) # p = 0.375. Stor standard error. Dåligt.
plot(rs.hr.df.transect)

#Går inte att köra fourier/uniform med covariates så hoppar den

#testar elevation.bin som covariate med half normal
rs.hn.df.elev <- ds(skit_dist, truncation = 1.5, adjustment = NULL, formula = ~as.factor(elevation.bin))
summary(rs.hn.df.elev)
gof_ds(rs.hn.df.elev, chisq = TRUE) # ok standard error men lågt p värde
plot(rs.hn.df.elev)

#testar elevation.bin som covariate med hazard rate
rs.hr.df.elev <- ds(skit_dist, truncation = 1.5, adjustment = NULL, key ="hr", formula = ~as.factor(elevation.bin))
summary(rs.hr.df.elev)
gof_ds(rs.hr.df.elev, chisq = TRUE) # högre standard error men högre p värde = 0.227. Bättre qq än half-normal med elevation.bin
plot(rs.hr.df.elev, main = "Hazard-rate with elevation covariate")


#testar med adjustment cosinus order 2
rs.hr.df.elev.cos <- ds(skit_dist, truncation = 1.5, adjustment = "cos", key ="hr", order = 2, monotonicity = FALSE, formula = ~as.factor(elevation.bin))
summary(rs.hr.df.elev.cos)
gof_ds(rs.hr.df.elev.cos, chisq = TRUE)
plot(rs.hr.df.elev.cos)

# testar med cosinus order 3
rs.hr.df.elev.cos3 <- ds(skit_dist, truncation = 1.5, adjustment = "cos", key ="hr", order = 3, monotonicity = FALSE, formula = ~as.factor(elevation.bin))
summary(rs.hr.df.elev.cos3)
gof_ds(rs.hr.df.elev.cos3, chisq = TRUE)
plot(rs.hr.df.elev.cos3)

check.mono(rs.hr.df.elev.cos3, plot = TRUE)
?check.mono

# Mixture model detection function package MMDS

rs.df.mix <- fitmix(skit_dist, width = 1.5 , model.formula = ~as.factor(elevation.bin)) #funkar inte. Paketet verkar inte fungera
summary(rs.df.mix)


?fitmix
#' Jämför AIC-scores för alla modellerna. Om skillnaden är mindre än 2 är det bäst att ta den enklare
#' modellen.
summarize_ds_models(rs.fourier.df.cos, rs.hn.df.transect, rs.hr.df.transect,rs.hn.df.elev, rs.hr.df.elev, rs.hr.df.elev.cos, rs.hr.df.elev.cos3, rs.fourier.df.poly)
#' elevation med cosinus (order 3) är bäst men den innehåller både adjustment och covariate vilket kan
#' vara dåligt eftersom detection probability kan bli högre än 1 i vissa fall eftersom monotonicity
#' är avslaget. Får inte funktionen check.mono att fungera. Paketet med mixture models (mmds) vill inte 
#' fungera heller och det är gammalt och hittar inga trådar om paketet online.
mrds::check.mono(rs.fourier.df.cos)

plot(rs.hr.df.elev.cos3)
elev.cos.fit <- ddf.gof(rs.hr.df.elev.cos3$ddf, main = "Hazard rate with elevation covariate and order 3 cosine adjustment") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(elev.cos.fit$dsgof$CvM$W,3), 
                 "\nP=", round(elev.cos.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8) #första delen anger positionen i grafen där texten "message" ska klistras in

plot(rs.fourier.df.cos)
unif.fit <- ddf.gof(rs.fourier.df.cos$ddf, main = "Uniform fit, no covariates") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(unif.fit$dsgof$CvM$W,3), 
                 "\nP=", round(unif.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8)

#' Abundance estimation finns i summary. Eftersom transekterna ligger under region label får jag abundance per lya direkt
#' Modellerna med elevation som covariate och cos adjustments är egentligen bättre men tror
#' inte att jag borde använda dem. Har inte fått till check uniform än.
summary(rs.fourier.df.cos)

#' Gör en tabel med kable från paketet knitr. 
#' Står beskrivet i Miller et al. Distance sampling in R. (har på PDF)
#' här förklaras uncertainty: http://converged.yt/RDistanceBook/distance-uncertainty.html#fn6
skit_table <- summary(rs.fourier.df.cos)$dht$individuals$N
skit_table$lcl <- skit_table$ucl <- skit_table$df <- NULL
colnames(skit_table) <- c("Dropping piles", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)",
                         "$\\text{CV}(\\hat{N}$)")


?kable
kable(rip_table, format = "markdown")

#printar en excelfil

ripskit_estimated <- summary(rs.fourier.df.cos)$dht$individuals$N
ripskit_estimated

ripskit_estimated<-ripskit_estimated %>% 
  select(Label, Estimate) %>% 
  slice(-11) #nedersta raden innehåller totala antalet ripor. Tar bort den

colnames(ripskit_estimated) <- c("lya", "uppskattat_antal_ripspillningshögar")

ripskit_estimated
write_xlsx(ripskit_estimated, path = "Data/Uppskattat antal ripspillningshögar Helags sommar 2018 distance_sampling.xlsx")
