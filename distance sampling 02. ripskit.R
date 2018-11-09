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
rip.orginal<-read_xlsx("Den and territory selection/Rawdata/tor.riptransekter helags sommar 2018 modifierade RS_avstånd för dubbla högar.xlsx") 
View(rip.orginal)
rip.orginal<-as.data.frame(rip.orginal) # DEN VAR BÅDE DATAFRAME, TBL och TBL_DF på samma gång! Därför funkade det inte att lägga på en detection function. Det måste vara en ren data frame
#Plockar ut ripskit
ripskit <- "RS"

#' 
#' Kör bara höjd som covariate
#' Det var bara tre rader med dubbla högar så ingen mening att köra högar
#' som size. Jag behöver inte vinkel eftersom jag bara tog 90 och 270 grader
#' på ripskiten. Distansen jag har är alltså redan perpendicular-distansen.
skit_dist <- rip.orginal %>%
  filter(observation %in% ripskit) %>% 
  dplyr::select(lya, observation, distans, Höjd)
  

View(skit_dist)

length(unique(skit_dist$lya)) # alla lyor är med. Ingen saknar observationer.

par(mfrow=c(1,1))
hist(skit_dist$distans, xlab = "perpendicular distance from transect (m)", main = "Observed ptarmigan dropping piles")

#byter namn på kolumnerna så att Distance känner igen dem
names(skit_dist)
colnames(skit_dist) <- c("Sample.Label", "observation", "distance", "elevation")
class(skit_dist$elevation)#numeric
head(skit_dist)

#plockar bort observation
skit_dist <-skit_dist %>% 
  dplyr::select(-observation)

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
                          elevation <= (1100)] <- "<1100"
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

class(skit_dist) 
skit_dist <- as.data.frame(skit_dist) #ändrar om till data frame

max(skit_dist$distance) 
#'observationen längst från linjen var 3,42 meter. Sätter truncation till 3 m
#'så länge. Kommer nog få justera ned det senare.

length(skit_dist$distance) # 214 skitar. Alltså mycket data.
hist(skit_dist$distance, breaks = 100, xlim = c(0, 1.5)) #en "bump" i datat vid 0,3 - 0,4. Inte bra. Det blir svårt att sätta en detection function.

#funkar inte att plotta. Då kan man pröva:
par("mar")
par(mar=c(1,1,1,1))

View(skit_dist)
par(mfrow=c(1,2))
#sätter en half-normal detection funktion först.

rs.hn.df <- ds(skit_dist, truncation = 1.5, adjustment = NULL) # Nu funkar det!!! Var en bugg innan.
summary(rs.hn.df)
gof_ds(rs.hn.df)
plot(rs.hn.df, main = "Half-normal, no adjustments")

## ÖKA model fit med adjustments ##

rs.hn.df.cos <- ds(skit_dist, truncation = 1.5) #cosine adjustment.
summary(rs.hn.df.cos)
gof_ds(rs.hn.df.cos)
plot(rs.hn.df.cos, main = "Half-normal, cosine adjustment")


# Half-normal med Hermite polynomial adjustment
rs.hn.df.hermite <- ds(skit_dist, truncation = 1.5, adjustment = "herm") 
summary(rs.hn.df.hermite)
gof_ds(rs.hn.df.hermite)
plot(rs.hn.df.hermite, main = "Half-normal, Hermite polynomial adjustment")


# Hazard-rate detection function
rs.hr.df <- ds(skit_dist, truncation=1.5, key= "hr", adjustment = NULL) 
summary(rs.hr.df)
gof_ds(rs.hr.df)
plot(rs.hr.df, main = "Hazard rate, no adjustments")


# Hazard-rate detection function med cosine adjustment
rs.hr.df.cos <- ds(skit_dist, truncation=1.5, key= "hr") 
summary(rs.hr.df.cos)
gof_ds(rs.hr.df.cos)
plot(rs.hr.df.cos, main = "Hazard rate, cosine adjustment")


# Hazard-rate detection function med simple polynomial adjustment
rs.hr.df.poly <- ds(skit_dist, truncation = 1.5, key = "hr", adjustment = "poly")
summary(rs.hr.df.poly)
gof_ds(rs.hr.df.poly)
plot(rs.hr.df.poly, main = "Hazard rate with simple polynomial adjustment")

#' Börjar med en ny df fourier/uniform med cosine adjustment. Den måste ha cosine annars funkar den inte. 
#' Behöver inte specificera order
rs.fourier.df.cos <- ds(skit_dist, truncation=1.5, key = "unif") # cos är default
summary(rs.fourier.df.cos)
gof_ds(rs.fourier.df.cos, chisq = TRUE) 
plot(rs.fourier.df.cos, main = "Uniform")


"Uniform med polynomial också"
rs.fourier.df.poly <- ds(skit_dist, truncation=1.5, key = "unif", adjustment = "poly") #
summary(rs.fourier.df.poly)
gof_ds(rs.fourier.df.poly, chisq = TRUE) #mindre standard error
plot(rs.fourier.df.poly, main = "Uniform med simple polynomial")

#Lägger på höjd över havet som covariate

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
plot(rs.hr.df.elev.cos, main = "Hazard rate with elevation covariate and order 2 cosine adjustment")
check.mono(rs.hr.df.elev.cos$ddf,n.pts=100, plot = TRUE) #den är ganska ickemonoton under en lång sträcka

# testar med cosinus order 3
rs.hr.df.elev.cos3 <- ds(skit_dist, truncation = 1.5, adjustment = "cos", key ="hr", order = 3, monotonicity = FALSE, formula = ~as.factor(elevation.bin))
summary(rs.hr.df.elev.cos3)
gof_ds(rs.hr.df.elev.cos3, chisq = TRUE)
plot(rs.hr.df.elev.cos3)


check.mono(hn.df.weather.cos$ddf,n.pts=100, plot=TRUE)#den är monoton hela vägen! Bra!

# Mixture model detection function package MMDS

#Eftersom det inte är någon mixture angedd i den här blir det bara en half normal.
rs.df.mix <- fitmix(skit_dist, width = 1.5) #funkar!
step.ds.mixture(rs.df.mix)# den här tar fram den bästa mixturen. Den säger att tre-point är bäst.
summary(rs.df.mix)
gof_ds(rs.df.mix)
plot(rs.df.mix, main = "1 point half normal mixture model, the same as half-normal", breaks = 100)


#2 point mixture model of half normals - En blandning av två half normals.
rs.df.mix2 <- fitmix(skit_dist, width = 1.5, mix.terms = 2) #funkar!
summary(rs.df.mix2)
gof_ds(rs.df.mix2)
plot(rs.df.mix2, main = "2 point mixture model of half normals", breaks = 100)
?plot.ds.mixture
#3 point mixture. Borde vara bäst
rs.df.mix3 <- fitmix(skit_dist, width = 1.5, mix.terms = 3) #funkar!
summary(rs.df.mix3)
gof_ds(rs.df.mix3)
plot(rs.df.mix3, main = "3 point mixture model of half normals", breaks = 100) #blir inget

#Mix med elevation bin som covariate
rs.df.mix2.elev <- fitmix(skit_dist, width = 1.5, mix.terms = 2, model.formula = "~elevation.bin") #hoppas jag har gjort rätt med model.formula. Man kan se nivåerna i elevation.bin med rs.df.mix2.elev$z
summary(rs.df.mix2.elev)
gof_ds(rs.df.mix2.elev)
step.ds.mixture(rs.df.mix2.elev)
plot(rs.df.mix2.elev, main = "2 point mixture model of half normals with elevation as a factor covariate") #vill inte plotta

rs.df.mix$aic
rs.df.mix2$aic
rs.df.mix3$aic
rs.df.mix2.elev$aic #-43.69423 lägst

rs.df.mix2.elev$cvm # Cramer von Mises p-value = 0.05670862, W = 0.4401926
rs.df.mix2.elev$N #antal totalt???

#' Jämför AIC-scores för alla modellerna. Om skillnaden är mindre än 2 är det bäst att ta den enklare
#' modellen.
summarize_ds_models(rs.fourier.df.cos,
                    rs.fourier.df.poly, rs.hn.df, rs.hn.df.cos, rs.hn.df.elev,
                    rs.hn.df.hermite, rs.hr.df, rs.hr.df.cos,
                    rs.hr.df.elev, rs.hr.df.elev.cos, rs.hr.df.elev.cos3,
                    rs.hr.df.poly)

# De här tre var bäst. Kollar deras plottar lite nogrannare.
AIC(rs.hr.df.poly)#-17.71738 Den här har bäst qq-plot och högst p-värde och ger bäst vikt åt obsar nära linjen. Det blir den.
AIC(rs.hn.df.cos)#-16.96951
AIC(rs.hr.df.cos)# -16.06829 


rs.df.mix2.elev$aic #-43.69423 lägst
#mixture models fungerar inte i summarize_ds_models
summarize_ds_models(rs.df.mix, rs.df.mix2, rs.df.mix3, rs.df.mix2.elev)

#'Det verkar ändå inte gå att få abundance per Region.Label med mixture models
#så jag kör med rs.hr.df.poly

#' elevation med cosinus (order 3) är bäst men den innehåller både adjustment och covariate vilket kan
#' vara dåligt eftersom detection probability kan bli högre än 1 i vissa fall eftersom monotonicity
#' är avslaget. Får inte funktionen check.mono att fungera. Paketet med mixture models (mmds) vill inte 
#' fungera heller och det är gammalt och hittar inga trådar om paketet online.


plot(rs.hn.df.cos, main = "Half normal with cosine adjustment")
hn.cos.fit <- ddf.gof(rs.hn.df.cos$ddf, main = "Half normal with cosine adjustment") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(hn.cos.fit$dsgof$CvM$W,3), 
                 "\nP=", round(hn.cos.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8) #första delen anger positionen i grafen där texten "message" ska klistras in

plot(rs.hr.df.poly, main = "Ptarmigan droppings summer, Hazard rate detection function, simple polynomial adjustment")
hr.poly.fit <- ddf.gof(rs.hr.df.poly$ddf, main = "QQ-plot") #plottar qq-plot. Syns tydligt att den blev sämre
message <- paste("Cramer von-Mises W=", round(hr.poly.fit$dsgof$CvM$W,3), 
                 "\nP=", round(hr.poly.fit$dsgof$CvM$p,3))
text(0.6, 0.1, message, cex=0.8)

#' Abundance estimation finns i summary. 
#' Eftersom transekterna ligger under region label får jag abundance per lya direkt
#' 
summary(rs.hr.df.poly)

#' Gör en tabel med kable från paketet knitr. 
#' Står beskrivet i Miller et al. Distance sampling in R. (har på PDF)
#' här förklaras uncertainty: http://converged.yt/RDistanceBook/distance-uncertainty.html#fn6
skit_table <- summary(rs.hr.df.poly)$dht$individuals$N
skit_table$lcl <- skit_table$ucl <- skit_table$df <- NULL
colnames(skit_table) <- c("Den code", "$\\hat{N}$", "$\\text{se}(\\hat{N}$)",
                         "$\\text{CV}(\\hat{N}$)")

skit_table$`Den code` <- paste0('fs', skit_table$`Den code`)
skit_table<- skit_table%>% 
  mutate(`Den code` = toupper(`Den code`)) %>% 
  mutate_if(is.numeric, round, 2) #avrundar till två värdesiffror

skit_table$`Den code`[11] <- "Total"
skit_table
#printar en fil för markdowntabell
write_xlsx(skit_table, path = "Den and territory selection/plottar/skittabell.estimat.stats.xlsx")
?kable

?kable
kable(skit_table, format = "markdown")

#printar en excelfil

ripskit_estimated <- summary(rs.hr.df.poly)$dht$individuals$N
ripskit_estimated

ripskit_estimated<-ripskit_estimated %>% 
  select(Label, Estimate) %>% 
  slice(-11) #nedersta raden innehåller totala antalet ripor. Tar bort den

colnames(ripskit_estimated) <- c("lya", "uppskattat_antal_ripspillningshögar")

ripskit_estimated
write_xlsx(ripskit_estimated, path = "Data/Uppskattat antal ripspillningshögar Helags sommar 2018 distance_sampling.xlsx")
