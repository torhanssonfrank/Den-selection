install.packages("usdm")
## Gör en korrelationsmatris för att se om vissa variabler är korrelerade, det viss säga mäter samma effekt ####
#' jag misstänker tillexempel att rödrävsdensitet är korrelerad med avstånd till trädgräns
library(readxl)
variabler <- read_xlsx(path = "kärnlyor Helags AIC.xlsx")
str(variabler)
variabler$hojd_over_havet <- as.numeric(variabler$hojd_over_havet)
variabler <- as.data.frame(variabler)
x <- variabler %>%
  dplyr::select(rödräv_densitet, distans_till_skog, avs_kull, medelvärde_lämmelprediktion_uppgångsår, 
                lemmel_var, hojd_over_havet, area_myr, area_vatten, distans_till_vatten)
head(x)
colnames(x) <- c("red fox density", "distance to forest", "distance to reproduction", "mean lemming density", 
                 "lemming variance", "altitude", "area bogs", "area water", "distance to water")
pvars <- c("red fox density", "distance to forest", "distance to reproduction", "mean lemming density", 
           "lemming variance", "altitude", "area bogs", "area water", "distance to water")
#testar skalade värden
xsc <- x
xsc[pvars] <- lapply(xsc[pvars],scale)
res<-cor(xsc, method = c("pearson", "kendall", "spearman"))
round(res, 2)
library("Hmisc") # ger p-värden för korrelationsmatris
res2 <- rcorr(as.matrix(x))
res2
res2$r
res2$P

symnum(res, abbr.colnames = FALSE)
library("corrplot")


# Insignificant correlations display p-value (insig = "p-value")
par(mfrow=c(1,1))
corrplot(res2$r, method = "number", type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

# Scatterplots. Mycket är beroende av höjd över havet
scatterplot(xsc$`mean lemming density` , xsc$altitude)
scatterplot(x$`mean lemming density`, x$`lemming variance` )
scatterplot(x$`area bogs`, x$altitude, xlab = "area of bogs in a 1.5 km radius around dens", ylab = "altitude" )
scatterplot(x$`mean lemming density`, x$`area bogs`)
scatterplot(x$`distance to forest`, x$altitude )


library(usdm)
?vif
vif(x)
#' mean lemming density har VIF över 5 men enligt paketet är 10 eller över illa 
#' (Montgomery and Peck 1992 använder 10). Zuur et al. 2013 använder 3. Desto lägre desto bättre.
#' Med för hög VIF blir parametrar icke-signifikanta, speciellt om den ekologiska signalen är låg. Till och 
#' med en VIF på 2 kan ge för höga p-värden. Det är därför kanske bäst att plocka bort
#' höjd över havet eftersom så många andra parametrar korrelerar med den.

#testar utan höjd över havet, lämmelvarians och distans till reproduktion
y <- x %>%
  dplyr::select(-altitude, -`lemming variance`, -`distance to reproduction`)
res3 <- rcorr(as.matrix(y))
corrplot(res3$r, method = "number", type="upper", order="hclust", 
         p.mat = res3$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

library(usdm) # vif-värden för dataramar
vif(y)
tabley<-vif(y) # nu har mean lemming density låg VIF 

# printar en fil så att jag kan göra tabell i markdown
write_xlsx(tabley, path = "Den and territory selection/Plottar/tabell_VIF-värden_GISdata.xlsx")

class(variabler$Namn)
variabler$Namn <- as.factor(variabler$Namn)
plot(variabler$kull, variabler$Namn)
namnkorr<-glmer(kull~ (1 | Namn), data = variabler, family = binomial)

residout<-simulateResiduals(namnkorr)
plot(residout, quantreg = FALSE, asFactor = TRUE) # qq-ploten tyder på att det är ett linjärt samband. Det räcker för att anta att Namn inte är oberoende.



# korrelationsmatris för lydatan
lydata.long <- read_xlsx(path = "Den and territory selection/Data/lyvariabler.lång.aic.xlsx")

lydata.long<- as.data.frame(lydata.long)
str(lydata.long)
lydata.long$area <- as.numeric(lydata.long$area)
lydata.long$snöfri.area <- as.numeric(lydata.long$snöfri.area)

names(lydata.long)
View(lydata.long)
lydata.long <- lydata.long[!lydata.long$Namn == "FSZZ099", ] # tar bort 99:an. Kommer inte använda den.
View(lydata.long)

z <- lydata.long %>% 
  dplyr::select(area, vinkel,öppning.v, öppning.s, m.temp.s,m.temp.v,m.snödjup, snöfri.area, uppskattat_antal_ripspillningshögar,
                uppskattat_antal_ripor) %>% 
  dplyr::rename(ripor = uppskattat_antal_ripor) %>% 
  dplyr::rename(ripspillning = uppskattat_antal_ripspillningshögar)
  

library("Hmisc") # ger p-värden för korrelationsmatris
res3 <- rcorr(as.matrix(z))
res3
res3$r
res3$P


library("corrplot")


# Insignificant correlations display p-value (insig = "p-value")
par(mfrow=c(1,1))
corrplot(res3$r, method = "number", type="upper", order="hclust", 
         p.mat = res3$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

#' tar bort marktermperatur sommar (den var så väderberoende ändå), öppningar sommar (väldigt korrelerad mer area).
#' snöfri area  var korrelerad till 0.78 med ripspillning vilket antagligen bara var en slump.Kändes dock dumt att ta bort
#' den eftersom det är det bästa snömåttet. Tog bort snödjup istället och fick ner VIF-värdena under
#' tio i all fall. Tar även bort
#' marktemperatur vår. Det blir för många parametrar annars. Vinkel, riktning, lyöppningar och snöfri.area mäter typ samma sak. 
#' Överraskande nog är antalet sedda ripor på våren inte korrelerat med ripspillning på sommaren.

z2<- z %>% 
  dplyr::select(-m.temp.s, -öppning.s, -m.snödjup, -m.temp.v)
res4 <- rcorr(as.matrix(z2))
# Insignificant correlations display p-value (insig = "p-value")

corrplot(res4$r, method = "number", type="upper", order="hclust", 
         p.mat = res4$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

library(usdm)

# VIF 

class(z2)
vif(z2)
vifstep(z, th = 10) #vill inte ta bort area av lyan, som den här automatiserade varianten föreslår
?vif
 # jag behåller alltså vinkel, öppning.v, ripor, ripspillning, area, snöfri.area