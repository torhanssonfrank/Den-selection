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
xsc <- x
xsc[pvars] <- lapply(xsc[pvars],scale)
res<-cor(xsc, method = c("pearson", "kendall", "spearman"))
round(res, 2)
library("Hmisc") # ger p-värden för korrelationsmatris
res2 <- rcorr(as.matrix(xsc))
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
y <- xsc %>%
  dplyr::select(-altitude, -`lemming variance`, -`distance to reproduction`)
res3 <- rcorr(as.matrix(y))
corrplot(res3$r, method = "number", type="upper", order="hclust", 
         p.mat = res3$P, sig.level = 0.05, insig = "pch" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant correlations are crossed (p > 0.05)", side=3)

vif(y)
tabley<-vif(y) # nu har mean lemming density låg VIF 

?kable
class(variabler$Namn)
variabler$Namn <- as.factor(variabler$Namn)
plot(variabler$kull, variabler$Namn)
namnkorr<-glmer(kull~ (1 | Namn), data = variabler, family = binomial)

residout<-simulateResiduals(namnkorr)
plot(residout, quantreg = FALSE, asFactor = TRUE) # qq-ploten tyder på att det är ett linjärt samband. Det räcker för att anta att Namn inte är oberoende.

