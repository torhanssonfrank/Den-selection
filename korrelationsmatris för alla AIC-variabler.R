
## Gör en korrelationsmatris för att se om vissa variabler är korrelerade, det viss säga mäter samma effekt ####
#' jag misstänker tillexempel att rödrävsdensitet är korrelerad med avstånd till trädgräns
library(readxl)
variabler <- read_xlsx(path = "kärnlyor Helags AIC.xlsx")
str(variabler)
variabler$hojd_over_havet <- as.numeric(variabler$hojd_over_havet)
x <- variabler %>%
  dplyr::select(rödräv_densitet, distans_till_skog, avs_kull, medelvärde_lämmelprediktion_uppgångsår, 
                lemmel_var, hojd_over_havet, area_myr, area_vatten, distans_till_vatten)
head(x)
colnames(x) <- c("red fox density", "distance to forest", "distance to reproduction", "mean lemming density", 
                 "lemming variance", "altitude", "area bogs", "area water", "distance to water")
res<-cor(x, method = c("pearson", "kendall", "spearman"))
round(res, 2)
library("Hmisc") # ger p-värden för korrelationsmatris
res2 <- rcorr(as.matrix(x))
res2
res2$r
res2$P

symnum(res, abbr.colnames = FALSE)
library("corrplot")


# Insignificant correlations display p-value (insig = "p-value")
corrplot(res2$r, method = "circle", type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "p-value" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20)
mtext("Insignificant p-values are shown (p > 0.05)", side=3)

# testar utan distans till vatten och area vatten


y<- x %>% 
  dplyr::select(-"area water", -"distance to water")

res3 <- rcorr(as.matrix(y))
corrplot(res3$r, method = "circle", type="upper", order="hclust", 
         p.mat = res3$P, sig.level = 0.05, insig = "p-value" , tl.cex = 0.7,
         title = "Variable Correlation matrix, Helags", 
         mar = c(1,0,3,0), tl.col = "black",
         tl.srt = 20) # alla kvarvarande variabler är signifikant olika
