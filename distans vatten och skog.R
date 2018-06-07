library("sp")
library("rgdal")
library("rgeos")


lyor  <- readOGR(dsn = file.choose(), layer = "lyor")
summary(lyor) #den är projected redan!####
plot(lyor, col ="red", pch = ".", cex = 3)


vegfjall <- readOGR(dsn = file.choose(), layer = "vegfjall") 
summary(vegfjall) #också projected och  har samma proj4string (koordinatsystem/CRS) som lyor####
View(vegfjall)
#jag är bara intresserad av skogen, inte av myrar och annat. Därför måste jag plocka ut skogsfeatures####

skog <- subset(vegfjall, VEGETATION == "Barrskog, lavristyp" | VEGETATION == "Barrskog, lavtyp" | VEGETATION == "Fuktig-våt barrskog" | VEGETATION == "Lavmarksbarrskog" | VEGETATION == "Lavmarkslövskog" | VEGETATION == "Mossmarksbarrskog" | VEGETATION == "Mossmarkslövskog" | VEGETATION == "Sumplövskog" | VEGETATION == "Torr-frisk barrskog")
summary(skog)

alla_vatten <- readOGR(dsn = file.choose(), layer = "vatten_alla", stringsAsFactors = FALSE)
summary(alla_vatten)



plot(skog, col = "green")
plot(vattenNV, col = "royalblue1", lwd = 2) #det går inte plotta storavattenNV som lines eftersom det är sjöar med. R vill inte rita upp dem som linjer#
lines(mindrevattenNV,"royalblue1", lwd = 1 )
points(lyor, col = "red", pch =".", cex = 3)


