
#städar lydata vår och sommar
library(tidyverse)
library(readxl)
library(writexl)


vår <- read_xlsx(path = "Den and territory selection/Rawdata/tor.lydata.xlsx")
sommar <- read_xlsx(path = "Den and territory selection/Rawdata/tor.lydata.sommar.xlsx")

ripor.rel <- read_xlsx(path = "Den and territory selection/Rawdata/Uppskattat antal ripor vår distance_sampling.xlsx")
skit.rel <- read_xlsx(path = "Den and territory selection/Data/Uppskattat antal ripspillningshögar Helags sommar 2018 distance_sampling.xlsx")

vår<- as.data.frame(vår)
sommar<- as.data.frame(sommar)
ripor.rel<- as.data.frame(ripor.rel)
skit.rel<- as.data.frame(skit.rel)

head(ripor.rel)
head(skit.rel)
#byter namn från lya till Namn
colnames(ripor.rel)[1] <- "Namn"
colnames(skit.rel)[1] <- "Namn"

#lägger till fs framför zz på lynamnen
ripor.rel$Namn<- paste0('fs',ripor.rel$Namn)
skit.rel$Namn<- paste0('fs',skit.rel$Namn)

#ändrar till stora bokstäver
ripor.rel <- ripor.rel %>% 
  mutate(Namn = toupper(Namn))

skit.rel <- skit.rel %>% 
  mutate(Namn = toupper(Namn))
head(ripor.rel)
head(skit.rel)

head(vår)
head(sommar)
class(vår$Namn)

names(vår)
names(sommar)
# jag vill ha vårvariablerna "Namn" "snöfri area (m^2)", "snödjup","marktemperatur orange", "marktemperatur svart", "antal lyöppningar"

#' jag vill ha sommarvariablerna "area", "marktemperatur orange", "marktemperatur svart","riktning (grader)",
#' "vinkel", "antal lyöppningar"
str(vår)
#lite strulig kod men kom inte på ett bättre sätt att ta medelvärdet av två kolumner
vårvars <- vår %>%
  dplyr::select(Namn, lufttemp, `snöfri area (m^2)`, snödjup, `marktemperatur orange`, 
                `marktemperatur svart`, `antal lyöppningar`) %>%
  group_by(Namn) %>% 
  mutate(tempO = mean(`marktemperatur orange`)) %>% 
  mutate(tempS = mean(`marktemperatur svart`)) %>% 
  mutate(m.temp.v = (tempO + tempS)/2) %>% 
  mutate(m.snödjup = mean(snödjup)) %>%
  dplyr::select(-snödjup, -`marktemperatur orange`, - `marktemperatur svart`,-tempO, -tempS) %>%
  dplyr::rename(öppning.v = `antal lyöppningar`) %>%
  dplyr::rename(snöfri.area = `snöfri area (m^2)`) %>% 
  distinct()

View(vårvars)
vårvars <- as.data.frame(vårvars) #den blev sparad i alla möjlig format efter manipulation med dplyr

# tar bort rader som inte ska vara med. Skräp från när jag knappade in datat.
vårvars <- vårvars %>% 
  slice(-c(3,8))

# ändrar till stora bokstäver i Namn
vårvars <- vårvars %>% 
  mutate(Namn = toupper(Namn))

#ändrar två lyor som har a i slutet av namnet
vårvars$Namn[vårvars$Namn == "FSZZ104A"] <- "FSZZ104"
vårvars$Namn[vårvars$Namn == "FSZZ062A"] <- "FSZZ062"

# saknas lufttemp på 104 och 42. Lägger in medeltemperaturen för alla lyor på dem istället.
class(vårvars$lufttemp)
vårvars$lufttemp <- as.numeric(vårvars$lufttemp)
vårvars$lufttemp[is.na(vårvars$lufttemp)] <- mean(vårvars$lufttemp, na.rm = TRUE)

View(sommar)

# saknas mätvärden för den svarta termometern på rad 1 till 30. Lägger in värdena från den orangea termometern istället
sommar$`marktemperatur svart`[1:30] <- sommar$`marktemperatur orange`[1:30]
sommar$`marktemperatur svart`

somvars <- sommar %>%
  dplyr::select(Namn, area, lufttemp, `marktemperatur orange`, `marktemperatur svart`,`riktning (grader)`,
              `vinkel`, `antal lyöppningar`) %>%
  group_by(Namn) %>% 
  mutate(tempO = mean(`marktemperatur orange`)) %>% 
  mutate(tempS = + mean(`marktemperatur svart`)) %>% 
  mutate(m.temp.s = (tempO + tempS)/2) %>% 
  dplyr::select(-`marktemperatur orange`, - `marktemperatur svart`,-tempO, -tempS) %>%
  dplyr::rename(öppning.s = `antal lyöppningar`) %>% 
  dplyr::rename(riktning = `riktning (grader)`) %>% 
  distinct()

View(somvars)
somvars<- as.data.frame(somvars) #den blev sparad i alla möjlig format efter manipulation med dplyr
class(somvars)


# ändrar allt under 45 och över 315 till norr
namevector <- "riktning2"
somvars[ ,namevector] <- NA

somvars$riktning2[somvars$riktning <= 45] <- "N"
somvars$riktning2[somvars$riktning >= 315 &
                    somvars$riktning <= 360] <- "N" 
somvars$riktning2[somvars$riktning >= 45 &
                   somvars$riktning <= 135] <- "O"
somvars$riktning2[somvars$riktning >= 135 &
                    somvars$riktning <= 225] <- "S"
somvars$riktning2[somvars$riktning >= 225 &
                    somvars$riktning <= 315] <- "V"

# ändrar till stora bokstäver i Namn
somvars <- somvars %>% 
  mutate(Namn = toupper(Namn))

# byter namn på lufttemp så att de blir unika för säsong
vårvars <- vårvars %>% 
  dplyr::rename(lufttemp.v = lufttemp)

View(vårvars)

somvars <- somvars %>% 
  dplyr::rename(lufttemp.s = lufttemp)

# tar bort riktning med grader och byter namn på factorriktningskolumnen till riktnin
somvars <- somvars %>%   
dplyr::select(-riktning) %>% 
  dplyr::rename(riktning = riktning2)
View(somvars)

#smackar ihop allt

lyor.vars <- somvars %>% 
  left_join(skit.rel, by = "Namn") %>% 
  left_join(vårvars, by = "Namn") %>%
  left_join(ripor.rel, by = "Namn") %>%
  dplyr::select(-standard_error)

View(lyor.vars)
#FSZZ99 är en bonuslya som ligger inom riptriangeln för FSZZ33. Lägger in samma ripor och skitar på den. Använder den nog inte
lyor.vars$uppskattat_antal_ripspillningshögar[is.na(lyor.vars$uppskattat_antal_ripspillningshögar)] <- lyor.vars$uppskattat_antal_ripspillningshögar[4]
lyor.vars$uppskattat_antal_ripor[is.na(lyor.vars$uppskattat_antal_ripor)] <- lyor.vars$uppskattat_antal_ripor[4]

kull.tot<-read_xlsx(path= "Den and territory selection/Rawdata/antal kullar per lya 2000_2018.xlsx")

lyor.vars<-lyor.vars %>% 
  left_join(kull.tot, by = "Namn")

lyor.vars %>% 
  dplyr::select(Namn, kullar_totalt)


write_xlsx(lyor.vars, path = "Den and territory selection/Data/lyvariabler.aic.xlsx")

# gör en lång fil för alla år

kärnlyor<-read_xlsx(path = "kärnlyor Helags AIC 2000 - 2018.xlsx")
kärnlyor <- as.data.frame(kärnlyor)

head(kärnlyor)
kärnlyor.sub <- kärnlyor %>% 
  dplyr::select(obsID, Namn,kull, År, Fas)

head(kärnlyor.sub)

head(lyor.vars)
lydata.long <- kärnlyor.sub %>%
  left_join(lyor.vars, by = "Namn")


lydata.long <- lydata.long[complete.cases(lydata.long), ] # tar bort alla andra lyor. De raderna har NA och försvinner med det här kommandot

View(lydata.long)
#printar fil
write_xlsx(lydata.long, path = "Den and territory selection/Data/lyvariabler.lång.aic.xlsx")
