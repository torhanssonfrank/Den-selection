library(readxl)
library(tidyverse)
library(lubridate)
relevant_events<-read_xlsx(file.choose())
dendate<-subset(relevant_events, select = c(DenCode, EventDate))
dendate
yeareleven<-filter(dendate, EventDate >= ymd("2011-01-01")) #funktionen ymd från paketet Lubridate gör att R fattar vad datum är
yeareleven
unique(yeareleven$DenCode) #endast fyra lyor är registrerade med datum i 
# Alvas fil (RelevantEvents). I Rasmus fil (komplexa kullar Helags - genetiskt och observationer)
# är det 22 lyor med kullar i Helags då.

