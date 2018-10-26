# AIC -analys matade lyor

library(readxl)
library(writexl)
library(tidyverse)
library(MuMIn)

fedDenYear<- read_xlsx(path= "Lyor, kullar, gps-punkter, yta och avstånd/fedDenYears.xlsx")

View(fedDenYear)
