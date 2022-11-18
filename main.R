library(dplyr)
library(tibble)
library(did)
library(conleyreg)
library(gridExtra)
library(ggplot2)
library(fixest)
library(estimatr)
library(data.table) 
library(readxl)

data = fread("/Users/maxime/Documents/Université/HEC/PhD/6.1/FE I/HW4/JS_data.csv")
data = as.data.table( data )
df = data %>%
  filter(year >= 1972,
         year <= 1992,
         state != "Delaware",
         year != ma )

