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

data = as.data.table( fread( "/Users/maxime/Documents/Université/HEC/PhD/6.1/FE I/HW4/JS_data.csv " ) )

df = data %>%
  filter( year >= 1971 & year <= 1992, # include 1971 because of growth
          state != "Delaware",
          year != ma ) %>%
  group_by( state ) %>%
  mutate( realGrowthIncome = ( pi_percap / lag( pi_percap ) ) / ( cpi / lag( cpi ) ),
          Deregulation = ifelse( ma <=1992, 1, 0 ) ) %>%
  na.omit() %>%
  ungroup()

m1 = feols( realGrowthIncome ~ d | year + state,
          data = df,
          vcov = cluster ~ state + year )
etable( m1 )

