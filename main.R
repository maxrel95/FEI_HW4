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

dataTable = as.data.table( fread( "/Users/maxime/Documents/Université/HEC/PhD/6.1/FE I/HW4/JS_data.csv" ) )

df = dataTable %>%
  filter( year >= 1971 & year <= 1992,
          state != "Delaware" ) %>%
  group_by( state ) %>%
  mutate( realIncome = pi_percap / cpi, 
          realGrowthIncome = 100 * realIncome / lag( realIncome )  ) %>%
  na.omit() %>%
  ungroup()

dfNoDeregulatedYear = df %>%
  filter( year != ma )

m1ClusteredExclude = feols( realGrowthIncome ~ d | state + year,
          data = dfNoDeregulatedYear,
          vcov = cluster ~ state + year )

m1RobustExclude = feols( realGrowthIncome ~ d | state + year,
            data = dfNoDeregulatedYear,
            vcov = "hetero" )

m1ClusteredInclude = feols( realGrowthIncome ~ d | state + year,
                     data = df,
                     vcov = cluster ~ state + year )

m1RobustInclude = feols( realGrowthIncome ~ d | state + year,
                  data = df,
                  vcov = "hetero" )

etable( m1ClusteredExclude, m1RobustExclude, m1ClusteredInclude, m1RobustInclude )



