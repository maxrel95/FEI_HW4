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
library(bacondecomp)


############### data cleaning ###############
dataTable = as.data.table( fread( "/Users/maxime/Documents/Université/HEC/PhD/6.1/FE I/HW4/JS_data.csv" ) )

df = dataTable %>%
  group_by( state ) %>%
  mutate( realIncome = pi_percap / cpi, 
          realGrowthIncome = 100 * realIncome / lag( realIncome ) ) %>%
  na.omit() %>%
  ungroup()

############### regressions ###############
r1ClusteredExclude = feols( realGrowthIncome ~ d | state + year,
          data = df %>% filter(year >= 1972 & year <= 1992,
                                                state != "Delaware",
                                                year != ma),
          vcov = cluster ~ state + year )

r1RobustExclude = feols( realGrowthIncome ~ d | state + year,
            data = df %>% filter(year >= 1972 & year <= 1992,
                                 state != "Delaware",
                                 year != ma),
            vcov = "hetero" )

r1ClusteredInclude = feols( realGrowthIncome ~ d | state + year,
                     data = df %>% filter(year >= 1972 & year <= 1992,
                                          state != "Delaware"),
                     vcov = cluster ~ state + year )

r1RobustInclude = feols( realGrowthIncome ~ d | state + year,
                  data = df %>% filter(year >= 1972 & year <= 1992,
                                       state != "Delaware"),
                  vcov = "hetero" )

etable( r1ClusteredExclude, r1RobustExclude, r1ClusteredInclude, r1RobustInclude )

r1ClusteredExcludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                            data = df %>% filter(year >= 1972 & year <= 1992,
                                                 state != "Delaware",
                                                 year != ma,
                                                 state != "Alaska",
                                                 state != "Hawaii"),
                            vcov = cluster ~ state + year )

r1RobustExcludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1992,
                                                         state != "Delaware",
                                                         year != ma,
                                                         state != "Alaska",
                                                         state != "Hawaii"),
                                    vcov = "hetero" )

r1ClusteredIncludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1992,
                                                         state != "Delaware",
                                                         state != "Alaska",
                                                         state != "Hawaii"),
                                    vcov = cluster ~ state + year )

r1RobustIncludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                 data = df %>% filter(year >= 1972 & year <= 1992,
                                                      state != "Delaware",
                                                      state != "Alaska",
                                                      state != "Hawaii"),
                                 vcov = "hetero" )

etable( r1ClusteredExcludeRegional, r1RobustExcludeRegional,  r1ClusteredIncludeRegional, r1RobustIncludeRegional )

r1ClusteredExclude99 = feols( realGrowthIncome ~ d | state + year,
                            data = df %>% filter(year >= 1972 & year <= 1999,
                                                 state != "Delaware",
                                                 year != ma),
                            vcov = cluster ~ state + year )

r1ClusteredInclude99 = feols( realGrowthIncome ~ d | state + year,
                            data = df %>% filter(year >= 1972 & year <= 1999,
                                                 state != "Delaware"),
                            vcov = cluster ~ state + year )

r1ClusteredExcludeRegional99 = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1999,
                                                         state != "Delaware",
                                                         year != ma,
                                                         state != "Alaska",
                                                         state != "Hawaii"),
                                    vcov = cluster ~ state + year )

r1ClusteredIncludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1999,
                                                         state != "Delaware",
                                                         state != "Alaska",
                                                         state != "Hawaii"),
                                    vcov = cluster ~ state + year )

etable( r1ClusteredExclude99, r1ClusteredInclude99,  r1ClusteredExcludeRegional99, r1ClusteredIncludeRegional )


############### event study ###############
df_event = df %>%
  filter( year >= 1972 & year <= 1992,
          state != "Delaware") %>%
  mutate( firstTreatment = ifelse( ma < 1972 ,0, ma ),
          stateId = as.numeric(as.factor( state )))

attgt = att_gt(
  yname = "realGrowthIncome",
  tname = "year",
  idname = "stateId",
  gname = "firstTreatment",
  data = df_event,
  cluster = "state",
  control_group = "notyettreated",
  allow_unbalanced_panel = TRUE
)


es = aggte(attgt,
  type = "dynamic", # For event study
  min_e = -20, max_e = +20,
  bstrap = TRUE,
  clustervars = "state",
  na.rm = TRUE
)

ggdid(es)


mod_twfe = feols(realGrowthIncome ~ i(firstTreatment, d, ref = -1) + ## Our key interaction: time × treatment status
                   pcinc + asmrh + cases |                    ## Other controls
                   stfips + year,                             ## FEs
                 cluster = ~stfips,                          ## Clustered SEs
                 data = dat)

############### bacon ###############
df_bacon = bacon(
  realGrowthIncome ~ d,
  data = 
  
)