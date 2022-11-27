library(dplyr)
library(did)
library(ggplot2)
library(fixest)
library(data.table) 
library(bacondecomp)
library(did2s)
library(ggthemes)

############### data cleaning ###############
dataTable = as.data.table( fread( "/Users/maxime/Documents/Université/HEC/PhD/6.1/FE I/HW4/JS_data.csv" ) ) # import data

df = dataTable %>% # compute the growth real income per capita
  group_by( state ) %>%
  mutate( realIncome = pi_percap / cpi, # compute the real income per capita by state
          realGrowthIncome = 100 * realIncome / lag( realIncome ) ) %>% # compute the growth rate
  na.omit() %>% # remove na in the dataset
  ungroup()

############### regressions ###############
r1ClusteredExclude = feols( realGrowthIncome ~ d | state + year, # regress real growth income on the treatment, FE unit and time 
          data = df %>% filter(year >= 1972 & year <= 1992, # keep only obs between 1972 and 1992
                                                state != "Delaware", # remove delaware state
                                                year != ma), # remove obs at deregulation date
          vcov = cluster ~ state + year ) # cluster se by state and year

r1RobustExclude = feols( realGrowthIncome ~ d | state + year,
            data = df %>% filter(year >= 1972 & year <= 1992,
                                 state != "Delaware",
                                 year != ma),
            vcov = "hetero" ) # robust se as in paper

r1ClusteredInclude = feols( realGrowthIncome ~ d | state + year,
                     data = df %>% filter(year >= 1972 & year <= 1992,
                                          state != "Delaware"), # keep obs at the deregulation date
                     vcov = cluster ~ state + year ) # cluster se by state and year

r1RobustInclude = feols( realGrowthIncome ~ d | state + year,
                  data = df %>% filter(year >= 1972 & year <= 1992,
                                       state != "Delaware"), # keeps obs at the deregulation date
                  vcov = "hetero" ) # robust s.e.

etable( r1ClusteredExclude, r1RobustExclude, r1ClusteredInclude, r1RobustInclude,
        file = "Results/eq1.tex" ) # exporte regression result table in a tex file 

r1ClusteredExcludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year, # add regional time
                                    #FE by interacting region and time
                            data = df %>% filter(year >= 1972 & year <= 1992,
                                                 state != "Delaware" & state != "Alaska" & state != "Hawaii", # remove  delaware alaska and hawai
                                                 year != ma), # remove obs at deregulation date
                            vcov = cluster ~ state + year ) # clustered s.e.

r1RobustExcludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1992,
                                                         state != "Delaware" & state != "Alaska" & state != "Hawaii",
                                                         year != ma),
                                    vcov = "hetero" ) # robuste s.e

r1ClusteredIncludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1992,
                                                         state != "Delaware" & state != "Alaska" & state != "Hawaii"),
                                    vcov = cluster ~ state + year ) # removed the restriction on deregulation date keep clustered se

r1RobustIncludeRegional = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                 data = df %>% filter(year >= 1972 & year <= 1992,
                                                      state != "Delaware" & state != "Alaska" & state != "Hawaii"),
                                 vcov = "hetero" ) # removed the restriction on deregulation date keep robust se

etable( r1ClusteredExcludeRegional, r1RobustExcludeRegional,  r1ClusteredIncludeRegional, r1RobustIncludeRegional,
        file = "Results/eq2.tex") # save output of regression 

r1ClusteredExclude99 = feols( realGrowthIncome ~ d | state + year,
                            data = df %>% filter(year >= 1972 & year <= 1999, # increase window of study, i.e until 1999
                                                 state != "Delaware",
                                                 year != ma),
                            vcov = cluster ~ state + year )

r1ClusteredExcludeRegional99 = feols( realGrowthIncome ~ d | state + s^year + mw^year + w^year + ne^year,
                                    data = df %>% filter(year >= 1972 & year <= 1999, # increase window of study
                                                         state != "Delaware" & state != "Alaska" & state != "Hawaii", # removed two additional states
                                                         year != ma),
                                    vcov = cluster ~ state + year )

etable( r1ClusteredExclude99,  r1ClusteredExcludeRegional99,
        file = "Results/eq1n2Extend.tex")

etable(r1RobustExclude,  r1RobustInclude, r1ClusteredExclude, r1ClusteredExclude99, r1RobustExcludeRegional,
       r1RobustIncludeRegional, r1ClusteredExcludeRegional, r1ClusteredExcludeRegional99,
       file = "Results/eqResults.tex", digits = 2, se.row = TRUE) 

############### event study ###############
esTWFETreated = feols(realGrowthIncome ~ i(time_to_treat, treat, ref = -1) | # regress each relative period, ref period -1
                      state + year, # unit and time fe                             
                      vcov = "hc1", # robust se                        
                      data = df %>%
                   filter(year >= 1972 & year <= 1992,
                          state != "Delaware") %>%
                   mutate( stateId = as.numeric(as.factor( state ) ), # create numeric state id
                           firstTreatment = ifelse( ma > 1992, 0, ifelse( ma == 1900, 1972, ma ) ), # vector with date of
                           #treatment 0 otherwise, already treated state as treated in 1972
                           treat = ifelse( ma > 1992, 0, 1 ), # vector of 1 for treated states
                           time_to_treat = ifelse(treat == 1, year - firstTreatment, 0) )) # relative year of treatment

esTWFEUntreated = feols(realGrowthIncome ~ i(time_to_treat, treat, ref = -1) | 
                   state + year,                             
                 vcov = "hc1",                        
                 data = df %>%
                   filter(year >= 1972 & year <= 1992,
                          state != "Delaware") %>%
                   mutate( stateId = as.numeric(as.factor( state ) ),
                           firstTreatment = ifelse( ma > 1992 | ma < 1972, 0, ma ), # consider deregulated state before 1972
                           #as not treated
                           treat = ifelse( ma > 1992 | ma < 1972, 0, 1 ),
                           time_to_treat = ifelse( treat == 1, year - firstTreatment, 0) ))

esTWFERemoved = feols(realGrowthIncome ~ i(time_to_treat, treat, ref = -1) | 
                   state + year,                             
                 vcov = "hc1",                        
                 data = df %>%
                   filter(year >= 1972 & year <= 1992,
                          state != "Delaware",
                          ma != 1900 ) %>%
                   mutate( stateId = as.numeric(as.factor( state ) ),
                           firstTreatment = ifelse( ma > 1992, 0, ma ),
                           treat = ifelse( ma > 1992, 0, 1 ),
                           time_to_treat = ifelse( treat == 1, year - firstTreatment, 0) ))

png( filename = "Results/es.png", width=1600, height=1600, res=300)
iplot(list(esTWFETreated, esTWFEUntreated, esTWFERemoved), # plot results
           xlab = 'Time to treatment',
           main = 'Event study: Staggered treatment (TWFE)',
      col = c(1, "#696969", "#FF0000"), pt.cex = 0.5)
legend("bottomleft", col = c(1, "#696969", "#FF0000"), pch = c(20, 17), 
             legend = c("Early as treated", "Early as untreated", "Early removed"), cex=0.5)
dev.off()

############### bacon ###############
df_bacon = bacon( # apply bacon decomposition from package
  realGrowthIncome ~ d,
  data = df %>% filter(year >= 1972 & year <= 1992,
                       state != "Delaware"),
  id_var = "state",
  time_var = "year")

group1 = df_bacon %>%
  filter( type == "Earlier vs Later Treated" | type == "Later vs Earlier Treated") 

ggplot(df_bacon, aes(x = weight, # plot results
                     y = estimate,
                     shape = factor(type),
                     colour = factor(type))) +
  scale_shape_manual(values=c(4, 1, 4, 17))+
  geom_point(size = 1.5) +
  geom_hline(yintercept = mean(df_bacon$estimate), 
             linetype = "longdash") +
  labs(x = "Weight",
       y = "Estimate",
       shape = "Type",
       colour = "Type",
       title = "Goodman-Bacon decomposition") +
  theme_stata() + scale_color_stata() +
  theme(legend.position = "bottom", legend.key.size = unit(0.5, 'cm'))+
  theme(legend.title = element_text(size = 3), 
        legend.text = element_text(size = 6))
ggsave( filename = "Results/bacon.png", dpi = 300 )

ggplot(group1, aes(x = weight, # plot results
                     y = estimate,
                     shape = factor(type),
                     colour = factor(type))) +
  scale_shape_manual(values=c(4, 4))+
  geom_point(size = 1.5) +
  geom_hline(yintercept = mean(group1$estimate), 
             linetype = "longdash") +
  labs(x = "Weight",
       y = "Estimate",
       shape = "Type",
       colour = "Type",
       title = "Goodman-Bacon decomposition") +
  theme_stata() + scale_color_stata() +
  theme(legend.position = "bottom", legend.key.size = unit(0.5, 'cm'))+
  theme(legend.title = element_text(size = 3), 
        legend.text = element_text(size = 6))
ggsave( filename = "Results/bacon2.png", dpi = 300 )


############### Bonus ###############
attgtTreated = att_gt( # apply Callaway Sant'Anna estimator for Event study 
  yname = "realGrowthIncome",
  tname = "year",
  idname = "stateId",
  gname = "firstTreatment",
  data = df %>%
    filter(year >= 1972 & year <= 1992,
           state != "Delaware") %>%
    mutate( stateId = as.numeric(as.factor( state ) ),
            firstTreatment = ifelse( ma > 1992, 0, ifelse( ma == 1900, 1972, ma ) ),
            treat = ifelse( ma > 1992, 0, 1 ),
            time_to_treat = ifelse(treat == 1, year - firstTreatment, 0) ),
  cluster = "state",
  control_group = "notyettreated",
  base_period = 'universal'
)

esTreated = aggte(attgtTreated,
           type = "dynamic", # For event study
           min_e = -20, max_e = +20,
           bstrap = TRUE,
           clustervars = "state",
           na.rm = TRUE
)

ggdid(esTreated, title = "Early as Treated", xgap = 5)
ggsave( filename = "Results/ESCallawayTreated.png", dpi = 300 )

# redo but consider the states treated before 1972 as untreated
attgtUntreated = att_gt(
  yname = "realGrowthIncome",
  tname = "year",
  idname = "stateId",
  gname = "firstTreatment",
  data = df %>%
    filter(year >= 1972 & year <= 1992,
           state != "Delaware") %>%
    mutate( stateId = as.numeric(as.factor( state ) ),
            firstTreatment = ifelse( ma > 1992 | ma < 1972, 0, ma ),
            treat = ifelse( ma > 1992 | ma < 1972, 0, 1 ),
            time_to_treat = ifelse( treat == 1, year - firstTreatment, 0) ),
  cluster = "state",
  control_group = "notyettreated",
  base_period = 'universal'
)

esUntreated = aggte(attgtUntreated,
           type = "dynamic", # For event study
           min_e = -20, max_e = +20,
           bstrap = TRUE,
           clustervars = "state",
           na.rm = TRUE
)

ggdid(esUntreated, title = "Early as Untreated", xgap = 5)
ggsave( filename = "Results/ESCallawayUntreated.png", dpi = 300 )
