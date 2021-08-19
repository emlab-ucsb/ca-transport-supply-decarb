## Tracey Mangin
## August 19, 2021
## Tax outputs

## libraries
library(tidyverse)
library(data.table)

## paths
proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
outputs_path <- 'outputs/predict-production/extraction_2021-08-19/tax_scenarios/'
scen_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/'


## files
state_file <- 'tax_scens-state-level-results.csv'

## read in files
state_out <- fread(paste0(proj_dir, outputs_path, state_file), header = T, colClasses = c('total_ghg_mtCO2e' = 'numeric',
                                                                                         'year' = 'integer',
                                                                                         'total_prod_bbl' = 'numeric',
                                                                                         'total_ghg_kgCO2e', 'numeric'))
  
## setback scenarios
setback_out <- state_out[(oil_price_scenario == 'reference case' & 
                            innovation_scenario == 'low innovation' & 
                            carbon_price_scenario == 'price floor' & 
                            ccs_scenario == 'medium CCS cost' & 
                            excise_tax_scenario == 'no tax' & 
                            # setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota')]

setback_out[, scenario_grp := 'setback scenarios']

tax_out <- state_out[(oil_price_scenario == 'reference case' & 
                      innovation_scenario == 'low innovation' & 
                      carbon_price_scenario == 'price floor' & 
                      ccs_scenario == 'medium CCS cost' & 
                      # excise_tax_scenario == 'no tax' & 
                      setback_scenario == 'no_setback' &
                      prod_quota_scenario == 'no quota')]

tax_out[, scenario_grp := 'tax scenarios']


## all scens
all_scens <- rbind(setback_out, tax_out)

## find excise taxes that hit setback outputs
setback_final <- setback_out[year == 2045 & setback_scenario != "no_setback", .(scenario_grp, setback_scenario, excise_tax_scenario, 
                                             year, total_prod_bbl, total_ghg_kgCO2e, total_ghg_mtCO2e)]


tax_final <- tax_out[year == 2045, .(scenario_grp, setback_scenario, excise_tax_scenario, 
                                     year, total_prod_bbl, total_ghg_kgCO2e, total_ghg_mtCO2e)]



tax_match_list <- list()

for(i in 1:nrow(setback_final)) {
  
  scen_tmp <- setback_final[i]
  setback_scen_tmp <- scen_tmp[, setback_scenario][1]
  ghg_tmp <- scen_tmp[, total_ghg_kgCO2e][1]
  
  tax_match_tmp <- tax_final %>%
    filter(abs(total_ghg_kgCO2e - ghg_tmp) == min(abs(total_ghg_kgCO2e - ghg_tmp))) %>%
    mutate(match_scen = setback_scen_tmp)
  
  tax_match_list[[i]] <- tax_match_tmp
  
}

tax_match_df <- bind_rows(tax_match_list)

fwrite(tax_match_df, paste0(scen_path, 'setback_tax_values.csv'))





ggplot(setback_out, aes(x = year, y = total_ghg_mtCO2e, color = setback_scenario, group = setback_scenario)) +
  geom_line() +
  labs(y = 'GHG emissions (MtCO2e)',
       x = NULL) +
  scale_y_continuous(limits = c(0, 15)) +
  theme_bw()

ggplot(tax_out, aes(x = year, y = total_ghg_mtCO2e, color = excise_tax_scenario, group = excise_tax_scenario)) +
  geom_line() +
  labs(y = 'GHG emissions (MtCO2e)',
       x = NULL) +
  scale_y_continuous(limits = c(0, 15)) +
  theme_bw()


