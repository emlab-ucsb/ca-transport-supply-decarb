## Tracey Mangin
## August 25, 2021
## Review outputs to determine runs needed to get policies

## libraries
library(tidyverse)
library(data.table)

## paths
proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path <- 'data/stocks-flows/processed/'
outputs_path <- 'outputs/predict-production/extraction_2021-08-20/tax_update_correction/'
tax_path <- 'outputs/predict-production/extraction_2021-08-27/tax-scenarios/'
carbon_path <- 'outputs/predict-production/extraction_2021-09-01/carbon-scens/'
scen_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/'


## files
benchmark_file <- 'benchmark-state-level-results.csv'
state_file <- 'tax_scens-state-level-results.csv'
ghg_file <- 'indust_emissions_2000-2019.csv'
carbon_file <- 'carbon_scens-state-level-results.csv'


## 2019 GHG emissions
## --------------------------
hist_ghg <- fread(paste0(proj_dir, data_path, ghg_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])
ghg_target_90 <- 0.1 * ghg_2019

## benchmark outputs with correction
## ----------------------------

benchmark_out <- fread(paste0(proj_dir, outputs_path, benchmark_file), header = T)

## tax scenarios out
## -----------------------------
tax_out <- fread(paste0(proj_dir, tax_path, state_file), header = T)


## setback ghg endpoints in 2045
## -----------------------------

## setback scenarios
setback_out <- tax_out[(oil_price_scenario == 'reference case' & 
                              innovation_scenario == 'low innovation' & 
                              carbon_price_scenario == 'price floor' & 
                              ccs_scenario == 'medium CCS cost' & 
                              excise_tax_scenario == 'no tax' & 
                              # setback_scenario == 'no_setback' &
                              prod_quota_scenario == 'no quota')]

setback_out <- setback_out[year == 2045]

setback_out <- setback_out[setback_scenario != "no_setback", .(oil_price_scenario, innovation_scenario,
                                                               carbon_price_scenario, ccs_scenario,
                                                               setback_scenario, prod_quota_scenario,
                                                               excise_tax_scenario, year, total_ghg_mtCO2e)]




## review tax scenario outputs 
## create scenarios that hit 2045 GHG emissions for all 3 setbacks and 90% of 2019 ghg emissions
## -----------------------------------

tax_out <- tax_out[(oil_price_scenario == 'reference case' & 
                    innovation_scenario == 'low innovation' & 
                    carbon_price_scenario == 'price floor' & 
                    ccs_scenario == 'medium CCS cost' & 
                    # excise_tax_scenario == 'no tax' & 
                    setback_scenario == 'no_setback' &
                    prod_quota_scenario == 'no quota')]

tax_out <- tax_out[year == 2045, .(oil_price_scenario, innovation_scenario,
                                   carbon_price_scenario, ccs_scenario,
                                   setback_scenario, prod_quota_scenario,
                                   excise_tax_scenario, year, total_ghg_mtCO2e)]


## find excise taxes that hit setback outputs
## -------------------------------------------

target_tax_df <- setback_out[, .(setback_scenario, total_ghg_mtCO2e)]
setnames(target_tax_df, c("setback_scenario", "total_ghg_mtCO2e"), c("target_scen", "target_ghg_mtCO2e"))

reduction_df <- data.table(target_scen = "90_perc_reduction",
                           target_ghg_mtCO2e = ghg_target_90)

target_tax_df <- rbind(target_tax_df, reduction_df)


tax_match_list <- list()

for(i in 1:nrow(target_tax_df)) {
  
  scen_tmp <- target_tax_df[i]
  scen_name_tmp <- scen_tmp[, target_scen][1]
  ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
  tax_match_tmp <- tax_out %>%
    filter(abs(total_ghg_mtCO2e - ghg_tmp) == min(abs(total_ghg_mtCO2e - ghg_tmp))) %>%
    mutate(match_scen = scen_name_tmp,
           match_emission = ghg_tmp) %>%
    select(excise_tax_scenario, total_ghg_mtCO2e, match_scen, match_emission)
  
  tax_match_list[[i]] <- tax_match_tmp
  
}

tax_match_df <- bind_rows(tax_match_list)


fwrite(tax_match_df, paste0(scen_path, 'setback_tax_values.csv'))



## carbon price
## create scenarios that hit the 2045 GHG emissions for all 3 setbacks and 90% of ghg emissions
## -----------------------------------

carbon_out <- fread(paste0(proj_dir, carbon_path, carbon_file), header = T)

carbon_out <- carbon_out[(oil_price_scenario == 'reference case' & 
                            innovation_scenario == 'low innovation' & 
                            # carbon_price_scenario == 'price floor' & 
                            ccs_scenario == 'medium CCS cost' & 
                            excise_tax_scenario == 'no tax' &
                            setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota')]

carbon_out <- carbon_out[year == 2045, .(oil_price_scenario, innovation_scenario,
                                         carbon_price_scenario, ccs_scenario,
                                         setback_scenario, prod_quota_scenario,
                                         excise_tax_scenario, year, total_ghg_mtCO2e)]

## find carbon px that hit setback outputs
## -------------------------------------------

target_carbon_df <- setback_out[, .(setback_scenario, total_ghg_mtCO2e)]
setnames(target_carbon_df, c("setback_scenario", "total_ghg_mtCO2e"), c("target_scen", "target_ghg_mtCO2e"))

reduction_df <- data.table(target_scen = "90_perc_reduction",
                           target_ghg_mtCO2e = ghg_target_90)

target_carbon_df <- rbind(target_carbon_df, reduction_df)


carbon_match_list <- list()

for(i in 1:nrow(target_carbon_df)) {
  
  scen_tmp <- target_carbon_df[i]
  scen_name_tmp <- scen_tmp[, target_scen][1]
  ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
  carbon_match_tmp <- carbon_out %>%
    filter(abs(total_ghg_mtCO2e - ghg_tmp) == min(abs(total_ghg_mtCO2e - ghg_tmp))) %>%
    mutate(match_scen = scen_name_tmp,
           match_emission = ghg_tmp) %>%
    select(carbon_price_scenario, total_ghg_mtCO2e, match_scen, match_emission)
  
  carbon_match_list[[i]] <- carbon_match_tmp
  
}

carbon_match_df <- bind_rows(carbon_match_list)


fwrite(carbon_match_df, paste0(scen_path, 'setback_carbon_values.csv'))







