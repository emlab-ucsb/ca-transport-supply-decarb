## Tracey Mangin
## August 25, 2021
## Review outputs to determine runs needed to get policies

## libraries
library(tidyverse)
library(data.table)

## paths
proj_dir <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path <- 'data/stocks-flows/processed/'
outputs_path <- 'outputs/predict-production/extraction_2021-10-29/tax_scens_revised/state-out/'
carbon_out_path <- 'outputs/predict-production/extraction_2021-11-02/carbon_tax_scens_search9/state-out/'
carbon_sb_out_path <- 'outputs/predict-production/extraction_2021-12-02/carbon_setback_search/state-out/'
# tax_path <- 'outputs/predict-production/extraction_2021-08-27/tax-scenarios/'
# carbon_path <- 'outputs/predict-production/extraction_2021-09-01/carbon-scens/'
scen_path  = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/'


## files
# benchmark_file <- 'benchmark-state-level-results.csv'
# state_file <- 'tax_scens-state-level-results.csv'
ghg_file <- 'indust_emissions_2000-2019.csv'
# carbon_file <- 'carbon_scens-state-level-results.csv'


## 2019 GHG emissions
## --------------------------
hist_ghg <- fread(paste0(proj_dir, data_path, ghg_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])
ghg_target_90 <- 0.1 * ghg_2019

## create df for all excise tax outputs
## ---------------------------------------

## read in files
files_to_process <- list.files(paste0(proj_dir, outputs_path))

state_out_list <- list()

for (i in 1:length(files_to_process)) {
  
  id_name_tmp <- files_to_process[i]
  
  state_out_tmp <- readRDS(paste0(proj_dir, outputs_path, id_name_tmp))

  state_out_list[[i]]  <- state_out_tmp
  
}

state_all <- rbindlist(state_out_list)



## find baselines for both medium CCS cost and no CCS cost
## ----------------------------

baselines_df <- state_all[(oil_price_scenario == 'reference case' & 
                           innovation_scenario == 'low innovation' & 
                           carbon_price_scenario == 'price floor' & 
                           # ccs_scenario %chin% c('no css') & 
                           excise_tax_scenario == 'no tax' & 
                           # setback_scenario == 'no_setback' &
                           prod_quota_scenario == 'no quota' &
                           year == 2045)]

## tax scenarios out
## -----------------------------
tax_out <- state_all[(oil_price_scenario == 'reference case' & 
                            innovation_scenario == 'low innovation' & 
                            carbon_price_scenario == 'price floor' & 
                            # ccs_scenario %chin% c('no css') & 
                            # excise_tax_scenario == 'no tax' & 
                            setback_scenario == 'no_setback' &
                            prod_quota_scenario == 'no quota' &
                            year == 2045), .(oil_price_scenario, innovation_scenario,
                                             carbon_price_scenario, ccs_scenario,
                                             setback_scenario, prod_quota_scenario,
                                             excise_tax_scenario, year, total_ghg_mtCO2e)]


## setback ghg endpoints in 2045
## -----------------------------
setback_out <- baselines_df[setback_scenario != "no_setback", .(oil_price_scenario, innovation_scenario,
                                                               carbon_price_scenario, ccs_scenario,
                                                               setback_scenario, prod_quota_scenario,
                                                               excise_tax_scenario, year, total_ghg_mtCO2e)]

## find excise taxes that hit setback outputs
## -------------------------------------------

target_tax_df <- setback_out[, .(setback_scenario, ccs_scenario, total_ghg_mtCO2e)]
setnames(target_tax_df, c("setback_scenario", "total_ghg_mtCO2e"), c("target_scen", "target_ghg_mtCO2e"))

reduction_df <- data.table(target_scen = rep("90_perc_reduction", 2),
                           ccs_scenario = unique(setback_out$ccs_scenario),
                           target_ghg_mtCO2e = rep(ghg_target_90, 2))

target_tax_df <- rbind(target_tax_df, reduction_df)


tax_match_list <- list()
tax_match_noccs_list <- list()



for(i in 1:nrow(target_tax_df[ccs_scenario == "medium CCS cost"])) {
  
    target_tax_df_tmp <- target_tax_df[ccs_scenario == "medium CCS cost"]
    
    scen_tmp <- target_tax_df_tmp[i]
  
    scen_name_tmp <- scen_tmp[, target_scen][1]
    ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
    tax_match_tmp <- tax_out %>%
      filter(ccs_scenario == "medium CCS cost",
             total_ghg_mtCO2e <= ghg_tmp) %>%
      filter(ghg_tmp - total_ghg_mtCO2e == min(ghg_tmp - total_ghg_mtCO2e)) %>%
      mutate(target_scen = scen_name_tmp,
             target_emission = ghg_tmp) %>%
      select(ccs_scenario, excise_tax_scenario, total_ghg_mtCO2e, target_scen, target_emission)
    
    tax_match_list[[i]] <- tax_match_tmp
  
  
}

tax_match_df1 <- bind_rows(tax_match_list)

## now no ccs
for(i in 1:nrow(target_tax_df[ccs_scenario == "no ccs"])) {
  
  target_tax_df_tmp <- target_tax_df[ccs_scenario == "no ccs"]
  
  scen_tmp <- target_tax_df_tmp[i]
  
  scen_name_tmp <- scen_tmp[, target_scen][1]
  ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
  tax_match_tmp <- tax_out %>%
    filter(ccs_scenario == "no ccs",
           total_ghg_mtCO2e <= ghg_tmp) %>%
    filter(ghg_tmp - total_ghg_mtCO2e == min(ghg_tmp - total_ghg_mtCO2e)) %>%
    filter(abs(total_ghg_mtCO2e - ghg_tmp) == min(abs(total_ghg_mtCO2e - ghg_tmp))) %>%
    mutate(target_scen = scen_name_tmp,
           target_emission = ghg_tmp) %>%
    select(ccs_scenario, excise_tax_scenario, total_ghg_mtCO2e, target_scen, target_emission)
  
  tax_match_noccs_list[[i]] <- tax_match_tmp
  
  
}

tax_match_df2 <- bind_rows(tax_match_noccs_list)

tax_match_df <- rbind(tax_match_df1, tax_match_df2)





fwrite(tax_match_df, paste0(scen_path, 'setback_tax_values.csv'))



## carbon price
## create scenarios that hit the 2045 GHG emissions for all 3 setbacks and 90% of ghg emissions
## -----------------------------------

## read in files
carbon_files_to_process <- list.files(paste0(proj_dir, carbon_out_path))

carbon_out_list <- list()

for (i in 1:length(carbon_files_to_process)) {
  
  id_name_tmp <- carbon_files_to_process[i]
  
  state_out_tmp <- readRDS(paste0(proj_dir, carbon_out_path, id_name_tmp))
  
  carbon_out_list[[i]]  <- state_out_tmp
  
}

carbon_state_all <- rbindlist(carbon_out_list)

## carbon out list
carbon_out <- carbon_state_all[year == 2045 &
                               setback_scenario == "no_setback", .(oil_price_scenario, innovation_scenario,
                                               carbon_price_scenario, ccs_scenario,
                                               setback_scenario, prod_quota_scenario,
                                               excise_tax_scenario, year, total_ghg_mtCO2e)]

## find carbon px that hit setback outputs
## -------------------------------------------

target_carbon_df <- carbon_state_all[carbon_price_scenario == "price floor" &
                                    setback_scenario != "no_setback" &
                                    year == 2045, .(setback_scenario, ccs_scenario, total_ghg_mtCO2e)]

setnames(target_carbon_df, c("setback_scenario", "total_ghg_mtCO2e"), c("target_scen", "target_ghg_mtCO2e"))

reduction_df_ccs <- data.table(target_scen = rep("90_perc_reduction", 2),
                               ccs_scenario = unique(carbon_out$ccs_scenario),
                               target_ghg_mtCO2e = rep(ghg_target_90, 2))

target_carbon_df <- rbind(target_carbon_df, reduction_df_ccs)


carbon_match_list <- list()
carbon_match_noccs_list <- list()


for(i in 1:nrow(target_carbon_df[ccs_scenario == "medium CCS cost"])) {
  
  target_carbon_df_tmp <- target_carbon_df[ccs_scenario == "medium CCS cost"]
  
  scen_tmp <- target_carbon_df_tmp[i]
  scen_name_tmp <- scen_tmp[, target_scen][1]
  ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
  carbon_match_tmp <- carbon_out %>%
    filter(ccs_scenario == "medium CCS cost",
           total_ghg_mtCO2e <= ghg_tmp) %>%
    filter(ghg_tmp - total_ghg_mtCO2e == min(ghg_tmp - total_ghg_mtCO2e)) %>%
    mutate(target_scen = scen_name_tmp,
           target_emission = ghg_tmp) %>%
    select(ccs_scenario, carbon_price_scenario, total_ghg_mtCO2e, target_scen, target_emission)
  
  carbon_match_list[[i]] <- carbon_match_tmp
  
}

carbon_match_df <- bind_rows(carbon_match_list)

## now do no CCS
for(i in 1:nrow(target_carbon_df[ccs_scenario == "no ccs"])) {
  
  target_carbon_df_tmp <- target_carbon_df[ccs_scenario == "no ccs"]
  
  scen_tmp <- target_carbon_df_tmp[i]
  scen_name_tmp <- scen_tmp[, target_scen][1]
  ghg_tmp <- scen_tmp[, target_ghg_mtCO2e][1]
  
  carbon_match_tmp <- carbon_out %>%
    filter(ccs_scenario == "no ccs",
           total_ghg_mtCO2e <= ghg_tmp) %>%
    filter(ghg_tmp - total_ghg_mtCO2e == min(ghg_tmp - total_ghg_mtCO2e)) %>%
    mutate(target_scen = scen_name_tmp,
           target_emission = ghg_tmp) %>%
    select(ccs_scenario, carbon_price_scenario, total_ghg_mtCO2e, target_scen, target_emission)
  
  carbon_match_noccs_list[[i]] <- carbon_match_tmp
  
}

carbon_match_noccs_df <- bind_rows(carbon_match_noccs_list)

## bind
carbon_match_df <- rbind(carbon_match_df, carbon_match_noccs_df)



## ----------------------------------------------------------------------------------
## just 90% for 2019 emissions, carbon tax + excise tax
## ----------------------------------------------------------------------------------

## read in files
carbon_sb_files_to_process <- list.files(paste0(proj_dir, carbon_sb_out_path))

carbon_sb_out_list <- list()

for (i in 1:length(carbon_sb_files_to_process)) {
  
  id_name_tmp <- carbon_sb_files_to_process[i]
  
  state_out_tmp <- readRDS(paste0(proj_dir, carbon_sb_out_path, id_name_tmp))
  
  carbon_sb_out_list[[i]]  <- state_out_tmp
  
}

carbon_sb_state_all <- rbindlist(carbon_sb_out_list)

## carbon out list
carbon_sb_out <- carbon_sb_state_all[year == 2045, .(scen_id, oil_price_scenario, innovation_scenario,
                                                     carbon_price_scenario, ccs_scenario,
                                                     setback_scenario, prod_quota_scenario,
                                                     excise_tax_scenario, year, total_ghg_mtCO2e)]

carbon_sb_out[, target_scen := "90_perc_reduction"]
carbon_sb_out[, target_ghg_mtCO2e := ghg_target_90]


carbon_sb_target <- carbon_sb_out %>%
  filter(target_ghg_mtCO2e >= total_ghg_mtCO2e) %>%
  group_by(setback_scenario, ccs_scenario) %>%
  filter(abs(ghg_target_90 - total_ghg_mtCO2e) == min(abs(ghg_target_90 - total_ghg_mtCO2e))) %>%
  ungroup() %>%
  select(ccs_scenario, setback_scenario, carbon_price_scenario, total_ghg_mtCO2e, target_scen, target_emission = target_ghg_mtCO2e)
  
## join with other carbon taxes
carbon_match_df <- carbon_match_df %>%
  mutate(setback_scenario = "no_setback") %>%
  select(ccs_scenario, setback_scenario, carbon_price_scenario, total_ghg_mtCO2e, target_scen, target_emission) %>%
  rbind(carbon_sb_target)

## save
fwrite(carbon_match_df, paste0(scen_path, 'setback_carbon_values.csv'))


