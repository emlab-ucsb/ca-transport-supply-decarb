## Tracey Mangin
## February 2, 2022
## Check targets

## libraries
library(data.table)
library(tidyverse)

## model output location
save_external <- 1

## path names, ## UPDATE THESE WITH NEW RUNS!!!!!
extraction_folder_path <- 'outputs/predict-production/extraction_2021-12-06/'
extraction_folder_name <- 'subset_target_scens/'
external_path <- 'extraction-out/extraction_2022-02-02/all_target/'


## current date
cur_date              = Sys.Date()

## paths 
main_path  <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path  <-'data/stocks-flows/processed/'

## external paths
main_path_external <- '/Volumes/calepa/'

## compiled state outputs 
state_out_list <- list()

## files to process
state_files_to_process <- list.files(paste0(main_path_external, external_path, 'state-out/'))


for (i in 1:length(state_files_to_process)) {
  
  print(i)
  
  state_file_name <- state_files_to_process[i]
  
  state_scen_out <- readRDS(paste0(main_path_external, external_path, 'state-out/', state_file_name))
  
  state_out_list[[i]]  <- state_scen_out
  
}

state_subset_all <- rbindlist(state_out_list)

## check 2045 values
state_out_2045 <- state_subset_all[year == 2045, .(scen_id, oil_price_scenario, innovation_scenario,
                                                   carbon_price_scenario, ccs_scenario, setback_scenario,
                                                   prod_quota_scenario, excise_tax_scenario, target, 
                                                   target_policy, year, total_ghg_mtCO2e, tax_rate)]

## change setback target
state_out_2045[, target := fifelse(setback_scenario != "no_setback" & target == "no_target", 
                                   setback_scenario, target)]


## for plotting
state_subset_all[, target := fifelse(setback_scenario != "no_setback" & target == "no_target", 
                                                       setback_scenario, target)]

## for plotting
state_subset_all[, target_policy := fifelse(setback_scenario != "no_setback" & target_policy == "no_target_policy", 
                                     "setback_scenario", target_policy)]

## ghgs
ghgs <- ggplot(state_subset_all, aes(x = year, y = total_ghg_mtCO2e, color = target_policy, group = scen_id)) +
  geom_line() +
  facet_wrap(~oil_price_scenario)

plotly::ggplotly(ghgs)

## carbon price
ggplot(state_subset_all, aes(x = year, y = carbon_price_usd_per_kg * 1000, color = carbon_price_scenario, group = scen_id)) +
  geom_line() +
  labs(y = "carbon price usd per mt")





