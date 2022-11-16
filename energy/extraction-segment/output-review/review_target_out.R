## Tracey Mangin
## February 2, 2022
## Check targets

## libraries
library(data.table)
library(tidyverse)
library(openxlsx)

## model output location
save_external <- 1

## path names, ## UPDATE THESE WITH NEW RUNS!!!!!
# extraction_folder_path <- 'outputs/predict-production/extraction_2022-11-15/revision-sb-test/'
# extraction_folder_name <- 'subset_target_scens/'
external_path <- 'extraction-out/extraction_2022-11-15/revision-setbacks/'


## current date
cur_date              = Sys.Date()

## paths 
main_path  <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path  <-'data/stocks-flows/processed/'


## files
oil_price_file    <- 'oil_price_projections_revised.xlsx'

## external paths
main_path_external <- '/Volumes/calepa/'

## compiled state outputs 
state_out_list <- list()

## files to process
state_files_to_process <- list.files(paste0(main_path_external, external_path, 'state-out/'))
state_files_to_process <- list.files(paste0(main_path, extraction_folder_path, 'state-out/'))

for (i in 1:length(state_files_to_process)) {
  
  print(i)
  
  state_file_name <- state_files_to_process[i]
  
  state_scen_out <- readRDS(paste0(main_path_external, external_path, 'state-out/', state_file_name))
  # state_scen_out <- readRDS(paste0(main_path, extraction_folder_path, 'state-out/', state_file_name))
  
  state_out_list[[i]]  <- state_scen_out
  
}

state_subset_all <- rbindlist(state_out_list)

## save a version with the tax values
## ---------------------------------------------

## add oil price
oilpx_scens = setDT(read.xlsx(paste0(main_path, data_path, oil_price_file), 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

oilpx_scens <- oilpx_scens %>%
  mutate(scenario_name = ifelse(oil_price_scenario == "reference case", "EIA reference case",
                                ifelse(oil_price_scenario == "high oil price", "EIA high case",
                                       ifelse(oil_price_scenario == "low oil price", "EIA low case", "NA"))))

oilpx_scens$scenario_name <- factor(oilpx_scens$scenario_name, levels = c('EIA low case', 'EIA reference case', 'EIA high case'))

oilpx_scens <- oilpx_scens[, .(year, oil_price_scenario, oil_price_usd_per_bbl)]

## -------------------------------------

tax_val_df <- state_subset_all %>%
  select(scen_id, oil_price_scenario, carbon_price_scenario, setback_scenario, setback_existing,
         excise_tax_scenario, year, total_prod_bbl, total_ghg_mtCO2e, target_policy,
         target, target_val, tax_rate, carbon_price_usd_per_kg) %>%
  left_join(oilpx_scens) %>%
  mutate(excise_tax_val = tax_rate * oil_price_usd_per_bbl) %>%
  select(scen_id, oil_price_scenario, carbon_price_scenario, setback_scenario, setback_existing,
         excise_tax_scenario, target_policy, target, target_val,
         year, total_prod_bbl, total_ghg_mtCO2e, tax_rate, oil_price_usd_per_bbl,
         excise_tax_val, carbon_price_usd_per_kg)

fwrite(tax_val_df, paste0(save_info_path, 'state_levels_all_oil.csv'))


fwrite(tax_val_df, paste0(main_path, 'outputs/academic-out/extraction/nature-energy-rev-outputs/tax_values.csv'))




## check 2045 values
state_out_2045 <- state_subset_all[year == 2045, .(scen_id, oil_price_scenario, innovation_scenario,
                                                   carbon_price_scenario, ccs_scenario, setback_scenario, setback_existing,
                                                   prod_quota_scenario, excise_tax_scenario, target, 
                                                   target_policy, year, target_val, total_ghg_mtCO2e, tax_rate)]

state_out_2045[, diff := target_val - total_ghg_mtCO2e]

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
ghgs <- ggplot(state_subset_all, aes(x = year, y = total_ghg_mtCO2e, 
                                     color = target_policy, group = scen_id, lty = as.character(setback_existing))) +
  geom_line() +
  facet_wrap(~oil_price_scenario)

plotly::ggplotly(ghgs)

## carbon price
ggplot(state_subset_all, aes(x = year, y = carbon_price_usd_per_kg * 1000, color = carbon_price_scenario, group = scen_id)) +
  geom_line() +
  labs(y = "carbon price usd per mt")


## production
prod <- ggplot(state_subset_all, aes(x = year, y = total_prod_bbl / 1e6, color = target_policy, group = scen_id)) +
  geom_line() +
  facet_wrap(~oil_price_scenario)

plotly::ggplotly(prod)


