## Tracey Mangin
## November 25, 2020
## production and GHG emissions: BAU, E1, E2

library(tidyverse)
library(data.table)
library(openxlsx)


## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
results_path <- "outputs/predict-production/extraction_2021-08-10/update_final/"
save_path2    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios/'

## file names
prod_file  <- "well_prod_m_processed.csv"
ghg_file <- 'ghg_emissions_x_field_2018-2045.csv'
state_out_file <- "benchmark-state-level-results.csv"

## load data
## --------------

## monthly well prod
well_prod <- fread(paste0(calepa_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
## ghg factors
ghg_factors = fread(file.path(calepa_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## outputs
state_out <- fread(paste0(calepa_path, results_path, state_out_file))

## production in 2019
## --------------------------

## calculate 2019 production, emissions, revenue
init_prod <- well_prod %>%
  filter(year == 2019) %>%
  select(doc_field_code, doc_fieldname, year, OilorCondensateProduced) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(total_prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

setDT(init_prod)

## merge with ghg factors
init_prod <- merge(init_prod, ghg_factors_2019,
                   by = c('doc_field_code', 'year'),
                   all.x = T)

init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]


init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]


## summarise for the state
init_state <- init_prod %>%
  group_by(year) %>%
  summarise(prodution_bbl = sum(total_prod_bbl),
            total_ghg_kgCO2e = sum(total_ghg_kgCO2e)) %>%
  ungroup() %>%
  mutate(total_ghg_mmtCO2e = total_ghg_kgCO2e / (1e6 * 1e3)) 


# ## ghg emissions
# init_df3 <- well_prod19 %>%
#   left_join(ghg_factors) %>%
#   mutate(kgCO2e = upstream_kgCO2e_bbl * total_bbls,
#          ghg_mmt = kgCO2e / (1e6 * 1e3)) %>%
#   group_by(year) %>%
#   summarise(ghg_mmt = sum(ghg_mmt, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(indicator = "ghg_mmt") %>%
#   rename(X2019 = ghg_mmt) 

## get selected scenarios

bau_out <- state_out %>%
  filter(oil_price_scenario == "reference case",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax") %>%
  mutate(scenario_name = "BAU",
        scenario = "BAU") %>%
  select(scenario, scenario_name, oil_price_scenario:year, ghg_mmt_co2e = total_ghg_mtCO2e, production_bbl = total_prod_bbl)

ss_out <- state_out %>%
  filter(oil_price_scenario == "reference case",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "quota_20",
         setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
  mutate(scenario_name = ifelse(setback_scenario == "no_setback", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
         scenario = ifelse(setback_scenario == "no_setback", "E1", "E2")) %>%
  select(scenario, scenario_name, oil_price_scenario:year, ghg_mmt_co2e = total_ghg_mtCO2e, production_bbl = total_prod_bbl) 
  
## bind

all_scen <- tibble(scenario = c("BAU", "E1", "E2"),
                   scenario_name = c("BAU", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
                   oil_price_scenario = rep("reference case", 3),
                   innovation_scenario = rep("low innovation", 3),
                   carbon_price_scenario = rep("price floor", 3),
                   ccs_scenario = rep("medium CCS cost", 3),
                   excise_tax_scenario = rep("no tax", 3),
                   prod_quota_scenario = c("no quota", rep("quota_20", 2)),
                   setback_scenario = c("no_setback", "no_setback", "setback_2500ft"),
                   year = rep(2019, 3),
                   ghg_mmt_co2e = rep(init_state$total_ghg_mmtCO2e, 3),
                   production_bbl = rep(init_state$prodution_bbl, 3)) %>%
  rbind(bau_out) %>%
  rbind(ss_out) %>%
  arrange(scenario, year)


## cumulative
## ----------------------

all_scens_cumul <- all_scen %>%
  group_by(scenario, scenario_name, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario, excise_tax_scenario, prod_quota_scenario, setback_scenario) %>%
  mutate(cumulative_ghg = cumsum(ghg_mmt_co2e),
         cumulative_prod_bbl = cumsum(production_bbl)) %>%
  ungroup() %>%
  select(scenario:ghg_mmt_co2e, cumulative_ghg, production_bbl, cumulative_prod_bbl)

write_csv(all_scens_cumul, paste0(save_path2, "extraction_prod_ghg_outputs_revised.csv"))

