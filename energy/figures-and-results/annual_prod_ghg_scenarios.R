## Tracey Mangin
## November 25, 2020
## production and GHG emissions: BAU, E1, E2

library(tidyverse)
library(data.table)
library(openxlsx)
library(feather)

## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
save_path2    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs/selected-scenarios/'

## load data
well_prod_org <- read_rds("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds") %>% as.data.table()

wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  select(doc_field_code = FieldCode, FieldName) %>%
  unique() %>%
  as.data.table()

ghg_factors <- fread(file.path(calepa_path, 'outputs', 'stocks-flows', 'ghg_emissions_x_field_2015.csv'), header = T)

summary_out <- read_feather(paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/summary_outputs_2020-2045.csv"))

## production in 2019
setnames(well_prod_org, "FieldCode", "doc_field_code")
well_prod <- well_prod_org[year == 2019]

well_prod19 <- well_prod %>%
  mutate(doc_field_code = paste0("00", doc_field_code),
         doc_field_code = str_sub(doc_field_code, start= -3)) %>%
  group_by(doc_field_code, year) %>%
  summarise(total_bbls = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  left_join(wells_19) %>%
  ## remove 000, remove gas
  filter(doc_field_code != "000") %>%
  mutate(gas = str_extract(FieldName, "Gas")) %>%
  filter(is.na(gas)) %>%
  select(-gas)

prod_hist19 <- well_prod19 %>%
  group_by(year) %>%
  summarise(production_bbl = sum(total_bbls)) %>%
  ungroup()

## ghg
ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]
ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]

## ghg emissions
init_df3 <- well_prod19 %>%
  left_join(ghg_factors) %>%
  mutate(kgCO2e = upstream_kgCO2e_bbl * total_bbls,
         ghg_mmt = kgCO2e / (1e6 * 1e3)) %>%
  group_by(year) %>%
  summarise(ghg_mmt = sum(ghg_mmt, na.rm = T)) %>%
  ungroup() %>%
  mutate(indicator = "ghg_mmt") %>%
  rename(X2019 = ghg_mmt) 

## get selected scenarios

bau_out <- summary_out %>%
  filter(oil_price_scenario == "iea oil price",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax") %>%
  mutate(scenario_name = "BAU",
        scenario = "BAU") %>%
  select(scenario, scenario_name, oil_price_scenario:year, ghg_mmt_co2e = ghg_mmt_inno_ccs_adj, production_bbl = prod_mbbl) %>%
  mutate(production_bbl = production_bbl * 1e6)

ss_out <- summary_out %>%
  filter(oil_price_scenario == "iea oil price",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "quota_20",
         setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
  mutate(scenario_name = ifelse(setback_scenario == "no_setback", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
         scenario = ifelse(setback_scenario == "no_setback", "E1", "E2")) %>%
  select(scenario, scenario_name, oil_price_scenario:year, ghg_mmt_co2e = ghg_mmt_inno_ccs_adj, production_bbl = prod_mbbl) %>%
  mutate(production_bbl = production_bbl * 1e6)
  
## bind

all_scen <- tibble(scenario = c("BAU", "E1", "E2"),
                   scenario_name = c("BAU", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
                   oil_price_scenario = rep("iea oil price", 3),
                   innovation_scenario = rep("low innovation", 3),
                   innovation_multiplier = rep(1, 3),
                   carbon_price_scenario = rep("price floor", 3),
                   ccs_scenario = rep("medium CCS cost", 3),
                   excise_tax_scenario = rep("no tax", 3),
                   prod_quota_scenario = c("no quota", rep("quota_20", 2)),
                   setback_scenario = c("no_setback", "no_setback", "setback_2500ft"),
                   year = rep(2019, 3),
                   ghg_mmt_co2e = rep(init_df3$X2019, 3),
                   production_bbl = rep(prod_hist19$production_bbl)) %>%
  rbind(bau_out) %>%
  rbind(ss_out) %>%
  arrange(scenario, year)


## cumulative
## ----------------------

all_scens_cumul <- all_scen %>%
  group_by(scenario, scenario_name, oil_price_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario,
           ccs_scenario, excise_tax_scenario, prod_quota_scenario, setback_scenario) %>%
  mutate(cumulative_ghg = cumsum(ghg_mmt_co2e),
         cumulative_prod_bbl = cumsum(production_bbl)) %>%
  ungroup() %>%
  select(scenario:ghg_mmt_co2e, cumulative_ghg, production_bbl, cumulative_prod_bbl)

write_csv(all_scens_cumul, paste0(save_path2, "extraction_prod_ghg_outputs.csv"))

