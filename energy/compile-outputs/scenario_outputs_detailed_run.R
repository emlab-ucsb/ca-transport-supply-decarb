## Tracey Mangin
## November 10, 2020
## Scenario outputs

library(tidyverse)
library(data.table)

## paths
calepa_path   <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
extract_path  <- paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/")
extract_vintage_path <- paste0(extract_path, "scen_outputs/vintage/")
model_path    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
save_path     <-'/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens'
save_path2    <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs'

## ----------------------------------------------------
## read in output
## ----------------------------------------------------

extract_field_out <- fread(file.path(save_path2, 'all-scenarios', 'field_level_prod_rev_2020-2045.csv'), header = T)
extract_county_out <- fread(file.path(save_path2, 'all-scenarios', 'extraction_county_outputs.csv'), header = T)
# refining_site_out <- read_csv(paste0(save_path2, "/all-scenarios/refinery_site_outputs.csv"))
# refining_county_out <- read_csv(paste0(save_path2, "/all-scenarios/refinery_county_outputs.csv"))

refining_site_out <- read_csv(paste0(save_path2, "/all-scenarios/refinery_site_outputs_ratios.csv"))
refining_county_out <- read_csv(paste0(save_path2, "/all-scenarios/refinery_county_outputs_ratios.csv"))

# bau_fields <- read_csv(paste0(save_path2,'/bau/bau_field_level_prod_rev_2020-2045.csv'))
# bau_county <- read_csv(paste0(save_path2,'/bau/bau_extraction_county_outputs.csv'))

## extraction scenario selection

## Macro conditions:
## oil price: IEA
## carbon price: price floor
## innovation: low
## CCS: medium 

## Extraction policy 1:
##  -20% production quota
## no set back
## no tax

## Extraction policy 2:
#  -20% production quota and 2500 ft setback
## no tax

ss_extract_field <- extract_field_out %>%
  filter(oil_price_scenario == "iea oil price",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "quota_20",
         setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
  mutate(scenario_name = ifelse(setback_scenario == "no_setback", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
         scenario = ifelse(setback_scenario == "no_setback", "E1", "E2")) %>%
  select(scenario, scenario_name, year:revenue) %>%
  arrange(scenario)

write_csv(ss_extract_field, paste0(save_path2, "/selected-scenarios/selected_scens_extract_field_outputs.csv"))




## county
ss_extract_county <- extract_county_out %>%
  filter(oil_price_scenario == "iea oil price",
         carbon_price_scenario == "price floor",
         innovation_scenario == "low innovation",
         ccs_scenario == "medium CCS cost",
         excise_tax_scenario == "no tax",
         prod_quota_scenario == "quota_20",
         setback_scenario %in% c("no_setback", "setback_2500ft")) %>%
  mutate(scenario_name = ifelse(setback_scenario == "no_setback", "Extraction Scenario 1: Quota", "Extraction Scenario 2: Quota + 2500ft setback"),
         scenario = ifelse(setback_scenario == "no_setback", "E1", "E2")) %>%
  select(scenario, scenario_name, county_name:revenue) %>%
  arrange(scenario)

write_csv(ss_extract_county, paste0(save_path2, "/selected-scenarios/selected_scens_extract_county_outputs.csv"))

# ## compare bau and scenario extraction
# bau_fields %>% 
#   mutate(scenario = "BAU") %>%
#   filter(doc_fieldname %in% c("Alegria Offshore (ABD)", "Belridge  South", "Elk Hills")) %>%
#   ggplot(aes(x = year, y = production_bbl, color = scenario)) +
#   geom_line(size = 1, alpha = 0.5) +
#   geom_line(data = ss_extract_field %>% filter(doc_fieldname %in% c("Alegria Offshore (ABD)", "Belridge  South", "Elk Hills")), aes(x = year, y = production_bbl)) + 
#   facet_wrap(~doc_fieldname, scales = "free") 
  


## refining
## -------------------------

## Refining policy 1:
##   -demand: LC1
##   -oil price: IEA
##   -carbon price: price floor
##   -innovation: low
##   -CCS: medium 
## -exports: historic

ss_site_out <- refining_site_out %>%
  filter(demand_scenario == "LC1",
         refining_scenario %in% c("historic exports", "low exports"),
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost") %>%
  mutate(scenario_name = ifelse(refining_scenario == "historic exports", "Refining Scenario 1: LC1 + historic exports", "Refining Scenario 2: LC1 + low exports"),
         scenario = ifelse(refining_scenario == "historic exports", "R1", "R2")) %>%
  select(scenario, scenario_name, year:ratio) %>%
  arrange(scenario)

# write_csv(ss_site_out, paste0(save_path2, "/selected-scenarios/selected_scens_refining_site_outputs.csv"))
write_csv(ss_site_out, paste0(save_path2, "/selected-scenarios/selected_scens_refining_site_outputs_ratios.csv"))

##  Refining policy 2:
##   -demand: LC1
##   -exports: low
##   -oil price: IEA
##   -carbon price: price floor
##   -innovation: low
##   -CCS: medium 

ss_refining_county_out <- refining_county_out %>%
  filter(demand_scenario == "LC1",
         refining_scenario %in% c("historic exports", "low exports"),
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         oil_price_scenario == "iea oil price") %>%
  mutate(scenario_name = ifelse(refining_scenario == "historic exports", "Refining Scenario 1: LC1 + historic exports", "Refining Scenario 2: LC1 + low exports"),
         scenario = ifelse(refining_scenario == "historic exports", "R1", "R2")) %>%
  select(scenario, scenario_name, year:ratio) %>%
  arrange(scenario) 

# write_csv(ss_refining_county_out, paste0(save_path2, "/selected-scenarios/selected_scens_refining_county_outputs.csv"))
write_csv(ss_refining_county_out, paste0(save_path2, "/selected-scenarios/selected_scens_refining_county_outputs_ratios.csv"))

