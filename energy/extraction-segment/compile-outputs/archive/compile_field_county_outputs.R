## Tracey Mangin
## October 27, 2020
## compile outputs

library(tidyverse)
library(feather)
library(data.table)
library(readxl)
library(openxlsx)

## start time
start_time <- Sys.time()

## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
extract_path <- paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/")
extract_vintage_path <- paste0(extract_path, "scen_outputs/vintage/")
extract_vintage_pathr <- paste0(extract_path, "scen_outputs/vintage-replace/")
model_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'

## save path
save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens'
save_path2      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs'
review_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens/post-review/'

# load field-county matches
county_file <- 'annual_field_county_production_proportion.csv'
field_counties = fread(file.path(model_path, 'stocks-flows', county_file), header = T)

prices <- read_xlsx(paste0(calepa_path, "data/stocks-flows/processed/oil_price_projections.xlsx"), sheet = "real")

prices2 <- prices %>%
  pivot_longer(`EIA Reference case 2020$/b`:`BP oil price 2020$/b`, names_to = "oil_price_scenario", values_to = "oil_price_usd_per_bbl_real") %>%
  mutate(oil_price_scenario = ifelse(oil_price_scenario == "EIA Reference case 2020$/b", "reference case",
                                     ifelse(oil_price_scenario == "EIA High oil price 2020$/b", "high oil price",
                                            ifelse(oil_price_scenario == "EIA Low oil price 2020$/b", "low oil price",
                                                   ifelse(oil_price_scenario == "IEA Delayed Recovery scenario 2020$/b", "iea oil price", "bp oil price"))))) %>%
  rename(year = Year) %>%
  arrange(oil_price_scenario, year) %>%
  filter(year >= 2020)



## clear field counties
field_counties = field_counties[, oil_prod := NULL]
field_counties = field_counties[, FieldName := NULL]
field_counties[, FieldCode := sprintf("%03d", FieldCode)]
setnames(field_counties, "FieldCode", "doc_field_code")

field_counties <- field_counties %>%
  filter(!is.na(prop_production),
         prop_production > 0) %>%
  group_by(doc_field_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(-year) %>%
  as.data.table()



## files to pull info from
file_list_vintage <- list.files(extract_vintage_path)


## function for field level prod

create_field_summary_df <- function(file_name) {
  
  # out <- readRDS(paste0(extract_vintage_path, file_name))
  out <- setDT(read_feather(paste0(extract_vintage_path, file_name)))
  
  summary_field_dt <- out[, .(year, doc_field_code, doc_fieldname, vintage_start, oil_price_scenario, innovation_scenario,
                              carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                              ccs_adopted, n_wells, production_bbl, upstream_kgCO2e_inno_ccs_adj)]
  
  summary_field_dt2 <- summary_field_dt[, well_type := ifelse(vintage_start >= 2020, "new", "existing")]
  
  summary_field_dt3 <- summary_field_dt2[, vintage_start := NULL]
  
  summary_field_dt4 <- summary_field_dt3[, .(ccs_adopted = unique(ccs_adopted),
                                            n_wells = sum(n_wells, na.rm = T),
                                            production_bbl = sum(production_bbl, na.rm = T),
                                            upstream_kgCO2e = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T)), 
                                         by = .(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                                ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type)]
  
  summary_field_df5 <- summary_field_dt4[, n_wells := ifelse(well_type == "new", n_wells, NA)]
  
  # summary_field_dt <- out %>%
  #   mutate(well_type = ifelse(vintage == "new", "new", "existing")) %>%
  #   group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
  #            setback_scenario, prod_quota_scenario, excise_tax_scenario, well_type, year) %>%
  #   summarise(ccs_adopted = median(ccs_adopted),
  #             n_wells = sum(n_wells, na.rm = T),
  #             upstream_kgCO2e_adj = sum(upstream_kgCO2e_adj, na.rm = T),
  #             production_bbl = sum(production_bbl, na.rm = T)) %>%
  #   ungroup() %>%
  #   rename(upstream_kgCO2e = upstream_kgCO2e_adj) %>%
  #   as.data.table()
  
}



field_level_outputs <- purrr::map(file_list_vintage, create_field_summary_df) %>%
  bind_rows()

fwrite(field_level_outputs, file.path(save_path, 'compiled', 'field_level_prod_emissions_2020-2045.csv'), row.names = F)
saveRDS(field_level_outputs, paste0(save_path, '/compiled/field_level_prod_emissions_2020-2045.rds'))

## now summarise by field and year, add revenue and save to save2path
full_field_out <- field_level_outputs %>%
  group_by(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
  summarise(production_bbl = sum(production_bbl)) %>%
  ungroup() %>%
  left_join(prices2) %>%
  mutate(revenue = production_bbl * oil_price_usd_per_bbl_real)

fwrite(full_field_out, file.path(save_path2, 'all-scenarios', 'field_level_prod_rev_2020-2045.csv'), row.names = F)

## create bau outputs

## 1. no policy (for any policy)
## 2. carbon price: price floor
## 3. oil price: iea oil price
## 4. ccs: medium CCS cost
## 5. innovation: low innovation
## 6. demand: BAU
## 7. refinery: historic exports (edited) 

bau_field_out <- full_field_out %>%
  filter(oil_price_scenario == "iea oil price",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax")

fwrite(bau_field_out, file.path(save_path2, 'bau', 'bau_field_level_prod_rev_2020-2045.csv'), row.names = F)


## create county-level outputs
## -----------------------------------

county_out <- full_field_out %>%
  select(-revenue) %>%
  left_join(field_counties) %>%
  mutate(county_production_bbl = production_bbl * prop_production) %>%
  group_by(county_name, year, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
  summarise(county_production_bbl = sum(county_production_bbl, na.rm = T)) %>%
  ungroup() %>%
  left_join(prices2) %>%
  mutate(revenue = county_production_bbl * oil_price_usd_per_bbl_real) 

fwrite(county_out, file.path(save_path2, 'all-scenarios', 'extraction_county_outputs.csv'), row.names = F)


bau_county_out <- county_out %>%
  filter(oil_price_scenario == "iea oil price",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost",
         setback_scenario == "no_setback",
         prod_quota_scenario == "no quota",
         excise_tax_scenario == "no tax")

fwrite(bau_county_out, file.path(save_path2, 'bau', 'bau_extraction_county_outputs.csv'), row.names = F)


## 2019 bbl production (field and state), 2019 ghg emissions, 2019 brent price
## -------------------------------------------------------------------------------


# load historic production
well_prod_org <- read_rds("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/well_prod_m.rds") %>% as.data.table()

setnames(well_prod_org, "FieldCode", "doc_field_code")
well_prod <- well_prod_org[year == 2019]


wells_19 <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/wells_19.csv") %>%
  select(doc_field_code = FieldCode, FieldName) %>%
  unique() %>%
  as.data.table()

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
  select(-gas) %>%
  filter(total_bbls > 0)

## 2019 brent price
brent_2019 <- prices %>%
  pivot_longer(`EIA Reference case 2020$/b`:`BP oil price 2020$/b`, names_to = "oil_price_scenario", values_to = "oil_price_usd_per_bbl_real") %>%
  mutate(oil_price_scenario = ifelse(oil_price_scenario == "EIA Reference case 2020$/b", "reference case",
                                     ifelse(oil_price_scenario == "EIA High oil price 2020$/b", "high oil price",
                                            ifelse(oil_price_scenario == "EIA Low oil price 2020$/b", "low oil price",
                                                   ifelse(oil_price_scenario == "IEA Delayed Recovery scenario 2020$/b", "iea oil price", "bp oil price"))))) %>%
  rename(year = Year) %>%
  arrange(oil_price_scenario, year) %>%
  filter(year == 2019,
         oil_price_scenario == "reference case") %>%
  select(-oil_price_scenario) %>%
  rename(price = oil_price_usd_per_bbl_real)


## all scenarios
## for all combos

## prod, pollution, revenue
prod_2019 <- well_prod19 %>%
  left_join(field_counties) %>% 
  mutate(county_prod = total_bbls * prop_production) %>%
  left_join(brent_2019) %>%
  select(year, doc_field_code, FieldName, total_bbls, county_name, prop_production, county_prod, oil_price_usd_per_bbl_real = price)


fwrite(prod_2019, file.path(save_path2, 'baseline-2019', 'baseline_field_level_extraction.csv'), row.names = F)








# ## create df with number of new wells by field by year
# ## -------------------------------------------------------
# 
# ## quota and setback new well prod
# 
# create_new_wells_df <- function(file_name) {
#   
#   # out <- readRDS(paste0(extract_vintage_path, file_name))
#   out <- read_feather(paste0(extract_vintage_path, file_name))
#   
#   temp1 <- out[vintage_start == year]
#   
#   temp2 <- temp1[, .(new_wells_pred = sum(n_wells, na.rm = T)), 
#                     by = .(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
#                     ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
#   
#   # new_wells_temp <- out %>%
#   #   filter(vintage_start == year) %>%
#   #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario,
#   #            ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
#   #   ## for now, na.rm = T, but make sure the NAs make sense
#   #   summarise(new_wells_pred = sum(n_wells, na.rm = T)) %>%
#   #   mutate(type = "non-weighted mean forecast") %>%
#   #   left_join(oil_price_df) %>%
#   #   as.data.table()
#   
# }
# 
# field_well_entry <- purrr::map(file_list_vintage, create_new_wells_df) %>%
#   bind_rows()
# 
# fwrite(field_well_entry, file.path(save_path, 'compiled', 'field_new_wells_2020-2045.csv'), row.names = F)
# saveRDS(field_well_entry, paste0(save_path, '/compiled/field_new_wells_2020-2045.rds'))
# 
# ## county level production
# ## --------------------------------
# 
# create_county_df <- function(file_name) {
#   
#   out <- readRDS(paste0(extract_vintage_path, file_name))
#   out <- setDT(read_feather(paste0(extract_vintage_path, file_name)))
#   
#   temp_county <- out[, .(year, doc_field_code, doc_fieldname, vintage_start, oil_price_scenario, innovation_scenario,
#                               carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
#                               production_bbl)]
#   
#   temp_county2 <- temp_county[, .(production_bbl = sum(production_bbl, na.rm = T)), 
#                               by = .(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
#                                      ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
#   
#   temp_county3 <- merge(temp_county2, field_counties, allow.cartesian = T)
#   
#   temp_county4 <- temp_county3[, county_production_bbl := production_bbl * prop_production]
#   
#   temp_county5 <- temp_county4[, .(county_production_bbl = sum(county_production_bbl, na.rm = T)), 
#                                by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario,
#                                       ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year, county_name)]
#   
#   
# }
# 
# 
# county_prod <- purrr::map(file_list_vintage, create_county_df) %>%
#   bind_rows()
# 
# fwrite(county_prod, file.path(save_path, 'compiled', 'county_level_prod_2020-2045.csv'), row.names = F)
# saveRDS(county_prod, paste0(save_path, '/compiled/county_level_prod_2020-2045.rds'))
# 
# print(Sys.time() - start_time)
# 
