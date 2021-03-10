## Tracey Mangin
## October 24, 2020
## Summarise extraction outputs for scenario selection


library(tidyverse)
library(data.table)
library(feather)
# library(plotly)
library(readxl)
library(openxlsx)

## file names
# histprod_file   = 'crude_prod_x_field.csv'
ghg_file        = 'ghg_emissions_x_field_2015.csv'

## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"

extract_path <- paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/")

extract_vintage_path <- paste0(extract_path, "/scen_outputs/vintage/")
extract_summary_path <- paste0(extract_path, "/scen_outputs/summary/")
model_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
sselection_path <- paste0(calepa_path, "model-development/scenario-plot/")
scen_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'

# # Start time
# start_time <- Sys.time()

## scenarios
scenarios_dt <- read_feather('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs/input_variables_scenarios.csv')

## keep scenarios and innovation multiplier for 2045
scenarios_inno <- scenarios_dt %>%
  filter(year == 2045) %>%
  select(year:excise_tax_scenario, innovation_multiplier) %>%
  unique()


## read in county-level and field level
county_outputs <- read_csv(paste0(model_path, "/energy-model-outputs/all-scenarios/extraction_county_outputs.csv"))
field_outputs <- read_csv(paste0(model_path, "/energy-model-outputs/all-scenarios/field_level_prod_rev_2020-2045.csv"))

## county df
county_file <- 'annual_field_county_production_proportion.csv'
field_counties = fread(file.path(model_path, 'stocks-flows', county_file), header = T)

## clear field counties
field_counties = field_counties[, oil_prod := NULL]
field_counties = field_counties[, FieldName := NULL]
field_counties[, FieldCode := sprintf("%03d", FieldCode)]
setnames(field_counties, "FieldCode", "doc_field_code")

field_counties <- field_counties %>%
  filter(!is.na(prop_production)) %>%
  group_by(doc_field_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(-year) %>%
  as.data.table() 

## prices
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

# # load field-county matches 
# county_file <- 'annual_field_county_production_proportion.csv'
# field_counties = fread(file.path(model_path, 'stocks-flows', county_file), header = T)
# field_counties = field_counties[, oil_prod := NULL]
# field_counties = field_counties[, FieldName := NULL]
# field_counties[, FieldCode := sprintf("%03d", FieldCode)]
# setnames(field_counties, "FieldCode", "doc_field_code")
# 
# field_counties <- field_counties %>%
#   filter(!is.na(prop_production)) %>%
#   group_by(doc_field_code) %>%
#   filter(year == max(year)) %>%
#   ungroup() %>%
#   select(-year) %>%
#   as.data.table()


## populations
extract_pol <- read_csv(paste0(sselection_path, "extraction_wighted_unweighted_population_oct27.csv")) %>%
  mutate(field_code = str_sub(paste0("00", field_code), start = -3)) 

extract_pol2 <- extract_pol %>%
  select(field_code, weighted_population, weighted_dac)

## emissions factors
emissions_factors <- read_csv(paste0(calepa_path, "data/health/processed/CAP_TAC_EmissionsFactors_ss.csv"))

emissions_factors <- janitor::clean_names(emissions_factors) %>%
  filter(activity == "extraction",
         pollutant != "Total_TAC_Emissions") %>%
  select(activity:pollutant, emissions_factor_kg)

## labor
labor <- read_csv(paste0(sselection_path, "labor-extraction-county-multipliers.csv")) %>%
  mutate(activity = "extraction") %>%
  select(-X1)


## summary output
# summary_out <- readRDS(paste0(extract_path, "summary_outputs_2020-2045.rds"))
summary_out <- read_feather(paste0(extract_path, "summary_outputs_2020-2045.csv"))

## initial values
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
  select(-gas)

prod_hist19 <- well_prod19 %>%
  group_by(year) %>%
  summarise(production_bbl = sum(total_bbls)) %>%
  ungroup()
  

## ghg emissions
ghg_factors = fread(file.path(calepa_path, 'outputs', 'stocks-flows', ghg_file), header = T)
ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]
ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]

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

## for bounding
# all_scens <- summary_out %>%
#   select(oil_price_scenario:excise_tax_scenario) %>%
#   unique() %>%
#   mutate(year = 2019)

## for all combos
all_scens <- expand.grid(oil_price_scenario = unique(summary_out$oil_price_scenario),
                         innovation_scenario =  unique(summary_out$innovation_scenario),
                         carbon_price_scenario = unique(summary_out$carbon_price_scenario),
                         ccs_scenario = unique(summary_out$ccs_scenario),
                         setback_scenario = unique(summary_out$setback_scenario),
                         prod_quota_scenario = unique(summary_out$prod_quota_scenario),
                         excise_tax_scenario = unique(summary_out$excise_tax_scenario),
                         year = 2019)

## prod, pollution, revenue
init_df1 <- prod_hist19 %>%
  mutate(activity = "extraction") %>%
  left_join(emissions_factors) %>%
  ## calculate pollution metric
  mutate(loc_pollution = emissions_factor_kg * production_bbl) %>%
  group_by(year, activity, production_bbl) %>%
  summarise(pollution_kg = sum(loc_pollution)) %>%
  ungroup() %>%
  left_join(brent_2019) %>%
  mutate(revenue_usd_real = production_bbl * price,
         cumul_ghg_mmt = NA) %>%
  select(-activity) %>%
  pivot_longer(production_bbl:cumul_ghg_mmt, names_to = "indicator", values_to = "X2019")


## dac
init_df2 <- well_prod19 %>%
  mutate(activity = "extraction") %>%
  left_join(emissions_factors) %>%
  mutate(emission = emissions_factor_kg * total_bbls) %>%
  group_by(doc_field_code, year, total_bbls) %>%
  summarise(emission_kg = sum(emission, na.rm = T)) %>%
  ungroup() %>%
  left_join(extract_pol2, by = c("doc_field_code" = "field_code")) %>%
  mutate(tot_pop_affected = ifelse(total_bbls > 0, weighted_population, 0),
         dac_pop_affected = ifelse(total_bbls > 0, weighted_dac, 0),
         pwee_total_pop = emission_kg * weighted_population,
         pwee_dac_pop = emission_kg * weighted_dac) %>%
  group_by(year) %>%
  summarise(tot_pop_affected = sum(tot_pop_affected),
            dac_pop_affected = sum(dac_pop_affected),
            pwee_total_pop = sum(pwee_total_pop),
            pwee_dac_pop = sum(pwee_dac_pop)) %>%
  ungroup() %>%
  mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
  pivot_longer(tot_pop_affected:dac_ratio, names_to = "indicator", values_to = "X2019")
  
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

## labor (county)
## ----------------------------------

init_df4 <- well_prod19 %>%
  filter(total_bbls > 0) %>%
  left_join(field_counties) %>% 
  rename(county = county_name) %>%
  mutate(county = ifelse(county == "Los Angeles Offshore", "Los Angeles",
                         ifelse(county == "Orange Offshore", "Orange",
                                ifelse(county == "Ventura Offshore", "Ventura", county)))) %>%
  mutate(county_prod = total_bbls * prop_production) %>%
  group_by(year, county) %>%
  summarise(county_prod = sum(county_prod)) %>%
  ungroup() %>%
  left_join(brent_2019) %>%
  mutate(revenue_usd_real = county_prod * price) %>%
  mutate(activity = "extraction") %>%
  left_join(labor) %>%
  select(-activity) %>%
  mutate(employment_total_n = (employment * -1) * (revenue_usd_real / 1e6),
         employment_compensation = (compensation * -1) * (revenue_usd_real / 1e6)) %>%
  group_by(year) %>%
  summarise(employment_total_n = sum(employment_total_n),
            employment_compensation = sum(employment_compensation)) %>%
  ungroup() %>%
  pivot_longer(employment_total_n:employment_compensation, names_to = "indicator", values_to = "values") %>%
  mutate(year = paste0("X", year)) %>%
  pivot_wider(names_from = "year", values_from = "values") %>%
  mutate(year = 2019)
  

## init df final
init_df_all <- rbind(init_df1, init_df2, init_df3, init_df4)

init_df_final <- left_join(all_scens, init_df_all) %>%
  select(-year)
  
## now 2045, join to init
## ---------------------------------------------------------

## 
summary_out2 <- summary_out %>%
  group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, 
           prod_quota_scenario, excise_tax_scenario) %>%
  mutate(cumul_ghg_mmt = sum(ghg_mmt_inno_ccs_adj)) %>%
  ungroup() %>%
  filter(year == 2045) %>%
  select(oil_price_scenario:excise_tax_scenario, oil_price_usd_per_bbl, year, total_prod, ghg_mmt_inno_ccs_adj, cumul_ghg_mmt) %>%
  rename(ghg_mmt = ghg_mmt_inno_ccs_adj) %>%
  left_join(prices2) %>%
  mutate(revenue_usd_real = oil_price_usd_per_bbl_real * total_prod) %>%
  select(oil_price_scenario:excise_tax_scenario, year, production_bbl = total_prod, ghg_mmt, revenue_usd_real, cumul_ghg_mmt) %>%
  pivot_longer(production_bbl:cumul_ghg_mmt, names_to = "indicator", values_to = "values") %>%
  # select(oil_price_scenario:oil_price_usd_per_bbl, indicator, year, values) %>%
  mutate(year = paste0("X", year)) %>%
  pivot_wider(names_from = "year", values_from = "values") %>%
  left_join(init_df_final) %>%
  rowwise() %>%
  mutate(diff = X2045 - X2019,
          rel_change = diff / X2019) %>%
  ungroup() %>%
  select(oil_price_scenario, innovation_scenario, carbon_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
  as.data.table() 

## pollution

inno_mult <-summary_out %>%
  filter(year == 2045) %>%
  select(oil_price_scenario:excise_tax_scenario)

pollution_df <- summary_out2 %>%
  select(oil_price_scenario:indicator, X2045) %>%
  pivot_longer(X2045, names_to = "year", values_to = "values") %>%
  filter(indicator == "production_bbl") %>%
  mutate(activity = "extraction") %>%
  left_join(emissions_factors) %>%
  left_join(inno_mult) %>%
  mutate(emission = emissions_factor_kg * values * innovation_multiplier) %>%
  group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
           excise_tax_scenario, year) %>%
  summarise(emission_kg = sum(emission, na.rm = T)) %>%
  ungroup() %>%
  mutate(indicator = "pollution_kg") %>%
  pivot_wider(names_from = year, values_from = emission_kg) %>%
  left_join(init_df_final) %>%
  rowwise() %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  ungroup() %>%
  select(oil_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
  as.data.table()



# ## employment -- calculate 2020 and 2045 number of jobs
# jobs_df1 <- summary_out2 %>%
#   select(oil_price_scenario:indicator, X2045) %>%
#   pivot_longer(X2045, names_to = "year", values_to = "values") %>%
#   filter(indicator == "revenue_usd_real") %>%
#   mutate(activity = "extraction") %>%
#   left_join(labor) %>%
#   select(-activity) %>%
#   mutate(employment_total_n = (employment * -1) * (values / 1e6),
#          employment_compensation = (compensation * -1) * (values / 1e6)) %>%
#   select(-employment, -compensation, -indicator, - values) %>%
#   pivot_longer(employment_total_n:employment_compensation, names_to = "indicator", values_to = "values") %>%
#   pivot_wider(names_from = "year", values_from = "values") %>%
#   left_join(init_df_final) %>%
#   rowwise() %>%
#   mutate(diff = X2045 - X2019,
#          rel_change = diff / X2019) %>%
#   ungroup() %>%
#   select(oil_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
#   as.data.table()

## make sure this works

jobs_df2 <- county_outputs %>%
  filter(year == 2045) %>%
  select(county_name:excise_tax_scenario, revenue) %>%
  mutate(activity = "extraction") %>%
  rename(county = county_name) %>%
  mutate(county = ifelse(county == "Los Angeles Offshore", "Los Angeles",
                         ifelse(county == "Orange Offshore", "Orange",
                                ifelse(county == "Ventura Offshore", "Ventura", county)))) %>%
  left_join(labor) %>%
  mutate(employment_total_n = (employment * -1) * (revenue / 1e6),
         employment_compensation = (compensation * -1) * (revenue / 1e6)) %>%
  group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
  summarise(employment_total_n = sum(employment_total_n, na.rm = T),
            employment_compensation = sum(employment_compensation, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(employment_total_n:employment_compensation, names_to = "indicator", values_to = "values") %>%
  mutate(year = paste0("X", year)) %>%
  pivot_wider(names_from = "year", values_from = "values") %>%
  left_join(init_df_final) %>%
  rowwise() %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  ungroup() %>%
  select(oil_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
  as.data.table()


## ---------------------------------------------------------
## create DAC outputs for scenario selection

dac_outputs <- field_outputs %>%
  filter(year == 2045) %>%
  left_join(scenarios_inno) %>%
  group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
  summarise(production_bbl = sum(production_bbl,  na.rm = T)) %>%
  ungroup() %>%
  mutate(activity = "extraction") %>%
  left_join(emissions_factors) %>%
  mutate(emission = emissions_factor_kg * production_bbl * innovation_multiplier) %>%
  group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
           excise_tax_scenario, year, production_bbl) %>%
  summarise(emission_kg = sum(emission, na.rm = T)) %>%
  ungroup() %>%
  left_join(extract_pol2, by = c("doc_field_code" = "field_code")) %>%
  mutate(tot_pop_affected = ifelse(production_bbl > 0, weighted_population, 0),
         dac_pop_affected = ifelse(production_bbl > 0, weighted_dac, 0),
         pwee_total_pop = emission_kg * weighted_population,
         pwee_dac_pop = emission_kg * weighted_dac) %>%
  group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
           excise_tax_scenario, year) %>%
  summarise(tot_pop_affected = sum(tot_pop_affected),
            dac_pop_affected = sum(dac_pop_affected),
            pwee_total_pop = sum(pwee_total_pop),
            pwee_dac_pop = sum(pwee_dac_pop)) %>%
  ungroup() %>%
  mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
  pivot_longer(tot_pop_affected:dac_ratio, names_to = "indicator", values_to = "values") %>%
  mutate(year = paste0("X", year)) %>%
  pivot_wider(names_from = "year", values_from = "values") %>%
  left_join(init_df2) %>%
  select(-year) %>%
  rowwise() %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  ungroup() %>%
  select(oil_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
  as.data.table()



# file_list <- list.files(extract_vintage_path)
# 
# 
# ############################
# 
# ## create DAC outputs for scenario selection
# ## ----------------------------
# 
# 
# create_scen_select_df <- function(file_name) {
# 
#   # out <- readRDS(paste0(extract_vintage_path, file_name))
#   # out <- fread(file.path(extract_vintage_path, file_name))
#   out <- read_feather(paste0(extract_vintage_path, file_name))
#   
#   # out[, doc_field_code := sprintf("%03d", doc_field_code)]
#   
#   dac_outputs <- out %>%
#     filter(year == 2045) %>%
#     group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario,
#              setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
#     summarise(production_bbl = sum(production_bbl,  na.rm = T)) %>%
#     ungroup() %>%
#     mutate(activity = "extraction") %>%
#     left_join(emissions_factors) %>%
#     mutate(emission = emissions_factor_kg * production_bbl * innovation_multiplier) %>%
#     group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
#              excise_tax_scenario, year, production_bbl) %>%
#     summarise(emission_kg = sum(emission, na.rm = T)) %>%
#     ungroup() %>%
#     left_join(extract_pol2, by = c("doc_field_code" = "field_code")) %>%
#     mutate(tot_pop_affected = ifelse(production_bbl > 0, weighted_population, 0),
#            dac_pop_affected = ifelse(production_bbl > 0, weighted_dac, 0),
#            pwee_total_pop = emission_kg * weighted_population,
#            pwee_dac_pop = emission_kg * weighted_dac) %>%
#     group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
#              excise_tax_scenario, year) %>%
#     summarise(tot_pop_affected = sum(tot_pop_affected),
#               dac_pop_affected = sum(dac_pop_affected),
#               pwee_total_pop = sum(pwee_total_pop),
#               pwee_dac_pop = sum(pwee_dac_pop)) %>%
#     ungroup() %>%
#     mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
#     pivot_longer(tot_pop_affected:dac_ratio, names_to = "indicator", values_to = "values") %>%
#     mutate(year = paste0("X", year)) %>%
#     pivot_wider(names_from = "year", values_from = "values") %>%
#     left_join(init_df2) %>%
#     select(-year) %>%
#     rowwise() %>%
#     mutate(diff = X2045 - X2019,
#            rel_change = diff / X2019) %>%
#     ungroup() %>%
#     select(oil_price_scenario:indicator, X2019, X2045, diff, rel_change) %>%
#     as.data.table()
#   
# }
# 
# dac_out_extraction <- purrr::map(file_list, create_scen_select_df) %>%
#   bind_rows()

## bind all relevant dfs together, fill cumulative ghg
# 
# all_scen_combos <- expand.grid(oil_price_scenario = unique(summary_out3$oil_price_scenario),
#                                innovation_scenario =  unique(summary_out3$innovation_scenario),
#                                carbon_price_scenario = unique(summary_out3$carbon_price_scenario),
#                                ccs_scenario = unique(summary_out3$ccs_scenario),
#                                setback_scenario = unique(summary_out3$setback_scenario),
#                                prod_quota_scenario = unique(summary_out3$prod_quota_scenario),
#                                excise_tax_scenario = unique(summary_out3$excise_tax_scenario),
#                                indicator = c(unique(summary_out3$indicator), unique(pollution_df$indicator),
#                                              unique(dac_out_extraction$indicator), unique(jobs_df2$indicator)))

# finala <- all_scen_combos %>%
#   filter(indicator %in% unique(summary_out3$indicator)) %>%
#   left_join(summary_out3)
# 
# finalb <- all_scen_combos %>%
#   filter(indicator %in% unique(pollution_df$indicator)) %>%
#   left_join(pollution_df)
# 
# finalc <- all_scen_combos %>%
#   filter(indicator %in% unique(jobs_df2$indicator)) %>%
#   left_join(jobs_df2)
# 
# 
# finald <- all_scen_combos %>%
#   filter(indicator %in% unique(dac_out_extraction$indicator)) %>%
#   left_join(dac_out_extraction)


## bind them


## combine
extract_sselection_out <- rbind(summary_out2, pollution_df, dac_outputs, jobs_df2) %>%
  mutate(indicator = ifelse(indicator == "total_prod", "production_bbl",
                            ifelse(indicator == "ghg_mmt", "ghg_mmt_co2e", indicator))) %>%
  arrange(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
  as.data.table() 


## save for use in scenario outputs
write_csv(extract_sselection_out, paste0(extract_path, "summary_output/state_level_extraction_outputs.csv"))



