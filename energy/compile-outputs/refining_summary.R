## Tracey Mangin
## October 27, 2020
## Summarise refining outputs for scenario selection

library(tidyverse)
library(data.table)
library(rebus)
library(readxl)
library(openxlsx)

## file paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
scen_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
model_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
sselection_path <- paste0(calepa_path, "model-development/scenario-plot/")
extract_path <- paste0(calepa_path, "outputs/predict-production/scenarios_20_all_scens/")

## refinery outputs
refinery_state_out_total_exp <- read_csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_state_total_exports.csv'))
refinery_state_out_net_exp <- read_csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_state_net_exports.csv'))

refinery_site_out_total_exp <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_refinery_total_exports.csv'))
refinery_site_out_net_exp <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_refinery_net_exports.csv'))

refinery_2019 <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_emissions_state_2019.csv'))

## extraction outputs
# extract_out <- read_csv(paste0(extract_path, "summary_output/state_level_extraction_outputs.csv"))

## site ids 
site_id <- read_csv(paste0(calepa_path, "data/stocks-flows/processed/refinery_loc_cap.csv")) %>%
  select(refinery_name, county, site_id)

## prices, usd real 2020
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

## populations
refining_pol <- read_csv(paste0(sselection_path, "refinery_weighted_unweighted_population_fixed.csv")) 

refining_pol2 <- refining_pol %>%
  select(site_id, weighted_population, weighted_dac)

## emissions factors
emissions_factors <- read_csv(paste0(calepa_path, "data/health/processed/CAP_TAC_EmissionsFactors_ss.csv"))

emissions_factors <- janitor::clean_names(emissions_factors) %>%
  filter(activity == "refining",
         pollutant != "Total_TAC_Emissions") %>%
  select(activity:pollutant, emissions_factor_kg)

## labor
labor <- read_csv(paste0(sselection_path, "labor-refine-county-multipliers.csv")) %>%
  janitor::clean_names() %>%
  mutate(activity = "refining") %>%
  select(-x1)


## initial values
## 2019 movement of crude (crude imported), 2019 brent price, 
## crack spread, emissions, pollution, quantity refined products
## -------------------------------------------------------------------------------

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

## crack spread and prices for revenue
crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                       spread = c(23, 20, 23),
                       brent = rep(as.numeric(brent_2019$price[1], 3)),
                       product_price = spread + brent)

## calculate revenue with finished product qs, crack spread, and brent oil price

## make the following a function so that we have two outputs
## -------------------------------------------------------------

make_refinery_outputs <- function(state_df, site_df, inc_exports) {

  state_refinery_df <- state_df
  
  site_refinery_df <- site_df
  
  export_name <- inc_exports
  
  
## finished products from weekly fuel watch
## note: Other Diesel Fuel* includes renewable diesel
fw_df <- read_csv(paste0("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/fuel_watch_data.csv"))

products_q <- fw_df %>%
  filter(year == 2019,
         stock_flow == "Refinery Production") %>%
  group_by(stock_flow, category, sub_cat, year) %>%
  summarise(thous_barrels = sum(thous_barrels, na.rm = T)) %>%
  ungroup() %>%
  filter(category != "Residual") %>%
  mutate(sub_cat = ifelse(category == "Jet Fuel: Kerosene-Naphtha", "Jet fuel", sub_cat),
         category = ifelse(sub_cat == "Other Diesel Fuel*", "Distillates: Other Diesel", 
                           ifelse(category == "Motor Gasoline", paste0(category, " ", sub_cat), category))) %>%
  group_by(category, year) %>%
  summarise(thous_barrels = sum(thous_barrels)) %>%
  ungroup() %>%
  mutate(bbls = thous_barrels * 1000) %>%
  select(-thous_barrels) %>%
  mutate(region = "state")


## 2019 renewable fuel production
rdiesel_19 <- read_csv(paste0(sselection_path, "renewable_diesel_2019.csv"))

renewd <- rdiesel_19 %>%
  select(fuel_type, year, production) %>%
  ## convert gallons to bbls
  mutate(bbls = production / 44) %>%
  select(-production) %>%
  rename(category = fuel_type) %>%
  mutate(region = "North")

bbls_rd <- as.numeric(renewd$bbls)

## get kern production of renewables
renewables_ratio <- read_csv(paste0(model_path, "/stocks-flows/renewable_diesel_credits.csv"))

kern_ratio_df <- renewables_ratio %>%
  filter(lcfs_credit_recipient == "Kern Oil & Refining Co,")

kern_ratio <- as.numeric(kern_ratio_df$credit_ratio)

## adjust fw data -- separate renewable diesel, apply kern ratio (alt air not included)
## ------------------------------

products_q2 <- products_q %>%
  ## subtract renewable diesel from Other Diesel category
  mutate(bbls =  ifelse(category == "Distillates: Other Diesel", bbls - bbls_rd, bbls)) %>%
  ## adjust for ethanol
  mutate(bbls = ifelse(category == "Motor Gasoline Reformulated", bbls * 0.9, bbls)) %>%
  mutate(category = ifelse(category == "Distillates: Other Diesel", "Distillates: Other Diesel Adjusted", category)) %>%
  ## rbind renewable diesel -- gets its own row
  rbind(renewd) %>%
  ## adjust for kern county ratio
  mutate(bbls = ifelse(category == "Renewable Diesel", bbls * kern_ratio, bbls))


## 2019 revenue
revenue_df <- products_q2 %>%
  mutate(product = ifelse(category == "Jet Fuel: Kerosene-Naphtha", "jet_fuel",
                          ifelse(category %in% c("Motor Gasoline Reformulated", "Motor Gasoline Other Finished"), "gasoline", "diesel"))) %>%
  left_join(crack_spread) %>%
  mutate(prod_revenue = bbls * product_price) %>%
  group_by(year) %>%
  summarise(revenue_usd_real = sum(prod_revenue)) %>%
  ungroup() %>%
  mutate(indicator = "revenue_usd_real") %>%
  select(year, indicator, X2019 = revenue_usd_real)

## update labor to be by county
# %>%
#   ungroup() %>%
#   mutate(activity = "refining") 
#   left_join(labor) %>%
#   mutate(employment_total_n = (employment * -1) * (revenue_usd_real / 1e6),
#          employment_compensation = (compensation * -1) * (revenue_usd_real / 1e6)) %>%
#   select(-activity, -employment, -compensation) %>%
#   pivot_longer(revenue_usd_real:employment_compensation, names_to = "indicator", values_to = "X2019")


## calculate crude equivalent with outputs
## ---------------------------------------------------
site_ratios <- read_csv(paste0(model_path, "/stocks-flows/refinery_capacity_ratios.csv"))
intensities  <- read_csv(paste0(model_path, "/stocks-flows/crude_and_refined_products__energy_intensities_and_coefficients.csv"))

## fw by region
region_products <- fw_df %>%
  filter(year == 2019,
         stock_flow == "Refinery Production") %>%
  group_by(region, stock_flow, category, sub_cat, year) %>%
  summarise(thous_barrels = sum(thous_barrels, na.rm = T)) %>%
  ungroup() %>%
  filter(category != "Residual") %>%
  mutate(sub_cat = ifelse(category == "Jet Fuel: Kerosene-Naphtha", "Jet fuel", sub_cat),
         category = ifelse(sub_cat == "Other Diesel Fuel*", "Distillates: Other Diesel",
                           ifelse(category == "Motor Gasoline", paste0(category, " ", sub_cat), category))) %>%
  group_by(region, category, year) %>%
  summarise(thous_barrels = sum(thous_barrels)) %>%
  ungroup() %>%
  mutate(bbls = thous_barrels * 1000) %>%
  select(-thous_barrels)

## adjust
region_products_adj <- region_products %>%
  ## adjust for ethanol
  mutate(bbls = ifelse(category == "Motor Gasoline Reformulated", bbls * 0.9, bbls)) %>%
  ## subtract renewable diesel from Other Diesel category
  mutate(bbls =  ifelse(region == "North" & category == "Distillates: Other Diesel", bbls - bbls_rd, bbls)) %>%
  mutate(category = ifelse(region == "North" & category == "Distillates: Other Diesel", "Distillates: Other Diesel Adjusted", category)) %>%
  ## rbind renewable diesel -- gets its own row
  rbind(renewd) %>%
  ## adjust for kern county ratio
  mutate(bbls = ifelse(category == "Renewable Diesel", bbls * kern_ratio, bbls)) %>%
  mutate(fuel = ifelse(category %in% c("Distillates", "Distillates: Other Diesel Adjusted", "Distillates: Other Diesel", "Renewable Diesel"), "diesel",
                       ifelse(category == "Jet Fuel: Kerosene-Naphtha", "jet_fuel", "gasoline")))

## intensities
intensities2 <- intensities %>%
  select(region, ei_crude_mmbtu_bbl:coef) %>%
  pivot_longer(ei_crude_mmbtu_bbl:coef, names_to = "fuel", values_to = "intensities")

intensity_values <- intensities2 %>%
  filter(fuel != "coef") %>%
  mutate(fuel = ifelse(fuel == "ei_gasoline_mmbtu_bbl", "gasoline",
                       ifelse(fuel == "ei_diesel_mmbtu_bbl", "diesel",
                              ifelse(fuel == "ei_jet_mmbtu_bbl", "jet_fuel", "crude"))))

coef_values <- intensities2 %>%
  filter(fuel == "coef") %>%
  select(-fuel) %>%
  rename(coef = intensities)

vc_values <- intensities2 %>%
  filter(fuel == "ei_crude_mmbtu_bbl") %>%
  select(-fuel) %>%
  rename(crude_intensity = intensities)

crude_eq <- region_products_adj %>%
  left_join(intensity_values) %>%
  mutate(v_x_i = bbls * intensities) %>%
  group_by(region) %>%
  mutate(sum_crude_equiv = sum(v_x_i)) %>%
  ungroup() 

regional_eq <- crude_eq %>%
  select(region, sum_crude_equiv) %>%
  unique() %>%
  left_join(coef_values) %>%
  left_join(vc_values) %>%
  rowwise() %>%
  mutate(sum_crude_eqiuv_bbls = sum_crude_equiv / (crude_intensity - coef)) %>%
  ungroup()

state_crude_eq <- regional_eq %>%
  mutate(year = 2019) %>%
  group_by(year) %>%
  summarise(X2019 = sum(sum_crude_eqiuv_bbls)) %>%
  ungroup() %>%
  mutate(indicator = "crude_equivalent")

## state-level pollution
pollution19 <- state_crude_eq %>%
  mutate(activity = "refining") %>%
  left_join(emissions_factors) %>%
  mutate(pollution_kg = X2019 * emissions_factor_kg) %>%
  group_by(year) %>%
  summarise(X2019 = sum(pollution_kg, na.rm = T)) %>%
  ungroup() %>%
  mutate(indicator = "pollution_kg")


## total crude imported
## CEC data?

## read in the data
refin_clusters <- read_csv("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/reg_refin_crude_receipts.csv")

crude_supply <- refin_clusters %>%
  filter(year == 2019) %>%
  group_by(year, origin) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  mutate(origin = ifelse(origin == 'domestic_sans_ak', "CA", "imported")) %>%
  group_by(year, origin) %>%
  summarise(bbls = sum(bbls, na.rm = T)) %>%
  ungroup() %>%
  filter(origin == "imported") %>%
  mutate(crude_net_movement_mmt = bbls / 1e6) %>%
  select(-origin) %>%
  pivot_longer(crude_net_movement_mmt, names_to = "indicator", values_to = "X2019") %>%
  select(-bbls)


## ghg emissions
ghg_factors = fread(file.path(calepa_path, 'outputs', 'stocks-flows', 'refinery_ghg_factor_x_indiv_refinery.csv'), header = T)
ghg_factors = ghg_factors[year == max(year)]

ghg_factors2 <- ghg_factors %>%
  select(year, region, region_kgco2e_bbl) %>%
  unique()

# ghg19 <- regional_eq %>%
#   left_join(ghg_factors2) %>%
#   mutate(ghg = region_kgco2e_bbl * sum_crude_eqiuv_bbls) %>%
#   group_by(year) %>%
#   summarise(state_kgco2e = sum(ghg)) %>%
#   ungroup() %>%
#   mutate(X2019 = state_kgco2e / (1e6 * 1e3)) %>%
#   mutate(indicator = "ghg_mmt_co2e",
#          year = 2019) %>%
#   select(-state_kgco2e) %>%
#   mutate(boundary = "complete")

## add for all boundaries
## "in-state", "out-of-state"
ghg19 <- refinery_2019 %>%
  select(year, boundary, value) %>%
  mutate(X2019 = value / (1e6 * 1e3)) %>%
  mutate(indicator = "ghg_mmt_co2e") %>%
  select(-value)


## population indicators

site_crude_equiv <- region_products_adj %>%
  left_join(intensity_values) %>%
  left_join(vc_values) %>%
  left_join(coef_values) %>%
  rowwise() %>%
  mutate(crude_equiv_bbls = bbls * intensities / (crude_intensity - coef)) %>%
  select(region, category, year, crude_equiv_bbls) %>%
  mutate(region = ifelse(category == "Renewable Diesel", "Kern", region)) %>%
  group_by(region, year) %>%
  summarise(crude_equiv_bbls = sum(crude_equiv_bbls)) %>%
  ungroup()

kern_df <- site_crude_equiv %>%
  filter(region == "Kern") %>%
  mutate(site_id = 202) %>%
  select(-region) %>%
  rename(rd_crude_equiv_bbls = crude_equiv_bbls)

site_pop1 <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  left_join(site_crude_equiv) %>%
  mutate(site_bbls_equiv = capacity_ratio_within_region * crude_equiv_bbls) %>%
  select(-capacity_ratio_within_region, -crude_equiv_bbls) %>%
  left_join(kern_df) %>%
  pivot_longer(site_bbls_equiv:rd_crude_equiv_bbls, names_to = "cat", values_to = "crude_equiv_bbls") %>%
  group_by(site_id, refinery_name, region, year) %>%
  summarise(site_crude_equiv_bbls = sum(crude_equiv_bbls, na.rm = T)) %>%
  ungroup() %>%
  mutate(activity = "refining") %>%
  left_join(emissions_factors) %>%
  mutate(pollution_kg = site_crude_equiv_bbls * emissions_factor_kg) %>%
  group_by(site_id, refinery_name, region, year) %>%
  summarise(pollution_kg = sum(pollution_kg, na.rm = T)) %>%
  ungroup() 

site_pop2 <- site_pop1 %>%
  left_join(refining_pol2) %>%
  mutate(tot_pop_affected = ifelse(pollution_kg > 0, weighted_population, 0),
         dac_pop_affected = ifelse(pollution_kg > 0, weighted_dac, 0),
         pwee_total_pop = pollution_kg * weighted_population,
         pwee_dac_pop = pollution_kg * weighted_dac) %>%
  group_by(year) %>%
  summarise(tot_pop_affected = sum(tot_pop_affected),
            dac_pop_affected = sum(dac_pop_affected),
            pwee_total_pop = sum(pwee_total_pop),
            pwee_dac_pop = sum(pwee_dac_pop)) %>%
  ungroup() %>%
  mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
  pivot_longer(tot_pop_affected:dac_ratio, names_to = "indicator", values_to = "X2019")

## employment by county
## ----------------------------

## site revenue
region_revenue <- region_products_adj %>%
  left_join(crack_spread, by = c("fuel" = "product")) %>%
  mutate(revenue_usd_real = bbls * product_price) %>%
  select(region, category, year, revenue_usd_real) %>%
  mutate(region = ifelse(category == "Renewable Diesel", "Kern", region)) %>%
  group_by(region, year) %>%
  summarise(revenue_usd_real = sum(revenue_usd_real)) %>%
  ungroup()

kern_rev_df <- region_revenue %>%
  filter(region == "Kern") %>%
  mutate(site_id = 202) %>%
  select(-region) %>%
  rename(rd_revenue_usd_real = revenue_usd_real)

## county_df
county_df <- site_id %>%
  select(-refinery_name)

county_labor <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  left_join(region_revenue) %>%
  mutate(site_rev = capacity_ratio_within_region * revenue_usd_real) %>%
  select(-capacity_ratio_within_region, -revenue_usd_real) %>%
  left_join(kern_rev_df) %>%
  pivot_longer(site_rev:rd_revenue_usd_real, names_to = "cat", values_to = "revenue_usd_real") %>%
  left_join(county_df) %>%
  group_by(county, year) %>%
  summarise(county_revenue_usd_real = sum(revenue_usd_real, na.rm = T)) %>%
  ungroup() %>%
  mutate(activity = "refining") %>%
  mutate(county = ifelse(county == "Solano County", "Solano", county)) %>%
  ##  make sure this works
  left_join(labor) %>%
  mutate(employment_total_n = (employment * -1) * (county_revenue_usd_real / 1e6),
         employment_compensation = (compensation * -1) * (county_revenue_usd_real / 1e6)) %>%
  select(-activity, -employment, -compensation) %>%
  group_by(year) %>%
  summarise(employment_total_n = sum(employment_total_n, na.rm = T),
            employment_compensation = sum(employment_compensation, na.rm = T)) %>%
  pivot_longer(employment_total_n:employment_compensation, names_to = "indicator", values_to = "X2019")


## cumulative ghg
cumul_ghg19 <- tibble(indicator = rep("cumul_ghg_mmt", length(unique(state_refinery_df$boundary))),
                      boundary = unique(state_refinery_df$boundary),
                      X2019 = rep(NA, length(unique(state_refinery_df$boundary))),
                      year = rep(2019, length(unique(state_refinery_df$boundary))))
  

## all boundary options
boundary_df <- expand.grid(year = 2019,
                           boundary = unique(state_refinery_df$boundary))


## all 2019 
indicators_19 <- rbind(pollution19, revenue_df, crude_supply, site_pop2, county_labor) %>%
  left_join(boundary_df) %>%
  rbind(ghg19) %>%
  rbind(cumul_ghg19) %>%
  select(boundary, indicator, X2019)


## 2045
## -----------------------

## all scenarios
demand_scens <- expand.grid(oil_price_scenario = unique(prices2$oil_price_scenario),
                            demand_scenario = unique(state_refinery_df$demand_scenario),
                            refining_scenario= unique(state_refinery_df$refining_scenario),
                            innovation_scenario =  unique(state_refinery_df$innovation_scenario),
                            carbon_price_scenario = unique(state_refinery_df$carbon_price_scenario),
                            ccs_scenario = unique(state_refinery_df$ccs_scenario),
                            year = 2045)



## state revenue and labor outputs
rev_modeled_df <- state_refinery_df %>%
  filter(year == 2045,
         type == "production") %>%
  mutate(product = ifelse(fuel %in% c("gasoline", "drop-in gasoline"), "gasoline",
                          ifelse(fuel %in% c("diesel", "renewable diesel"), "diesel", "jet_fuel")))

rev_modeled_df2 <- left_join(demand_scens, rev_modeled_df) %>%
  left_join(prices2) %>%
  left_join(crack_spread) %>%
  select(-brent, -product_price, -innovation_multiplier) %>%
  mutate(product_price = oil_price_usd_per_bbl_real + spread,
         prod_revenue = value * product_price) %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario, year) %>%
  summarise(revenue_usd_real = sum(prod_revenue)) %>%
  ungroup() %>%
  mutate(indicator = "revenue_usd_real") %>%
  rename(X2045 = revenue_usd_real) %>%
  select(-year)

## state pollution
pollution_modeled <- state_refinery_df %>%
  filter(year == 2045,
         type == "consumption",
         fuel == "crude",
         source == "total")

pollution_modeled2 <- left_join(demand_scens, pollution_modeled) %>%
  mutate(activity = "refining") %>%
  left_join(emissions_factors) %>%
  mutate(pollution_kg = value * emissions_factor_kg * innovation_multiplier) %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario) %>%
  summarise(X2045 = sum(pollution_kg, na.rm = T)) %>%
  ungroup() %>%
  mutate(indicator = "pollution_kg")

## ghg
ghg_modeled <- state_refinery_df %>%
  filter(year == 2045,
         type == "ghg",
         fuel == "crude",
         source == "total")

ghg_modeled2 <- left_join(demand_scens, ghg_modeled) %>%
  mutate(X2045 = value / (1e6 * 1e3)) %>%
  mutate(indicator = "ghg_mmt_co2e") %>%
  select(-year, -fuel, -source, - units, -value, -type, -innovation_multiplier)

## cumulative ghg
cumul_ghg <- demand_scens %>%
  select(-year) %>%
  left_join(state_refinery_df) %>%
  filter(type == "ghg",
         fuel == "crude",
         source == "total") %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, boundary) %>%
  summarise(cumul_ghg_kg = sum(value)) %>%
  ungroup() %>%
  mutate(X2045 = cumul_ghg_kg / (1e6 * 1e3),
         indicator = "cumul_ghg_mmt") %>%
  select(-cumul_ghg_kg)

## populations indicators

pops_modeled <- site_refinery_df %>%
  filter(year == 2045,
         type == "consumption",
         fuel == "crude",
         source == "total") %>%
  mutate(site_id =  as.numeric(str_extract(site_id, pattern = START %R% one_or_more(DGT))))

pops_modeled2 <- left_join(demand_scens, pops_modeled) %>%
  mutate(activity = "refining") %>%
  left_join(emissions_factors) %>%
  mutate(pollution_kg = value * emissions_factor_kg * innovation_multiplier) %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario, site_id) %>%
  summarise(pollution_kg = sum(pollution_kg, na.rm = T)) %>%
  ungroup() %>%
  left_join(refining_pol2) %>%
  mutate(tot_pop_affected = ifelse(pollution_kg > 0, weighted_population, 0),
         dac_pop_affected = ifelse(pollution_kg > 0, weighted_dac, 0),
         pwee_total_pop = pollution_kg * weighted_population,
         pwee_dac_pop = pollution_kg * weighted_dac) %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario) %>%
  summarise(tot_pop_affected = sum(tot_pop_affected),
            dac_pop_affected = sum(dac_pop_affected),
            pwee_total_pop = sum(pwee_total_pop),
            pwee_dac_pop = sum(pwee_dac_pop)) %>%
  ungroup() %>%
  mutate(dac_ratio = dac_pop_affected / tot_pop_affected) %>%
  pivot_longer(tot_pop_affected:dac_ratio, names_to = "indicator", values_to = "X2045")

## labor
labor_modeled <- site_refinery_df %>%
  filter(year == 2045,
         type == "production") %>%
  mutate(product = ifelse(fuel %in% c("gasoline", "drop-in gasoline"), "gasoline",
                          ifelse(fuel %in% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))) %>%
  mutate(site_id =  as.numeric(str_extract(site_id, pattern = START %R% one_or_more(DGT)))) %>%
  left_join(county_df) %>%
  mutate(county = ifelse(site_id == "99999", "Kern", county)) %>%
  left_join(prices2) %>%
  left_join(crack_spread) %>%
  select(-brent, - product_price) %>%
  mutate(product_price = oil_price_usd_per_bbl_real + spread,
         prod_revenue = value * product_price) %>%
  group_by(year, oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, county) %>%
  summarise(prod_revenue = sum(prod_revenue)) %>%
  ungroup() %>%
  mutate(activity = "refining") %>%
  mutate(county = ifelse(county == "Solano County", "Solano", county)) %>%
  ##  make sure this works
  left_join(labor) %>%
  mutate(employment_total_n = (employment * -1) * (prod_revenue / 1e6),
         employment_compensation = (compensation * -1) * (prod_revenue / 1e6)) %>%
  select(-activity, -employment, -compensation, -prod_revenue) %>%
  group_by(demand_scenario, refining_scenario, oil_price_scenario, innovation_scenario, carbon_price_scenario,
           ccs_scenario) %>%
  summarise(employment_total_n = sum(employment_total_n, na.rm = T),
            employment_compensation = sum(employment_compensation, na.rm = T)) %>%
  ungroup() %>%
  pivot_longer(employment_total_n:employment_compensation, names_to = "indicator", values_to = "X2045") 



## movement -- make na for now... need extraction
movement_19 <- demand_scens %>%
  mutate(indicator = "crude_net_movement_mmt",
         X2045 = NA) %>%
  select(-year)

## stitch indicators together

boundary_df2 <- expand.grid(indicator = unique(indicators_19$indicator),
                            boundary = unique(state_refinery_df$boundary))


all_modeled <- rbind(pollution_modeled2, rev_modeled_df2, pops_modeled2, movement_19, labor_modeled) %>%
  left_join(boundary_df2) %>%
  rbind(ghg_modeled2) %>%
  rbind(cumul_ghg) %>%
  left_join(indicators_19) %>%
  select(demand_scenario:ccs_scenario, boundary, indicator, X2019, X2045) %>%
  rowwise() %>%
  mutate(diff = X2045 - X2019,
         rel_change = diff / X2019) %>%
  ungroup() %>%
  as.data.table()



## save for use in scenario outputs
write_csv(all_modeled, paste0(sselection_path, "ss_refinery_outputs/state_level_refining_outputs", export_name, ".csv"))

## save for storage
write_csv(all_modeled, paste0(extract_path, "summary_output/state_level_refining_outputs", export_name, ".csv"))


return(all_modeled)

## save 
# write_csv(all_modeled, paste0(extract_path, "summary_output/state_level_refining_outputs.csv"))

}


## save two versions
net_df <-  make_refinery_outputs(state_df = refinery_state_out_net_exp, site_df = refinery_site_out_net_exp, inc_exports = "_net_exports")
total_df <- make_refinery_outputs(state_df = refinery_state_out_total_exp, site_df = refinery_site_out_total_exp, inc_exports = "_total_exports")

# test <- read_csv(paste0(sselection_path, "state_level_refining_outputs.csv")) %>%
#   rename(orig_rel_change = rel_change,
#          orig_x2019 = X2019,
#          orig_x2045 = X2045,
#          orig_diff = diff) %>%
#   left_join(total_df) %>%
#   mutate(diff2 = orig_rel_change - rel_change)
# 
# 
# test2 <- read_csv(paste0(sselection_path, "state_level_refining_outputs.csv")) %>%
#   rename(orig_rel_change = rel_change) %>%
#   left_join(net_df) %>%
#   mutate(diff2 = orig_rel_change - rel_change)




