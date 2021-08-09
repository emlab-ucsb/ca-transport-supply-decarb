## Tracey Mangin
## August 6, 2021
## Site-level refining outputs by site for 2019

## libraries
library(data.table)  
library(tidyverse)
library(openxlsx)

## output folder
refining_folder  = file.path("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/")

## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
data_path  <-'data/stocks-flows/processed'
outputs_path <- 'model-development/scenario-plot/refinery-outputs'

## files
oil_price_file <- 'oil_price_projections_revised.xlsx'
refining_file <- 'refining_scenario_outputs_refinery_net_exports_revised.csv'


## read in files
## ---------------------------------------

## oil prices
oilpx_scens_real = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1, 7:9)))
colnames(oilpx_scens_real) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens_real = melt(oilpx_scens_real, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                        variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens_real[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens_real[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens_real <- oilpx_scens_real[year == 2019]
oilpx_real_2019 <- unique(oilpx_scens_real[, .(year, oil_price_usd_per_bbl)])


## refining outputs
refining_out <- fread(file.path(main_path, outputs_path, refining_file))

## site ids 
site_id <- fread(paste0(main_path, "/data/stocks-flows/processed/refinery_loc_cap_manual.csv"), colClasses = c("site_id" = "character")) 

## capacities
renewable_capacity <- setDT(read.xlsx(paste0(main_path, "/data/stocks-flows/processed/renewable_refinery_capacity.xlsx")))


alt_air_capacity <- setDT(read.xlsx(paste0(main_path, "/data/stocks-flows/raw/altair_refinery_capacity.xlsx")))



##
## crack spread and prices for revenue
crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                       spread = c(23, 21, 23),
                       brent = rep(as.numeric(oilpx_real_2019$oil_price_usd_per_bbl[1])),
                       product_price = spread + brent)


## state emissions for 2019
## ***NOTE: needs to be rerun (and possibly updated), made in refining_ghg_emissions_boundary.R
refinery_2019 <- read.csv(file.path(main_path, outputs_path, 'refining_emissions_state_2019.csv'))

## finished products from weekly fuel watch
## note: Other Diesel Fuel* includes renewable diesel
fw_df <- read_csv(paste0("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/fuel_watch_data.csv"))

## 2019 state-wide renewable fuel production (made in renewables.R) -- 
## note: maybe change this
rdiesel_19 <- read_csv(file.path(main_path, "model-development/scenario-plot/renewable_diesel_2019.csv"))

## get kern and alt air production of renewables (made in kern_renewable_d.R)
renewables_ratio <- read_csv(paste0(main_path, "/outputs/stocks-flows/renewable_diesel_credits.csv"))

## site ratios (created in refinery_module_net.R)
site_ratios <- read_csv(paste0(main_path, "/outputs/stocks-flows/refinery_capacity_ratios.csv"))

## for crude equivalent (created in refinery_module_net.R)
intensities  <- read_csv(paste0(main_path, "/outputs/stocks-flows/crude_and_refined_products__energy_intensities_and_coefficients.csv"))


## finished products by region: use fuel watch data
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

## prepare renewable diesel info for binding with regional production
renewd <- rdiesel_19 %>%
  select(fuel_type, year, production) %>%
  ## convert gallons to bbls
  mutate(bbls = production / 44) %>%
  select(-production) %>%
  rename(category = fuel_type)
# %>%
#   mutate(region = "North")

## determine ratio of renewable production occurring at Kern Oil
kern_ratio_df <- renewables_ratio %>%
  filter(lcfs_credit_recipient == "Kern Oil & Refining Co,")

kern_ratio <- as.numeric(kern_ratio_df$credit_ratio)

## calculate bbls of renewable diesel by region
bbls_rd <- as.numeric(renewd$bbls)
bbls_rd_kern <- bbls_rd * kern_ratio
bbls_rd_altair <- bbls_rd * (1 - kern_ratio)

## create renewable diesel tibble for binding later
renewd_bind <- tibble(region = c("North", "South"),
                      category = rep("Renewable Diesel", 2),
                      year = rep(2019, 2),
                      bbls = c(bbls_rd_kern, bbls_rd_altair))

## adjust regional production for ethanol and renewable diesel production from Kern Oil
region_products_adj <- region_products %>%
  ## adjust for ethanol
  mutate(bbls = ifelse(category == "Motor Gasoline Reformulated", bbls * 0.9, bbls)) %>%
  ## subtract renewable diesel (bbls_rd) from Other Diesel category
  mutate(bbls =  ifelse(region == "North" & category == "Distillates: Other Diesel", bbls - bbls_rd_kern, 
                        ifelse(region == "South" & category == "Distillates: Other Diesel", bbls - bbls_rd_altair, bbls))) %>%
  mutate(category = ifelse(category == "Distillates: Other Diesel", "Distillates: Other Diesel Adjusted", category)) %>%
  ## rbind renewable diesel -- gets its own row
  rbind(renewd_bind) %>%
  mutate(fuel = ifelse(category %in% c("Distillates", "Distillates: Other Diesel Adjusted", "Distillates: Other Diesel", "Renewable Diesel"), "diesel",
                       ifelse(category == "Jet Fuel: Kerosene-Naphtha", "jet_fuel", "gasoline")))


## break down into site-level production, site-level crude consumption, add prices, add counties
## ----------------------------

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

## site-level crude equivalent

## county_df
county_df <- site_id %>%
  select(site_id, county)

site_crude_equiv <- region_products_adj %>%
  left_join(intensity_values) %>%
  left_join(vc_values) %>%
  left_join(coef_values) %>%
  rowwise() %>%
  mutate(crude_equiv_bbls = bbls * intensities / (crude_intensity - coef)) %>%
  ungroup() %>%
  select(region, category, year, crude_equiv_bbls) %>%
  mutate(region = ifelse(category == "Renewable Diesel" & region == "North", "Kern", 
                         ifelse(category == "Renewable Diesel" & region == "South", "Paramount", region))) %>%
  group_by(region, year) %>%
  summarise(crude_equiv_bbls = sum(crude_equiv_bbls)) %>%
  ungroup()

kern_altair_df <- site_crude_equiv %>%
  filter(region %in% c("Kern", "Paramount"))%>%
  mutate(site_id = ifelse(region == "Kern", "202", "t-800")) %>%
  select(-region) %>%
  rename(rd_crude_equiv_bbls = crude_equiv_bbls)

## create t-800 row, bind
alt_air_df <- tibble(site_id = "t-800",
                     refinery_name = "AltAir Paramount",
                     region = "South",
                     capacity_ratio_within_region = 0)

## here
site_crude_equiv2 <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  rbind(alt_air_df) %>%
  left_join(site_crude_equiv) %>%
  mutate(site_bbls_equiv = capacity_ratio_within_region * crude_equiv_bbls) %>%
  select(-capacity_ratio_within_region, -crude_equiv_bbls) %>%
  left_join(kern_altair_df) %>%
  pivot_longer(site_bbls_equiv:rd_crude_equiv_bbls, names_to = "cat", values_to = "crude_equiv_bbls") %>%
  group_by(site_id, refinery_name, region, year) %>%
  summarise(site_crude_equiv_bbls = sum(crude_equiv_bbls, na.rm = T)) %>%
  ungroup() %>%
  mutate(type = "consumption",
         fuel = "crude_equivalent") %>%
  rename(value_bbls = site_crude_equiv_bbls)
  
## balance
missing_sites <- refining_out %>% 
  select(site_id, refinery_name, region) %>%
  unique() %>%
  filter(!site_id %in% site_crude_equiv2$site_id) %>%
  mutate(year = 2019,
         value_bbls = 0,
         type = "consumption",
         fuel = "crude_equivalent") 

site_crude_equiv3 <- site_crude_equiv2 %>%
  rbind(missing_sites) %>%
  left_join(county_df) %>%
  mutate(county = ifelse(site_id == "342-2", "Contra Costa",
                         ifelse(site_id == "99999", "Kern",
                                 ifelse(site_id == "t-800", "Los Angeles", county))))

## save consumption output, 2019
## -----------------------------
refining_site19_fname = paste0('site_refining_outputs_2019.csv')
fwrite(site_crude_equiv3, file.path(main_path, outputs_path, refining_site19_fname), row.names = F)
print(paste0('2019 refining site outputs saved to ', refining_site19_fname))




## county level revenue
## --------------------------------


## site-level production
region_products_adj2 <- region_products_adj %>%
  mutate(region = ifelse(category == "Renewable Diesel" & region == "North", "Kern",
                         ifelse(category == "Renewable Diesel" & region == "South", "Paramount", region))) 

kern_altair_prod <- region_products_adj2 %>%
  filter(region %in% c("Kern", "Paramount")) %>%
  mutate(site_id = ifelse(region == "Kern", "202", "t-800"),
         refinery_name = ifelse(region == "Kern", "Kern Oil & Refining Company, Bakersfield Refinery", "AltAir Paramount"),
         site_prod = bbls) 


site_production <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  rbind(alt_air_df) %>%
  left_join(region_products_adj2) %>%
  mutate(site_prod = capacity_ratio_within_region * bbls) %>%
  select(-capacity_ratio_within_region) %>%
  rbind(kern_altair_prod) %>%
  select(-bbls) %>%
  rename(value_bbls = site_prod) %>%
  left_join(crack_spread, by = c("fuel" = "product")) %>%
  left_join(county_df) %>%
  mutate(county = ifelse(site_id == "t-800", "Los Angeles", county)) %>%
  mutate(type = "production") %>%
  select(site_id, refinery_name, county, region, year, type, category, fuel, value_bbls, brent, spread, product_price)


## summarise by county, add missing counties (if any are missing)
county_revenue <- site_production %>%
  mutate(revenue = value_bbls * product_price) %>%
  group_by(county, year) %>%
  summarise(revenue = sum(revenue)) %>%
  ungroup()



## save county output, 2019
## -----------------------------
refining_county19_fname = paste0('county_refining_outputs_2019.csv')
fwrite(county_revenue, file.path(main_path, outputs_path, refining_county19_fname), row.names = F)
print(paste0('2019 refining county outputs saved to ', refining_county19_fname))


# ## (meas) region/cluster level crude equiv by fuel
# region_crude_equiv <- region_products_adj %>%
#   left_join(intensity_values) %>%
#   left_join(vc_values) %>%
#   left_join(coef_values) %>%
#   rowwise() %>%
#   mutate(crude_equiv_bbls = bbls * intensities / (crude_intensity - coef)) %>%
#   ungroup() %>%
#   select(region, category, year, crude_equiv_bbls) %>%
#   mutate(region = ifelse(category == "Renewable Diesel", "Kern", region)) %>%
#   group_by(region, year)

# fwrite(region_crude_equiv, file.path(save_path2, 'baseline-2019', 'baseline_region_level_crude_equiv.csv'), row.names = F)
# 
# 
# 
# 
