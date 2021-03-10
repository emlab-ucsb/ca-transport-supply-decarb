## Tracey Mangin
## November 16, 2020
## Baseline refinery outputs

library(tidyverse)
library(data.table)
library(rebus)
library(readxl)
library(openxlsx)

## file paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
model_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
sselection_path <- paste0(calepa_path, "model-development/scenario-plot/")

## saving
save_path2      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs'



## site ids 
site_id <- read_csv(paste0(calepa_path, "data/stocks-flows/processed/refinery_loc_cap.csv")) %>%
  select(refinery_name, county, site_id)

## prices, usd real 2020
prices <- read_xlsx(paste0(calepa_path, "data/stocks-flows/processed/oil_price_projections.xlsx"), sheet = "real")

## finished products from weekly fuel watch
## note: Other Diesel Fuel* includes renewable diesel
fw_df <- read_csv(paste0("/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/fuel_watch_data.csv"))

## 2019 renewable fuel production
rdiesel_19 <- read_csv(paste0(sselection_path, "renewable_diesel_2019.csv"))

## get kern production of renewables
renewables_ratio <- read_csv(paste0(model_path, "/stocks-flows/renewable_diesel_credits.csv"))

## site ratios
site_ratios <- read_csv(paste0(model_path, "/stocks-flows/refinery_capacity_ratios.csv"))

## for crude equivalent
intensities  <- read_csv(paste0(model_path, "/stocks-flows/crude_and_refined_products__energy_intensities_and_coefficients.csv"))


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
  rename(category = fuel_type) %>%
  mutate(region = "North")

bbls_rd <- as.numeric(renewd$bbls)


## determine ratio of renewable production occurring at Kern Oil
kern_ratio_df <- renewables_ratio %>%
  filter(lcfs_credit_recipient == "Kern Oil & Refining Co,")

kern_ratio <- as.numeric(kern_ratio_df$credit_ratio)

## adjust regional production for ethanol and renewable diesel production from Kern Oil
region_products_adj <- region_products %>%
  ## adjust for ethanol
  mutate(bbls = ifelse(category == "Motor Gasoline Reformulated", bbls * 0.9, bbls)) %>%
  ## subtract renewable diesel (bbls_rd) from Other Diesel category
  mutate(bbls =  ifelse(region == "North" & category == "Distillates: Other Diesel", bbls - bbls_rd, bbls)) %>%
  mutate(category = ifelse(region == "North" & category == "Distillates: Other Diesel", "Distillates: Other Diesel Adjusted", category)) %>%
  ## rbind renewable diesel -- gets its own row
  rbind(renewd) %>%
  ## adjust for kern county ratio
  mutate(bbls = ifelse(category == "Renewable Diesel", bbls * kern_ratio, bbls)) %>%
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
  select(-refinery_name)

site_crude_equiv <- region_products_adj %>%
  left_join(intensity_values) %>%
  left_join(vc_values) %>%
  left_join(coef_values) %>%
  rowwise() %>%
  mutate(crude_equiv_bbls = bbls * intensities / (crude_intensity - coef)) %>%
  ungroup() %>%
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

site_crude_equiv2 <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  left_join(site_crude_equiv) %>%
  mutate(site_bbls_equiv = capacity_ratio_within_region * crude_equiv_bbls) %>%
  select(-capacity_ratio_within_region, -crude_equiv_bbls) %>%
  left_join(kern_df) %>%
  pivot_longer(site_bbls_equiv:rd_crude_equiv_bbls, names_to = "cat", values_to = "crude_equiv_bbls") %>%
  group_by(site_id, refinery_name, region, year) %>%
  summarise(site_crude_equiv_bbls = sum(crude_equiv_bbls, na.rm = T)) %>%
  ungroup() %>%
  mutate(type = "consumption",
         fuel = "crude_equivalent") %>%
  rename(value_bbls = site_crude_equiv_bbls) %>%
  left_join(county_df) %>%
  select(site_id, refinery_name, county, region, year, type, fuel, value_bbls) %>%
  mutate(brent = brent_2019$price,
         spread = NA,
         product_price = NA,
         category = NA)


## site-level production
region_products_adj2 <- region_products_adj %>%
  mutate(region = ifelse(category == "Renewable Diesel", "Kern", region))

kern_prod <- region_products_adj2 %>%
  filter(region == "Kern") %>%
  mutate(site_id = 202,
         refinery_name = "Kern Oil & Refining Company, Bakersfield Refinery",
         region = "North",
         site_prod = bbls)


site_production <- site_ratios %>%
  select(site_id, refinery_name, region, capacity_ratio_within_region) %>%
  left_join(region_products_adj2) %>%
  mutate(site_prod = capacity_ratio_within_region * bbls) %>%
  select(-capacity_ratio_within_region) %>%
  rbind(kern_prod) %>%
  select(-bbls) %>%
  rename(value_bbls = site_prod) %>%
  left_join(crack_spread, by = c("fuel" = "product")) %>%
  left_join(county_df) %>%
  mutate(type = "production") %>%
  select(site_id, refinery_name, county, region, year, type, category, fuel, value_bbls, brent, spread, product_price)


## all
all_refinery_baseline <- rbind(site_production, site_crude_equiv2) %>%
  arrange(type)

fwrite(all_refinery_baseline, file.path(save_path2, 'baseline-2019', 'baseline_site_level_refining.csv'), row.names = F)

## (meas) region/cluster level crude equiv by fuel 
  region_crude_equiv <- region_products_adj %>%
    left_join(intensity_values) %>%
    left_join(vc_values) %>%
    left_join(coef_values) %>%
    rowwise() %>%
    mutate(crude_equiv_bbls = bbls * intensities / (crude_intensity - coef)) %>%
    ungroup() %>%
    select(region, category, year, crude_equiv_bbls) %>%
    mutate(region = ifelse(category == "Renewable Diesel", "Kern", region)) %>%
    group_by(region, year)

  fwrite(region_crude_equiv, file.path(save_path2, 'baseline-2019', 'baseline_region_level_crude_equiv.csv'), row.names = F)
  
