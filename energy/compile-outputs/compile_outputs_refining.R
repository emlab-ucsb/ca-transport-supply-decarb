## Tracey Mangin
## November 4, 2020 
## Compile refining outputs

library(tidyverse)
library(data.table)
library(rebus)
library(readxl)
library(openxlsx)


## file paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"

save_path2      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/energy-model-outputs'

sselection_path <- paste0(calepa_path, "model-development/scenario-plot/")

## refinery outputs
refinery_site_out <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_refinery.csv'), stringsAsFactors = F)

## site ids 
site_id <- read_csv(paste0(calepa_path, "data/stocks-flows/processed/refinery_loc_cap.csv")) %>%
  select(site_id, county)

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
 

##
crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                       spread = c(23, 20, 23))

## outputs for health ---
## bbl processed by site



total_processed <- refinery_site_out %>%
  mutate(site_id =  as.numeric(str_extract(site_id, pattern = START %R% one_or_more(DGT)))) %>%
  mutate(refinery_name = ifelse(site_id == 342, "Phillips 66, Rodeo San Francisco Refinery & Separate Unit", refinery_name)) %>%
  filter(type == "consumption",
         fuel == "crude",
         source == "total",
         boundary == "complete") %>%
  group_by(year, demand_scenario, refining_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario,
           site_id, refinery_name, location, region, cluster, type) %>%
  summarise(crude_e_total_bbl = sum(value, na.rm = T)) %>%
  ungroup()

write_csv(total_processed, paste0(save_path2, "/all-scenarios/refinery_site_outputs.csv"))


county_revenue <- refinery_site_out %>%
  mutate(site_id =  as.numeric(str_extract(site_id, pattern = START %R% one_or_more(DGT)))) %>%
  filter(type == "production") %>%
  mutate(product = ifelse(fuel %in% c("gasoline", "drop-in gasoline"), "gasoline",
                          ifelse(fuel %in% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))) %>%
  left_join(site_id) %>%
  mutate(county = ifelse(site_id == "99999", "Kern", county)) %>%
  left_join(prices2) %>%
  left_join(crack_spread) %>%
  mutate(product_price = oil_price_usd_per_bbl_real + spread,
         prod_revenue = value * product_price) %>%
  group_by(year, oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario, county) %>%
  summarise(prod_revenue = sum(prod_revenue)) %>%
  ungroup()

write_csv(county_revenue, paste0(save_path2, "/all-scenarios/refinery_county_outputs.csv"))

## save BAU
## ---------------------------------

## 1. no policy (for any policy)
## 2. carbon price: price floor
## 3. oil price: iea oil price
## 4. ccs: medium CCS cost
## 5. innovation: low innovation
## 6. demand: BAU
## 7. refinery: historic exports 

bau_site_out <- total_processed %>%
  filter(demand_scenario == "BAU",
         refining_scenario == "historic exports",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost")

write_csv(bau_site_out, paste0(save_path2, "/bau/bau_refinery_site_outputs.csv"))


bau_county_out <- county_revenue %>%
  filter(demand_scenario == "BAU",
         oil_price_scenario == "iea oil price",
         refining_scenario == "historic exports",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost")

write_csv(bau_county_out, paste0(save_path2, "/bau/bau_refinery_county_outputs.csv"))
