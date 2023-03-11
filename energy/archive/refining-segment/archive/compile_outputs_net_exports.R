## Tracey Mangin
## November 21, 2020
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

## refinery outputs -- use net e
refinery_site_out_net_e <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_refinery_net_exports.csv'), stringsAsFactors = F)
refinery_site_out_total_e <- read.csv(paste0(sselection_path, 'refinery-outputs/refining_scenario_outputs_refinery_total_exports.csv'), stringsAsFactors = F)

## old results
original_site_out <- read.csv(paste0(save_path2, '/all-scenarios/refinery_site_outputs.csv'), stringsAsFactors = F)
original_county_out <- read.csv(paste0(save_path2, '/all-scenarios/refinery_county_outputs.csv'), stringsAsFactors = F)

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

save_site_outputs <- function(df, included_exports) {

  export_name <- included_exports
  
  total_processed <- df %>%
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

  write_csv(total_processed, paste0(save_path2, "/all-scenarios/refinery_site_outputs_", export_name, ".csv"))

return(total_processed)

}

site_out_net_exp <- save_site_outputs(df = refinery_site_out_net_e, included_exports = "net_exports")
site_out_total_exp <- save_site_outputs(df = refinery_site_out_total_e, included_exports = "total_exports")

# ## are the old results and the "new" total export results the same?
# comp <- site_out_total_exp %>%
#   rename(new_val = crude_e_total_bbl) %>%
#   left_join(original_site_out) %>%
#   mutate(new_val - crude_e_total_bbl)
# ## hooray! the same. carry on


## create ratio df
site_ratio_df <- site_out_net_exp %>%
  rename(crude_e_total_bbl_net_exp = crude_e_total_bbl) %>%
  full_join(site_out_total_exp) %>%
  rename(crude_e_total_bbl_total_exp = crude_e_total_bbl) %>%
  mutate(crude_e_total_bbl_net_exp = ifelse(is.na(crude_e_total_bbl_net_exp), 0, crude_e_total_bbl_net_exp),
         crude_e_total_bbl_total_exp = ifelse(is.na(crude_e_total_bbl_total_exp), 0, crude_e_total_bbl_total_exp),
         ratio = crude_e_total_bbl_net_exp / crude_e_total_bbl_total_exp)

write_csv(site_ratio_df, paste0(save_path2, "/all-scenarios/refinery_site_outputs_ratios.csv"))


save_county_outputs <- function(df, included_exports) {

  export_name <- included_exports

  county_revenue <- df %>%
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
  
  write_csv(county_revenue, paste0(save_path2, "/all-scenarios/refinery_county_outputs_", export_name, ".csv"))

  return(county_revenue)
  
}

county_out_net_exp <- save_county_outputs(df = refinery_site_out_net_e, included_exports = "net_exports")
county_out_total_exp <- save_county_outputs(df = refinery_site_out_total_e, included_exports = "total_exports")

# ## are the old results and the "new" total export results the same?
# comp_county <- county_out_total_exp %>%
#   rename(new_val = prod_revenue) %>%
#   left_join(original_county_out) %>%
#   mutate(new_val - prod_revenue)
# ## the same! carry on

## create ratio df
county_ratio_df <- county_out_net_exp %>%
  rename(prod_revenue_net_exp = prod_revenue) %>%
  full_join(county_out_total_exp) %>%
  rename(prod_revenue_total_exp = prod_revenue) %>%
  mutate(prod_revenue_net_exp = ifelse(is.na(prod_revenue_net_exp), 0, prod_revenue_net_exp),
         prod_revenue_total_exp = ifelse(is.na(prod_revenue_total_exp), 0, prod_revenue_total_exp),
         ratio = prod_revenue_net_exp / prod_revenue_total_exp)

write_csv(county_ratio_df, paste0(save_path2, "/all-scenarios/refinery_county_outputs_ratios.csv"))

## save BAU
## ---------------------------------

## 1. no policy (for any policy)
## 2. carbon price: price floor
## 3. oil price: iea oil price
## 4. ccs: medium CCS cost
## 5. innovation: low innovation
## 6. demand: BAU
## 7. refinery: historic exports 

bau_site_out <- site_ratio_df %>%
  filter(demand_scenario == "BAU",
         refining_scenario == "historic exports",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost")

write_csv(bau_site_out, paste0(save_path2, "/bau/bau_refinery_site_outputs_ratios.csv"))



bau_county_out <- county_ratio_df %>%
  filter(demand_scenario == "BAU",
         oil_price_scenario == "iea oil price",
         refining_scenario == "historic exports",
         innovation_scenario == "low innovation",
         carbon_price_scenario == "price floor",
         ccs_scenario == "medium CCS cost")

write_csv(bau_county_out, paste0(save_path2, "/bau/bau_refinery_county_outputs_ratios.csv"))
