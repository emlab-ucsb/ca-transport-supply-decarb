## Tracey Mangin
## August 1, 2021
## Compile refining outputs (site and county, include 2019)

library(data.table)  
library(tidyverse)
library(openxlsx)

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
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))


## refining outputs
refining_out <- fread(file.path(main_path, outputs_path, refining_file))

## site ids 
site_id <- fread(paste0(main_path, "/data/stocks-flows/processed/refinery_loc_cap.csv")) %>%
  select(site_id, county)

##
crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                       spread = c(23, 21, 23))


## create site level outputs for health (crude bbls processed)
## ---------------------------------
site_out_refining <- refining_out[type == "consumption" & 
                                  fuel == "crude" &
                                  source == "total" &
                                  boundary == "complete", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                   ccs_scenario, site_id, location, region, cluster, refinery_name, location, year, fuel, source,
                                      boundary, type, value)]


## county out
## --------------------------------

county_out_refining <- refining_out[type == "production", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                            ccs_scenario, site_id, location, region, cluster, refinery_name, location, year, fuel, source,
                                                            boundary, type, value)]
## add product for calculating price
county_out_refining[, product := fifelse(fuel %chin% c("gasoline", "drop-in gasoline"), "gasoline",
                                         fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))]

## merge with site_id df for counties

# county_revenue <- df %>%
#   mutate(site_id =  as.numeric(str_extract(site_id, pattern = START %R% one_or_more(DGT)))) %>%
#   filter(type == "production") %>%
#   mutate(product = ifelse(fuel %in% c("gasoline", "drop-in gasoline"), "gasoline",
#                           ifelse(fuel %in% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))) %>%
#   left_join(site_id) %>%
#   mutate(county = ifelse(site_id == "99999", "Kern", county)) %>%
#   left_join(prices2) %>%
#   left_join(crack_spread) %>%
#   mutate(product_price = oil_price_usd_per_bbl_real + spread,
#          prod_revenue = value * product_price) %>%
#   group_by(year, oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario, county) %>%
#   summarise(prod_revenue = sum(prod_revenue)) %>%
#   ungroup()
# 
# 
