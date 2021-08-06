## Tracey Mangin
## August 1, 2021
## Compile refining outputs (site and county, include 2019)

library(data.table)  
library(tidyverse)
library(openxlsx)

## create a folder to store outputs
cur_date              = Sys.Date()
compiled_save_path_refining  = file.path('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/', paste0('refining_', cur_date))
dir.create(compiled_save_path_refining, showWarnings = FALSE)  


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
oilpx_scens_real <- oilpx_scens_real[year >= 2019]
setorderv(oilpx_scens_real, c('oil_price_scenario', 'year'))

## refining outputs
refining_out <- fread(file.path(main_path, outputs_path, refining_file))

## site ids 
site_id <- fread(paste0(main_path, "/data/stocks-flows/processed/refinery_loc_cap_manual.csv"), colClasses = c("site_id" = "character")) %>%
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
                                                            ccs_scenario, site_id, location, region, cluster, refinery_name, year, fuel, source,
                                                            boundary, type, value)]


## save outputs for health and labor
refining_site_fname = paste0('site_refining_outputs.csv')
fwrite(site_out_refining, file.path(compiled_save_path_refining, refining_site_fname), row.names = F)
print(paste0('Refining site outputs saved to ', refining_site_fname))



## county out
## --------------------------------

county_out_refining <- refining_out[type == "production", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                            ccs_scenario, site_id, location, region, cluster, refinery_name, year, fuel, source,
                                                            boundary, type, value)]
## add product for calculating price
county_out_refining[, product := fifelse(fuel %chin% c("gasoline", "drop-in gasoline"), "gasoline",
                                         fifelse(fuel %chin% c("diesel", "renewable diesel"), "diesel", "jet_fuel"))]

## merge with counties
county_out_refining <- merge(county_out_refining, site_id,
                             by = c("site_id"),
                             all.x = T)

## fill in missing counties
county_out_refining[, county := fifelse(site_id == "342-2", "Contra Costa",
                                         fifelse(site_id == "99999", "Kern",
                                                 fifelse(site_id == "t-800", "Los Angeles", county)))]

## merge with prices
county_out_refining <- merge(county_out_refining, oilpx_scens_real,
                             by = c("year"),
                             all.x = T,
                             allow.cartesian = TRUE)

## join with crack spread
county_out_refining <- merge(county_out_refining, crack_spread,
                             by = c("product"),
                             all.x = T)

county_out_refining[, product_price := oil_price_usd_per_bbl + spread]
county_out_refining[, revenue := value * product_price]

## summarize at the county level
county_out_refining_summary <- county_out_refining[, .(revenue = sum(revenue)), by = .(oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario,
                                                                                       carbon_price_scenario, ccs_scenario, county, year)]

## save outputs for health and labor
refining_county_fname = paste0('county_refining_outputs.csv')
fwrite(county_out_refining_summary, file.path(compiled_save_path_refining, refining_county_fname), row.names = F)
print(paste0('Refining county outputs saved to ', refining_county_fname))


