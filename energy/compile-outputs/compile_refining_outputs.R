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
site_2019_file <- 'site_refining_outputs_2019.csv'
county_2019_file <- 'county_refining_outputs_2019.csv'

## read in files
## ---------------------------------------

## oil prices
oilpx_scens_real = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens_real) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens_real = melt(oilpx_scens_real, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens_real[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens_real[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens_real <- oilpx_scens_real[year >= 2019]
setorderv(oilpx_scens_real, c('oil_price_scenario', 'year'))

## refining outputs
refining_out <- fread(file.path(main_path, outputs_path, refining_file))

## 2019 site level out
site_2019 <- fread(file.path(main_path, outputs_path, site_2019_file))

## 2019 county level out
county_2019 <- fread(file.path(main_path, outputs_path, county_2019_file))

## site ids 
site_id <- fread(paste0(main_path, "/data/stocks-flows/processed/refinery_loc_cap_manual.csv"), colClasses = c("site_id" = "character")) %>%
  select(site_id, county)

##
crack_spread <- tibble(product = c("gasoline", "jet_fuel", "diesel"),
                       spread = c(23, 20, 23))



## create site level outputs for health (crude bbls processed)
## ---------------------------------
site_out_refining <- refining_out[type == "consumption" & 
                                  fuel == "crude" &
                                  source == "total" &
                                  boundary == "complete", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                            ccs_scenario, site_id, year, fuel, type, value)]
## all scenario, refinery, year combinations
all_scens <- unique(refining_out[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)])

all_scens[, scen_id := .I]
setcolorder(all_scens, c("scen_id", "demand_scenario", "refining_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario"))

full_site_df <- expand.grid(scen_id = unique(all_scens$scen_id),
                            site_id = unique(site_out_refining$site_id),
                            year = seq(2019, 2045, 1))

setDT(full_site_df)

## add scenario information using id
full_site_df <- merge(full_site_df, all_scens,
                      by = c("scen_id"),
                      all.x = T)

setcolorder(full_site_df, c("scen_id", "demand_scenario", "refining_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", "site_id", "year"))

## 2019
full_site_df_2019 <- full_site_df[year == 2019]

full_site_df_2019 <- merge(full_site_df_2019, site_2019,
                           by = c("site_id", "year"),
                           all.x = T)

full_site_df_2019 <- full_site_df_2019[, .(scen_id, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                           ccs_scenario, site_id, type, fuel, year, value_bbls)]

setnames(full_site_df_2019, "value_bbls", "value")


## now do projection
## -------------------------------------------------

full_site_df_proj <- full_site_df[year > 2019]

full_site_df_proj <- merge(full_site_df_proj, site_out_refining,
                           by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                  'site_id', 'year'),
                           all.x = T)

full_site_df_proj[,':=' (value = fifelse(is.na(value), 0, value),
                         type = fifelse(is.na(type), "consumption", type),
                         fuel = fifelse(is.na(fuel), "crude", fuel))]

## bind 2019 to projected
full_site_out <- rbind(full_site_df_2019, full_site_df_proj)

## refinery names
refinery_names <- unique(refining_out[, .(site_id, refinery_name)])

full_site_out <- merge(full_site_out, refinery_names,
                       by = "site_id",
                       all.x = T)

full_site_out[, scen_id := paste0("R", scen_id)]

setcolorder(full_site_out, c('scen_id', 'demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'site_id', 'refinery_name', 'type', 'fuel', 'year', 'value'))

full_site_out[, scen_id := NULL]

## save outputs for health and labor
refining_site_fname = paste0('site_refining_outputs.csv')
fwrite(full_site_out, file.path(compiled_save_path_refining, refining_site_fname), row.names = F)
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

## add 2019, make full df
all_county_scens <- unique(county_out_refining_summary[, .(oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)])

all_county_scens[, scen_id := .I]
setcolorder(all_county_scens, c("scen_id", "oil_price_scenario", "demand_scenario", "refining_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario"))

full_county_df <- expand.grid(scen_id = unique(all_county_scens$scen_id),
                              county = unique(county_out_refining_summary$county),
                              year = seq(2019, 2045, 1))

setDT(full_county_df)

## 2019
full_county_2019 <- full_county_df[year == 2019]

full_county_2019 <- merge(full_county_2019, county_2019,
                           by = c("county", "year"),
                           all.x = T)

full_county_2019 <- merge(full_county_2019, all_county_scens,
                          by = c("scen_id"))

setcolorder(full_county_2019, c('scen_id', 'oil_price_scenario', 'demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'county', 'year', 'revenue'))


## projection
full_county_proj <- full_county_df[year > 2019]

full_county_proj <- merge(full_county_proj, all_county_scens,
                          by = "scen_id",
                          all.x = T)

full_county_proj <- merge(full_county_proj, county_out_refining_summary,
                                     by = c("oil_price_scenario", "demand_scenario", "refining_scenario", "innovation_scenario", 
                                            "carbon_price_scenario", "ccs_scenario", "county", "year"),
                          all.x = T)


full_county_proj[, revenue := fifelse(is.na(revenue), 0, revenue)]

## bind 2019 to projected
county_out_refining_all <- rbind(full_county_2019, full_county_proj)
county_out_refining_all[, scen_id := NULL]



## save outputs for health and labor
refining_county_fname = paste0('county_refining_outputs.csv')
fwrite(county_out_refining_all, file.path(compiled_save_path_refining, refining_county_fname), row.names = F)
print(paste0('Refining county outputs saved to ', refining_county_fname))


