## Tracey Mangin
## August 1, 2021
## Compile refining outputs (site and county, include 2019)

library(data.table)  
library(tidyverse)
library(openxlsx)
library(readxl)

## create a folder to store outputs
cur_date              = Sys.Date()
compiled_save_path_refining  = file.path('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining/', paste0('refining_', cur_date))
dir.create(compiled_save_path_refining, showWarnings = FALSE)  


## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
data_path  <-'data/stocks-flows/processed'
outputs_path <- 'model-development/scenario-plot/refinery-outputs'
model_outputs_path <- 'outputs/predict-production/refining_2021-09-06/CUF0.6/outputs'

## labor path
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed'


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
refining_out <- fread(file.path(main_path, model_outputs_path, refining_file))

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


## labor
total_multipliers_ref <- read_xlsx(file.path(main_path, labor_processed, 'ica_multipliers_v2.xlsx'), sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "refining") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, indi_emp_mult = indirect_emp, indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, indi_comp_mult = indirect_comp, indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, ip.indi_comp_mult = ip.indirect_comp, ip.indu_comp_mult = ip.induced_comp)



## create site level outputs for health (crude bbls processed)
## ---------------------------------
site_out_refining <- refining_out[type == "consumption" & 
                                  fuel == "crude" &
                                  source == "total" &
                                  boundary == "complete", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                            ccs_scenario, site_id, year, fuel, type, value)]
## all scenario, refinery, year combinations
all_scens <- unique(refining_out[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)])

## merge with prices
oilpx_scen_names <- unique(oilpx_scens_real[, .(oil_price_scenario)])

all_scens <- crossing(all_scens, oilpx_scen_names)

setDT(all_scens)

## add id
all_scens[, scen_id := .I]



setcolorder(all_scens, c("scen_id", "oil_price_scenario", "demand_scenario", "refining_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario"))

full_site_df <- expand.grid(scen_id = unique(all_scens$scen_id),
                            site_id = unique(site_out_refining$site_id),
                            year = seq(2019, 2045, 1))

setDT(full_site_df)

## add scenario information using id
full_site_df <- merge(full_site_df, all_scens,
                      by = c("scen_id"),
                      all.x = T)

setcolorder(full_site_df, c("scen_id", "oil_price_scenario", "demand_scenario", "refining_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", "site_id", "year"))

## 2019
full_site_df_2019 <- full_site_df[year == 2019]

full_site_df_2019 <- merge(full_site_df_2019, site_2019,
                           by = c("site_id", "year"),
                           all.x = T)

full_site_df_2019 <- full_site_df_2019[, .(scen_id, oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
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

setcolorder(full_site_out, c('scen_id', 'oil_price_scenario', 'demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'site_id', 'refinery_name', 'type', 'fuel', 'year', 'value'))

setorder(full_site_out, "scen_id", "site_id", "year")


## bau scen ids
full_site_out[, scen_id := fifelse((oil_price_scenario == 'reference case' & 
                                  innovation_scenario == 'low innovation' & 
                                  carbon_price_scenario == 'price floor' & 
                                  ccs_scenario == 'medium CCS cost' &
                                  demand_scenario == 'BAU' &
                                  refining_scenario == 'historic exports'), 'R-BAU', paste0("R-", scen_id))]


## save outputs for health and labor
refining_site_fname = paste0('site_refining_outputs.csv')
fwrite(full_site_out, file.path(compiled_save_path_refining, refining_site_fname), row.names = F)
print(paste0('Refining site outputs saved to ', refining_site_fname))



## county out
## --------------------------------

county_out_refining <- refining_out[type == "production", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                            ccs_scenario, site_id, location, region, cluster, refinery_name, year, fuel, source,
                                                            boundary, type, value)]

## create county scenarios
county_scens <- full_site_df[year != 2019]

prod_options <- unique(county_out_refining[, .(fuel, source, boundary, type)])

county_scens <- crossing(county_scens, prod_options)

setDT(county_scens)

## merge
county_out_refining <- merge(county_scens, county_out_refining,
                             by = c("demand_scenario", "refining_scenario", "innovation_scenario",
                                    "carbon_price_scenario", "ccs_scenario", "site_id", "year", "fuel",
                                    "source", "boundary", "type"),
                             all.x = T)

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
                             by = c("year", "oil_price_scenario"),
                             all.x = T)

## join with crack spread
county_out_refining <- merge(county_out_refining, crack_spread,
                             by = c("product"),
                             all.x = T)

county_out_refining[, value := fifelse(is.na(value), 0, value)]
county_out_refining[, product_price := oil_price_usd_per_bbl + spread]
county_out_refining[, revenue := value * product_price]

## summarize at the county level
county_out_refining_summary <- county_out_refining[, .(revenue = sum(revenue)), by = .(scen_id, oil_price_scenario, demand_scenario, refining_scenario, innovation_scenario,
                                                                                       carbon_price_scenario, ccs_scenario, county, year)]

## add 2019, make full df
## -----------------------------

county_scens2 <- unique(county_out_refining_summary[, .(scen_id, oil_price_scenario, demand_scenario, refining_scenario,
                                                       innovation_scenario, carbon_price_scenario, ccs_scenario, county)])


## 2019
full_county_2019 <- merge(county_scens2, county_2019,
                           by = c("county"),
                           all.x = T)

# full_county_2019 <- merge(full_county_2019, all_county_scens,
#                           by = c("scen_id"))
# 
# setcolorder(full_county_2019, c('scen_id', 'oil_price_scenario', 'demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
#                              'county', 'year', 'revenue'))
# 
# 
# ## projection
# full_county_proj <- full_county_df[year > 2019]
# 
# full_county_proj <- merge(full_county_proj, all_county_scens,
#                           by = "scen_id",
#                           all.x = T)
# 
# full_county_proj <- merge(full_county_proj, county_out_refining_summary,
#                                      by = c("oil_price_scenario", "demand_scenario", "refining_scenario", "innovation_scenario", 
#                                             "carbon_price_scenario", "ccs_scenario", "county", "year"),
#                           all.x = T)
# 
# 
# full_county_proj[, revenue := fifelse(is.na(revenue), 0, revenue)]

## bind 2019 to projected
county_out_refining_all <- rbind(full_county_2019, county_out_refining_summary)

setcolorder(county_out_refining_all, c('scen_id', 'oil_price_scenario', 'demand_scenario', 'refining_scenario', 'innovation_scenario', 
                                       'carbon_price_scenario', 'ccs_scenario', 'county', 'year', 'revenue'))

setorder(county_out_refining_all, "scen_id", "county", "year")


## bau scen ids
county_out_refining_all[, scen_id := fifelse((oil_price_scenario == 'reference case' & 
                                      innovation_scenario == 'low innovation' & 
                                      carbon_price_scenario == 'price floor' & 
                                      ccs_scenario == 'medium CCS cost' &
                                      demand_scenario == 'BAU' &
                                      refining_scenario == 'historic exports'), 'R-BAU', paste0("R-", scen_id))]

county_out_refining_all[, county := fifelse(county == "Solano County", "Solano", county)]


## calculate labor impacts
county_out_labor <- merge(county_out_refining_all, total_multipliers_ref,
                          by = c("county"),
                          all.x = T)

county_out_labor[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult, 
                         c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult, 
                         c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
                         c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult, 
                         c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult, 
                         c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]

county_out_labor <- county_out_labor[, .(scen_id, oil_price_scenario, demand_scenario, refining_scenario,
                                         innovation_scenario, carbon_price_scenario, ccs_scenario, county,
                                         year, revenue, c.dire_emp, c.indi_emp, c.indu_emp, c.dire_comp, c.indi_comp,
                                         c.indu_comp)]


## save outputs for health and labor
refining_county_fname = paste0('county_refining_outputs.csv')
fwrite(county_out_labor, file.path(compiled_save_path_refining, refining_county_fname), row.names = F)
print(paste0('Refining county outputs saved to ', refining_county_fname))


