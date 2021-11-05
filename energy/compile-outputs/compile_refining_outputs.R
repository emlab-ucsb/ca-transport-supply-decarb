## Tracey Mangin
## August 1, 2021
## Compile refining outputs (site and county, include 2019)

library(data.table)  
library(tidyverse)
library(openxlsx)
library(readxl)
library(furrr)
library(future)
library(doParallel)

## create a folder to store outputs
cur_date              = Sys.Date()
compiled_save_path_refining  = file.path('/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/refining', paste0('refining_', cur_date))
dir.create(compiled_save_path_refining, showWarnings = FALSE)  

## create a folder for health outputs
dir.create(paste0(compiled_save_path_refining, '/census-tract-results'), showWarnings = FALSE)
dir.create(paste0(compiled_save_path_refining, '/subset-census-tract-results'), showWarnings = FALSE)

## paths
main_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn"
data_path  <-'data/stocks-flows/processed'
outputs_path <- 'model-development/scenario-plot/refinery-outputs'
model_outputs_path <- 'outputs/predict-production/refining_2021-11-03/CUF0.6/outputs'
inmap_re_files  <- paste0(main_path, "/data/health/source_receptor_matrix/inmap_processed_srm/refining")
ref_save_path <- paste0(main_path, '/outputs/academic-out/refining/')

## set this based on where the outputs will be
pm25_folder <- paste0("refining_", cur_date, "/census-tract-results/")

## labor path
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed'


## files
oil_price_file <- 'oil_price_projections_revised.xlsx'
refining_file <- 'refining_scenario_outputs_refinery_net_exports_revised.csv'
site_2019_file <- 'site_refining_outputs_2019.csv'
county_2019_file <- 'county_refining_outputs_2019.csv'
refin_file <- 'ref_scenario_id_list.csv'
ghg_2019_file <- 'refining_emissions_state_2019.csv'

## read in files
## ---------------------------------------

## DAC and CES
dac_ces <- read_xlsx(paste0(main_path, '/data/health/raw/ces3results.xlsx'))

ces_county <- dac_ces %>%
  select(`Census Tract`, `California County`) %>%
  rename(census_tract = `Census Tract`,
         county = `California County`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 

## income -- cencus tract
med_house_income <- fread(paste0(main_path, "/data/Census/ca-median-house-income.csv"), stringsAsFactors = F)
med_house_income[, census_tract := paste0("0", GEOID)]
med_house_income <- med_house_income[, .(census_tract, estimate)]
setnames(med_house_income, "estimate", "median_hh_income")

## income -- county
county_income <- fread(paste0(main_path, "/data/Census/ca-median-house-income-county.csv"), stringsAsFactors = F)
county_income <- county_income[, .(county, estimate)]
setnames(county_income, "estimate", "median_hh_income")

## health data
## ------------------------------------------------
refin_scens <- fread(paste0(ref_save_path, refin_file))
refin_scens <- refin_scens[, .(scen_id, BAU_scen, subset_scens)]


#  Load extraction source receptor matrix (srm) #######
n_cores <- availableCores() - 1
plan(multisession, workers = n_cores, gc = TRUE)

## refining sites
sites_vector <- c(97, 119, 164, 202, 209, 226, 271, 279, 332, 342, 343, 800, 3422, 34222, 99999)

read_refining <- function(buff_site){
  
  bsite <- buff_site
  
  nh3 <- read_csv(paste0(inmap_re_files, "/nh3/srm_nh3_site", bsite, ".csv", sep="")) %>% mutate(poll = "nh3")
  nox <- read_csv(paste0(inmap_re_files, "/nox/srm_nox_site", bsite, ".csv", sep="")) %>% mutate(poll = "nox")
  pm25 <- read_csv(paste0(inmap_re_files, "/pm25/srm_pm25_site", bsite, ".csv", sep="")) %>% mutate(poll = "pm25")
  sox <- read_csv(paste0(inmap_re_files, "/sox/srm_sox_site", bsite, ".csv", sep="")) %>% mutate(poll = "sox")
  voc <- read_csv(paste0(inmap_re_files, "/voc/srm_voc_site", bsite, ".csv", sep="")) %>% mutate(poll = "voc")
  
  all_polls <- rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$site = bsite
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}

#build refining srm
srm_all_pollutants_refining <- future_map_dfr(sites_vector, read_refining) %>% 
  bind_rows() %>%
  rename(weighted_totalpm25 = totalpm25_aw) %>%
  select(-totalpm25) %>%
  spread(poll, weighted_totalpm25) %>%
  rename(weighted_totalpm25nh3 = nh3,
         weighted_totalpm25nox = nox,
         weighted_totalpm25pm25 = pm25,
         weighted_totalpm25sox = sox,
         weighted_totalpm25voc = voc,
         site_id = site)

setDT(srm_all_pollutants_refining)

future:::ClusterRegistry("stop")

## (2.1) Load demographic data
# Disadvantaged community definition
ces3 <- read.csv(paste0(main_path, "/data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
  select(census_tract, population, CES3_score, disadvantaged) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

## add counties
ces3 <- merge(ces3, ces_county, 
              by = "census_tract")

## DAC proportion
county_dac <- dcast(ces3, county + census_tract ~ disadvantaged, value.var = "population")
county_dac[, Yes := fifelse(is.na(Yes), 0, Yes)]
county_dac[, No := fifelse(is.na(No), 0, No)]
county_dac[, total_pop := No + Yes]
county_dac[, dac_share := Yes / total_pop]
county_dac <- county_dac[, .(dac_share = weighted.mean(dac_share, total_pop, na.rm = T)), by = .(county)]


# Population and incidence
ct_inc_pop_45 <- fread(paste0(main_path, "/data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census-tract level population-weighted incidence rate (for age>29)
ct_inc_pop_45_weighted <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  group_by(ct_id, year) %>%
  mutate(ct_pop = sum(pop, na.rm = T),
         share = pop/ct_pop,
         weighted_incidence = sum(share * incidence_2015, na.rm = T)) %>%
  summarize(weighted_incidence = unique(weighted_incidence),
            pop = unique(ct_pop)) %>%
  ungroup() %>%
  as.data.table()

## Coefficients from Krewski et al (2009) for mortality impact
beta <- 0.00582
se <- 0.0009628

## for monetary mortality impact
growth_rates <- read.csv(paste0(main_path, "/data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
  filter(year > 2018) %>%
  mutate(growth = ifelse(year == 2019, 0, growth_2030),
         cum_growth = cumprod(1 + growth)) %>%
  select(-growth_2030, -growth) %>%
  as.data.table()

#Parameters for monetary health impact
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * 107.8645906/100 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
income_elasticity_mort <- 0.4
discount_rate <- 0.03

future_WTP <- function(elasticity, growth_rate, WTP){
  return(elasticity * growth_rate * WTP + WTP) 
}



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

## ghg 2019
ghg_2019 <- fread(paste0(main_path, "/model-development/scenario-plot/refinery-outputs/", ghg_2019_file))

ghg_2019 <- ghg_2019[boundary == "in-state", .(year, value)]
ghg_2019[, mtco2e := value / 1e9]
ghg_2019[, value := NULL]

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

## ghg values
## ----------------------------------

site_out_ghg <- refining_out[type == "ghg" & 
                             source == "total" &
                             boundary == "complete", .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario,
                                                              ccs_scenario, site_id, year, fuel, type, value)]

## bind
site_out_refining <- rbind(site_out_refining, site_out_ghg)


## all scenario, refinery, year combinations
all_scens <- unique(refining_out[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)])

## merge with prices
oilpx_scen_names <- unique(oilpx_scens_real[, .(oil_price_scenario)])

all_scens <- crossing(all_scens, oilpx_scen_names)

setDT(all_scens)

## add id
all_scens[, scen_id := paste(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, oil_price_scenario)]


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

setnames(full_site_df_2019, "value_bbls", "bbls_consumed")

full_site_df_2019[, fuel := NULL]
full_site_df_2019[, type := NULL]
full_site_df_2019[, ghg_kg := NA]

## now do projection
## -------------------------------------------------

full_site_df_proj <- full_site_df[year > 2019]

## metrics 
metrics <- unique(site_out_refining[, .(type)])

full_site_df_proj <- crossing(full_site_df_proj, metrics)

setDT(full_site_df_proj)

full_site_df_proj <- merge(full_site_df_proj, site_out_refining,
                           by = c('demand_scenario', 'refining_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                  'site_id', 'year', 'type'),
                           all.x = T)

full_site_df_proj[, fuel := NULL]

## fill in NA
full_site_df_proj[, value := fifelse(is.na(value), 0, value)]

## dcast
full_site_df_proj <- dcast(full_site_df_proj, scen_id + oil_price_scenario + demand_scenario + 
                             refining_scenario + innovation_scenario + carbon_price_scenario +
                             ccs_scenario + site_id + year ~ type, value.var = "value")

setnames(full_site_df_proj, c("consumption", "ghg"), c("bbls_consumed", "ghg_kg"))


# full_site_df_proj[,':=' (value = fifelse(is.na(value), 0, value),
#                          type = fifelse(is.na(type), "consumption", type),
#                          fuel = fifelse(is.na(fuel), "crude", fuel))]

## bind 2019 to projected
full_site_out <- rbind(full_site_df_2019, full_site_df_proj)

## refinery names
refinery_names <- unique(refining_out[, .(site_id, refinery_name)])

full_site_out <- merge(full_site_out, refinery_names,
                       by = "site_id",
                       all.x = T)

setcolorder(full_site_out, c('scen_id', 'oil_price_scenario', 'demand_scenario', 'refining_scenario', 
                             'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'site_id', 'refinery_name', 'year', 'bbls_consumed', 'ghg_kg'))

setorder(full_site_out, "scen_id", "site_id", "year")

full_site_out <- merge(full_site_out, refin_scens,
                       by = "scen_id",
                       all.x = T)

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

county_out_labor[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
                         total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]


## merge with county dac proportion
county_out_labor <- merge(county_out_labor, county_dac,
                    by = "county",
                    all.x = T)

## merge with income
county_out_labor <- merge(county_out_labor, county_income,
                    by = "county",
                    all.x = T)

county_out_labor <- county_out_labor[, .(scen_id, oil_price_scenario, demand_scenario, refining_scenario,
                                         innovation_scenario, carbon_price_scenario, ccs_scenario, county, dac_share, median_hh_income,
                                         year, revenue, c.dire_emp, c.indi_emp, c.indu_emp, c.dire_comp, c.indi_comp,
                                         c.indu_comp, total_emp, total_comp)]

county_out_labor <- merge(county_out_labor, refin_scens,
                       by = "scen_id",
                       all.x = T)




## save outputs for health and labor
refining_county_fname = paste0('county_refining_outputs.csv')
fwrite(county_out_labor, file.path(compiled_save_path_refining, refining_county_fname), row.names = F)
print(paste0('Refining county outputs saved to ', refining_county_fname))


## health outputs
## --------------------------------------------------------

# (1.3) Calculate census tract ambient emissions for refining  #######

refining_outputs_health <- full_site_out %>%
  mutate(site_id = ifelse(site_id == "t-800", "800", site_id),
         site_id = ifelse(site_id == "342-2", "34222", site_id),
         site_id = as.numeric(site_id))%>%
  mutate(nh3=value*0.00056/1000,
         nox=value*0.01495/1000,
         pm25=value*0.00402/1000,
         sox=value*0.00851/1000,
         voc=value*0.01247/1000) %>%
  as.data.table()


# Merge refining srm to obtain census tract pm25 exposure
#Loop over scenarios  

scenarios <- unique(refining_outputs_health$scen_id)

## run this to save PM 2.5 exposure x census tract by scenario
## -----------------------------------------------------------------

# cores
doParallel::registerDoParallel(cores = n_cores)

run_pm25_exposure <- 1

calc_pm25 <- function(scen_index) {

  scen_name <- scenarios[scen_index]

  health_tmp <- refining_outputs_health[scen_id == scen_name]

  health_tmp <- health_tmp %>%
    right_join(srm_all_pollutants_refining, by = c("site_id"))%>%
    mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
           tot_nox = weighted_totalpm25nox * nox,
           tot_sox = weighted_totalpm25sox * sox,
           tot_pm25 = weighted_totalpm25pm25 * pm25,
           tot_voc = weighted_totalpm25voc * voc,
           total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
           prim_pm25 = tot_pm25)

  health_tmp <- health_tmp %>%
    ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 
    ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
    mutate(GEOID = ifelse(GEOID == "06037137000", "06037930401", GEOID)) %>%
    group_by(GEOID, year, scen_id) %>%
    summarize(total_pm25 = sum(total_pm25, na.rm = T),
              prim_pm25 = sum(prim_pm25, na.rm = T)) %>%
    ungroup() %>%
    rename(census_tract = GEOID) %>%
    as.data.table()
  
  setorder(health_tmp, "census_tract", "year")
  
  ## add ces score, income, and dac
  health_tmp <- merge(health_tmp, ces3[, .(census_tract, population, CES3_score, disadvantaged)],
                       by = c("census_tract"),
                       all.x = T)
  
  ## add income
  health_tmp <- merge(health_tmp, med_house_income,
                       by = c("census_tract"),
                       all.x = T)
  
  setorder(health_tmp, "census_tract", "year")
  
  setcolorder(health_tmp, c("scen_id", "census_tract", "population", "disadvantaged", "CES3_score", "median_hh_income", "year", "total_pm25"))
  
  health_tmp[, prim_pm25 := NULL]
  

  saveRDS(health_tmp, paste0(compiled_save_path_refining, '/census-tract-results/', scen_name, "_ct_results.rds"))

}

if(run_pm25_exposure == 1) {
  
  foreach(i = 1:length(scenarios)) %dopar% {
    calc_pm25(i)
  }

}


## make comparisons to BAU
## read in refining pm25 BAU

bau_scen_name <- refin_scens[BAU_scen == 1, scen_id]

refining_BAU <- readRDS(paste0(main_path, "/outputs/academic-out/refining/", pm25_folder,  bau_scen_name, "_ct_results.rds"))

setnames(refining_BAU, c("total_pm25"), c("bau_total_pm25"))

refining_BAU <- refining_BAU[, .(census_tract, year, bau_total_pm25)]

## calculate health indicators for the subset
refining_sub <- refin_scens[BAU_scen == 1 | subset_scens == 1, .(scen_id)]
refining_sub_vec <- as.vector(refining_sub$scen_id)

health_impacts_list <- list()

for (i in 1:length(refining_sub_vec)) {
  
  scen_tmp <- refining_sub_vec[i]
  
  ## make sure to update this
  ct_scen_out <- readRDS(paste0(main_path, "/outputs/academic-out/refining/refining_2021-10-26/census-tract-results/", scen_tmp, "_ct_results.rds"))
  setDT(ct_scen_out)
  
  ## refining pm25 difference
  deltas_refining <- merge(ct_scen_out, refining_BAU,
                           by = c("census_tract", "year"),
                           all.x = T)

  deltas_refining[, delta_total_pm25 := total_pm25 - bau_total_pm25]


  ## (2.2) Merge demographic data to pollution scenarios
  
  deltas_refining <- deltas_refining %>%
    select(scen_id, census_tract, median_hh_income, year, total_pm25, bau_total_pm25, delta_total_pm25) %>%
    left_join(ces3, by = c("census_tract" = "census_tract")) %>%
    right_join(ct_inc_pop_45_weighted, by = c("census_tract" = "ct_id", "year" = "year")) %>%
    # remove census tracts that are water area only tracts (no population)
    drop_na(scen_id)

  ## Mortality impact fold adults (>=29 years old)
  deltas_refining <- deltas_refining %>%
    mutate(mortality_delta = ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop,
           mortality_level = ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop)

  ## Calculate the cost per premature mortality
  deltas_refining <- deltas_refining %>%
    mutate(VSL_2019 = VSL_2019)%>%
    left_join(growth_rates, by = c("year"="year"))%>%
    mutate(VSL = future_WTP(income_elasticity_mort, 
                            (cum_growth-1),
                            VSL_2019),
           cost_2019 = mortality_delta*VSL_2019,
           cost = mortality_delta*VSL)%>%
    group_by(year)%>%
    mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
           cost_PV = cost/((1+discount_rate)^(year-2019)))
  
  ## final census tract level health outputs
  deltas_refining <- deltas_refining %>%
    select(scen_id, census_tract, CES3_score, disadvantaged, median_hh_income, year, weighted_incidence, pop, total_pm25, bau_total_pm25, delta_total_pm25, 
           mortality_delta, mortality_level, cost_2019, cost, cost_2019_PV, cost_PV) %>%
    as.data.table()
  
  ## resave the census tract data
  saveRDS(deltas_refining, paste0(compiled_save_path_refining, "/subset-census-tract-results/", scen_tmp, "_ct_results.rds"))
  
  health_impacts_list[[i]] <- deltas_refining
  
  
}

subset_health_impacts <- rbindlist(health_impacts_list)
  
## ----------------------------------------------------------------------------
## calculate state values, read in state value, resave with mortality values
## ----------------------------------------------------------------------------

## health  
health_state <- subset_health_impacts[, lapply(.SD, sum, na.rm = T), .SDcols = c("mortality_delta", "mortality_level", 
                                                                                 "cost_2019", "cost", 
                                                                                 "cost_2019_PV", "cost_PV"), by = .(scen_id, year)]
  

health_state_pm25 <- subset_health_impacts[, lapply(.SD, mean, na.rm = T), .SDcols = c("total_pm25", "bau_total_pm25",
                                                                                       "delta_total_pm25"), by = .(scen_id, year)]


setnames(health_state_pm25, c("total_pm25", "bau_total_pm25", "delta_total_pm25"), c("mean_total_pm25", "mean_bau_total_pm25", "mean_delta_total_mp25"))


health_state <- merge(health_state, health_state_pm25,
                      by = c("scen_id", "year"),
                      all.x = T)


## labor
labor_state <- county_out_labor[, lapply(.SD, sum, na.rm = T), .SDcols = c("revenue", "c.dire_emp", "c.indi_emp", 
                                                                           "c.indu_emp", "c.dire_comp", 
                                                                           "c.indi_comp", "c.indu_comp", 
                                                                           "total_emp", "total_comp"), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                                                                 carbon_price_scenario, ccs_scenario,
                                                                                                                 demand_scenario, refining_scenario, year)]

setnames(labor_state, c("revenue"), c("total_state_revenue"))

## bbls refined
refined_state <- full_site_out[, lapply(.SD, sum, na.rm = T), .SDcols = c("bbls_consumed", "ghg_kg"), by = .(scen_id, year)]

refined_state[, ghg_kg := fifelse(year == 2019, ghg_2019[, mtco2e][1] * 1e9, ghg_kg)]


state_out <- merge(labor_state, refined_state,
                   by = c("scen_id", "year"),
                   all.x = T)

state_out <- merge(state_out, health_state,
                   by = c("scen_id", "year"),
                   all.x = T)
  
setcolorder(state_out, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                         "demand_scenario", "refining_scenario", "year", "state_crude_eq_consumed_bbl", "total_state_revenue",
                         "c.dire_emp", "c.indi_emp",  "c.indu_emp", "c.dire_comp", "c.indi_comp", 
                         "c.indu_comp", "total_emp", "total_comp", "mean_total_pm25", "mean_bau_total_pm25", "mean_delta_total_mp25",
                         "mortality_delta", "mortality_level", "cost_2019", "cost",  "cost_2019_PV", "cost_PV"))

subset_state_out <- state_out[scen_id %in% refining_sub_vec]

## resave state outputs
fwrite(subset_state_out, paste0(compiled_save_path_refining, "/subset_state_results.csv"))




