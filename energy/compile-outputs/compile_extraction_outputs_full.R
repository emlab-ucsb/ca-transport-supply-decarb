
## Tracey Mangin
## September 2, 2021
## Compile extraction outputs (site and county, include 2019) -- use individual rds files

## libraries
library(data.table)
library(tidyverse)
library(purrr)
library(readxl)
library(openxlsx)
library(furrr)
library(future)


## model output location
save_external <- 1

## current date
cur_date              = Sys.Date()

## paths 
main_path     <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'

## UPDATE THESE WITH NEW RUNS!!!!!
extraction_folder_path <- 'outputs/predict-production/extraction_2021-12-06/'
extraction_folder_name <- 'subset_target_scens/'
data_path  <-'data/stocks-flows/processed/'

## health code paths
source_path   <- paste0(main_path, 'data/health/source_receptor_matrix/')
inmap_ex_path  <- paste0(main_path, "data/health/source_receptor_matrix/inmap_processed_srm/extraction")

## external paths
main_path_external <- '/Volumes/calepa/'

if(save_external == 1) {
  
  ## UPDATE THIS WITH NEW RUNS!!!!!
  extraction_path <- paste0(main_path_external, 'extraction-out/extraction_2022-02-01/test_target/')
  
  dir.create(paste0(main_path_external, 'academic-out/'), showWarnings = FALSE)
  compiled_save_path  <- paste0(main_path_external, 'academic-out/extraction_', cur_date, '/')

} else {
  
  extraction_path <- paste0(main_path, extraction_folder_path, extraction_folder_name)

  compiled_save_path  <- paste0(main_path, 'outputs/academic-out/extraction/extraction_', cur_date, '/')

}

## labor path
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'


## save path
field_save_path     = paste0(compiled_save_path, 'field-results/')
state_save_path     = paste0(compiled_save_path, 'state-results/')
county_save_path    = paste0(compiled_save_path, 'county-results/')
ct_save_path        = paste0(compiled_save_path, 'census-tract-results/')

## files
prod_file       <- "well_prod_m_processed.csv"
oil_price_file  <- 'oil_price_projections_revised.xlsx'
ghg_file        <- 'ghg_emissions_x_field_2018-2045.csv'
ghg_hist_file   <- 'indust_emissions_2000-2019.csv'

## create folder for outputs
dir.create(compiled_save_path, showWarnings = FALSE)
dir.create(field_save_path, showWarnings = FALSE) 
dir.create(state_save_path, showWarnings = FALSE)  
dir.create(county_save_path, showWarnings = FALSE)
dir.create(ct_save_path, showWarnings = FALSE)

## outputs should include:
## - county-level health
## - state production, health, and labor outputs


## read in files
## --------------------------------

## DAC and CES
dac_ces <- read_xlsx(paste0(main_path, 'data/health/raw/ces3results.xlsx'))

ces_county <- dac_ces %>%
  select(`Census Tract`, `California County`) %>%
  rename(census_tract = `Census Tract`,
         county = `California County`) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) 

## income -- cencus tract
med_house_income <- fread(paste0(main_path, "data/Census/ca-median-house-income.csv"), stringsAsFactors = F)
med_house_income[, census_tract := paste0("0", GEOID)]
med_house_income <- med_house_income[, .(census_tract, estimate)]
setnames(med_house_income, "estimate", "median_hh_income")

## income -- county
county_income <- fread(paste0(main_path, "data/Census/ca-median-house-income-county.csv"), stringsAsFactors = F)
county_income <- county_income[, .(county, estimate)]
setnames(county_income, "estimate", "median_hh_income")


## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))

## historical GHG emissions, 2019
## --------------------------
hist_ghg <- fread(paste0(main_path, 'data/stocks-flows/processed/', ghg_hist_file), header = T)

hist_ghg <- hist_ghg[segment %chin% c('Oil & Gas: Production & Processing') &
                       year == 2019, .(segment, unit, year, value)]

ghg_2019 <- as.numeric(hist_ghg[, value][1])



## ghg factors
ghg_factors = fread(file.path(main_path, 'outputs/stocks-flows', ghg_file), header = T, colClasses = c('doc_field_code' = 'character'))
ghg_factors_2019 = ghg_factors[year == 2019, c('doc_field_code', 'year', 'upstream_kgCO2e_bbl')]

## oil prices
oilpx_scens = setDT(read.xlsx(file.path(main_path, data_path, oil_price_file), sheet = 'real', cols = c(1:4)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year >= 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## multipliers

# 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 
## NOTE: multipliers are per $1 million of output value
## NOTE: Prefix "ip." denotes a multiplier that has been replaced with the sample average because IMPLAN has no 
## data on extraction in this county 

total_multipliers_ext <- read_xlsx(paste0(main_path, labor_processed, 'ica_multipliers_v2.xlsx'), sheet = 'ica_total') %>% 
  filter((county != "Statewide" & segment == "extraction") | is.na(segment)==T) %>% 
  rename(dire_emp_mult = direct_emp, 
         indi_emp_mult = indirect_emp, 
         indu_emp_mult = induced_emp,
         dire_comp_mult = direct_comp, 
         indi_comp_mult = indirect_comp, 
         indu_comp_mult = induced_comp,
         ip.dire_comp_mult = ip.direct_comp, 
         ip.indi_comp_mult = ip.indirect_comp, 
         ip.indu_comp_mult = ip.induced_comp)


## county information
## -------------------------------

## county lut
county_lut <- well_prod %>%
  dplyr::select(doc_field_code, county_name) %>%
  unique() %>%
  mutate(adj_county_name = str_remove(county_name, " Offshore"))

## field name look up 
fname_lut <- well_prod %>%
  dplyr::select(doc_field_code, doc_fieldname) %>%
  unique()

## get relative county production (most recent year of nonzero production available for each field)
prod_x_county <- well_prod %>%
  left_join(county_lut) %>%
  group_by(doc_field_code, doc_fieldname, year, adj_county_name) %>%
  summarise(prod = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup() %>%
  group_by(doc_field_code, year) %>%
  mutate(field_total = sum(prod, na.rm = T)) %>%
  ungroup() %>%
  mutate(rel_prod = prod / field_total,
         rel_prod = ifelse(is.na(rel_prod) & prod == 0 & field_total == 0, 0, rel_prod)) %>%
  filter(rel_prod > 0) %>%
  group_by(doc_field_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(doc_field_code, adj_county_name, rel_prod)

## how many fields with positive prod?
# View(field_out[, c(prod = sum(total_prod_bbl, na.rm = T)), by = doc_field_code][V1 > 0])

## calculate 2019 production, emissions, revenue
init_prod <- well_prod %>%
  filter(year == 2019) %>%
  select(doc_field_code, doc_fieldname, year, OilorCondensateProduced) %>%
  group_by(doc_field_code, doc_fieldname, year) %>%
  summarise(total_prod_bbl = sum(OilorCondensateProduced, na.rm = T)) %>%
  ungroup()

setDT(init_prod)

## merge with ghg factors
init_prod <- merge(init_prod, ghg_factors_2019,
                   by = c('doc_field_code', 'year'),
                   all.x = T)

init_prod[, total_ghg_kgCO2e := total_prod_bbl * upstream_kgCO2e_bbl]


init_prod <- init_prod[, .(doc_field_code, year, total_prod_bbl, total_ghg_kgCO2e)]

## remove fields that do not ever produce oil and do not show up in results, as well as "Any Field)
init_prod <- init_prod[!doc_field_code %in% c("302", "502", "000")]

# init_prod_bbls_only <- init_prod[, .(doc_field_code, year, total_prod_bbl)]

## health data
## ------------------------------------------------

#  Load extraction source receptor matrix (srm) #######
n_cores <- availableCores() - 1
plan(multisession, workers = n_cores, gc = TRUE)

#fields vector
fields_vector <- c(1:26)

#function
read_extraction <- function(buff_field){
  
  bfield <- buff_field
  
  nh3  <- read_csv(paste0(inmap_ex_path, "/nh3/srm_nh3_field", bfield, ".csv", sep="")) %>% mutate(poll = "nh3")
  nox  <- read_csv(paste0(inmap_ex_path, "/nox/srm_nox_field", bfield, ".csv", sep="")) %>% mutate(poll = "nox")
  pm25 <- read_csv(paste0(inmap_ex_path, "/pm25/srm_pm25_field", bfield, ".csv", sep="")) %>% mutate(poll = "pm25")
  sox  <- read_csv(paste0(inmap_ex_path, "/sox/srm_sox_field", bfield, ".csv", sep="")) %>% mutate(poll = "sox")
  voc  <- read_csv(paste0(inmap_ex_path, "/voc/srm_voc_field", bfield, ".csv", sep="")) %>% mutate(poll = "voc")
  
  all_polls <- rbind(nh3, nox, pm25, sox, voc)
  
  all_polls$field = bfield
  
  tmp <- as.data.frame(all_polls) 
  
  return(tmp)
  
}

#build extraction srm
srm_all_pollutants_extraction <- future_map_dfr(fields_vector, read_extraction) %>% 
  bind_rows() %>%
  rename(weighted_totalpm25 = totalpm25_aw)%>%
  select(-totalpm25) %>%
  spread(poll, weighted_totalpm25) %>%
  rename(weighted_totalpm25nh3 = nh3,
         weighted_totalpm25nox = nox,
         weighted_totalpm25pm25 = pm25,
         weighted_totalpm25sox = sox,
         weighted_totalpm25voc = voc,
         id = field)

future:::ClusterRegistry("stop")

# (1.2) Calculate census tract ambient emissions for extraction  #######

#load and process cross-walk between fields and clusters 
extraction_field_clusters_10km <- read_csv(paste0(source_path,"/extraction_fields_clusters_10km.csv",sep="")) %>%
  select(OUTPUT_FID, INPUT_FID) %>%
  rename(id = OUTPUT_FID, input_fid = INPUT_FID)

extraction_fields_xwalk <- foreign::read.dbf(paste0(source_path, "/extraction_fields_xwalk_id.dbf", sep = "")) %>%
  rename(input_fid = id, doc_field_code = dc_fld_)

extraction_xwalk <- extraction_field_clusters_10km %>%
  left_join(extraction_fields_xwalk, by = c("input_fid")) 


## (2.1) Load demographic data
# Disadvantaged community definition
ces3 <- read.csv(paste0(main_path, "data/health/processed/ces3_data.csv"), stringsAsFactors = FALSE) %>%
  select(census_tract, population, CES3_score, disadvantaged) %>%
  mutate(census_tract = paste0("0", census_tract, sep="")) %>%
  as.data.table()

## add counties
ces3 <- merge(ces3, ces_county, 
              by = "census_tract")

## remove population
ces3[, population := NULL]

# Population and incidence
ct_inc_pop_45 <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, upper_age, year, pop, incidence_2015) %>%
  as.data.table()

## census tract, population > 29
ct_pop_time <- ct_inc_pop_45 %>%
  filter(lower_age > 29) %>%
  group_by(ct_id, year) %>%
  summarize(ct_pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  rename(census_tract = ct_id) %>%
  as.data.table()

## dac share over time
ct_pop_time <- merge(ct_pop_time, ces3, 
                   by = c("census_tract"),
                   all.x = T)

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

## county pop
county_pop <- ct_inc_pop_45_weighted %>%
  left_join(ces_county, by = c('ct_id' = 'census_tract')) %>%
  group_by(county, year) %>%
  summarise(county_pop = sum(pop)) %>%
  ungroup() %>%
  filter(!is.na(county))

## DAC proportion
county_dac <- dcast(ct_pop_time, county + census_tract + year ~ disadvantaged, value.var = "ct_pop")
county_dac <- county_dac[!is.na(county)]
county_dac[, Yes := fifelse(is.na(Yes), 0, Yes)]
county_dac[, No := fifelse(is.na(No), 0, No)]
county_dac[, total_pop := No + Yes]
county_dac[, dac_share := Yes / total_pop]
county_dac <- county_dac[, .(dac_share = weighted.mean(dac_share, total_pop, na.rm = T)), by = .(county, year)]


## Coefficients from Krewski et al (2009) for mortality impact
beta <- 0.00582
se <- 0.0009628

## for monetary mortality impact
growth_rates <- read.csv(paste0(main_path, "data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
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

## ------------------------------------------------------------
## process all scenarios
## ------------------------------------------------------------

## read in files
field_files_to_process <- list.files(paste0(extraction_path, 'field-out/'))


# set start time -----
start_time <- Sys.time()
print(paste("Starting extraction compiling at ", start_time))

# cores
doParallel::registerDoParallel(cores = n_cores)

for (i in 1:length(field_files_to_process)) {
  
  
  field_file_name <- field_files_to_process[i]
  
  field_scen_out <- readRDS(paste0(extraction_path, 'field-out/', field_file_name))

  # ## check fields in 2019 vs outputs
  # ## ------------------------------------------------------
  # anti_join(init_prod %>% select(doc_field_code), field_scen_out %>% select(doc_field_code) %>% unique())
  # 
  # check_projection <- anti_join(field_scen_out %>% select(doc_field_code) %>% unique(), init_prod %>% select(doc_field_code))
  # 
  # View(field_scen_out[doc_field_code %chin% check_projection[, doc_field_code] & total_prod_bbl > 0])
  # ## English Colony (ABD) has existing well production; Pacoima (ABD) has new well production
  # 
  ## prepare projection outputs
  site_out <- field_scen_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                            setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code,
                            year, total_prod_bbl, total_ghg_kgCO2e)]
  
  full_site_df <- expand.grid(scen_id = unique(site_out$scen_id),
                              doc_field_code = unique(c(site_out$doc_field_code, init_prod$doc_field_code)),
                              year = seq(2019, 2045, 1))
  
  setDT(full_site_df)
  
  scen_info <- distinct(site_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                     setback_scenario, prod_quota_scenario, excise_tax_scenario)])
  
  scenario_id_tmp <- scen_info[ , scen_id]
  
  full_site_df <- merge(full_site_df, scen_info,
                        by = c("scen_id"),
                        all.x = T)
  
  ## add scenario information using id
  full_site_df <- merge(full_site_df, site_out,
                        by = c("scen_id", 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 
                               'ccs_scenario', 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario',  
                               'year', 'doc_field_code'),
                        all.x = T)
  
  ## 2019
  full_site_df_2019 <- full_site_df[year == 2019]
  
  full_site_df_2019 <- merge(full_site_df_2019, fname_lut,
                             by = c("doc_field_code"),
                             all.x = T)
  
  full_site_df_2019 <- merge(full_site_df_2019[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                   setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code,
                                                   doc_fieldname, year)], init_prod,
                             by = c("doc_field_code", "year"),
                             all.x = T)
  
  full_site_df_2019[,':=' (total_prod_bbl = fifelse(is.na(total_prod_bbl), 0, total_prod_bbl))]
  full_site_df_2019[,':=' (total_ghg_kgCO2e = fifelse(is.na(total_ghg_kgCO2e), 0, total_ghg_kgCO2e))]
  
  setcolorder(full_site_df_2019, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                              "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "doc_field_code", 
                              "doc_fieldname", "year", "total_prod_bbl", "total_ghg_kgCO2e"))
  
  
  ## now do projection
  full_site_df_proj <- full_site_df[year > 2019]
  
  full_site_df_proj <- merge(full_site_df_proj, fname_lut,
                             by = c("doc_field_code"),
                             all.x = T)
  
  ## bind 2019 to projected
  full_site_out <- rbind(full_site_df_2019, full_site_df_proj)
  
  
  ## add oil prices (including 2019... 2020 real dollars)
  full_site_out <- merge(full_site_out, oilpx_scens,
                         by = c("oil_price_scenario", "year"),
                         all.x = T)
  
  full_site_out[, revenue := total_prod_bbl * oil_price_usd_per_bbl]
  
  full_site_out[, oil_price_usd_per_bbl := NULL]
  
  setcolorder(full_site_out, c("scen_id", "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario",
                                   "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", "doc_field_code", 
                                   "doc_fieldname", "year", "total_prod_bbl", "revenue", "total_ghg_kgCO2e"))
  
  setorder(full_site_out, "scen_id", "doc_field_code", "year")
  
  ## save site level output for production 
  saveRDS(full_site_out, paste0(field_save_path, scenario_id_tmp, "_field_results.rds"))
  
  
  
  ## now do county-level, add labor impacts
  ## ---------------------------------------------------------
  
  county_out <- merge(full_site_out, prod_x_county,
                      by = c("doc_field_code"),
                      all.x = T,
                      allow.cartesian = T)
  
  setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                            'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                            'year', 'doc_field_code', 'doc_fieldname', 'total_prod_bbl', 'adj_county_name', 'rel_prod'))
  
  ## summarise at the county level
  county_out[, county_prod_bbl := total_prod_bbl * rel_prod]
  county_out[, county_ghg_kgCO2e := total_ghg_kgCO2e * rel_prod]
  
  county_out <- county_out[, .(total_county_bbl = sum(county_prod_bbl),
                               total_county_ghg_kgCO2e = sum(county_ghg_kgCO2e)), by = .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                                                                         ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                                                                         year, adj_county_name)]
  
  ## add oil price
  county_out <- merge(county_out, oilpx_scens,
                      by = c("oil_price_scenario", "year"),
                      all.x = T)
  
  county_out[, revenue := total_county_bbl * oil_price_usd_per_bbl]
  
  setcolorder(county_out, c('scen_id', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                            'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                            'year', 'adj_county_name', 'total_county_bbl', 'oil_price_usd_per_bbl', 'revenue'))
  
  setorder(county_out, "scen_id", "adj_county_name", "year")
  
  setnames(county_out, "adj_county_name", "county")
  

  # Part A: Compute total impacts by county, year, and scenario 
   ## 1. Import processed IMPLAN multipliers for the labor analysis and remove the statewide multipliers 
   ## 2. Join multipliers to output from the energy modeling team by county and compute county level labor impacts
   ## 3. Save extraction output
  
  
  # Part A: file with total impacts by county, year, and scenario
  
  # 3. Join multipliers to output from the energy modeling team by county and compute county level labor impacts 
  
  ## Note: c. indicates county 
  ## c.dire_emp : Direct employment impact in either the refining or extraction segment 
  ## c.dire_comp: Direct compensation impact in either the refining or extraction segment 
  ## c.indi.emp: Indirect employment impact in either the refining or extraction segment
  ## c.indi.comp: Indirect compensation impact in either the refining or extraction segment
  ## c.indu.emp: Induced employment impact in either the refining or extraction segment
  ## c.indu.comp: Induced compensation impact in either the refining or extraction segment
  ## To get total impact for a county: add direct + indirect + induced 
  
  county_out <- merge(county_out, total_multipliers_ext,
                      by = "county",
                      all.x = T)
  
  county_out[, proj_prod := sum(total_county_bbl), by = .(scen_id, county)]
  county_out <- county_out[proj_prod > 0]
  county_out[, proj_prod := NULL]
  
  county_out[, ':=' (c.dire_emp = (revenue / (10 ^ 6)) * dire_emp_mult, 
                     c.indi_emp = (revenue / (10 ^ 6)) * indi_emp_mult, 
                     c.indu_emp = (revenue / (10 ^ 6)) * indu_emp_mult,
                     c.dire_comp = (revenue / (10 ^ 6)) * dire_comp_mult, 
                     c.indi_comp = (revenue / (10 ^ 6)) * ip.indi_comp_mult, 
                     c.indu_comp = (revenue / (10 ^ 6)) * ip.indu_comp_mult)]
  
  county_out[, ':=' (total_emp = c.dire_emp + c.indi_emp + c.indu_emp,
                     total_comp = c.dire_comp + c.indi_comp + c.indu_comp)]
  
  ## merge with county dac proportion
  county_out <- merge(county_out, county_dac,
                      by = c("county", "year"),
                      all.x = T)
  
  county_out <- merge(county_out, county_income,
                      by = "county",
                      all.x = T)
  
  county_out <- merge(county_out, county_pop,
                      by = c("county", "year"),
                      all.x = T)
  
  ## 
  county_out <- county_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                               setback_scenario, prod_quota_scenario, excise_tax_scenario, county, dac_share, median_hh_income,
                               year, county_pop, total_county_bbl, total_county_ghg_kgCO2e, revenue,
                               c.dire_emp, c.indi_emp, c.indu_emp, c.dire_comp, c.indi_comp, c.indu_comp, total_emp, total_comp)]
  
 
  
  
  ## save county outputs (labor, production, and revenue)
  saveRDS(county_out, paste0(county_save_path, scenario_id_tmp, "_county_results.rds"))
  
  
  
  ## HEALTH IMPACTS: calculate census level health impacts
  ## -------------------------------------------------------------
  
  ## merge extraction production scenarios with extraction cluster 
  health_site_out <- merge(full_site_out, extraction_xwalk,
                           by = c("doc_field_code"),
                           all.x = T)
  
  ## summarize extraction production per cluster
  total_clusters <- health_site_out[, .(total_prod_bbl = sum(total_prod_bbl)), by = .(id, year, scen_id, oil_price_scenario,
                                                                                      carbon_price_scenario, ccs_scenario, setback_scenario,
                                                                                      excise_tax_scenario)] 
  
  ## calculate air pollution using emission factors
  total_clusters <- total_clusters %>%
    mutate(nh3 = total_prod_bbl * 0.00061 / 1000,
           nox = total_prod_bbl * 0.04611 / 1000,
           pm25 = total_prod_bbl * 0.00165 / 1000,
           sox = total_prod_bbl * 0.01344 / 1000,
           voc = total_prod_bbl * 0.02614 / 1000)
  
  total_clusters <- total_clusters %>%
    right_join(srm_all_pollutants_extraction, by = c("id")) %>%
    mutate(tot_nh3 = weighted_totalpm25nh3 * nh3,
           tot_nox = weighted_totalpm25nox * nox,
           tot_sox = weighted_totalpm25sox * sox,
           tot_pm25 = weighted_totalpm25pm25 * pm25,
           tot_voc = weighted_totalpm25voc * voc,
           total_pm25 = tot_nh3 + tot_nox + tot_pm25 + tot_sox + tot_voc,
           prim_pm25 = tot_pm25)
  
  ct_exposure <- total_clusters %>%
    ## Adjust mismatch of census tract ids between inmap and benmap (census ID changed in 2012 
    ## http://www.diversitydatakids.org/sites/default/files/2020-02/ddk_coi2.0_technical_documentation_20200212.pdf)
    mutate(GEOID = ifelse(GEOID == "06037137000", "06037930401", GEOID)) %>%
    group_by(GEOID, year, scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, excise_tax_scenario) %>%
    summarize(total_pm25 = sum(total_pm25, na.rm = T), 
              prim_pm25 = sum(prim_pm25, na.rm = T)) %>%
    ungroup() %>%
    rename(census_tract = GEOID) %>%
    select(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, excise_tax_scenario,
           census_tract, year, total_pm25)
  
  ## add ces score, income, and dac
  ct_exposure <- merge(ct_exposure, ces3[, .(census_tract, CES3_score, disadvantaged)],
                       by = c("census_tract"),
                       all.x = T)
  
  ## add income
  ct_exposure <- merge(ct_exposure, med_house_income,
                       by = c("census_tract"),
                       all.x = T)
  
  
  setorder(ct_exposure, "census_tract", "year")
  
  setcolorder(ct_exposure, c("scen_id", "oil_price_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario",
                             "excise_tax_scenario", "census_tract", "disadvantaged", "CES3_score", "median_hh_income", "year", "total_pm25"))
  
  
  
  
  ## save census tract outputs (pm2.5 levels, mortality level, cost)
  saveRDS(ct_exposure, paste0(ct_save_path, scenario_id_tmp, "_ct_results.rds"))
  
  

  ## state outputs
  ## -------------------------------------
  
  state_out <- county_out[, lapply(.SD, sum, na.rm = T), .SDcols = c("county_pop", "total_county_bbl", "revenue",
                                                                     "total_county_ghg_kgCO2e",
                                                                     "c.dire_emp", "c.indi_emp", 
                                                                     "c.indu_emp", "c.dire_comp", 
                                                                     "c.indi_comp", "c.indu_comp",
                                                                     "total_emp", "total_comp"), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                                                           carbon_price_scenario, ccs_scenario,
                                                                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
  
  setnames(state_out, c("county_pop", "total_county_bbl", "revenue",  "total_county_ghg_kgCO2e"), c("state_pop", "total_state_bbl", "total_state_revenue", "total_state_ghg_kgCO2"))
                                                
  
  ## save state outputs (labor, production, and revenue)
  saveRDS(state_out, paste0(state_save_path, scenario_id_tmp, "_state_results.rds"))
  
}

elapsed_time <- Sys.time() - start_time
print(elapsed_time)

## -------------------------------------------------------------
## now calculate relative health outputs
## add to ct_results
## add state values to state_results
## -------------------------------------------------------------

## extraction pm25 BAU
extraction_BAU <- readRDS(paste0(ct_save_path, "reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_ct_results.rds")) %>%
  rename(bau_total_pm25 = total_pm25) %>%
  select(census_tract, year, bau_total_pm25) %>% 
  as.data.table()

## scenarios
scenarios_to_process <- str_remove_all(field_files_to_process, "_field.rds")


for (i in 1:length(field_files_to_process)) {
  
  scen_file_name <- scenarios_to_process[i]
  
  ct_scen_out <- readRDS(paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  setDT(ct_scen_out)
  
  ## calculate delta pm 2.5
  delta_extraction <- merge(ct_scen_out, extraction_BAU,
                            by = c("census_tract", "year"),
                            all = T)
  
  delta_extraction[, delta_total_pm25 := total_pm25 - bau_total_pm25]
  
  
  ## Merge demographic data to pollution scenarios
  ct_incidence <- delta_extraction %>%
    # left_join(ces3, by = c("census_tract")) %>%
    right_join(ct_inc_pop_45_weighted, by = c("census_tract" = "ct_id", "year" = "year")) %>%
    # remove census tracts that are water area only tracts (no population)
    drop_na(scen_id) %>% 
    as.data.table()
  
  ## Calculate mortality impact ######################################
  
  #Mortality impact fold adults (>=29 years old)
  ct_incidence <- ct_incidence[, mortality_delta := ((exp(beta * delta_total_pm25) - 1)) * weighted_incidence * pop]
  ct_incidence <- ct_incidence[, mortality_level := ((exp(beta * total_pm25) - 1)) * weighted_incidence * pop]

  ## Monetizing mortality ############################################
  
  ## Calculate the cost per premature mortality
  ct_incidence <- ct_incidence %>%
    mutate(VSL_2019 = VSL_2019)%>%
    left_join(growth_rates, by = c("year" = "year"))%>%
    mutate(VSL = future_WTP(income_elasticity_mort, 
                            (cum_growth-1),
                            VSL_2019),
           cost_2019 = mortality_delta * VSL_2019,
           cost = mortality_delta * VSL)%>%
    group_by(year) %>%
    mutate(cost_2019_PV = cost_2019/((1+discount_rate)^(year-2019)),
           cost_PV = cost/((1+discount_rate)^(year-2019))) %>%
    ungroup()
  
  ## final census tract level health outputs
  ct_incidence <- ct_incidence %>%
    select(scen_id, census_tract, CES3_score, disadvantaged, median_hh_income, year, weighted_incidence, pop, total_pm25, bau_total_pm25, delta_total_pm25, 
           mortality_delta, mortality_level, cost_2019, cost, cost_2019_PV, cost_PV) %>%
    as.data.table()
  
  ## resave the census tract data
  saveRDS(ct_incidence, paste0(ct_save_path, scen_file_name, "_ct_results.rds"))
  
  ## ----------------------------------------------------------------------------
  ## calculate state values, read in state value, resave with mortality values
  ## ----------------------------------------------------------------------------
  
  ct_incidence_state <- ct_incidence[, lapply(.SD, sum, na.rm = T), .SDcols = c("mortality_delta", "mortality_level", 
                                                                                "cost_2019", "cost", 
                                                                                "cost_2019_PV", "cost_PV"), by = .(scen_id, year)]
  
  
  ct_pm_state <- ct_incidence[, lapply(.SD, mean, na.rm = T), .SDcols = c("total_pm25", "delta_total_pm25"), by = .(scen_id,  year)]
  setnames(ct_pm_state, c("total_pm25", "delta_total_pm25"), c("mean_total_pm25", "mean_delta_total_pm25"))
  
  ct_incidence_state <- merge(ct_incidence_state, ct_pm_state,
                              by = c("scen_id", "year"))
  
  
  
  state_out <- readRDS(paste0(state_save_path, scen_file_name, "_state_results.rds"))
  setDT(state_out)
  
  state_out <- merge(state_out, ct_incidence_state,
                     by = c("scen_id", "year"),
                     all.x = T)
  
  ## resave state outputs
  saveRDS(state_out, paste0(state_save_path, scen_file_name, "_state_results.rds"))
  

}













## segment, year, doc_field_code, doc_field_name, oil_price_scenario, innovation_scenario,	carbon_price_scenario,	ccs_scenario,
## setback_scenario,	prod_quota_scenario,	excise_tax_scenario,	production_bbl,	oil_price_usd_per_bbl_real, revenue,
## refining specific: demand_scenario, refining scenario, site_id, refinery_name, location, region, cluster, crude_e_total_bbl_net_exp

