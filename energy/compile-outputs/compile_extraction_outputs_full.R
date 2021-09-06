
## Tracey Mangin
## September 2, 2021
## Compile extraction outputs (site and county, include 2019) -- use individual rds files

## libraries
library(data.table)
library(tidyverse)
library(readxl)

## paths
main_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
main_path_external <- '/Volumes/calepa/'
# extraction_path <- 'outputs/predict-production/extraction_2021-09-02/'
extraction_path <- paste0(main_path_external, 'extraction-out/extraction_2021-09-02/full_run/')
data_path  <-'data/stocks-flows/processed/'

## labor path
labor_processed <- 'data/labor/processed/implan-results/academic-paper-multipliers/processed/'

cur_date              = Sys.Date()

## save paths
compiled_save_path  = paste0(main_path_external, 'academic-out/extraction_', cur_date, '/')
field_save_path     = paste0(compiled_save_path, 'field-results/')
state_save_path     = paste0(compiled_save_path, 'state-results/')
county_save_path    = paste0(compiled_save_path, 'county-results/')
# censust_save_path   = paste0(compiled_save_path, 'census-tract-results/')

## files
prod_file       <- "well_prod_m_processed.csv"
oil_price_file  <- 'oil_price_projections_revised.xlsx'
ghg_file        <- 'ghg_emissions_x_field_2018-2045.csv'

## create folder for outputs
dir.create(paste0(main_path_external, 'academic-out/'), showWarnings = FALSE)
dir.create(compiled_save_path, showWarnings = FALSE)
dir.create(field_save_path, showWarnings = FALSE) 
dir.create(state_save_path, showWarnings = FALSE)  
dir.create(county_save_path, showWarnings = FALSE)
# dir.create(censust_save_path, showWarnings = FALSE)  

## outputs should include:
## - county-level health
## - state production, health, and labor outputs


## read in files
## --------------------------------

## monthly well production
well_prod <- fread(paste0(main_path, "/data/stocks-flows/processed/", prod_file), colClasses = c('api_ten_digit' = 'character',
                                                                                                 'doc_field_code' = 'character'))
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


# read in files
field_files_to_process <- list.files(paste0(extraction_path, 'field-out/'))

# set start time -----
start_time <- Sys.time()
print(paste("Starting extraction compiling at ", start_time))

# cores
n_cores <- 8
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
  
  ## 
  county_out <- county_out[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                               setback_scenario, prod_quota_scenario, excise_tax_scenario, year, total_county_bbl, total_county_ghg_kgCO2e, revenue,
                               c.dire_emp, c.indi_emp, c.indu_emp, c.dire_comp, c.indi_comp, c.indu_comp)]
  
  # 4. Save extraction and refining output to 1 excel file with 2 sheets 
  
  ## save county outputs (labor, production, and revenue)
  saveRDS(county_out, paste0(county_save_path, scenario_id_tmp, "_county_results.rds"))
  
  
  ## state outputs
  ## -------------------------------------
  
  state_out <- county_out[, lapply(.SD, sum, na.rm = T), .SDcols = c("total_county_bbl", "revenue",
                                                                     "total_county_ghg_kgCO2e",
                                                                     "c.dire_emp", "c.indi_emp", 
                                                                     "c.indu_emp", "c.dire_comp", 
                                                                     "c.indi_comp", "c.indu_comp"), by = .(scen_id, oil_price_scenario, innovation_scenario,
                                                                                                           carbon_price_scenario, ccs_scenario,
                                                                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
  
  setnames(state_out, c("total_county_bbl", "revenue",  "total_county_ghg_kgCO2e"), c("total_state_bbl", "total_state_revenue", "total_state_ghg_kgCO2"))
                                                
  
  ## save state outputs (labor, production, and revenue)
  saveRDS(state_out, paste0(state_save_path, scenario_id_tmp, "_state_results.rds"))
  

  
}

elapsed_time <- Sys.time() - start_time
print(elapsed_time)



## segment, year, doc_field_code, doc_field_name, oil_price_scenario, innovation_scenario,	carbon_price_scenario,	ccs_scenario,
## setback_scenario,	prod_quota_scenario,	excise_tax_scenario,	production_bbl,	oil_price_usd_per_bbl_real, revenue,
## refining specific: demand_scenario, refining scenario, site_id, refinery_name, location, region, cluster, crude_e_total_bbl_net_exp

