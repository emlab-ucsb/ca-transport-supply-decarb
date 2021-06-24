# preparation of price and cost variables under various scenarios to use in entry forecast (source-able version)
# created: march 9, 2021
# author: meas meng

load_scenarios_dt = function(oil_px_selection) {
  # inputs -----
  
  outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
  data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
  scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
  forecast_file     = 'field_capex_opex_forecast_final.csv'
  resource_file     = 'field_resource.csv'
  # brent_file        = 'brent_oil_price_projections.csv'
  # brent_file        = 'brent_oil_price_projections_real.xlsx'
  oil_price_file    = 'oil_price_projections.xlsx'
  inn_file          = 'innovation_scenarios.csv'
  carbon_file       = 'carbon_prices_revised.csv'
  ccs_ext_file      = 'ccs_extraction_scenarios.csv'
  ccs_ref_file      = 'ccs_refining_scenarios.csv'
  ghg_file          = 'ghg_emissions_x_field_2015_revised.csv'
  setback_file      = 'setback_coverage_R.csv'
  prod_quota_file   = 'prod_quota_scenarios.csv'
  excise_tax_file   = 'excise_tax_scenarios.csv'
  

  # load packages -----
  
  library(data.table)
  library(openxlsx)
  # library(tidyverse)
  
  # load data -----
  
  oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = 1:5))
  colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price') #'bp_oil_price'
  oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price'), 
                     variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
  oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
  oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price', 'iea oil price'))]
  setorderv(oilpx_scens, c('oil_price_scenario', 'year'))
  
  if(oil_px_selection %chin% c('reference', 'high', 'low', 'iea', 'diagnostic')) {
    
    # filter oil price scenario to only keep what is specified as input
    oilpx_scens = oilpx_scens[oil_price_scenario == fcase(oil_px_selection == 'reference', 'reference case',
                                                          oil_px_selection == 'high', 'high oil price',
                                                          oil_px_selection == 'low', 'low oil price',
                                                          oil_px_selection == 'iea', 'iea oil price',
                                                          oil_px_selection == 'diagnostic', 'iea oil price')]
  
  }
  # oilpx_scens = fread(file.path(data_path, brent_file), header = T)
  # oilpx_scens = oilpx_scens[scenario %in% c('high_oil_price', 'low_oil_price', 'reference_case')]
  # oilpx_scens[, oil_price_scenario := gsub('_', ' ', scenario)]
  # oilpx_scens = oilpx_scens[, c('year', 'oil_price_scenario', 'nom_usd_per_bbl')]
  # setnames(oilpx_scens, 'nom_usd_per_bbl', 'oil_price_usd_per_bbl')
  
  innovation_scens = fread(file.path(scen_path, inn_file), header = T)
  
  carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
  # carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
  carbonpx_scens[, carbon_price_usd_per_kg := carbon_price/1000] # convert from usd per metric ton to usd per kg
  carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]
  
  ccs_scens = fread(file.path(scen_path, ccs_ext_file), header = T)
  ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
  ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
  ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  
  price_data = fread(file.path(outputs_path, 'stocks-flows', forecast_file), header = T)
  
  resource_data = fread(file.path(outputs_path, 'entry-model-results', resource_file), header = T)
  resource_data = resource_data[, c('doc_field_code', 'resource')]
  
  ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T)
  ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]
  
  # load setback coverage file
  setback_scens = fread(file.path(outputs_path, 'setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))
  setback_scens <- setback_scens[, c("doc_field_code", "setback_scenario", "rel_coverage")]
  setnames(setback_scens, 'rel_coverage', 'area_coverage')
  setback_scens[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]
  
  
  # load production quota file
  prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)
  prod_quota_scens = subset(prod_quota_scens, select = -units)
  
  ## make sure quota is numeric
  prod_quota_scens[, quota := as.numeric(gsub(",", "", quota))]
  
  # ## load range of prod quotas
  # range_prod_quota_scens = fread(file.path(scen_path, range_prod_quota_file), header = T)
  # range_prod_quota_scens = range_prod_quota_scens[, units := NULL]
  # 
  # load excise tax file
  excise_tax_scens = fread(file.path(scen_path, excise_tax_file), header = T)
  excise_tax_scens = subset(excise_tax_scens, select = -units)
  
  # # load range of excise tax values
  # range_excise_tax_scens = fread(file.path(scen_path, range_excise_tax_file), header = T)
  # range_excise_tax_scens = range_excise_tax_scens[, units := NULL]
  # 
  # pad field codes with leading zeroes ------
  
  price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
  resource_data[, doc_field_code := sprintf("%03d", doc_field_code)]
  ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]
  # setback_scens[, doc_field_code := sprintf("%03d", doc_field_code)]
  
  # create datatable of forecasted input variables -----
  
  vars_dt = merge(price_data[year >= 2020], resource_data, by = c('doc_field_code'))
  vars_dt = merge(vars_dt, ghg_factors, by = c('doc_field_code'))
  setcolorder(vars_dt, c('doc_field_code', 'doc_fieldname', 'year', 
                         'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 
                         'upstream_kgCO2e_bbl'))
  
  
  ## list through each oil price scenario ------
  
  scenarios_dt = vars_dt[oilpx_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[innovation_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[carbonpx_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[ccs_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[setback_scens, on = .(doc_field_code), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[prod_quota_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[excise_tax_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  
  scenarios_dt[, tax := tax_rate * oil_price_usd_per_bbl]
  scenarios_dt[, tax_rate:=NULL]
  
  # keep diagnostics only (if that is input) ------
  
  if (oil_px_selection == 'diagnostic') {
    
    scenarios_dt = scenarios_dt[(oil_price_scenario == 'iea oil price' & 
                                   innovation_scenario == 'low innovation' & 
                                   carbon_price_scenario == 'price floor' & 
                                   ccs_scenario == 'medium CCS cost' &
                                   excise_tax_scenario == 'no tax' &
                                   setback_scenario == 'no_setback' &
                                   prod_quota_scenario == 'no quota') |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'quota_20') |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'setback_2500ft' &
                                     prod_quota_scenario == 'quota_20')]
  }
  
  if (oil_px_selection == 'benchmark') {
    
    scenarios_dt = scenarios_dt[(innovation_scenario == 'low innovation' & 
                                   carbon_price_scenario == 'price floor' & 
                                   ccs_scenario == 'medium CCS cost' &
                                   excise_tax_scenario == 'no tax' &
                                   setback_scenario == 'no_setback' &
                                   prod_quota_scenario == 'no quota') | ## all oil scenarios, hold everything else BAU
                                  (oil_price_scenario == 'iea oil price' & 
                                     # innovation_scenario == 'low innovation' &  ## all innovation scenarios, everything else BAU
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota') |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     # carbon_price_scenario == 'price floor' & ## all carbon scenarios, everything else BAU
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota') |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price ceiling' & 
                                     # ccs_scenario == 'medium CCS cost' & ## all CCS
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota') |
                                (oil_price_scenario == 'iea oil price' & 
                                    innovation_scenario == 'low innovation' & 
                                    carbon_price_scenario == 'price floor' & 
                                    ccs_scenario == 'medium CCS cost' & 
                                    # excise_tax_scenario == 'no tax' & ## all tax
                                    setback_scenario == 'no_setback' &
                                    prod_quota_scenario == 'no quota')  |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' & 
                                     excise_tax_scenario == 'no tax' & 
                                     # setback_scenario == 'setback_2500ft' & ## all setback
                                     prod_quota_scenario == 'no quota') |
                                  (oil_price_scenario == 'iea oil price' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' & 
                                     excise_tax_scenario == 'no tax' & 
                                     setback_scenario == 'no_setback') ] ## all quota
  }
  
  
  
  # reorder columns -----
  
  setcolorder(scenarios_dt, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                              'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                              'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg',
                              'area_coverage', 'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 
                              'wm_capex_imputed', 'resource', 'upstream_kgCO2e_bbl'))
  
  return(scenarios_dt)
}

  
# remove all objects except one -------
  
  # rm(list=setdiff(ls(), 'scenarios_dt'))
  