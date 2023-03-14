# preparation of price and cost variables under various scenarios to use in entry forecast
# created: september 23, 2020
# author: meas meng

# inputs -----

  outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
  data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
  scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
  forecast_file     = 'field_capex_opex_forecast_revised.csv'
  resource_file     = 'field_resource.csv'
  # brent_file        = 'brent_oil_price_projections.csv'
  # brent_file        = 'brent_oil_price_projections_real.xlsx'
  oil_price_file        = 'oil_price_projections.xlsx'
  inn_file          = 'innovation_scenarios.csv'
  carbon_file       = 'carbon_prices.csv'
  ccs_ext_file      = 'ccs_extraction_scenarios.csv'
  ccs_ref_file      = 'ccs_refining_scenarios.csv'
  ghg_file          = 'ghg_emissions_x_field_2015.csv'
  setback_file      = 'setback_coverage.csv'
  prod_quota_file   = 'prod_quota_scenarios.csv'
  excise_tax_file   = 'excise_tax_scenarios.csv'
  
## inputs for full range of quota and excise tax values
  # range_prod_quota_file   = 'prod_quota_scenarios_fullrange.csv'
  # range_excise_tax_file   = 'excise_tax_scenarios_fullrange.csv'
  
# outputs -----
  
  # save_bounding_csv     = 'input_variables_scenarios_bounding.csv'
  save_file_csv         = 'input_variables_scenarios.csv'
  save_file_csv_subset  = 'input_variables_scenarios_subset.csv'
  save_file_rds         = 'input_variables_scenarios.rds'
  save_file_rds_subset  = 'input_variables_scenarios_subset.rds'
  save_file_ccs_carbon  = 'input_variables_ccs_and_carbon_only.csv'
  save_file_csv_refine  = 'input_variables_scenarios_refining.csv'
  save_file_csv_bau     = 'input_variables_scenarios_bau.csv'
  
# load packages -----
  
  library(data.table)
  library(openxlsx)
  library(tidyverse)

# load data -----
  
  oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = 1:5))
  colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price') #'bp_oil_price'
  oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price', 'iea_oil_price'), 
                     variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
  oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
  oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price', 'iea oil price'))]
  setorderv(oilpx_scens, c('oil_price_scenario', 'year'))
  
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

  price_data = fread(file.path(outputs_path, 'stocks-flows', 'entry-input-df', 'final', forecast_file), header = T)
  
  resource_data = fread(file.path(outputs_path, 'entry-model-results', resource_file), header = T)
  resource_data = resource_data[, c('doc_field_code', 'resource')]
  
  ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T)
  ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]
  
  # load setback coverage file
  setback_scens = fread(file.path(outputs_path, 'setback', 'model-inputs', setback_file), header = T) %>%
    select(-V1) %>%
    rename(doc_field_code = FieldCode)
  
  # load production quota file
  prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)
  prod_quota_scens = subset(prod_quota_scens, select = -units)
  
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
  setback_scens[, doc_field_code := sprintf("%03d", doc_field_code)]
  
# create datatable of forecasted input variables -----
  
  vars_dt = merge(price_data[year >= 2020], resource_data, by = c('doc_field_code'))
  vars_dt = merge(vars_dt, ghg_factors, by = c('doc_field_code'))
  setcolorder(vars_dt, c('doc_field_code', 'doc_fieldname', 'year', 
                         'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 
                         'upstream_kgCO2e_bbl'))
  

# ## create "bounding" input sheet (prod quota and excise tax scenarios, no interaction, bau)  
#   
#   ## production quota
#     dt_quota = merge(vars_dt, oilpx_scens[oil_price_scenario == "iea oil price"], by = 'year')
#     dt_quota = merge(dt_quota, innovation_scens[innovation_scenario == "low innovation"], by = 'year', allow.cartesian = T)
#     dt_quota = merge(dt_quota, carbonpx_scens[carbon_price_scenario == "price floor"], by = 'year', allow.cartesian = T)
#     dt_quota = merge(dt_quota, ccs_scens[ccs_scenario == "medium CCS cost"], by = 'year', allow.cartesian = T)
#     dt_quota = merge(dt_quota, setback_scens[setback_scenario == "no_setback"], by = 'doc_field_code', allow.cartesian = T)
#     dt_quota = merge(dt_quota, range_prod_quota_scens, by = 'year', allow.cartesian = T)
#     dt_quota = merge(dt_quota, excise_tax_scens[excise_tax_scenario == "no tax"], by = 'year', allow.cartesian = T)
#     
#     # convert excise tax from rate (%) to $/bbl
#     dt_quota$tax = dt_quota$tax_rate * dt_quota$oil_price_usd_per_bbl
#     dt_quota[,tax_rate:=NULL]
# 
#   ## excise tax
#     dt_tax = merge(vars_dt, oilpx_scens[oil_price_scenario == "iea oil price"], by = 'year')
#     dt_tax = merge(dt_tax, innovation_scens[innovation_scenario == "low innovation"], by = 'year', allow.cartesian = T)
#     dt_tax = merge(dt_tax, carbonpx_scens[carbon_price_scenario == "price floor"], by = 'year', allow.cartesian = T)
#     dt_tax = merge(dt_tax, ccs_scens[ccs_scenario == "medium CCS cost"], by = 'year', allow.cartesian = T)
#     dt_tax = merge(dt_tax, setback_scens[setback_scenario == "no_setback"], by = 'doc_field_code', allow.cartesian = T)
#     dt_tax = merge(dt_tax, prod_quota_scens[prod_quota_scenario == "no quota"], by = 'year', allow.cartesian = T)
#     dt_tax = merge(dt_tax, range_excise_tax_scens, by = 'year', allow.cartesian = T)
#     
#     # convert excise tax from rate (%) to $/bbl
#     dt_tax$tax = dt_tax$tax_rate * dt_tax$oil_price_usd_per_bbl
#     dt_tax[,tax_rate:=NULL]
#   
# bounding_scen_dt <- rbind(dt_quota, dt_tax)
    
# save
  
# setcolorder(bounding_scen_dt, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
#                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
#                             'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg',
#                             'area_coverage', 'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 'upstream_kgCO2e_bbl'))
#   
# # export to csv -----
#   
# ## csv
# fwrite(bounding_scen_dt, file.path(scen_path, save_bounding_csv), row.names = F)
  

## all scenario combos for full scenario run
## list through each oil price scenario ------
  
  scens_oil_prices = unique(oilpx_scens[, oil_price_scenario])
  list_dts = list()
  
    for (i in seq_along(scens_oil_prices)) {
      
      dt = merge(vars_dt, oilpx_scens[oil_price_scenario == scens_oil_prices[i]], by = 'year')
      dt = merge(dt, innovation_scens, by = 'year', allow.cartesian = T)
      dt = merge(dt, carbonpx_scens, by = 'year', allow.cartesian = T)
      dt = merge(dt, ccs_scens, by = 'year', allow.cartesian = T)
      dt = merge(dt, setback_scens, by = 'doc_field_code', allow.cartesian = T)
      dt = merge(dt, prod_quota_scens, by = 'year', allow.cartesian = T)
      dt = merge(dt, excise_tax_scens, by = 'year', allow.cartesian = T)

      # convert excise tax from rate (%) to $/bbl
      dt$tax = dt$tax_rate*dt$oil_price_usd_per_bbl
      dt[,tax_rate:=NULL]
      
      # # adjust ghg emissions factor by innovation scenario
      #   dt[, upstream_kgCO2e_bbl_adj := upstream_kgCO2e_bbl*innovation_multiplier]
      # 
      # # adjust opex by innovation by innovation scenario
      #   dt[, m_opex_imputed_adj := m_opex_imputed*innovation_multiplier]
      #   dt[, wm_opex_imputed_adj := wm_opex_imputed*innovation_multiplier]
      # 
      # # determine if firm adopts ccs
      #   dt[, ccs_adoption := ccs_price_usd_per_kg - carbon_price_usd_per_kg]
      # 
      # # adjust opex depending on if firm adopts ccs. if adopts ccs, use ccs price as modification. if not, use carbon price as modification
      #   dt[, m_opex_imputed_adj := ifelse(ccs_adoption < 0,
      #                                     m_opex_imputed_adj + (ccs_price_usd_per_kg*upstream_kgCO2e_bbl_adj),
      #                                     m_opex_imputed_adj + (carbon_price_usd_per_kg*upstream_kgCO2e_bbl_adj))]
      #   dt[, wm_opex_imputed_adj := ifelse(ccs_adoption < 0,
      #                                      wm_opex_imputed_adj + (ccs_price_usd_per_kg*upstream_kgCO2e_bbl_adj),
      #                                      wm_opex_imputed_adj + (carbon_price_usd_per_kg*upstream_kgCO2e_bbl_adj))]
        
      # save to list
        list_dts[[i]] = dt
        
      rm(dt)
      
    }
  
  
# save matrix of scenarios and input variables as datatable -----
  
  scenarios_dt = rbindlist(list_dts)
  
  setcolorder(scenarios_dt, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                              'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                              'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg',
                              'area_coverage', 'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 'upstream_kgCO2e_bbl'))
  
# export to csv -----
  
  ## feather file
  write_feather(scenarios_dt, paste0(scen_path, "/", save_file_csv))
  
  # ## csv
  # fwrite(scenarios_dt, file.path(scen_path, save_file_csv), row.names = F)
  # 
  # ## rds
  # saveRDS(scenarios_dt, file = paste0(scen_path, "/", save_file_rds))
  
## scenario subset --------
  
  scenarios_subset_dt = scenarios_dt[(oil_price_scenario=="iea oil price"|oil_price_scenario=="high oil price")
                                     & innovation_scenario=="low innovation"
                                     & carbon_price_scenario=="price floor"
                                     & ccs_scenario=="high CCS cost"
                                     & (excise_tax_scenario=="no tax" | excise_tax_scenario=="medium tax")
                                     & (prod_quota_scenario=="no quota" | prod_quota_scenario=="medium quota")
                                     & (setback_scenario=="no_setback" | setback_scenario=="setback_2500ft")
                                        ]
  fwrite(scenarios_subset_dt, file.path(scen_path, save_file_csv_subset), row.names = F)
  saveRDS(scenarios_subset_dt, file = paste0(scen_path, "/", save_file_rds_subset))
  
  
# ccs costs and carbon prices only ------
  
  scenarios_ccs_carbon = unique(scenarios_dt[, .(year, doc_field_code, doc_fieldname, m_opex_imputed, m_capex_imputed, wm_opex_imputed, wm_capex_imputed,
                                                 carbon_price_scenario, ccs_scenario, carbon_price_usd_per_kg, ccs_price_usd_per_kg)])
  
  fwrite(scenarios_ccs_carbon, file.path(scen_path, save_file_ccs_carbon), row.names = F)
  
# scenarios that pertain to refining only -------
  
  # use new ccs costs for refineries
    ccs_scens_ref = fread(file.path(scen_path, ccs_ref_file), header = T)
    ccs_scens_ref[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
    ccs_scens_ref = ccs_scens_ref[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
    ccs_scens_ref[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  
  # merge scenarios
    scenarios_refine_dt = innovation_scens[carbonpx_scens, on = .(year), allow.cartesian = T]
    scenarios_refine_dt = scenarios_refine_dt[ccs_scens_ref, on = .(year), allow.cartesian = T]
    
    fwrite(scenarios_refine_dt, file.path(scen_path, save_file_csv_refine), row.names = F)
    
# bau, no policies --------  
    
    scenarios_bau_dt = scenarios_dt[oil_price_scenario=="iea oil price"
                                       & innovation_scenario=="low innovation"
                                       & carbon_price_scenario=="price floor"
                                       & ccs_scenario=="medium CCS cost"
                                       & excise_tax_scenario=="no tax"
                                       & prod_quota_scenario=="no quota"
                                       & setback_scenario=="no_setback"
    ]
    fwrite(scenarios_bau_dt, file.path(scen_path, save_file_csv_bau), row.names = F)
