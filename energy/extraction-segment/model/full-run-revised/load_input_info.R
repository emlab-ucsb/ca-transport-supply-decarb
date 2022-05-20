## Tracey Mangin
## February 1, 2022
## model: load input info

# paths -----
model_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
academic_out      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'

# file names  
entry_file        = 'stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
coef_file         = 'poisson_regression_coefficients_revised.csv'
param_file        = 'forecasted_decline_parameters_2020_2045.csv' 
peak_file         = 'field-year_peak-production_yearly.csv' 
prod_vintage_file = 'pred_prod_no_exit_2020-2045_field_start_year_revised.csv' 
histprod_file     = 'crude_prod_x_field_revised.csv'
exit_file         = 'exit_regression_coefficients.csv'
ccs_capture_rate  = 0.61
forecast_file     = 'field_capex_opex_forecast_final.csv'
resource_file     = 'field_resource.csv'
oil_price_file    = 'oil_price_projections_revised.xlsx'
inn_file          = 'innovation_scenarios.csv'
carbon_file       = 'carbon_prices_revised.csv' 
ccs_ext_file      = 'ccs_extraction_scenarios_revised.csv' ## revised includes ccs cost = inf
ghg_file          = 'ghg_emissions_x_field_2018-2045.csv'
setback_file      = 'setback_coverage_R.csv'
prod_quota_file   = 'prod_quota_scenarios_with_sb.csv'
excise_tax_file   = 'excise_tax_non_target_scens.csv' 
incentive_file    = 'CCS_LCFS_45Q.xlsx'
n_wells_file      = 'n_wells_area.csv'
# scen_id_file      = 'scenario_id_list_targets.csv'
emis_reduc_file     = 'emission_reduction_90.csv'


# source from other scripts -----

# source function to rank costs
# source(here::here('energy', 'extraction-segment', 'prod_quota.R'))

# source ccs emissions mean b calculation script
source(here::here('energy', 'scenario-prep', 'ccs_parameterization.R'))

## source all functions for optimization
items <- list.files(here::here("energy", "extraction-segment", "model", "full-run-revised", "target-functions"))
walk(items, ~ here::here("energy", "extraction-segment", "model", "full-run-revised", "target-functions", .x) %>% source()) 

# # source function to create matrix of scenarios and forecasted variables
#   source(here::here('energy', 'extraction-segment', 'full-run', 'fun_input_scenarios_full.R'))

# # source function to filter scenario selection
# source(here::here('energy', 'extraction-segment', 'full-run', 'fun_filter_scenarios.R'))

## functions and info for calculating ccs info
a = 4

solve_tc <- function(a, b, q) {
  f <- (q*(a*b - a*(q^(1/a)) + b))/(a + 1)
  return(f)
}

## exit function
calc_num_well_exits <- function(fe_val, bhat, p_oil, op_hat, opex_val, dhat, depl_val) {
  
  n_well_exit = exp(bhat * p_oil + op_hat * opex_val + dhat * depl_val) * fe_val 
  
}


# load data -----

## scen id list
# scen_id_list = fread(file.path(academic_out, scen_id_file), header = T)

## emission reduction df
emis_reduc_90 = fread(paste0(scen_path, emis_reduc_file), header = T)
emis_reduc_90_val = emis_reduc_90[, ghg_emission_MtCO2e][1]

# load oil price data
oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## load innovation scenarios
innovation_scens = fread(file.path(scen_path, inn_file), header = T)

## load carbon px scens
carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
# carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
carbonpx_scens[, carbon_price_usd_per_kg := carbon_price/1000] # convert from usd per metric ton to usd per kg
carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]

## load ccs scenarios
ccs_scens = fread(file.path(scen_path, ccs_ext_file), header = T)
ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('no ccs', 'high CCS cost', 'medium CCS cost', 'low CCS cost'))]

## load price data
price_data = fread(file.path(outputs_path, 'stocks-flows', forecast_file), header = T)

## load resource data
resource_data = fread(file.path(outputs_path, 'entry-model-results', resource_file), header = T)
resource_data = resource_data[, c('doc_field_code', 'resource')]

## oad ghg factors
ghg_factors = fread(file.path(outputs_path, 'stocks-flows', ghg_file), header = T)
# ghg_factors = ghg_factors[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]

# load n wells in setbacks and setback coverage file
n_wells_setbacks = fread(file.path(outputs_path, 'predict-production', 'existing_production', n_wells_file), header = T, colClasses = c('doc_field_code' = 'character'))

## load setback scenarios
setback_scens = fread(file.path(outputs_path, 'setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))
setback_scens[, scen_area_m2 := orig_area_m2 *  (1 - rel_coverage)]
setback_scens <- setback_scens[, c("doc_field_code", "setback_scenario", "orig_area_m2", "scen_area_m2", "rel_coverage")]
setnames(setback_scens, 'rel_coverage', 'area_coverage')

setback_scens = merge(setback_scens, n_wells_setbacks,
                      by = c('doc_field_code', 'setback_scenario'),
                      all = T)

setback_scens[, doc_fieldname := NULL]
setback_scens[, n_wells_in_setback := NULL]

setnames(setback_scens, 'n_wells', 'n_wells_start')
setnames(setback_scens, 'adj_no_wells', 'n_wells_setback')

setback_scens[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]


# load production quota file
prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)

# load excise tax file
excise_tax_scens = fread(file.path(scen_path, excise_tax_file), header = T)
excise_tax_scens = subset(excise_tax_scens, select = -units)

# load ccs incentives file 
incentives_scens = setDT(read.xlsx(file.path(data_path, incentive_file), sheet = 'scenarios', cols = c(1:3)))

# pad field codes with leading zeroes ------
price_data[, doc_field_code := sprintf("%03d", doc_field_code)]
resource_data[, doc_field_code := sprintf("%03d", doc_field_code)]
ghg_factors[, doc_field_code := sprintf("%03d", doc_field_code)]

# create datatable of forecasted input variables -----

vars_dt = merge(price_data[year >= 2020], resource_data, by = c('doc_field_code'))
vars_dt = merge(vars_dt, ghg_factors[year >= 2020], by = c('doc_field_code', 'year'))
setcolorder(vars_dt, c('doc_field_code', 'doc_fieldname', 'year', 
                       'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 'wm_capex_imputed', 'resource', 
                       'upstream_kgCO2e_bbl'))

# create adjusted ccs costs ------

ccs_scens_adj = ccs_scens[incentives_scens, on = .(year), allow.cartesian = T, nomatch = 0]
ccs_scens_adj[, ccs_scenario_adj := fcase(incentive_scenario == 'no incentives', paste0(ccs_scenario),
                                          incentive_scenario == '45Q only', paste0(ccs_scenario, ' - 45Q'),
                                          incentive_scenario == '45Q + LCFS', paste0(ccs_scenario, ' - 45Q - LCFS'))]


## remove redundant scenarios
ccs_scens_adj = ccs_scens_adj[!ccs_scenario_adj %in% c('no ccs - 45Q', 'no ccs - 45Q - LCFS')]


# adjust ccs price with incentives
ccs_scens_adj[, ccs_price_usd_per_kg_adj := ccs_price_usd_per_kg - (incentive_price/1e3)]

# create constrained version 
ccs_scens_neg = ccs_scens_adj[ccs_scenario_adj %in% unique(ccs_scens_adj[ccs_price_usd_per_kg_adj < 0, ccs_scenario_adj])]
ccs_scens_neg[, ccs_scenario_adj := paste0(ccs_scenario_adj, ' (constrained)') ]
ccs_scens_neg[, ccs_price_usd_per_kg_adj := fifelse(ccs_price_usd_per_kg_adj < 0, 0, ccs_price_usd_per_kg_adj)]

# combine ccs scenarios
ccs_scens_all = rbind(ccs_scens_adj, ccs_scens_neg)

# select columns 
ccs_scens_all = ccs_scens_all[, .(year, ccs_scenario_adj, ccs_price_usd_per_kg_adj)]
setnames(ccs_scens_all, c('ccs_scenario_adj', 'ccs_price_usd_per_kg_adj'), c('ccs_scenario', 'ccs_price_usd_per_kg'))

## ----------------------------------------
## ----------------------------------------


# load entry data
entry_dt = fread(file.path(model_path, entry_file), header = T, colClasses = c('doc_field_code' = 'character'))

# # load matrix of scenarios and forecasted variables
# scenarios_dt = load_scenarios_dt(scenario_selection)

# load coefficients from poisson regression of historic data
coefs_dt = fread(file.path(model_path, 'entry-model-results', coef_file), header = T, colClasses = c('doc_field_code' = 'character'))
coefs_dt = unique(coefs_dt)

# load decline parameters  
decline_dt = fread(file.path(model_path, 'decline-historic', 'parameters', param_file), header = T, colClasses = c('doc_field_code' = 'character'))

# load peak production for each field
peak_dt = fread(file.path(model_path, 'decline-historic', 'data', peak_file), header = T, colClasses = c('doc_field_code' = 'character'))


# exit file
exit_coefs = fread(file.path(model_path, 'exit', exit_file), header = T, colClasses = c('doc_field_code' = 'character'))
exit_coefs[, doc_field_code := sprintf("%03s", doc_field_code)]
exit_coefs[, doc_fieldname := NULL]

# load forecasted production from existing (pre 2020) wells
prod_existing_vintage = fread(file.path(model_path, 'predict-production', 'existing_production', prod_vintage_file), header = T, colClasses = c('doc_field_code' = 'character'))
prod_existing_vintage[, vintage := as.character(start_year)]
prod_existing_vintage[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]


# load historic modeled well entry
# hist_modeled = fread(file.path(model_path, 'entry-model-results', hist_file), header = T)

# load historic production
prod_hist = fread(file.path(model_path, 'stocks-flows', histprod_file), header = T, colClasses = c('doc_field_code' = 'character'))

# # rename FieldCode -> doc_field_code -----
# 
#   setnames(decline_dt, "FieldCode", "doc_field_code")
#   setnames(peak_dt, "FieldCode", "doc_field_code")
#   setnames(prod_hist, "FieldCode", "doc_field_code")

# # rename FieldName -> doc_fieldname -----
#   setnames(decline_dt, "FieldName", "doc_fieldname")
#   setnames(peak_dt, "FieldName", "doc_fieldname")

# pad field codes with leading zeroes -----
coefs_dt[, doc_field_code := sprintf("%03s", doc_field_code)]

# get peak production median of last two vintages -----

# meas-note: so this next line calculates the median of the peak production (per well) of the last few vintages to apply to new well entries.
# meas-note: i would change to use the last few start-years (maybe 2000 onwards? or something like that) instead
peak_prod_median = peak_dt[start_year >= 2000, 
                           lapply(.SD, median, na.rm = TRUE), .SDcols = c("peak_tot_prod", "peak_avg_well_prod", "peak_well_prod_rate"),
                           by = .(doc_field_code)]

# from entry data, keep: field, new wells, depletion ------

setnames(entry_dt, 'n_new_wells', 'new_wells')
setnames(entry_dt, 'm_cumsum_div_my_prod', 'depl')
entry_dt_vars = entry_dt[, .(doc_field_code, doc_fieldname, year, brent, capex_imputed, opex_imputed, depl, new_wells)]
entry_dt = entry_dt[, .(doc_field_code, doc_fieldname, year, new_wells, depl)]

# ccs emissions scalar ---------

ccs_ghg_scalar <- 1 - ccs_capture_rate
