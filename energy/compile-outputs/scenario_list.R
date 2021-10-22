## Tracey Mangin
## September 2, 2021
## scenario list

## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)


# paths -----
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
academic_out      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'
refining_out_path = 'predict-production/refining_2021-09-06/CUF0.6/outputs'

## file names  
oil_price_file    = 'oil_price_projections_revised.xlsx'
inn_file          = 'innovation_scenarios.csv'
carbon_file       = 'final_carbon_tax_scenarios.csv' ## includes equiv setback and 90%
ccs_ext_file      = 'ccs_extraction_scenarios_revised.csv'
setback_file      = 'setback_coverage_R.csv'
prod_quota_file   = 'prod_quota_scenarios_with_sb.csv' ## two setback scenarios added
excise_tax_file   = 'final_excise_tax_scenarios.csv' ## includes equiv setback and 90%
incentive_file    = 'CCS_LCFS_45Q.xlsx'
refining_file     = 'refining_scenario_outputs_refinery_net_exports_revised.csv'

# load data -----

## load oil price data
oilpx_scens = setDT(read.xlsx(file.path(data_path, oil_price_file), sheet = 'nominal', cols = c(1, 7:9)))
colnames(oilpx_scens) = c('year', 'reference_case', 'high_oil_price', 'low_oil_price')
oilpx_scens = melt(oilpx_scens, measure.vars = c('reference_case', 'high_oil_price', 'low_oil_price'), 
                   variable.name = 'oil_price_scenario', value.name = 'oil_price_usd_per_bbl')
oilpx_scens[, oil_price_scenario := gsub('_', ' ', oil_price_scenario)]
oilpx_scens[, oil_price_scenario := factor(oil_price_scenario, levels = c('reference case', 'high oil price', 'low oil price'))]
oilpx_scens <- oilpx_scens[year > 2019]
setorderv(oilpx_scens, c('oil_price_scenario', 'year'))

## unique oil px scens
oilpx_scens_names <- distinct(oilpx_scens[, .(oil_price_scenario)])

## load innovation scenarios
innovation_scens = fread(file.path(scen_path, inn_file), header = T)

innovation_scens_name <- distinct(innovation_scens[, .(innovation_scenario)])

## load carbon px scens
carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)

carbonpx_scens_name <- distinct(carbonpx_scens[, .(carbon_price_scenario)])

## load ccs scenarios
ccs_scens = fread(file.path(scen_path, ccs_ext_file), header = T)
ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('no ccs', 'high CCS cost', 'medium CCS cost', 'low CCS cost'))]


## load setback scenarios
setback_scens = fread(file.path(outputs_path, 'setback', 'model-inputs', setback_file), header = T, colClasses = c('doc_field_code' = 'character'))

setback_scens[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]

setback_scens_name <- distinct(setback_scens[, .(setback_scenario)])


# load production quota file
prod_quota_scens = fread(file.path(scen_path, prod_quota_file), header = T)

prod_quota_scens_names <- distinct(prod_quota_scens[, .(prod_quota_scenario)])

# load excise tax file
excise_tax_scens = fread(file.path(scen_path, excise_tax_file), header = T)

excise_tax_scens_name <- distinct(excise_tax_scens[, .(excise_tax_scenario)])

# load ccs incentives file 
incentives_scens = setDT(read.xlsx(file.path(data_path, incentive_file), sheet = 'scenarios', cols = c(1:3)))

# create adjusted ccs costs ------

ccs_scens_adj = ccs_scens[incentives_scens, on = .(year), allow.cartesian = T, nomatch = 0]
ccs_scens_adj[, ccs_scenario_adj := fcase(incentive_scenario == 'no incentives', paste0(ccs_scenario),
                                          incentive_scenario == '45Q only', paste0(ccs_scenario, ' - 45Q'),
                                          incentive_scenario == '45Q + LCFS', paste0(ccs_scenario, ' - 45Q - LCFS'))]


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

ccs_scens_names <- distinct(ccs_scens_all[, .(ccs_scenario)])

## ----------------------------------------
## ----------------------------------------


## scenario combinations ----------
scen_sel <- expand.grid(oil_price_scenario = unique(oilpx_scens_names[, oil_price_scenario]),
                        setback_scenario = unique(setback_scens_name[, setback_scenario]), 
                        prod_quota_scenario = unique(prod_quota_scens_names[, prod_quota_scenario]),
                        carbon_price_scenario = unique(carbonpx_scens_name[, carbon_price_scenario]),
                        ccs_scenario = unique(ccs_scens_names[, ccs_scenario]), 
                        innovation_scenario = unique(innovation_scens_name[, innovation_scenario]), 
                        excise_tax_scenario = unique(excise_tax_scens_name[, excise_tax_scenario]))

setDT(scen_sel)

## add ID column
scen_sel[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                            carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "_")]

setcolorder(scen_sel, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                        'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))


## filter scen_sel for appropriate set of scenarios
scen_sel = scen_sel[prod_quota_scenario == 'no quota']

## bau scen ids
scen_sel[, BAU_scen := fifelse((oil_price_scenario == 'reference case' & 
                                innovation_scenario == 'low innovation' & 
                                carbon_price_scenario == 'price floor' & 
                                ccs_scenario == 'medium CCS cost' &
                                excise_tax_scenario == 'no tax' &
                                setback_scenario == 'no_setback' &
                                prod_quota_scenario == 'no quota'), 1, 0)]

## find scen selection
carbon_subset_vec <- c("price floor", "price ceiling", "central SCC")
carbon_scens_vec <- c("carbon_setback_1000ft", "carbon_setback_5280ft", "carbon_90_perc_reduction", "central SCC")
ccs_subset_vec <- c("no ccs", "medium CCS cost", "high CCS cost", "medium CCS cost - 45Q - LCFS", "high CCS cost - 45Q - LCFS")
tax_subset_vec <- c("tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_5280ft", "tax_90_perc_reduction")

setback_dt = scen_sel[(innovation_scenario == 'low innovation' &
                           carbon_price_scenario %in% carbon_subset_vec &
                           ccs_scenario == 'medium CCS cost' & 
                           excise_tax_scenario == 'no tax' &
                           prod_quota_scenario == 'no quota' &
                           oil_price_scenario == 'reference case') |
                          (innovation_scenario == 'low innovation' &
                             carbon_price_scenario == 'price floor' &
                             ccs_scenario %in% ccs_subset_vec & 
                             excise_tax_scenario == 'no tax' &
                             prod_quota_scenario == 'no quota' &
                             oil_price_scenario == 'reference case') |
                          (innovation_scenario == 'low innovation' &
                             carbon_price_scenario == 'price floor' &
                             ccs_scenario == "medium CCS cost" & 
                             excise_tax_scenario == 'no tax' &
                             prod_quota_scenario == 'no quota')]

tax_dt = scen_sel[(innovation_scenario == 'low innovation' &
                   carbon_price_scenario %in% carbon_subset_vec &
                   ccs_scenario == 'medium CCS cost' & 
                   excise_tax_scenario %in% tax_subset_vec &
                   prod_quota_scenario == 'no quota' &
                   oil_price_scenario == 'reference case' &
                   setback_scenario == 'no_setback') |
                  (innovation_scenario == 'low innovation' &
                     carbon_price_scenario == 'price floor' &
                     ccs_scenario %in% ccs_subset_vec & 
                     excise_tax_scenario %in% tax_subset_vec &
                     prod_quota_scenario == 'no quota' &
                     oil_price_scenario == 'reference case' &
                     setback_scenario == 'no_setback') |
                  (innovation_scenario == 'low innovation' &
                     carbon_price_scenario == 'price floor' &
                     ccs_scenario == "medium CCS cost" & 
                     excise_tax_scenario %in% tax_subset_vec &
                     prod_quota_scenario == 'no quota' &
                     setback_scenario == 'no_setback')]

carbon_dt = scen_sel[(innovation_scenario == 'low innovation' &
                         carbon_price_scenario %in% carbon_scens_vec &
                         ccs_scenario == 'medium CCS cost' & 
                         excise_tax_scenario == "no tax" &
                         prod_quota_scenario == 'no quota' &
                         setback_scenario == 'no_setback') |
                        (innovation_scenario == 'low innovation' &
                           carbon_price_scenario %in% carbon_scens_vec &
                           ccs_scenario %in% ccs_subset_vec & 
                           excise_tax_scenario == "no tax" &
                           prod_quota_scenario == 'no quota' &
                           oil_price_scenario == 'reference case' &
                           setback_scenario == 'no_setback')]


## bind
selected_scens = rbind(setback_dt, tax_dt, carbon_dt)
selected_scens = distinct(selected_scens)


## indicate scenarios
scen_sel[, subset_scens := fifelse(scen_id %in% selected_scens[, scen_id], 1, 0)]

fwrite(scen_sel, file.path(academic_out, 'scenario_id_list.csv'), row.names = F)

## refining scenarios
## -----------------------------------------------

## load refining outputs
refining_out <- fread(file.path(outputs_path, refining_out_path, refining_file))

## select scenarios
refining_scens <- unique(refining_out[, .(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)])

## add oil price
refining_scens <- crossing(refining_scens, oilpx_scens_names)

## add scen_id
refining_scens <- refining_scens %>%
  mutate(scen_id = paste(demand_scenario, refining_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, oil_price_scenario)) %>%
  select(scen_id, demand_scenario:oil_price_scenario) %>%
  as.data.table()

## add bau
refining_scens[, BAU_scen := fifelse((oil_price_scenario == 'reference case' & 
                                     innovation_scenario == 'low innovation' & 
                                     carbon_price_scenario == 'price floor' & 
                                     ccs_scenario == 'medium CCS cost' &
                                     demand_scenario == 'BAU' &
                                     refining_scenario == 'historic exports'), 1, 0)]

# ## find scen selection
# carbon_subset_vec <- c("price floor", "price ceiling", "central SCC")
# carbon_scens_vec <- c("carbon_setback_1000ft", "carbon_setback_5280ft", "carbon_90_perc_reduction", "central SCC")
# ccs_subset_vec <- c("medium CCS cost", "high CCS cost", "medium CCS cost - 45Q - LCFS", "high CCS cost - 45Q - LCFS")
# tax_subset_vec <- c("tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_5280ft", "tax_90_perc_reduction")

## find subset scenarios
ref_carbon_dt = refining_scens[(innovation_scenario == 'low innovation' &
                                carbon_price_scenario %in% c(carbon_subset_vec, carbon_scens_vec) &
                                ccs_scenario == 'medium CCS cost') |
                               (innovation_scenario == 'low innovation' &
                                carbon_price_scenario %in% c(carbon_subset_vec, carbon_scens_vec) &
                                ccs_scenario %in% ccs_subset_vec & 
                                oil_price_scenario == 'reference case')]

## indicate scenarios
refining_scens[, subset_scens := fifelse(scen_id %in% ref_carbon_dt[, scen_id], 1, 0)]

fwrite(refining_scens, file.path(outputs_path, 'academic-out/refining/ref_scenario_id_list.csv'), row.names = F)
