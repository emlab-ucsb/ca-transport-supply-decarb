## Tracey Mangin
## September 2, 2021
## scenario list

## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)
library(rebus)


# paths -----
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
academic_out      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/academic-out/extraction/'

## file names  
oil_price_file    = 'oil_price_projections_revised.xlsx'
inn_file          = 'innovation_scenarios.csv'
carbon_file       = 'final_carbon_tax_scenarios.csv' ## includes equiv setback and 90%
ccs_ext_file      = 'ccs_extraction_scenarios_revised.csv'
setback_file      = 'setback_coverage_R.csv'
prod_quota_file   = 'prod_quota_scenarios_with_sb.csv' ## two setback scenarios added
excise_tax_file   = 'final_excise_tax_scenarios.csv' ## includes equiv setback and 90%
incentive_file    = 'CCS_LCFS_45Q.xlsx'

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

## carbon px - non targets
carbon_px_vec <- c("price floor", "price ceiling", "central SCC")

carbon_target_vec <- c("carbon_setback_1000ft-no_setback-medium CCS cost",
                       "carbon_setback_2500ft-no_setback-medium CCS cost",
                       "carbon_setback_5280ft-no_setback-medium CCS cost",
                       "carbon_90_perc_reduction-no_setback-medium CCS cost",
                       "carbon_setback_1000ft-no_setback-no ccs",
                       "carbon_setback_2500ft-no_setback-no ccs",
                       "carbon_setback_5280ft-no_setback-no ccs",
                       "carbon_90_perc_reduction-no_setback-no ccs")

carbon_sb_target_vec <- c("carbon_sb_90_perc_reduction-setback_1000ft-medium CCS cost",
                          "carbon_sb_90_perc_reduction-setback_1000ft-no ccs",
                          "carbon_sb_90_perc_reduction-setback_2500ft-no ccs",
                          "carbon_sb_90_perc_reduction-setback_2500ft-medium CCS cost",
                          "carbon_sb_90_perc_reduction-setback_5280ft-no ccs",
                          "carbon_sb_90_perc_reduction-setback_5280ft-medium CCS cost")

## target excise taxes
tax_px_vec <- c("no tax", "tax_0.05", "tax_0.1", "tax_0.5", "tax_0.9", "tax_1")

tax_target_vec <- c("tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_5280ft", "tax_90_perc_reduction")

## scenario combinations ----------

## first exclude the targets
scen_sel <- expand.grid(oil_price_scenario = unique(oilpx_scens_names[, oil_price_scenario]),
                        setback_scenario = unique(setback_scens_name[, setback_scenario]), 
                        prod_quota_scenario = unique(prod_quota_scens_names[, prod_quota_scenario]),
                        carbon_price_scenario = c(carbon_px_vec),
                        ccs_scenario = unique(ccs_scens_names[, ccs_scenario]), 
                        innovation_scenario = unique(innovation_scens_name[, innovation_scenario]), 
                        excise_tax_scenario = c(tax_px_vec))

setDT(scen_sel)

## add ID column
scen_sel[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                            carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "_")]

setcolorder(scen_sel, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                        'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))


## remove production quota
scen_sel = scen_sel[prod_quota_scenario == 'no quota']

## remove no ccs with incentives
scen_sel = scen_sel[!ccs_scenario %in% c('no ccs - 45Q', 'no ccs - 45Q - LCFS')]

## refactor
scen_sel$ccs_scenario <- droplevels(scen_sel$ccs_scenario)


## bau scen ids
scen_sel[, BAU_scen := fifelse((oil_price_scenario == 'reference case' & 
                                innovation_scenario == 'low innovation' & 
                                carbon_price_scenario == 'price floor' & 
                                ccs_scenario == 'no ccs' &
                                excise_tax_scenario == 'no tax' &
                                setback_scenario == 'no_setback' &
                                prod_quota_scenario == 'no quota'), 1, 0)]


## create dfs for target scenarios, bind to main df
## -------------------------------------------------

## excise targets
## -----------------------
excise_target_df <- expand.grid(oil_price_scenario = "reference case",
                                setback_scenario = "no_setback",
                                prod_quota_scenario = "no quota",
                                carbon_price_scenario = "price floor",
                                ccs_scenario = c("no ccs", "medium CCS cost"),
                                innovation_scenario = "low innovation",
                                excise_tax_scenario = tax_target_vec,
                                BAU_scen = 0)

setDT(excise_target_df)

excise_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                  carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "_")]

setcolorder(excise_target_df, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                        'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))

## carbon tax targets
## ---------------------------------
carbon_target_df <- expand.grid(oil_price_scenario = "reference case",
                                setback_scenario = "no_setback",
                                prod_quota_scenario = "no quota",
                                carbon_price_scenario = carbon_target_vec,
                                innovation_scenario = "low innovation",
                                excise_tax_scenario = "no tax",
                                BAU_scen = 0)

setDT(carbon_target_df)

## add ccs column
carbon_target_df[, ccs_scenario := sub('.*\\-', '', carbon_price_scenario)]

carbon_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                    carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "_")]

setcolorder(carbon_target_df, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                                'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))


## carbon - setback targets
## ----------------------------------
carbon_sb_target_df <- expand.grid(oil_price_scenario = "reference case",
                                # setback_scenario = setback_scens_name,
                                prod_quota_scenario = "no quota",
                                carbon_price_scenario = carbon_sb_target_vec,
                                innovation_scenario = "low innovation",
                                excise_tax_scenario = "no tax",
                                BAU_scen = 0)

setDT(carbon_sb_target_df)

## add ccs column
carbon_sb_target_df[, ccs_scenario := sub('.*\\-', '', carbon_price_scenario)]


## add setback scneario
carbon_sb_target_df[, setback_scenario := str_extract(carbon_price_scenario, pattern = 'setback_' %R% one_or_more(DIGIT) %R% 'ft')]

carbon_sb_target_df[, scen_id := paste(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                    carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario, sep = "_")]

setcolorder(carbon_sb_target_df, c('scen_id', 'oil_price_scenario', 'setback_scenario', 'prod_quota_scenario',
                                'carbon_price_scenario', 'ccs_scenario', 'innovation_scenario', 'excise_tax_scenario'))

## bind
target_scens <- rbind(excise_target_df, carbon_target_df, carbon_sb_target_df)

## add target scens to scen sel
## --------------------------------------------------
scen_sel <- rbind(scen_sel, target_scens)

## find scen selection
## ------------------------------

## 
setback_dt = unique(scen_sel[(innovation_scenario == 'low innovation' &
                       carbon_price_scenario %in% carbon_px_vec &
                       ccs_scenario %in% c("medium CCS cost", "no ccs") & 
                       excise_tax_scenario == 'no tax' &
                       prod_quota_scenario == 'no quota' &
                       oil_price_scenario == 'reference case') |
                      (innovation_scenario == 'low innovation' &
                        carbon_price_scenario == 'price floor' &
                        ccs_scenario %in% c("medium CCS cost", "no ccs") &
                        excise_tax_scenario == 'no tax' &
                        prod_quota_scenario == 'no quota')])



## bind
selected_scens = rbind(setback_dt, target_scens)
selected_scens = distinct(selected_scens)


## indicate scenarios
scen_sel[, subset_scens := fifelse(scen_id %in% selected_scens[, scen_id], 1, 0)]


fwrite(scen_sel, file.path(academic_out, 'scenario_id_list.csv'), row.names = F)

