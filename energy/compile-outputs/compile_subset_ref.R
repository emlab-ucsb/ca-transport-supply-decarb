## scenario list for refining
## October 22, 2021

## libraries
library(data.table)
library(tidyverse)
library(readxl)
library(openxlsx)


# paths -----
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
refining_out_path = 'predict-production/refining_2021-10-26/CUF0.6/outputs'

## file names
refining_file     = 'refining_scenario_outputs_refinery_net_exports_revised.csv'

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

## find scen selection
carbon_subset_vec <- c("price floor", "price ceiling", "central SCC")
carbon_scens_vec <- c("carbon_setback_1000ft", "carbon_setback_5280ft", "carbon_90_perc_reduction", "central SCC")
ccs_subset_vec <- c("no ccs", "medium CCS cost", "high CCS cost", "medium CCS cost - 45Q - LCFS", "high CCS cost - 45Q - LCFS")
tax_subset_vec <- c("tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_5280ft", "tax_90_perc_reduction")

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

