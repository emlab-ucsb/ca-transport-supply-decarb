library(data.table)
library(tidyverse)

# 
base_path <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'


density_out = fread(file.path(base_path, 'extraction_2021-07-07', 'revised-density-calc', 'diagnostic-density-results.csv'), header = T, colClasses = c('doc_field_code' = 'character'))

field_out = fread(file.path(base_path, 'extraction_2021-07-07', 'revised-density-calc', 'diagnostic-field-level-results.csv'), header = T, colClasses = c('doc_field_code' = 'character'))

pos_field_dt = field_out[, .(prod = sum(total_prod_bbl, na.rm = T)), by = .(doc_field_code, oil_price_scenario,
                                                                            innovation_scenario, carbon_price_scenario,
                                                                            ccs_scenario, setback_scenario, prod_quota_scenario,
                                                                            excise_tax_scenario)]
density_out = merge(density_out, pos_field_dt,
                    by = c('doc_field_code', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                           'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'),
                    all = T)

density_out[, prod := fifelse(is.na(prod), 0, prod)]
density_out = density_out[prod > 0]



density_out[, scen_name := fifelse(setback_scenario == "setback_2500ft", "LCE2",
                                   fifelse(setback_scenario == "no_setback" & prod_quota_scenario == "quota_20", "LCE1", "BAU"))]

