## Tracey Mangin
## August 13, 2021
## Determine quota scenarios that result in setback endpoints

library(tidyverse)
library(data.table)

## paths
calepa_path <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/"
extraction_path <- "outputs/predict-production/extraction_2021-08-20/tax_update_correction/"
inputs_path <- "project-materials/scenario-inputs/"

## file names
state_results_file  <- "benchmark-state-level-results.csv"
prod_quota_file   <- 'prod_quota_scenarios.csv'

# load production quota file
prod_quota_scens = fread(paste0(calepa_path, inputs_path, prod_quota_file), header = T)
prod_quota_scens = subset(prod_quota_scens, select = -units)
prod_quota_scens[, quota := str_remove_all(quota, ",")]
prod_quota_scens[, quota := as.numeric(quota)]

## load benchmark
state_out <- fread(paste0(calepa_path, extraction_path, state_results_file))

## filter for setback scenarios
setback_out <- state_out[(oil_price_scenario == 'reference case' & 
                            innovation_scenario == 'low innovation' & 
                            carbon_price_scenario == 'price floor' & 
                            ccs_scenario == 'medium CCS cost' & 
                            excise_tax_scenario == 'no tax' & 
                            prod_quota_scenario == 'no quota' &
                            year == 2045)]

setback_out <- setback_out[setback_scenario %in% c("setback_1000ft", "setback_2500ft", "setback_5280ft"), .(setback_scenario, total_prod_bbl)]

## add setback scenarios
orig_val <- 155747231

quota_scens <- tibble(year = 2019:2045,
                      setback_1000_quota = seq(orig_val, setback_out$total_prod_bbl[1], length.out = length(2019:2045)),
                      setback_2500_quota = seq(orig_val, setback_out$total_prod_bbl[2], length.out = length(2019:2045)),
                      setback_5280_quota = seq(orig_val, setback_out$total_prod_bbl[3], length.out = length(2019:2045)))

quota_scens <- quota_scens %>%
  filter(year > 2019) %>%
  pivot_longer(setback_1000_quota:setback_5280_quota, names_to = "prod_quota_scenario", values_to = "quota") %>%
  mutate(quota = round(quota)) %>%
  rbind(prod_quota_scens)

fwrite(quota_scens, paste0(calepa_path, inputs_path, "prod_quota_scenarios_with_sb.csv"), row.names = F)

