## Tracey Mangin
## March 20, 2023
## make version of county-level outputs for zenodo

library(tidyverse)
library(data.table)

##
comp_result_date <- "2022-12-27"

## drive paths 
main_path              = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
extraction_folder_path = paste0('outputs/academic-out/extraction/extraction_', comp_result_date, '/')
county_save_path       = paste0(main_path, extraction_folder_path, 'county-results/')
save_info_path         = paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/final/')

## labor scen out
labor_scen_file <- "reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_county_results.rds"

## labor, county
labor_out      <- fread(paste0(county_save_path, 'subset_county_results.csv'))
labor_scen_out <- readRDS(paste0(main_path, "outputs/academic-out/extraction/extraction_2022-12-27/county-results/", labor_scen_file))

## remove multipliers
labor_out_adj <- labor_out %>%
  select(scen_id:revenue, total_emp:target_policy)

fwrite(labor_out_adj, paste0(save_info_path, 'subset_county_results_adj.csv'))

## remove multipliers from scenario file
labor_scen_adj <- labor_scen_out %>%
  select(scen_id:revenue, total_emp:total_comp)

fwrite(labor_scen_adj, paste0(save_info_path, 'county_level_out_adjusted.csv'))



