## Tracey Mangin
## September 16, 2021
## Figure 2

## extraction and refining, production, and ghg emissions
## state outputs

## libraries
library(data.table)
library(tidyverse)

## paths
main_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
academic_path     = paste0(main_path, 'outputs/academic-out/extraction/')
output_folder     = 'extraction_2021-09-07/'
academic_out_path = paste0(academic_path, output_folder, "/")

# source figure themes
source(here::here('energy', 'figures-and-results', 'academic-paper', 'figure_themes.R'))

## files
scen_file <- 'scenario_id_list.csv'

## scenarios
scen_list <- fread(file.path(academic_path, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]

## read in the extraction outputs
## ---------------------------------

state_diff_list <- list()
scen_out_list <- list()

## main scenarios
main_scens <- c('reference case_no_setback_no quota_price floor_medium CCS cost_low innovation_no tax',
                'reference case_setback_5280ft_no quota_price floor_medium CCS cost_low innovation_no tax')



for (i in 1:length(subset_ids)) {
  
  id_name_tmp <- subset_ids[i]
  
  state_out_tmp <- readRDS(paste0(academic_out_path, 'state-results/subset/', id_name_tmp, '_state_results.rds'))
  
  prod_diff_tmp <- state_out_tmp[year == 2019 | year == 2045, .(scen_id, oil_price_scenario, innovation_scenario,
                                                                carbon_price_scenario, ccs_scenario, setback_scenario,
                                                                prod_quota_scenario, excise_tax_scenario, year, total_state_bbl)]
  
  prod_diff_tmp <- dcast(prod_diff_tmp, scen_id + oil_price_scenario + innovation_scenario +
                         carbon_price_scenario + ccs_scenario + setback_scenario +
                         prod_quota_scenario ~ year)
  
  
  prod_diff_tmp[, prod_diff := `2045` - `2019`]
  
  state_diff_list[[i]] <- prod_diff_tmp
  
  if(isTRUE(id_name_tmp %in% main_scens)){
    
    scen_out_list[[i]] <- state_out_tmp
    
  }
  
}

state_diff_vals <- rbindlist(state_diff_list)
scenario_prod <- rbindlist(scen_out_list)



## three scenarios (extraction)







