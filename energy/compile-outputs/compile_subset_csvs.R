## Tracey Mangin
## September 16, 2021
## create csv versions of outputs

## libraries
library(data.table)

## paths
main_path         <- '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
academic_out_path      <- file.path(main_path, 'outputs/academic-out/extraction/extraction')

## read in saved rds files
extraction_folder = '_2021-09-07'
compiled_path  = paste0(academic_out_path, extraction_folder, '/')
field_path     = paste0(compiled_path, 'field-results/subset/')
state_path     = paste0(compiled_path, 'state-results/subset/')
county_path    = paste0(compiled_path, 'county-results/subset/')
# censust_save_path   = paste0(compiled_path, 'census-tract-results/')


## files
scen_file <- 'scenario_id_list.csv'

## load files
scen_list <- fread(file.path(academic_out_path, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]

## start function
## 1) read in rds for subset ids; 2) save to drive; 3) compile field level outputs for health (for now)

county_out_list <- list()
state_out_list <- list()

for (i in 1:length(subset_ids)) {
  
  id_name_tmp <- subset_ids[i]
  
  county_out_tmp <- readRDS(paste0(county_path, id_name_tmp, '_county_results.rds'))
  
  state_out_tmp <- readRDS(paste0(state_path, id_name_tmp, '_state_results.rds'))
  
  county_out_list[[i]] <- county_out_tmp
  state_out_list[[i]] <- state_out_tmp
  
}

county_subset_all <- rbindlist(county_out_list)
state_subset_all <- rbindlist(state_out_list)

fwrite(county_subset_all, paste0(county_path, "subset_county_results.csv"))
fwrite(state_subset_all, paste0(state_path, "subset_state_results.csv"))


