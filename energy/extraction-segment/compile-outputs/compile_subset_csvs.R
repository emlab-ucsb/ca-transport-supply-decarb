## Tracey Mangin
## September 16, 2021
## create csv versions of outputs

## libraries
library(data.table)

external_save <- 1

## paths
main_path         <- '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
academic_out_path      <- file.path(main_path, 'outputs/academic-out/extraction/')
input_path        <- file.path(main_path, 'outputs/academic-out/extraction/nature-energy-rev-outputs/')

## read in saved rds files - updates as needed
# extraction_folder = 'extraction_2022-11-07'

external_path <- '/Volumes/calepa/academic-out/extraction_2022-11-16/'

## get correct path

if(external_save == 1) {
  
  compiled_path = external_path
} else {

 compiled_path  = paste0(academic_out_path, extraction_folder, '/')

}

## sub folders
field_path     = paste0(compiled_path, 'field-results/')
state_path     = paste0(compiled_path, 'state-results/')
county_path    = paste0(compiled_path, 'county-results/')
ct_path        = paste0(compiled_path, 'census-tract-results/')


## files
scen_file <- 'scenario_id_list_targets.csv'

## load files
scen_list <- fread(file.path(input_path, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, .(scen_id, target, target_policy)]

## start function
## 1) read in rds for subset ids; 2) save to drive; 3) compile field level outputs for health (for now)

county_out_list <- list()
state_out_list <- list()
ct_out_list <- list()

for (i in 1:nrow(subset_ids)) {
  
  print(i)
  
  id_name_tmp <- subset_ids[i, scen_id]
  
  ct_out_tmp <- readRDS(paste0(ct_path, id_name_tmp, '_ct_results.rds'))
  
  ct_out_tmp <- merge(ct_out_tmp, subset_ids,
                      by = "scen_id")
  
  county_out_tmp <- readRDS(paste0(county_path, id_name_tmp, '_county_results.rds'))
  
  county_out_tmp <- merge(county_out_tmp, subset_ids,
                      by = "scen_id")
  
  state_out_tmp <- readRDS(paste0(state_path, id_name_tmp, '_state_results.rds'))
  
  state_out_tmp <- merge(state_out_tmp, subset_ids,
                          by = "scen_id")
  
  ct_out_list[[i]]     <- ct_out_tmp
  county_out_list[[i]] <- county_out_tmp
  state_out_list[[i]]  <- state_out_tmp
  
}

ct_subset_all <- rbindlist(ct_out_list)
county_subset_all <- rbindlist(county_out_list)
state_subset_all <- rbindlist(state_out_list)

fwrite(ct_subset_all, paste0(ct_path, "subset_census_tract_results.csv"))
fwrite(county_subset_all, paste0(county_path, "subset_county_results.csv"))
fwrite(state_subset_all, paste0(state_path, "subset_state_results.csv"))


