## Tracey Mangin
## September 6, 2021
## compile and save 108 scenarios

## libraries
library(data.table)

## paths
main_path         <- '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
academic_out      <- file.path(main_path, 'outputs/academic-out/extraction')
main_path_external <- '/Volumes/calepa/'

## read in from external
extraction_folder = '_2021-09-04'
compiled_ext_path  = paste0(main_path_external, 'academic-out/extraction', extraction_folder, '/')
field_ext_path     = paste0(compiled_ext_path, 'field-results/')
state_ext_path     = paste0(compiled_ext_path, 'state-results/')
county_ext_path    = paste0(compiled_ext_path, 'county-results/')
# censust_save_path   = paste0(compiled_ext_path, 'census-tract-results/')

## save paths for drive
cur_date              = Sys.Date()

compiled_save_path  = paste0(academic_out, '/extraction_', cur_date, '/')
field_save_path     = paste0(compiled_save_path, 'field-results/')
state_save_path     = paste0(compiled_save_path, 'state-results/')
county_save_path    = paste0(compiled_save_path, 'county-results/')
# censust_save_path   = paste0(compiled_save_path, 'census-tract-results/')

## subset paths
field_subset_path     = paste0(field_save_path, 'subset/')
state_subset_path     = paste0(state_save_path, 'subset/')
county_subset_path    = paste0(county_save_path, 'subset/')

## create folder for outputs
dir.create(paste0(academic_out), showWarnings = FALSE)
dir.create(compiled_save_path, showWarnings = FALSE)
dir.create(field_save_path, showWarnings = FALSE) 
dir.create(state_save_path, showWarnings = FALSE)  
dir.create(county_save_path, showWarnings = FALSE)
# dir.create(censust_save_path, showWarnings = FALSE)
dir.create(field_subset_path, showWarnings = FALSE) 
dir.create(state_subset_path, showWarnings = FALSE)  
dir.create(county_subset_path, showWarnings = FALSE)



## files
scen_file <- 'scenario_id_list.csv'

## load files
scen_list <- fread(file.path(academic_out, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]

## start function
## 1) read in rds for subset ids; 2) save to drive; 3) compile field level outputs for health (for now)

field_out_list <- list()


  for (i in 1:length(subset_ids)) {
   
    id_name_tmp <- subset_ids[i]
    
    field_out_tmp <- readRDS(paste0(field_ext_path, id_name_tmp, '_field_results.rds'))
    saveRDS(field_out_tmp, paste0(field_subset_path, id_name_tmp, "_field_results.rds"))
    
    county_out_tmp <- readRDS(paste0(county_ext_path, id_name_tmp, '_county_results.rds'))
    saveRDS(county_out_tmp, paste0(county_subset_path, id_name_tmp, "_county_results.rds"))
    
    state_out_tmp <- readRDS(paste0(state_ext_path, id_name_tmp, '_state_results.rds'))
    saveRDS(state_out_tmp, paste0(state_subset_path, id_name_tmp, "_state_results.rds"))
    
    field_out_list[[i]] <- field_out_tmp
     
  }

field_subset_all <- rbindlist(field_out_list)

fwrite(field_subset_all, paste0(field_subset_path, "subset_field_results.csv"))


