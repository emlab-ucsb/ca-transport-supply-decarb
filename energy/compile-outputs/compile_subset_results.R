## Tracey Mangin
## September 6, 2021
## compile and save 108 scenarios

## libraries
library(data.table)

## paths
main_path         <- '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
academic_out      <- file.path(main_path, 'outputs/academic-out/extraction')


## files
scen_file <- 'scenario_id_list.csv'

## load files
scen_list <- fread(file.path(academic_out, scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]

## start function
## 1) read in rds for subset ids; 2) save to drive; 3) 