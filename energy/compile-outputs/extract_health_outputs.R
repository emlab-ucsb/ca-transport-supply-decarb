## Tracey Mangin
## October 6, 2021
## Extract health outputs for select scenarios

## libraries
library(data.table)
library(tidyverse)

## paths
main_path           = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
academic_path       = paste0(main_path, 'outputs/academic-out/')

## files
health_file     <- "census_tract_mortality.csv"
scen_file       <- "scenario_id_list.csv"

## health outputs
health_out <- fread(file.path(academic_path, 'health', health_file), header = T)

## subset scenarios
scen_list <- fread(file.path(academic_path, 'extraction', scen_file), header = T) 

subset_list <- scen_list[BAU_scen == 1 | subset_scens == 1]

subset_ids <- subset_list[, scen_id]
