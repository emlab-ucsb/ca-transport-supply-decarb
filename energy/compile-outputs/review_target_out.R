## Tracey Mangin
## February 2, 2022
## Check targets

## libraries
library(data.table)
library(tidyverse)

## model output location
save_external <- 1

## path names, ## UPDATE THESE WITH NEW RUNS!!!!!
extraction_folder_path <- 'outputs/predict-production/extraction_2021-12-06/'
extraction_folder_name <- 'subset_target_scens/'
external_path <- 'extraction-out/extraction_2022-02-01/test_target/'


## current date
cur_date              = Sys.Date()

## paths 
main_path  <- '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
data_path  <-'data/stocks-flows/processed/'

## external paths
main_path_external <- '/Volumes/calepa/'

## compiled state outputs 
state_out_list <- list()

## files to process
state_files_to_process <- list.files(paste0(external_path, 'state-out/'))


for (i in 1:length(state_files_to_process)) {
  
  print(i)
  
  state_file_name <- state_files_to_process[i]
  
  state_scen_out <- readRDS(paste0(external_path, 'state-out/', state_file_name))
  
  state_out_list[[i]]  <- state_out_tmp
  
}

state_subset_all <- rbindlist(state_out_list)

