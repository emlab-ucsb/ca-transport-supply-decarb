
## the user must define run_name (which will be used when saving your results) and 
## the save path, which should direct to a folder where the outputs will be saved

# ## zenodo users define run name save path here ----
# run_name = ''
# save_path  = ''

## emlab users define run name and save path here ----

# args = commandArgs(trailingOnly = TRUE)
run_name        = "revision-setbacks"
# save_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
save_path       = '/Volumes/calepa/extraction-out'

# create save path that is based on the specified path and the run date ------

cur_date              = Sys.Date()
save_path             = file.path(save_path, paste0('extraction_', cur_date))
dir.create(save_path, showWarnings = FALSE)

# create directories for individual outputs

save_info_path = file.path(save_path, run_name)
dir.create(save_info_path)

dir.create(file.path(save_path, run_name, 'vintage-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'field-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'state-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'density-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'depl-out'), showWarnings = FALSE)
dir.create(file.path(save_path, run_name, 'exit-out'), showWarnings = FALSE)

# set seed
set.seed(228)

# load libraries ------

library(data.table)
library(openxlsx)
library(tidyverse)
# Multiprocessing
library(doParallel)
library(foreach)

# source from other scripts -------

# source load_input_info.R to load input info
source(here::here('energy', 'extraction-segment', 'model', 'full-run-revised', 'load_input_info.R'))

# source function to predict extraction
source(here::here('energy', 'extraction-segment', 'model', 'full-run-revised', 'fun_extraction_model_targets.R'))


## step 0: load the inputs

scen_id_file      = 'scenario_id_list_targets.csv'
scen_id_list = fread(file.path(revision_path, scen_id_file), header = T)

## filter for scenarios to run
selected_scens <- scen_id_list[subset_scens == 1]

# step 1: run extraction model and get outputs -------

# set start time -----
start_time <- Sys.time()
print(paste("Starting extraction model at ", start_time))

# cores
n_cores <- future::availableCores() - 2
doParallel::registerDoParallel(cores = n_cores)

run_extraction_model(input_scenarios = selected_scens)

elapsed_time <- Sys.time() - start_time
print(elapsed_time)
