# inputs -----

  # args = commandArgs(trailingOnly = TRUE)
  # oil_price_selection    = args[1]
  scen_selection   = 'full_run_subset' ## diagnostic, benchmark, tax_scens, full_run, full_run_subset
  run_type = "full_run_subset"
    
# outputs -------
  
  save_path             = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  # save_path             = '/Volumes/calepa/extraction-out/'
  
# create save path that is based on the specified path and the run date ------
  
  cur_date              = Sys.Date()
  
  save_path             = file.path(save_path, paste0('extraction_', cur_date))
  dir.create(save_path, showWarnings = FALSE)

  # create directories for individual outputs

  save_info_path = file.path(save_path, run_type)
  dir.create(save_info_path)
  
  # dir.create(file.path(save_path, run_type, 'vintage-out'), showWarnings = FALSE)
  dir.create(file.path(save_path, run_type, 'field-out'), showWarnings = FALSE)
  dir.create(file.path(save_path, run_type, 'state-out'), showWarnings = FALSE)
  # dir.create(file.path(save_path, run_type, 'density-out'), showWarnings = FALSE)
  # dir.create(file.path(save_path, run_type, 'depl-out'), showWarnings = FALSE)
  # dir.create(file.path(save_path, run_type, 'exit-out'), showWarnings = FALSE)
  
# set seed
  set.seed(228)
      
# source from other scripts -------
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'full-run', 'fun_extraction_model_full.R'))
  
  
# load libraries ------

  library(data.table)
  library(openxlsx)
  library(tidyverse)
  # Multiprocessing
  library(doParallel)
  library(foreach)

# step 1: run extraction model and get outputs -------
  
  # set start time -----
  start_time <- Sys.time()
  print(paste("Starting extraction model at ", start_time))
  
  # cores
  n_cores <- 8
  doParallel::registerDoParallel(cores = n_cores)
  
  run_extraction_model(scen_selection)
  
  elapsed_time <- Sys.time() - start_time
  print(elapsed_time)
  
  