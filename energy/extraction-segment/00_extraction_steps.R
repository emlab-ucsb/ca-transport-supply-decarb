# inputs -----

  # args = commandArgs(trailingOnly = TRUE)
  # oil_price_selection    = args[1]
  oil_price_selection   = 'iea'
    # choose from: reference, high, low, iea
    
# outputs -------
  
  save_path             = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  
# create save path that is based on the specified path and the run date ------
  
  cur_date              = Sys.Date()
  save_path             = file.path(save_path, paste0('extraction_', cur_date))
  dir.create(save_path, showWarnings = FALSE)
  
# source from other scripts -------
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'extraction_model_fun.R'))
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'fun_process_extraction.R'))
  
# load libraries ------

  library(data.table)
  library(dplyr)

# step 1: run extraction model and get outputs -------
  
  output_extraction = run_extraction_model(oil_price_selection)
  
# step 2: process outputs ------
  
  # output_processed = process_extraction_outputs(oil_price_selection, output_extraction)
  
# step 3: plot outputs -------
