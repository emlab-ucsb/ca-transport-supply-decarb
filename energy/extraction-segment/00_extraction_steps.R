# inputs -----

  # args = commandArgs(trailingOnly = TRUE)
  # oil_price_selection    = args[1]
  oil_price_selection   = 'diagnostic' ## diagnostic, benchmark
    # choose from: reference, high, low, iea
  run_type = "revised-density-calc"
    
# outputs -------
  
  save_path             = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  
# create save path that is based on the specified path and the run date ------
  
  cur_date              = Sys.Date()
  save_path             = file.path(save_path, paste0('extraction_', cur_date))
  
  dir.create(save_path, showWarnings = FALSE)
  
# set binary switches
  run_diagnostic_figs   = 1
  run_benchmark_figs    = 0
  
# set seed
  set.seed(228)
    
# source from other scripts -------
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'fun_extraction_model.R'))
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'fun_process_extraction.R'))
  
# load libraries ------

  library(data.table)
  library(hrbrthemes)
  library(tidyverse)
  library(sf)
  library(maps)
  library(cowplot)

# step 1: run extraction model and get outputs -------
  
  output_extraction = run_extraction_model(oil_price_selection)
  
# step 2: if relevant, run diagnostic plots/ benchmark plots
  
  # source function to predict extraction
  source(here::here('energy', 'extraction-segment', 'fun_diagnostic_plot.R'))
  
  if (run_diagnostic_figs == 1) {
  
    library(stringr)  
    library(hrbrthemes)
    library(extrafont)
  
    plot_diagnostic_outputs(oil_price_selection, output_extraction)
    
  }
  
  
  source(here::here('energy', 'extraction-segment', 'fun_benchmark.R'))
  
  if (run_benchmark_figs == 1) {
    
    library(stringr)  
    library(hrbrthemes)
    library(extrafont)
    library(cowplot)
    
    benchmark_outputs(oil_price_selection, output_extraction)
    
  }
  
    
# step 3: process outputs ------
  
# output_processed = process_extraction_outputs(oil_price_selection, output_extraction)
  
# step 4: plot outputs -------
