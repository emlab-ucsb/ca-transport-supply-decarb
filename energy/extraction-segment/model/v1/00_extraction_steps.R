# inputs -----

  # args = commandArgs(trailingOnly = TRUE)
  # oil_price_selection    = args[1]
  scen_selection   = 'tax_scens' ## diagnostic, benchmark, tax_scens, carbon_scens
  run_type = "excise-scens"
    
# outputs -------
  
  save_path             = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production'
  
# create save path that is based on the specified path and the run date ------
  
  cur_date              = Sys.Date()
  save_path             = file.path(save_path, paste0('extraction_', cur_date))
  
  dir.create(save_path, showWarnings = FALSE)
  
# set binary switches
  run_diagnostic_figs   = 0
  run_benchmark_figs    = 0
  processes_out         = 0
  
# set seed
  set.seed(228)
    
# source from other scripts -------
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'model', 'v1', 'fun_extraction_model.R'))
  
  # source function to predict extraction
    source(here::here('energy', 'extraction-segment', 'model', 'v1', 'fun_process_extraction.R'))
  
# load libraries ------

  library(data.table)
  library(tidyverse)


# step 1: run extraction model and get outputs -------
  
  output_extraction = run_extraction_model(scen_selection)
  
# step 2: if relevant, run diagnostic plots/ benchmark plots
  
  library(hrbrthemes)
  library(sf)
  library(maps)
  library(cowplot)
  
  # source function to predict extraction
  source(here::here('energy', 'extraction-segment', 'model', 'v1', 'fun_diagnostic_plot.R'))
  
  if (run_diagnostic_figs == 1) {
  
    library(stringr)  
    library(hrbrthemes)
    library(extrafont)
  
    plot_diagnostic_outputs(scen_selection, output_extraction)
    
  }
  
  
  source(here::here('energy', 'extraction-segment', 'model', 'v1', 'fun_benchmark.R'))
  
  if (run_benchmark_figs == 1) {
    
    library(stringr)  
    library(hrbrthemes)
    library(extrafont)
    library(cowplot)
    
    benchmark_outputs(scen_selection, output_extraction)
    
  }
  
    
# step 3: process outputs for health and labor ------
  source(here::here('energy', 'compile-outputs', 'model', 'v1', 'compile_extraction_outputs.R'))
  
  if (processes_out == 1) {
    
    library(data.table)  
    library(tidyverse)
    library(openxlsx)
    
    output_processed = process_extraction_outputs(output_extraction)
    
  }  
  
  
# step 4: plot outputs -------
