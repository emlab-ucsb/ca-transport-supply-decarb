# forecast ghg emission factors
# created: june 23, 2021
# author: @measrainsey

# inputs ------------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  res_path        = 'data/OPGEE'
  
# outputs -----------
  
  
# load packages ------
  
  library(data.table)
  
# read in results files -----
  
  files_res = list.files(path = file.path(emlab_path, res_path), pattern = 'opgee_doc_results')
  list_res = lapply(file.path(emlab_path, res_path, files_res), fread)
  dt_res = rbindlist(list_res)

# calculate upstream emissions ------
  
  dt_res[, upstream_gCO2e_MJ := exploration_gCO2e_MJ + drilling_gCO2e_MJ + crude_production_gCO2e_MJ + 
           surface_processing_gCO2e_MJ + maintenance_gCO2e_MJ + waste_gCO2e_MJ + other_gCO2e_MJ]
  dt_res[, upstream_kgCO2e_bbl := upstream_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  
  