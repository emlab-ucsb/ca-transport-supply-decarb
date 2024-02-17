## meas meng
## august 8, 2020
## script for analyzing economically recoverable resources scenarios

# revise: Feb 14, 2024 - Haejin 

# load libraries -------- 

  library(data.table)

# inputs and file paths -----

  rystad_path   = '/capstone/freshcair/meds-freshcair-capstone/data/'
  err_file      = '/processed/economically_recoverable_resources_scenarios_wide.csv' #--missing, need to run other file to generate this ------

# read in data ------
  
  dt_err = fread(paste0(rystad_path, err_file))
  
# reformat from long to wide ----
  
  colnames(dt_err) = c('asset', 'year', 'production', 'resources', 'my_production')

# sum my_production -----

  sum_myprod = dt_err[!is.na(my_production), .(sum_err_my_production = sum(my_production)), by = .(asset)]  
  
# max resources ----
  
  max_resources = dt_err[, .SD[which.max(resources)], by = asset]
  max_resources[, max_err_resources := resources ]
  max_resources[, max_year := year ]
  max_resources = max_resources[, c('asset', 'max_err_resources', 'max_year')]

# cumultive sum of production ----
  
  dt_err[, production2 := production ]
  dt_err[ is.na(production2), production2 := 0]
  cumsum_prod = dt_err[, .(year = year, production = production, cumsum_err_production = cumsum(production2)), by = .(asset)] 
  # dt_err[, cum_production := cumsum(production), by = asset]
  
# merge sum of my_production with max resources -----
  
  myprod_res = merge(sum_myprod, max_resources, on = 'asset')
  myprod_res = merge(cumsum_prod, myprod_res, by = 'asset')
  myprod_res[, cumsum_div_my_prod := cumsum_err_production/sum_err_my_production]
  myprod_res[, cumsum_div_max_resources := cumsum_err_production/max_err_resources]
  
# add units -----
  
  sum_myprod[, units := 'million bbl']
  max_resources[, units := 'million bbl']
  myprod_res[, units := 'million bbl']
  cumsum_prod[, units := 'million bbl']
  
# save csv -----
  
  fwrite(sum_myprod, paste0(rystad_path, "processed/asset_sum_my-production.csv"))
  fwrite(max_resources, paste0(rystad_path, "processed/asset_max_resources.csv"))
  fwrite(cumsum_prod, paste0(rystad_path, "processed/asset-year_cumulative_sum_production.csv"))
  fwrite(myprod_res, paste0(rystad_path, "processed/asset-year_production_my-production_resources.csv"))
  