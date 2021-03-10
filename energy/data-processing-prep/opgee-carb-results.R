# meas meng
# september 4, 2020
# opgee ghg emission intensities

# ------------------------------------------- INPUTS -----------------------------------

  data_dir    = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/OPGEE/'
  opgee_fil   = 'OPGEE_v2.0_with-CARB-inputs.xlsm'
  names_fil   = 'opgee_field_names.csv'

# ------------------------------------------- MAIN -----------------------------------
  
# load libraries -------- 

  library(data.table)  
  library(openxlsx)

# load data ------
  
  raw_opgee = as.data.table(read.xlsx(paste0(data_dir, opgee_fil), sheet = 'Results', startRow = 6, 
                                     rows = c(6,46:48,63,131,137,143,149,155,161,167,170,172,179), 
                                     cols = c(2,8:164), detectDates = T))
  
  dt_fn = fread(paste0(data_dir, names_fil), header = T, select = 1, fill = T)
  
  
# rename opgee dt and fill in blank variables -----
  
  colnames(raw_opgee) =  c('variable', dt_fn[, field_names] )
  raw_opgee[is.na(variable), 'variable'] = c('Other Small Sources', 'Offsite emissions credit/debit', 'Lifecycle GHG emissions')
  
# melt raw data ------
  
  dt_res = melt(raw_opgee, measure.vars = colnames(raw_opgee)[2:158], variable.name = 'field_name', value.name = 'value')
  dt_res[, value := as.numeric(value)]
  dt_res = dcast(dt_res, field_name ~ variable, value.var = 'value')
  colnames(dt_res) = c('field_name', 'gor_bbl_bbl', 'wor_bbl_bbl', 'water_injection_ratio_bbl_bbl', 'sor_bbl_bbl', 
                       'exploration_gCO2e_MJ', 'drilling_gCO2e_MJ', 'crude_production_gCO2e_MJ', 'surface_processing_gCO2e_MJ', 'maintenance_gCO2e_MJ', 'waste_gCO2e_MJ',
                       'transport_gCO2e_MJ', 'lifecycle_gCO2e_MJ', 'offsite_credit-debit_gCO2e_MJ', 'other_gCO2e_MJ')
  
# calculate upstream emissions ------
  
  dt_res[, upstream_gCO2e_MJ := exploration_gCO2e_MJ + drilling_gCO2e_MJ + crude_production_gCO2e_MJ]
  dt_res[, difference_gCO2e_MJ := surface_processing_gCO2e_MJ + maintenance_gCO2e_MJ + waste_gCO2e_MJ + other_gCO2e_MJ]
  
# convert some columns to kg/bbl ------
  
  dt_res[, upstream_kgCO2e_bbl := upstream_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  dt_res[, difference_kgCO2e_bbl := difference_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  dt_res[, lifecycle_kgCO2e_bbl := lifecycle_gCO2e_MJ*(1/(2e-4))*(1/1000)]
  
# save to csv -----
  
  fwrite(dt_res, paste0(data_dir, 'field-level-emissions-results_processed.csv'), row.names = F)
    