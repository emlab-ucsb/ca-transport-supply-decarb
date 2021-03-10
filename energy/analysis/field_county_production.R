## meas meng
## september 6, 2020
## calculate proportion of yearly oil production from each county in each field

# inputs ------

data_directory  = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed/'
prod_file       = 'well_prod_m.rds'
well_file       = 'wells_19.csv'
save_dir        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows/'

# load libraries -------- 

  library(data.table)  
  library(lubridate)
  library(zoo)
  library(stringr)
  library(openxlsx)

# read in data ------

  well_prod = readRDS(paste0(data_directory, prod_file))
  well_prod = setDT(well_prod)
  
  well_info = fread(paste0(data_directory, well_file), colClasses = c(rep('character',2), rep(NA, 21))) # info on wells, operators, location, etc
  
# get field code - field names -----
  
  field_names = unique(well_info[, c('FieldCode', 'FieldName')])
  
# aggregate production annually by field-county ------
  
  prod_field_county = well_prod[, .(oil_prod = sum(OilorCondensateProduced, na.rm = TRUE)), by = .(year,FieldCode,county_name)]

# get annual field-level production  ----
  
  prod_field = well_prod[, .(oil_prod = sum(OilorCondensateProduced, na.rm = TRUE)), by = .(year,FieldCode)]
  
# pad field code with leading zeroes -----
  
  field_names[, FieldCode := str_pad(FieldCode, 3, pad = '0') ]
  prod_field_county[, FieldCode := str_pad(FieldCode, 3, pad = '0') ]
  prod_field[, FieldCode := str_pad(FieldCode, 3, pad = '0') ]
  
# calculate proportions -----
  
  prod_field_county[, prop_production := oil_prod/sum(oil_prod, na.rm = TRUE), by = .(year,FieldCode)]
  prod_field_county[is.na(prop_production), prop_production := 0]
  setorderv(prod_field_county, c('year','FieldCode','county_name'))
  
# merge with field names ------
  
  prod_field_county_2 = prod_field_county[field_names, on = 'FieldCode', nomatch = 0]
  
# reorder columns -----
  
  setcolorder(prod_field_county_2, c('year', 'FieldCode', 'FieldName', 'county_name', 'oil_prod', 'prop_production'))
  
# get max year of non zero production for each field ----
  
  field_last_year = prod_field[oil_prod > 0, .SD[which.max(year)], by = .(FieldCode)]
  field_last_year[, oil_prod := NULL]
  field_county_last_year = field_last_year[prod_field_county_2, on = c('FieldCode', 'year'), nomatch = 0]
  setcolorder(field_county_last_year, c('FieldCode', 'FieldName', 'year', 'county_name', 'oil_prod', 'prop_production'))
  setorderv(field_county_last_year, c('FieldCode'))

# export to csv ------
  
  fwrite(prod_field_county_2, paste0(save_dir, 'annual_field_county_production_proportion.csv'), row.names = F)
  fwrite(field_county_last_year, paste0(save_dir, 'annual_final_year_field_county_production_proportion.csv'), row.names = F)
  