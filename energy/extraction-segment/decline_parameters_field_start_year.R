# parameterize decline at field-start year level
# created: may 27, 2021
# author: meas meng

# inputs ------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  fv_year_file    = 'outputs/decline-historic/data/production_field-year_yearly_entry.csv'
  peak_file       = 'outputs/decline-historic/data/field-year_peak-production_yearly.csv'
  entry_file      = 'outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
  plot_dir        = 'outputs/figures/interim-report-figures/drafts/fuels-model/decline_figs/'
  
# outputs -------
  
  save_dir        = 'outputs/decline-historic/parameters/'

# load libraries -------- 
  
  library(data.table)  
  
# read in data ------
  
  prod_fv_year = fread(file.path(emlab_path, fv_year_file), colClasses = c(rep('character',2), rep(NA,12)))
  peak_prod = fread(file.path(emlab_path, peak_file), header = T)
  dt_entry = fread(file.path(emlab_path, entry_file), header = T)
  
# pad field code with leading zeroes -----
  
  prod_fv_year[, doc_field_code := sprintf("%03s", doc_field_code)]
  peak_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_entry[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# get list of unique field-vintages -----
  
  un_fy = unique(prod_fv_year[, c('doc_fieldname', 'doc_field_code', 'start_year', 'no_wells')])
  un_fy = un_fy[order(doc_field_code, start_year)]
  
# create start year column for entry df -----
  
  dt_entry[, start_year := year]
  
# get field-vintage combos in the entry df that are not in the production dataset -----  
  
  nonmatch = dt_entry[!un_fy, on = c('doc_field_code', 'start_year')]
  