# get field-level emission factors (including for those not included in CARB's model)
# created: september 23, 2020
# author: meas meng

# inputs -----

  data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
  opgee_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/OPGEE'
  entry_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows'
  inj_file          = 'injection-by-well-type-per-field-per-year_1977-2018.csv'
  emfactor_file     = 'field-level-emissions-results_processed_revised.csv'
  entry_file        = 'entry-input-df/final/entry_df_final_revised.csv'
  prod_file         = 'crude_prod_x_field_revised.csv'

# outputs -----
  
  save_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows'
  pre_file          = 'opgee_emission_factors_x_field_2015_revised.csv'
  save_file         = 'ghg_emissions_x_field_2015_revised.csv'
  
# load packages -----
  
  library(data.table)
  
# load data -----
  
  # load field injection data
    field_inj = fread(file.path(data_path, inj_file), header = T)
    field_inj = field_inj[, c('FieldCode', 'FieldName', 'year', 'well_type_name', 'type_inj', 'total_inj_year', 'perc')]
    setnames(field_inj, 'FieldCode', 'doc_field_code')
    setnames(field_inj, 'FieldName', 'doc_fieldname')
  
  # load opgee emissions factors
    emission_factors = fread(file.path(opgee_path, emfactor_file), header = T)
    emission_factors = emission_factors[, c('field_name', 'upstream_kgCO2e_bbl', 'lifecycle_kgCO2e_bbl')]
    setnames(emission_factors, 'field_name', 'doc_fieldname')

  # load entry data file
    entry_df =  fread(file.path(entry_path, entry_file), header = T)
  
  # load field-level production
    prod_dt = fread(file.path(entry_path, prod_file), header = T)

# pad field codes ------
  
  field_inj[, doc_field_code := sprintf("%03s", doc_field_code)]
  entry_df[, doc_field_code := sprintf("%03s", doc_field_code)]
  prod_dt[, doc_field_code := sprintf("%03s", doc_field_code)]

# get unique list of field codes and field names that are used in the entry df AND the production file -----
  
  un_fields = unique(rbindlist(list(entry_df[, .(doc_field_code, doc_fieldname)],
                                    prod_dt[, .(doc_field_code, doc_fieldname)])))
  
# fix field names to match DOC naming conventions -----
  
  emission_factors[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
  emission_factors[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
  emission_factors[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']
  
# match field codes to emission factors -----
  
  emission_factors = emission_factors[un_fields, on = 'doc_fieldname', nomatch = 0]
  setcolorder(emission_factors, c('doc_field_code', 'doc_fieldname', 'upstream_kgCO2e_bbl', 'lifecycle_kgCO2e_bbl'))

# separate fields that don't use steam injection with fields that do -----
  
  field_inj[well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'steam']
  field_inj[!well_type_name %in% c('Cyclic Steam', 'Steam Flood'), type := 'other']
  
  field_inj_agg = field_inj[, .(type_inj = sum(type_inj, na.rm = T)), by = .(doc_field_code, doc_fieldname, year, type)]

  steam_fields = field_inj_agg[doc_field_code %in% field_inj_agg[year == 2015 & type == 'steam' & type_inj > 0, doc_field_code]]
  nonsteam_fields = field_inj_agg[!doc_field_code %in% field_inj_agg[type == 'steam' & type_inj > 0, doc_field_code]]
  
# separate emission factors by steam and non steam fields -----
  
  steam_ghg = emission_factors[doc_field_code %in% steam_fields[, doc_field_code]]
  nonsteam_ghg = emission_factors[doc_field_code %in% nonsteam_fields[, doc_field_code]]
  
  # there are some emission factors for fields without any injection info:
    # emission_factors[!doc_field_code %in% unique(c(nonsteam_fields[, doc_field_code], steam_fields[, doc_field_code]))] 
  
# get medians for steam and non-steam emission factors
  
  steam_ghg_median = steam_ghg[, lapply(.SD, median, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl', 'lifecycle_kgCO2e_bbl') ] 
  nonsteam_ghg_median = nonsteam_ghg[, lapply(.SD, median, na.rm = T), .SDcols = c('upstream_kgCO2e_bbl', 'lifecycle_kgCO2e_bbl') ] 
  
# merge entry fields with emission factors -----
  
  all_fields = merge(un_fields, emission_factors, by = c('doc_field_code', 'doc_fieldname'), all.x = T)
  
# for NA fields, decide to match with either median of steam or non-steam fields -----
  
  all_fields[is.na(upstream_kgCO2e_bbl), upstream_kgCO2e_bbl := ifelse(doc_field_code %in% steam_fields[, doc_field_code],
                                                                       steam_ghg_median[, upstream_kgCO2e_bbl],
                                                                       nonsteam_ghg_median[, upstream_kgCO2e_bbl])]
  all_fields[is.na(lifecycle_kgCO2e_bbl), lifecycle_kgCO2e_bbl := ifelse(doc_field_code %in% steam_fields[, doc_field_code],
                                                                        steam_ghg_median[, lifecycle_kgCO2e_bbl],
                                                                        nonsteam_ghg_median[, lifecycle_kgCO2e_bbl])]
  
# calculate total GHG emissions based on 2015 production ------
  
  all_fields_prod = all_fields[prod_dt[year == 2015], on = c('doc_field_code', 'doc_fieldname'), nomatch = 0]
  all_fields_prod[, upstream_kgCO2e := upstream_kgCO2e_bbl*total_bbls]
  all_fields_prod[, lifecycle_kgCO2e := lifecycle_kgCO2e_bbl*total_bbls]
  setcolorder(all_fields_prod, c('doc_field_code', 'doc_fieldname', 'year', 'total_bbls', 
                                 'upstream_kgCO2e_bbl', 'lifecycle_kgCO2e_bbl', 
                                 'upstream_kgCO2e', 'lifecycle_kgCO2e'))
   
# write to csv -----
  
  fwrite(emission_factors, file.path(save_path, pre_file), row.names = F)
  fwrite(all_fields_prod, file.path(save_path, save_file), row.names = F)
