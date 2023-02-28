# ccs paramaterization of field extraction and refinery ghg emissions combined
# created: october 12, 2020
# author: meas meng

# inputs ------------

  # emdata_path   = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows'
  emdata_path   = file.path('/Volumes/GoogleDrive-103159311076289514198/.shortcut-targets-by-id/139aDqzs5T2c-DtdKyLw7S5iJ9rqveGaP/calepa-cn', 'outputs/stocks-flows')
  fprod_file    = 'crude_prod_x_field_revised.csv'
  emfac_file    = 'ghg_emissions_x_field_2018-2045.csv'
  ref_file      = 'refinery_ghg_emissions.csv'
  
# selections -------
  
  select_year = 2018
  
# load packages -------
  
  library(data.table)
  
# import data -------
  
  # load field-level production data
    field_prod = fread(file.path(emdata_path, fprod_file), header = T)
  
  # load field-level emissions factors
    field_emfac = fread(file.path(emdata_path, emfac_file), header = T)
    # field_emfac = field_emfac[, .(doc_field_code, doc_fieldname, upstream_kgCO2e_bbl)]
    
  # load refinery-level emissions
    ref_ghg = fread(file.path(emdata_path, ref_file), header = T)
    
# rename FieldCode -> doc_field_code -----
  
  # setnames(field_prod, "FieldCode", "doc_field_code")

# pad field codes with leading zeroes -----
  
  field_prod[, doc_field_code := sprintf("%03d", doc_field_code)]
  field_emfac[, doc_field_code := sprintf("%03d", doc_field_code)]
  
# rename emissions columns in refinery data -----
  
  setnames(ref_ghg, 'adj_total_co2e', 'co2e_tonnes')
  setnames(ref_ghg, 'adj_total_Mt_co2e', 'co2e_megatonnes')
  
# only use refineries with non-NA site IDs ------
  
  ref_ghg = ref_ghg[!is.na(site_id)]
  
# combine field production with field emissions factor -----
  
  field_ghg = field_prod[field_emfac, on = .(doc_field_code, doc_fieldname, year)]
  setcolorder(field_ghg, c('doc_field_code', 'doc_fieldname', 'year', 'total_bbls', 'upstream_kgCO2e_bbl'))
  
# calculate field-level extraction emissions ------
  
  field_ghg[, co2e_kg := upstream_kgCO2e_bbl*total_bbls]
  field_ghg[, co2e_tonnes := co2e_kg/1e3]
  
# fix some refinery names ------
  
  ref_ghg[refinery_name %like% 'Kern Oil', updated_name2 := 'Kern Oil & Refining Company, Bakersfield Refinery']
  ref_ghg[refinery_name %like% 'San Joaquin Refining', updated_name2 := 'San Joaquin Refining Company Inc., Bakersfield Refinery']
  ref_ghg[refinery_name %like% 'PBF Energy, Martinez Refinery', updated_name2 := 'Shell Oil Products US, Martinez Refinery']
  
# combine extraction and refining emissions together -----
  
  ghg_all = rbindlist(list(field_ghg[, .(doc_field_code, doc_fieldname, year, co2e_tonnes)],
                           ref_ghg[, .(updated_name2, year, co2e_tonnes)]),
                      use.names = T, fill = T)
  setnames(ghg_all, 'updated_name2', 'refinery_name')
  
  setcolorder(ghg_all, c('doc_field_code', 'doc_fieldname', 'refinery_name', 'year', 'co2e_tonnes'))
  
# only use selected year data -----
  
  ghg_all = ghg_all[year == select_year]
  
# separate ghg by extraction and refining ------
  
  ghg_all[!is.na(doc_field_code), type := 'extraction']
  ghg_all[!is.na(refinery_name), type := 'refining']
  
# write function to solve for mean b value given values of a and ccs price -----
  
  solve_b <- function(a, p, q) {
    f <- (a*p + a*(q^(1/a)) + p)/(a + 1)
    return(f)
  }
  
  solve_mean_b <- function(a, p, sector){
    b = solve_b(a, p, ghg_all[type == sector, co2e_tonnes])
    val = mean(b, na.rm = T)
    # ghg_all[, b := solve_b(a, p, co2e_tonnes)]
    # val = mean(ghg_all[, b], na.rm = T)
    return(val)
  }
  
# remove items from environment -----
  
  rm(emdata_path, fprod_file, emfac_file, ref_file, field_prod, field_emfac, ref_ghg, field_ghg, select_year)