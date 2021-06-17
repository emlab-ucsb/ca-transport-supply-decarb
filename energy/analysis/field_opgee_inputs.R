# get opgee relevant inputs for fields over multiple years
# created: june 16, 2021
# author: @measrainsey

# inputs ------------

  emlab_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn'
  prod_file       = 'data/stocks-flows/processed/well_prod_m_processed.csv'
  inj_file        = 'data/stocks-flows/processed/well_inj_m_processed.csv'
  opgee_file      = 'data/OPGEE/opgee_field_names.csv'
  
# outputs -----------
  
  save_path       = 'data/stocks-flows/processed'
  save_file       = 'field_opgee_inputs_1977_2019.csv'
  
# libraries ----------
  
  library(data.table)

# read in data ---------
  
  dt_prod = fread(file.path(emlab_path, prod_file), header = T)
  dt_inj = fread(file.path(emlab_path, inj_file), header = T)
  dt_opgee = fread(file.path(emlab_path, opgee_file), header = T, fill = T)
  
# pad field code with leading zeroes -----
  
  dt_prod[, doc_field_code := sprintf("%03s", doc_field_code)]
  dt_inj[, doc_field_code := sprintf("%03s", doc_field_code)]
  
# rename opgee field names ------
  
  dt_opgee[, doc_fieldname := field_names]
  dt_opgee[doc_fieldname %like% 'North$', doc_fieldname := gsub('North', ' North', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'South$', doc_fieldname := gsub('South', ' South', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'East$', doc_fieldname := gsub('East', ' East', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'West$', doc_fieldname := gsub('West', ' West', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'N$', doc_fieldname := gsub('N', ' North', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'S$', doc_fieldname := gsub('S', ' South', doc_fieldname)]
  dt_opgee[doc_fieldname %like% 'Northwest$', doc_fieldname := gsub('Northwest', ' Northwest', doc_fieldname)]
  dt_opgee[doc_fieldname == 'Elwood S. Offshore', doc_fieldname := 'Elwood  South  Offshore']
  
# get oil/gas production info ------
  
  agg_prod = dt_prod[OilorCondensateProduced > 0 | GasProduced > 0, 
                     .(oil_produced_bbl = sum(OilorCondensateProduced, na.rm = T),
                       gas_produced_mcf = sum(GasProduced, na.rm = T),
                       water_produced_bbl = sum(WaterProduced, na.rm = T),
                       no_prod_wells = uniqueN(api_ten_digit)), 
                     by = .(doc_field_code, doc_fieldname, year)]
  agg_prod[, daily_production_bopd := oil_produced_bbl/365]
  agg_prod[, gor_scf_bbl := (gas_produced_mcf*1e3)/oil_produced_bbl]
  agg_prod[, wor_bbl_bbl := water_produced_bbl/oil_produced_bbl]
  
# get aggregate injection info -------
  
  dt_inj[well_type_name %chin% c('Cyclic Steam', 'Steam Flood Injector'), customized_well_type := 'steam_injection']
  dt_inj[well_type_name %chin% c('Water Flood Injector', 'Water Disposal Injector'), customized_well_type := 'water_injection']
  dt_inj[well_type_name %chin% c('Gas Storage Injector/Producer', 'Air Injector', 
                                 'Gas Disposal Injector', 'Liquid Petroleum Gas Injector/Producer', 'Liquid Gas'), customized_well_type := 'gas_air_injection']
  dt_inj[is.na(customized_well_type), customized_well_type := 'other']
  
# get aggregate injection info -------
  
  agg_inj = dt_inj[GasAirInjected > 0 | SteamWaterInjected > 0, 
                     .(steam_water_injected = sum(SteamWaterInjected, na.rm = T),
                       gas_air_injected = sum(GasAirInjected, na.rm = T),
                       no_inj_wells = uniqueN(api_ten_digit)), 
                     by = .(doc_field_code, doc_fieldname, customized_well_type, year)]
  
# get wide-form steam/water injection data ------
  
  steam_water_inj_wide = dcast(agg_inj, doc_field_code + doc_fieldname + year ~ customized_well_type, value.var = 'steam_water_injected')
  steam_water_inj_wide = steam_water_inj_wide[, .(doc_field_code, doc_fieldname, year, steam_injection, water_injection)]
  setnames(steam_water_inj_wide, 'steam_injection', 'steam_injection_bwe')
  setnames(steam_water_inj_wide, 'water_injection', 'water_injection_bbl')
  
# get wide-form gas/air injection data -------
  
  gas_air_inj_wide = dcast(agg_inj, doc_field_code + doc_fieldname + year ~ customized_well_type, value.var = 'gas_air_injected')
  gas_air_inj_wide = gas_air_inj_wide[, .(doc_field_code, doc_fieldname, year, gas_air_injection)]
  setnames(gas_air_inj_wide, 'gas_air_injection', 'gas_air_injection_mcf')
  
# get number of water injection wells -------
  
  water_inj_wells = dcast(agg_inj, doc_field_code + doc_fieldname + year ~ customized_well_type, value.var = 'no_inj_wells')
  water_inj_wells = water_inj_wells[, .(doc_field_code, doc_fieldname, year, water_injection)]
  setnames(water_inj_wells, 'water_injection', 'no_water_inj_wells')
  
# merge production data with injection data ------
  
  combined_info = merge(agg_prod, steam_water_inj_wide, by = c('doc_field_code', 'doc_fieldname', 'year'), all.x = T)
  combined_info = merge(combined_info, gas_air_inj_wide, by = c('doc_field_code', 'doc_fieldname', 'year'), all.x = T)
  combined_info = merge(combined_info, water_inj_wells, by = c('doc_field_code', 'doc_fieldname', 'year'), all.x = T)
  
  combined_info[is.na(steam_injection_bwe), steam_injection_bwe := 0]
  combined_info[is.na(water_injection_bbl), water_injection_bbl := 0]
  combined_info[is.na(gas_air_injection_mcf), gas_air_injection_mcf := 0]
  combined_info[is.na(no_water_inj_wells), no_water_inj_wells := 0]
  
# calculate other variables --------
  
  combined_info[, fraction_natural_gas_injected := ifelse(gas_air_injection_mcf == 0, 0, 
                                                          ifelse(gas_air_injection_mcf/gas_produced_mcf > 1, 1, gas_air_injection_mcf/gas_produced_mcf))]
  combined_info[, gas_flooding_injection_ratio := ifelse(gas_air_injection_mcf == 0, NA, 
                                                  ifelse(gas_air_injection_mcf/gas_produced_mcf > 1, (gas_air_injection_mcf*1e3)/oil_produced_bbl, NA))]
  combined_info[, fraction_prodced_water_reinjected := ifelse(water_injection_bbl/water_produced_bbl > 1, 1, water_injection_bbl/water_produced_bbl)]
  combined_info[, water_injection_ratio := ifelse(water_injection_bbl/water_produced_bbl > 1, water_injection_bbl/oil_produced_bbl, NA)]
  combined_info[, sor_bbl_bbl := ifelse(steam_injection_bwe > 0, steam_injection_bwe/oil_produced_bbl, NA)]
  
# keep selected oil fields -------
  
  combined_sel = combined_info[dt_opgee, on = .(doc_fieldname), nomatch = 0]
  
# reorganize datatable ------
  
  combined_sel_keep = combined_sel[, .(doc_fieldname, year, daily_production_bopd, no_prod_wells, no_water_inj_wells, 
                                       gor_scf_bbl, wor_bbl_bbl, water_injection_ratio, gas_flooding_injection_ratio,
                                       sor_bbl_bbl, fraction_natural_gas_injected, fraction_prodced_water_reinjected)]
  
  all_vars = c('daily_production_bopd', 'no_prod_wells', 'no_water_inj_wells', 
               'gor_scf_bbl', 'wor_bbl_bbl', 'water_injection_ratio', 'gas_flooding_injection_ratio',
               'sor_bbl_bbl', 'fraction_natural_gas_injected', 'fraction_prodced_water_reinjected')
  
  l_inputs = list()
  for (i in 1:length(all_vars)) {
    
    dt = dcast(combined_sel_keep, year ~ doc_fieldname, value.var = all_vars[i])
    dt[, variable := paste0(i, '_', all_vars[i])]
    setcolorder(dt, c(colnames(dt)[159], colnames(dt)[1:158]))
    
    l_inputs[[i]] = dt
    
    rm(dt)
    
  }
  
  dt_inputs = rbindlist(l_inputs)

# save to csv ------
  
  fwrite(dt_inputs, file.path(emlab_path, save_path, save_file), row.names = F)
