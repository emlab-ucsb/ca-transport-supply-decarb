process_extraction_outputs <- function(oil_price_selection, output_extraction) {
  
  print('Now processing outputs...')
  
  # create objects for list items -------
  
    existing_prod_dt = output_extraction[[1]]
    new_prod_dt = output_extraction[[2]]

  # convert ccs_adopted column to character ------
    
    existing_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
    new_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
    
  # set keys -------
    
    setkey(existing_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
    setkey(new_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
           setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
    
  # create field-level information --------
  
    field_well_entry = new_prod_dt[vintage_start == year, .(new_wells = sum(n_wells, na.rm = T)), 
                                   by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                          setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                          doc_field_code, doc_fieldname, year)]
    
    field_existing_info = existing_prod_dt[, .(existing_prod_bbl = sum(production_bbl, na.rm = T),
                                               existing_ghg_kgCO2e = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T)), 
                                                 by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                        setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                        doc_field_code, doc_fieldname, year, ccs_adopted)]
    
    field_new_info = new_prod_dt[, .(new_prod_bbl = sum(production_bbl, na.rm = T),
                                     new_ghg_kgCO2e = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T)),
                                 by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                        setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                        doc_field_code, doc_fieldname, year, ccs_adopted)]

    # field_existing_ghg = existing_prod_dt[, .(existing_ghg_kgCO2e = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T)), 
    #                                       by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
    #                                              setback_scenario, prod_quota_scenario, excise_tax_scenario, 
    #                                              doc_field_code, doc_fieldname, ccs_adopted, year)]
    
    # field_new_ghg = new_prod_dt[, .(new_ghg_kgCO2e = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T)), 
    #                             by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
    #                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, 
    #                                    doc_field_code, doc_fieldname, ccs_adopted, year)]
    
  # combine all field-level information -------
    
    field_all = merge(field_existing_info, field_new_info,
                      by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                             'doc_field_code', 'doc_fieldname', 'ccs_adopted', 'year'),
                      all = T)

    field_all[is.na(new_prod_bbl), new_prod_bbl := 0]
    field_all[, total_prod_bbl := existing_prod_bbl + new_prod_bbl]
    
    field_all[is.na(new_ghg_kgCO2e), new_ghg_kgCO2e := 0]
    field_all[, total_ghg_kgCO2e := existing_ghg_kgCO2e + new_ghg_kgCO2e]

    field_all = merge(field_all, field_well_entry,
                      by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                             'doc_field_code', 'doc_fieldname', 'year'),
                      all = T)
    
    # setcolorder(field_all, c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
    #                          'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
    #                          'doc_field_code', 'doc_fieldname', 'ccs_adopted', 'year', 
    #                          'existing_prod_bbl', 'new_prod_bbl', 'total_prod_bbl', 
    #                          'new_wells', 'existing_ghg_kgCO2e', 'new_ghg_kgCO2e', 'total_ghg_kgCO2e'))
    
    # field_all = merge(field_all, field_existing_ghg,
    #                   by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
    #                          'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
    #                          'doc_field_code', 'doc_fieldname', 'year'),
    #                   all = T)
    # 
    # field_all = merge(field_all, field_new_ghg,
    #                   by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
    #                          'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
    #                          'doc_field_code', 'doc_fieldname', 'ccs_adopted', 'year'),
    #                   all = T)
    # 
    # field_all[is.na(new_ghg_kgCO2e), new_ghg_kgCO2e := 0]
    # 
    # field_all[, total_ghg_kgCO2e := existing_ghg_kgCO2e + new_ghg_kgCO2e]
    
  # aggregate to state level ------
    
    cols = c('new_wells', 'existing_prod_bbl', 'new_prod_bbl', 'total_prod_bbl', 
             'existing_ghg_kgCO2e', 'new_ghg_kgCO2e', 'total_ghg_kgCO2e')
    state_all = field_all[ , lapply(.SD, sum, na.rm = T), .SDcols = cols,
                           by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                  setback_scenario, prod_quota_scenario, excise_tax_scenario, year)] 
    
    state_all[, total_ghg_mtCO2e := total_ghg_kgCO2e/1e9]
    
    
  # save outputs to csv -----
    
    # create subdirectory of save_path for each oil price ------
    
      save_processed_path = file.path(save_path, oil_price_selection)
      dir.create(save_processed_path, showWarnings = FALSE)
      
    # save field-level results -----
      
      field_fname = paste0(oil_price_selection, '-field-level-results.csv')
      fwrite(field_all, file.path(save_processed_path, field_fname), row.names = F)
      print(paste0('Saved field-level results to ', file.path(save_processed_path, field_fname)))
      
    # save state-level results ------
      
      state_fname = paste0(oil_price_selection, '-state-level-results.csv')
      fwrite(state_all, file.path(save_processed_path, state_fname), row.names = F)
      print(paste0('Saved state-level results to ', file.path(save_processed_path, state_fname)))
      
  # create output objects --------
      
    outputs_processed = list(field_all,
                             state_all)
      
    return(outputs_processed)
      

} 


