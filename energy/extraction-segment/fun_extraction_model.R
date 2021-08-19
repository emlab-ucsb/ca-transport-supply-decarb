
run_extraction_model <- function(scenario_selection) {
  
  # set start time -----
    start_time <- Sys.time()
    print(paste("Starting extraction model at ", start_time))
  
  # inputs -----
    model_path        = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
    scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
    entry_file        = 'stocks-flows/entry-input-df/final/entry_df_final_revised.csv'
    coef_file         = 'poisson_regression_coefficients_revised.csv'
    param_file        = 'forecasted_decline_parameters_2020_2045.csv' 
    peak_file         = 'field-year_peak-production_yearly.csv' 
    # prod_file       = 'predicted-production_2020-2045_field_revised.csv'
    prod_vintage_file = 'pred_prod_no_exit_2020-2045_field_start_year_revised.csv' 
    # hist_file       = 'new_wells_pred_weighted_R.csv'  ## update this
    histprod_file     = 'crude_prod_x_field_revised.csv'
    exit_file         = 'exit_regression_coefficients.csv'
    ccs_capture_rate  = 0.61
  
  # source from other scripts -----
    
    # source function to rank costs
      # source(here::here('energy', 'extraction-segment', 'prod_quota.R'))
  
    # source ccs emissions mean b calculation script
      source(here::here('energy', 'scenario-prep', 'ccs_parameterization.R'))
    
    # source function to create matrix of scenarios and forecasted variables
      source(here::here('energy', 'extraction-segment', 'fun_input_scenarios.R'))
    
    ## exit function
    calc_num_well_exits <- function(fe_val, bhat, p_oil, op_hat, opex_val, dhat, depl_val) {
      
      n_well_exit = exp(bhat * p_oil + op_hat * opex_val + dhat * depl_val) * fe_val 
      
    }
    
    
  # load data -----
    
    # load entry data
    entry_dt = fread(file.path(model_path, entry_file), header = T, colClasses = c('doc_field_code' = 'character'))
    
    # load matrix of scenarios and forecasted variables
    scenarios_dt = load_scenarios_dt(scenario_selection)
    
    # load coefficients from poisson regression of historic data
    coefs_dt = fread(file.path(model_path, 'entry-model-results', coef_file), header = T, colClasses = c('doc_field_code' = 'character'))
    coefs_dt = unique(coefs_dt)
    
    # load decline parameters  
    decline_dt = fread(file.path(model_path, 'decline-historic', 'parameters', param_file), header = T, colClasses = c('doc_field_code' = 'character'))
    
    # load peak production for each field
    peak_dt = fread(file.path(model_path, 'decline-historic', 'data', peak_file), header = T, colClasses = c('doc_field_code' = 'character'))
    
    
    # exit file
    exit_coefs = fread(file.path(model_path, 'exit', exit_file), header = T, colClasses = c('doc_field_code' = 'character'))
    exit_coefs[, doc_field_code := sprintf("%03s", doc_field_code)]
    exit_coefs[, doc_fieldname := NULL]
    
    # load forecasted production from existing (pre 2020) wells
    prod_existing_vintage = fread(file.path(model_path, 'predict-production', 'existing_production', prod_vintage_file), header = T, colClasses = c('doc_field_code' = 'character'))
    prod_existing_vintage[, vintage := as.character(start_year)]
    prod_existing_vintage[, setback_scenario := fifelse(setback_scenario == "no_setback", setback_scenario, paste0(setback_scenario, "ft"))]
    
    
    # load historic modeled well entry
    # hist_modeled = fread(file.path(model_path, 'entry-model-results', hist_file), header = T)
    
    # load historic production
    prod_hist = fread(file.path(model_path, 'stocks-flows', histprod_file), header = T, colClasses = c('doc_field_code' = 'character'))
  
  # # rename FieldCode -> doc_field_code -----
  # 
  #   setnames(decline_dt, "FieldCode", "doc_field_code")
  #   setnames(peak_dt, "FieldCode", "doc_field_code")
  #   setnames(prod_hist, "FieldCode", "doc_field_code")
  
  # # rename FieldName -> doc_fieldname -----
  #   setnames(decline_dt, "FieldName", "doc_fieldname")
  #   setnames(peak_dt, "FieldName", "doc_fieldname")
  
  # pad field codes with leading zeroes -----
    coefs_dt[, doc_field_code := sprintf("%03s", doc_field_code)]

  # get peak production median of last two vintages -----
    
    # meas-note: so this next line calculates the median of the peak production (per well) of the last few vintages to apply to new well entries.
    # meas-note: i would change to use the last few start-years (maybe 2000 onwards? or something like that) instead
    peak_prod_median = peak_dt[start_year >= 2000, 
                               lapply(.SD, median, na.rm = TRUE), .SDcols = c("peak_tot_prod", "peak_avg_well_prod", "peak_well_prod_rate"),
                               by = .(doc_field_code)]
  
  # from entry data, keep: field, new wells, depletion ------
    
    setnames(entry_dt, 'n_new_wells', 'new_wells')
    setnames(entry_dt, 'm_cumsum_div_my_prod', 'depl')
    entry_dt_vars = entry_dt[, .(doc_field_code, doc_fieldname, year, brent, capex_imputed, opex_imputed, depl, new_wells)]
    entry_dt = entry_dt[, .(doc_field_code, doc_fieldname, year, new_wells, depl)]
  
  # ccs emissions scalar ---------
  
    ccs_ghg_scalar <- 1 - ccs_capture_rate
  
  # calculate depletion in 2020 -----
  
    depl_2019 = unique(scenarios_dt[year == 2020, .(doc_field_code, 
                                                    oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                    setback_scenario, prod_quota_scenario, excise_tax_scenario)])
    depl_2019 = merge(depl_2019,
                      entry_dt[year == 2019, .(doc_field_code, depl)],
                      by = 'doc_field_code')
    setnames(depl_2019, 'depl', 'depl2019')
    
    prod_2019 = prod_hist[year == 2019, .(doc_field_code, total_bbls)]
    trr_2020 = unique(scenarios_dt[year == 2020, .(doc_field_code, 
                                                   oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                   setback_scenario, prod_quota_scenario, excise_tax_scenario, resource)])
    
    depl_2020 = prod_2019[trr_2020, on = 'doc_field_code']
    depl_2020 = depl_2020[depl_2019, on = .(doc_field_code, 
                                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                            setback_scenario, prod_quota_scenario, excise_tax_scenario)]
    depl_2020 = depl_2020[!is.na(resource)]
    depl_2020 = depl_2020[is.na(total_bbls), total_bbls := 0]
    
    depl_2020[, year := 2020]
    depl_2020[, depl := depl2019 + (total_bbls/resource)]
    
    dt_depl = depl_2020[, .(doc_field_code, 
                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                            setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                            year, depl)]
    
  # calculate ccs costs in 2020 using 2019 production ------
  
    a = 4
    dt_info = unique(scenarios_dt[year == 2020])
    dt_info = merge(dt_info,
                    prod_2019,
                    by = 'doc_field_code')
  
    # adjust ghg emissions factor by innovation scenario
      dt_info[, upstream_kgCO2e_bbl_inno_adj := upstream_kgCO2e_bbl * innovation_multiplier]
    
    # adjust opex by innovation by innovation scenario
      dt_info[, m_opex_imputed_adj := m_opex_imputed*innovation_multiplier]
      dt_info[, wm_opex_imputed_adj := wm_opex_imputed*innovation_multiplier]
    
    # add excise tax to opex
      dt_info[, m_opex_imputed_adj := m_opex_imputed_adj + tax]
      dt_info[, wm_opex_imputed_adj := wm_opex_imputed_adj + tax]
    
      # write functions to solve for b and total cost
      # solve_b <- function(a, p, q) {
      #   f <- (a*p + a*(q^(1/a)) + p)/(a + 1)
      #   return(f)
      # }
      
      solve_tc <- function(a, b, q) {
        f <- (q*(a*b - a*(q^(1/a)) + b))/(a + 1)
        return(f)
      }
    
    # calculate ccs
      dt_info[, upstream_kgCO2e := upstream_kgCO2e_bbl_inno_adj * total_bbls]
      dt_info[, upstream_mtCO2e := upstream_kgCO2e/1e3]
      dt_info[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg*1e3, 'extraction'), 
              by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, 
                     prod_quota_scenario, excise_tax_scenario)]
      dt_info[, total_cost := solve_tc(a, mean_b, upstream_mtCO2e)]
      # dt_info[, b := solve_b(a, ccs_price_usd_per_kg*1e3, upstream_mtCO2e)]
      # dt_info[, mean_b := mean(b, na.rm = T), 
      #         by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
      # dt_info[, total_cost := solve_tc(a, mean_b, upstream_mtCO2e)]
      dt_info[, ccs_adj_usd_per_mt := total_cost/upstream_mtCO2e]
      dt_info[, ccs_adj_usd_per_kg := total_cost/upstream_kgCO2e]
      dt_info[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg] # if no a value (zero emissions), use non-field adjusted ccs price
      # dt_info[, ccs_adoption := ccs_adj_usd_per_kg - carbon_price_usd_per_kg]
      dt_info[, ccs_adoption := ((ccs_capture_rate)*ccs_adj_usd_per_kg + ((1 - ccs_capture_rate)*carbon_price_usd_per_kg)) - (carbon_price_usd_per_kg)]
      dt_info[steam_field == 'no', ccs_adoption := 1] # if not a steam field, make ccs_adoption a positive value so ccs will not be adopted
      dt_info[, m_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                              m_opex_imputed_adj + ((ccs_capture_rate)*upstream_kgCO2e_bbl_inno_adj*ccs_adj_usd_per_kg + (1 - ccs_capture_rate)*carbon_price_usd_per_kg*upstream_kgCO2e_bbl_inno_adj),
                                             m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
      dt_info[, wm_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                               wm_opex_imputed_adj + ((ccs_capture_rate)*upstream_kgCO2e_bbl_inno_adj*ccs_adj_usd_per_kg + (1 - ccs_capture_rate)*carbon_price_usd_per_kg*upstream_kgCO2e_bbl_inno_adj),
                                              wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
    
    ## track ccs adoption
      dt_info[, ccs_adopted := fifelse(ccs_adoption < 0,
                                      1, 0)] 

    ## adjust emissions intensity value again for ccs adoption
      dt_info[, ccs_scalar := ccs_ghg_scalar]
      
      dt_info[, upstream_kgCO2e_bbl_inno_ccs_adj := fifelse(ccs_adoption < 0,
                                                           upstream_kgCO2e_bbl_inno_adj * ccs_ghg_scalar,
                                                           upstream_kgCO2e_bbl_inno_adj)]

    ## add cumulative wells and density   
      dt_info[, cumulative_wells := n_wells_setback]
      dt_info[, wells_km2 := cumulative_wells / (scen_area_m2 / 1e6)]
      
      # dt_info[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
      # dt_info[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
  
  # create data table of all variables needed ------
  
    # dt_info = scenarios_dt[coefs_dt, on = .(doc_field_code, doc_fieldname)]
  
  # vector of years -----
      
    pred_years = c(2020:2045)
  
  # functions ------ 
  
    hypfunc = function(b,t,q_i,D_h) { q_i/((1 + b*D_h*t)^(1/b)) }
    expfunc = function(q_i,d,t) {   q_i*exp(-d*t) }
  

  # scenario combinations ----------
  
    scen_sel = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                       carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])
    
  # keep diagnostics only (if that is input) ------
    
    if (scenario_selection == 'diagnostic') {

      scen_sel = scen_sel[(oil_price_scenario == 'reference case' &
                                     innovation_scenario == 'low innovation' &
                                     carbon_price_scenario == 'price floor' &
                                     ccs_scenario == 'medium CCS cost' &
                                     excise_tax_scenario == 'no tax' &
                                     setback_scenario == 'no_setback' &
                                     prod_quota_scenario == 'no quota') |
                                    (oil_price_scenario == 'reference case' &
                                       innovation_scenario == 'low innovation' &
                                       carbon_price_scenario == 'price floor' &
                                       ccs_scenario == 'medium CCS cost' &
                                       excise_tax_scenario == 'no tax' &
                                       setback_scenario == 'no_setback' &
                                       prod_quota_scenario == 'quota_20') |
                                    (oil_price_scenario == 'reference case' &
                                       innovation_scenario == 'low innovation' &
                                       carbon_price_scenario == 'price floor' &
                                       ccs_scenario == 'medium CCS cost' &
                                       excise_tax_scenario == 'no tax' &
                                       setback_scenario == 'setback_2500ft' &
                                       prod_quota_scenario == 'quota_20')]
    }
    
    
  # START OF LOOP -------

    # scen_sel = scen_combos[oil_price_scenario == fcase(oil_px_selection == 'reference', 'reference case',
    #                                                    oil_px_selection == 'high', 'high oil price',
    #                                                    oil_px_selection == 'low', 'low oil price',
    #                                                    oil_px_selection == 'iea', 'iea oil price')]

    func_yearly_production <- function(z) {
      
      
      print(z)
      scen = scen_sel[z]
      
      dt_info_z = dt_info[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                       carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
      
      dt_depl_z = dt_depl[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                       carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
      
      scenarios_dt_z = scenarios_dt[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                                 carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
      
      setkey(dt_info_z, doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
             setback_scenario, prod_quota_scenario, excise_tax_scenario, year)
      
      setkey(dt_depl_z, doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
             setback_scenario, prod_quota_scenario, excise_tax_scenario, year)
      
      setkey(scenarios_dt_z, doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
             setback_scenario, prod_quota_scenario, excise_tax_scenario, year)

      ## filter prod_existing_vintage
      setback_scen <- unique(scen[, setback_scenario])
      prod_existing_vintage_z = prod_existing_vintage[setback_scenario %in% setback_scen]
      
      ## add column with original year (for diagnostic purposes)
      prod_existing_vintage_z[, orig_year := year]
      prod_existing_vintage_z[, prod_per_well_bbl := fifelse(adj_no_wells > 0, production_bbl / adj_no_wells, 0)]

      ## add column to track if vintage exits
      prod_existing_vintage_z[, no_wells_after_exit := adj_no_wells]
      prod_existing_vintage_z[, doc_fieldname := NULL]
      
      ## create lists and dt for storage
      list_pred_prod = list()
      list_exits = list()
      list_prod_existing = list()
      list_prod_new = list()
      list_cumulative_wells = list()
      prod_new_vintage_z <- data.table()
      
      for (i in seq_along(pred_years)) {
        
        t = pred_years[i]
        
        # print(t)
        
        ## first: predict new wells ----------------------------------------------------------------------
        new_wells = dt_info_z[year == t]
        new_wells = new_wells[dt_depl_z[year == t], on = .(doc_field_code,
                                                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                            setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                                            year), nomatch = 0]
        new_wells = new_wells[coefs_dt, on = .(doc_field_code, doc_fieldname), nomatch = 0]
        
        # poisson regression for all fields
        new_wells[, m_new_wells_pred := fifelse(depl < 0.9999,
                                                 exp(brent_hat*oil_price_usd_per_bbl + capex_hat*m_capex_imputed + opex_hat*m_opex_imputed_adj + depl_hat*depl) * fixed_effect,
                                                 0)]
        # new_wells[, wm_new_wells_pred := fifelse(depl < 0.9999,
        #                                           exp(brent_hat*oil_price_usd_per_bbl + capex_hat*wm_capex_imputed + opex_hat*wm_opex_imputed_adj + depl_hat*depl) * fixed_effect,
        #                                           0)]
        
        setorder(new_wells, 'doc_field_code')
        
        # round number of wells to integer
        # new_wells[, m_new_wells_pred_round := round(m_new_wells_pred, 0)]
        # new_wells[, wm_new_wells_pred_round := round(wm_new_wells_pred, 0)]
        
        ## store the new wells and associated information for year t
        # list_pred_wells[[i]] = new_wells
        
        ## apply exit model: update prod_existing_vintage_z and prod_new_vintage_z to account for exits --------------------------
        ## --------------------------------------------------------------------------------
        
        ## newly predicted in t
        new_wells_exit_t <- new_wells[, c("doc_field_code", "year", "m_new_wells_pred")]
        new_wells_exit_t[, no_wells_after_exit := m_new_wells_pred]
        new_wells_exit_t[, vintage := "new"]
        setnames(new_wells_exit_t, c("year", "m_new_wells_pred"), c("start_year", "adj_no_wells"))
        setcolorder(new_wells_exit_t, c("doc_field_code", "vintage", "start_year", "adj_no_wells", "no_wells_after_exit"))
        
        ## existing
        prod_existing_exit_t = prod_existing_vintage_z[year == t, .(doc_field_code, vintage, start_year, adj_no_wells, no_wells_after_exit)]
        
        
        ## if i > 1, add new wells to the df 
        if(i > 1) {
          
          prod_new_exit_t = prod_new_vintage_z[year == t, .(doc_field_code, vintage, vintage_start, m_new_wells_pred, no_wells_after_exit)]
          setnames(prod_new_exit_t, c("vintage_start", "m_new_wells_pred"), c("start_year", "adj_no_wells"))
          
          exit_dt_t = rbind(prod_existing_exit_t, prod_new_exit_t, new_wells_exit_t)
          
          
        } else { exit_dt_t = rbind(prod_existing_exit_t, new_wells_exit_t)}
        
        ## create df of exit params
        exit_dt_t = exit_dt_t[order(doc_field_code, start_year)]
        
        exit_model_dt = dt_info_z[year == t, .(doc_field_code, oil_price_usd_per_bbl, m_opex_imputed)]
        
        exit_model_dt = merge(exit_model_dt, dt_depl_z[year == t, .(doc_field_code, depl)],
                              by = 'doc_field_code',
                              all.x = T)
        
        exit_model_dt = merge(exit_model_dt, exit_coefs,
                              by = 'doc_field_code',
                              all.x = T)
        
        exit_model_dt[, n_well_exit := calc_num_well_exits(fe_val = fixed_effect,
                                                           bhat = brent_hat,
                                                           p_oil = oil_price_usd_per_bbl, 
                                                           op_hat = opex_hat,
                                                           opex_val = m_opex_imputed, 
                                                           dhat = depl_hat, 
                                                           depl_val = depl)]
        ## store it
        exit_save = copy(exit_model_dt)
        exit_save[, year := t]
        exit_save = merge(dt_info_z[year == t, .(doc_field_code, doc_fieldname, oil_price_scenario,
                                                 innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                 setback_scenario, prod_quota_scenario, excise_tax_scenario)],
                          exit_save,
                          by = "doc_field_code",
                          all.x = T)
        
        list_exits[[i]] = exit_save
        
        ## join well exit to exit_dt_t
        exit_dt_t = merge(exit_dt_t, exit_model_dt[, .(doc_field_code, n_well_exit)],
                          by = 'doc_field_code',
                          all.x = T)
        
        ## for each field, identify oldest vintage that still has wells
        exit_dt_t[, pos_wells := ifelse(no_wells_after_exit > 0, 1, 0)]
        exit_dt_t[, pos_wells := cumsum(pos_wells), by = .(doc_field_code)]
        
        ## cumulative sum of wells
        exit_dt_t[, cumul_wells := cumsum(no_wells_after_exit), by = doc_field_code]
        
        ## determine which vintages get modified by exits
        exit_dt_t[, modify_wells := fifelse(n_well_exit >= cumul_wells, no_wells_after_exit,
                                            fifelse(n_well_exit < cumul_wells & pos_wells == 1, n_well_exit, 0)), by = doc_field_code]
        exit_dt_t[, cumul_exit := cumsum(modify_wells), by = doc_field_code]
        exit_dt_t[, prev_modify_wells := shift(modify_wells, n = 1, type = 'lag'), by = doc_field_code]
        
        ## modify to partially exit from vintage, if necessary
        exit_dt_t[, modify_wells := fifelse(!is.na(prev_modify_wells) & modify_wells == 0 & cumul_exit < n_well_exit & prev_modify_wells > 0, n_well_exit - cumul_exit,
                                            modify_wells)]
        
        exit_dt_t[, modify_wells := fifelse(modify_wells > no_wells_after_exit, no_wells_after_exit, modify_wells)]
        
        ## modify number of wells and production for future years
        exit_dt_t[, no_wells_after_exit_t := no_wells_after_exit - modify_wells]
        
        
        ## modify newly predicted entry in time t
        exit_dt_new_t = exit_dt_t[start_year == t, .(doc_field_code, no_wells_after_exit_t)]
        
        new_wells = merge(new_wells, exit_dt_new_t,
                          by = "doc_field_code",
                          all.x = T)
        
        new_wells[, m_new_wells_pred := no_wells_after_exit_t]
        new_wells[, no_wells_after_exit_t := NULL]
        
        
        ## modify prod_existing_vintage_z
        exit_dt_existing = exit_dt_t[vintage != "new", .(doc_field_code, vintage, start_year, no_wells_after_exit_t)]
        
        prod_existing_vintage_z = merge(prod_existing_vintage_z, exit_dt_existing,
                                        by = c("doc_field_code", "vintage", "start_year"),
                                        all.x = T)
        
        prod_existing_vintage_z[, no_wells_after_exit := fifelse(orig_year >= t, no_wells_after_exit_t, no_wells_after_exit)]
        prod_existing_vintage_z[, production_bbl := no_wells_after_exit * prod_per_well_bbl]
        
        prod_existing_vintage_z[, no_wells_after_exit_t := NULL]
        
        ## if i > 1, modify prod_existing_vintage_z
        if(i > 1) {
          
          exit_dt_new = exit_dt_t[vintage == "new" & start_year < t, .(doc_field_code, vintage, start_year, no_wells_after_exit_t)]
          setnames(exit_dt_new, "start_year", "vintage_start")
          
          prod_new_vintage_z = merge(prod_new_vintage_z, exit_dt_new,
                                     by = c("doc_field_code", "vintage", "vintage_start"),
                                     all.x = T)
          
          prod_new_vintage_z[, no_wells_after_exit := fifelse(orig_year >= t, no_wells_after_exit_t, no_wells_after_exit)]
          prod_new_vintage_z[, production_bbl := no_wells_after_exit * prod_per_well_bbl]
          
          prod_new_vintage_z[, no_wells_after_exit_t := NULL]
          
        }
        
        
        ## determine production from new wells (after exits) -------------------------------------------
        ## --------------------------------------------------------------------------------------------
        
        # calculate prediction of new wells into 2045
        ## meas-check: check to make sure the updates below are correct. original code commented out.
        
        # new_wells_prod = merge(new_wells,
        #                        decline_dt[t == year, .(doc_field_code, q_i, D, b1, b2, d, int_year)], # meas-note: change b1, b2, d --> b, d
        #                        by = 'doc_field_code',
        #                        all.x = T)
        
        
        new_wells_prod = merge(new_wells,
                               decline_dt[t == year, .(doc_field_code, q_i, D, b, d, int_year)], # meas-note: change b1, b2, d --> b, d
                               by = 'doc_field_code',
                               all.x = T)
        
        # param_other = unique(decline_dt[t == year & doc_fieldname == 'other', .(q_i, D, b1, b2, d, int_year)])
        param_other = unique(decline_dt[t == year & doc_fieldname == 'other', .(q_i, D, b, d, int_year)])
        
        new_wells_prod[is.na(q_i), q_i := param_other[, q_i]]
        new_wells_prod[is.na(D), D := param_other[, D]]
        
        # new_wells_prod[is.na(b2) & is.na(b1), b2 := param_other[, b2]]
        # new_wells_prod[is.na(d) & is.na(b1), d := param_other[, d]]
        # new_wells_prod[is.na(int_year) & is.na(b1), int_year := param_other[, int_year]]
        new_wells_prod[is.na(b), b := param_other[, b]]
        new_wells_prod[is.na(d), d := param_other[, d]]
        new_wells_prod[is.na(int_year), int_year := param_other[, int_year]]
        
        
        new_wells_prod = new_wells_prod[peak_prod_median[, .(doc_field_code, peak_avg_well_prod)], on = 'doc_field_code', nomatch = 0]
        
        ## adjust number of wells for setback scenario
        new_wells_prod[, m_new_wells_pred := m_new_wells_pred * (1 - area_coverage)]
        
        ## calculate peak production
        new_wells_prod[, peak_production := m_new_wells_pred * peak_avg_well_prod]
        
        
        
        ## recalculate density with new wells 
        # new_wells_prod[, density_check := cumulative_wells + m_new_wells_pred / (scen_area_m2 / 1e6)]

        ## implement production quota before calculating prod from new wells ---------------------------------------------
        ## rank existing and new wells in each field by costs
        
        ## select cost info for year t
        dtt = unique(dt_info_z[year == t,
                               .(doc_field_code, doc_fieldname, m_capex_imputed, m_opex_imputed_adj,
                                 oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                 setback_scenario, prod_quota_scenario, excise_tax_scenario)])
        ## create columns: cost for existing wells and cost for new wells
        dtt[, cost_existing := m_opex_imputed_adj]
        dtt[, cost_new := m_capex_imputed + m_opex_imputed_adj]
        ## melt by costs
        dtt_long = melt(dtt, measure.vars = c('cost_new', 'cost_existing'), variable.name = 'cost_type', value.name = 'cost')
        ## rank field (existing and new) costs -- note that multiple fields have same cost
        dtt_long[, cost_rank := rank(unclass(cost)), by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                            setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        ## wide format
        dt_info_rank = dcast(dtt_long, doc_field_code + doc_fieldname + m_capex_imputed + m_opex_imputed_adj + oil_price_scenario + innovation_scenario +  carbon_price_scenario + ccs_scenario +  setback_scenario + prod_quota_scenario + excise_tax_scenario ~ cost_type, 
                             value.var = c('cost', 'cost_rank'))
        ## rename
        setnames(dt_info_rank, 'cost_rank_cost_new', 'cost_new_rank')
        setnames(dt_info_rank, 'cost_rank_cost_existing', 'cost_existing_rank')
        
        ## select columns
        temp_dt_info_rank = dt_info_rank[, c("doc_field_code","doc_fieldname",
                                            "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                            "setback_scenario", "prod_quota_scenario", "excise_tax_scenario",
                                            "cost_existing_rank","cost_new_rank")]
        
        # set up copy of dataframe for newly predicted wells for time t
        # use this for production in time t and projecting future prod for this vintage
        # only include fields that are predicted to have entry and production
        temp_new_wells_prod = new_wells_prod[year==t & m_new_wells_pred > 0 & peak_production > 0] 
        temp_new_wells_prod = temp_new_wells_prod[,c("doc_field_code", "doc_fieldname", 
                                                     "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                                     "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", 
                                                     "innovation_multiplier", "quota", "ccs_adopted", "ccs_scalar",  "upstream_kgCO2e_bbl", "upstream_kgCO2e_bbl_inno_adj", 
                                                     "upstream_kgCO2e_bbl_inno_ccs_adj", "m_new_wells_pred","peak_production")]
        setnames(temp_new_wells_prod, "peak_production", "production_bbl")
        setnames(temp_new_wells_prod, "m_new_wells_pred", "num_wells")
        temp_new_wells_prod[, vintage := 'new']
        temp_new_wells_prod[, vintage_start := t]
        
        # set up dataframe with production in year t for existing field-vintages 
        ## ---------------------------------------------------------
        
        
        ## filter dt_info_z for year t since actual quota number, ccs adoption, and ghg emissions intensity change year to year
        temp_prod_existing_vintage = dt_info_z[year == t, . (doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                                             ccs_scenario, prod_quota_scenario, excise_tax_scenario, setback_scenario, quota, innovation_multiplier,
                                                             ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj)]
        ## filter existing vintage production for year t
        existing_vintage_prod_t = prod_existing_vintage_z[year == t]
        existing_vintage_prod_t = existing_vintage_prod_t[, .(setback_scenario, doc_field_code, vintage, production_bbl)]
       
        ## merge field info for time t with production for time t
        temp_prod_existing_vintage = merge(temp_prod_existing_vintage, existing_vintage_prod_t,
                                           by = c("doc_field_code", "setback_scenario"),
                                           all.x = T,
                                           allow.cartesian = T)
        ## add number of wells column, == 1
        temp_prod_existing_vintage[, num_wells := 1]
        # temp_prod_existing_vintage[, num_wells := n_wells]
        # temp_prod_existing_vintage[, n_wells := NULL]
        
        ## select, reorder columns
        temp_prod_existing_vintage = temp_prod_existing_vintage[, .(doc_field_code, doc_fieldname, oil_price_scenario, 
                                                                    innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                                    quota, innovation_multiplier, ccs_adopted, 
                                                                    ccs_scalar, upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, 
                                                                    upstream_kgCO2e_bbl_inno_ccs_adj, num_wells, production_bbl, vintage)]
        
        
        ## add vintage start column
        # temp_prod_existing_vintage[, vintage_start := fifelse(vintage == 'pre 1978', 1977, as.numeric(substr(vintage, 1, 4)))]
        temp_prod_existing_vintage[, vintage_start := as.numeric(vintage)]
        
        # set up dataframe with production in year t for field-vintages that start 2020 to t - 1
        ## -------------------------------------------------------------------------
        # temp_prod_new_vintage = rbindlist(list_pred_prod)
        if(i > 1) {
            
            temp_prod_new_vintage_info = dt_info_z[year == t, . (doc_field_code, oil_price_scenario, 
                                                                 innovation_scenario, carbon_price_scenario, 
                                                                 ccs_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                                 setback_scenario, quota, innovation_multiplier, ccs_adopted, ccs_scalar, 
                                                                 upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj)]
            
            temp_prod_new_vintage_t <- prod_new_vintage_z[year == t]
            
            temp_prod_new_vintage_t <- merge(temp_prod_new_vintage_t, temp_prod_new_vintage_info,
                                        by = c("doc_field_code", "oil_price_scenario", 
                                               "innovation_scenario", "carbon_price_scenario", 
                                               "ccs_scenario", "setback_scenario", 
                                               "prod_quota_scenario", "excise_tax_scenario"),
                                        all.x = T)
            
            setnames(temp_prod_new_vintage_t, 'm_new_wells_pred', 'num_wells')
            
            temp_prod_new_vintage_t <- temp_prod_new_vintage_t[, .(doc_field_code, doc_fieldname, oil_price_scenario, 
                                       innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                       setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                       innovation_multiplier, quota, ccs_adopted, 
                                       ccs_scalar, upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, 
                                       upstream_kgCO2e_bbl_inno_ccs_adj, num_wells, production_bbl,
                                       vintage, vintage_start)]
            
          # one data table with production data of both new and existing wells
          temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage, temp_prod_new_vintage_t, use.names = T)
          
        } else {
          
          temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage, use.names = T)
          
        }
        
    ## join costs to production info; rank; implement quota
    ## ---------------------------------------------------------
        
        # join production dataframe to cost rankings
        temp_prod_quota = merge(temp_prod_quota, temp_dt_info_rank, 
                                by = c("doc_field_code", "doc_fieldname", "oil_price_scenario", "innovation_scenario", 
                                       "carbon_price_scenario", "ccs_scenario", "setback_scenario", "prod_quota_scenario", 
                                       "excise_tax_scenario"),
                                all.x = T)
        
        # create column with appropriate cost rank based on if vintage is new as of year t
        temp_prod_quota[, cost_rank := fifelse(vintage_start == t, cost_new_rank, cost_existing_rank)]
        # remove other cost rank columns
        temp_prod_quota[, c('cost_existing_rank', 'cost_new_rank') := NULL]
        
        # calculate cumulative production in ascending order of rank then descending order of vintage 
        # (i.e. assume that within a field, older vintages get shut first)
        setkey(temp_prod_quota, doc_field_code,
               oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario)
        
        ## calculate cumulative sum by scenario, arranging by cost rank and cost vintage rank
        temp_prod_quota = temp_prod_quota[!is.na(cost_rank)]
        
        ## add column to randomize order when cost_rank and vintge_start are the same
        temp_prod_quota[, random := sample.int(.N, replace = FALSE), by = .(cost_rank, vintage_start)]
        
        ## set order based on cost_rank, vintage_start, and random
        setorder(temp_prod_quota, cost_rank, -vintage_start, random)

        ## replace na prod with zero
        temp_prod_quota[is.na(production_bbl), production_bbl := 0]
        
        ## calculate cumulative sum of extraction
        temp_prod_quota[, prod_cumsum := cumsum(production_bbl), by = .(oil_price_scenario, innovation_scenario, 
                                                                        carbon_price_scenario, ccs_scenario, 
                                                                        setback_scenario, prod_quota_scenario, excise_tax_scenario)]

        
        ## new column for how much is produced under prod quota
        ## set all field-vintages that are greater than prod quota to zero, i.e. not allowed to produce
        # temp_prod_quota[, prod_limited := fifelse(prod_cumsum > quota, 0, prod_cumsum)]
        
        # find field-vintage that prod quota lands in and refine  
        # if vintage is before 
        # if vintage is new, calculate no. of new wells that can enter
        
        ## add column that indicates if cumulative production is greater than quota
        temp_prod_quota[, over_quota_ranks := fifelse(prod_cumsum > quota, 1, 0)]
        
        
        ## create column with previous cumulative production
        ## create column of cumulative sum of over_quota_ranks to identify first field vintage to bust quota
        temp_prod_quota[, ':=' (last_prod = shift(prod_cumsum, n = 1, fill = NA, type = "lag"),
                                sum_over = cumsum(over_quota_ranks)), by = .(oil_price_scenario, innovation_scenario, 
                                                                             carbon_price_scenario, ccs_scenario, 
                                                                             setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        
        ## calculate and use remainder of quota for field-vintage that "busts" quota
        ## all other field-vintages over the quota produce 0
        temp_prod_quota[, prod_quota_remainder := quota - last_prod]
        temp_prod_quota[, adj_prod_limited := fifelse(sum_over == 1, prod_quota_remainder,
                                                      fifelse(sum_over > 1, 0, production_bbl))]
        
        ## recalculate the number of new wells for field-vintage starting in t if that field-vintage busts quota
        temp_prod_quota[, adj_new_wells := fifelse(sum_over == 1 & vintage_start == t, adj_prod_limited / (production_bbl / num_wells), num_wells)]
        
        ## set columns as numeric
        temp_prod_quota[, c('adj_prod_limited', 'adj_new_wells') := lapply(.SD, as.numeric), .SDcols = c('adj_prod_limited', 'adj_new_wells')] 
        
        ## add column to track if field-vintage did not produce because of quota
        temp_prod_quota[, zero_prod_quota := fifelse(adj_prod_limited == 0 & sum_over > 0, 1, 0)]
        
        ## update existing vintage production years to reflect if a field-vintage doesn't produce due to the quota
        ## start by creating df filtered for "existing" vintages
        zero_prod_quota_old <- temp_prod_quota[vintage != "new", .(doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                                                   ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                                                   vintage, zero_prod_quota)]
        
        ## merge prod existing with previous dt
        prod_existing_vintage_z <- merge(prod_existing_vintage_z, zero_prod_quota_old,
                                         by = c('doc_field_code', 'vintage', 'setback_scenario'),
                                         all.x = T)
      
      
        ## shift years if vintage doesn't produce due to quota
        prod_existing_vintage_z[, year := fifelse(zero_prod_quota == 1, year + 1, year)]
        prod_existing_vintage_z[, c('zero_prod_quota', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario',
                                    'ccs_scenario', 'prod_quota_scenario', 'excise_tax_scenario') := NULL]
      
        ## update new vintage production curve later
  
      
        ## save production for year t
        ## -----------------------------------------------------------
        
        ## remove columns
        temp_prod_quota[, c('production_bbl', 'num_wells', 'over_quota_ranks', 
                            'sum_over', 'last_prod', 'prod_quota_remainder', 'prod_cumsum') := NULL]
        
        ## calculate emissions
        temp_prod_quota[, ':=' (year = t,
                                upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
                                upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
                                upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited)]
        
        
        ## select and rename columns
        temp_prod_quota = temp_prod_quota[, .(year, doc_field_code, doc_fieldname, vintage, 
                                              vintage_start, oil_price_scenario, innovation_scenario, 
                                              carbon_price_scenario, ccs_scenario, setback_scenario, 
                                              prod_quota_scenario, excise_tax_scenario, 
                                              quota, innovation_multiplier, ccs_adopted, 
                                              ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
                                              n_wells = adj_new_wells, production_bbl = adj_prod_limited, zero_prod_quota,
                                              upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
        
        
        ## filter out vintages that start in year t (predicted) but produce 0 due to quota
        temp_prod_quota = temp_prod_quota[!(vintage_start == t & zero_prod_quota == 1)]
        

        ## store new well production (all vintages) for time t
        list_prod_new[[i]] = temp_prod_quota[vintage == "new"]
        
        
        ## store existing well production for time t
        list_prod_existing[[i]] = temp_prod_quota[vintage != "new"]
        
        
        ## set up new wells prod df that will be used to find future production for new wells
        ## this will be used to predict future annual production for new wells
        
        ## tracey's attempt: set vintage == t to filter for only new wells from time t (not previous "new" wells vintages between 2020 and t-1)
        ## vintage == t
        # temp_prod_quota_new_wells[, vintage_start := t]
        temp_prod_quota_new_wells = temp_prod_quota[vintage_start == t]

        temp_prod_quota_new_wells[, c('quota', 'ccs_adopted', 'innovation_multiplier', 'ccs_scalar',
                                      'upstream_kgCO2e_bbl_inno_adj', 'upstream_kgCO2e_bbl_inno_ccs_adj',
                                      'upstream_kgCO2e', 'upstream_kgCO2e_inno_adj', 'upstream_kgCO2e_inno_ccs_adj', 'zero_prod_quota') := NULL]

        ## filter predicted wells dt for only those that have predicted wells and production
        new_wells_prod_new = new_wells_prod[year == t & m_new_wells_pred > 0 & peak_production > 0]
        new_wells_prod_new[, c('peak_production', 'm_new_wells_pred') := NULL]
        
       
        ## merge with production info from year t
        new_wells_prod_new = merge(new_wells_prod_new, temp_prod_quota_new_wells,
                                   by = c("doc_field_code", "year", 
                                          "doc_fieldname", "oil_price_scenario", 
                                          "innovation_scenario", "carbon_price_scenario", 
                                          "ccs_scenario", "setback_scenario", 
                                          "prod_quota_scenario", "excise_tax_scenario"),
                                   all.x = T)
        
        new_wells_prod_new = unique(new_wells_prod_new)
        new_wells_prod_new[, vintage := NULL]
        ## rename, prepare to project production out for new vintages from time t
        setnames(new_wells_prod_new, c('production_bbl', 'n_wells'), c('peak_production', 'm_new_wells_pred'))
        ## filter new field vintages that have positive production 
        new_wells_prod_new = new_wells_prod_new[peak_production > 0]
        
        ## update cumulative number of wells
        ## tracey -- move this somewhere else?
        new_wells_prod_new = new_wells_prod_new[, cumulative_wells := cumulative_wells + m_new_wells_pred]
        new_wells_prod_new = new_wells_prod_new[, wells_km2 := cumulative_wells / (scen_area_m2 / 1e6)]
        
        ## cumulative wells
        cumulative_wells_dt = new_wells_prod_new[, .(doc_field_code, year, oil_price_scenario, innovation_scenario,
                                                     carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
                                                     excise_tax_scenario, cumulative_wells, wells_km2)]
        
        
        list_cumulative_wells[[i]] = cumulative_wells_dt
  
        ## remove objects
        rm(dt_info_rank, temp_dt_info_rank, existing_vintage_prod_t, dtt_long, temp_new_wells_prod, 
           temp_prod_existing_vintage, temp_prod_quota, temp_prod_quota_new_wells, zero_prod_quota_old)  
        ## end section that implements production quota
        
        ## predict decline curves for new field vintages (those that start in year t)
        ## --------------------------------------------------------------------------
        
        # for 2020 up to current year, make prod = 0
        for (j in 2020:t) { 
          new_wells_prod_new[, col := 0]
          setnames(new_wells_prod_new, 'col', as.character(j))
        }
        
        # at entry year, make production = peak production
        new_wells_prod_new[, as.character(t) := peak_production]
        
        
        # for years following entrance, implement decline curves
        # rl revert to this code to remove setback adjustment
        ## meas-check: confirm that updated code below is correct
        for (j in (t + 1):2045) {
          new_wells_prod_new[is.na(b), col := expfunc(peak_production, d, j - t)]
          new_wells_prod_new[! is.na(b), col :=  fifelse(j < t + int_year,
                                                         hypfunc(b, j - t, peak_production, D),
                                                         expfunc(peak_production, d, j - t))  ]
          setnames(new_wells_prod_new, 'col', as.character(j))
        }
        
        # organize production
        setnames(new_wells_prod_new, 'year', 'vintage_start')
        
        ## get rid of wm_new_wells_pred
        new_wells_prod_new = new_wells_prod_new[, c('vintage_start', 'doc_field_code', 'doc_fieldname', 
                                                    'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                                    'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                                    'oil_price_usd_per_bbl', 'm_new_wells_pred', as.character(2020:2045))]
        
        # melt longer
        new_wells_prod_long = melt(new_wells_prod_new, id.vars = c('vintage_start', 'doc_field_code', 'doc_fieldname', 
                                                                   'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                                                   'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                                                   'oil_price_usd_per_bbl', 'm_new_wells_pred'),
                                   measure.vars = as.character(2020:2045), variable.name = 'year', value.name = 'production_bbl')
        
        new_wells_prod_long[, year := as.numeric(as.character(year))]
        
        
        ## store future production of new vintages
        list_pred_prod[[i]] = new_wells_prod_long[year > t,  .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
                                                               carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
                                                               excise_tax_scenario, vintage_start, year, m_new_wells_pred, production_bbl)]
        
        ## update production curve years for new vintages (vintage start 2020 on) based on quota
        ## -------------------------------------------------------------------------------------
          new_wells_prod_long = new_wells_prod_long[year >= vintage_start]
          new_wells_prod_long[, orig_year := year]
          new_wells_prod_long[, vintage := "new"]
          new_wells_prod_long[, no_wells_after_exit := m_new_wells_pred]
          new_wells_prod_long[, prod_per_well_bbl := production_bbl / m_new_wells_pred]
        
        ## update new well production df
          prod_new_vintage_z <- rbind(prod_new_vintage_z, new_wells_prod_long)
          
          ## production from new vintages in time t
          zero_prod_quota_new <- list_prod_new[[i]]
          
          ## select columns
          zero_prod_quota_new <- zero_prod_quota_new[, .(doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                                     ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                                     vintage, vintage_start, zero_prod_quota)]
          
          setorder(prod_new_vintage_z, doc_field_code, vintage_start, year)
          prod_new_vintage_z <- merge(prod_new_vintage_z, zero_prod_quota_new,
                                      by = c("doc_field_code", "oil_price_scenario", 
                                             "innovation_scenario", "carbon_price_scenario", 
                                             "ccs_scenario", "setback_scenario", 
                                             "prod_quota_scenario", "excise_tax_scenario",
                                             "vintage", "vintage_start"),
                                      all.x = T)
          
          ## adjust year if no production due to quota
          prod_new_vintage_z[, year := fifelse(zero_prod_quota == 1, year + 1, year)]
          prod_new_vintage_z[, zero_prod_quota := NULL]
          
        if(t < max(pred_years)) {
          
          ## set up parameters for next year, e.g., calculate depletion of next year
          ## -------------------------------------------
          
          ## production from new wells
          prod_new = list_prod_new[[i]]
          prod_new = prod_new[, .(production_bbl = sum(production_bbl, na.rm = T)),
                              by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                     ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
          setnames(prod_new, 'production_bbl', 'new_bbl')
          # prod_new[, year := as.numeric(as.character(year))]
          
          ## production from old wells
          prod_old = list_prod_existing[[i]]
          prod_old = prod_old[, .(old_bbl = sum(production_bbl, na.rm = T)),
                              by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
                                     carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
          # prod_old[, year := as.numeric(year)]
          
          ## merge prod_new and prod_old
          prod_next_year = merge(prod_new, prod_old, 
                                 by = c("doc_field_code", "doc_fieldname", 
                                        "oil_price_scenario", "innovation_scenario", 
                                        "carbon_price_scenario", "ccs_scenario", 
                                        "setback_scenario", "prod_quota_scenario", 
                                        "excise_tax_scenario", "year"),
                                 all = T)
          
          ## replace NAs after join with zero
          prod_next_year[, ':=' (old_bbl = fifelse(is.na(old_bbl), 0, old_bbl),
                                 new_bbl = fifelse(is.na(new_bbl), 0, new_bbl))]
          
          ## create column with sum of production by field
          prod_next_year[, production_bbl := old_bbl + new_bbl]
          prod_next_year = prod_next_year[, .(doc_field_code, 
                                              oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                              setback_scenario, prod_quota_scenario, excise_tax_scenario, year, production_bbl)]
          
          
          ## filter depletion df for year t
          depl_prev = dt_depl_z[year == t, .(doc_field_code, oil_price_scenario, innovation_scenario, 
                                             carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, depl)]
          setnames(depl_prev, 'depl', 'depl_prev')
          depl_prev = unique(depl_prev)
          
         
          ## filter total recoverable resource for year t
          trr_prev = unique(scenarios_dt_z[year == t, .(doc_field_code, oil_price_scenario, 
                                                        innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                        setback_scenario, prod_quota_scenario, excise_tax_scenario, resource)])
          trr_prev = unique(trr_prev)
          
          ## merge previous depl and trr 
          prod_next_year = prod_next_year[depl_prev, on = .(doc_field_code, 
                                                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                            setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          
          prod_next_year = prod_next_year[trr_prev, on = .(doc_field_code, 
                                                           oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario)]
         
          ## fill in missing info
          prod_next_year[, ':=' (year = fifelse(is.na(year), t, year),
                                 production_bbl = fifelse(is.na(production_bbl), 0 , production_bbl))]
          
          ## calculate depletion for year t + 1
          prod_next_year[, depl := depl_prev + (production_bbl / resource)]
          prod_next_year = unique(prod_next_year)
        
          
          ## select depl column, update year to be t + 1
          depl_next_year = prod_next_year[, .(doc_field_code, 
                                              oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                              setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                              year, depl)]
          depl_next_year[, year := t + 1]
          depl_next_year = unique(depl_next_year)
          
          ## bind to dt_depl_z
          dt_depl_z = rbindlist(list(dt_depl_z, depl_next_year))
          
          
          # calculate adjustments to input variables for next year
          ## ----------------------------------------------------
          
          ## filter for t + 1
          info_next_year = unique(scenarios_dt_z[year == t + 1])
          
          ## merge info next year to prod_next year
          info_next_year = merge(info_next_year,
                                 prod_next_year[, .(doc_field_code, 
                                                    oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, production_bbl)],
                                 by = c('doc_field_code',  
                                        'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                        'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'))
          
          setnames(info_next_year, 'production_bbl', 'total_bbls')
          
          ## ccs adoption in time t
          ccs_prev = dt_info_z[year == t, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                            setback_scenario, prod_quota_scenario, excise_tax_scenario, ccs_adoption)]
          setnames(ccs_prev, 'ccs_adoption', 'adoption_prev')
          
          ## merge with info next year dt
          info_next_year = merge(info_next_year, ccs_prev, 
                                 by = c('doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                        'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'))
          

          
          ## update well density for year t
          ## --------------------------------------------
          
          cumul_wells_end = cumulative_wells_dt[year == t]
          setnames(cumul_wells_end, c('cumulative_wells', 'wells_km2'), c('cumul_wells_end', "wells_km2_end"))
          
          ## first update dt_info_z
          dt_info_z = merge(dt_info_z, cumul_wells_end,
                            by = c('doc_field_code', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                   'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'year'),
                            all = T)
          
          dt_info_z = dt_info_z[, cumulative_wells := fifelse(is.na(cumul_wells_end), cumulative_wells, cumul_wells_end)]
          dt_info_z = dt_info_z[, wells_km2 := fifelse(is.na(wells_km2_end), wells_km2, wells_km2_end)]
          
          dt_info_z = dt_info_z[, c('cumul_wells_end', 'wells_km2_end') := NULL]
      
          ## add updated cumulative wells and density
          density_start_next = dt_info_z[year == t, .(doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                      setback_scenario, prod_quota_scenario, excise_tax_scenario, cumulative_wells, wells_km2)]
          
          info_next_year = merge(info_next_year, density_start_next,
                                 by = c('doc_field_code', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                        'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'),
                                 all = T)
    
          
          # adjust ghg emissions factor by innovation scenario
          info_next_year[, upstream_kgCO2e_bbl_inno_adj := upstream_kgCO2e_bbl * innovation_multiplier]
          
          # adjust opex by innovation scenario
          info_next_year[, m_opex_imputed_adj := m_opex_imputed * innovation_multiplier]
          info_next_year[, wm_opex_imputed_adj := wm_opex_imputed * innovation_multiplier]
          
          # add excise tax to opex
          info_next_year[, m_opex_imputed_adj := m_opex_imputed_adj + tax]
          info_next_year[, wm_opex_imputed_adj := wm_opex_imputed_adj + tax]
          
          # calculate ccs
          info_next_year[, upstream_kgCO2e := upstream_kgCO2e_bbl_inno_adj * total_bbls]
          info_next_year[, upstream_mtCO2e := upstream_kgCO2e / 1e3]
          info_next_year[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg * 1e3, 'extraction'), 
                         by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          info_next_year[, total_cost := solve_tc(a, mean_b, upstream_mtCO2e)]
          
          
          info_next_year[, ccs_adj_usd_per_mt := total_cost / upstream_mtCO2e]
          info_next_year[, ccs_adj_usd_per_kg := total_cost / upstream_kgCO2e]
          info_next_year[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg]
          # info_next_year[, ccs_adoption := fifelse(adoption_prev > 0,
          #                                         ccs_adj_usd_per_kg - carbon_price_usd_per_kg,
          #                                         adoption_prev)]
          info_next_year[, ccs_adoption := fifelse(adoption_prev > 0,
                                                   ((ccs_capture_rate)*ccs_adj_usd_per_kg + ((1 - ccs_capture_rate)*carbon_price_usd_per_kg)) - (carbon_price_usd_per_kg),
                                                   adoption_prev)]
          info_next_year[steam_field == 'no', ccs_adoption := 1] # if not a steam field, make ccs_adoption a positive value so ccs will not be adopted
          info_next_year[, m_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                                        m_opex_imputed_adj + ((ccs_capture_rate)*upstream_kgCO2e_bbl_inno_adj*ccs_adj_usd_per_kg + (1 - ccs_capture_rate)*carbon_price_usd_per_kg*upstream_kgCO2e_bbl_inno_adj),
                                                        m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
          info_next_year[, wm_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                                         wm_opex_imputed_adj + ((ccs_capture_rate)*upstream_kgCO2e_bbl_inno_adj*ccs_adj_usd_per_kg + (1 - ccs_capture_rate)*carbon_price_usd_per_kg*upstream_kgCO2e_bbl_inno_adj),
                                                         wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
          # info_next_year[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
          # info_next_year[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
          info_next_year[, adoption_prev := NULL]
          
          ## track ccs adoption
          info_next_year[, ccs_adopted := fifelse(ccs_adoption < 0,
                                                 1, 0)] 
          ## add scalar info
          info_next_year[, ccs_scalar := ccs_ghg_scalar]
          
          ## adjust emissions intensity value again for ccs adoption
          info_next_year[, upstream_kgCO2e_bbl_inno_ccs_adj := fifelse(ccs_adoption < 0,
                                                                      upstream_kgCO2e_bbl_inno_adj * ccs_ghg_scalar,
                                                                      upstream_kgCO2e_bbl_inno_adj)]
          
          info_next_year = unique(info_next_year)
          
          
          # combine input variables
          dt_info_z = rbindlist(list(dt_info_z, info_next_year), use.names = T)
          
        
          
        }  
        
        rm(t,j,new_wells,param_other,new_wells_prod, new_wells_prod_new, new_wells_prod_long,
           prod_new,prod_old,prod_next_year,depl_prev,trr_prev,depl_next_year,info_next_year,dtt,zero_prod_quota_new, cumulative_wells_dt, density_start_next,
           prod_existing_exit_t, prod_new_exit_t, exit_dt_t, exit_model_dt, exit_save, exit_dt_existing, exit_dt_new, new_wells_exit_t, exit_dt_new_t)
        
      }
      
      # rm(dt_depl_z)
      
      # # pred_prod = rbindlist(list_pred_prod) ##
      # # pred_prod_wide = rbindlist(list_pred_prod_wide)
      # list_existing_z[[z]] = rbindlist(list_prod_existing)
      # # pred_prod[, year := as.numeric(as.character(year))]
      # list_new_z[[z]] = rbindlist(list_prod_new)
      
      # prod_existing = rbindlist(list_prod_existing)
      # prod_new = rbindlist(list_prod_new)

      # output_list = list(rbindlist(list_prod_existing)[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
      #                                                      setback_scenario, prod_quota_scenario, excise_tax_scenario, 
      #                                                      doc_field_code, doc_fieldname, year, ccs_adopted, production_bbl, 
      #                                                      upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)],
      #                    rbindlist(list_prod_new)[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
      #                                                 setback_scenario, prod_quota_scenario, excise_tax_scenario, 
      #                                                 doc_field_code, doc_fieldname, year, vintage_start, ccs_adopted, production_bbl, n_wells,
      #                                                 upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)])

      
      
      existing_prod_dt = rbindlist(list_prod_existing)[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                           setback_scenario, prod_quota_scenario, quota, excise_tax_scenario, doc_field_code, 
                                                           doc_fieldname, vintage, vintage_start, year, ccs_adopted, production_bbl, 
                                                           zero_prod_quota, n_wells, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
      new_prod_dt = rbindlist(list_prod_new)[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                 setback_scenario, prod_quota_scenario, quota, excise_tax_scenario, doc_field_code, 
                                                 doc_fieldname, vintage, vintage_start, year, ccs_adopted, production_bbl, 
                                                 zero_prod_quota, n_wells, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
      
      
      exit_dt = rbindlist(list_exits)
      setnames(exit_dt, "year", "orig_year")
    
      
      ## join with prod_existing_vintage_z, prod_new_vintage_z

      exit_out = merge(prod_new_vintage_z[, .(doc_field_code, year, orig_year, vintage, vintage_start, m_new_wells_pred,
                                              prod_per_well_bbl, no_wells_after_exit, production_bbl)],
                       exit_dt[, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                   ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, orig_year, n_well_exit)],
                       by = c('doc_field_code', 'orig_year'),
                       all.x = T)
      
      setnames(exit_out, c('vintage_start', 'm_new_wells_pred'), c('start_year', 'adj_no_wells'))
      
      exit_out_existing = merge(prod_existing_vintage_z[, .(doc_field_code, year, orig_year, vintage, start_year, adj_no_wells,
                                                            prod_per_well_bbl, no_wells_after_exit, production_bbl)],
                                exit_dt[, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                            ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, orig_year, n_well_exit)],
                                by = c('doc_field_code', 'orig_year'),
                                all.x = T)
      
      exit_out = rbind(exit_out, exit_out_existing)
      exit_out = exit_out[order(doc_field_code, start_year)]
      
      
      rm(list_pred_prod, list_prod_existing, list_prod_new, exit_out_existing, prod_existing_vintage_z, prod_new_vintage_z, list_exits)
      
      
      
      existing_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
      new_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
      
      setkey(existing_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
             setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
      setkey(new_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
             setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
      
      ## combine new and old production
      vintage_all <- rbind(existing_prod_dt, new_prod_dt)
      setorder(vintage_all, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, 
               doc_fieldname, year, vintage_start)
      
      
      ## field well entry
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
      
      field_all = merge(field_existing_info, field_new_info,
                        by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                               'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                               'doc_field_code', 'doc_fieldname', 'ccs_adopted', 'year'),
                        all = T)
      
      field_all[is.na(existing_prod_bbl), new_prod_bbl := 0]
      field_all[is.na(new_prod_bbl), new_prod_bbl := 0]
      field_all[, total_prod_bbl := existing_prod_bbl + new_prod_bbl]
      
      field_all[is.na(existing_ghg_kgCO2e), new_ghg_kgCO2e := 0]
      field_all[is.na(new_ghg_kgCO2e), new_ghg_kgCO2e := 0]
      field_all[, total_ghg_kgCO2e := existing_ghg_kgCO2e + new_ghg_kgCO2e]
      
      field_all = merge(field_all, field_well_entry,
                        by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                               'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                               'doc_field_code', 'doc_fieldname', 'year'),
                        all = T)
      
      field_all[is.na(new_wells), new_wells := 0]
      
      ## add density
    
      density_dt = dt_info_z[, c('doc_field_code', 'year', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'n_wells_start',
                                 'orig_area_m2', 'n_wells_setback', 'scen_area_m2',  'cumulative_wells', 'wells_km2')]
      
      density_dt = merge(density_dt, field_all,
                         by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                'doc_field_code', 'year'),
                         all = T)
      
      density_dt = density_dt[, .(doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                  ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, n_wells_start,
                                  orig_area_m2, n_wells_setback, scen_area_m2, year, new_wells, cumulative_wells, wells_km2)]
      
      density_dt_merg = density_dt[, .(doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                       year, cumulative_wells, wells_km2)]
      
      field_all = merge(field_all, density_dt_merg,
                        by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                               'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                               'doc_field_code', 'year'))
      
      
      

      cols = c('new_wells', 'existing_prod_bbl', 'new_prod_bbl', 'total_prod_bbl', 
               'existing_ghg_kgCO2e', 'new_ghg_kgCO2e', 'total_ghg_kgCO2e')
      state_all = field_all[ , lapply(.SD, sum, na.rm = T), .SDcols = cols,
                             by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, year)] 
      
      state_all[, total_ghg_mtCO2e := total_ghg_kgCO2e/1e9]
      
      output_scen = list(vintage_all,
                         field_all,
                         state_all,
                         density_dt,
                         exit_out,
                         dt_depl_z)
      
      rm(vintage_all, state_all, field_all, existing_prod_dt, new_prod_dt, dt_info_z, density_dt_merg, density_dt, exit_out)

      return(output_scen)
      
    }
    
    # SAVE OUTPUTS -------
    
    # prod_existing = rbindlist(list_existing_z)
    # prod_new = rbindlist(list_new_z)
    # 
    # output_list = list(prod_existing,
    #                    prod_new)
    
    ## res selection
    res = lapply(1:nrow(scen_sel), func_yearly_production)

    ## for diagnostic
    # res = lapply(2:2, func_yearly_production)
    
    output_list = do.call(Map, c(f = rbind, res))
    
    end_time = Sys.time()
    time_diff = difftime(end_time, start_time, units='mins')
    
    print(paste("Ended extraction model at ", end_time))
    
    print(paste("Model took ", round(time_diff[[1]]), " minutes to complete. Now saving results ..."))
    
    # save info file
      save_info_path = file.path(save_path, run_type)
      dir.create(save_info_path)
      print(paste0("Saving run information file to ", save_info_path))
      run_info = data.table(scenario_selection = scenario_selection,
                            start_time = start_time,
                            end_time = end_time,
                            duration = paste0(round(time_diff[[1]]), ' minutes'))
      fwrite(run_info, file.path(save_info_path, 'run_info.csv'), row.names = F)
    
    # save outputs to csv -----
    
    # create subdirectory of save_path, currently based on run_type ------
    
    save_processed_path = file.path(save_path, run_type)
    dir.create(save_processed_path, showWarnings = FALSE)
    
    # save vintage-level results ----
    
    vintage_fname = paste0(scenario_selection, '-vintage-level-results.csv')
    fwrite(output_list[[1]], file.path(save_processed_path, vintage_fname), row.names = F)
    print(paste0('Saved vintage-level results to ', vintage_fname))
    
    # save field-level results -----
    
    field_fname = paste0(scenario_selection, '-field-level-results.csv')
    fwrite(output_list[[2]], file.path(save_processed_path, field_fname), row.names = F)
    print(paste0('Saved field-level results to ', field_fname))
    
    # save state-level results ------
    
    state_fname = paste0(scenario_selection, '-state-level-results.csv')
    fwrite(output_list[[3]], file.path(save_processed_path, state_fname), row.names = F)
    print(paste0('Saved state-level results to ', state_fname))
    
    rm(solve_b, solve_mean_b, ghg_all)
    
    # save density results ------
    
    density_fname = paste0(scenario_selection, '-density-results.csv')
    fwrite(output_list[[4]], file.path(save_processed_path, density_fname), row.names = F)
    print(paste0('Density results to ', density_fname))
    
    # save exit results ------
    
    exit_fname = paste0(scenario_selection, '-exit-results.csv')
    fwrite(output_list[[5]], file.path(save_processed_path, exit_fname), row.names = F)
    print(paste0('Exit results to ', exit_fname))
    
    # save exit results ------
    
    exit_fname = paste0(scenario_selection, '-depletion-results.csv')
    fwrite(output_list[[6]], file.path(save_processed_path, exit_fname), row.names = F)
    print(paste0('Depletion results to ', exit_fname))
    
    
    rm(solve_b, solve_mean_b, ghg_all)
    
    return(output_list)
    
    # all_prod = rbind(prod_new, prod_existing_updated, use.names = T, fill = T)
    # all_prod[, batch := z]
      # prev-mmeng:
      # all_prod <- prod_new %>%
      #   rbind(prod_existing_updated) %>%
      #   mutate(batch = z) %>%
      #   as.data.table()
    
    
    # rm(pred_prod, pred_prod_wide, prod_existing_updated, prod_new, annual_outputs, pq_dt, all_prod)

}
