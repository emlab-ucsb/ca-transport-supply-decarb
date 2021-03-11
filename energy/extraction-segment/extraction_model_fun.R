
run_extraction_model <- function(oil_px_selection) {
  
  # set start time -----
    start_time <- Sys.time()
    print(paste("Starting extraction model at ", start_time))
  
  # inputs -----
    model_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
    scen_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
    entry_file      = 'entry_df_final.csv'
    coef_file       = 'poisson_regression_coefficients_10132020_v3.csv'
    param_file      = 'forecasted_decline_parameters_2020_2050.csv'
    peak_file       = 'field-vintage_peak-production_yearly.csv'
    prod_file       = 'predicted-production_2020-2045_field_test.csv'
    prod_vintage_file = 'predicted-production_2020-2045_field_vintage_test.csv'
    # hist_file       = 'new_wells_pred_weighted_R.csv'  ## update this
    histprod_file   = 'crude_prod_x_field.csv'
  
  # source from other scripts -----
    
    # source function to rank costs
      source(here::here('scripts', 'model', 'entry', 'prod_quota.R'))
  
    # source ccs emissions mean b calculation script
      source(here::here('scripts', 'model', 'ghg-emissions', 'ccs_parameterization.R'))
    
    # source function to create matrix of scenarios and forecasted variables
      source(here::here('scripts', 'model', 'entry', 'input_scenarios_fun.R'))
    
  # load data -----
    
    # load entry data
    entry_dt = fread(file.path(model_path, 'stocks-flows', entry_file), header = T)
    
    # load matrix of scenarios and forecasted variables
    # source(here::here('scripts', 'model', 'entry', 'input_scenarios_fun.R'))
    scenarios_dt = load_scenarios_dt(oil_px_selection)
    
    # load coefficients from poisson regression of historic data
    coefs_dt = fread(file.path(model_path, 'entry-model-results', coef_file), header = T)
    coefs_dt = unique(coefs_dt)
    
    # load decline parameters  
    decline_dt = fread(file.path(model_path, 'decline-historic', 'parameters', param_file), header = T)
    
    # load peak production for each field
    peak_dt = fread(file.path(model_path, 'decline-historic', 'data', peak_file), header = T)
    
    # load forecasted production from existing (pre 2020) wells
    prod_existing = fread(file.path(model_path, 'predict-production', 'production_with_exit', prod_file), header = T)
    #rl
    prod_existing_vintage = fread(file.path(model_path, 'predict-production', 'production_with_exit', prod_vintage_file), header = T)
    
    # load historic modeled well entry
    # hist_modeled = fread(file.path(model_path, 'entry-model-results', hist_file), header = T)
    
    # load historic production
    prod_hist = fread(file.path(model_path, 'stocks-flows', histprod_file), header = T)
  
  # rename FieldCode -> doc_field_code -----
  
    setnames(decline_dt, "FieldCode", "doc_field_code")
    setnames(peak_dt, "FieldCode", "doc_field_code")
    setnames(prod_hist, "FieldCode", "doc_field_code")
  
  # rename FieldName -> doc_fieldname -----
  
    setnames(decline_dt, "FieldName", "doc_fieldname")
    setnames(peak_dt, "FieldName", "doc_fieldname")
  
  # pad field codes with leading zeroes -----
  
    entry_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
    # scenarios_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
    coefs_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
    decline_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
    peak_dt[, doc_field_code := sprintf("%03d", doc_field_code)]
    prod_existing[, doc_field_code := sprintf("%03d", doc_field_code)]
    #rl
    prod_existing_vintage[, doc_field_code := sprintf("%03d", doc_field_code)]
    # hist_modeled[, doc_field_code := sprintf("%03d", doc_field_code)]
    prod_hist[, doc_field_code := sprintf("%03d", doc_field_code)]
  
  # get peak production median of last two vintages -----
  
    peak_prod_median = peak_dt[vintage %in% c("2008-2012", "2013-2019"), 
                               lapply(.SD, median, na.rm = TRUE), .SDcols = c("peak_tot_prod", "peak_avg_well_prod", "peak_well_prod_rate"),
                               by = .(doc_field_code)]
  
  # from entry data, keep: field, new wells, depletion ------
  
    setnames(entry_dt, 'n_new_wells', 'new_wells')
    setnames(entry_dt, 'm_cumsum_div_my_prod', 'depl')
    entry_dt_vars = entry_dt[, .(doc_field_code, doc_fieldname, year, brent, capex_imputed, opex_imputed, depl, new_wells)]
    entry_dt = entry_dt[, .(doc_field_code, doc_fieldname, year, new_wells, depl)]
  
  # ccs emissions scalar ---------
  
    ccs_ghg_scalar <- 1 - 0.875
  
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
      dt_info[, ccs_adoption := ccs_adj_usd_per_kg - carbon_price_usd_per_kg]
      dt_info[, m_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                             m_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                             m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
      dt_info[, wm_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                              wm_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                              wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
    
    ## track ccs adoption
      dt_info[, ccs_adopted := fifelse(ccs_adoption < 0,
                                      1, 0)] 

    ## adjust emissions intensity value again for ccs adoption
      dt_info[, ccs_scalar := ccs_ghg_scalar]
      
      dt_info[, upstream_kgCO2e_bbl_inno_ccs_adj := fifelse(ccs_adoption < 0,
                                                           upstream_kgCO2e_bbl_inno_adj * ccs_ghg_scalar,
                                                           upstream_kgCO2e_bbl_inno_adj)]
  
      # dt_info[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
      # dt_info[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
  
  # create data table of all variables needed ------
  
    # dt_info = scenarios_dt[coefs_dt, on = .(doc_field_code, doc_fieldname)]
  
  # vector of years -----
      
    pred_years = c(2020:2045)
  
  # functions ------ 
  
    hypfunc = function(b,t,q_i,D_h) { q_i/((1 + b*D_h*t)^(1/b)) }
    expfunc = function(q_i,d,t) {   q_i*exp(-d*t) }
  
  # get top ten and non-top ten fields ------
  
    top10_fields = coefs_dt[!is.na(cons_hat)]
    other_fields = coefs_dt[is.na(cons_hat)]

  # scenario combinations ----------
  
    scen_sel = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                       carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])
  
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
      # prod_existing_vintage_z = prod_existing_vintage[setback_scen, on = .(setback_scenario), nomatch = 0]
      
      list_pred_prod = list()
      # list_pred_prod_wide = list()
      list_prod_existing = list()
      list_prod_new = list()
      
      for (i in seq_along(pred_years)) {
        
        t = pred_years[i]
        # print(t)
        
        # if(t == 2045){browser()}
        
        # set up variables for top 10 fields
        temp_top10 = dt_info_z[year == t & doc_field_code %in% top10_fields[, doc_field_code]]
        temp_top10 = temp_top10[dt_depl_z[year == t], on = .(doc_field_code, 
                                                             oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                             setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                             year), nomatch = 0]
        temp_top10 = temp_top10[coefs_dt, on = .(doc_field_code, doc_fieldname), nomatch = 0]
        
        # poisson regression of top 10 fields
        temp_top10[, m_new_wells_pred := fifelse(depl < 0.9999, 
                                                exp(brent_hat*oil_price_usd_per_bbl + capex_hat*m_capex_imputed + opex_hat*m_opex_imputed_adj + depl_hat*depl + cons_hat),
                                                0)]
        temp_top10[, wm_new_wells_pred := fifelse(depl < 0.9999, 
                                                 exp(brent_hat*oil_price_usd_per_bbl + capex_hat*wm_capex_imputed + opex_hat*wm_opex_imputed_adj + depl_hat*depl + cons_hat),
                                                 0)]
        
        # set up variables for all other fields
        temp_other = dt_info_z[year == t & doc_field_code %in% other_fields[, doc_field_code]]
        temp_other = temp_other[unique(dt_depl_z[year == t]), on = .(doc_field_code, 
                                                                     oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                                     setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                                     year), nomatch = 0]
        temp_other = temp_other[coefs_dt, on = .(doc_field_code, doc_fieldname), nomatch = 0]
        
        # fixed effects poisson regression of all other fields 
        temp_other[, m_yvar_poisson := exp(brent_hat*oil_price_usd_per_bbl + capex_hat*m_capex_imputed + opex_hat*m_opex_imputed_adj + depl_hat*depl)]
        temp_other[, wm_yvar_poisson := exp(brent_hat*oil_price_usd_per_bbl + capex_hat*wm_capex_imputed + opex_hat*wm_opex_imputed_adj + depl_hat*depl)]
        temp_other[, m_new_wells_pred := ifelse(depl < 0.9999, 
                                                yvar_exp_alpha*m_yvar_poisson,
                                                0)]
        temp_other[, wm_new_wells_pred := ifelse(depl < 0.9999, 
                                                 yvar_exp_alpha*wm_yvar_poisson,
                                                 0)]
        
        temp_other[, m_yvar_poisson := NULL]
        temp_other[, wm_yvar_poisson := NULL]
        
        new_wells = rbindlist(list(temp_top10, temp_other), use.names = T)
        setorder(new_wells, 'doc_field_code')
        
        # round number of wells to integer
        # new_wells[, m_new_wells_pred_round := round(m_new_wells_pred, 0)]
        # new_wells[, wm_new_wells_pred_round := round(wm_new_wells_pred, 0)]
        
        ## store the new wells and associated information for year t
        # list_pred_wells[[i]] = new_wells
        
        # calculate prediction of new wells into 2045
        
        new_wells_prod = merge(new_wells,
                               decline_dt[t == year, .(doc_field_code, q_i, D, b1, b2, d, int_year)],
                               by = 'doc_field_code',
                               all.x = T)
        param_other = unique(decline_dt[t == year & doc_fieldname == 'other', .(q_i, D, b1, b2, d, int_year)])
        new_wells_prod[is.na(q_i), q_i := param_other[, q_i]]
        new_wells_prod[is.na(D), D := param_other[, D]]
        new_wells_prod[is.na(b2) & is.na(b1), b2 := param_other[, b2]]
        new_wells_prod[is.na(d) & is.na(b1), d := param_other[, d]]
        new_wells_prod[is.na(int_year) & is.na(b1), int_year := param_other[, int_year]]
        
        new_wells_prod = new_wells_prod[peak_prod_median[, .(doc_field_code, peak_avg_well_prod)], on = 'doc_field_code', nomatch = 0]
        new_wells_prod[, peak_production := m_new_wells_pred * peak_avg_well_prod]
        
        ## adjust production for setback scenario
        new_wells_prod[, peak_production := peak_production * (1 - area_coverage)]
        # meas-previously : 
        # new_wells_prod$peak_production = new_wells_prod$peak_production * (1 - new_wells_prod$area_coverage)
        
        ## rl
        ## implement production quota before calculating prod from new wells
        # rank existing and new wells in each field by costs
        # dt_info_rank = rank_costs(dt_info,t)
        # create dataframe with field code, existing/new flag, rank, predicted production
        # by merging dt_info_rank with prod_existing_vintage and new_well_prod
        
        ## calculate field level costs
        dtt = unique(dt_info_z[year == t,
                               .(doc_field_code, doc_fieldname, m_capex_imputed, m_opex_imputed_adj,
                                 oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                 setback_scenario, prod_quota_scenario, excise_tax_scenario)])
        # dtt = dt_info[year == t]
        dtt[, cost_existing := m_opex_imputed_adj]
        dtt[, cost_new := m_capex_imputed + m_opex_imputed_adj]
        dtt_long = melt(dtt, measure.vars = c('cost_new', 'cost_existing'), variable.name = 'cost_type', value.name = 'cost')
        dtt_long[, cost_rank := rank(unclass(cost)), by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                            setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        # dtt_long[, cost_rank := frank(cost), by = list(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
        #                                                setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        dt_info_rank = dcast(dtt_long, doc_field_code + doc_fieldname + m_capex_imputed + m_opex_imputed_adj + oil_price_scenario + innovation_scenario +  carbon_price_scenario + ccs_scenario +  setback_scenario + prod_quota_scenario + excise_tax_scenario ~ cost_type, 
                             value.var = c('cost', 'cost_rank'))
        
        setnames(dt_info_rank, 'cost_rank_cost_new', 'cost_new_rank')
        setnames(dt_info_rank, 'cost_rank_cost_existing', 'cost_existing_rank')
        
        
        temp_dt_info_rank = dt_info_rank[,c("doc_field_code","doc_fieldname",
                                            "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                            "setback_scenario", "prod_quota_scenario", "excise_tax_scenario",
                                            "cost_existing_rank","cost_new_rank")]
        
        # set up copy of dataframe for new wells 
        temp_new_wells_prod = new_wells_prod[year==t & m_new_wells_pred > 0 & peak_production > 0] # only include fields that are predicted to have entry and production
        temp_new_wells_prod = temp_new_wells_prod[,c("doc_field_code", "doc_fieldname", 
                                                     "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
                                                     "setback_scenario", "prod_quota_scenario", "excise_tax_scenario", 
                                                     "innovation_multiplier", "quota", "ccs_adopted", "ccs_scalar",  "upstream_kgCO2e_bbl", "upstream_kgCO2e_bbl_inno_adj", 
                                                     "upstream_kgCO2e_bbl_inno_ccs_adj", "m_new_wells_pred","peak_production")]
        setnames(temp_new_wells_prod, "peak_production", "production_bbl")
        setnames(temp_new_wells_prod, "m_new_wells_pred", "num_wells")
        temp_new_wells_prod[, vintage := 'new']
        temp_new_wells_prod[, vintage_start := t]
        
        # prev:
        # temp_new_wells_prod$vintage = "new" # call new wells' vintage "new"
        # temp_new_wells_prod$vintage_start = t # vintage start is t
        
        # set up copy of dataframe for existing wells 
        ## ---------------------------------------------------------
        
        ## filter for year since actual quota number, ccs adoption, and ghg emissions intensity change year to year
        # scen_combos = unique(dt_info[year == t, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, 
        #                                           ccs_scenario, prod_quota_scenario, excise_tax_scenario, setback_scenario, quota, ccs_adopted, upstream_kgCO2e_bbl_adj)])
        # 
        temp_prod_existing_vintage = dt_info_z[year == t, . (doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                                             ccs_scenario, prod_quota_scenario, excise_tax_scenario, setback_scenario, quota, innovation_multiplier,
                                                             ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj)]
        
        existing_vintage_prod_t = prod_existing_vintage_z[year == t]
        existing_vintage_prod_t = subset(existing_vintage_prod_t, select = -c(year))
        
        temp_prod_existing_vintage = merge(temp_prod_existing_vintage, existing_vintage_prod_t,
                                           by = c("doc_field_code", "setback_scenario"),
                                           all.x = T,
                                           allow.cartesian = T)
        temp_prod_existing_vintage[, num_wells := 1]
        # mmeng-prev:
        # temp_prod_existing_vintage <- left_join(temp_prod_existing_vintage, existing_vintage_prod_t) %>%
        #   mutate(num_wells = 1) %>%
        #   as.data.table()
        
        
        # temp_prod_existing_vintage = temp_prod_existing_vintage[scen_combos, on = .(setback_scenario), allow.cartesian = T]
        # temp_prod_existing_vintage = crossing(temp_prod_existing_vintage, scen_combos)
        # temp_prod_existing_vintage[, num_wells := 1]
        # temp_prod_existing_vintage$num_wells = 1
        # temp_prod_existing_vintage <- setDT(temp_prod_existing_vintage)
        
        # ## filter for relevant scenario combinations when running a subset
        # temp_prod_existing_vintage = temp_prod_existing_vintage[un_scens[ran_un], on = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
        #                                                      setback_scenario, prod_quota_scenario, excise_tax_scenario), nomatch = 0]
        
        # temp_prod_existing_vintage[, year := NULL]
        # scen_combos[, k := 1]
        # temp_prod_existing_vintage[, k := 1]
        # temp_prod_existing_vintage = temp_prod_existing_vintage[scen_combos, on = 'k', allow.cartesian = T]
        # temp_prod_existing_vintage[, k := NULL]
        # temp_prod_existing_vintage[, num_wells := 1]
        # scen_combos[, k := NULL]
        
        ## if t > 1, refer to pred_prod_existing in t - 1 to find out which field vintages are still producing
        if(i > 1) {
          
          prev_existing_prod = copy(list_prod_existing[[i-1]])
          prev_existing_prod = prev_existing_prod[, .(doc_field_code, doc_fieldname, vintage, vintage_start, 
                                                      oil_price_scenario, innovation_scenario,
                                                      carbon_price_scenario, ccs_scenario, setback_scenario, 
                                                      prod_quota_scenario, excise_tax_scenario, production_bbl)]
          prev_existing_prod = unique(prev_existing_prod)
          setnames(prev_existing_prod, 'production_bbl', 'prev_prod_bbl')
          # prev-mmeng:
          # prev_existing_prod <- list_prod_existing[[i-1]] %>%
          #   select(doc_field_code:excise_tax_scenario, prev_prod_bbl = production_bbl)
          
          temp_prod_existing_vintage = merge(temp_prod_existing_vintage, prev_existing_prod,
                                             by = c("doc_field_code", "setback_scenario", 
                                                    "oil_price_scenario", "innovation_scenario", 
                                                    "carbon_price_scenario", "ccs_scenario", 
                                                    "prod_quota_scenario", "excise_tax_scenario", 
                                                    "doc_fieldname", "vintage"),
                                             all.x = T)
          temp_prod_existing_vintage[, production_bbl_adj := fifelse(prev_prod_bbl == 0 & production_bbl > 0, 0, production_bbl)]
          temp_prod_existing_vintage[, c('production_bbl', 'prev_prod_bbl') := NULL]
          setnames(temp_prod_existing_vintage, 'production_bbl_adj', 'production_bbl')
          temp_prod_existing_vintage = temp_prod_existing_vintage[, .(doc_field_code, doc_fieldname, oil_price_scenario, 
                                                                      innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                                      setback_scenario, prod_quota_scenario, excise_tax_scenario, quota, innovation_multiplier, 
                                                                      ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl, 
                                                                      upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj, 
                                                                      num_wells, production_bbl,
                                                                      vintage, vintage_start)]
          
          # mmeng-prev:
          # temp_prod_existing_vintage_adj <- temp_prod_existing_vintage %>%
          #   left_join(prev_existing_prod) %>%
          #   select(doc_field_code:vintage, vintage_start, num_wells, production_bbl, prev_prod_bbl) %>%
          #   mutate(production_bbl_adj = fifelse(prev_prod_bbl == 0, 0, production_bbl)) %>%
          #   select(-production_bbl, -prev_prod_bbl) %>%
          #   rename(production_bbl = production_bbl_adj) %>%
          #   select(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
          #          setback_scenario, prod_quota_scenario, excise_tax_scenario, quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl, 
          #          upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj, num_wells, production_bbl,
          #          vintage, vintage_start) 
          # temp_prod_existing_vintage <- setDT(temp_prod_existing_vintage_adj)
          
        }
        
        temp_prod_existing_vintage[, vintage_start := fifelse(vintage == 'pre 1978', 1977, as.numeric(substr(vintage, 1, 4)))]
        # mmeng-prev:
        # temp_prod_existing_vintage <- temp_prod_existing_vintage %>%
        #   mutate(vintage_start = substr(vintage, 1, 4),
        #          vintage_start = as.numeric(fifelse(vintage_start == "pre ", 1977, vintage_start))) %>%
        #   as.data.table()
        
        # set up copy of dataframe for vintages from previous interations
        ## -------------------------------------------------------------------------
        
        temp_prod_new_vintage = rbindlist(list_pred_prod)

        # temp_prod_new_vintage = unique(temp_prod_new_vintage)
        
        if(i > 1) {
          
          temp_prod_new_vintage_info = dt_info_z[year == t, . (doc_field_code, oil_price_scenario, 
                                                               innovation_scenario, carbon_price_scenario, 
                                                               ccs_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                               setback_scenario, quota, innovation_multiplier, ccs_adopted, ccs_scalar,
                                                               upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj)]
          
          temp_prod_new_vintage = temp_prod_new_vintage[year == t]
          temp_prod_new_vintage[, ':=' (vintage = "new",
                                        vintage_start = entry_year)]
          temp_prod_new_vintage = temp_prod_new_vintage[, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
                                                            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
                                                            excise_tax_scenario, num_wells = m_new_wells_pred, production_bbl, vintage, vintage_start)]
          temp_prod_new_vintage = merge(temp_prod_new_vintage, temp_prod_new_vintage_info,
                                        by = c("doc_field_code", "oil_price_scenario", 
                                               "innovation_scenario", "carbon_price_scenario", 
                                               "ccs_scenario", "setback_scenario", 
                                               "prod_quota_scenario", "excise_tax_scenario"),
                                        all.x = T)
          # temp_prod_new_vintage = unique(temp_prod_new_vintage)
          # mmeng-prev:
          # temp_prod_new_vintage <- temp_prod_new_vintage %>%
          #   filter(year == t) %>%
          #   mutate(vintage = "new",
          #          vintage_start = entry_year) %>%
          #   select(doc_field_code:excise_tax_scenario, num_wells = m_new_wells_pred, production_bbl, vintage, vintage_start) %>%
          #   left_join(temp_prod_new_vintage_info) %>%
          #   as.data.table()
          
          ## refer to t - 1 to determine which recent field-vintages are still producing
          
          prev_new_prod = copy(list_prod_new[[i - 1]])[, .(doc_field_code, doc_fieldname, oil_price_scenario, 
                                                           innovation_scenario, carbon_price_scenario,
                                                           ccs_scenario, setback_scenario, prod_quota_scenario, 
                                                           excise_tax_scenario, vintage, vintage_start,
                                                           num_wells = n_wells, prev_prod_bbl = production_bbl)]
          prev_new_prod = unique(prev_new_prod)
          # mmeng-prev:
          # prev_new_prod <- list_prod_new[[i - 1]] %>%
          #   select(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
          #          ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, vintage, vintage_start,
          #          num_wells = n_wells, prev_prod_bbl = production_bbl)
          
          temp_prod_new_vintage = merge(temp_prod_new_vintage, prev_new_prod,
                                        by = c("doc_field_code", "oil_price_scenario", 
                                               "innovation_scenario", "carbon_price_scenario", 
                                               "ccs_scenario", "setback_scenario", "prod_quota_scenario", 
                                               "excise_tax_scenario", "doc_fieldname", 
                                               "num_wells", "vintage", "vintage_start"),
                                        all.x = T)
          temp_prod_new_vintage[, ':=' (production_bbl_adj = fifelse(prev_prod_bbl == 0, 0, production_bbl),
                                        num_wells = fifelse(prev_prod_bbl == 0, 0, num_wells))]
          temp_prod_new_vintage[, production_bbl_adj := fifelse(prev_prod_bbl == 0 & production_bbl > 0, 0, production_bbl)]
          temp_prod_new_vintage[, c('production_bbl', 'prev_prod_bbl') := NULL]
          setnames(temp_prod_new_vintage, 'production_bbl_adj', 'production_bbl')
          # temp_prod_new_vintage = unique(temp_prod_new_vintage)
          
          # mmeng-prev:
          # temp_prod_new_vintage_adj <- temp_prod_new_vintage %>%
          #   left_join(prev_new_prod) %>%
          #   mutate(production_bbl_adj = fifelse(prev_prod_bbl == 0, 0, production_bbl),
          #          num_wells = fifelse(prev_prod_bbl == 0, 0, num_wells)) %>%
          #   select(-production_bbl, -prev_prod_bbl) %>%
          #   rename(production_bbl = production_bbl_adj)
          # temp_prod_new_vintage <- setDT(temp_prod_new_vintage_adj)
          
          # one data table with production data of both new and existing wells
          temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage, temp_prod_new_vintage, use.names = T)
          
        } else {
          
          temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage, use.names = T)
          
        }
        
        # # join production dataframe to cost rankings
        # temp_prod_quota = temp_dt_info_rank %>% 
        #   full_join(temp_prod_quota, by = c("doc_field_code","doc_fieldname","oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario", "prod_quota_scenario"))
        # # note: fields without entry show NA for num_wells, production_bbl, vintage because these fields are not in temp_dt_info_rank
        # temp_prod_quota[is.na(num_wells), num_wells := 0]
        # temp_prod_quota[is.na(production_bbl), production_bbl := 0]
        # temp_prod_quota[is.na(vintage), vintage := "new"]
        
        # join production dataframe to cost rankings
        temp_prod_quota = merge(temp_prod_quota, temp_dt_info_rank, 
                                by = c("doc_field_code", "doc_fieldname", "oil_price_scenario", "innovation_scenario", 
                                       "carbon_price_scenario", "ccs_scenario", "setback_scenario", "prod_quota_scenario", 
                                       "excise_tax_scenario"),
                                all.x = T)
        # mmeng-prev:
        # temp_prod_quota = temp_prod_quota %>%
        #   left_join(temp_dt_info_rank)
        
        # put ranks in one column
        temp_prod_quota[, cost_rank := fifelse(vintage_start == t, cost_new_rank, cost_existing_rank)]
        temp_prod_quota[, c('cost_existing_rank', 'cost_new_rank') := NULL]
        # mmeng-prev:
        # temp_prod_quota$cost_rank = fifelse(temp_prod_quota$vintage_start == t, temp_prod_quota$cost_new_rank, temp_prod_quota$cost_existing_rank)
        # temp_prod_quota = subset(temp_prod_quota, select = -c(cost_existing_rank, cost_new_rank))
        
        # # column with integers representing vintages, for secondary ranking by vintage later
        ## NOTE: this happens earlier now
        
        # temp_prod_quota[, vintage_start := as.numeric(substr(vintage,1,4)) ]
        # temp_prod_quota$vintage_start = fifelse(temp_prod_quota$vintage == "pre 1978", 1977, temp_prod_quota$vintage_start) 
        # temp_prod_quota$vintage_start = fifelse(temp_prod_quota$vintage == "new", t, temp_prod_quota$vintage_start) 
        
        # calculate cumulative production in ascending order of rank then descending order of vintage 
        # (i.e. assume that within a field, older vintages get shut first)
        setkey(temp_prod_quota, doc_field_code,
               oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario)
        setorder(temp_prod_quota, -vintage_start)
        temp_prod_quota[, cost_vintage_rank := rank(unclass(-vintage_start)), by = .(doc_field_code,
                                                                                     oil_price_scenario, innovation_scenario, 
                                                                                     carbon_price_scenario, ccs_scenario,
                                                                                     setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        # mmeng-prev:
        # temp_prod_quota2 = temp_prod_quota %>%
        #   group_by(doc_field_code,
        #            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
        #            setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   arrange(-vintage_start) %>%
        #   mutate(cost_vintage_rank = 1:n()) %>%
        #   ungroup()
        
        ## calculate cumulative sum by scenario, arranging by cost rank and cost vintage rank
        temp_prod_quota = temp_prod_quota[!is.na(cost_rank)]
        setorder(temp_prod_quota, cost_rank, cost_vintage_rank)
        temp_prod_quota[is.na(production_bbl), production_bbl := 0]
        temp_prod_quota[, prod_cumsum := cumsum(production_bbl), by = .(oil_price_scenario, innovation_scenario, 
                                                                        carbon_price_scenario, ccs_scenario, 
                                                                        setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        # mmeng-prev:
        # temp_prod_quota3 = temp_prod_quota2 %>%
        #   ## remove fields/field vintages with NA costs for now -- those are all gas fields right now
        #   filter(!is.na(cost_rank)) %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   arrange(cost_rank, cost_vintage_rank) %>%
        #   mutate(prod_cumsum = cumsum(production_bbl)) %>%
        #   ungroup()
        
        ## new column for how much is produced under prod quota
        ## set all field-vintages that are greater than prod quota to zero, i.e. not allowed to produce
        temp_prod_quota[, prod_limited := fifelse(prod_cumsum > quota, 0, prod_cumsum)]
        # mmeng-prev:
        # temp_prod_quota4 <- temp_prod_quota3 %>%
        #   mutate(prod_limited = fifelse(prod_cumsum > quota, 0, prod_cumsum)) 
        
        
        ## previous code
        # temp_prod_quota3$prod_limited = fifelse(temp_prod_quota$prod_cumsum > temp_prod_quota$quota, 0, temp_prod_quota$prod_cumsum) 
        # temp_prod_quota3$num_wells = fifelse(temp_prod_quota$prod_limited == 0 & temp_prod_quota$vintage == "new", 0, temp_prod_quota$num_wells) 
        
        
        # find field-vintage that prod quota lands in and refine  
        # if vintage is not new, do nothing (assume that an entire field-vintage shuts down or not, since we don't want to keep track of well-level prod for existing wells)
        # if vintage is new, calculate no. of new wells that can enter
        
        ## calculating remaining production if relevant
        temp_prod_quota[, over_quota_ranks := fifelse(prod_cumsum > quota, 1, 0)]
        temp_prod_quota[, ':=' (last_prod = lag(prod_cumsum),
                                sum_over = cumsum(over_quota_ranks)), by = .(oil_price_scenario, innovation_scenario, 
                                                                             carbon_price_scenario, ccs_scenario, 
                                                                             setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        temp_prod_quota[, prod_quota_remainder := quota - last_prod]
        temp_prod_quota[, adj_prod_limited := fifelse(sum_over == 1 & vintage == "new", prod_quota_remainder,
                                                      fifelse(sum_over > 1 & vintage == "new", 0,
                                                              fifelse(sum_over > 0 & vintage != "new", 0, production_bbl)))]
        temp_prod_quota[, adj_new_wells := fifelse(sum_over == 1 & vintage == "new", adj_prod_limited / (production_bbl / num_wells), 
                                                   fifelse(sum_over > 1 & vintage == "new", 0, 
                                                           fifelse(sum_over > 0 & vintage != "new", 0, num_wells)))]
        # mmeng-prev:
        # temp_prod_quota5 <- temp_prod_quota4 %>%
        #   mutate(over_quota_ranks = fifelse(prod_cumsum > quota, 1, 0)) %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   ## find previous productoin, calc cumulative sum
        #   mutate(last_prod = lag(prod_cumsum),
        #          sum_over = cumsum(over_quota_ranks)) %>%
        #   ungroup() %>%
        #   mutate(prod_quota_remainder = quota - last_prod,
        #          adj_prod_limited = fifelse(sum_over == 1 & vintage == "new", prod_quota_remainder,
        #                                    fifelse(sum_over > 1 & vintage == "new", 0,
        #                                           fifelse(sum_over > 0 & vintage != "new", 0, production_bbl))),
        #          adj_new_wells = fifelse(sum_over == 1 & vintage == "new", adj_prod_limited / (production_bbl / num_wells), 
        #                                        fifelse(sum_over > 1 & vintage == "new", 0, 
        #                                               fifelse(sum_over > 0 & vintage != "new", 0, num_wells))))
        
        
        
        
        ## previous code
        ## Get the field-vintage quota lands in (call it quota_bin)
        ## temp_prod_quota$over_quota_ranks = fifelse(temp_prod_quota$prod_cumsum > temp_prod_quota$quota, temp_prod_quota$cost_vintage_rank, 9999) 
        
        # temp_prod_quota6 = temp_prod_quota5 %>% 
        #   rowwise() %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(quota_bin = min(over_quota_ranks, na.rm = TRUE)) %>% 
        #   ungroup()
        
        # Get cum prod of field-vintage before quota's bin
        # temp_prod_quota$prod_quota_bin_prev = fifelse(temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin-1, temp_prod_quota$prod_cumsum, -1)
        # # Calculate production allowed for quota bin
        # temp_prod_quota$prod_quota_remainder = fifelse(temp_prod_quota$prod_quota_bin_prev!=-1, temp_prod_quota$quota-temp_prod_quota$prod_cumsum, 0)
        # temp_prod_quota = temp_prod_quota %>%
        #   rowwise() %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(prod_quota_remainder = max(prod_quota_remainder)) %>% 
        #   ungroup()
        
        ## update production and no. new wells entered for quota bin if vintage is new
        ## can discuss if want to let existing field-vintage continue producing partially
        # temp_prod_quota$prod_limited = fifelse((temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin & temp_prod_quota$vintage=="new"), temp_prod_quota$prod_quota_remainder, temp_prod_quota$prod_limited) 
        # temp_prod_quota$num_wells = fifelse((temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin & temp_prod_quota$vintage=="new"), temp_prod_quota$prod_limited/(temp_prod_quota$production_bbl/temp_prod_quota$num_wells), temp_prod_quota$num_wells) 
        
        ## remove some columns, add 
        temp_prod_quota[, c('production_bbl', 'num_wells', 'prod_limited', 'over_quota_ranks', 
                            'sum_over', 'last_prod', 'prod_quota_remainder', 'prod_cumsum') := NULL]
        # mmeng-prev:
        # temp_prod_quota6 <- temp_prod_quota5 %>%
        #   select(-production_bbl, -num_wells, -prod_limited, -over_quota_ranks, -sum_over, -last_prod, -prod_quota_remainder, -prod_cumsum) 
        
        # update new_wells_prod (wells that start in year t)
        temp_prod_quota_new_wells = temp_prod_quota[vintage == "new"]
        temp_prod_quota_new_wells[, ':=' (year = t,
                                          upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
                                          upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
                                          upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited)]
        temp_prod_quota_new_wells = temp_prod_quota_new_wells[, .(year, doc_field_code, doc_fieldname, vintage, 
                                                                  vintage_start, oil_price_scenario, innovation_scenario, 
                                                                  carbon_price_scenario, ccs_scenario, setback_scenario, 
                                                                  prod_quota_scenario, excise_tax_scenario, 
                                                                  quota, innovation_multiplier, ccs_adopted, 
                                                                  ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
                                                                  n_wells = adj_new_wells, production_bbl = adj_prod_limited, 
                                                                  upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
        # mmeng-prev:
        # temp_prod_quota_new_wells <- temp_prod_quota %>%
        #   filter(vintage == "new") %>%
        #   mutate(year = t,
        #          upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
        #          upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
        #          upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited) %>%
        #   select(year, doc_field_code, doc_fieldname, vintage, vintage_start, oil_price_scenario, innovation_scenario, 
        #          carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
        #          quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
        #          n_wells = adj_new_wells, production_bbl = adj_prod_limited, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj) %>%
        #   as.data.table()
        
        # ## test
        # t <- temp_prod_quota6 %>%
        #   group_by(oil_price_scenario, innovation_scenario, 
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(upstream_kgCO2e_adj  = upstream_kgCO2e_bbl_adj * adj_prod_limited,
        #          upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited) %>%
        #   summarise(bbl = sum(adj_prod_limited, na.rm = T),
        #             kgco2e_adj = sum(upstream_kgCO2e_adj),
        #             kgco2e = sum(upstream_kgCO2e)) %>%
        #   ungroup()
        
        
        # temp_prod_quota_new_wells2 <- temp_prod_quota_new_wells %>%
        #   ## keep number of new wells in
        #   # select(-wm_new_wells_pred) %>%
        #   rename(production_bbl = peak_production) %>%
        #   ## filter for productive field vintage
        #   filter(production_bbl > 0) %>%
        #   mutate(year = t) %>%
        #   as.data.table()
        
        # ## create an object with production from POST 2019 VINTAGES BUT NOT CURRENT VINTAGE T
        # temp_prod_recent_wells <- temp_prod_quota6 %>%
        #   filter(vintage == "new" & vintage_start != t) %>%
        #   select(doc_field_code, doc_fieldname, vintage, vintage_start, oil_price_scenario, innovation_scenario, 
        #          carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, production_bbl = adj_prod_limited) %>%
        #   # group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
        #   #          carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   # summarise(production_bbl = sum(adj_prod_limited)) %>%
        #   # ungroup() %>%
        #   mutate(year = t) %>%
        #   as.data.table()
        
        ## old code
        # setnames(temp_prod_quota_new_wells, "prod_limited", "peak_production")
        # setnames(temp_prod_quota_new_wells, "num_wells", "wm_new_wells_pred")
        
        
        ## old code
        # new_wells_prod_new = subset(new_wells_prod, select=-c(peak_production, wm_new_wells_pred))
        # new_wells_prod_new = new_wells_prod_new %>%
        #   left_join(temp_prod_quota_new_wells, by = c("doc_field_code","doc_fieldname","oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario", "prod_quota_scenario", "excise_tax_scenario"))
        # new_wells_prod_new[is.na(peak_production), peak_production := 0]
        # new_wells_prod_new[is.na(wm_new_wells_pred), wm_new_wells_pred := 0]
        
        # replace new_wells_prod dt with one that accounts for quota
        ## this will be used to predict future annual production for new wells
        # new_wells_prod = setDT(new_wells_prod_new)
        
        # list_pred_prod[[i]] = new_wells_prod
        
        # test_new <- new_wells_prod %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   summarise(total_prod = sum(peak_production)) %>%
        #   ungroup() %>%
        #   mutate(total_prod_m = total_prod / 1e6)
        
        
        ## old code
        # update existing prod
        # temp_prod_quota_existing_wells = subset(temp_prod_quota, vintage!="new", select=c(doc_field_code, doc_fieldname, setback_scenario, prod_quota_scenario, excise_tax_scenario, vintage, prod_limited))
        # temp_prod_quota_existing_wells = temp_prod_quota_existing_wells %>%
        #   rowwise() %>%
        #   group_by(doc_field_code, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(production_bbl = sum(prod_limited)) %>% 
        #   ungroup()
        
        ## store new well production (all vintages) for time t
        list_prod_new[[i]] = copy(temp_prod_quota_new_wells)
        
        ## create object with production for existing wells (quota incorporated)
        temp_prod_quota_existing_wells = temp_prod_quota[vintage != "new"]
        temp_prod_quota_existing_wells[, ':=' (year = t,
                                               upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
                                               upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
                                               upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited)]
        temp_prod_quota_existing_wells = temp_prod_quota_existing_wells[, .(year, doc_field_code, doc_fieldname, vintage, 
                                                                            vintage_start, oil_price_scenario, innovation_scenario, 
                                                                            carbon_price_scenario, ccs_scenario, setback_scenario, 
                                                                            prod_quota_scenario, excise_tax_scenario, 
                                                                            quota, innovation_multiplier, ccs_adopted, 
                                                                            ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
                                                                            n_wells = adj_new_wells, production_bbl = adj_prod_limited, 
                                                                            upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
        # mmeng-prev:
        # temp_prod_quota_existing_wells <- temp_prod_quota %>%
        #   filter(vintage != "new") %>%
        #   mutate(year = t,
        #          upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
        #          upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
        #          upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited) %>%
        #   select(year, doc_field_code, doc_fieldname, vintage, vintage_start, oil_price_scenario, innovation_scenario, 
        #          carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
        #          quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
        #          n_wells = adj_new_wells, production_bbl = adj_prod_limited, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj) %>%
        #   as.data.table()
        
        
        ### test - sum production by scenario
        # test <- temp_prod_quota6 %>%
        #   mutate(type = fifelse(vintage == "new", "new", "existing")) %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, type) %>%
        #   summarise(total_prod = sum(adj_prod_limited)) %>%
        #   ungroup() %>%
        #   mutate(total_prod_m = total_prod / 1e6) %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(total_scen_prod = sum(total_prod_m)) %>%
        #   ungroup()
        # 
        # test2 <- temp_prod_quota_existing_wells %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   summarise(total_prod = sum(production_bbl)) %>%
        #   ungroup() %>%
        #   mutate(total_prod_m = total_prod / 1e6)
        
        
        ## old code
        # temp_prod_existing = unique(subset(temp_prod_quota_existing_wells, select = -c(prod_limited, vintage)))
        # temp_prod_existing$year = t
        
        # uncomment to implement
        # setDT(temp_prod_quota_existing_wells)
        ## note, this includes zero production field-vintages -- make sure they remain 0 in t + 1
        list_prod_existing[[i]] = temp_prod_quota_existing_wells
        
        # ## save recent vintages (2020-t)
        # list_pred_prod_recent[[i]]= temp_prod_recent_wells
        
        
        ## set up new wells prod df that will be used to find future production for new wells
        ## this will be used to predict future annual production for new wells
        
        ## vintage == t
        temp_prod_quota_new_wells[, vintage_start := t]
        temp_prod_quota_new_wells[, c('quota', 'ccs_adopted', 'innovation_multiplier', 'ccs_scalar',
                                      'upstream_kgCO2e_bbl_inno_adj', 'upstream_kgCO2e_bbl_inno_ccs_adj',
                                      'upstream_kgCO2e', 'upstream_kgCO2e_inno_adj', 'upstream_kgCO2e_inno_ccs_adj') := NULL]
        # temp_prod_quota_new_wells = unique(temp_prod_quota_new_wells)
        # mmeng-prev:
        # new_entry_prod <- temp_prod_quota_new_wells %>%
        #   filter(vintage_start == t) %>%
        #   select(-quota, -ccs_adopted, -innovation_multiplier, -ccs_scalar, - upstream_kgCO2e_bbl_inno_adj, -upstream_kgCO2e_bbl_inno_ccs_adj,
        #          -upstream_kgCO2e, -upstream_kgCO2e_inno_adj, -upstream_kgCO2e_inno_ccs_adj)
        
        new_wells_prod_new = new_wells_prod[year == t & m_new_wells_pred > 0 & peak_production > 0]
        new_wells_prod_new[, c('peak_production', 'm_new_wells_pred') := NULL]
        new_wells_prod_new = merge(new_wells_prod_new, temp_prod_quota_new_wells,
                                   by = c("doc_field_code", "year", 
                                          "doc_fieldname", "oil_price_scenario", 
                                          "innovation_scenario", "carbon_price_scenario", 
                                          "ccs_scenario", "setback_scenario", 
                                          "prod_quota_scenario", "excise_tax_scenario"),
                                   all.x = T)
        new_wells_prod_new = unique(new_wells_prod_new)
        new_wells_prod_new[, vintage := NULL]
        setnames(new_wells_prod_new, c('production_bbl', 'n_wells'), c('peak_production', 'm_new_wells_pred'))
        new_wells_prod_new = new_wells_prod_new[peak_production > 0]
        # mmeng-prev:
        # new_wells_prod_new <- 
        #   ## new wells prod filtered for fields that enter and peak prod > 0
        #   new_wells_prod[year == t & m_new_wells_pred > 0 & peak_production > 0] %>%
        #   select(-peak_production, -m_new_wells_pred) %>%
        #   left_join(new_entry_prod) %>%
        #   select(-vintage) %>%
        #   rename(peak_production = production_bbl) %>%
        #   ## filter out field vintages that are not extracted bc of quota
        #   filter(peak_production > 0) %>%
        #   rename(m_new_wells_pred = n_wells) %>%
        #   as.data.table()
        
        
        # prod_existing = temp_prod_existing
        # setDT(prod_existing)
        
        rm(dt_info_rank, temp_dt_info_rank, prev_existing_prod, existing_vintage_prod_t, 
           temp_new_wells_prod, temp_prod_new_vintage_adj, temp_prod_new_vintage, temp_prod_existing_vintage, temp_prod_existing_vintage_adj, 
           temp_prod_quota, temp_prod_quota_new_wells, temp_prod_quota_existing_wells)  
        ## end section that implements production quota
        
        # for 2020 up to current year, make prod = 0
        
        for (j in 2020:t) { 
          new_wells_prod_new[, col := 0]
          setnames(new_wells_prod_new, 'col', as.character(j))
        }
        
        # at entry year, make production = peak production
        
        new_wells_prod_new[, as.character(t) := peak_production]
        
        # for years following entrance, implement decline curves
        # rl revert to this code to remove setback adjustment
        for (j in (t+1):2045) {
          new_wells_prod_new[is.na(b2), col := hypfunc(b1, j - t, peak_production, D)]
          new_wells_prod_new[! is.na(b2), col :=  fifelse(j < t + int_year,
                                                         hypfunc(b2, j - t, peak_production, D),
                                                         expfunc(peak_production, d, j - t))  ]
          setnames(new_wells_prod_new, 'col', as.character(j))
        }
        
        # organize production
        
        setnames(new_wells_prod_new, 'year', 'entry_year')
        #rl
        # new_wells_prod_new = new_wells_prod_new[, c('entry_year', 'doc_field_code', 'doc_fieldname', 
        #                                             'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
        #                                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
        #                                             'oil_price_usd_per_bbl', 'm_new_wells_pred', 'wm_new_wells_pred', 
        #                                             as.character(2020:2045))]
        
        ## get rid of wm_new_wells_pred
        new_wells_prod_new = new_wells_prod_new[, c('entry_year', 'doc_field_code', 'doc_fieldname', 
                                                    'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                                    'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                                    'oil_price_usd_per_bbl', 'm_new_wells_pred', as.character(2020:2045))]
        # new_wells_prod_new = unique(new_wells_prod_new)
        
        # new_wells_prod_long = melt(new_wells_prod_new, id.vars = c('entry_year', 'doc_field_code', 'doc_fieldname', 
        #                                                            'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
        #                                                            'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
        #                                                            'oil_price_usd_per_bbl', 'm_new_wells_pred', 'wm_new_wells_pred'),
        #                            measure.vars = as.character(2020:2045), variable.name = 'year', value.name = 'production_bbl')
        
        ## get rid of wm_new_wells_pred
        new_wells_prod_long = melt(new_wells_prod_new, id.vars = c('entry_year', 'doc_field_code', 'doc_fieldname', 
                                                                   'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                                                   'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                                                   'oil_price_usd_per_bbl', 'm_new_wells_pred'),
                                   measure.vars = as.character(2020:2045), variable.name = 'year', value.name = 'production_bbl')
        # new_wells_prod_long = unique(new_wells_prod_long)
        
        ## test - sum production by scenario
        # test3 <- new_wells_prod_long %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
        #   summarise(total_prod = sum(production_bbl)) %>%
        #   ungroup() %>%
        #   mutate(total_prod_m = total_prod / 1e6,
        #          type = "new")
        
        
        # list_pred_prod_wide[[i]] = new_wells_prod_new
        # if (i > 1) {
        #   list_pred_prod[[i-1]] = list_pred_prod[[i-1]][as.numeric(as.character(year)) > t]
        # }
        
        list_pred_prod[[i]] = new_wells_prod_long[as.numeric(as.character(year)) > t, 
                                                  .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
                                                    carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario,
                                                    excise_tax_scenario, year, entry_year, m_new_wells_pred, production_bbl)]
        
        
        if(t < max(pred_years)) {
          
          # calculate depletion of next year
          
          ## prod_new = rbindlist(list_pred_prod)[year == t]
          prod_new = list_prod_new[[i]]
          prod_new = prod_new[, .(production_bbl = sum(production_bbl, na.rm = T)),
                              by = .(doc_field_code, doc_fieldname, 
                                     oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                     year)]
          setnames(prod_new, 'production_bbl', 'new_bbl')
          prod_new[, year := as.numeric(as.character(year))]
          
          ## prod from old wells
          prod_old = list_prod_existing[[i]]
          prod_old = prod_old[, .(old_bbl = sum(production_bbl, na.rm = T)),
                              by = .(doc_field_code, doc_fieldname, 
                                     oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                     year)]
          prod_old[, year := as.numeric(year)]
          
          # ## prod from recent wells
          # prod_recent = list_pred_prod_recent[[i]]
          # prod_recent = prod_recent[, .(recent_bbl = sum(production_bbl, na.rm = T)),
          #                     by = .(doc_field_code, doc_fieldname, 
          #                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
          #                            year)]
          # setnames(prod_recent, 'production_bbl', 'recent_bbl', skip_absent = T)
          
          prod_next_year = merge(prod_new, prod_old, 
                                 by = c("doc_field_code", "doc_fieldname", 
                                        "oil_price_scenario", "innovation_scenario", 
                                        "carbon_price_scenario", "ccs_scenario", 
                                        "setback_scenario", "prod_quota_scenario", 
                                        "excise_tax_scenario", "year"),
                                 all = T)
          prod_next_year[, ':=' (old_bbl = fifelse(is.na(old_bbl), 0, old_bbl),
                                 new_bbl = fifelse(is.na(new_bbl), 0, new_bbl))]
          prod_next_year[, production_bbl := old_bbl + new_bbl]
          prod_next_year = prod_next_year[, .(doc_field_code, 
                                              oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                              setback_scenario, prod_quota_scenario, excise_tax_scenario, year, production_bbl)]
          # mmeng-prev:
          # prod_next_year <- prod_new %>%
          #   full_join(prod_old) %>%
          #   mutate(old_bbl = fifelse(is.na(old_bbl), 0, old_bbl),
          #          new_bbl = fifelse(is.na(new_bbl), 0, new_bbl)) %>%
          #   rowwise() %>%
          #   mutate(production_bbl = old_bbl + new_bbl) %>%
          #   ungroup() %>% 
          #   select(doc_field_code, 
          #          oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
          #          setback_scenario, prod_quota_scenario, excise_tax_scenario, year, production_bbl) %>%
          #   as.data.table()
          
          
          
          # #rl 
          # prod_next_year = merge(prod_new[, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
          #                                     carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year, new_bbl)],
          #                        prod_old[, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
          #                                     carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year, old_bbl)],
          #                        by = c('doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 
          #                               'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'year'),
          #                        all = T)
          # 
          # prod_next_year[is.na(old_bbl), old_bbl := 0]
          # prod_next_year[, production_bbl := new_bbl + old_bbl]
          
          
          
          #rl
          
          # prod_next_year <- setDT(prod_next_year)
          
          # prod_next_year = prod_next_year[, .(production_bbl = sum(production_bbl, na.rm = T)),
          #                                 by = .(doc_field_code, doc_fieldname, 
          #                                        oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
          #                                        setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
          #rl
          depl_prev = dt_depl_z[year == t, .(doc_field_code, oil_price_scenario, innovation_scenario, 
                                             carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, depl)]
          setnames(depl_prev, 'depl', 'depl_prev')
          depl_prev = unique(depl_prev)
          #rl
          trr_prev = unique(scenarios_dt_z[year == t, .(doc_field_code, oil_price_scenario, 
                                                        innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                        setback_scenario, prod_quota_scenario, excise_tax_scenario, resource)])
          trr_prev = unique(trr_prev)
          
          #rl - match 
          prod_next_year = prod_next_year[depl_prev, on = .(doc_field_code, 
                                                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                            setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          prod_next_year = prod_next_year[trr_prev, on = .(doc_field_code, 
                                                           oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          
          ## fill in missing info
          prod_next_year[, ':=' (year = fifelse(is.na(year), t, year),
                                 production_bbl = fifelse(is.na(production_bbl), 0 , production_bbl))]
          # mmeng-prev:
          # prod_next_year <- prod_next_year %>%
          #   mutate(year = fifelse(is.na(year), t, year),
          #          production_bbl = fifelse(is.na(production_bbl), 0 , production_bbl)) %>%
          #   as.data.table()
          
          ## calculate depletion
          prod_next_year[, depl := depl_prev + (production_bbl / resource)]
          prod_next_year = unique(prod_next_year)
          
          #rl
          depl_next_year = prod_next_year[, .(doc_field_code, 
                                              oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                              setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                              year, depl)]
          depl_next_year[, year := t + 1]
          depl_next_year = unique(depl_next_year)
          
          dt_depl_z = rbindlist(list(dt_depl_z, depl_next_year))
          
          # calculate adjustments to input variables for next year
          
          info_next_year = unique(scenarios_dt_z[year == t+1])
          #rl
          info_next_year = merge(info_next_year,
                                 prod_next_year[, .(doc_field_code, 
                                                    oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, production_bbl)],
                                 by = c('doc_field_code',  
                                        'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
                                        'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'))
          setnames(info_next_year, 'production_bbl', 'total_bbls')
          
          ccs_prev = dt_info_z[year == t, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                            setback_scenario, prod_quota_scenario, excise_tax_scenario, ccs_adoption)]
          setnames(ccs_prev, 'ccs_adoption', 'adoption_prev')
          
          info_next_year = merge(info_next_year, ccs_prev, 
                                 by = c('doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                        'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'))
          
          # adjust ghg emissions factor by innovation scenario
          info_next_year[, upstream_kgCO2e_bbl_inno_adj := upstream_kgCO2e_bbl * innovation_multiplier]
          
          # adjust opex by innovation by innovation scenario
          info_next_year[, m_opex_imputed_adj := m_opex_imputed*innovation_multiplier]
          info_next_year[, wm_opex_imputed_adj := wm_opex_imputed*innovation_multiplier]
          
          # add excise tax to opex
          info_next_year[, m_opex_imputed_adj := m_opex_imputed_adj + tax]
          info_next_year[, wm_opex_imputed_adj := wm_opex_imputed_adj + tax]
          
          # calculate ccs
          info_next_year[, upstream_kgCO2e := upstream_kgCO2e_bbl_inno_adj * total_bbls]
          info_next_year[, upstream_mtCO2e := upstream_kgCO2e / 1e3]
          info_next_year[, mean_b := solve_mean_b(a, ccs_price_usd_per_kg*1e3, 'extraction'), 
                         by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          info_next_year[, total_cost := solve_tc(a, mean_b, upstream_mtCO2e)]
          # info_next_year[, b := solve_b(a, ccs_price_usd_per_kg * 1e3, upstream_mtCO2e)]
          # info_next_year[, mean_b := mean(b, na.rm = T), 
          #                by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)]
          # info_next_year[, total_cost := solve_tc(a, mean_b, upstream_mtCO2e)]
          info_next_year[, ccs_adj_usd_per_mt := total_cost / upstream_mtCO2e]
          info_next_year[, ccs_adj_usd_per_kg := total_cost / upstream_kgCO2e]
          info_next_year[is.na(ccs_adj_usd_per_kg), ccs_adj_usd_per_kg := ccs_price_usd_per_kg]
          info_next_year[, ccs_adoption := fifelse(adoption_prev > 0,
                                                  ccs_adj_usd_per_kg - carbon_price_usd_per_kg,
                                                  adoption_prev)]
          info_next_year[, m_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                                        m_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                                        m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
          info_next_year[, wm_opex_imputed_adj := fifelse(ccs_adoption < 0,
                                                         wm_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                                         wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
          # info_next_year[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
          # info_next_year[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
          info_next_year[, adoption_prev := NULL]
          
          ## track ccs adoption
          info_next_year[, ccs_adopted := fifelse(ccs_adoption < 0,
                                                 1, 0)] 
          
          info_next_year[, ccs_scalar := ccs_ghg_scalar]
          
          ## adjust emissions intensity value again for ccs adoption
          info_next_year[, upstream_kgCO2e_bbl_inno_ccs_adj := fifelse(ccs_adoption < 0,
                                                                      upstream_kgCO2e_bbl_inno_adj * ccs_ghg_scalar,
                                                                      upstream_kgCO2e_bbl_inno_adj)]
          
          
          info_next_year = unique(info_next_year)
          
          # # adjust ghg emissions factor by innovation scenario
          #   info_next_year[, upstream_kgCO2e_bbl := upstream_kgCO2e_bbl*innovation_multiplier]
          #   
          # # adjust opex by innovation by innovation scenario
          #   info_next_year[, m_opex_imputed := m_opex_imputed*innovation_multiplier]
          #   info_next_year[, wm_opex_imputed := wm_opex_imputed*innovation_multiplier]
          #   
          # # calculate ghg emissions
          #   info_next_year[, upstream_kgCO2e := upstream_kgCO2e_bbl*total_bbls]
          #   
          # # calculate a
          #   info_next_year[, a := -log(upstream_kgCO2e)/(lambertW(-log(upstream_kgCO2e)/(upstream_kgCO2e*ccs_price_usd_per_kg)) + log(upstream_kgCO2e))]
          #   
          # # calculate mean a
          #   info_next_year[, mean_a := mean(a, na.rm = T), 
          #                  by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario)]
          #   
          # # calculate total cost
          #   info_next_year[, total_cost := f_tc(mean_a, upstream_kgCO2e)]
          #   
          # # calculate average cost
          #   info_next_year[, ccs_avg_usd_per_kg := total_cost/upstream_kgCO2e]
          #   
          # # determine if firm adopts ccs
          #   info_next_year[, ccs_adoption := ccs_avg_usd_per_kg - carbon_price_usd_per_kg]
          #   
          # # adjust opex depending on if firm adopts ccs. if adopts ccs, use ccs price as modification. if not, use carbon price as modification
          #   info_next_year[, m_opex_imputed_adj := fifelse(ccs_adoption < 0,
          #                                          m_opex_imputed + (ccs_avg_usd_per_kg*upstream_kgCO2e_bbl),
          #                                          m_opex_imputed + (carbon_price_usd_per_kg*upstream_kgCO2e_bbl))]
          #   info_next_year[, wm_opex_imputed_adj := fifelse(ccs_adoption < 0,
          #                                           wm_opex_imputed + (ccs_avg_usd_per_kg*upstream_kgCO2e_bbl),
          #                                           wm_opex_imputed + (carbon_price_usd_per_kg*upstream_kgCO2e_bbl))]
          #   
          # # if adjusted opex are NA, fill with non-ccs/carbon adjusted opex
          #   info_next_year[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
          #   info_next_year[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
          #   info_next_year = unique(info_next_year)
          
          # combine input variables
          dt_info_z = rbindlist(list(dt_info_z, info_next_year), use.names = T)
          
          
        }  
        
        rm(t,j,temp_top10,temp_other,new_wells,param_other,new_wells_prod, new_wells_prod_new, new_wells_prod_long,
           prod_new,prod_old,prod_next_year,depl_prev,trr_prev,depl_next_year,info_next_year,dtt)
        
      }
      
      rm(dt_info_z, dt_depl_z)
      
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
                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                           doc_field_code, doc_fieldname, year, ccs_adopted, production_bbl, 
                                                           upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
      new_prod_dt = rbindlist(list_prod_new)[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                                 setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                 doc_field_code, doc_fieldname, year, vintage_start, ccs_adopted, production_bbl, n_wells,
                                                 upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj)]
      
      rm(list_pred_prod, list_prod_existing, list_prod_new)
      
      existing_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
      new_prod_dt[, ccs_adopted := as.character(ccs_adopted)]
      
      setkey(existing_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
             setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
      setkey(new_prod_dt, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
             setback_scenario, prod_quota_scenario, excise_tax_scenario, doc_field_code, year, ccs_adopted)
      
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
      
      field_all[is.na(new_prod_bbl), new_prod_bbl := 0]
      field_all[, total_prod_bbl := existing_prod_bbl + new_prod_bbl]
      
      field_all[is.na(new_ghg_kgCO2e), new_ghg_kgCO2e := 0]
      field_all[, total_ghg_kgCO2e := existing_ghg_kgCO2e + new_ghg_kgCO2e]
      
      field_all = merge(field_all, field_well_entry,
                        by = c('oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                               'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                               'doc_field_code', 'doc_fieldname', 'year'),
                        all = T)
      
      cols = c('new_wells', 'existing_prod_bbl', 'new_prod_bbl', 'total_prod_bbl', 
               'existing_ghg_kgCO2e', 'new_ghg_kgCO2e', 'total_ghg_kgCO2e')
      state_all = field_all[ , lapply(.SD, sum, na.rm = T), .SDcols = cols,
                             by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, year)] 
      
      state_all[, total_ghg_mtCO2e := total_ghg_kgCO2e/1e9]
      
      output_scen = list(field_all,
                         state_all)
      
      rm(state_all, field_all, existing_prod_dt, new_prod_dt)

      return(output_scen)
      
    }
    
    # SAVE OUTPUTS -------
    
    # prod_existing = rbindlist(list_existing_z)
    # prod_new = rbindlist(list_new_z)
    # 
    # output_list = list(prod_existing,
    #                    prod_new)
    
    res = lapply(1:nrow(scen_sel), func_yearly_production)
    
    output_list = do.call(Map, c(f = rbind, res))
    
    end_time = Sys.time()
    time_diff = difftime(end_time, start_time, units='mins')
    
    print(paste("Ended extraction model at ", end_time))
    
    print(paste("Model took ", round(time_diff[[1]]), " minutes to complete. Now saving results ..."))
    
    # save info file
      save_info_path = file.path(save_path, oil_price_selection)
      dir.create(save_info_path)
      print(paste0("Saving run information file to ", save_info_path))
      run_info = data.table(oil_price_selection = oil_price_selection,
                            start_time = start_time,
                            end_time = end_time,
                            duration = paste0(round(time_diff[[1]]), ' minutes'))
      fwrite(run_info, file.path(save_info_path, 'run_info.csv'), row.names = F)
    
    # save outputs to csv -----
    
    # create subdirectory of save_path for each oil price ------
    
    save_processed_path = file.path(save_path, oil_price_selection)
    dir.create(save_processed_path, showWarnings = FALSE)
    
    # save field-level results -----
    
    field_fname = paste0(oil_price_selection, '-field-level-results.csv')
    fwrite(output_list[[1]], file.path(save_processed_path, field_fname), row.names = F)
    print(paste0('Saved field-level results to ', field_fname))
    
    # save state-level results ------
    
    state_fname = paste0(oil_price_selection, '-state-level-results.csv')
    fwrite(output_list[[2]], file.path(save_processed_path, state_fname), row.names = F)
    print(paste0('Saved state-level results to ', state_fname))
    
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