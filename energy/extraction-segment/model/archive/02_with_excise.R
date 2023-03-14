# predict extraction 
# created: september 24, 2020
# author: meas meng

# rm(list=ls())

# inputs -----

  model_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
  scen_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
  entry_file      = 'entry_df_final.csv'
  scen_file       = 'input_variables_scenarios.csv' #rl
  # scen_file       = 'input_variables_scenarios_subset.csv' ## use this for troubleshooting and testing
  # scen_file       = 'input_variables_scenarios_bounding.csv' ## for bounding
  coef_file       = 'poisson_regression_coefficients_10132020_v3.csv'
  # coef_file       = 'poisson_regression_coefficients_11062020.csv' ## trying something else
  param_file      = 'forecasted_decline_parameters_2020_2050.csv'
  peak_file       = 'field-vintage_peak-production_yearly.csv'
  prod_file       = 'predicted-production_2020-2045_field_test.csv'
  #rl
  prod_vintage_file = 'predicted-production_2020-2045_field_vintage_test.csv'
  # hist_file       = 'new_wells_pred_weighted_R.csv'  ## update this
  histprod_file   = 'crude_prod_x_field.csv'
  
# outputs -----

  save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_20_all_scens'
  
  # save_path       = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/predict-production/scenarios_21_bau_new_coefs'
  
  
  
  # load packages -----
  
  library(data.table)
  library(ggplot2)
  library(hrbrthemes)
  library(extrafont)
  library(cowplot)
  library(tidyverse)
  library(extrafont)
  library(feather)
  
  ## set start time
  start_time <- Sys.time()
  
  # source from other scripts -----
    source(here::here('scripts/model/entry/prod_quota.R'))
  
    # source ccs emissions mean b calculation script
    source(here::here('scripts', 'model', 'ghg-emissions', 'ccs_parameterization.R'))

  # load data -----
  
  # load entry data
    entry_dt = fread(file.path(model_path, 'stocks-flows', entry_file), header = T)

  # load matrix of scenarios and forecasted variables
    # scenarios_dt = fread(file.path(scen_path, scen_file), header = T)
    scenarios_dt <- read_feather(paste0(scen_path, "/", scen_file))
  
    ## make sure quota is numeric
    scenarios_dt$quota = as.numeric(gsub(",", "", scenarios_dt$quota))
    
    ## convert to dt
    scenarios_dt <- setDT(scenarios_dt)
    
    # ## IF running BAU....
    # scenarios_dt <- scenarios_dt %>%
    #   filter(oil_price_scenario == "iea oil price",
    #          innovation_scenario == "low innovation",
    #          carbon_price_scenario == "price floor",
    #          ccs_scenario == "medium CCS cost",
    #          setback_scenario == "no_setback",
    #          excise_tax_scenario == "no tax",
    #          prod_quota_scenario == "no quota")
    # 
    
    
    
    
    # ## random selection of scenarios
    # un_scens = unique(scenarios_dt[, .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
    #                                    setback_scenario, prod_quota_scenario, excise_tax_scenario)])
    # set.seed(1)
    # ran_un = sample(1:nrow(un_scens), 500, replace = F)
    # 
    # scenarios_dt = scenarios_dt[un_scens[ran_un], on = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
    #                                                      setback_scenario, prod_quota_scenario, excise_tax_scenario), nomatch = 0]

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

## ccs emissions scalar
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
    dt_info[, m_opex_imputed_adj := ifelse(ccs_adoption < 0,
                                           m_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                           m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
    dt_info[, wm_opex_imputed_adj := ifelse(ccs_adoption < 0,
                                            wm_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                            wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
    
    ## track ccs adoption
    dt_info[, ccs_adopted := ifelse(ccs_adoption < 0,
                                    1, 0)] 
    

    
    ## adjust emissions intensity value again for ccs adoption
    dt_info[, ccs_scalar := ccs_ghg_scalar]
    
    dt_info[, upstream_kgCO2e_bbl_inno_ccs_adj := ifelse(ccs_adoption < 0,
                                                upstream_kgCO2e_bbl_inno_adj * ccs_ghg_scalar,
                                                upstream_kgCO2e_bbl_inno_adj)]
    
    # dt_info[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
    # dt_info[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]

# create data table of all variables needed ------
  
  # dt_info = scenarios_dt[coefs_dt, on = .(doc_field_code, doc_fieldname)]

# vector of years -----
  
  # pred_years = c(2020:2025) ## for testing
  pred_years = c(2020:2045)
  
# functions ------ 
  
  hypfunc = function(b,t,q_i,D_h) { q_i/((1 + b*D_h*t)^(1/b)) }
  expfunc = function(q_i,d,t) {   q_i*exp(-d*t) }
  
# iteratively predict entry of new wells and production in each year ------
  
  top10_fields = coefs_dt[!is.na(cons_hat)]
  other_fields = coefs_dt[is.na(cons_hat)]
  
  list_pred_prod_all = list()
  list_pred_prod_wide_all = list()
  list_prod_existing_all = list()
  list_prod_new_all = list()
    
  ## scenarios
  ### ----------------------------------
  
  scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                        carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])

  
  ## batching when only one batch needed
  # scen_combos = scen_combos[, batch := 1]
  
  
  ## batching with many scenarios
  batch_dt = data.table(batch = c(rep(1, 500), rep(2, 500), rep(3, 500), rep(4, 500), rep(5, 500), rep(6, 500),
                                  rep(7, 500), rep(8, 500), rep(9, 500), rep(10, 500), rep(11, 500), rep(12, 500),
                                  rep(13, 500), rep(14, 500), rep(15, 200)))
  scen_combos = cbind(batch_dt, scen_combos)

  
  # for (z in 1:length(unique(scen_combos$batch))) {
  for (z in 15:15) {
  
    print(z)
    
    list_pred_prod = list()
    list_pred_prod_wide = list()
    list_prod_existing = list()
    list_prod_new = list()
    
    scen = scen_combos[batch == z]
    scen = scen[, batch := NULL]
    # scen = scen_combos[z]
    # scen_name = paste(unlist(t(scen)), collapse = '_')
    # scen_name = gsub(' ', '_', scen_name)
    
    dt_info_z = dt_info[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                    carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
    
    dt_depl_z = dt_depl[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                   carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
    
    scenarios_dt_z = scenarios_dt[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                   carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]
    
    ## filter prod_existing_vintage
    setback_scen <- unique(scen[, setback_scenario])
    prod_existing_vintage_z = prod_existing_vintage[setback_scenario %in% setback_scen]
    # prod_existing_vintage_z = prod_existing_vintage[setback_scen, on = .(setback_scenario), nomatch = 0]
    
    for (i in seq_along(pred_years)) {
      
      
      t = pred_years[i]
      print(t)
      
      # if(t == 2045){browser()}
      
      # set up variables for top 10 fields
        temp_top10 = dt_info_z[year == t & doc_field_code %in% top10_fields[, doc_field_code]]
        temp_top10 = temp_top10[dt_depl_z[year == t], on = .(doc_field_code, 
                                                           oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                           setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                                                           year), nomatch = 0]
        temp_top10 = temp_top10[coefs_dt, on = .(doc_field_code, doc_fieldname), nomatch = 0]

      # poisson regression of top 10 fields
        temp_top10[, m_new_wells_pred := ifelse(depl < 0.9999, 
                                                exp(brent_hat*oil_price_usd_per_bbl + capex_hat*m_capex_imputed + opex_hat*m_opex_imputed_adj + depl_hat*depl + cons_hat),
                                                0)]
        temp_top10[, wm_new_wells_pred := ifelse(depl < 0.9999, 
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
        new_wells_prod$peak_production = new_wells_prod$peak_production * (1 - new_wells_prod$area_coverage)
      
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
        dtt_long[, cost_rank := frank(cost), by = list(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                       setback_scenario, prod_quota_scenario, excise_tax_scenario)]
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
        
        temp_prod_existing_vintage <- left_join(temp_prod_existing_vintage, existing_vintage_prod_t) %>%
          mutate(num_wells = 1) %>%
          as.data.table()
        
        
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
        
          prev_existing_prod <- list_prod_existing[[i-1]] %>%
            select(doc_field_code:excise_tax_scenario, prev_prod_bbl = production_bbl)
            # prev_existing_prod = list_prod_existing[[i-1]][, -'year']
            # prev_existing_prod = unique(prev_existing_prod)
            # setnames(prev_existing_prod, 'production_bbl', 'prev_prod_bbl')
          
          temp_prod_existing_vintage_adj <- temp_prod_existing_vintage %>%
            left_join(prev_existing_prod) %>%
            select(doc_field_code:vintage, vintage_start, num_wells, production_bbl, prev_prod_bbl) %>%
            mutate(production_bbl_adj = ifelse(prev_prod_bbl == 0, 0, production_bbl)) %>%
            select(-production_bbl, -prev_prod_bbl) %>%
            rename(production_bbl = production_bbl_adj) %>%
            select(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                   setback_scenario, prod_quota_scenario, excise_tax_scenario, quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl, 
                   upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj, num_wells, production_bbl,
                   vintage, vintage_start)
            # temp_prod_existing_vintage = merge(temp_prod_existing_vintage, prev_existing_prod,
            #                                    by = c('doc_field_code', 'doc_fieldname', 'vintage',
            #                                           'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
            #                                           'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'),
            #                                        all.x = T)
            # temp_prod_existing_vintage[, production_bbl_adj := ifelse(prev_prod_bbl == 0 & production_bbl > 0, 0, production_bbl)]
            # temp_prod_existing_vintage[, c('production_bbl', 'prev_prod_bbl') := NULL]
            # setnames(temp_prod_existing_vintage, 'production_bbl_adj', 'production_bbl')
          
          temp_prod_existing_vintage <- setDT(temp_prod_existing_vintage_adj)
          
        }
        
        temp_prod_existing_vintage <- temp_prod_existing_vintage %>%
          mutate(vintage_start = substr(vintage, 1, 4),
                 vintage_start = as.numeric(ifelse(vintage_start == "pre ", 1977, vintage_start))) %>%
          as.data.table()
          # temp_prod_existing_vintage[, vintage_start := ifelse(vintage == 'pre 1978', 1977, as.numeric(substr(vintage, 1, 4)))]
        
        # set up copy of dataframe for vintages from previous interations
        ## -------------------------------------------------------------------------
        
        temp_prod_new_vintage = rbindlist(list_pred_prod) %>%
          as.data.table()
        
        ##   
      
        
        if(i > 1) {
          
          temp_prod_new_vintage_info = dt_info_z[year == t, . (doc_field_code, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
                                                             ccs_scenario, prod_quota_scenario, excise_tax_scenario, setback_scenario, quota, innovation_multiplier, ccs_adopted, ccs_scalar,
                                                             upstream_kgCO2e_bbl, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj)]
          
          temp_prod_new_vintage <- temp_prod_new_vintage %>%
            filter(year == t) %>%
            mutate(vintage = "new",
                   vintage_start = entry_year) %>%
            select(doc_field_code:excise_tax_scenario, num_wells = m_new_wells_pred, production_bbl, vintage, vintage_start) %>%
            left_join(temp_prod_new_vintage_info) %>%
            as.data.table()
              # temp_prod_new_vintage = rbindlist(list_pred_prod)[year == t]
              # temp_prod_new_vintage[, vintage := 'new']
              # temp_prod_new_vintage[, vintage_start := entry_year]
              # setnames(temp_prod_new_vintage, 'm_new_wells_pred', 'num_wells')
              # temp_prod_new_vintage[, c('entry_year', 'wm_new_wells_pred', 'oil_price_usd_per_bbl', 'year') := NULL]
              # temp_prod_new_vintage = merge(temp_prod_new_vintage, scen_combos,
              #                               by = c("oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
              #                                      "prod_quota_scenario", "excise_tax_scenario"),
              #                               all.x = T,
              #                               allow.cartesian = T)
          
        
          ## refer to t - 1 to determine which recent field-vintages are still producing
          
          prev_new_prod <- list_prod_new[[i - 1]] %>%
            select(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                   ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, vintage, vintage_start,
                   num_wells = n_wells, prev_prod_bbl = production_bbl)
            # prev_new_prod = list_prod_new[[i - 1]][, .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
            #                                            ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, vintage, vintage_start,
            #                                            m_new_wells_pred, production_bbl)]
            # prev_new_prod = unique(prev_new_prod)
            # setnames(prev_new_prod, 'production_bbl', 'prev_prod_bbl')
          
          temp_prod_new_vintage_adj <- temp_prod_new_vintage %>%
            left_join(prev_new_prod) %>%
            mutate(production_bbl_adj = ifelse(prev_prod_bbl == 0, 0, production_bbl),
                   num_wells = ifelse(prev_prod_bbl == 0, 0, num_wells)) %>%
            select(-production_bbl, -prev_prod_bbl) %>%
            rename(production_bbl = production_bbl_adj)
            
          
              # temp_prod_new_vintage = merge(temp_prod_new_vintage, prev_new_prod,
              #                               by = c( "doc_field_code", "doc_fieldname", 
              #                                       "oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", 
              #                                       "setback_scenario", "prod_quota_scenario", "excise_tax_scenario",
              #                                       "vintage", "vintage_start"),
              #                               all.x = T)
              # temp_prod_new_vintage[, production_bbl_adj := ifelse(prev_prod_bbl == 0 & production_bbl > 0, 0, production_bbl)]
              # temp_prod_new_vintage[, c('production_bbl', 'prev_prod_bbl', 'num_wells') := NULL]
              # setnames(temp_prod_new_vintage, 'production_bbl_adj', 'production_bbl')
              # setnames(temp_prod_new_vintage, 'm_new_wells_pred', 'num_wells')
          
            temp_prod_new_vintage <- setDT(temp_prod_new_vintage_adj)
          
          # one data table with production data of both new and existing wells
          temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage, temp_prod_new_vintage)
        
        } else {
        
            temp_prod_quota = rbind(temp_new_wells_prod, temp_prod_existing_vintage)

        }

        # # join production dataframe to cost rankings
        # temp_prod_quota = temp_dt_info_rank %>% 
        #   full_join(temp_prod_quota, by = c("doc_field_code","doc_fieldname","oil_price_scenario", "innovation_scenario", "carbon_price_scenario", "ccs_scenario", "setback_scenario", "prod_quota_scenario"))
        # # note: fields without entry show NA for num_wells, production_bbl, vintage because these fields are not in temp_dt_info_rank
        # temp_prod_quota[is.na(num_wells), num_wells := 0]
        # temp_prod_quota[is.na(production_bbl), production_bbl := 0]
        # temp_prod_quota[is.na(vintage), vintage := "new"]
        
        # join production dataframe to cost rankings
        temp_prod_quota = temp_prod_quota %>%
          left_join(temp_dt_info_rank)
            # temp_prod_quota = merge(temp_prod_quota, temp_dt_info_rank, 
            #                         by = c('doc_field_code', 'doc_fieldname',
            #                                'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
            #                                'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'), 
            #                         all.x = T)
        
        # put ranks in one column
        temp_prod_quota$cost_rank = ifelse(temp_prod_quota$vintage_start == t, temp_prod_quota$cost_new_rank, temp_prod_quota$cost_existing_rank)
        temp_prod_quota = subset(temp_prod_quota, select = -c(cost_existing_rank, cost_new_rank))
          # temp_prod_quota[, cost_rank := ifelse(vintage_start == t, cost_new_rank, cost_existing_rank)]
          # temp_prod_quota[, c('cost_existing_rank', 'cost_new_rank') := NULL]
        
        # # column with integers representing vintages, for secondary ranking by vintage later
        ## NOTE: this happens earlier now
        
        # temp_prod_quota[, vintage_start := as.numeric(substr(vintage,1,4)) ]
        # temp_prod_quota$vintage_start = ifelse(temp_prod_quota$vintage == "pre 1978", 1977, temp_prod_quota$vintage_start) 
        # temp_prod_quota$vintage_start = ifelse(temp_prod_quota$vintage == "new", t, temp_prod_quota$vintage_start) 
        
        # calculate cumulative production in ascending order of rank then descending order of vintage 
        # (i.e. assume that within a field, older vintages get shut first)
        temp_prod_quota2 = temp_prod_quota %>%
          group_by(doc_field_code,
                   oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                   setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
          arrange(-vintage_start) %>%
          mutate(cost_vintage_rank = 1:n()) %>%
          ungroup()
        
        ## calculate cumulative sum by scenario, arranging by cost rank and cost vintage rank
        temp_prod_quota3 = temp_prod_quota2 %>%
          ## remove fields/field vintages with NA costs for now -- those are all gas fields right now
          filter(!is.na(cost_rank)) %>%
          group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
          arrange(cost_rank, cost_vintage_rank) %>%
          mutate(prod_cumsum = cumsum(production_bbl)) %>%
          ungroup()
        
        ## new column for how much is produced under prod quota
        ## set all field-vintages that are greater than prod quota to zero, i.e. not allowed to produce
        temp_prod_quota4 <- temp_prod_quota3 %>%
          mutate(prod_limited = ifelse(prod_cumsum > quota, 0, prod_cumsum)) 
        
        
        ## previous code
        # temp_prod_quota3$prod_limited = ifelse(temp_prod_quota$prod_cumsum > temp_prod_quota$quota, 0, temp_prod_quota$prod_cumsum) 
        # temp_prod_quota3$num_wells = ifelse(temp_prod_quota$prod_limited == 0 & temp_prod_quota$vintage == "new", 0, temp_prod_quota$num_wells) 
        
      
        # find field-vintage that prod quota lands in and refine  
        # if vintage is not new, do nothing (assume that an entire field-vintage shuts down or not, since we don't want to keep track of well-level prod for existing wells)
        # if vintage is new, calculate no. of new wells that can enter
        
        ## calculating remaining production if relevant
        temp_prod_quota5 <- temp_prod_quota4 %>%
          mutate(over_quota_ranks = ifelse(prod_cumsum > quota, 1, 0)) %>%
          group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
          ## find previous productoin, calc cumulative sum
          mutate(last_prod = lag(prod_cumsum),
                 sum_over = cumsum(over_quota_ranks)) %>%
          ungroup() %>%
          mutate(prod_quota_remainder = quota - last_prod,
                 adj_prod_limited = ifelse(sum_over == 1 & vintage == "new", prod_quota_remainder,
                                           ifelse(sum_over > 1 & vintage == "new", 0,
                                                  ifelse(sum_over > 0 & vintage != "new", 0, production_bbl))),
                 adj_new_wells = ifelse(sum_over == 1 & vintage == "new", adj_prod_limited / (production_bbl / num_wells), 
                                               ifelse(sum_over > 1 & vintage == "new", 0, 
                                                      ifelse(sum_over > 0 & vintage != "new", 0, num_wells))))
        
        
        
        
        ## previous code
        ## Get the field-vintage quota lands in (call it quota_bin)
        ## temp_prod_quota$over_quota_ranks = ifelse(temp_prod_quota$prod_cumsum > temp_prod_quota$quota, temp_prod_quota$cost_vintage_rank, 9999) 
        
        # temp_prod_quota6 = temp_prod_quota5 %>% 
        #   rowwise() %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(quota_bin = min(over_quota_ranks, na.rm = TRUE)) %>% 
        #   ungroup()
        
        # Get cum prod of field-vintage before quota's bin
        # temp_prod_quota$prod_quota_bin_prev = ifelse(temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin-1, temp_prod_quota$prod_cumsum, -1)
        # # Calculate production allowed for quota bin
        # temp_prod_quota$prod_quota_remainder = ifelse(temp_prod_quota$prod_quota_bin_prev!=-1, temp_prod_quota$quota-temp_prod_quota$prod_cumsum, 0)
        # temp_prod_quota = temp_prod_quota %>%
        #   rowwise() %>%
        #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario) %>%
        #   mutate(prod_quota_remainder = max(prod_quota_remainder)) %>% 
        #   ungroup()
        
        ## update production and no. new wells entered for quota bin if vintage is new
        ## can discuss if want to let existing field-vintage continue producing partially
        # temp_prod_quota$prod_limited = ifelse((temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin & temp_prod_quota$vintage=="new"), temp_prod_quota$prod_quota_remainder, temp_prod_quota$prod_limited) 
        # temp_prod_quota$num_wells = ifelse((temp_prod_quota$cost_vintage_rank==temp_prod_quota$quota_bin & temp_prod_quota$vintage=="new"), temp_prod_quota$prod_limited/(temp_prod_quota$production_bbl/temp_prod_quota$num_wells), temp_prod_quota$num_wells) 
        
        ## remove some columns, add 
        temp_prod_quota6 <- temp_prod_quota5 %>%
          select(-production_bbl, -num_wells, -prod_limited, -over_quota_ranks, -sum_over, -last_prod, -prod_quota_remainder, -prod_cumsum) 
        
        # update new_wells_prod (wells that start in year t)
        temp_prod_quota_new_wells <- temp_prod_quota6 %>%
          filter(vintage == "new") %>%
          mutate(year = t,
                 upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
                 upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
                 upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited) %>%
          select(year, doc_field_code, doc_fieldname, vintage, vintage_start, oil_price_scenario, innovation_scenario, 
                 carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                 quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
                 n_wells = adj_new_wells, production_bbl = adj_prod_limited, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj) %>%
          as.data.table()
        
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
        list_prod_new[[i]] = temp_prod_quota_new_wells
        
        ## create object with production for existing wells (quota incorporated)
        temp_prod_quota_existing_wells <- temp_prod_quota6 %>%
          filter(vintage != "new") %>%
          mutate(year = t,
                 upstream_kgCO2e_inno_ccs_adj = upstream_kgCO2e_bbl_inno_ccs_adj * adj_prod_limited,
                 upstream_kgCO2e_inno_adj = upstream_kgCO2e_bbl_inno_adj * adj_prod_limited,
                 upstream_kgCO2e = upstream_kgCO2e_bbl * adj_prod_limited) %>%
          select(year, doc_field_code, doc_fieldname, vintage, vintage_start, oil_price_scenario, innovation_scenario, 
                 carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, 
                 quota, innovation_multiplier, ccs_adopted, ccs_scalar, upstream_kgCO2e_bbl_inno_adj, upstream_kgCO2e_bbl_inno_ccs_adj,
                 n_wells = adj_new_wells, production_bbl = adj_prod_limited, upstream_kgCO2e, upstream_kgCO2e_inno_adj, upstream_kgCO2e_inno_ccs_adj) %>%
          as.data.table()
      
        
        ### test - sum production by scenario
        # test <- temp_prod_quota6 %>%
        #   mutate(type = ifelse(vintage == "new", "new", "existing")) %>%
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
        new_entry_prod <- temp_prod_quota_new_wells %>%
          filter(vintage_start == t) %>%
          select(-quota, -ccs_adopted, -innovation_multiplier, -ccs_scalar, - upstream_kgCO2e_bbl_inno_adj, -upstream_kgCO2e_bbl_inno_ccs_adj,
                 -upstream_kgCO2e, -upstream_kgCO2e_inno_adj, -upstream_kgCO2e_inno_ccs_adj)
        
        new_wells_prod_new <- 
          ## new wells prod filtered for fields that enter and peak prod > 0
          new_wells_prod[year == t & m_new_wells_pred > 0 & peak_production > 0] %>%
          select(-peak_production, -m_new_wells_pred) %>%
          left_join(new_entry_prod) %>%
          select(-vintage) %>%
          rename(peak_production = production_bbl) %>%
          ## filter out field vintages that are not extracted bc of quota
          filter(peak_production > 0) %>%
          rename(m_new_wells_pred = n_wells) %>%
          as.data.table()
        
        
        # prod_existing = temp_prod_existing
        # setDT(prod_existing)
          
        rm(dt_info_rank, temp_dt_info_rank, prev_existing_prod, existing_vintage_prod_t, 
           temp_new_wells_prod, temp_prod_new_vintage_adj, temp_prod_new_vintage, temp_prod_existing_vintage, temp_prod_existing_vintage_adj, 
           temp_prod_quota, temp_prod_quota2, temp_prod_quota3, temp_prod_quota4, temp_prod_quota5, temp_prod_quota6, 
           temp_prod_quota_new_wells, temp_prod_quota_existing_wells)  
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
          new_wells_prod_new[! is.na(b2), col :=  ifelse(j < t + int_year,
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
        
        ## test - sum production by scenario
        # test3 <- new_wells_prod_long %>%
        #   group_by(oil_price_scenario, innovation_scenario,
        #            carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
        #   summarise(total_prod = sum(production_bbl)) %>%
        #   ungroup() %>%
        #   mutate(total_prod_m = total_prod / 1e6,
        #          type = "new")
        
        
        list_pred_prod_wide[[i]] = new_wells_prod_new
        list_pred_prod[[i]] = new_wells_prod_long
        
        
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
          
        prod_next_year <- prod_new %>%
          full_join(prod_old) %>%
          mutate(old_bbl = ifelse(is.na(old_bbl), 0, old_bbl),
                 new_bbl = ifelse(is.na(new_bbl), 0, new_bbl)) %>%
          rowwise() %>%
          mutate(production_bbl = old_bbl + new_bbl) %>%
          ungroup() %>% 
          select(doc_field_code, 
                 oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                 setback_scenario, prod_quota_scenario, excise_tax_scenario, year, production_bbl) %>%
          as.data.table()
          
       
        
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
        #rl
        trr_prev = unique(scenarios_dt_z[year == t, .(doc_field_code, oil_price_scenario, 
                                                    innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                    setback_scenario, prod_quota_scenario, excise_tax_scenario, resource)])
        
        #rl - match 
        prod_next_year = prod_next_year[depl_prev, on = .(doc_field_code, 
                                                          oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                          setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        prod_next_year = prod_next_year[trr_prev, on = .(doc_field_code, 
                                                         oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                                         setback_scenario, prod_quota_scenario, excise_tax_scenario)]
        
        ## fill in missing info
        prod_next_year <- prod_next_year %>%
          mutate(year = ifelse(is.na(year), t, year),
                 production_bbl = ifelse(is.na(production_bbl), 0 , production_bbl)) %>%
          as.data.table()
                 
        ## calculate depletion
        prod_next_year[, depl := depl_prev + (production_bbl / resource)]
        
        #rl
        depl_next_year = prod_next_year[, .(doc_field_code, 
                                            oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
                                            setback_scenario, prod_quota_scenario, excise_tax_scenario,
                                            year, depl)]
        depl_next_year[, year := t + 1]
        # depl_next_year = unique(depl_next_year)
        
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
        info_next_year[, ccs_adoption := ifelse(adoption_prev > 0,
                                                ccs_adj_usd_per_kg - carbon_price_usd_per_kg,
                                                adoption_prev)]
        info_next_year[, m_opex_imputed_adj := ifelse(ccs_adoption < 0,
                                                      m_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                                      m_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
        info_next_year[, wm_opex_imputed_adj := ifelse(ccs_adoption < 0,
                                                       wm_opex_imputed_adj + (ccs_adj_usd_per_kg * upstream_kgCO2e_bbl_inno_adj),
                                                       wm_opex_imputed_adj + (carbon_price_usd_per_kg * upstream_kgCO2e_bbl_inno_adj))]
        # info_next_year[is.na(m_opex_imputed_adj), m_opex_imputed_adj := m_opex_imputed]
        # info_next_year[is.na(wm_opex_imputed_adj), wm_opex_imputed_adj := wm_opex_imputed]
        info_next_year[, adoption_prev := NULL]
        
        ## track ccs adoption
        info_next_year[, ccs_adopted := ifelse(ccs_adoption < 0,
                                        1, 0)] 
        
        info_next_year[, ccs_scalar := ccs_ghg_scalar]
        
        ## adjust emissions intensity value again for ccs adoption
        info_next_year[, upstream_kgCO2e_bbl_inno_ccs_adj := ifelse(ccs_adoption < 0,
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
      #   info_next_year[, m_opex_imputed_adj := ifelse(ccs_adoption < 0,
      #                                          m_opex_imputed + (ccs_avg_usd_per_kg*upstream_kgCO2e_bbl),
      #                                          m_opex_imputed + (carbon_price_usd_per_kg*upstream_kgCO2e_bbl))]
      #   info_next_year[, wm_opex_imputed_adj := ifelse(ccs_adoption < 0,
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
    
    # pred_wells = rbindlist(list_pred_wells) ## not updated for quota
    pred_prod = rbindlist(list_pred_prod) ##
    pred_prod_wide = rbindlist(list_pred_prod_wide)
    prod_existing_updated = rbindlist(list_prod_existing)
    pred_prod[, year := as.numeric(as.character(year))]
    prod_new = rbindlist(list_prod_new)

    all_prod <- prod_new %>%
      rbind(prod_existing_updated) %>%
      mutate(batch = z) %>%
      as.data.table()

    # output_folder <- 'scen_outputs'
    # output_type <- 'vintage'
    # vname <- paste0('vintage_outputs_batch', z, '.csv')
    
    # saveRDS(all_prod, file = paste0(save_path, '/scen_outputs/vintage/', scen_name, '_vintage_outputs.rds'))
    # saveRDS(all_prod, file = paste0(save_path, '/scen_outputs/vintage/vintage_outputs_batch', z, '.rds'))
    # fwrite(all_prod, file.path(save_path, output_folder, output_type, vname), row.names = F)
    write_feather(all_prod, paste0(save_path, '/scen_outputs/vintage-replace/vintage_outputs_batch', z, '.csv'))


    ## summarise by scenario

    # ## quota scen
    # pq_dt <- scenarios_dt_z %>%
    #   select(year, oil_price_usd_per_bbl, prod_quota_scenario, quota) %>%
    #   unique() %>%
    #   mutate(quota = quota / 1e6) %>%
    #   as.data.table()
    # 
    # annual_outputs <- all_prod %>%
    #   group_by(oil_price_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario,
    #            setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
    #   summarise(total_upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
    #             total_upstream_kgCO2e_inno_adj = sum(upstream_kgCO2e_inno_adj, na.rm = T),
    #             total_upstream_kgCO2e_inno_ccs_adj = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T),
    #             total_prod = sum(production_bbl, na.rm = T)) %>%
    #   ungroup() %>%
    #   mutate(ghg_mmt = total_upstream_kgCO2e / (1e3 * 1e6),
    #          ghg_mmt_inno_adj =  total_upstream_kgCO2e_inno_adj / (1e3 * 1e6),
    #          ghg_mmt_inno_ccs_adj = total_upstream_kgCO2e_inno_ccs_adj / (1e3 * 1e6),
    #          prod_mbbl = total_prod / 1e6) %>%
    #   left_join(pq_dt) %>%
    #   as.data.table()
    # 
    # 
    # # output_type2 <- 'summary'
    # # sname <- paste0('summary_outputs_batch', z, '.csv')
    # 
    # # saveRDS(annual_outputs, file = paste0(save_path, '/scen_outputs/summary/', scen_name, '_summary_outputs.rds'))
    # # saveRDS(annual_outputs, file = paste0(save_path, '/scen_outputs/summary/summary_outputs_batch', z, '.rds'))
    # # fwrite(annual_outputs, file.path(paste0(save_path, '/scen_outputs/summary/summary_outputs_batch', z,'.csv')), row.names = F)
    # # fwrite(annual_outputs, file.path(save_path, output_folder, output_type2, sname), row.names = F)
    # write_feather(annual_outputs, paste0(save_path, '/scen_outputs/summary/summary_outputs_batch', z, '.csv'))

    ## store in main lists
    # 
    # list_pred_prod_all[[z]] = pred_prod
    # list_pred_prod_wide_all[[z]] = pred_prod_wide
    # list_prod_existing_all[[z]] = prod_existing_updated
    # list_prod_new_all[[z]] = prod_new

  
    rm(pred_prod, pred_prod_wide, prod_existing_updated, prod_new, annual_outputs, pq_dt, all_prod)
    
    print(z)
    print(Sys.time() - start_time)
    
    
    
  }  

  
  
  # bind lists into data tables -----
  
  # pred_wells = rbindlist(list_pred_wells) ## not updated for quota
  # pred_prod = rbindlist(list_pred_prod_all) ## 
  # pred_prod_wide = rbindlist(list_pred_prod_wide_all)
  # prod_existing_updated = rbindlist(list_prod_existing_all)
  # pred_prod[, year := as.numeric(as.character(year))]
  # prod_new = rbindlist(list_prod_new_all)
  # 
  
  ## read in rds files (batches), stitch together, try to make figs and output files
  ## --------------------------------------------------------------------------
  
  start_time <- Sys.time()
  
  ## create summary df from vintage files
  ## ------------------------------------------
  
  ## quota scen
  pq_dt <- scenarios_dt %>%
    select(year, oil_price_scenario, oil_price_usd_per_bbl, prod_quota_scenario, quota) %>%
    unique() %>%
    mutate(quota = quota / 1e6) %>%
    as.data.table()

  ## summarise by scenario
  bind_summary_df <- function(file_name) {
    
    # out <- fread(paste0(save_path, '/scen_outputs/summary/', file_name))
    
    out <- read_feather(paste0(save_path, '/scen_outputs/vintage/', file_name))
    
    # write_csv(out, paste0(save_path, '/scen_outputs/vintage/bau_outputs.csv'))
    
    z_temp <- str_remove_all(file_name, pattern = "vintage_outputs_batch")
    z_temp2 <- str_remove(z_temp, pattern = ".csv")
    
    out_temp <- out %>%
      group_by(oil_price_scenario, innovation_scenario, innovation_multiplier, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
      summarise(total_upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
                total_upstream_kgCO2e_inno_adj = sum(upstream_kgCO2e_inno_adj, na.rm = T),
                total_upstream_kgCO2e_inno_ccs_adj = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T),
                total_prod = sum(production_bbl, na.rm = T)) %>%
      ungroup() %>%
      mutate(ghg_mmt = total_upstream_kgCO2e / (1e3 * 1e6),
             ghg_mmt_inno_adj =  total_upstream_kgCO2e_inno_adj / (1e3 * 1e6),
             ghg_mmt_inno_ccs_adj = total_upstream_kgCO2e_inno_ccs_adj / (1e3 * 1e6),
             prod_mbbl = total_prod / 1e6) %>%
      left_join(pq_dt) %>%
      as.data.table()
    
    write_feather(out_temp, paste0(save_path, '/scen_outputs/summary/summary_outputs_batch', z_temp2, '.csv'))
    
  }
  
  file_list_vintage <- list.files(paste0(save_path, "/scen_outputs/vintage/"))
  
  annual_outputs <- purrr::map(file_list_vintage, bind_summary_df) %>%
    bind_rows() 
  
  # saveRDS(annual_outputs, file = paste0(save_path, '/summary_outputs_2020-2045.rds'))
  write_feather(annual_outputs, paste0(save_path, '/summary_outputs_2020-2045.csv'))
  write_csv(annual_outputs, paste0(save_path, '/summary_outputs_2020-2045.csv'))
    
## summary df for plotting  
  create_summary_df <- function(file_name) {
    
    # out <- readRDS(paste0(save_path, '/scen_outputs/vintage/', file_name))
    out <- read_feather(paste0(save_path, '/scen_outputs/vintage/', file_name))
    
    summary_df <- out %>%
      mutate(vgroup = ifelse(vintage == "new", "new", "existing")) %>%
      group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario, vgroup, year) %>%
      summarise(upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
                upstream_kgCO2e_inno_adj = sum(upstream_kgCO2e_inno_adj, na.rm = T),
                upstream_kgCO2e_inno_ccs_adj = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T),
                production_bbl = sum(production_bbl, na.rm = T)) %>%
      ungroup() %>%
      as.data.table()
    
  }
  
  outputs_plotting <- purrr::map(file_list_vintage, create_summary_df) %>%
    bind_rows()

  ## create df with number of new wells by year
  ## -------------------------------------------------------
  
  ## quota and setback new well prod
  oil_price_df <- scenarios_dt %>%
    select(oil_price_scenario:oil_price_usd_per_bbl, year) %>%
    unique()
  
  create_new_wells_df <- function(file_name) {
    
    out <- read_feather(paste0(save_path, '/scen_outputs/vintage/', file_name))
    
    new_wells_temp <- out %>%
      filter(vintage_start == year) %>%
      group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario,
               ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
      ## for now, na.rm = T, but make sure the NAs make sense
      summarise(new_wells_pred = sum(n_wells, na.rm = T)) %>%
      mutate(type = "non-weighted mean forecast") %>%
      left_join(oil_price_df) %>%
      as.data.table()
    
  }
  
  agg_well_entry <- purrr::map(file_list_vintage, create_new_wells_df) %>%
    bind_rows()
  

  
  # ### summarise outputs by scenario
  # ### ----------------------------------------------------------

  # all_prod <- prod_new %>%
  #   rbind(prod_existing_updated) %>%
  #   as.data.table()
  # 
  # 
  # saveRDS(all_prod, file = paste0(save_path, '/vintage_outputs_2020-2045.rds'))
# 
# 
#   ## quota scen
#   pq_dt <- scenarios_dt %>%
#     select(year, innovation_scenario, innovation_multiplier, oil_price_usd_per_bbl, prod_quota_scenario, quota) %>%
#     unique() %>%
#     mutate(quota = quota / 1e6) %>%
#     as.data.table()
# 
#   annual_outputs <- outputs_plotting %>%
#     group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
#              setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
#     summarise(total_upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
#               total_upstream_kgCO2e_adj = sum(upstream_kgCO2e_adj, na.rm = T),
#               total_prod = sum(production_bbl, na.rm = T)) %>%
#     ungroup() %>%
#     mutate(ghg_mmt = total_upstream_kgCO2e / (1e3 * 1e6),
#            ghg_mmt_adj =  total_upstream_kgCO2e_adj / (1e3 * 1e6),
#            prod_mbbl = total_prod / 1e6) %>%
#     left_join(pq_dt) %>%
#     as.data.table()

  # saveRDS(annual_outputs, file = paste0(save_path, '/summary_outputs_2020-2045.rds'))
  # 
  # print(Sys.time() - start_time)
  # 
  # 
  
  # test <- all_prod[order(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
  #                         setback_scenario, prod_quota_scenario, doc_field_code, doc_fieldname, vintage,
  #                         vintage_start, year)]
  # 
  # View(test %>% filter(oil_price_scenario == "high oil price",
  #                      innovation_scenario == "low innovation",
  #                      carbon_price_scenario == "price floor",
  #                      ccs_scenario == "high CCS cost",
  #                      setback_scenario == "setback_2500ft",
  #                      prod_quota_scenario == "medium quota",
  #                      excise_tax_scenario == "no tax"))
  
  # ## quota scen 
  # quota_dt <- scenarios_dt %>%
  #   select(year, prod_quota_scenario, quota) %>%
  #   unique() %>%
  #   mutate(quota = quota / 1e6) %>%
  #   as.data.table()
  # 
  # prod_test <- all_prod2 %>%
  #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
  #            setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
  #   summarise(total_prod = sum(production_bbl, na.rm = T)) %>%
  #   ungroup() %>%
  #   mutate(total_prod_m = total_prod / 1e6,
  #          total_prod_m = ifelse(is.na(total_prod_m), 0, total_prod_m)) %>%
  #   left_join(quota_dt) %>%
  #   as.data.table()
  # 
  # ghg_test <- all_prod %>%
  #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
  #            setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
  #   summarise(total_upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
  #             total_upstream_kgCO2e_adj = sum(upstream_kgCO2e_adj, na.rm = T),
  #             total_prod = sum(production_bbl, na.rm = T)) %>%
  #   ungroup() %>%
  #   mutate(ghg_mmt = total_upstream_kgCO2e / (1e3 * 1e6),
  #          ghg_mmt_adj =  total_upstream_kgCO2e_adj / (1e3 * 1e6),
  #          prod_mbbl = total_prod / 1e6) %>%
  #   as.data.table()

  
  # test <- prod_new %>%
  #   group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, 
  #            ccs_scenario, setback_scenario, prod_quota_scenario, year) %>%
  #   summarise(total_prod = sum(production_bbl)) %>%
  #   ungroup() %>%
  #   mutate(total_prod_m = total_prod / 1e6,
  #          type = "new") %>%
  #   as.data.table()
  # 
  #  test_old <- prod_existing_updated %>%
  #    group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, 
  #             ccs_scenario, setback_scenario, prod_quota_scenario, year) %>%
  #    summarise(total_prod = sum(production_bbl)) %>%
  #    ungroup() %>%
  #    mutate(total_prod_m = total_prod / 1e6,
  #           type = "old") %>%
  #    as.data.table()
  # 
  #  ## check this object out -- looks like quota is working (scen_total <= quota)
  #  test_prod <- rbind(test, test_old) %>%
  #    group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, 
  #             ccs_scenario, setback_scenario, prod_quota_scenario, year) %>%
  #    mutate(scen_total = sum(total_prod_m)) %>%
  #    ungroup()
   
  

# subset columns for new well entries ------
  
  # pred_wells_2 = pred_wells[, .(year, doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
  #                               carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, oil_price_usd_per_bbl, 
  #                               m_new_wells_pred, wm_new_wells_pred)]
  # 
  # pred_wells_long = melt(pred_wells_2, 
  #                        id.vars = c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 
  #                                    'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
  #                                    'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
  #                                    'oil_price_usd_per_bbl'),
  #                        measure.vars = c('m_new_wells_pred', 'wm_new_wells_pred'), variable.name = 'type', value.name = 'new_wells_pred')
  # pred_wells_long[, type := ifelse(type %like% "wm_", "weighted mean forecast", "non-weighted mean forecast")]
  # 
# combine historic (actual and modeled) well entry with forecasted well entry ------
  #rl
  un_scens = unique(scenarios_dt[, c('doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                     'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario')])
  # 
  # hist_modeled_2 = hist_modeled[unique(entry_dt[, .(doc_field_code, doc_fieldname)]), on = 'doc_field_code', nomatch = 0]
  # hist_modeled_2[, type := 'historic modeled']
  # hist_modeled_2 = un_scens[hist_modeled_2, on = .(doc_field_code, doc_fieldname), allow.cartesian = T]
  # hist_modeled_2 = hist_modeled_2[unique(entry_dt_vars[, .(year, brent)]), on = 'year', nomatch = 0]
  # setnames(hist_modeled_2, 'brent', 'oil_price_usd_per_bbl')
  
  hist_actual = entry_dt_vars[, .(doc_field_code, doc_fieldname, year, brent, new_wells)]
  setnames(hist_actual, 'new_wells', 'new_wells_pred')
  setnames(hist_actual, 'brent', 'oil_price_usd_per_bbl')
  hist_actual[, type := 'historic actual']
  
  hist_actual_2 = un_scens[hist_actual, on = .(doc_field_code, doc_fieldname), allow.cartesian = T]
  
  
  ## combine historical
  # well_entry_hist = rbindlist(list(hist_actual_2, hist_modeled_2), use.names = T, fill = T) 
  well_entry_hist = hist_actual_2
  
  well_entry_hist <- well_entry_hist %>%
    filter(!is.na(oil_price_scenario)) %>%
    as.data.table()
  
  agg_hist_well_entry = well_entry_hist[, .(oil_price_usd_per_bbl = median(oil_price_usd_per_bbl, na.rm = T),
                                      new_wells_pred = sum(new_wells_pred, na.rm = T)),
                                  by = .(year, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
                                         setback_scenario, prod_quota_scenario, excise_tax_scenario, type)]
  
### 
   # 
   # adj_well_entry <- outputs_plotting %>%
   #  filter(vintage_start == year) %>%
   #  group_by(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario,
   #           ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
   #  ## for now, na.rm = T, but make sure the NAs make sense
   #  summarise(new_wells_pred = sum(n_wells, na.rm = T)) %>%
   #  mutate(type = "non-weighted mean forecast") %>%
   #  left_join(oil_price_df) %>%
   #  as.data.table()
   # 

    
   # well_entry_all = rbindlist(list(pred_wells_long, hist_actual_2, hist_modeled_2), use.names = T, fill = T)
 
  ### old 
  
  # well_entry_all = rbindlist(list(adj_well_entry, hist_actual_2, hist_modeled_2), use.names = T, fill = T) 
  # 
  # well_entry_all <- well_entry_all %>%
  #   filter(!is.na(oil_price_scenario)) %>%
  #   as.data.table()
   
    #rl
  
  
  agg_well_entry <- rbind(agg_hist_well_entry, agg_well_entry)
  
  # setorderv(well_entry_all, c('year', 'doc_field_code', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario', 
  #                             'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario'))
  
# aggregate well entry by year at the state level ------
  #rl
  # agg_well_entry = well_entry_all[, .(oil_price_usd_per_bbl = median(oil_price_usd_per_bbl, na.rm = T),
  #                                     new_wells_pred = sum(new_wells_pred, na.rm = T)),
  #                                 by = .(year, oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
  #                                        setback_scenario, prod_quota_scenario, excise_tax_scenario, type)]
  
  agg_well_entry[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
  agg_well_entry[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
  agg_well_entry[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  agg_well_entry[, setback_scenario := factor(setback_scenario, levels = c('setback_5280ft', 'setback_2500ft', 'setback_1000ft', 'no_setback'))]
  #rl
  agg_well_entry[, prod_quota_scenario := factor(prod_quota_scenario, levels = c('no quota', 'quota_10', 'quota_20', 'quota_40', 'quota_00'))]

# combine historic and forecasted production ------

  # historic (actual) production, remove gas fields
    name_df <- unique(entry_dt[, .(doc_field_code, doc_fieldname)])
  
    agg_prod_hist <- left_join(prod_hist, name_df) %>%
      mutate(gas = str_extract(doc_fieldname, "Gas")) %>%
      filter(is.na(gas),
             doc_field_code != "000") %>%
      group_by(year) %>%
      summarise(production_bbl = sum(total_bbls, na.rm = T)) %>%
      as.data.table()
  
  
  # agg_prod_hist = prod_hist[!doc_field_code == '000', .(production_bbl = sum(total_bbls, na.rm = T)),  by = .(doc_field_code, year)]
    # agg_prod_hist = prod_hist[!doc_field_code == '000', .(production_bbl = sum(total_bbls, na.rm = T)),  by = .(year)]
    agg_prod_hist[, type := 'historic production']
    agg_prod_hist[, year := as.numeric(as.character(year))]
    
  # set historic production for all scenarios
    
    scens <- expand.grid(oil_price_scenario = unique(scenarios_dt$oil_price_scenario),
                         innovation_scenario =  unique(scenarios_dt$innovation_scenario),
                         carbon_price_scenario = unique(scenarios_dt$carbon_price_scenario),
                         ccs_scenario = unique(scenarios_dt$ccs_scenario),
                         setback_scenario = unique(scenarios_dt$setback_scenario),
                         prod_quota_scenario = unique(scenarios_dt$prod_quota_scenario),
                         excise_tax_scenario = unique(scenarios_dt$excise_tax_scenario),
                         year = unique(agg_prod_hist$year))
    
    
    agg_prod_hist = agg_prod_hist[scens, on = .(year), allow.cartesian = T]
    agg_prod_hist = agg_prod_hist[, type := 'historic production']
    
  
    setcolorder(agg_prod_hist, c('oil_price_scenario', 'innovation_scenario', 
                                 'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 
                                 'year', 'type', 'production_bbl'))
    
  # # modeled production for existing wells
    # agg_pred_old = prod_existing_updated[, .(existing_wells_production = sum(production_bbl, na.rm = T)),  
    #                                      by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
    #                                             carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
    # agg_pred_old[, year := as.numeric(as.character(year))]
    
    agg_pred_old = outputs_plotting[vgroup == "existing"]
    agg_pred_old = agg_pred_old[, vgroup := NULL]
    agg_pred_old = agg_pred_old[, type := 'future production from existing wells']
    
    # agg_pred_old = agg_pred_old[, .(existing_wells_production = sum(production_bbl, na.rm = T)),  
    #                                      by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, 
    #                                             carbon_price_scenario, ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
    # agg_pred_old[, year := as.numeric(as.character(year))]

  # modeled production for new wells
    #rl
    # agg_pred_new = pred_prod[, .(new_wells_production = sum(production_bbl, na.rm = T)),
    #                          by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
    #                                 ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
    # agg_pred_new[, year := as.numeric(as.character(year))]
    
    # agg_pred_new = prod_new[, .(new_wells_production = sum(production_bbl, na.rm = T)),
    #                          by = .(doc_field_code, doc_fieldname, oil_price_scenario, innovation_scenario, carbon_price_scenario, 
    #                                 ccs_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario, year)]
    # agg_pred_new[, year := as.numeric(as.character(year))]
    
    
    agg_pred_new = outputs_plotting[vgroup == "new"]
    agg_pred_new = agg_pred_new[, vgroup := NULL]
    agg_pred_new = agg_pred_new[, type := 'future production from new wells']

  
# 
#     # combine all forecasted production
#     agg_pred_all = merge(agg_pred_new,
#                          agg_pred_old,
#                          by = c('doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 
#                                 'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'year'),
#                          all = T)
#     agg_pred_all[is.na(existing_wells_production), existing_wells_production := 0]
# 
#   # melt forecasted production from wide to long
#     agg_pred_all_long =  melt(agg_pred_all, measure.vars = c('existing_wells_production', 'new_wells_production'), variable.name = 'type', value.name = 'production_bbl')
#     agg_pred_all_long[, type := ifelse(type == 'existing_wells_production',
#                                        'future production from existing wells',
#                                        'future production from new wells')]
#     
#   # combine historic and forecasted production
#     agg_prod_field = rbindlist(list(agg_prod_hist, agg_pred_all_long), use.names = T, fill = T)
#     #rl
#     setorderv(agg_prod_field, c('doc_field_code', 'oil_price_scenario', 'innovation_scenario', 
#                                 'carbon_price_scenario', 'ccs_scenario', 'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'year'))
#   
#   # aggregate production to state level
#     #rl
#     agg_prod_state = agg_prod_field[, .(production_bbl = sum(production_bbl, na.rm = T)),
#                                     by = .(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario, 
#                                            setback_scenario, prod_quota_scenario, excise_tax_scenario, 
#                                            year, type)]
  
    agg_prod_state = rbind(agg_pred_new, agg_pred_old)
    
    
    ## aggregate ghg emissions
    agg_ghg_state <- agg_prod_state %>%
      group_by(oil_price_scenario, innovation_scenario, carbon_price_scenario, ccs_scenario,
               setback_scenario, prod_quota_scenario, excise_tax_scenario, year) %>%
      summarise(upstream_kgCO2e = sum(upstream_kgCO2e, na.rm = T),
                upstream_kgCO2e_inno_adj = sum(upstream_kgCO2e_inno_adj, na.rm = T),
                upstream_kgCO2e_inno_ccs_adj = sum(upstream_kgCO2e_inno_ccs_adj, na.rm = T),
                total_prod = sum(production_bbl, na.rm = T)) %>%
      ungroup() %>%
      mutate(ghg_mmt = upstream_kgCO2e / (1e3 * 1e6),
             ghg_mmt_inno_adj =  upstream_kgCO2e_inno_adj / (1e3 * 1e6),
             ghg_mmt_inno_ccs_adj =  upstream_kgCO2e_inno_ccs_adj / (1e3 * 1e6),
             prod_mbbl = total_prod / 1e6) %>%
      as.data.table()
    
  # # reorder factor levels
  #   agg_prod_field = agg_prod_field[, type := factor(type, levels = c('historic production', 'future production from new wells', 'future production from existing wells'))]
  # 
    agg_prod_state = agg_prod_state[, upstream_kgCO2e := NULL]
    agg_prod_state = agg_prod_state[, upstream_kgCO2e_inno_adj := NULL]
    agg_prod_state = agg_prod_state[, upstream_kgCO2e_inno_ccs_adj := NULL]
    agg_prod_state = rbind(agg_prod_state, agg_prod_hist)
    
    
    agg_prod_state[, type := factor(type, levels = c('historic production', 'future production from new wells', 'future production from existing wells'))]
    agg_prod_state[, innovation_scenario := factor(innovation_scenario, levels = c('low innovation', 'high innovation'))]
    agg_prod_state[, carbon_price_scenario := factor(carbon_price_scenario, levels = c('price floor', 'central SCC', 'price ceiling'))]
    agg_prod_state[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
    agg_prod_state[, setback_scenario := factor(setback_scenario, levels = c('setback_5280ft', 'setback_2500ft', 'setback_1000ft', 'no_setback'))]
    #rl
    agg_prod_state[, prod_quota_scenario := factor(prod_quota_scenario, levels = c('no quota', 'quota_10', 'quota_20', 'quota_40', 'quota_00'))]
    
    
# export to csvs -----
    
  # fwrite(pred_wells_long, file.path(save_path, 'well_entry_field_2020-2045.csv'), row.names = F)
  fwrite(agg_well_entry, file.path(save_path, 'well_entry_state_1977-2045.csv'), row.names = F)
  # fwrite(agg_pred_all_long, file.path(save_path, 'production_field_2020-2045.csv'), row.names = F)
  fwrite(agg_prod_state, file.path(save_path, 'production_state_1977-2045.csv'), row.names = F)
  

  
  
# ---------------------------------------------  PLOTS ---------------------------------------------  
  
# 
#   scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, 
#                                         carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])

  
  ## first plot at vintage level to make sure rules are working (ie vintages shut off)
  
  # for (i in 1:nrow(scen_combos)) {
  # 
  #   # scen = unique(scenarios_dt[, oil_price_scenario])[i]
  #   scen = scen_combos[i]
  #   scen_title = paste(scen, collapse = " + ")
  #   scen_title = gsub('_', ' ', scen_title)
  #   scen_name = gsub(' ', '_', scen)
  #   scen_dir = paste(scen_name, collapse = "_")
  # 
  #   sname = paste0(scen_dir, '_outputs.pdf')
  # 
  #   field_sub <- c("052", "464", "340", "190", "849", "432", "644", "228", "150", "566")
  # 
  #   new_wells_dt = adj_well_entry[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
  #                                    carbon_price_scenario, ccs_scenario, innovation_scenario), nomatch = 0] %>%
  #     as.data.table()
  # 
  #   new_wells_dt2 <- new_wells_dt %>%
  # 
  #     filter(doc_field_code %in% field_sub) %>%
  #     as.data.table()
  # 
  #   scen_prod = all_prod2[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
  #                                      carbon_price_scenario, ccs_scenario, innovation_scenario), nomatch = 0] %>%
  #     as.data.table()
  # 
  #   scen_prod2 <- scen_prod %>%
  #     filter(doc_field_code %in% field_sub) %>%
  #     as.data.frame()
  # 
  #   ## plot -- filter for 10ish fields, facet by field
  # 
  #   ggplot(scen_prod2, aes(x = year, y = production_bbl / 1e6 , color = as.character(vintage_start))) +
  #     geom_line() +
  #     facet_wrap(~doc_fieldname) +
  #     labs(title = paste0(scen_title))
  #   
  #   test_prod <- setDT(test_prod)
  #   
  #   test_prod_filt <- test_prod[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
  #                                                                carbon_price_scenario, ccs_scenario, innovation_scenario), nomatch = 0] %>%
  #     as.data.table()
  #   
  #   ggplot(test_prod_filt, aes(x = year, y = total_prod_m, fill = type)) + 
  #     geom_area() +
  #     labs(title = paste0(scen_title))
  # 
  # }

  
  
  # theme ------
  
    pal_type = c("historic production" = "#34495e",
                 "future production from existing wells" = "#95a5a6",
                 "future production from new wells" = "#e74c3c",
                 "historic actual" = "#34495e",
                 "historic modeled" = "#3498db",
                 "forecasted" = "#e74c3c",
                 "weighted mean forecast" = "#e30011",
                 "non-weighted mean forecast" = "#198a2f",
                 "brent" = "#95a5a6")
    
  ## choose your font
    # font_fam <- "Secca Soft"
    font_fam <- "Arial"

    theme_line = theme_ipsum(base_family = font_fam,
                             grid = 'Y', 
                             plot_title_size = 20, 
                             subtitle_size = 18,
                             axis_title_just = 'center',
                             axis_title_size = 18, 
                             axis_text_size = 16,
                             strip_text_size = 16)  +
      theme(plot.title = element_text(hjust = 0, face = 'bold'),
            plot.title.position = 'plot',
            plot.subtitle = element_text(hjust = 0),
            plot.caption = element_text(size = 10, color = '#5c5c5c', face = 'plain'),
            axis.text.x = element_text(margin = margin(t = .3, unit = "cm")),
            axis.line.x = element_line(color = 'black'),
            axis.ticks.x = element_line(color = 'black'),
            axis.ticks.length.x = unit(0.25, "cm"),
            axis.text.y = element_text(margin = margin(r = .3, unit = "cm")),
            plot.margin = unit(c(1,2,1,1), "lines"),
            strip.text = element_text(hjust = 0.5),
            legend.text = element_text(size = 16),
            legend.position = 'bottom')
  
  
    
  # plot: line - well entry vs oil price (by oil price, setback, and quota scenarios) ------
    
    ## test
    scen_combos = unique(agg_prod_state[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)])
    
    
    # scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)])
    
    # scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
    #                                       carbon_price_scenario, ccs_scenario, innovation_scenario)])
    
    for (i in 1:nrow(scen_combos)) {
      
      # scen = unique(scenarios_dt[, oil_price_scenario])[i]
      scen = scen_combos[i]
      scen_title = paste(unlist(t(scen)),collapse=" + ")
      scen_title = gsub('_', ' ', scen_title)
      scen_name = paste(unlist(t(scen)), collapse = '_')
      scen_name = gsub(' ', '_', scen_name)
      scen_dir = paste(scen_name, collapse = "_")
      #rl
      fname = paste0(scen_dir, '_line_brent_vs_new_wells_1978_2045.pdf')
      
      dt = agg_well_entry[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario), nomatch = 0]
      dt = dt[year > 1977]
      # dt = dt[! type == 'non-weighted mean forecast']
      
      max_wells = signif(max(dt[, new_wells_pred], na.rm = T), 1)
      max_price = signif(max(dt[, oil_price_usd_per_bbl], na.rm = T), 1)
      br_wells = seq(0, max_wells, length.out = 5)
      br_price = seq(0, max_price, length.out = 5)
      
      line_brent_wells = ggplot(dt) + 
        geom_line(aes(x = year, y = new_wells_pred, color = type), size = 0.7) +
        geom_line(aes(x = year, y = oil_price_usd_per_bbl*(max_wells/max_price), color = 'brent'), linetype = 2, size = 0.7) +
        labs(title = paste0('New wells entered in ', scen_title, ' scenario'),
             # title = paste0('New wells entered each year (1978-2045) in ', scen, ' scenario'),
             subtitle = 'Number of wells', 
             x = NULL,
             y = NULL,
             color = NULL) +
        facet_grid(innovation_scenario + ccs_scenario ~ carbon_price_scenario,
                   labeller = labeller(carbon_price_scenario = c('price floor' = 'low carbon price',
                                                                 'central SCC' = 'medium carbon price',
                                                                 'price ceiling' = 'high carbon price'))) +
        scale_x_continuous(breaks = seq(1980,2045,15), limits = c(1978, 2045), expand = c(0,0)) +
        scale_y_continuous(labels = scales::comma, breaks = br_wells, 
                           limits = c(0, 1.15*max_wells), expand = c(0,0),
                           sec.axis = sec_axis(trans=~./(max_wells/max_price), breaks = br_price)) +
        scale_color_manual(breaks = c(levels(factor(agg_well_entry[, type])), 'brent'),
                           values = pal_type,
                           labels = c(levels(factor(agg_well_entry[, type])), scen)) +
        annotate("text", label = '# of wells', x = 1995, y = 1.08*max_wells,
                 size = 5, fontface = 'plain', family = font_fam, hjust = 1) +
        annotate("text", label = '$/bbl', x = 2040, y = 1.08*max_wells, 
                 size = 5, fontface = 'plain', family = font_fam, hjust = 0.5) +
        guides(color = guide_legend(nrow = 1, title = NULL),
               linetype = guide_legend(title = NULL)) +
        geom_segment(x = 2019, xend = 2019, y = 0, yend = Inf, color = 'black', linetype = 2)  +
        theme_line
      
      line_brent_wells = ggplotGrob(line_brent_wells)
      line_brent_wells$layout$clip[line_brent_wells$layout$name == "panel"] = "off"
      
      ggsave(line_brent_wells, 
             filename = file.path(save_path, 'line_brent_vs_new_wells', fname), 
             width = 12, 
             height = 14.5)
      
      embed_fonts(file.path(save_path, 'line_brent_vs_new_wells', fname),
                  outfile = file.path(save_path, 'line_brent_vs_new_wells', fname))
      
      rm(scen, scen_dir, fname, max_wells, max_price, br_wells, br_price, line_brent_wells)
      
      
    }
    
  
    ### ghg emissions
    ## ------------------------------------------------------------------------------
    
    # scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)])
    
    # scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
    #                                       carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])
    
    scen_combos = unique(agg_prod_state[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                          carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario)])
    
    for (i in 1:nrow(scen_combos)) {
      
      # scen = unique(scenarios_dt[, oil_price_scenario])[i]
      scen = scen_combos[i]
      scen_title = paste(unlist(t(scen)),collapse=" + ")
      scen_title = gsub('_', ' ', scen_title)
      scen_name = paste(unlist(t(scen)), collapse = '_')
      scen_name = gsub(' ', '_', scen_name)
      scen_dir = paste(scen_name, collapse = "_")
      #rl
      fname = paste0(scen_dir, '_line_ghg_2020_2045.pdf')
      
      dt = agg_ghg_state[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                      carbon_price_scenario, ccs_scenario, innovation_scenario, excise_tax_scenario), nomatch = 0]

      dt2 <- dt %>%
        pivot_longer(ghg_mmt:ghg_mmt_inno_ccs_adj, names_to = "type", values_to = "mmt_CO2e") %>%
        mutate(type = ifelse(type == "ghg_mmt", "no adjustment",
                             ifelse(type == "ghg_mmt_inno_adj", "innovation adjustment", "innovation and ccs adjustments"))) 
        
      ghg_lines = ggplot(dt2) + 
        geom_line(aes(x = year, y = mmt_CO2e, color = type), alpha = 0.8, size = 1) +
        labs(title = paste0("GHGe emissions: ", scen_title),
             # title = paste0('New wells entered each year (1978-2045) in ', scen, ' scenario'),
             # subtitle = 'Number of wells', 
             y = "MtCO2e",
             x = NULL,
             color = NULL) +
        theme_line
      
      # line_brent_wells = ggplotGrob(line_brent_wells)
      # line_brent_wells$layout$clip[line_brent_wells$layout$name == "panel"] = "off"
      
      ggsave(ghg_lines, 
             filename = file.path(save_path, '/ghg/', fname), 
             width = 14.5, 
             height = 12)
      
      embed_fonts(file.path(save_path, 'ghg', fname),
                  outfile = file.path(save_path, 'ghg', fname))
      
      rm(scen, scen_dir, fname, dt, dt2, ghg_lines)
      
      
    }
    
    
      
  # plot: area - crude production (by oil price, setback, and quota scenarios) ------
    
    # might need to debug full plot script
    
    scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)])

    scen_combos = unique(agg_prod_state[, .(oil_price_scenario, setback_scenario, prod_quota_scenario,
                                             excise_tax_scenario)])
    
    for (i in 1:nrow(scen_combos)) {
      
      # scen = unique(scenarios_dt[, oil_price_scenario])[i]
      # scen_dir = gsub(' ', '_', scen)
      scen = scen_combos[i]
      scen_title = paste(unlist(t(scen)),collapse=" + ")
      scen_title = gsub('_', ' ', scen_title)
      scen_name = paste(unlist(t(scen)), collapse = '_')
      scen_name = gsub(' ', '_', scen_name)
      scen_dir = paste(scen_name, collapse = "_")
      
      fname = paste0(scen_dir, '_area_annual_production_1977_2045.pdf')
      
      dt = agg_prod_state[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario), nomatch = 0]
      
      area_annual = ggplot(dt, aes(x = year, y = production_bbl/1e6, fill = type, group = type)) + 
        geom_area() +
        facet_grid(innovation_scenario + ccs_scenario ~ carbon_price_scenario,
                   labeller = labeller(carbon_price_scenario = c('price floor' = 'low carbon price',
                                                                 'central SCC' = 'medium carbon price',
                                                                 'price ceiling' = 'high carbon price'))) +
        labs(title = paste0('Annual oil production in ', scen_title, ' scenario'),
             subtitle = 'Million barrels', 
             x = NULL,
             y = NULL,
             fill = NULL) +
        scale_x_continuous(breaks = seq(1980,2045,20), limits = c(1977, 2045), expand = c(0,0)) +
        scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,400,100)) +
        scale_fill_manual(values = pal_type) + 
        geom_segment(x = 2019, xend = 2019, y = 0, yend = 400, color = 'black', linetype = 2)  +
        theme_line
      
      ggsave(area_annual, 
             filename = file.path(save_path, 'area_production', fname), 
             width = 12, 
             height = 14.5)
      
      embed_fonts(file.path(save_path, 'area_production', fname),
                  outfile = file.path(save_path, 'area_production', fname))
      
      rm(scen, scen_dir, fname, area_annual)
      
      
    }
  
    
    
  # # plot: line - well entry vs oil price ------
  # 
  #   scen_combos = unique(scenarios_dt[, .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario)])
  #   
  #   
  #   for (i in 1:nrow(scen_combos)) {
  #     
  #     scen = scen_combos[i]
  #     scen_title = paste(unlist(t(scen)),collapse=" + ")
  #     scen_title = gsub('_', ' ', scen_title)
  #     scen_name = paste(unlist(t(scen)), collapse = '_')
  #     scen_name = gsub(' ', '_', scen_name)
  #     oil_scen <- scen[, oil_price_scenario][1]
  #     # scen_dir = paste(scen_name, collapse = "_")
  #     # scen_dir = gsub(' ', '_', scen)
  #     
  #     #rl
  #     fname = paste0(scen_name, '_line_brent_vs_new_wells_1978_2045.pdf')
  #     
  #     max_wells = signif(max(agg_well_entry[year > 1977 & oil_price_scenario == oil_scen, new_wells_pred]), 1)
  #     max_price = signif(max(agg_well_entry[year > 1977 & oil_price_scenario == oil_scen, oil_price_usd_per_bbl]), 1)
  #     br_wells = seq(0, max_wells, length.out = 5)
  #     br_price = seq(0, max_price, length.out = 5)
  #     
  #     agg_well_entry_filt <- agg_well_entry[scen, on = .(oil_price_scenario, setback_scenario, prod_quota_scenario, excise_tax_scenario), nomatch = 0]
  #     agg_well_entry_filt <- agg_well_entry_filt[year > 1977]
  #     
  #     
  #     line_brent_wells = 
  #       # ggplot(agg_well_entry[year > 1977  & ! type == 'non-weighted mean forecast' & oil_price_scenario == scen]) + 
  #       ggplot(agg_well_entry_filt) + 
  #       geom_line(aes(x = year, y = new_wells_pred, color = type), size = 0.7) +
  #       geom_line(aes(x = year, y = oil_price_usd_per_bbl*(max_wells/max_price), color = 'brent'), linetype = 2, size = 0.7) +
  #       labs(title = paste0('New wells entered each year (1978-2045) in ', scen, ' scenario'),
  #            subtitle = paste0(scen_title, '\nNumber of wells'), 
  #            x = NULL,
  #            y = NULL,
  #            color = NULL) +
  #       facet_grid(innovation_scenario + ccs_scenario ~ carbon_price_scenario,
  #                  labeller = labeller(carbon_price_scenario = c('price floor' = 'low carbon price',
  #                                                                'central SCC' = 'medium carbon price',
  #                                                                'price ceiling' = 'high carbon price'))) +
  #       scale_x_continuous(breaks = seq(1980,2045,15), limits = c(1978, 2045), expand = c(0,0)) +
  #       scale_y_continuous(labels = scales::comma, breaks = br_wells, 
  #                          limits = c(0, 1.15*max_wells), expand = c(0,0),
  #                          sec.axis = sec_axis(trans=~./(max_wells/max_price), breaks = br_price)) +
  #       scale_color_manual(breaks = c(levels(factor(agg_well_entry[, type])), 'brent'),
  #                          values = pal_type,
  #                          labels = c(levels(factor(agg_well_entry[, type])), scen)) +
  #       annotate("text", label = '# of wells', x = 1995, y = 1.08*max_wells,
  #                size = 5, fontface = 'plain', family = font_fam, hjust = 1) +
  #       annotate("text", label = '$/bbl', x = 2040, y = 1.08*max_wells, 
  #                size = 5, fontface = 'plain', family = font_fam, hjust = 0.5) +
  #       guides(color = guide_legend(nrow = 1, title = NULL),
  #              linetype = guide_legend(title = NULL)) +
  #       geom_segment(x = 2019, xend = 2019, y = 0, yend = Inf, color = 'black', linetype = 2)  +
  #       theme_line
  #     
  #     line_brent_wells = ggplotGrob(line_brent_wells)
  #     line_brent_wells$layout$clip[line_brent_wells$layout$name == "panel"] = "off"
  #     
  #     ggsave(line_brent_wells, 
  #            filename = file.path(save_path, 'line_figs',  fname), 
  #            width = 12, 
  #            height = 14.5)
  #     
  #     embed_fonts(file.path(save_path, fname),
  #                 outfile = file.path(save_path, 'line_figs', fname))
  #     
  #     rm(scen, scen_dir, fname, max_wells, max_price, br_wells, br_price, line_brent_wells)
  #     
  #     
  #   }

  # # plot: area - crude production ------
  # 
  #   for (i in seq_along(unique(scenarios_dt[, oil_price_scenario]))) {
  #     
  #     scen = unique(scenarios_dt[, oil_price_scenario])[i]
  #     scen_dir = gsub(' ', '_', scen)
  #     fname = paste0(scen_dir, '_area_annual_production_1977_2045.pdf')
  # 
  #     area_annual = ggplot(agg_prod_state[oil_price_scenario == scen], aes(x = year, y = production_bbl/1e6, fill = type)) + 
  #       geom_area() +
  #       facet_grid(innovation_scenario + ccs_scenario + setback_scenario ~ carbon_price_scenario,
  #                  labeller = labeller(carbon_price_scenario = c('price floor' = 'low carbon price',
  #                                                                'central SCC' = 'medium carbon price',
  #                                                                'price ceiling' = 'high carbon price'))) +
  #       labs(title = paste0('Annual oil production (1977-2045) in ', scen, ' scenario'),
  #            subtitle = 'Million barrels', 
  #            x = NULL,
  #            y = NULL,
  #            fill = NULL) +
  #       scale_x_continuous(breaks = seq(1980,2045,20), limits = c(1977, 2045), expand = c(0,0)) +
  #       scale_y_continuous(labels = scales::comma, expand = c(0,0), breaks = seq(0,400,100)) +
  #       scale_fill_manual(values = pal_type) + 
  #       geom_segment(x = 2019, xend = 2019, y = 0, yend = 400, color = 'black', linetype = 2)  +
  #       theme_line
  #     
  #     ggsave(area_annual, 
  #            filename = file.path(save_path, fname), 
  #            width = 12, 
  #            height = 14.5)
  #     
  #     embed_fonts(file.path(save_path, fname),
  #                 outfile = file.path(save_path, fname))
  #     
  #     rm(scen, scen_dir, fname, area_annual)
  #     
  #     
  #   }
  # 
  # 

  # # plot: line - well entry ------
  #   
  #   for (i in seq_along(unique(scenarios_dt[, oil_price_scenario]))) {
  #     
  #     scen = unique(scenarios_dt[, oil_price_scenario])[i]
  #     scen_dir = gsub(' ', '_', scen)
  #     fname = paste0(scen_dir, '_line_new_wells_1978_2045.pdf')
  #     
  #     line_wells = ggplot(agg_well_entry[year > 1977 & oil_price_scenario == scen], 
  #                         aes(x = year, y = new_wells_pred, color = type)) + geom_line(size = 1) +
  #       labs(title = paste0('New wells entered each year (1978-2045) in ', scen, ' scenario'),
  #            subtitle = 'Number of wells', 
  #            x = NULL,
  #            y = NULL,
  #            color = NULL) +
  #       facet_grid(innovation_scenario + ccs_scenario + setback_sscenario ~ carbon_price_scenario) +
  #       scale_x_continuous(breaks = seq(1980,2045,15), limits = c(1978, 2045), expand = c(0,0)) +
  #       scale_y_continuous(labels = scales::comma) +
  #       scale_color_manual(values = pal_type) + 
  #       geom_segment(x = 2019, xend = 2019, y = 0, yend = Inf, color = 'black', linetype = 2)  +
  #       theme_line
  #     
  #     ggsave(line_wells, 
  #            filename = file.path(save_path, fname), 
  #            width = 12, 
  #            height = 14.5)
  #     
  #     embed_fonts(file.path(save_path, fname),
  #                 outfile = file.path(save_path, fname))
  #     
  #     rm(scen, scen_dir, fname)
  #     
  #     
  #   }
  #   
    

    end_time <- Sys.time()

    print(end_time - start_time)