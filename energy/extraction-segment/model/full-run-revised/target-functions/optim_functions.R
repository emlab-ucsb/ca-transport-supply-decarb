## Tracey Mangin
## January 31, 2022
## optimization functions

## function to calculate the excise tax that yields target 2045 ghg emissions 
## --------------------------------------------------------------------------  

find_excise_tax <- function(scen_z) {
  
  # scen_z
  scen_id_z <- scen_z[, scen_id][1]
  target_z <- scen_z[, target][1]

  ## find target
  ## if target is not 90 percent reduction, calculate target 2045 emission
   if(target_z != "90perc_reduction") {
     
     target_scen <- scen_z %>%
       select(oil_price_scenario:excise_tax_scenario, setback_existing) %>%
       mutate(excise_tax_scenario = "no tax",
              setback_scenario = target_z)
     
     ## create input sheet
     target_scen_dt_z = target_scen[oilpx_scens, on = .(oil_price_scenario), allow.cartesian = T, nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[vars_dt, on = .(year), allow.cartesian = T, nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[innovation_scens, on = .(year, innovation_scenario), nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[carbonpx_scens, on = .(year, carbon_price_scenario), nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[ccs_scens_all, on = .(year, ccs_scenario), nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[setback_scens, on = .(doc_field_code, setback_scenario, setback_existing), nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[prod_quota_scens, on = .(year, prod_quota_scenario), nomatch = 0]
     target_scen_dt_z = target_scen_dt_z[excise_tax_scens, on = .(year, excise_tax_scenario), nomatch = 0]
     
     ## compute tax
     target_scen_dt_z[, tax := tax_rate * oil_price_usd_per_bbl]
     target_scen_dt_z[, tax_rate := NULL]
     
     ## set order
     setcolorder(target_scen_dt_z, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                   'setback_scenario', 'setback_existing', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                                   'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg', 'orig_area_m2', 'scen_area_m2', 'area_coverage', 'n_wells_start', 'n_wells_setback', 
                                   'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 
                                   'wm_capex_imputed', 'resource',  'steam_field', 'upstream_kgCO2e_bbl'))
     
     ## run function to calc target ghg emissions
     target_ghg <- calc_2045_ghg(inputs = target_scen_dt_z, scen = target_scen)
    
   } else {target_ghg <- emis_reduc_90_val} ## otherwise target = 90% reduction val
  
  
  # Define objection function that changes based on the target value
  # The objective function seeks to minimize the difference between the target
  # value and the value achieved at a given tax
  
  obj_fun_excise <- function(excise_tax_val){

    ghg_actual <- calc_excise_ghg(excise_tax_val = excise_tax_val, scen_z = scen_z)
    diff_abs <- target_ghg - ghg_actual
    diff_use <- ifelse(ghg_actual > target_ghg, 1000, diff_abs)

    return(diff_use)

  }

  
  
  ## Solve problem
  fit <- optim(par = 0.1,
               fn = obj_fun_excise,
               method = "Brent", # suggested for one dimension optimiation
               # method = "L-BFGS-B",
               lower = 0, # set based on tax knowledge
               upper = 2, # set based on tax knowledge
               control = list(maxit = 10000)) 
  
  # Extract tax
  tax_est <- fit$par
  
  tax_est_target <- tibble(target_val = target_ghg,
                           tax_est_val = tax_est) %>%
    as.data.table()
  
  
  return(tax_est_target)
  
}


find_carbonpx_start <- function(scen_z) {
  
  # scen_z
  scen_id_z <- scen_z[, scen_id][1]
  target_z <- scen_z[, target][1]
  
  ## find target
  ## if target is not 90 percent reduction, calculate target 2045 emission
  if(target_z != "90perc_reduction") {
    
    target_scen <- scen_z %>%
      select(oil_price_scenario:excise_tax_scenario, setback_existing) %>%
      mutate(carbon_price_scenario = "price floor",
             setback_scenario = target_z)
    
    ## create input sheet
    target_scen_dt_z = target_scen[oilpx_scens, on = .(oil_price_scenario), allow.cartesian = T, nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[vars_dt, on = .(year), allow.cartesian = T, nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[innovation_scens, on = .(year, innovation_scenario), nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[carbonpx_scens, on = .(year, carbon_price_scenario), nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[ccs_scens_all, on = .(year, ccs_scenario), nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[setback_scens, on = .(doc_field_code, setback_scenario, setback_existing), nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[prod_quota_scens, on = .(year, prod_quota_scenario), nomatch = 0]
    target_scen_dt_z = target_scen_dt_z[excise_tax_scens, on = .(year, excise_tax_scenario), nomatch = 0]
    
    ## compute tax
    target_scen_dt_z[, tax := tax_rate * oil_price_usd_per_bbl]
    target_scen_dt_z[, tax_rate:= NULL]
    
    ## set order
    setcolorder(target_scen_dt_z, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                    'setback_scenario', 'setback_existing', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                                    'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg', 'orig_area_m2', 'scen_area_m2', 'area_coverage', 'n_wells_start', 'n_wells_setback', 
                                    'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 
                                    'wm_capex_imputed', 'resource',  'steam_field', 'upstream_kgCO2e_bbl'))
    
    ## run function to calc target ghg emissions
    target_ghg <- calc_2045_ghg(inputs = target_scen_dt_z, scen = target_scen)
    
  } else {target_ghg <- emis_reduc_90_val} ## otherwise target = 90% reduction val
  
  
  # Define objection function that changes based on the target value
  # The objective function seeks to minimize the difference between the target
  # value and the value achieved at a given tax
  
  obj_fun_carbonpx <- function(carbonpx_val){
    
    ghg_actual <- calc_carbonpx_ghg(carbonpx_val = carbonpx_val, scen_z = scen_z)
    diff_abs <- target_ghg - ghg_actual
    diff_use <- ifelse(ghg_actual > target_ghg, 1000, diff_abs)
    
    return(diff_use)
    
  }
  
  ## Solve problem
  fit <- optim(par = 1,
               fn = obj_fun_carbonpx,
               method = "Brent", # suggested for one dimension optimization
               lower = 0, # set based on tax knowledge
               upper = 5000, # set based on tax knowledge
               control = list(maxit = 10000)) 
  
  # Extract tax
  carbonpx_est <- fit$par
  
  carbonpx_est_target <- tibble(target_val = target_ghg,
                                carbonpx_est_val = carbonpx_est) %>%
    as.data.table()
  
  return(carbonpx_est_target)
  
}




  
  