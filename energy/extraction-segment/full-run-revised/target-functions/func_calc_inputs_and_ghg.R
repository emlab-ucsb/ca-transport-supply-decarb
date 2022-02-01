## Tracey Mangin
## February 1, 2022
## Functions for prepping input values

calc_excise_ghg <- function(excise_tax_val, scen_z) {
  
  ## create input df
  excise_input_z <- scen_z %>%
    select(oil_price_scenario:excise_tax_scenario)
  
  ## create input sheet
  excise_input_z = excise_input_z[oilpx_scens, on = .(oil_price_scenario), allow.cartesian = T, nomatch = 0]
  excise_input_z = excise_input_z[vars_dt, on = .(year), allow.cartesian = T, nomatch = 0]
  excise_input_z = excise_input_z[innovation_scens, on = .(year, innovation_scenario), nomatch = 0]
  excise_input_z = excise_input_z[ccs_scens_all, on = .(year, ccs_scenario), nomatch = 0]
  excise_input_z = excise_input_z[setback_scens, on = .(doc_field_code, setback_scenario), nomatch = 0]
  excise_input_z = excise_input_z[prod_quota_scens, on = .(year, prod_quota_scenario), nomatch = 0]
  excise_input_z = excise_input_z[carbonpx_scens, on = .(year, carbon_price_scenario), nomatch = 0]
  
  ## add tax val to input
  excise_input_z[, tax_rate := excise_tax_val]
  
  ## compute tax
  excise_input_z[, tax := tax_rate * oil_price_usd_per_bbl]
  excise_input_z[, tax_rate:= NULL]
  
  ## set order
  setcolorder(excise_input_z, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                  'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                                  'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg', 'orig_area_m2', 'scen_area_m2', 'area_coverage', 'n_wells_start', 'n_wells_setback', 
                                  'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 
                                  'wm_capex_imputed', 'resource',  'steam_field', 'upstream_kgCO2e_bbl'))
  
  
  ## run the model to get 2045 ghg
  excise_2045_ghg <- calc_2045_ghg(inputs = excise_input_z, scen = scen_z)
  
  return(excise_2045_ghg)
  
  } 
  
calc_carbonpx_ghg <- function(carbonpx_val, scen_z) {
  
  ## create input df
  carbonpx_input_z <- scen_z %>%
    select(oil_price_scenario:excise_tax_scenario)
    
  ## create input sheet
  carbonpx_input_z = carbonpx_input_z[oilpx_scens, on = .(oil_price_scenario), allow.cartesian = T, nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[vars_dt, on = .(year), allow.cartesian = T, nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[innovation_scens, on = .(year, innovation_scenario), nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[ccs_scens_all, on = .(year, ccs_scenario), nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[setback_scens, on = .(doc_field_code, setback_scenario), nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[prod_quota_scens, on = .(year, prod_quota_scenario), nomatch = 0]
  carbonpx_input_z = carbonpx_input_z[excise_tax_scens, on = .(year, excise_tax_scenario), nomatch = 0]
    
 ## compute tax
  carbonpx_input_z[, tax := tax_rate * oil_price_usd_per_bbl]
  carbonpx_input_z[, tax_rate:= NULL]
    
 ## create carbon price stream
 ## ---------------------------
    
    carbonpx_stream <- tibble(year = c(2020:2045)) %>%
      mutate(carbon_price = carbonpx_val,
             tval = row_number() - 1) %>%
      fill(carbon_price) %>%
      mutate(carbon_price = ifelse(tval == 0, carbon_price, calculate_carbonpx_val(x0 = carbon_price, r = perc_inc, t = tval)),
             carbon_price_usd_per_kg = carbon_price / 1000) %>%
      select(year, carbon_price_usd_per_kg) %>%
    as.data.table()
    
    
    carbonpx_input_z = carbonpx_input_z[carbonpx_stream, on = .(year), nomatch = 0]
    
    ## set order
    setcolorder(carbonpx_input_z, c('year', 'doc_field_code', 'doc_fieldname', 'oil_price_scenario', 'innovation_scenario', 'carbon_price_scenario', 'ccs_scenario',
                                  'setback_scenario', 'prod_quota_scenario', 'excise_tax_scenario', 'oil_price_usd_per_bbl', 'innovation_multiplier', 
                                  'carbon_price_usd_per_kg', 'ccs_price_usd_per_kg', 'orig_area_m2', 'scen_area_m2', 'area_coverage', 'n_wells_start', 'n_wells_setback', 
                                  'quota', 'tax', 'm_opex_imputed', 'm_capex_imputed', 'wm_opex_imputed', 
                                  'wm_capex_imputed', 'resource',  'steam_field', 'upstream_kgCO2e_bbl'))
    
    ## run the model to get 2045 ghg
    carbonpx_2045_ghg <- calc_2045_ghg(inputs = carbonpx_input_z, scen = scen_z)
    
    return(carbonpx_2045_ghg)
    
  }
  
  