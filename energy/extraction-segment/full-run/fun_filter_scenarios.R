
filter_run_scens = function(scenario_selection, scens) {
  
  
  sel_scenarios_dt <- scens
  
  ## filter for tax scenarios
  
  if (scenario_selection == 'tax_scens') {

    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           # excise_tax_scenario == 'no tax' & ## all tax
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                          (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           # setback_scenario == 'no_setback' & ## all setback
                                           prod_quota_scenario == 'no quota')]
  }

  
  
  # keep diagnostics only (if that is input) ------

  if (scenario_selection == 'diagnostic') {

    sel_scenarios_dt = sel_scenarios_dt[(oil_price_scenario == 'reference case' &
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

  ## filter for benchmark scenarios
  
  if (scenario_selection == 'benchmark') {

    sel_scenarios_dt = sel_scenarios_dt[(innovation_scenario == 'low innovation' &
                                         carbon_price_scenario == 'price floor' &
                                         ccs_scenario == 'medium CCS cost' &
                                         excise_tax_scenario == 'no tax' &
                                         setback_scenario == 'no_setback' &
                                         prod_quota_scenario == 'no quota') | ## all oil scenarios, hold everything else BAU
                                        (oil_price_scenario == 'reference case' &
                                           # innovation_scenario == 'low innovation' &  ## all innovation scenarios, everything else BAU
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           # carbon_price_scenario == 'price floor' & ## all carbon scenarios, everything else BAU
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'central SCC' &
                                           # ccs_scenario == 'medium CCS cost' & ## all CCS
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback' &
                                           prod_quota_scenario == 'no quota') |
                                      (oil_price_scenario == 'reference case' &
                                          innovation_scenario == 'low innovation' &
                                          carbon_price_scenario == 'price floor' &
                                          ccs_scenario == 'medium CCS cost' &
                                         # excise_tax_scenario == 'no tax' & ## all tax
                                          setback_scenario == 'no_setback' &
                                          prod_quota_scenario == 'no quota')  |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           # setback_scenario == 'setback_2500ft' & ## all setback
                                           prod_quota_scenario == 'no quota') |
                                        (oil_price_scenario == 'reference case' &
                                           innovation_scenario == 'low innovation' &
                                           carbon_price_scenario == 'price floor' &
                                           ccs_scenario == 'medium CCS cost' &
                                           excise_tax_scenario == 'no tax' &
                                           setback_scenario == 'no_setback') ] ## all quota
  }


  
    if (scenario_selection == 'full_run') {
      
      sel_scenarios_dt = sel_scenarios_dt[prod_quota_scenario == 'no quota']
    }
  
    
    if (scenario_selection == 'full_run_subset') {
      
      carbon_subset_vec <- c("price floor", "price ceiling", "central SCC")
      carbon_scens_vec <- c("carbon_setback_1000ft", "carbon_setback_5280ft", "carbon_90_perc_reduction", "central SCC")
      ccs_subset_vec <- c("no ccs", "medium CCS cost", "high CCS cost", "medium CCS cost - 45Q - LCFS", "high CCS cost - 45Q - LCFS")
      tax_subset_vec <- c("tax_setback_1000ft", "tax_setback_2500ft", "tax_setback_5280ft", "tax_90_perc_reduction")
      
      setback_dt = sel_scenarios_dt[prod_quota_scenario == 'no quota']
      setback_dt = setback_dt[(innovation_scenario == 'low innovation' &
                                carbon_price_scenario %in% carbon_subset_vec &
                                ccs_scenario == 'medium CCS cost' & 
                                excise_tax_scenario == 'no tax' &
                                prod_quota_scenario == 'no quota' &
                                oil_price_scenario == 'reference case') |
                               (innovation_scenario == 'low innovation' &
                                carbon_price_scenario == 'price floor' &
                                ccs_scenario %in% ccs_subset_vec & 
                                excise_tax_scenario == 'no tax' &
                                prod_quota_scenario == 'no quota' &
                                oil_price_scenario == 'reference case') |
                               (innovation_scenario == 'low innovation' &
                                carbon_price_scenario == 'price floor' &
                                ccs_scenario == "medium CCS cost" & 
                                excise_tax_scenario == 'no tax' &
                                prod_quota_scenario == 'no quota')]
      
      tax_dt = sel_scenarios_dt[prod_quota_scenario == 'no quota']
      tax_dt = tax_dt[(innovation_scenario == 'low innovation' &
                       carbon_price_scenario %in% carbon_subset_vec &
                       ccs_scenario == 'medium CCS cost' & 
                       excise_tax_scenario %in% tax_subset_vec &
                       prod_quota_scenario == 'no quota' &
                       oil_price_scenario == 'reference case' &
                       setback_scenario == 'no_setback') |
                      (innovation_scenario == 'low innovation' &
                       carbon_price_scenario == 'price floor' &
                       ccs_scenario %in% ccs_subset_vec & 
                       excise_tax_scenario %in% tax_subset_vec &
                       prod_quota_scenario == 'no quota' &
                       oil_price_scenario == 'reference case' &
                       setback_scenario == 'no_setback') |
                      (innovation_scenario == 'low innovation' &
                       carbon_price_scenario == 'price floor' &
                       ccs_scenario == "medium CCS cost" & 
                       excise_tax_scenario %in% tax_subset_vec &
                       prod_quota_scenario == 'no quota' &
                       setback_scenario == 'no_setback')]
      
      carbon_dt = sel_scenarios_dt[prod_quota_scenario == 'no quota']
      carbon_dt = carbon_dt[(innovation_scenario == 'low innovation' &
                             carbon_price_scenario %in% carbon_scens_vec &
                             ccs_scenario == 'medium CCS cost' & 
                             excise_tax_scenario == "no tax" &
                             prod_quota_scenario == 'no quota' &
                             setback_scenario == 'no_setback') |
                            (innovation_scenario == 'low innovation' &
                             carbon_price_scenario %in% carbon_scens_vec &
                             ccs_scenario %in% ccs_subset_vec & 
                             excise_tax_scenario == "no tax" &
                             prod_quota_scenario == 'no quota' &
                             oil_price_scenario == 'reference case' &
                             setback_scenario == 'no_setback')]
      
      
      ## bind
      sel_scenarios_dt = rbind(setback_dt, tax_dt, carbon_dt)
      sel_scenarios_dt = distinct(sel_scenarios_dt)
      
    }
  
  
  
  return(sel_scenarios_dt)
  
  
  
}