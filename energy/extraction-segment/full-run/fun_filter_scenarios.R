
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
      
      sel_scenarios_dt = sel_scenarios_dt[prod_quota_scenario == 'no quota']
      sel_scenarios_dt = sel_scenarios_dt[1:1000]
      
      
    }
  
  
  
  
  return(sel_scenarios_dt)
  
  
  
}