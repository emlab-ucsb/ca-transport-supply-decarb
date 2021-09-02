## Tracey Mangin
## September 2, 2021






## bau scen ids
full_site_out[, scen_id := fifelse((oil_price_scenario == 'reference case' & 
                                      innovation_scenario == 'low innovation' & 
                                      carbon_price_scenario == 'price floor' & 
                                      ccs_scenario == 'medium CCS cost' &
                                      excise_tax_scenario == 'no tax' &
                                      setback_scenario == 'no_setback' &
                                      prod_quota_scenario == 'no quota'), 'E-BAU', paste0("E-", scen_id))]