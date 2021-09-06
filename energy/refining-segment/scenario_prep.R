# refinery scenario prep
# created: September 6, 2021
# author: Tracey Mangin

  # 
  # outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'
  data_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data/stocks-flows/processed'
  scen_path         = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/project-materials/scenario-inputs'
  inn_file          = 'innovation_scenarios.csv'
  carbon_file       = 'final_carbon_tax_scenarios.csv'
  ccs_ref_file      = 'ccs_refining_scenarios.csv'
  incentive_file    = 'CCS_LCFS_45Q.xlsx'
  
  
  # load packages -----
  
  library(data.table)
  library(openxlsx)
  
  # load data -----
  innovation_scens = fread(file.path(scen_path, inn_file), header = T)
  
  carbonpx_scens = fread(file.path(scen_path, carbon_file), header = T)
  # carbonpx_scens[carbon_price_scenario == 'last CA auction price', carbon_price := 0] # assume rystard's BAU opex already embeds carbon price
  carbonpx_scens[, carbon_price_usd_per_kg := carbon_price/1000] # convert from usd per metric ton to usd per kg
  carbonpx_scens = carbonpx_scens[, c('year', 'carbon_price_scenario', 'carbon_price_usd_per_kg')]
  
  ccs_scens = fread(file.path(scen_path, ccs_ref_file), header = T)
  ccs_scens[, ccs_price_usd_per_kg := ccs_price/1000] # convert from usd per metric ton to usd per kg
  ccs_scens = ccs_scens[, c('year', 'ccs_scenario', 'ccs_price_usd_per_kg')]
  ccs_scens[, ccs_scenario := factor(ccs_scenario, levels = c('high CCS cost', 'medium CCS cost', 'low CCS cost'))]
  
  # load ccs incentives file 
  incentives_scens = setDT(read.xlsx(file.path(data_path, incentive_file), sheet = 'scenarios', cols = c(1:3)))
  
  # create adjusted ccs costs ------
  
  ccs_scens_adj = ccs_scens[incentives_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  ccs_scens_adj[, ccs_scenario_adj := fcase(incentive_scenario == 'no incentives', paste0(ccs_scenario),
                                            incentive_scenario == '45Q only', paste0(ccs_scenario, ' - 45Q'),
                                            incentive_scenario == '45Q + LCFS', paste0(ccs_scenario, ' - 45Q - LCFS'))]
  
  
  # adjust ccs price with incentives
  ccs_scens_adj[, ccs_price_usd_per_kg_adj := ccs_price_usd_per_kg - (incentive_price/1e3)]
  
  # create constrained version 
  ccs_scens_neg = ccs_scens_adj[ccs_scenario_adj %in% unique(ccs_scens_adj[ccs_price_usd_per_kg_adj < 0, ccs_scenario_adj])]
  ccs_scens_neg[, ccs_scenario_adj := paste0(ccs_scenario_adj, ' (constrained)') ]
  ccs_scens_neg[, ccs_price_usd_per_kg_adj := fifelse(ccs_price_usd_per_kg_adj < 0, 0, ccs_price_usd_per_kg_adj)]
  
  # combine ccs scenarios
  ccs_scens_all = rbind(ccs_scens_adj, ccs_scens_neg)
  
  # select columns 
  ccs_scens_all = ccs_scens_all[, .(year, ccs_scenario_adj, ccs_price_usd_per_kg_adj)]
  setnames(ccs_scens_all, c('ccs_scenario_adj', 'ccs_price_usd_per_kg_adj'), c('ccs_scenario', 'ccs_price_usd_per_kg'))
  
  
  ## list through all scenarios ------
  
  scenarios_dt = innovation_scens[carbonpx_scens, on = .(year), allow.cartesian = T, nomatch = 0]
  scenarios_dt = scenarios_dt[ccs_scens_all, on = .(year), allow.cartesian = T, nomatch = 0]
  
  ## save
  fwrite(scenarios_dt, file.path(scen_path, 'refinery_scenario_inputs.csv'))
  




