## Tracey Mangin
## November 8, 2022
## Health outputs

## libraries
library(data.table)
library(tidyverse)
library(broom)
library(rebus)
library(readxl)
library(openxlsx)
# 
# ## paths 
# main_path              = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/'
# extraction_folder_path = 'outputs/academic-out/extraction/extraction_2022-11-07/'
# state_save_path        = paste0(main_path, extraction_folder_path, 'state-results/')
# field_out              = paste0(main_path, "outputs/predict-production/extraction_2022-11-07/revision-full-test/field-out/")
# health_out             = paste0(main_path, "outputs/academic-out/health/")
# 
# ## create a folder to store outputs
# cur_date              = Sys.Date()
# save_info_path        = paste0(main_path, 'outputs/academic-out/extraction/figures/nature-energy-revision/original_model/')
# dir.create(save_info_path, showWarnings = FALSE)  
# 
# ## files
# ghg_file            = 'indust_emissions_2000-2019.csv'
# scc_file            = 'social_cost_carbon.csv'
# carbon_px_file      = 'carbon_price_scenarios_revised.xlsx'
# field_cluster_file  = 'extraction_field_cluster_xwalk.csv'
# 
# 
# ## read in social cost of carbon
# scc_df <- fread(paste0(main_path, 'data/stocks-flows/processed/', scc_file))
# 
# ## filter for 3 percent, 
# scc_df_filt <- scc_df[discount_rate == 'three_perc_avg', .(year, social_cost_co2)]
# 
# ## Create population by year time series
# ct_population <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
#   mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
#                         stringr::str_sub(gisjoin, 5, 7),
#                         stringr::str_sub(gisjoin, 9, 14))) %>%
#   select(ct_id, lower_age, year, pop) %>%
#   as.data.table()
# 
# state_population <- ct_population %>%
#   filter(lower_age > 29)%>%
#   group_by(year) %>%
#   summarize(total_state_pop = sum(pop, na.rm = T)) %>%
#   ungroup() %>%
#   as.data.table()
# 
# #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
# cpi2020 <- 109.1951913
# cpi2019 <- 107.8645906
# cpi2015 <- 100
# 
# ## discount rate
# discount_rate <- 0.03
# 
# ## health params
# VSL_2015 <- 8705114.25462459
# VSL_2019 <- VSL_2015 * cpi2019 / cpi2015 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
# # VSL_2020 <- VSL_2015 * cpi2020 / cpi2015
# income_elasticity_mort <- 0.4
# 
# ## for monetary mortality impact
# growth_rates <- read.csv(paste0(main_path, "data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
#   filter(year > 2018) %>%
#   mutate(growth = ifelse(year == 2019, 0, growth_2030),
#          cum_growth = cumprod(1 + growth)) %>%
#   select(-growth_2030, -growth) %>%
#   as.data.table()
# 
# ## function for health impacts
# future_WTP <- function(elasticity, growth_rate, WTP){
#   return(elasticity * growth_rate * WTP + WTP) 
# }
# 
# 
# 
# ## health, relative to BAU
# ## -----------------------------------------------------
# 
# health_dac_bau <- health_scens[, .(scen_id, target, target_policy, policy_intervention, census_tract, year, mortality_delta, cost, cost_PV, dac_multiplier)]
# health_dac_bau[, oil_price_scenario := sub("-.*", "", scen_id)]
# 
# health_dac_bau[, dac_av_mort := dac_multiplier * mortality_delta]
# health_dac_bau[, dac_av_mort_cost := dac_multiplier * cost]
# health_dac_bau[, dac_av_mort_cost_pv := dac_multiplier * cost_PV]
# 
# ## aggregate at state level
# health_dac_bau <- health_dac_bau[, .(cumul_dac_av_mort = sum(dac_av_mort),
#                                      cumul_total_av_mort = sum(mortality_delta),
#                                      cumul_dac_av_mort_cost = sum(dac_av_mort_cost),
#                                      cumul_total_av_mort_cost = sum(cost),
#                                      cumul_dac_av_mort_pv = sum(dac_av_mort_cost_pv),
#                                      cumul_total_av_mort_pv = sum(cost_PV)), by = .(scen_id, oil_price_scenario, target, policy_intervention, target_policy)]
# 
# health_dac_bau[, dac_share_av_mort := cumul_dac_av_mort / cumul_total_av_mort]
# health_dac_bau[, dac_share_av_cost := cumul_dac_av_mort_cost / cumul_total_av_mort_cost]
# health_dac_bau[, dac_share_av_pv := cumul_dac_av_mort_pv / cumul_total_av_mort_pv]
# 
# # health_dac_state[, dac_share_health := fifelse(is.na(dac_share_health), 0, dac_share_health)]
# 
# # ## add cumulative ghg savings relative to bau
# # 
# # health_dac_ghg <- merge(health_dac_state, cumul_ghg_df,
# #                         by = "scen_id",
# #                         all.x = T)
# # 
# # health_dac_ghg[, dac_av_mort_ghg := cumul_dac_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, total_av_mort_ghg := cumul_total_av_mort / cumul_ghg_savings]
# # health_dac_ghg[, DAC_share_av_mort_ghg := dac_av_mort_ghg / total_av_mort_ghg]
# # 
# health_dac_bau <- melt(health_dac_bau, id.vars = c("scen_id", "oil_price_scenario", "target", "policy_intervention",
#                                                    "target_policy"),
#                        measure.vars = c("cumul_dac_av_mort", "cumul_total_av_mort", "dac_share_av_mort",
#                                         "cumul_dac_av_mort_cost", "cumul_total_av_mort_cost", "dac_share_av_cost",
#                                         "cumul_dac_av_mort_pv", "cumul_total_av_mort_pv", "dac_share_av_pv"),
#                        variable.name = "metric", value.name = "value")
# 
# health_dac_bau[, type := fifelse(metric %in% c("cumul_dac_av_mort", "cumul_dac_av_mort_cost", "cumul_dac_av_mort_pv"), "DAC population",
#                                  fifelse(metric %in% c("cumul_total_av_mort", "cumul_total_av_mort_cost", "cumul_total_av_mort_pv"), "Total population", "DAC share"))]
# 
# health_dac_bau$type <- factor(health_dac_bau$type, levels = c("Total population", "DAC population", "DAC share"))
# 
# health_dac_bau[, category := "Avoided mortalities"]
# 
# 
# ## bind and save
# dac_bau_df <- rbind(labor_bau_dac, health_dac_bau)
# 
# dac_bau_df <- merge(dac_bau_df, setback_2045,
#                     by = c("oil_price_scenario", "target"),
#                     all.x = T)
# 
# dac_bau_df[, target_label := fifelse(policy_intervention == "BAU", target,
#                                      fifelse(target == "90perc_reduction", "90%", target_label))]
# 
# dac_bau_df <- merge(dac_bau_df, ghg_2045,
#                     by = c("scen_id", "oil_price_scenario"),
#                     all.x = T)
# 
# 
# fwrite(dac_bau_df, paste0(save_info_path, 'dac_bau_health_labor_all_oil.csv'))