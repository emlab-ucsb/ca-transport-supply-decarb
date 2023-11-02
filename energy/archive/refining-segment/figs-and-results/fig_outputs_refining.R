## Tracey Mangin
## December 8 2021
## refining fig outputs

## libraries
library(data.table)
library(broom)
library(rebus)
library(tidyverse)

## paths 
main_path <- '/Users/tracey/Library/CloudStorage/GoogleDrive-tmangin@ucsb.edu/Shared\ drives/emlab/projects/current-projects/calepa-cn/'
refining_folder_path <- 'outputs/academic-out/refining/refining_2022-05-11/'
state_save_path     = paste0(main_path, refining_folder_path)

## create a folder to store outputs
save_info_path        = paste0(main_path, 'outputs/academic-out/refining/figures/')
dir.create(save_info_path, showWarnings = FALSE) 


## files
carbon_px_file <- 'carbon_price_scenarios_revised.xlsx'
scc_file <- 'social_cost_carbon.csv'

## read inputs
state_out <- fread(paste0(state_save_path, "/subset_state_results.csv"))
county_out <- fread(paste0(main_path, refining_folder_path, '/county_refining_outputs.csv'))
site_out <- fread(paste0(main_path, refining_folder_path, "site_refining_outputs.csv"))


## CPI values
#(https://fred.stlouisfed.org/series/CPALTT01USA661S)
cpi2020 <- 109.1951913
cpi2019 <- 107.8645906
cpi2015 <- 100

## health info
## -------------------------------------

## Coefficients from Krewski et al (2009) for mortality impact
beta <- 0.00582
se <- 0.0009628

## for monetary mortality impact
growth_rates <- read.csv(paste0(main_path, "/data/benmap/processed/growth_rates.csv"), stringsAsFactors = FALSE) %>%
  filter(year > 2018) %>%
  mutate(growth = ifelse(year == 2019, 0, growth_2030),
         cum_growth = cumprod(1 + growth)) %>%
  select(-growth_2030, -growth) %>%
  as.data.table()

#Parameters for monetary health impact
VSL_2015 <- 8705114.25462459
VSL_2019 <- VSL_2015 * cpi2019/cpi2015 #(https://fred.stlouisfed.org/series/CPALTT01USA661S)
income_elasticity_mort <- 0.4

future_WTP <- function(elasticity, growth_rate, WTP){
  return(elasticity * growth_rate * WTP + WTP) 
}



# cpi_df <- setDT(read.xlsx(paste0(main_path, 'data/stocks-flows/processed/', carbon_px_file), sheet = 'BLS Data Series', startRow = 12))
# 
# cpi_df <- cpi_df[Year %in% c(2019, 2020), .(Year, Annual)]
# 
# setnames(cpi_df, c("Year", "Annual"), c("year", "annual"))
# 
# cpi2020 <- cpi_df %>%
#   filter(year == 2020) %>%
#   select(annual) %>%
#   as.numeric()
# 
# cpi2019 <- cpi_df %>%
#   filter(year == 2019) %>%
#   select(annual) %>%
#   as.numeric()

## discount rate
discount_rate <- 0.03

## social cost of carbon
scc_df <- fread(paste0(main_path, 'data/stocks-flows/processed/', scc_file))

## filter for 3 percent
scc_df_filt <- scc_df[discount_rate == 'three_perc_avg', .(year, social_cost_co2)]


## Create population by year time series
ct_population <- fread(paste0(main_path, "data/benmap/processed/ct_inc_45.csv"), stringsAsFactors  = FALSE) %>%
  mutate(ct_id = paste0(stringr::str_sub(gisjoin, 2, 3),
                        stringr::str_sub(gisjoin, 5, 7),
                        stringr::str_sub(gisjoin, 9, 14))) %>%
  select(ct_id, lower_age, year, pop) %>%
  as.data.table()

state_population <- ct_population %>%
  filter(lower_age > 29)%>%
  group_by(year) %>%
  summarize(total_state_pop = sum(pop, na.rm = T)) %>%
  ungroup() %>%
  as.data.table()

## 2019 emissions
refin_file <- 'ref_scenario_id_list.csv'
ghg_2019_file <- 'refining_emissions_state_2019_revised.csv'
ghg_2019 <- fread(paste0(main_path, "/model-development/scenario-plot/refinery-outputs/", ghg_2019_file))

ghg_2019 <- ghg_2019[boundary == "complete", .(year, value)]
ghg_2019[, mtco2e := value / 1e9]
ghg_2019[, value := NULL]
ghg_2019_val <- ghg_2019$mtco2e[1]


## carbon price scenario values
carbon_scens <- c("price floor", "central SCC", "price ceiling")

## carbon target results
target_results <- state_out[year == 2045, .(scen_id, year, carbon_price_scenario, ghg_kg)]
target_results <- target_results[carbon_price_scenario %in% carbon_scens]
target_results[, total_state_ghg_MtCO2 := ghg_kg / (1000 * 1e6)]
target_results[, rel_reduction := (total_state_ghg_MtCO2 - ghg_2019_val) / ghg_2019_val]
target_results <- target_results[, .(scen_id, rel_reduction)]


## filter for subset
## ---------------------------------------

## state scens
state_scens <- state_out[carbon_price_scenario %in% carbon_scens]

## state labor levels
state_labor_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                      ccs_scenario, demand_scenario,  refining_scenario, year, total_emp, total_comp)]

## convert to 2019 dollars
state_labor_levels[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]

## calc PV
state_labor_levels[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate) ^ (year - 2019))]


## melt
state_labor_levels <- melt(state_labor_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                           'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                           measure.vars = c("total_emp", "total_comp", "total_comp_usd19", "total_comp_PV"),
                           variable.name = "metric",
                           value.name = "value")

## emissions, consumption, state revenue
## ------------------------------
state_energy_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, demand_scenario, refining_scenario, year, total_state_revenue, bbls_consumed, ghg_kg)]

## convert to Mt
state_energy_levels[, total_state_ghg_MtCO2 := ghg_kg / (1000 * 1e6)]
state_energy_levels[, ghg_kg := NULL]

## convert USD to 2019 USD?
# state_energy_levels[, total_state_revenue_usd19 := total_state_revenue * cpi2019 / cpi2020]

## melt
state_energy_levels <- melt(state_energy_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario',  'year'),
                            measure.vars = c("total_state_revenue", "bbls_consumed", "total_state_ghg_MtCO2"),
                            variable.name = "metric",
                            value.name = "value")

## health
## ------------------------------
state_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                       ccs_scenario, demand_scenario, refining_scenario, year, mean_total_pm25, mean_delta_total_pm25, mortality_level, mortality_delta, cost_2019_PV, cost_PV)]


## melt
state_health_levels <- melt(state_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario', 
                                                             'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                            measure.vars = c("mean_total_pm25", "mean_delta_total_pm25", 
                                             "mortality_level", "mortality_delta", 
                                             "cost_2019_PV", "cost_PV"),
                            variable.name = "metric",
                            value.name = "value")

## combine
##----------------------------------
state_levels <- rbind(state_energy_levels, state_labor_levels, state_health_levels)

## remove innovation scenario
state_levels[, innovation_scenario := NULL]

## add scneario names
state_levels[, scenario := paste0(refining_scenario, " - ", demand_scenario, " demand")]

## fix factor levels
state_levels$carbon_price_scenario <- factor(state_levels$carbon_price_scenario,
                                             levels = c('price floor', 'central SCC', 'price ceiling'))

state_levels$ccs_scenario <- factor(state_levels$ccs_scenario,
                                    levels = c('no ccs', 'medium CCS cost', 'high CCS cost'))

## add GHG reduction
## --------------------------------------------------

## 2045 emissions
ghg_2045 <- state_levels[metric == "total_state_ghg_MtCO2" &
                           year == 2045, .(scen_id, year, value)]

setnames(ghg_2045, "value", "ghg_2045_val")

ghg_2045[, ghg_2045_perc := (ghg_2045_val - ghg_2019$mtco2e[1]) / ghg_2019$mtco2e[1]]

# ghg_2045[, ghg_2045 := NULL]
ghg_2045[, year := NULL]
ghg_2045[, ghg_2045_perc_reduction := ghg_2045_perc * -100]

## merge with levels, save
state_levels <- merge(state_levels, ghg_2045,
                      by = c("scen_id"),
                      all.x = T)

## save levels
fwrite(state_levels, paste0(save_info_path, 'state_levels_subset_refining.csv'))


## ----------------------------------------------------------------------
## relative to BAU
## ----------------------------------------------------------------------

## select health outputs
## ---------------------------------------------------
rel_health_levels <- state_scens[, .(scen_id, oil_price_scenario, innovation_scenario, carbon_price_scenario,
                                     ccs_scenario, demand_scenario, refining_scenario, year,
                                     mean_delta_total_pm25, mortality_delta, cost_2019, cost, cost_2019_PV, cost_PV)]


rel_health_levels <- melt(rel_health_levels, id.vars = c('scen_id', 'oil_price_scenario', 'innovation_scenario',
                                                         'carbon_price_scenario', 'ccs_scenario', 'demand_scenario', 'refining_scenario', 'year'),
                          measure.vars = c("mean_delta_total_pm25", "mortality_delta", "cost_2019", "cost", "cost_2019_PV", "cost_PV"),
                          variable.name = "metric",
                          value.name = "value")

setnames(rel_health_levels, "value", "diff_bau")

rel_health_levels[, innovation_scenario := NULL]


rel_health_levels[, scenario := paste0(refining_scenario, " - ", demand_scenario, " demand")]

## add levels
rel_health_levels$carbon_price_scenario <- factor(rel_health_levels$carbon_price_scenario,
                                                  levels = c('price floor', 'central SCC', 'price ceiling'))

## add levels
rel_health_levels$ccs_scenario <- factor(rel_health_levels$ccs_scenario,
                                         levels = c('no ccs', 'medium CCS cost', 'high CCS cost'))

rel_health_levels <- merge(rel_health_levels, ghg_2045,
                      by = c("scen_id"),
                      all.x = T)


## BAU outputs for labor and energy
## -------------------------------------------
bau_out <- state_levels[carbon_price_scenario == "price floor" & ccs_scenario == "no ccs" &
                          demand_scenario == "BAU" & refining_scenario == "historic production" &
                          metric %in% c("total_state_revenue", "bbls_consumed", "total_state_ghg_MtCO2", 
                                        "total_emp", "total_comp_usd19", "total_comp_PV")]

setnames(bau_out, "value", "bau_value")
bau_out <- bau_out[, .(oil_price_scenario, year, metric, bau_value)]

## combine bau with scenario outputs
rel_vals <- state_levels[metric %in% c("total_state_revenue",
                                       "bbls_consumed",
                                       "total_state_ghg_MtCO2",
                                       "total_emp",
                                       "total_comp_usd19",
                                       "total_comp_PV")]


rel_vals <- merge(rel_vals, bau_out,
                  by = c("oil_price_scenario", "year", "metric"),
                  all.x = T)

rel_vals[, diff_bau := value - bau_value]

rel_vals <- rel_vals[, .(scen_id, scenario, oil_price_scenario, carbon_price_scenario, 
                         ccs_scenario, demand_scenario, refining_scenario, year, metric, 
                         diff_bau, ghg_2045_val, ghg_2045_perc, ghg_2045_perc_reduction)]

## bind
state_rel_vals <- rbind(rel_vals, rel_health_levels)


## cumulative ---------------------------------------------------------

## -----------------------------------------------------------------------
## NPV
## -----------------------------------------------------------------------

## 1) cumul benefit = cumul health benefit -compensation loss + carbon mitigation
## 2) cumul benefit / cumulative GHG emisisons

## social cost of carbon
scc_value <- state_rel_vals[metric == 'total_state_ghg_MtCO2']

## join with social cost of carbon
scc_value <- merge(scc_value, scc_df_filt,
                   by = 'year',
                   all.x = T)

## deflate to 2019 dollars
scc_value[, social_cost_usd19 := social_cost_co2 * (cpi2019 / cpi2020)]

scc_value[, scc_avoided_ghg := diff_bau * -1e6 * social_cost_usd19]

## apply discount rate
scc_value <- scc_value[year > 2019]
scc_value[, scc_avoided_ghg_pv := scc_avoided_ghg / ((1 + discount_rate) ^ (year - 2019))]


## summarise
cumul_scc_value <- scc_value[, .(scc_avoided_ghg = sum(scc_avoided_ghg, na.rm = T),
                                 scc_avoided_ghg_pv = sum(scc_avoided_ghg_pv)), by = .(scen_id, oil_price_scenario)]


cumul_rel_vals_bau <- state_rel_vals[, .(diff_bau = sum(diff_bau)), by = .(scen_id, scenario, oil_price_scenario, 
                                                                           carbon_price_scenario, ccs_scenario, demand_scenario,
                                                                           refining_scenario, ghg_2045_val, ghg_2045_perc,
                                                                           ghg_2045_perc_reduction, metric)]

cumul_rel_vals_bau <- cumul_rel_vals_bau[metric %in% c( "total_comp_usd19", "total_comp_PV", "cost_2019",
                                                        "cost", "cost_2019_PV", "cost_PV")]

cumul_rel_vals_bau <- dcast(cumul_rel_vals_bau, scen_id + scenario + oil_price_scenario + carbon_price_scenario +
                              ccs_scenario + demand_scenario + refining_scenario + ghg_2045_val +
                              ghg_2045_perc + ghg_2045_perc_reduction ~ metric, value.var = "diff_bau")

## join witih scc
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, cumul_scc_value,
                            by = c('scen_id', 'oil_price_scenario'),
                            all.x = T)

## total ghg emissions
ghg_total <- state_levels[metric == "total_state_ghg_MtCO2"]
ghg_total <- ghg_total[, .(cumul_ghg = sum(value)), by = .(scen_id)]

## cumul ghg
avoided_ghg <- state_rel_vals[metric == 'total_state_ghg_MtCO2']
avoided_ghg <- avoided_ghg[, .(cumul_diff_ghg = sum(diff_bau)), by = .(scen_id)]
avoided_ghg <- avoided_ghg[, cumul_diff_ghg := cumul_diff_ghg * -1]

## join
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, ghg_total,
                            by = "scen_id",
                            all.x = T)

## join with avoided ghg
cumul_rel_vals_bau <- merge(cumul_rel_vals_bau, avoided_ghg,
                            by = "scen_id",
                            all.x = T)



## calc benefit
cumul_rel_vals_bau[, benefit := (cost_PV * -1) + total_comp_PV + scc_avoided_ghg]
cumul_rel_vals_bau[, benefit_per_ghg := benefit / (cumul_diff_ghg * -1)]


npv_x_metric <- melt(cumul_rel_vals_bau, id.vars = c('scen_id', 'oil_price_scenario', 'carbon_price_scenario', 
                                                     'ccs_scenario', 'demand_scenario', 'refining_scenario', 
                                                     'cumul_ghg', 'cumul_diff_ghg'),
                     measure.vars = c("total_comp_PV", "cost_PV", "scc_avoided_ghg"),
                     variable.name = "metric",
                     value.name = "value")

npv_x_metric[, value := fifelse(metric == 'cost_PV', value * -1, value)]
npv_x_metric[, value_per_ghg :=  value / cumul_diff_ghg]
npv_x_metric[, value_billion := value / 1e9]
npv_x_metric[, value_per_ghg_million := value_per_ghg / 1e6]
npv_x_metric[, title := fifelse(metric == "total_comp_PV", "Labor: Compensation",
                                fifelse(metric == "cost_PV", "Health: Avoided mortality", "Abated GHG"))]

npv_x_metric <- merge(npv_x_metric, ghg_2045,
                      by = "scen_id",
                      all.x = T)

npv_x_metric[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]

npv_x_metric[, value_per_ghg_million := fifelse(is.na(value_per_ghg_million), 0, value_per_ghg_million)]

# ## value_billion, value_per_ghg_million
# npv_x_metric_long <- npv_x_metric[, .(scen_id, oil_price_scenario, carbon_price_scenario, ccs_scenario, 
#                                       demand_scenario, refining_scenario, title, rel_reduction, value_billion, value_per_ghg_million)]
# 
# npv_x_metric_long <- melt(npv_x_metric_long, measure.vars = c("value_billion", "value_per_ghg_million"),
#                                            variable.name = "unit", value.name = "value")
# 
# npv_x_metric_long[, unit_name := fifelse(unit == "value_billion", "NPV (2020 USD billion)", 
#                                          "NPV per avoided GHG emission\n(2020 USD million per MtCO2e)")]

fwrite(npv_x_metric, paste0(save_info_path, 'npv_x_metric_refining.csv'))


## ----------------------------------------------------------------------
## cumulative impacts / avoided GHG emissions (total, DAC, DAC share)
## ----------------------------------------------------------------------


## labor, county
labor_scens <- county_out[(carbon_price_scenario %in% carbon_scens &
                           innovation_scenario == "low innovation" &
                           ccs_scenario %in% c('no ccs', 'medium CCS cost', 'high CCS cost'))]

labor_scens[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]

## bau
bau_emp <- labor_scens[carbon_price_scenario == "price floor" & ccs_scenario == "no ccs" &
                         demand_scenario == "BAU" & refining_scenario == "historic production", .(oil_price_scenario, county, year, total_comp)]

## convert to 2019 dollars
bau_emp[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]

## calc PV
bau_emp[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate) ^ (year - 2019))]

setnames(bau_emp, "total_comp_usd19", "bau_comp_usd19")
setnames(bau_emp, "total_comp_PV", "bau_comp_PV")

bau_emp <- bau_emp[, .(oil_price_scenario, county, year, bau_comp_PV)]

## join
labor_scens <-  merge(labor_scens, bau_emp,
                      by = c("oil_price_scenario", "county", "year"),
                      all.x = T)

## convert outputs to 2019 USD and discount
labor_scens[, total_comp_usd19 := total_comp * cpi2019 / cpi2020]
labor_scens[, total_comp_PV := total_comp_usd19 / ((1 + discount_rate) ^ (year - 2019))]

## calc difference and dac share
labor_scens[, diff_pv := total_comp_PV - bau_comp_PV]
labor_scens[, dac_pv := diff_pv * dac_share]


## calculate dac share, labor FTE
# labor_dac <- labor_scens[, .(cumul_dac_emp_loss = sum(dac_emp), 
#                              cumul_total_emp_loss = sum(diff)), by = .(scen_id, oil_price_scenario, ccs_scenario, 
#                                                                        carbon_price_scenario, refining_scenario,
#                                                                        demand_scenario, scenario, county, median_hh_income)]
# 

## summarise labor outcomes (total and DAC)
labor_dac_state <- labor_scens[, .(cumul_dac_pv_loss = sum(dac_pv),
                                   cumul_total_pv_loss = sum(diff_pv)), by = .(scen_id, oil_price_scenario, ccs_scenario, 
                                                                                           carbon_price_scenario, refining_scenario,
                                                                                           demand_scenario, scenario)]

labor_dac_state[, dac_share_emp := fifelse(cumul_dac_pv_loss == 0 & cumul_total_pv_loss == 0, 0, cumul_dac_pv_loss / cumul_total_pv_loss)]


labor_dac_state <- merge(labor_dac_state, ghg_2045,
                         by = "scen_id",
                         all.x = T)

## cumulative job loss rel bau / cumulative ghg savings rel bau
## ---------------------------------------------------------------

# ## add cumulative ghg savings relative to bau
# cumul_ghg_df <- bau_cumulative_df[metric == "total_state_ghg_MtCO2", .(scen_id, sum_metric)]
# setnames(cumul_ghg_df, "sum_metric", "cumul_ghg_savings")
# cumul_ghg_df[, cumul_ghg_savings := cumul_ghg_savings * -1]
# 
# # labor ghg
# labor_ghg_df <- merge(labor_dac_state, cumul_ghg_df,
#                       by = "scen_id",
#                       all.x = T)
# 
# labor_ghg_df[, dac_loss_ghg := fifelse(cumul_dac_emp_loss == 0 & cumul_ghg_savings == 0, 0, cumul_dac_emp_loss / cumul_ghg_savings)]
# labor_ghg_df[, total_loss_ghg := fifelse(cumul_total_emp_loss == 0 & cumul_ghg_savings == 0, 0, cumul_total_emp_loss / cumul_ghg_savings)]
# labor_ghg_df[, dac_share_loss_ghg := fifelse(dac_loss_ghg == 0 & total_loss_ghg == 0, 0, dac_loss_ghg / total_loss_ghg)]

# labor_ghg_df <- melt(labor_ghg_df, id.vars = c("scen_id", "oil_price_scenario", "ccs_scenario", "carbon_price_scenario",
#                                                "refining_scenario", "demand_scenario", "scenario", "cumul_ghg_savings", "rel_reduction"),
#                      measure.vars = c("dac_loss_ghg", "total_loss_ghg", "dac_share_loss_ghg"),
#                      variable.name = "type", value.name = "emp_loss_ghg")
# 
# labor_ghg_df[, type := fifelse(type == "dac_loss_ghg", "DAC population",
#                                fifelse(type == "total_loss_ghg", "Total population", "DAC share"))]
# 
# labor_ghg_df$type <- factor(labor_ghg_df$type, levels = c("Total population", "DAC population", "DAC share"))
# 
# labor_ghg_df <- labor_ghg_df %>%
#   mutate(emp_loss_ghg_adj = ifelse(scenario == "historic production - BAU demand", NA, emp_loss_ghg))


## prep for joining with health
## ---------------------------------

labor_dac_state <- melt(labor_dac_state, id.vars = c("scen_id", "oil_price_scenario",
                                                     "ccs_scenario", "carbon_price_scenario", "refining_scenario",
                                                     "demand_scenario", "scenario"),
                      measure.vars = c("cumul_dac_pv_loss", "cumul_total_pv_loss", "dac_share_emp"),
                      variable.name = "metric", value.name = "value")

labor_dac_state[, type := fifelse(metric %in% c("cumul_dac_pv_loss"), "DAC population",
                                fifelse(metric %in% c("cumul_total_pv_loss"), "Total population", "DAC share"))]

labor_dac_state$type <- factor(labor_dac_state$type, levels = c("Total population", "DAC population", "DAC share"))

labor_dac_state[, category := "Employment"]





## --------------------------------------------------------------------------
## health DAC
## --------------------------------------------------------------------------

## compile health outputs
files_to_process <- paste0(unique(state_scens$scen_id), "_ct_results.rds")

health_out <- list()

for (i in 1:length(files_to_process)) {
  
  file_name <- files_to_process[i]
  
  scen_out <- readRDS(paste0(state_save_path, 'subset-census-tract-results/', file_name))
  
  health_out[[i]] <- scen_out
  
}

health_scens <- rbindlist(health_out)

## add columns for filtering
id_columns <- unique(state_levels[, .(scen_id, carbon_price_scenario, ccs_scenario, demand_scenario,
                               refining_scenario)])

health_scens <- merge(id_columns, health_scens,
                      by = c('scen_id'),
                      all.x = T)


## calculate DAC

health_scens[, dac_multiplier := fifelse(disadvantaged == "Yes", 1, 0)]

health_scens[, dac_share_mortality := dac_multiplier * cost_PV]


## calculate dac share, mortality
health_dac_state <- health_scens[, .(cumul_dac_mortality = sum(dac_share_mortality, na.rm = T), 
                                            cumul_total_mortality = sum(cost_PV, na.rm = T)), by = .(scen_id, oil_price_scenario,
                                                                                                     ccs_scenario, carbon_price_scenario,
                                                                                                     refining_scenario, demand_scenario)]

health_dac_state[, scenario := paste0(refining_scenario, ' - ', demand_scenario, ' demand')]

health_dac_state[, dac_share_health := cumul_dac_mortality / cumul_total_mortality]

health_dac_state[, dac_share_health := fifelse(is.na(dac_share_health), 0, dac_share_health)]

## prep for joining with health
## ---------------------------------

health_dac_state <- melt(health_dac_state, id.vars = c("scen_id", "oil_price_scenario",
                                                     "ccs_scenario", "carbon_price_scenario", "refining_scenario",
                                                     "demand_scenario", "scenario"),
                        measure.vars = c("cumul_dac_mortality", "cumul_total_mortality", "dac_share_health"),
                        variable.name = "metric", value.name = "value")

health_dac_state[, type := fifelse(metric %in% c("cumul_dac_mortality"), "DAC population",
                                  fifelse(metric %in% c("cumul_total_mortality"), "Total population", "DAC share"))]

health_dac_state$type <- factor(health_dac_state$type, levels = c("Total population", "DAC population", "DAC share"))

health_dac_state[, category := "Health"]



## cumulative job loss rel bau / cumulative ghg savings rel bau
## ---------------------------------------------------------------

# # labor ghg
# health_ghg_df <- merge(health_dac_state, cumul_ghg_df,
#                        by = "scen_id",
#                        all.x = T)
# 
# health_ghg_df[, dac_avoided_mort_ghg := fifelse(cumul_dac_mortality == 0 & cumul_ghg_savings == 0, 0, (cumul_dac_mortality *-1) / cumul_ghg_savings)]
# health_ghg_df[, total_avoided_mort_ghg := fifelse(cumul_total_mortality == 0 & cumul_ghg_savings == 0, 0, (cumul_total_mortality * -1) / cumul_ghg_savings)]
# health_ghg_df[, dac_share_loss_ghg := fifelse(dac_avoided_mort_ghg == 0 & total_avoided_mort_ghg == 0, 0, dac_avoided_mort_ghg / total_avoided_mort_ghg)]
# 
# health_ghg_df <- melt(health_ghg_df, id.vars = c("scen_id", "oil_price_scenario", "ccs_scenario", "carbon_price_scenario",
#                                                  "refining_scenario", "demand_scenario", "scenario", "cumul_ghg_savings", "rel_reduction"),
#                       measure.vars = c("dac_avoided_mort_ghg", "total_avoided_mort_ghg", "dac_share_loss_ghg"),
#                       variable.name = "type", value.name = "avoided_mort_ghg")
# 
# health_ghg_df[, type := fifelse(type == "dac_avoided_mort_ghg", "DAC population",
#                                 fifelse(type == "total_avoided_mort_ghg", "Total population", "DAC share"))]
# 
# health_ghg_df$type <- factor(health_ghg_df$type, levels = c("Total population", "DAC population", "DAC share"))
# 
# health_ghg_df <- health_ghg_df %>%
#   mutate(avoided_mort_ghg_adj = ifelse(scenario == "historic production - BAU demand", NA, avoided_mort_ghg))

## rbind and save
## ---------------------

# labor_bind <- labor_ghg_df %>%
#   mutate(metric = "Employment loss per avoided GHG") %>%
#   rename(value = emp_loss_ghg) %>%
#   select(-emp_loss_ghg_adj)
# 
# health_bind <- health_ghg_df %>%
#   mutate(metric = "Avoided mortalities per avoided GHG") %>%
#   rename(value = avoided_mort_ghg) %>%
#   select(-avoided_mort_ghg_adj)

## bind
dac_df <- rbind(labor_dac_state, health_dac_state)

## merge with ghg
dac_df <- merge(dac_df, ghg_2045,
                by = c('scen_id'))



fwrite(dac_df, paste0(save_info_path, 'dac_health_labor_refining.csv'))






